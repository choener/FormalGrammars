
-- | We define a simple domain-specific language for context-free languages.
--
-- TODO we still need to make sure to handle NTs correctly. It should be that
-- we write @[X,Y]@ in multidim cases and then we check in rules if @[X,Y]@ is
-- available ... of course for @[X,eps]@ we then need to check if @eps@ is an
-- epsilon symbol.

module FormalLanguage.CFG.Parser
  ( module FormalLanguage.CFG.Parser
  , Result (..)
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens hiding (Index, outside, indices, index)
import           Control.Monad
import           Control.Monad.State.Class (MonadState (..))
import           Control.Monad.Trans.State.Strict hiding (get)
import           Data.Default
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Sequence (Seq)
import           Debug.Trace
import qualified Data.HashSet as H
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Parser.Token.Style
import           Text.Printf
import           Text.Trifecta
import qualified Text.PrettyPrint.ANSI.Leijen as AL
import           Data.Monoid
import           Text.Trifecta.Delta (Delta (Directed))
import           Data.ByteString.Char8 (pack)
import           Data.List (nub,genericIndex)

import Data.Data.Lens

import           FormalLanguage.CFG.Grammar
import           FormalLanguage.CFG.Outside
import           FormalLanguage.CFG.PrettyPrint.ANSI
-- testPrint = test >>= \z -> case z of {Just g -> mapM_ (printDoc . genGrammarDoc) g}



-- | The environment captures both the current grammar we work with
-- (@current@) as well as everything we have parsed until now (@env@).

data GrammarEnv = GrammarEnv
  { _current  :: Grammar              -- ^ The grammar declaration we currently evaluate
  , _env      :: Map String Grammar   -- ^ grammars within the environment
  , _emit     :: Seq Grammar          -- ^ sequence of grammars to emit (in order)
  , _verbose  :: Bool                 -- ^ emit lots of informative messages
  }
  deriving (Show)

makeLenses ''GrammarEnv

instance Default GrammarEnv where
  def = GrammarEnv { _current = def
                   , _env     = def
                   , _emit    = def
                   , _verbose = False
                   }


test = parseFromFile ((evalStateT . runGrammarParser) (parseEverything empty) def{_verbose = True}) "tests/parsing.gra"



-- parse = parseString ((evalStateT . runGrammarParser) (parseEverything empty) def{_verbose = True})
parse = parseString ((evalStateT . runGrammarParser) (parseEverything empty) def) (Directed (pack "via QQ") (fromIntegral 0) 0 0 0)

-- | Parse everything in the grammar source. The additional argument, normally
-- @empty :: Alternative f a@, allows for providing additional parsing
-- capabilities -- e.g. for grammar products..

parseEverything :: Parse m () -> Parse m (Seq Grammar)
parseEverything ps = whiteSpace *> some (assign current def >> p) <* eof >> use emit
  where p = parseCommands <|> parseGrammar <|> parseOutside <|> parseNormStartEps <|> parseEmitGrammar <|> ps

-- | The basic parser, which generates a grammar from a description.

parseGrammar :: Parse m ()
parseGrammar = do
  reserve fgIdents "Grammar:"
  n <- newGrammarName
  current.grammarName    .= n
  current.params   <~ (M.fromList . fmap (_indexName &&& id))  <$> (option [] $ parseIndex EvalGrammar) <?> "global parameters"
  current.synvars  <~ (M.fromList . fmap (_name &&& id)) <$> some (parseSyntacticDecl EvalSymb)
  current.synterms <~ (M.fromList . fmap (_name &&& id)) <$> many (parseSynTermDecl EvalSymb)
  current.termvars <~ (M.fromList . fmap (_name &&& id)) <$> many parseTermDecl
  current.indices  <~ (M.fromList . fmap (_indexName &&& id)) <$> setIndices
  -- TODO current.epsvars <~ ...
  current.start    <~ parseStartSym
  current.rules    <~ (S.fromList . concat) <$> some parseRule
  reserve fgIdents "//"
  g <- use current
  v <- use verbose
  seq (unsafePerformIO $ if v then (printDoc . genGrammarDoc $ g) else return ())
    $ env %= M.insert n g

-- | Collect all indices and set them as active

setIndices :: Parse m [Index]
setIndices = do
  sv <- use (current . synvars  . folded . index)
  st <- use (current . synterms . folded . index)
  tv <- use (current . termvars . folded . index)
  return $ nub $ sv ++ st ++ tv

-- | Which of the intermediate grammar to actually emit as code or text in
-- TeX. Single line: @Emit: KnownGrammarName@

parseEmitGrammar :: Parse m ()
parseEmitGrammar = do
  reserve fgIdents "Emit:"
  g <- knownGrammarName
  v <- use verbose
  seq (unsafePerformIO $ if v then (printDoc . genGrammarDoc $ g) else return ())
    $ emit %= ( Seq.|> g) -- snoc the grammar

-- | Normalize start and epsilon rules in a known @Source:@, thereby
-- generating a new grammar.

parseNormStartEps :: Parse m ()
parseNormStartEps = do
  reserve fgIdents "NormStartEps:"
  n <- newGrammarName
  current.grammarName .= n
  reserve fgIdents "Source:"
  g <- (set grammarName n) <$> knownGrammarName <?> "known source grammar"
  reserve fgIdents "//"
  let h = normalizeStartEpsilon g
  v <- use verbose
  seq (unsafePerformIO $ if v then (printDoc . genGrammarDoc $ h) else return ())
    $ env %= M.insert n h

-- | Try to generate an outside grammar from an inside grammar. The @From:@
-- name is looked up in the environment.
--
-- @
-- Outside: NAME
-- From: (inside)NAME
-- //
-- @

parseOutside :: Parse m ()
parseOutside = do
  reserve fgIdents "Outside:"
  n <- newGrammarName
  reserve fgIdents "Source:"
  g <- knownGrammarName <?> "known source grammar"
  guard (not . isOutside $ g^.outside) <?> "source already is an outside grammar"
  reserve fgIdents "//"
  let h = set grammarName n $ toOutside g
  current .= h
  v <- use verbose
  seq (unsafePerformIO $ if v then (printDoc . genGrammarDoc $ h) else return ())
    $ env %= M.insert n h

-- | Some additional commands that change the parsing state.
--
-- TODO @MonoidOfPairs@ should generate an adapter function that turns any
-- 2-tape eval function into its k-tape version. This means collecting all
-- name pairs, then emitting the corresponding adapter. We'll also need
-- a monoidal function for combining pairs. (this is along the lines of
-- sum-of-pairs).

parseCommands :: Parse m ()
parseCommands = help <|> vrbose
  where help = reserve fgIdents "Help"
        vrbose = reserve fgIdents "Verbose" >> verbose .= True



-- * Helper parsers

-- |

fgIdents = set styleReserved rs emptyIdents
  where rs = H.fromList [ "Grammar:", "Outside:", "Source:", "NormStartEps:", "Emit:", "Help", "Verbose"
                        , "N:", "Y:", "T:", "S:", "->", "=", "<<<", "-", "e", "ε"
                        ]

-- |

newGrammarName :: Parse m String
newGrammarName = flip (<?>) "grammar name previously declared!" $ do
  n <- ident fgIdents
  e <- get
  let g = M.lookup n $ e^.env
  when (isJust g) $ unexpected "previously declared grammar name"
  return n

-- |

knownGrammarName :: Parse m Grammar
knownGrammarName = try $ do
  n <- ident fgIdents
  e <- get
  let g = M.lookup n $ e^.env
  when (isNothing g) $ unexpected "known source grammar"
  return $ fromJust g

-- | Parses a syntactic (or non-terminal) symbol (for the corresponding index type). Cf. 'parseSynTermDecl'.

parseSyntacticDecl :: EvalReq -> Parse m SynTermEps
parseSyntacticDecl e = do
  reserve fgIdents "N:"
  SynVar <$> (ident fgIdents <?> "syntactic variable name") <*> (option [] $ parseIndex e)

-- | Parses a syntactic terminal declaration; an inside syntactic variable in an outside context.

parseSynTermDecl :: EvalReq -> Parse m SynTermEps
parseSynTermDecl e = do
  reserve fgIdents "Y:"
  SynTerm <$> (ident fgIdents <?> "syntactic variable name") <*> (option [] $ parseIndex e)

-- |

parseTermDecl :: Parse m SynTermEps
parseTermDecl =
  (reserve fgIdents "T:" >> Term <$> (ident fgIdents <?> "terminal name") <*> pure [])
--  <|>
--  (reserve fgIdents "E:" >> Epsilon <$> (ident fgIdents <?> "epsilon terminal name"))

-- | The syntactic variable here needs to either have no index at all, have
-- a grammar-based index, or have a fully calculated index.

parseStartSym :: Parse m Symbol
parseStartSym
  =  (runUnlined $ reserve fgIdents "S:" *> knownSynVar EvalRule)
  <* someSpace

-- |

data EvalReq
  -- | Happens when we actually emit a grammar product (in development)
  = EvalFull
  -- | Happens when we work through the rules
  | EvalRule
  -- | Happens when we encounter @N: @ and define a symbol
  | EvalSymb
  -- | Happens when we define grammar-global parameters
  | EvalGrammar

-- |

knownSynVar :: EvalReq -> Stately m Symbol
knownSynVar e = Symbol <$> do
  ((:[]) <$> sv) <|> (brackets $ commaSep sv)
  where sv = flip (<?>) "known syntactic variable" . try $ do
               s <- ident fgIdents
               use (current . synvars . at s) >>= guard . isJust
               i <- option [] $ parseIndex e
               return $ SynVar s i

-- |

knownSynTerm :: EvalReq -> Stately m Symbol
knownSynTerm e = Symbol <$> do
  ((:[]) <$> sv) <|> (brackets $ commaSep sv)
  where sv = flip (<?>) "known syntactic terminal" . try $ do
               s <- ident fgIdents
               use (current . synterms . at s) >>= guard . isJust
               i <- option [] $ parseIndex e
               return $ SynVar s i

-- | Parses indices @{ ... }@ within curly brackets (@braces@).
--
-- When parsing the @EvalSymb@ case, indexed symbols are being created.
--
-- Parsing in rules is handled via @EvalRule@ and actually requires us
-- saying which explicit index we use.

parseIndex :: EvalReq -> Stately m [Index]
parseIndex e = concat <$> (braces . commaSep $ ix e) where
  -- only declare that indices exist, but do not set ranges, etc
  ix EvalGrammar = (\s -> [Index s 0 undefined [] 1]) <$> ident fgIdents
  -- TODO check if @n@ is globally known
  ix EvalSymb = do s <- ident fgIdents
                   reserve fgIdents "="
                   n <- natural
                   return [Index s 0 ISymbol [0..n-1] 1]
  ix EvalRule = do s <- ident fgIdents
                   let req    = (\k -> [Index s k IEq    [] 1]) <$ reserve fgIdents "=" <*> natural
                   let rminus = (\k -> [Index s k IMinus [] 1]) <$ reserve fgIdents "-" <*> natural
                   let rplus  = (\k -> [Index s k IPlus  [] 1]) <$> (option 0 $ reserve fgIdents "+" *> natural)    -- the option here is for @+0@
                   try req <|> try rminus <|> rplus
{-
parseIndex e = braces $ commaSep ix where
  ix = (\v -> Index v [] 0) <$> some alphaNum
-}

-- |

knownTermVar :: EvalReq -> Stately m Symbol
knownTermVar e = Symbol <$> do
  ((:[]) <$> (eps <|> tv)) <|> (brackets $ commaSep (del <|> eps <|> tv))
  where tv = flip (<?>) "known terminal variable" . try $ do
               i <- ident fgIdents
               t <- use (current . termvars . at i)
--               e <- use (current . epsvars  . at i)
               guard . isJust $ t -- <|> e
               return $ Term i []
               {-
               if isJust t
                then return $ Term i []
                else return $ Epsilon
                -}
        del = Deletion <$ reserve fgIdents "-"
        eps = Epsilon  <$ (reserve fgIdents "e" <|> reserve fgIdents "ε")

-- | Parses an already known symbol, either syntactic or terminal.
--
--TODO Correctly parse inside-syntactics in outside grammars? Do we want
--this explicitly?

knownSymbol :: EvalReq -> Stately m Symbol
knownSymbol e = try (knownSynVar e) <|> try (knownSynTerm e) <|> knownTermVar e

-- |

parseRule :: Parse m [Rule]
parseRule = (expandIndexed =<< runUnlined rule) <* someSpace
  where rule  = Rule
              <$> knownSynVar EvalRule
              <*  reserve fgIdents "->"
              <*> afun
              <*  string "<<<" <* spaces
              <*> some syms
        afun = (:[]) <$> ident fgIdents
        syms = knownSymbol EvalRule

-- | Once we have parsed a rule, we still need to extract all active
-- indices in the rule, and enumerate over them. This will finally generate
-- the set of rules we are interested in.

expandIndexed :: Rule -> Parse m [Rule]
expandIndexed r = do
  -- active index names
  let is :: [IndexName] = nub $ r ^.. biplate . indexName
  -- corresponding @Index@es
  js :: [Index] <- catMaybes <$> mapM (\i -> use (current . indices . at i)) is
  --error $ show js
  if null js
    then return [r]
    else mapM go $ sequence $ map expand js
  where -- updates the indices in the rules accordingly
        go :: [Index] -> Parse m Rule
        go ixs = foldM (\b a -> return $ b & biplate.index.traverse %~ changeIndex a) r ixs
        -- expands each index to all variants
        expand :: Index -> [Index]
        expand i = [ i & indexHere .~ j | j <- i^.indexRange ]
        changeIndex :: Index -> Index -> Index
        changeIndex i o
          | iin /= oin = o
          | o^.indexOp == IEq = o
          | null otr   = error $ printf "index %s uses var %d that is not in range %s!\n" (oin^.getIndexName) oih (show rng)
          | o^.indexOp == IPlus  = o & indexHere .~ ((otr ++ cycle rng)           `genericIndex` oih)
          | o^.indexOp == IMinus = o & indexHere .~ ((tro ++ cycle (reverse rng)) `genericIndex` oih)
          where rng = i^.indexRange
                otr = dropWhile (/= i^.indexHere) rng
                tro = dropWhile (/= i^.indexHere) $ reverse rng
                iin = i^.indexName
                iih = i^.indexHere
                oin = o^.indexName
                oih = o^.indexHere

-- |

type Parse m a = (TokenParsing m, MonadState GrammarEnv (Unlined m), MonadState GrammarEnv m, MonadPlus m) => m a

-- |

type Stately m a = (TokenParsing m, MonadState GrammarEnv m, MonadPlus m) => m a

-- |

newtype GrammarParser m a = GrammarParser { runGrammarParser :: StateT GrammarEnv m a }
  deriving
  ( Alternative
  , Applicative
  , Functor
  , MonadPlus
  , Monad
  , CharParsing
  , Parsing
  , MonadState GrammarEnv
  )

instance (MonadPlus m, CharParsing m) => TokenParsing (GrammarParser m) where
  someSpace = buildSomeSpaceParser (() <$ space) haskellCommentStyle

deriving instance MonadState GrammarEnv (Unlined (GrammarParser Parser))





{-
data Enumerated
  = Sing
  | ZeroBased Integer
--  | Enum      [String]
  deriving (Show)

-- | The 

data GrammarState = GrammarState
  { _nsys         :: M.Map String Enumerated
  , _tsys         :: S.Set String
  , _esys         :: S.Set String
  , _grammarNames :: S.Set String
  }
  deriving (Show)

instance Default GrammarState where
  def = GrammarState
          { _nsys = def
          , _tsys = def
          , _esys = def
          , _grammarNames = def
          }

makeLenses ''GrammarState

-- | Parse a single grammar.

grammar :: Parse Grammar
grammar = do
  reserveGI "Grammar:"
  _name :: String <- identGI
  _nsyms <- S.fromList . concat <$> many nts
  let _nIsms = S.empty
  _tsyms <- S.fromList . concat <$> many ts
  _epsis <- S.fromList <$> many epsP
  _start <- try (Just <$> startSymbol) <|> pure Nothing
  _rules <- (S.fromList . concat) <$> some rule
  reserveGI "//"
  grammarNames <>= S.singleton _name
  return Grammar { .. }

-- | Start symbol. Only a single symbol may be given
--
-- TODO for indexed symbols make sure we actually have one index to start with.

startSymbol :: Parse Symb
startSymbol = do
  reserveGI "S:"
  name :: String <- identGI
  -- TODO go and allow indexed NTs as start symbols, with one index given
  -- return $ nsym1 name Singular
  return $ Symb Inside [N name Singular]

-- | The non-terminal declaration "NT: ..." returns a list of non-terms as
-- indexed non-terminals are expanded.

nts :: Parse [Symb]
nts = do
  reserveGI "N:"
  name   <- identGI
  enumed <- option Sing $ braces enumeration
  let zs = expandNT name enumed
  nsys <>= M.singleton name enumed
  return zs

-- | expand set of non-terminals based on type of enumerations

expandNT :: String -> Enumerated -> [Symb]
expandNT name = go where
  go Sing          = [Symb Inside [N name Singular]]
  go (ZeroBased k) = [Symb Inside [N name (IntBased   z k)] | z <- [0..(k-1)]]
  --go (Enum es)     = [Symb [N name (Enumerated z es        )] | z <- es        ]

-- | Figure out if we are dealing with indexed (enumerable) non-terminals

enumeration =   ZeroBased <$> natural
--            <|> Enum      <$> sepBy1 identGI (string ",")

-- | Parse declared terminal symbols.

ts :: Parse [Symb]
ts = do
  reserveGI "T:"
  n <- identGI
  let z = Symb Inside [T n]
  tsys <>= S.singleton n
  return [z]

-- | Parse epsilon symbols

epsP :: Parse TN
epsP = do
  reserveGI "E:"
  e <- identGI
  esys <>= S.singleton e
  return E

-- | Parse a single rule. Some rules come attached with an index. In that case,
-- each rule is inflated according to its modulus (or more general the set of
-- indices indicated.
--
-- TODO add @fun@ to each PR

rule :: P m => m [Rule] -- Parse [Rule]
rule = do
  lhs <- runUnlined $ parsePreNN
  reserveGI "->"
  fun :: String <- identGI
  reserveGI "<<<"
  -- rhs <- runUnlined $ some (try (lift $ parsePreNN) <|> (lift $ parsePreTT))
  rhs <- runUnlined $ some (try parsePreNN <|> try parsePreTT <|> parsePreEE)
  whiteSpace
  s <- get
  return $ generateRules s lhs fun rhs

-- | Actually create a rule given both lhs and rhs. This means we need to
-- expand rules according to what we allow.
--
-- TODO need to handle epsilons correctly

generateRules :: GrammarState -> PreSymb -> String -> [PreSymb] -> [Rule]
generateRules gs lhs fun rhs = map buildRules js where
  -- gives (index,NT) list; from (NT,(index,integer)) list
  is = nub . map swap . over (mapped._2) indexName $ (lhs : rhs) ^.. folded.folded._OnlyIndexedPreN
  js = sequence $ map (expandIndex $ gs^.nsys) is
  expandIndex ns (i,n) =
    let expand Sing          = error "expanded index on singular"
        expand (ZeroBased z) = [0 .. (z-1)]
    in  map (i,) . expand $ ns M.! n
  buildTNE _  (PreE s) = E
  buildTNE _  (PreT s) = T s
  buildTNE _  (PreN s NotIndexed) = N s Singular
  buildTNE zs (PreN s (FixedInPreN   k)) =
    let ZeroBased m = (gs^.nsys) M.! s
    in  N s (IntBased k m)
  buildTNE zs (PreN s (IndexedPreN t k)) =
    let Just z = lookup t zs
        ZeroBased m = (gs^.nsys) M.! s
        l :: Integer = (z+k) `mod` m
    in  N s (IntBased l m)
  buildRules j = Rule (Symb Inside $ map (buildTNE j) lhs) [fun] (map (Symb Inside . map (buildTNE j)) rhs)

data IndexedPreN
  = NotIndexed
  | FixedInPreN Integer
  | IndexedPreN String Integer
  deriving (Show,Eq,Ord)

indexName (IndexedPreN s i) = s

_IndexedPreN :: Prism' IndexedPreN (String,Integer)
_IndexedPreN = prism (uncurry IndexedPreN) $ \case (IndexedPreN s i) -> Right (s,i)
                                                   other             -> Left  other

data PreTNE
  = PreN String IndexedPreN
  | PreT String
  | PreE String
  deriving (Show,Eq,Ord)

_PreN :: Prism' PreTNE (String,IndexedPreN)
_PreN = prism (uncurry PreN) $ \case (PreN s i) -> Right (s,i)
                                     other      -> Left  other

_OnlyIndexedPreN :: Prism' PreTNE (String,IndexedPreN)
_OnlyIndexedPreN = prism (uncurry PreN) $ \case (PreN s (IndexedPreN t i)) -> Right (s, IndexedPreN t i)
                                                other                      -> Left  other

_PreT :: Prism' PreTNE String
_PreT = prism PreT $ \case (PreT s) -> Right s
                           other    -> Left  other

_PreE :: Prism' PreTNE String
_PreE = prism PreE $ \case (PreE s) -> Right s
                           other    -> Left  other

type PreSymb = [PreTNE]

--parsePreN :: P m => m PreTNE
parsePreN = lift (use nsys) >>= \ks -> (PreN <$> (choice . map string . M.keys $ ks) <*> parseIndexedPreN)

--parsePreT :: P m => m PreTNE
parsePreT = PreT <$> (lift (use tsys) >>= choice . map string . S.elems)

--parsePreE :: P m => m PreTNE
parsePreE = PreE <$> (lift (use esys) >>= choice . map string . S.elems)

--parseIndexedPreN :: P m => m IndexedPreN
parseIndexedPreN = option NotIndexed (   (try . braces $ IndexedPreN <$> identGI <*> option 0 integer)
                                     <|> (braces $ FixedInPreN <$> integer)
                                     )

-- parsePreNN :: P m => m [PreTNE]
parsePreNN = do
  ns <- (:[]) <$> parsePreN <* whiteSpace <|> listP (try parsePreN <|> parsePreE)
  guard (notNullOf (folded._PreN) ns) <?> "no non-terminal encountered"
  return ns

--parsePreTT :: P m => m [PreTNE]
parsePreTT = do
  ts <- (:[]) <$> parsePreT <* whiteSpace <|> listP (try parsePreT <|> parsePreE)
  guard (notNullOf (folded._PreT) ts) <?> "no terminal encountered"
  return ts

parsePreEE = do
  es <- (:[]) <$> parsePreE <* whiteSpace <|> listP parsePreE
  guard (allOf (folded._PreT) (const True) es) <?> ""
  return es

-- | Parses a list of a la @[a,b,c]@

listP = brackets . commaSep



-- * Monadic Parsing Machinery

-- | Parser with 'GrammarState'

newtype GrammarParser m a = GrammarP { runGrammarP :: StateT GrammarState m a }
  deriving  ( Monad
            , MonadPlus
            , Alternative
            , Applicative
            , Functor
            , MonadState GrammarState
            , TokenParsing
            , CharParsing
            , MonadTrans
            )

deriving instance (Parsing m, MonadPlus m) => Parsing (GrammarParser m) -- Nominal role, ghc 7.8

-- | Functions that parse using the 'GrammarParser'

type Parse  a = ( Monad m
                , MonadPlus m
                , TokenParsing m
                ) => GrammarParser m a

-- | Parsing where we stop at a newline (which needs to be parsed explicitly)

type ParseU a = (Monad m
                , MonadPlus m
                , TokenParsing m
                ) => Unlined (GrammarParser m) a

type P m = ( Monad m
           , MonadPlus m
           , Alternative m
           , Parsing m
           , TokenParsing m
           , MonadState GrammarState m
           )

-- | grammar identifiers

grammarIdentifiers = set styleReserved rs emptyIdents where
  rs = H.fromList ["Grammar:", "N:", "T:", "E:"]

-- | partial binding of 'reserve' to idents

reserveGI = reserve grammarIdentifiers

identGI = ident grammarIdentifiers



parseGrammar :: String -> String -> Result Grammar
parseGrammar fname cnts = parseString
  ((evalStateT . runGrammarP) grammar def)
  (Directed (B.pack fname) 0 0 0 0)
  cnts


--
-- test stuff
--

testGrammar = unlines
  [ "Grammar: Align"
  , "N: X{2}"
  , "N: Y{2}"
  , "N: Z"
  , "T: a"
  , "T: e"
  , "E: ε"
  , "S: X"
  , "[X{i},Y{j}] -> many <<< [X{j+1},Y{i-1}]"
  , "[X{i},Y{i}] -> eeee <<< [e,e]"
  , "[X{1},Y{0}] -> blar <<< [X{0},Y{1}]"
  , "[X{1},Y{0}] -> blub <<< [X{0},Y{i}]"
  , "Z -> step  <<< Z a Z a Z"
--  , "Z -> done  <<< ε" -- this shouldn't actually be done, as @E@ symbols are to denote that nothing happens (so this is actually rather undefined)
--  , "X -> stand <<< X"
--  , "[X] -> oned <<< [X]"
--  , "X -> eps   <<< epsilon"
  , "//"
  ]

testParsing :: Result Grammar
testParsing = parseString
                ((evalStateT . runGrammarP) grammar def)
                (Directed (B.pack "testGrammar") 0 0 0 0)
                testGrammar

asG = let (Success g) = testParsing in g

-}

