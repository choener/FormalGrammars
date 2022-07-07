
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
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict hiding (get)
import           Data.ByteString.Char8 (pack)
import           Data.Char (isSpace)
import           Data.Data.Lens
import           Data.Default
import           Data.List (nub,genericIndex,mapAccumL)
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Monoid
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
import           Text.Trifecta.Delta (Delta (Directed))
import qualified Control.Effect.State as CE

import           FormalLanguage.CFG.Grammar
import           FormalLanguage.CFG.Outside
import           FormalLanguage.CFG.PrettyPrint.ANSI



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


test :: IO ()
test = do
  Just p <- parseFromFile (runP $ evalStateT (parseEverything empty) def{_verbose = True}) "./tests/pseudo.gra"
  mapM_ print p


parse = parseString (runP $ evalStateT (parseEverything empty) def) (Directed (pack "via QQ") (fromIntegral 0) 0 0 0)

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
                        , "N:", "Y:", "T:", "S:", "->", "=", "<<<", "-", "e", "ε", "l", "λ"
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

-- | Parses a syntactic (or non-terminal) symbol (for the corresponding
-- index type). Cf. 'parseSynTermDecl'.

parseSyntacticDecl :: EvalReq -> Parse m SynTermEps
parseSyntacticDecl e = do
  reserve fgIdents "N:"
  try split <|> normal
  where split = angles (flip (set splitN) <$> normal <* string "," <*> integer)
        normal = SynVar <$> (ident fgIdents <?> "syntactic variable name") <*> (option [] $ parseIndex e) <*> pure 1 <*> pure 0

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

-- | This function parses multidimensional syntactic variables. We only allow angled brackets from
-- now on, or a single syntactic variable without brackets around it.

knownSynVar :: EvalReq -> Stately m Symbol
knownSynVar e = SynLike <$> do
  ((:[]) <$> sv) <|> angles (commaSep sv)
  where sv = flip (<?>) "known syntactic variable" . try $ do
               s <- ident fgIdents
               l <- use (current . synvars . at s)
               r <- use (current . synvars)
               case l of
                Nothing -> unexpected $ "Unknown SynVar! " ++ show (s,r)
                Just (SynVar s' i' n' _) ->
                  do i <- option [] $ parseIndex e
                     return $ SynVar s i n' 0

-- |

knownSynTerm :: EvalReq -> Stately m Symbol
knownSynTerm e = SynLike <$> do
  ((:[]) <$> sv) <|> (angles $ commaSep sv)
  where sv = flip (<?>) "known syntactic terminal" . try $ do
               s <- ident fgIdents
               use (current . synterms . at s) >>= guard . isJust
               i <- option [] $ parseIndex e
               return $ SynVar s i 0 0

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

-- |

knownTermVar :: EvalReq -> Stately m Symbol
knownTermVar e = TermLike <$> do
  ((:[]) <$> (eps <|> tv)) <|> (brackets $ commaSep (del <|> eps <|> loc <|> tv))
  where tv = flip (<?>) "known terminal variable" . try $ do
               i <- ident fgIdents
               t <- use (current . termvars . at i)
               s <- use (current . synvars  . at i)
               guard . isJust $ t <|> s
               -- TODO this will produce bad @SynVar@ for indexed cases
               -- (and probably for split cases, but these are even more
               -- weird)
               return $ if isJust t then Term i [] else SynVar i [] 1 0
        del = Deletion <$ reserve fgIdents "-"
        eps = Epsilon Global  <$ (reserve fgIdents "e" <|> reserve fgIdents "ε")
        loc = Epsilon Local   <$ (reserve fgIdents "l" <|> reserve fgIdents "λ")

-- | Parses an already known symbol, either syntactic or terminal.
--
--TODO Correctly parse inside-syntactics in outside grammars? Do we want
--this explicitly?

knownSymbol :: EvalReq -> Stately m Symbol
knownSymbol e = try (knownSynVar e) <|> try (knownSynTerm e) <|> knownTermVar e

-- |

parseRule :: (Applicative m, Monad m, TokenParsing m, MonadState GrammarEnv m, MonadPlus m) => m [Rule]
parseRule = (expandIndexed =<< runUnlined rule) <* someSpace
  where rule  = Rule
              <$> knownSynVar EvalRule
              <*  reserve fgIdents "->"
              <*> afun
              <*  string "<<<" <* spaces
              <*> (updateSplitCounts <$> some syms)
        afun = (:[]) <$> ident fgIdents
        syms = knownSymbol EvalRule

-- | For split syntactic variables used in split manner
-- (i.e. @S -> X Y X Y)
--
-- TODO error control!

updateSplitCounts :: [Symbol] -> [Symbol]
updateSplitCounts = snd . mapAccumL go M.empty where
  go m (SynLike [SynVar s i n k])
    | n > 1                      = let o = M.findWithDefault 0 (s,i) m + 1
                                   in  (M.insert (s,i) o m, SynLike [SynVar s i n o])
  go m s                         = (m,s)

-- | Once we have parsed a rule, we still need to extract all active
-- indices in the rule, and enumerate over them. This will finally generate
-- the set of rules we are interested in.

expandIndexed :: forall m . (TokenParsing m, MonadState GrammarEnv m, MonadPlus m) => Rule -> m [Rule]
expandIndexed r = do
  -- active index names
  let is :: [IndexName] = nub $ r ^.. biplate . indexName
  -- corresponding @Index@es
  js :: [Index] <- catMaybes <$> mapM (\i -> use (current . indices . at i)) is
  if null js
    then return [r]
    else mapM go $ sequence $ map expand js
  where -- updates the indices in the rules accordingly
        go :: [Index] -> m Rule
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

type Parse m a = (TokenParsing m, MonadState GrammarEnv m, MonadPlus m) => m a
type Parse' a = StateT GrammarEnv P a

-- |

type Stately m a = (TokenParsing m, MonadState GrammarEnv m, MonadPlus m) => m a

-- | This newtype wrapper around 'Parser' is only used so that we can have @TokenParsing P@ with
-- Haskell-style comments.

newtype P a = P { runP :: Parser a }
  deriving newtype ( Functor, Applicative, Monad, MonadPlus, Alternative
                   , Parsing, CharParsing, DeltaParsing
                   )

-- | This enables the haskell-style comments.

instance TokenParsing P where
  someSpace = buildSomeSpaceParser
    (skipSome (satisfy isSpace))
    haskellCommentStyle

