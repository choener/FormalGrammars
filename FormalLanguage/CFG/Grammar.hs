
-- | The basic data types for formal languages up to and including context-free
-- grammars.
--
-- TODO we shall have to extend the system for multi-tape grammars to allow
-- combined terminal/non-terminal systems. This will basically mean dealing
-- with context-sensitive grammars, at which we can just fully generalize
-- everything.
--
-- TODO we need a general system to generate fresh variable names of varying
-- dimension. This is very much desired for certain operations (especially on
-- products).
--
-- BIGTODO @E _@ are actually the "None" thing in ADPfusion; while normal
-- epsilons are just terminals.

module FormalLanguage.CFG.Grammar
  ( module FormalLanguage.CFG.Grammar.Types
  , module FormalLanguage.CFG.Grammar.Util
  ) where

import FormalLanguage.CFG.Grammar.Types
import FormalLanguage.CFG.Grammar.Util

{-
import           Data.List (partition,sort,nub)
import           Data.Monoid
-}


{-

-- | A grammar with normalized start and epsilon symbols (i) has a start
-- symbol, whose RHSs only point to single syntactic variables. (ii) It has
-- terminating rules only when the single RHS symbol is a real epsilon symbol.

normalizeStartEpsilon :: Grammar -> Grammar
normalizeStartEpsilon g = gE
        -- good start rules go from the start symbol to a single syntactic
        -- symbol. It should not be the start symbol.
  where srs = [r | r<-g^..rules.folded , r^.lhs == g^.start]
        (gsr,bsr) = partition goodStartRule srs
        goodStartRule (Rule _ _ [r]) | isSyntactic r && r /= g^.start = True
        goodStartRule _                                               = False
        -- now we need to process the start rules. We create a fresh synvar,
        -- and the corresponding rules.
        d = dim g
        s = freshStartSymbol g
        sf = freshStartFun g
        srs' = [Rule s (replicate d sf) [r^.lhs] | r<-srs]
        gS = if null bsr
             then g
             else (g & rules %~ S.union (S.fromList srs')) & start .~ s -- otherwise, add new rules, set new start symbol
        -- good epsilon rules go from a syntactic variable directly to epsilon
        -- with no additional symbols on the RHS.
        ers = [r | r<-gS^..rules.folded, any isEpsilon (r^.rhs)]
        (ger,ber) = partition goodEpsilonRule ers
        goodEpsilonRule (Rule _ _ [r]) | isEpsilon r = True
        goodEpsilonRule _                            = False
        -- same for the epsilon rules
        e = freshTermSynVar gS
        ef = freshTermFun gS
        ers' = concat [ [ Rule e (replicate d ef) [r]
                        , Rule l f (rsl++[e]++rsr) ]
                      | (Rule l f rs') <- ers
                      , let (rsl,(r:rsr)) = span (not . isEpsilon) rs'
                      ]
        gE = if null ber
             then gS
             else (gS & rules %~ (S.\\ S.fromList ers)) & rules %~ S.union (S.fromList ers') -- otherwise, replace old rules

-- | Given a grammar, generate a fresh start syntactic variable with a name
-- that is not "too weird". Also transfers any index structure to the start
-- symbol.
--
-- TODO if indices are correctly transferred needs be closely checked.

freshStartSymbol :: Grammar -> Symbol
freshStartSymbol g
  | d == 0    = error "zero-dim grammar"
  | otherwise = zipWith SynVar (replicate d x) ix
  where ss = ["S"] ++ (map (++"'") $ g^..start.folded.name) ++ map (\i -> "S" ++ show i) [1 :: Int ..]
        x  = head $ dropWhile (`M.member` (g^.synvars)) ss
        d  = dim g
        ix :: [[Index]]
        ix = g^..start.folded.index

-- | Given a grammar, generate a fresh terminating syntactic variable (that
-- only traverses to @Epsilon@ rules), that is not "too weird".

freshTermSynVar :: Grammar -> Symbol
freshTermSynVar g
  | d == 0  = error "zero-dim grammar"
  | otherwise = replicate d (SynVar e [])
  where es = ["E"] ++ map (\i -> "E" ++ show i) [1 :: Int .. ]
        e  = head $ dropWhile (`M.member` (g^.synvars)) es
        d  = dim g

-- | Create a fresh start function symbol.

freshStartFun :: Grammar -> String
freshStartFun g
  | S.null ks = error "no rules in grammar?"
  | otherwise = f
  where ks = S.fromList $ g^..rules.folded.attr.folded
        fs = ["fS"] ++ map (\i -> "fS" ++ show i) [1::Int ..]
        f  = head $ dropWhile (`S.member` ks) fs

-- | Create a fresh terminating transition function symbol

freshTermFun :: Grammar -> String
freshTermFun g
  | S.null ks = error "no rules in grammar?"
  | otherwise = f
  where ks = S.fromList $ g^..rules.folded.attr.folded
        fs = ["fE"] ++ map (\i -> "fE" ++ show i) [1::Int ..]
        f  = head $ dropWhile (`S.member` ks) fs

-- | Collect all terminal symbols from the rules (for cfg's it's not really
-- needed to include the lhs).

grammarTerminals :: Grammar -> [Symbol]
grammarTerminals g = nub . sort . filter isTerminal $ (g^..rules.folded.lhs) ++ (g^..rules.folded.rhs.folded)

-- | Collect all non-terminal symbols from the rules.
--
-- TODO WARNING: Problems handling syn-terms in outside grammars.

grammarSynVars :: Grammar -> [Symbol]
grammarSynVars g = nub . sort . filter isSyntactic $ (g^..rules.folded.lhs) ++ (g^..rules.folded.rhs.folded)

-}

































{-

-- * Basic data types for formal grammars.

-- | Grammar indices are enumerable objects
--
-- TODO should we always assume operations "modulo"?

data Enumerable
  = Singular
  | IntBased Integer Integer -- current index, maximum index
--  | Enumerated String [String]
  deriving (Eq,Ord,Show,Typeable,Data)

_IntBased :: Prism' Enumerable (Integer,Integer)
_IntBased = prism (uncurry IntBased) $ f where
  f Singular       = Left  Singular
  f (IntBased c m) = Right (c,m)

ibCurrent = _IntBased . _1

ibModulus = _IntBased . _2

instance Default Enumerable where
  def = Singular

-- | A single-dimensional terminal or non-terminal symbol. @E@ is a special
-- symbol denoting that nothing should be done.
--
-- TODO write Eq,Ord by hand. Fail with error if Enumerable is not equal (this
-- should actually be caught in the combination operations).

-- | 'T' – A terminal symbol (excluding epsilon)
--
-- 'N' – A non-terminal symbol (again, excluding non-terminal epsilons)
--
-- 'E' – Epsilon characters, may be named differently

data TN where
  T :: String               -> TN
  N :: String -> Enumerable -> TN
  E ::                         TN

deriving instance Show     TN
deriving instance Eq       TN
deriving instance Ord      TN
deriving instance Typeable TN
deriving instance Data     TN

isT = \case {T _   -> True; _ -> False}
isN = \case {N _ _ -> True; _ -> False}
isE = \case {E     -> True; _ -> False}

tnName :: Lens' TN String
tnName f (T s  ) = T               <$> f s
tnName f (N s e) = (\s' -> N s' e) <$> f s
tnName f (E    ) = (const E)       <$> f "ε"

_T :: Prism' TN String
_T = prism T $ f where
  f (T s) = Right s
  f z     = Left  z

_N :: Prism' TN (String,Enumerable)
_N = prism (uncurry N) $ f where
  f (N s e) = Right (s,e)
  f z       = Left  z

_E :: Prism' TN ()
_E = prism (const E) $ f where
  f E = Right ()
  f z = Left  z

enumed = _N . _2

-- | Is a symbol of @Outside@ or @Inside@ type?

data InsideOutside = Inside | Outside
  deriving (Show,Eq,Ord,Typeable,Data)

-- | A complete grammatical symbol is multi-dimensional with 0..
-- dimensions.
--
-- TODO we should expand this to three cases: (i) only terminals, (ii) only
-- syntactic variables, (iii) mixed cases

data Symb = Symb
  { inOut    :: InsideOutside
  , getSymbs :: [TN]
  }

deriving instance Show     Symb
deriving instance Eq       Symb
deriving instance Ord      Symb
deriving instance Typeable Symb
deriving instance Data     Symb

symb :: Lens' Symb [TN]
symb f (Symb io xs) = Symb io <$> f xs -- are we sure?

symbInOut :: Lens' Symb InsideOutside
symbInOut f (Symb io xs) = (`Symb` xs) <$> f io

sDim :: Symb -> Int
sDim = length . getSymbs

type instance Index Symb = Int

type instance IxValue Symb = TN

instance Ixed Symb where
  ix k f (Symb io xs) = Symb io <$> ix k f xs
  {-# INLINE ix #-}

-- | A production rule goes from a left-hand side (lhs) to a right-hand side
-- (rhs). The rhs is evaluated using a function (fun).
--
-- TODO These production rules currently do not allow "typical"
-- context-sensitive grammars with terminal symbols on the left-hand side.

data Rule = Rule
  { _lhs :: Symb
  , _fun :: [String] -- Fun
  , _rhs :: [Symb]
  }
  deriving (Eq,Ord,Show,Typeable,Data)

makeLenses ''Rule

-- | A complete grammar with a set of terminal symbol (tsyms), non-terminal
-- symbol (nsyms), production rules (rules) and a start symbol (start).
--
-- TODO Combined terminal and non-terminal symbols in multi-tape grammars are
-- denoted as non-terminal symbols.
--
-- TODO rename epsis -> esyms
--
-- TODO tsyms, esyms are not symbols but should just be 1-dim things ...
--
-- TODO nsyms should maybe be 1-dim, then have another thing for the multidim things

data Grammar = Grammar
  { _tsyms :: Set Symb
  , _nsyms :: Set Symb
  , _nIsms :: Set Symb      -- in case of an outside grammar, this contains the inside syntactic variables that now act as "kind-of" terminals
  , _epsis :: Set TN
  , _rules :: Set Rule
  , _start :: Maybe Symb
  , _name  :: String
  } deriving (Show,Data,Typeable)

makeLenses ''Grammar

-- | Determines if this is an outside grammar

isOutsideGrammar :: Grammar -> Bool
isOutsideGrammar g = anyOf folded (\(Symb io _) -> io==Outside) $ g^.nsyms

-- | the dimension of the grammar. Grammars with no symbols have dimension 0.

gDim :: Grammar -> Int
gDim g
  | Just (x,_) <- S.minView (g^.nsyms) = length $ x^.symb
  | Just (x,_) <- S.minView (g^.tsyms) = length $ x^.symb
  | otherwise                          = 0

-- | Helper function giving the grammar name. Will add an @Outside@ prefix,
-- where necessary.

grammarName :: Grammar -> String
grammarName g = if isOutsideGrammar g then "Outside" ++ g^.name else "" ++ g^.name

-- * Helper functions on rules and symbols.

-- | Symb is completely in terminal form.

isSymbT :: Symb -> Bool
isSymbT (Symb io xs) = allOf folded tTN xs && anyOf folded (\case (T _) -> True ; _ -> False) xs

tTN :: TN -> Bool
tTN (T _  ) = True
tTN (E    ) = True
tTN (N _ _) = False

isSymbE :: Symb -> Bool
isSymbE (Symb io xs) = allOf folded (\case E -> True ; _ -> False) xs

-- | Symb is completely in non-terminal form.

isSymbN :: Symb -> Bool
isSymbN (Symb io xs) = allOf folded nTN xs && anyOf folded (\case (N _ _) -> True ; _ -> False) xs

{-
-- | Generalized non-terminal symbol with at least one non-terminal Symb.

nSymbG :: Symb -> Bool
nSymbG (Symb xs) = allOf folded nTN xs && anyOf folded (\case (N _ _) -> True ; _ -> False) xs
-}

nTN :: TN -> Bool
nTN (N _ _) = True
nTN (E    ) = True
nTN (T _  ) = False



-- * Determine grammar types
--
-- For grammars where the number of non-terminal symbols is restricted, we
-- allow as non-terminal also the generalized variants that have partial
-- terminal symbols.
--
-- TODO maybe restrict those to epsilon-type terminals in generalized
-- non-terminals.

-- | Right-linear grammars have at most one non-terminal on the RHS. It is the
-- last symbol.

isRightLinear :: Grammar -> Bool
isRightLinear g = allOf folded isrl $ g^.rules where
  isrl :: Rule -> Bool
  isrl (Rule l _ []) = isSymbN l
  isrl (Rule l _ rs) = isSymbN l && (allOf folded (not . isSymbN) $ init rs)

-- | Linear grammars just have a single non-terminal on the right-hand side.

isLinear :: Grammar -> Bool
isLinear g = error "isLinear: write me" -- allOf folded ((<=1) . length . filter nSymbG


-- * Different normal forms for grammars.

-- | Transform a grammar into CNF. (cf. COL-2007)
--
-- TODO make sure we use a variant that computes small grammars.

chomskyNF :: Grammar -> Grammar
chomskyNF = error "chomsky"

isChomskyNF :: Grammar -> Bool
isChomskyNF g = allOf folded isC $ g^.rules where
  isC :: Rule -> Bool
  isC (Rule _ _ [s])   = isSymbT s
  isC (Rule _ _ [s,t]) = isSymbN s && isSymbN t
  isC _                = False

-- | Transform grammar into GNF.
--
-- http://dl.acm.org/citation.cfm?id=321254

greibachNF :: Grammar -> Grammar
greibachNF = error "gnf"

-- | Check if grammar is in Greibach Normal Form

isGreibachNF :: Grammar -> Bool
isGreibachNF g = allOf folded isG $ g^.rules where
  isG :: Rule -> Bool
  isG (Rule _ _ (t:ns)) = isSymbT t && all isSymbN ns
  isG _                 = False

-- | A grammar is epsilon-free if no rule has an empty RHS, resp. any rhs-symb
-- is completely non-empty.
--
-- TODO we should tape-split multi-tape grammars here and make sure that all
-- individual tapes are epsilon-free as otherwise we can generate cases where
-- the epsilon-aligned symbols lead to weird problems. So split into single
-- tapes, then check.

epsilonFree :: Grammar -> Bool
epsilonFree g = allOf folded eFree $ g^.rules where
  eFree :: Rule -> Bool
  eFree (Rule l _ r) = undefined -- l == g^.start || (not $ null r) || anyOf folded (epsFree $ g^.start) r
  epsFree :: Symb -> Symb -> Bool
  epsFree = undefined

-- | Collect all non-terminal symbols from the rules

collectSymbN :: Grammar -> [Symb]
collectSymbN g = nub . sort . filter isSymbN $ (g^..rules.folded.lhs) ++ (g^..rules.folded.rhs.folded)

-- | Collect the syntactic variable symbols for either inside or outside,
-- depending on the grammar

collectInOutSymbN :: Grammar -> [Symb]
collectInOutSymbN g = filter f xs where
  xs = collectSymbN g
  f (Symb Outside _) = isO
  f (Symb Inside  _) = not isO
  isO = isOutsideGrammar g

-- | Collect all terminal symbols from the rules (for cfg's it's not really
-- needed to include the lhs).

collectSymbT :: Grammar -> [Symb]
collectSymbT g = nub . sort . filter isSymbT $ (g^..rules.folded.lhs) ++ (g^..rules.folded.rhs.folded)

-}

