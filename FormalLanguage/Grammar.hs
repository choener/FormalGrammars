{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

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

module FormalLanguage.Grammar where

import           Control.Lens
import           Data.Default
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Foldable
import           Control.Applicative
import qualified Control.Lens.Indexed as Lens
import           Prelude hiding (all)



-- * Basic data types for formal grammars.

-- | Grammar indices are enumerable objects
--
-- TODO should we always assume operations "modulo"?

data Enumerable
  = Singular
  | IntBased Integer Integer -- current index, maximum index
--  | Enumerated String [String]
  deriving (Eq,Ord,Show)

_IntBased :: Prism' Enumerable (Integer,Integer)
_IntBased = prism (uncurry IntBased) $ f where
  f Singular       = Left  Singular
  f (IntBased c m) = Right (c,m)

ibCurrent = _IntBased . _1

ibModulus = _IntBased . _2

instance Default Enumerable where
  def = Singular

-- | A single-dimensional terminal or non-terminal symbol.
--
-- TODO write Eq,Ord by hand. Fail with error if Enumerable is not equal (this
-- should actually be caught in the combination operations).

data TN where
  -- | A terminal symbol (excluding epsilon)
  T :: String               -> TN
  -- | A non-terminal symbol (again, excluding non-terminal epsilons)
  N :: String -> Enumerable -> TN
  -- | Epsilon characters, may be named differently
  E :: String               -> TN

deriving instance Show TN
deriving instance Eq   TN
deriving instance Ord  TN

tnName :: Lens' TN String
tnName f (T s  ) = T               <$> f s
tnName f (N s e) = (\s' -> N s' e) <$> f s
tnName f (E s  ) = E               <$> f s

_T :: Prism' TN String
_T = prism T $ f where
  f (T s) = Right s
  f z     = Left  z

_N :: Prism' TN (String,Enumerable)
_N = prism (uncurry N) $ f where
  f (N s e) = Right (s,e)
  f z       = Left  z

_E :: Prism' TN String
_E = prism E $ f where
  f (E s) = Right s
  f z     = Left  z

enumed = _N . _2

-- | The epsilon symbol (also accepted as part of a non-terminal).
--
-- TODO Still not sure if we should have data TN = E | ... for epsilon.

eps = E ""

-- | A complete grammatical symbol is multi-dimensional with 0..  dimensions.

newtype Symb = Symb { getSymbs :: [TN] }

deriving instance Show Symb
deriving instance Eq   Symb
deriving instance Ord  Symb

symb :: Lens' Symb [TN]
symb f (Symb xs) = Symb <$> f xs  -- are we sure?

type instance Index Symb = Int

type instance IxValue Symb = TN

instance Applicative f => Ixed f Symb where
  ix k f (Symb xs) = Symb <$> ix k f xs
  {-# INLINE ix #-}

-- |

{-
data Fun where
  Fun :: [String] -> Fun
  deriving (Eq,Ord,Show)

funName :: Lens' Fun [String]
funName f (Fun s) = Fun <$> f s
-}

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
  deriving (Eq,Ord,Show)

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
  , _epsis :: Set TN
  , _rules :: Set Rule
  , _start :: Maybe Symb
  , _name  :: String
  } deriving (Show)

makeLenses ''Grammar

-- | the dimension of the grammar. Grammars with no symbols have dimension 0.

gDim :: Grammar -> Int
gDim g
  | Just (x,_) <- S.minView (g^.nsyms) = length $ x^.symb
  | Just (x,_) <- S.minView (g^.tsyms) = length $ x^.symb
  | otherwise                          = 0



-- * Helper functions on rules and symbols.

-- | Symb is completely in terminal form.

tSymb :: Symb -> Bool
tSymb (Symb xs) = allOf folded tTN xs

tTN :: TN -> Bool
tTN (T _  ) = True
tTN (E _  ) = True
tTN (N _ _) = False

-- | Symb is completely in non-terminal form.

nSymb :: Symb -> Bool
nSymb (Symb xs) = allOf folded nTN xs

-- | Generalized non-terminal symbol with at least one non-terminal Symb.

nSymbG :: Symb -> Bool
nSymbG (Symb xs) = anyOf folded nTN xs

nTN :: TN -> Bool
nTN (N _ _) = True
nTN (E _  ) = True
nTN (T _  ) = False



-- * Determine grammar types
--
-- For grammars where the number of non-terminal symbols is restricted, we
-- allow as non-terminal also the generalized variants that have partial
-- terminal symbols.
--
-- TODO maybe restrict those to epsilon-type terminals in generalized
-- non-terminals.

-- | Left-linear grammars have at most one non-terminal on the RHS. It is the
-- first symbol.

isLeftLinear :: Grammar -> Bool
isLeftLinear g = allOf folded isll $ g^.rules where
  isll :: Rule -> Bool
  isll (Rule l _ []) = nSymbG l
  isll (Rule l _ rs) = nSymbG l && (allOf folded (not . nSymbG) $ tail rs) -- at most one non-terminal

-- | Right-linear grammars have at most one non-terminal on the RHS. It is the
-- last symbol.

isRightLinear :: Grammar -> Bool
isRightLinear g = allOf folded isrl $ g^.rules where
  isrl :: Rule -> Bool
  isrl (Rule l _ []) = nSymbG l
  isrl (Rule l _ rs) = nSymbG l && (allOf folded (not . nSymbG) $ init rs)

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
  isC (Rule _ _ [s])   = tSymb s
  isC (Rule _ _ [s,t]) = nSymb s && nSymb t
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
  isG (Rule _ _ (t:ns)) = tSymb t && all nSymb ns
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

{-

-- |

data NTSym where
  TSym :: [String]               -> NTSym
  NSym :: [(String, Enumerable)] -> NTSym
  deriving (Eq,Ord,Show)

-- | Grammar indices are enumerable objects
--
-- TODO should we always assume operations "modulo"?

data Enumerable
  = Singular
  | IntBased Integer [Integer]
  | Enumerated String [String]
  deriving (Eq,Ord,Show)

instance Default Enumerable where
  def = Singular

-- |

data Grammar = Grammar
  { _tsyms       :: Set NTSym
  , _nsyms       :: Set NTSym
  , _productions :: Set Production
  , _start       :: NTSym
  } deriving (Show)

makeLenses ''Grammar

-- | Construct regular grammar.

regular :: Set NTSym -> Set NTSym -> Set Production -> NTSym -> Grammar
regular = error "regular: not implemented"

nsym1 :: String -> Enumerable -> NTSym
nsym1 s e = NSym [(s,e)]

isN (NSym _) = True
isN _ = False

isT (TSym _) = True
isT _ = False

-- | The size of a grammar.

size :: Grammar -> Int
size = error "size"

-- | Transform a grammar into 2NF.

twonf :: Grammar -> Grammar
twonf = error "twonf"

-}

