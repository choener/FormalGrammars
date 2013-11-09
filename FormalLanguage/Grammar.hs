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
  | IntBased Integer [Integer]
  | Enumerated String [String]
  deriving (Eq,Ord,Show)

instance Default Enumerable where
  def = Singular

-- | A single-dimensional terminal or non-terminal symbol.

data TN where
  T :: String               -> TN
  N :: String -> Enumerable -> TN

deriving instance Show TN
deriving instance Eq   TN
deriving instance Ord  TN

symb :: Lens' TN String
symb f (T s  ) = T               <$> f s
symb f (N s e) = (\s' -> N s' e) <$> f s

_T :: Prism' TN String
_T = prism T $ f where
  f   (T s  ) = Right s
  f n@(N _ _) = Left n

_N :: Prism' TN (String,Enumerable)
_N = prism (uncurry N) $ f where
  f t@(T _  ) = Left t
  f   (N s e) = Right (s,e)

enumed = _N . _2

-- | A complete grammatical symbol is multi-dimensional with 0..  dimensions.

newtype Symb = Symb { getSymbs :: [TN] }

deriving instance Show Symb
deriving instance Eq   Symb
deriving instance Ord  Symb

symbol :: Lens' Symb [TN]
symbol f (Symb xs) = Symb <$> f xs  -- are we sure?

type instance Index Symb = Int

type instance IxValue Symb = TN

instance Applicative f => Ixed f Symb where
  ix k f (Symb xs) = Symb <$> ix k f xs
  {-# INLINE ix #-}

-- |

data Fun where
  Fun :: String -> Fun
  deriving (Eq,Ord,Show)

-- | A production rule goes from a left-hand side (lhs) to a right-hand side
-- (rhs). The rhs is evaluated using a function (fun).
--
-- TODO These production rules currently do not allow "typical"
-- context-sensitive grammars with terminal symbols on the left-hand side.

data Rule = Rule
  { _lhs :: Symb
  , _fun :: Fun
  , _rhs :: [Symb]
  }
  deriving (Eq,Ord,Show)

makeLenses ''Rule

-- | A complete grammar with a set of terminal symbol (tsyms), non-terminal
-- symbol (nsyms), production rules (rules) and a start symbol (start).
--
-- TODO Combined terminal and non-terminal symbols in multi-tape grammars are
-- denoted as non-terminal symbols.

data Grammar = Grammar
  { _tsyms :: Set Symb
  , _nsyms :: Set Symb
  , _rules :: Set Rule
  , _start :: Symb
  } deriving (Show)

makeLenses ''Grammar



-- * Helper functions on rules and symbols.

-- | Symb is completely in terminal form.

tSymb :: Symb -> Bool
tSymb (Symb xs) = allOf folded tTN xs

tTN :: TN -> Bool
tTN (T _  ) = True
tTN (N _ _) = False

-- | Symb is completely in non-terminal form.

nSymb :: Symb -> Bool
nSymb (Symb xs) = allOf folded nTN xs

-- | Generalized non-terminal symbol with at least one non-terminal Symb.

nSymbG :: Symb -> Bool
nSymbG (Symb xs) = anyOf folded nTN xs

nTN :: TN -> Bool
nTN (T _  ) = False
nTN (N _ _) = True



-- * Different normal forms for grammars.

-- | Transform a grammar into CNF.
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

epsilonFree :: Grammar -> Bool
epsilonFree g = allOf folded eFree $ g^.rules where
  eFree :: Rule -> Bool
  eFree (Rule l _ r) = l == g^.start || anyOf folded (xSymbG $ g^.start) r -- TODO need nSymbG that excludes the start symbol during considerations

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

