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

data Symb where
  T  :: String               -> Symb
  NT :: String -> Enumerable -> Symb

deriving instance Show Symb
deriving instance Eq   Symb

symb :: Lens' Symb String
symb f (T  s  ) = T                <$> f s
symb f (NT s e) = (\s' -> NT s' e) <$> f s

enumed :: Lens' Symb Enumerable
enumed f (T  _  ) = error "enumed doesn't exist for terminal symbols"
enumed f (NT s e) = NT s <$> f e

-- | A complete grammatical symbol is multi-dimensional with 0..  dimensions.

newtype Symbol = Symbol { getSymbs :: [Symb] }

deriving instance Show Symbol
deriving instance Eq   Symbol

symbol :: Lens' Symbol [Symb]
symbol f (Symbol xs) = Symbol <$> f xs  -- are we sure?

type instance Index Symbol = Int

type instance IxValue Symbol = Symb

instance Applicative f => Ixed f Symbol where
  ix k f (Symbol xs) = Symbol <$> ix k f xs

-- |

data Rule = Rule
  { _lhs :: Symbol
  , _fun :: ()
  , _rhs :: [Symbol]
  } deriving (Eq,Show)

makeLenses ''Rule

-- |

data Grammar = Grammar
  { _tsyms       :: Set Symbol
  , _nsyms       :: Set Symbol
  , _productions :: Set Rule
  , _start       :: Symbol
  } deriving (Show)

makeLenses ''Grammar

{-

-- |

data Production where
  Production :: NTSym -> EvalFun -> [NTSym] -> Production
  deriving (Eq,Ord,Show)

-- |

data EvalFun where
  EvalFun :: String -> EvalFun
  deriving (Eq,Ord,Show)

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

-- | Transform a grammar into CNF.
--
-- TODO make sure we use a variant that computes small grammars.

chomskyNF :: Grammar -> Grammar
chomskyNF = error "chomsky"

isChomskyNF :: Grammar -> Bool
isChomskyNF g = allOf folded isC $ g^.productions where
  isC :: Production -> Bool
  isC (Production _ _ [NSym _, NSym _]) = True
  isC (Production _ _ [TSym _]        ) = True
  isC _                                 = False

-- | Transform grammar into GNF.
--
-- http://dl.acm.org/citation.cfm?id=321254

greibachNF :: Grammar -> Grammar
greibachNF = error "gnf"

-- | Check if grammar is in Greibach Normal Form

isGreibachNF :: Grammar -> Bool
isGreibachNF g = allOf folded isG $ g^.productions where
  isG :: Production -> Bool
  isG (Production _ _ [TSym _, NSym _]) = True
  isG _                                 = False

-- | Transform a grammar into 2NF.

twonf :: Grammar -> Grammar
twonf = error "twonf"

-}

