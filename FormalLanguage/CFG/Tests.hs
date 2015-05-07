
-- | A simple module for testing the QQ / TH stuff.

module FormalLanguage.CFG.Tests where

import Data.Vector.Fusion.Stream.Monadic
import Data.Array.Repa.Index

import FormalLanguage.CFG.QQ
import FormalLanguage.CFG.Grammar

-- for testing purposes

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Control.Lens
import qualified Data.Set as S


[formalLanguage|
Grammar: Test
N: X
N: Y
T: a
T: b
S: X
X -> term <<< a
X -> two  <<< X a
//
|]
--[X,Y] -> multi <<< [X,Y] [a,b]
--Y -> two2 <<< X Y a X Y b
