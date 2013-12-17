{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A simple module for testing the QQ / TH stuff.

module FormalLanguage.CFG.Tests where

import Data.Vector.Fusion.Stream.Monadic
import Data.Array.Repa.Index

import FormalLanguage.CFG.QQ

-- for testing purposes

import Language.Haskell.TH
import Language.Haskell.TH.Quote


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
