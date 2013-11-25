{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A simple module for testing the QQ / TH stuff.

module FormalLanguage.Grammar.Tests where

import FormalLanguage.Grammar.QQ

-- for testing purposes

import Language.Haskell.TH
import Language.Haskell.TH.Quote


[formalLanguage|
Grammar: Test
N: X
T: a
S: X
X -> term <<< a
//
|]
