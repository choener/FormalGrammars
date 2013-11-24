{-# LANGUAGE QuasiQuotes #-}

-- | A simple module for testing the QQ / TH stuff.

module FormalLanguage.Grammar.Tests where

import FormalLanguage.Grammar.QQ



[formalLanguage|
Grammar: Test
N: X
T: a
S: X
X -> term <<< a
//
|]
