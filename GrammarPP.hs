{-# LANGUAGE DeriveDataTypeable #-}

-- | This small utility allows us to turn a formal language description into
-- either a LaTeX source file or a Haskell module.

module Main where

import System.Console.CmdArgs



data Options
  = LaTeX
    {
    }
  | Ansi
    {
    }
  deriving (Show,Data,Typeable)

optionLatex = LaTeX
  {
  }

optionAnsi = Ansi

main = do
  o <- cmdArgs $ modes [optionLatex,optionAnsi]
  print o
  return undefined
