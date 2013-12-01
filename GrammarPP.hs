{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This small utility allows us to turn a formal language description into
-- either a LaTeX source file or a Haskell module.

module Main where

import System.Console.CmdArgs

import FormalLanguage.Parser
import FormalLanguage.Grammar
import FormalLanguage.Grammar.PrettyPrint.ANSI (printDoc, grammarDoc)
import FormalLanguage.Grammar.PrettyPrint.LaTeX (renderFile, renderLaTeX)



data Options
  = LaTeX
    { inFile :: String
    , outFile ::String
    }
  | Ansi
    { inFile :: String
    }
  deriving (Show,Data,Typeable)

optionLatex = LaTeX
  { inFile = ""
  , outFile = ""
  }

optionAnsi = Ansi
  { inFile = ""
  }

main = do
  o <- cmdArgs $ modes [optionLatex,optionAnsi]
  print o
  pr <- case (inFile o) of
          "" -> getContents >>= return . parseGrammar "stdin"
          fn -> readFile fn >>= return . parseGrammar fn
  case pr of
    (Failure f) -> printDoc f
    (Success s) -> case o of
      (LaTeX{..}) -> case outFile of
        "" -> error "need to set output file name"
        fn -> renderFile fn $ renderLaTeX 2 s
      (Ansi {..}) -> printDoc $ grammarDoc s

