{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-- | This small utility allows us to turn a formal language description into
-- either a LaTeX source file or a Haskell module.

module Main where

import System.Console.CmdArgs
import System.IO (openFile, hClose, IOMode (..))
import Text.PrettyPrint.ANSI.Leijen (hPutDoc)

import FormalLanguage.CFG.Grammar
import FormalLanguage.CFG.Parser
import FormalLanguage.CFG.PrettyPrint.ANSI (printDoc, grammarDoc)
import FormalLanguage.CFG.PrettyPrint.Haskell (grammarHaskell)
import FormalLanguage.CFG.PrettyPrint.LaTeX (renderFile, renderLaTeX)



data Options
  = LaTeX
    { inFile :: String
    , outFile ::String
    }
  | Ansi
    { inFile :: String
    }
  | Haskell
    { inFile :: String
    , outFile :: String
    }
  deriving (Show,Data,Typeable)

optionLatex = LaTeX
  { inFile = ""
  , outFile = ""
  }

optionAnsi = Ansi
  { inFile = ""
  }

optionHaskell = Haskell
  { inFile = ""
  , outFile = ""
  }

main = do
  o <- cmdArgs $ modes [optionLatex,optionAnsi]
  print o
  pr <- case (inFile o) of
          "" -> getContents >>= return . parseGrammar "stdin"
          fn -> readFile fn >>= return . parseGrammar fn
  case pr of
    Failure f -> printDoc f
    Success s -> case o of
      LaTeX{..} -> case outFile of
        "" -> error "need to set output file name"
        fn -> renderFile fn $ renderLaTeX 2 s
      Ansi {..} -> printDoc $ grammarDoc s
      Haskell{..} -> case outFile of
        "" -> printDoc $ grammarHaskell s
        fn -> do h <- openFile fn WriteMode
                 hPutDoc h $ grammarHaskell s
                 hClose h

