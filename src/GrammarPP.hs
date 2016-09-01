
-- | This small utility allows us to turn a formal language description into
-- either a LaTeX source file or a Haskell module.

module Main where

import System.Console.CmdArgs
import System.IO (openFile, hClose, IOMode (..))
import Text.PrettyPrint.ANSI.Leijen (hPutDoc)
import Data.Foldable (toList)
import Text.Trifecta.Result (ErrInfo (..))

import FormalLanguage.CFG.Grammar
import FormalLanguage.CFG.Parser
import FormalLanguage.CFG.PrettyPrint.ANSI (printDoc, genGrammarDoc)
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
  o <- cmdArgs $ modes [{- optionLatex, -} optionAnsi]
--  print o
  pr <- case (inFile o) of
          "" -> getContents >>= return . parse
          fn -> readFile fn >>= return . parse
  case pr of
    Failure (ErrInfo f _) -> printDoc f
    Success s -> case o of
--      LaTeX{..} -> case outFile of
--        "" -> error "need to set output file name"
--        fn -> renderFile fn $ renderLaTeX 2 s
      Ansi {..} -> mapM_ (printDoc . genGrammarDoc) $ toList s
--      Haskell{..} -> case outFile of
--        "" -> printDoc $ grammarHaskell s
--        fn -> do h <- openFile fn WriteMode
--                 hPutDoc h $ grammarHaskell s
--                 hClose h

