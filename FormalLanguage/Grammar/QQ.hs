
-- | This QuasiQuoter turns the description of formal grammars into
-- ADPfusion-based code.

module FormalLanguage.Grammar.QQ where

import Control.Monad
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.ByteString.Char8 (pack)
import Data.Default (def)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Trifecta.Delta (Delta (Directed))
import Text.Trifecta (parseString)
import Text.Trifecta.Result (Result (..))

import FormalLanguage.Grammar
import FormalLanguage.Grammar.PrettyPrint.ANSI
import FormalLanguage.Parser



formalLanguage = QuasiQuoter
  { quoteDec = parseFormalLanguage
  }

parseFormalLanguage :: String -> Q [Dec]
parseFormalLanguage s = do
  loc <- location
  let (lpos,cpos) = loc_start loc
  let r = parseString ((evalStateT . runGrammarP) grammar def) (Directed (pack "via QQ") (fromIntegral lpos) 0 0 0) $ trim s
  case r of
    (Failure f) -> do
      runIO . printDoc $ f
      error "aborting parseFormalLanguage"
    (Success p) -> do
      runIO . printDoc . grammarDoc $ p
      -- TODO build signature and grammar using TH
      return []

trim ('\n':xs) = trim xs
trim xs = xs
