
-- | This QuasiQuoter turns the description of formal grammars into
-- ADPfusion-based code.
--
-- TODO use Quote.quoteFile to be able to read files as well

module FormalLanguage.CFG.QQ where

import Control.Monad
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.ByteString.Char8 (pack)
import Data.Default (def)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Trifecta.Delta (Delta (Directed))
import Text.Trifecta (parseString)
import Text.Trifecta.Result (Result (..))

import FormalLanguage.CFG.Grammar
import FormalLanguage.CFG.PrettyPrint.ANSI
import FormalLanguage.CFG.TH
import FormalLanguage.CFG.Parser



formalLangFile = quoteFile formalLanguage

-- |

formalLanguage = QuasiQuoter
  { quoteDec  = parseFormalLanguage
  , quoteExp  = error "there is only a Dec quoter"
  , quotePat  = error "there is only a Dec quoter"
  , quoteType = error "there is only a Dec quoter"
  }

-- |

parseFormalLanguage :: String -> Q [Dec]
parseFormalLanguage s = do
  loc <- location
  let (lpos,cpos) = loc_start loc
  let r = parseString ((evalStateT . runGrammarP) grammar def) (Directed (pack "via QQ") (fromIntegral lpos) 0 0 0) $ trim s
  case r of
    (Failure f) -> do
      runIO . printDoc $ f
      error "aborting parseFormalLanguage"
    (Success g) -> do
      runIO . printDoc . grammarDoc $ g
      thCodeGen g

-- |

trim ('\n':xs) = trim xs
trim xs = xs

