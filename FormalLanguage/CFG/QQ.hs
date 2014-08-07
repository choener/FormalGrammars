
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
  { quoteDec = parseFormalLanguage
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
      runIO $ print "TESTING BELOW"
      {-
      gSig <- genSignature g
      gGra <- genGrammar   g
      -}
      zs <- newGen2 g
      runIO $ print "TESTING ABOVE"
      -- TODO build signature and grammar using TH
      return zs -- [gSig,gGra]

-- |

trim ('\n':xs) = trim xs
trim xs = xs

