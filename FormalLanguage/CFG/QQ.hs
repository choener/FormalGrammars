
-- | This QuasiQuoter turns the description of formal grammars into
-- ADPfusion-based code.
--
-- TODO use Quote.quoteFile to be able to read files as well

module FormalLanguage.CFG.QQ where

import Control.Applicative ((<$>),(<*>),empty)
import Control.Lens hiding (outside)
import Control.Monad hiding (mapM)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.ByteString.Char8 (pack)
import Data.Default (def)
import Data.Foldable (concat)
import Data.List (nub)
import Data.List (transpose,sort,group)
import Data.Sequence (Seq)
import Data.Traversable (mapM)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Prelude hiding (mapM,concat)
import qualified Data.Sequence as Seq
import Text.Trifecta.Delta (Delta (Directed))
import Text.Trifecta (parseString,Parser)
import Text.Trifecta.Result (Result (..), ErrInfo (..))

import FormalLanguage.CFG.Grammar
import FormalLanguage.CFG.Outside
import FormalLanguage.CFG.Parser
import FormalLanguage.CFG.PrettyPrint.ANSI
import FormalLanguage.CFG.TH



formalLangFile = quoteFile formalLanguage

-- |

formalLanguage = QuasiQuoter
  { quoteDec  = parseFormalLanguage empty
  , quoteExp  = error "there is only a Dec quoter"
  , quotePat  = error "there is only a Dec quoter"
  , quoteType = error "there is only a Dec quoter"
  }

-- |

parseFormalLanguage ∷ Parse' () -> String -> Q [Dec]
parseFormalLanguage ps s = do
  loc <- location
  let (lpos,cpos) = loc_start loc
  -- let r = parseString ((evalStateT . runGrammarP) grammar def) (Directed (pack "via QQ") (fromIntegral lpos) 0 0 0) $ trim s
  let r = parseString (runP $ evalStateT (parseEverything ps) def) (Directed (pack "via QQ") (fromIntegral lpos) 0 0 0) $ trim s
  case r of
    (Failure (ErrInfo f _)) -> do
      runIO $ printDoc f
      error "aborting parseFormalLanguage"
    (Success g) -> do
      let l = uniquePrefixLength g
--      let gO = outsideFromInside g
--      runIO . printDoc . grammarDoc $ g
--      runIO . printDoc . grammarDoc $ gO
--      thCodeGen g
      -- (++) <$> thCodeGen g <*> thCodeGen gO
      -- TODO here, we should know how many grammars we have and be able to
      -- determine the required prefix to make everything unique in terms
      -- of attribute functions
      concat <$> mapM (thCodeGen l) g

-- |

trim ('\n':xs) = trim xs
trim xs = xs

-- | Determine the length of the unique prefix we need for algebra
-- functions.
--
-- TODO only go over inside grammars! unless the inside grammar for an outside
-- grammar has not been emitted.

uniquePrefixLength :: Seq Grammar -> Int
uniquePrefixLength xs
--  | l == 0    = 0
--  | l == 1    = 0
  | otherwise = go 0 . transpose $ nub [ getName x | x ← xs^..folded ]
  where l = Seq.length xs
        go :: Int -> [String] -> Int
        go acc []       = error $ "for whatever reason, there are two grammars with the same name!" ++ show xs
        go acc (xs:xss) = if (maximum . map length . group $ sort xs) > 1 then go (acc+1) xss else acc
        getName x
          | Outside gI ← x^.outside = gI^.grammarName
          | otherwise               = x^.grammarName

