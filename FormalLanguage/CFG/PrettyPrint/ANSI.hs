{-# LANGUAGE PatternGuards #-}

module FormalLanguage.CFG.PrettyPrint.ANSI
  ( grammarDoc
  , rulesDoc
  , printDoc
  ) where

import           Control.Lens
import qualified Data.Set as S
import           System.IO (stdout)
import           Text.PrettyPrint.ANSI.Leijen

import FormalLanguage.CFG.Grammar
import FormalLanguage.CFG.Parser



-- | Prettyprint a grammar ANSI-style.
--
-- TODO Later on, it would be really nice to better align the LHS, fun, and RHS
-- of the rules

grammarDoc :: Grammar -> Doc
grammarDoc g = text "Grammar: " <+> (text $ g^.name) <$> indent 2 (ns <$> ts <$> es <$> ss <$> rs) <$> line where
  ns = ind "non terminals:" 2 . vcat $ zipWith (\k z -> (fill 5 $ int k) <+> (symbolDoc z <+> (text . show $ z))) [1..] (g^..nsyms.folded)
  ts = ind "terminals:" 2 . vcat . map (\z -> symbolDoc z <+> (text . show $ z)) $ g^..tsyms.folded
  es = ind "epsilons:" 2 . vcat . map (\z -> tnDoc z <+> (text . show $ z)) $ g^..epsis.folded
  ss = ind "start symbol:" 2 . startDoc $ g^.start
  rs = ind "rules:" 2 . vcat $ zipWith (\k r -> (fill 5 $ int k) <+> (ruleDoc r)) [1..] (g^..rules.folded)
  ind s k d = text s <$> indent k d

-- | Print just a set of rules (for the GrammarProducts Proofs).

rulesDoc :: S.Set Rule -> Doc
rulesDoc rs = text "rules:" <$> (indent 2 . vcat . map ruleDoc $ rs^..folded) <$> line

-- | Prettify the start symbol, or give warning.

startDoc :: Maybe Symb -> Doc
startDoc Nothing = red $ text "no start symbol is set!"
startDoc (Just s) = symbolDoc s

-- | Render a rule.

ruleDoc :: Rule -> Doc
ruleDoc r = fill 10 l <+> text "->" <+> fill 10 f <+> rs where
  l = symbolDoc $ r^.lhs
  f = case r^.fun of
        []  -> text "MISSING!"
        [z] -> text z
        xs  -> list . map text $ xs
  rs = hcat $ punctuate space $ map symbolDoc $ r^.rhs

-- | A symbol is rendered either as a ``symbol'' or a list of symbols for
-- multi-tape grammars.

symbolDoc :: Symb -> Doc
symbolDoc s
  | [z] <- s^.symb = tnDoc z
  | otherwise      = list $ map tnDoc $ s^.symb

-- | Prettyprint a (non-)terminal symbol.

tnDoc :: TN -> Doc
tnDoc (E    ) = blue  $ text "ε"
tnDoc (T s  ) = green $ text s
tnDoc (N s e)
  | Singular <- e = red $ text s
  | IntBased k z <- e = (red $ text s) <+> (magenta $ text $ show k)

-- |

printDoc :: Doc -> IO ()
printDoc d = displayIO stdout (renderPretty 0.8 160 $ d <> linebreak)

-- Print the test grammar from the parser.

test = printDoc $ grammarDoc asG
