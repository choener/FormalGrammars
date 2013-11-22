{-# LANGUAGE PatternGuards #-}

module FormalLanguage.Grammar.PrettyPrint.ANSI where

import Text.PrettyPrint.ANSI.Leijen
import Control.Lens

import FormalLanguage.Grammar
import FormalLanguage.Parser



-- |
--
-- TODO Later on, it would be really nice to better align the LHS, fun, and RHS
-- of the rules

grammarDoc :: Grammar -> Doc
grammarDoc g = text "Grammar: " <$> indent 2 (ns <$> ts <$> ss <$> rs) <$> line where
  ns = ind "non terminals:" 2 . vcat . map (\z -> symbolDoc z <+> (text . show $ z)) $ g^..nsyms.folded
  ts = ind "terminals:" 2 . vcat . map (\z -> symbolDoc z <+> (text . show $ z)) $ g^..tsyms.folded
  ss = ind "start symbol:" 2 . startDoc $ g^.start
  rs = ind "rules:" 2 . vcat . map ruleDoc $ g^..rules.folded
  ind s k d = text s <$> indent k d

startDoc :: Maybe Symb -> Doc
startDoc Nothing = red $ text "no start symbol is set!"
startDoc (Just s) = symbolDoc s

ruleDoc :: Rule -> Doc
ruleDoc r = fill 16 (l <+> f) <+> text "->" <+> rs where
  l = symbolDoc $ r^.lhs
  f = text $ r^.fun.funName
  rs = hcat $ punctuate space $ map symbolDoc $ r^.rhs

symbolDoc :: Symb -> Doc
symbolDoc s = list $ map tnDoc $ s^.symb

tnDoc :: TN -> Doc
tnDoc (T s  ) = green $ text s
tnDoc (N s e)
  | Singular <- e = red $ text s

test = putDoc $ grammarDoc asG

