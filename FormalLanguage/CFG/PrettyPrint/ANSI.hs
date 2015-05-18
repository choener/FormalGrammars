
-- |
--
-- TODO grammar-level indices should be colored red! also, make grammar
-- globally available (reader monad)

module FormalLanguage.CFG.PrettyPrint.ANSI
--  ( grammarDoc
--  , rulesDoc
--  , printDoc
--  , symbolDoc
--  ) where
  where

import           Control.Lens hiding (outside,Index)
import           Control.Monad.Reader
import           Data.List (intersperse)
import           Prelude hiding ((<$>))
import qualified Data.Map as M
import qualified Data.Set as S
import           System.IO (stdout)
import           Text.PrettyPrint.ANSI.Leijen
import           Data.Char (toUpper)

import FormalLanguage.CFG.Grammar
--import FormalLanguage.CFG.Parser



genGrammarDoc :: Grammar -> Doc
genGrammarDoc g = runReader (grammarDoc g) g

grammarDoc :: Grammar -> Reader Grammar Doc
grammarDoc g = do
  ga <- indexDoc $ g^..params.folded
  ss <- fmap (ind "syntactic symbols:"   2 . vcat) . mapM steDoc $ g^..synvars.folded
  os <- fmap (ind "syntactic terminals:" 2 . vcat) . mapM steDoc $ g^..synterms.folded
  ts <- fmap (ind "terminals:"           2 . vcat) . mapM steDoc $ g^..termvars.folded
  s  <- fmap (ind "start symbol:"        2) $ symbolDoc (g^.start)
  rs <- fmap (ind "rules:"               2 . vcat) . rulesDoc $ g^..rules.folded
  ind <- undefined
  return $ text "Grammar: " <+> (text $ g^.grammarName) <+> ga <$> indent 2 (vsep $ [ss] ++ [os | Outside _ <- [g^.outside]] ++ [ts, s, rs]) <$> line
  where ind s k d = text s <$> indent k d

rulesDoc :: [Rule] -> Reader Grammar [Doc]
rulesDoc rs = mapM ruleDoc rs

ruleDoc :: Rule -> Reader Grammar Doc
ruleDoc (Rule lhs fun rhs)
  = do l  <- symbolDoc lhs
       rs <- fmap (intersperse (text "   ")) . mapM symbolDoc $ rhs
       return $ fill 10 l <+> text "->" <+> f <+> text "<<<" <+> hcat rs
  where f  = fill 10 . text . concat . (over (_tail.traverse._head) toUpper) $ fun^..folded.getAttr

steDoc :: SynTermEps -> Reader Grammar Doc
steDoc (SynVar  n i) = indexDoc i >>= return . blue . (text (n^.getSteName) <+>)
steDoc (SynTerm n i) = indexDoc i >>= return . magenta . (text (n^.getSteName) <+>)
steDoc (Term    n i) = return . green . text $ n^.getSteName
steDoc (Epsilon    ) = return . red   . text $ "ε"
steDoc (Deletion   ) = return . red   . text $ "-"

indexDoc :: [Index] -> Reader Grammar Doc
indexDoc [] = return empty
indexDoc xs = fmap (encloseSep lbrace rbrace comma) . mapM iDoc $ xs
  where iDoc (Index i _ _ s) = do ps <- asks _params
                                  return $ (if i `M.member` ps then red else id) $ text $ _getIndexName i
        sDoc s | s==0 = empty
               | s> 0 = text $ "+" ++ show s
               | s< 0 = text $        show s

symbolDoc :: Symbol -> Reader Grammar Doc
symbolDoc (Symbol [x]) = steDoc x
symbolDoc (Symbol xs ) = fmap list . mapM steDoc $ xs

printDoc :: Doc -> IO ()
printDoc d = displayIO stdout (renderPretty 0.8 160 $ d <> linebreak)

-- testPrint = test >>= \z -> case z of {Just g -> mapM_ (printDoc . genGrammarDoc) g}

{-
-- | Prettyprint a grammar ANSI-style.
--
-- TODO Later on, it would be really nice to better align the LHS, fun, and RHS
-- of the rules

grammarDoc :: Grammar -> Doc
grammarDoc g = text "Grammar: " <+> (text $ g^.name) <$> indent 2 (ns <$> is <$> ts <$> es <$> ss <$> rs) <$> line where
  ns = ind "syntactic symbols:" 2 . vcat $ map (\z -> (symbolDoc z <+> (text . show $ z))) (g^..nsyms.folded)
  is = if S.null (g^.nIsms) then text "" else ind "inside syntactic symbols (acting as terminals .. in a way):" 2 . vcat $ map (\z -> (symbolDoc z <+> (text . show $ z))) (g^..nIsms.folded)
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
  | [z] <- s^.symb = outside $ tnDoc z
  | otherwise      = outside . list $ map tnDoc $ s^.symb
  where outside = case s^.symbInOut of {Inside -> id; Outside -> underline . bold . (<> red (text "*"))}

-- | Prettyprint a (non-)terminal symbol.

tnDoc :: TN -> Doc
tnDoc (E    ) = blue  $ text "ε"
tnDoc (T s  ) = green $ text s
tnDoc (N s e)
  | Singular <- e = red $ text s
  | IntBased k z <- e = (red $ text s) <+> (magenta $ text $ show k)

-- |

-}

