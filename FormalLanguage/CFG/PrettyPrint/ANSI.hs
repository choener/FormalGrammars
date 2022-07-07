
-- |
--
-- TODO grammar-level indices should be colored red! also, make grammar
-- globally available (reader monad)

module FormalLanguage.CFG.PrettyPrint.ANSI
  where

import Control.Lens hiding (outside,Index)
import Control.Monad.Reader
import Data.Char (toUpper)
import Data.List (intersperse)
import Prelude hiding ((<$>))
import qualified Prelude as P
import Prettyprinter
import Prettyprinter.Render.Terminal
import Prettyprinter.Util
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO (stdout)

import FormalLanguage.CFG.Grammar



genGrammarDoc :: Grammar -> Doc AnsiStyle
genGrammarDoc g = runReader (grammarDoc g) g

grammarDoc :: Grammar -> Reader Grammar (Doc AnsiStyle)
grammarDoc g = do
  let numR = length $ g^..rules.folded
  ga <- indexDoc $ g^..params.folded
  ss <- fmap (ind "syntactic symbols:"             2 . vcat) . mapM steDoc $ g^..synvars.folded
  os <- fmap (ind "syntactic terminals:"           2 . vcat) . mapM steDoc $ g^..synterms.folded
  ts <- fmap (ind "terminals:"                     2 . vcat) . mapM steDoc $ g^..termvars.folded
  s  <- fmap (ind "start symbol:"                  2) $ symbolDoc (g^.start)
  rs <- fmap (ind ("rules (" <> pretty numR <> "):") 2 . vcat) . rulesDoc $ g^..rules.folded
  ind <- undefined
  return $ "Grammar: " <+> (pretty $ g^.grammarName) <+> ga <> indent 2 (vsep $ [ss] ++ [os | Outside _ <- [g^.outside]] ++ [ts, s, rs]) <> line
  where ind (s :: Doc AnsiStyle) k d = s <> indent k d

rulesDoc :: [Rule] -> Reader Grammar [Doc AnsiStyle]
rulesDoc = mapM ruleDoc

ruleDoc :: Rule -> Reader Grammar (Doc AnsiStyle)
ruleDoc (Rule lhs fun rhs)
  = do l  <- symbolDoc lhs
       rs <- fmap (intersperse "   ") . mapM (fmap (fill 5) . symbolDoc) $ rhs
       return $ fill 10 l <+> "->" <+> f <+> "<<<" <+> hcat rs
  where f  = fill 10 . pretty . concat . over (_tail.traverse._head) toUpper $ fun^..folded.getAttr

steDoc :: SynTermEps -> Reader Grammar (Doc AnsiStyle)
steDoc (SynVar  n i s k) = indexDoc i >>= return . {- . color Blue .   -} (pretty (n^.getSteName) <>)
steDoc (SynTerm n i    ) = indexDoc i >>= return . {- . color Magenta . -} (pretty (n^.getSteName) <>)
steDoc (Term    n i    ) = return $ {- color Green <> -} (pretty $ n^.getSteName)
steDoc (Epsilon _      ) = return $ annotate (color Red) "ε"
steDoc (Deletion       ) = return $ annotate (color Red) "-"

indexDoc :: [Index] -> Reader Grammar (Doc AnsiStyle)
indexDoc [] = return mempty
indexDoc xs = fmap (encloseSep lbrace rbrace comma) . mapM iDoc $ xs
  where iDoc (Index n i _ is s) = do ps <- asks _params
                                     -- return $ (if n `M.member` ps then color Red else mempty) <>
                                     return $ if not $ null is
                                       then pretty $ _getIndexName n ++ "∈" ++ show is
                                       else pretty $ _getIndexName n ++ "=" ++ show i 
        sDoc s | s==0 = mempty
               | s>=0 = "+" <> pretty s
               | s< 0 = pretty s

symbolDoc :: Symbol -> Reader Grammar (Doc AnsiStyle)
symbolDoc (view getSymbolList -> [x])
  | SynVar _ _ n k <- x
  , n > 1        = (<> "_" <> pretty k) P.<$> steDoc x
  | otherwise    = steDoc x
symbolDoc s@(view getSymbolList -> xs)
  | isAllSplit s = fmap (encloseSep langle rangle comma) . mapM steDoc $ xs
  | otherwise    = fmap list . mapM steDoc $ xs

printDoc :: Doc AnsiStyle -> IO ()
-- TODO fixme
--printDoc d = displayIO stdout (renderPretty 0.8 160 $ d <> hardline)
--printDoc = putDocW 200
printDoc = let opts = LayoutOptions { layoutPageWidth = Unbounded }
           in  renderIO stdout . layoutPretty opts

