
-- | A PrettyPrinter that generates "almost useable" Haskell modules. The
-- signature and grammar are created but the algebras are (obviously) missing.

module FormalLanguage.Grammar.PrettyPrint.Haskell
  ( grammarHaskell
  ) where

import           Control.Lens
import qualified Data.Set as S
import           Text.PrettyPrint.ANSI.Leijen
import           System.IO (stdout)
import Text.Printf
import Data.List (nub,sort,intersperse,nubBy,groupBy)
import Data.Function (on)

import FormalLanguage.Grammar
import FormalLanguage.Parser



-- | Render grammar

grammarHaskell :: Grammar -> Doc
grammarHaskell g = signatureD g <$> grammarD g

signatureD :: Grammar -> Doc
signatureD g = hdr <$> indent 2 fns where
  hdr = text $ printf "data Sig%s {-Monad-} m {-NT-} nt {-T-} %s = Sig%s" (g^.name) {- ns -} ts (g^.name)
  ns = concat . intersperse " " . nub . sort . map ntS . filter nSymb $ (g^..rules.folded.lhs) ++ (g^..rules.folded.rhs.folded)
  ts = concat . intersperse " " . map (view tnName)  $ g^..tsyms.folded.symb.folded
--  fns = encloseSep lbrace rbrace comma . map (text . concat) . (++[["h"]]) . nub . sort $ g^..rules.folded.fun
  fns = encloseSep lbrace rbrace comma . (++[h]) . map ruleSigDoc . nubBy ((==) `on` _fun) . sort $ g^..rules.folded
  h = text "h :: M.Stream m nt -> nt"

-- |
--
-- TODO extend to allow classified dp (need more than one NT type)

ruleSigDoc :: Rule -> Doc
ruleSigDoc (Rule lhs fun rhs) =
  text (concat fun) <+>
  text "::" <+>
  cat (punctuate (text " -> ") rs) <+>
  text "-> nt"
  where
    rs = map tOrNt rhs
    tOrNt r
      | nSymb r = text "nt"
      | tSymb r = case (r^.symb) of
                    [x] -> text $ x^.tnName
                    xs  -> encloseSep (text "(Z:.") rparen (text ":.") $ map (text . view tnName) xs

ntS :: Symb -> String
ntS (Symb []) = error "zero-dim symbol"
ntS (Symb xs) = "_" ++ concatMap (\x -> x^.tnName ++ addIndex x) xs

addIndex :: TN -> String
addIndex (N _ Singular) = ""
addIndex (N _ (IntBased k _)) = show k
addIndex _ = ""

-- |
--
-- TODO collect all rules with same lhs 

grammarD :: Grammar -> Doc
grammarD g = text ("grammar" ++ g^.name) <+>
             text ("Sig" ++ g^.name ++ "{..}") <+>
             text "{-NT-}" <+> hsep (map (text . ntS) . nub . sort $ g^..rules.folded.lhs) <+>
             text "{-T-}" <+> hsep (map (text . view tnName) $ g^..tsyms.folded.symb.folded) <+>
             text "="<$>
             indent 2 (tupled xs)
  where
    xs = map genForNT . groupBy ((==) `on` _lhs) $ g^..rules.folded

genForNT xs = tupled [l,r] where
  l = text . ntS $ head xs ^. lhs
  r = encloseSep empty (text " ... h") (text " ||| ") $ map genApp xs

genApp x =   (text $ concat $ x^.fun)
         <+> text "<<<"
         <+> (encloseSep empty empty (text " % ") $ map genSymb $ x^.rhs)

genSymb x
  | nSymbG x = text $ ntS x
  | tSymb  x = case (x^.symb) of
                 [z] -> text $ z^.tnName
                 zs  -> encloseSep (text "(Z:.") rparen (text ":.") $ map (text . view tnName) zs

test = printDoc $ grammarHaskell asG where
  printDoc :: Doc -> IO ()
  printDoc d = displayIO stdout (renderPretty 0.8 160 $ d <> linebreak)
