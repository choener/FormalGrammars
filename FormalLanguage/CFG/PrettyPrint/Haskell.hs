{-# LANGUAGE LambdaCase #-}

-- | A PrettyPrinter that generates "almost useable" Haskell modules. The
-- signature and grammar are created but the algebras are (obviously) missing.

module FormalLanguage.CFG.PrettyPrint.Haskell
  ( grammarHaskell
  ) where

import           Control.Lens
import           Data.Function (on)
import           Data.List (nub,sort,intersperse,nubBy,groupBy,foldl')
import qualified Data.Set as S
import           System.IO (stdout)
import           Text.PrettyPrint.ANSI.Leijen
import           Text.Printf
import           Control.Arrow hiding ((<+>))

import FormalLanguage.CFG.Grammar
import FormalLanguage.CFG.Parser



-- | Render grammar

grammarHaskell :: Grammar -> Doc
grammarHaskell g = signatureD g <$> empty <$> grammarD g <$> empty <$> productD g

signatureD :: Grammar -> Doc
signatureD g = hdr <$> indent 2 fns where
  hdr = text $ printf "data Sig%s {-Monad-} m {-NT-} nt hResT {-T-} %s = Sig%s" (g^.name) {- ns -} ts (g^.name)
  ns = concat . intersperse " " . nub . sort . map ntS . filter isSymbN $ (g^..rules.folded.lhs) ++ (g^..rules.folded.rhs.folded)
  ts = concat . intersperse " " . nub . sort
     . map (view tnName) . filter (\case (T _) -> True ; z -> False)
     $ g^..tsyms.folded.symb.folded
--  es = concat . intersperse " " . map (addEps . view tnName) $ g^..epsis.folded
--  fns = encloseSep lbrace rbrace comma . map (text . concat) . (++[["h"]]) . nub . sort $ g^..rules.folded.fun
  fns = encloseSep lbrace rbrace comma . (++[h]) . map ruleSigDoc . nubBy ((==) `on` _fun) . sort $ g^..rules.folded
  h = text "h :: Data.Vector.Fusion.Stream.Monadic.Stream m nt -> m hResT"

-- | Generate rule signatures for the 'Signature' data ctor.
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
      {-
      | isSymbE r = case (r^.symb) of
                      [x] -> text $ addEps $ x^.tnName
                      xs  -> encloseSep (text "(Z:.") rparen (text ":.") $ map (text . addEps . view tnName) xs
      -}
      | isSymbN r = text "nt"
      | isSymbT r = case (r^.symb) of
                      [x] -> text $ x^.tnName
                      xs  -> encloseSep (text "(Z:.") rparen (text ":.") $ map sigT xs
      | otherwise = error $ "ruleSigDoc: " ++ show r
      where sigT (T s) = text s
            sigT E     = text "()" -- important, EMIT NOTHING emits @()@

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
             text "{-T-}" <+> hsep (map (text . view tnName) . nub . sort
                      . filter (\case (T _) -> True ; z -> False)
                      $ g^..tsyms.folded.symb.folded) <+>
--             text "{-E-}" <+> hsep (map (text . addEps . view tnName) . nub . sort $ g^..epsis.folded) <+>
             text "="<$>
             indent 2 (tupled xs)
  where
    xs = map genForNT . groupBy ((==) `on` _lhs) $ g^..rules.folded

addEps "" = "eps"
addEps s  = s

genForNT xs = tupled [l,r] where
  l = text . ntS $ head xs ^. lhs
  r = encloseSep empty (text " ... h") (text " ||| ") $ map genApp xs

genApp x =   (text $ concat $ x^.fun)
         <+> text "<<<"
         <+> (encloseSep empty empty (text " % ") $ map genSymb $ x^.rhs)

genSymb x
  {-
  | isSymbE  x = case (x^.symb) of
                   [z] -> text $ theName z
                   zs  -> encloseSep (text "(Z:.") rparen (text ":.") $ map (text . theName) zs
                   -}
  | isSymbN x = text $ ntS x
  | isSymbT  x = case (x^.symb) of
                   [z] -> text $ theName z
                   zs  -> encloseSep (text "(T:!") rparen (text ":!") $ map (text . theName) zs
  where
    theName (E   ) = "None"
    theName (T s ) = s

productD g = (text $ printf "(<**) f g = Sig%s" (g^.name)) <$> indent 2 fs <$> bnd where
  fs = encloseSep lbrace rbrace comma $ (map productFun . nubBy ((==) `on` _fun) . sort $ g^..rules.folded) ++ [h]
  h = vcat $ map text
        [ "h xs = do"
        , "  hfs <- _Fh . Data.Vector.Fusion.Stream.Monadic.map fst $ xs"
        , "  let phfs = Data.Vector.Fusion.Stream.Monadic.concatMapM snd"
        , "           . Data.Vector.Fusion.Stream.Monadic.filter ((hfs==) . fst) $ xs"
        , "  _Gh phfs"
        ]
  bnd = indent 2 ((text "where") <$> indent 2 (bF <$> bG))
  bF = vcat $ map (\f -> let z = concat $ _fun f in text $ printf "_F%s = %s f" z z)
            $ fnubs ++ [Rule undefined ["h"] undefined]
  bG = vcat $ map (\f -> let z = concat $ _fun f in text $ printf "_G%s = %s g" z z)
            $ fnubs ++ [Rule undefined ["h"] undefined]
  {-
  bF = text (printf "Sig%s" (g^.name))
     <> (encloseSep lbrace rbrace comma . map text . (++["h_F"]) . map (("_F"++) . concat . _fun) $ fnubs)
     <> text " =f"
  bG = text (printf "Sig%s" (g^.name))
     <> (encloseSep lbrace rbrace comma . map text . (++["h_G"]) . map (("_G"++) . concat . _fun) $ fnubs)
     <> text " =g"
  -}
  fnubs = nubBy ((==) `on` _fun) . sort $ g^..rules.folded

productFun (Rule l f rs) = text (concat f) <> text " = \\" <> vars <> text " -> " <> parens (callF <> comma <> callG)
  where
    vars  = hsep $ zipWith mkVars rs vs
    callF = text (concat $ "_F" : f) <+> (hcat . punctuate space . map text $ take (length rs) vs)
    callG = let ns = map snd . filter (isSymbN . fst) $ zip rs vs
            in  text . genS $ zip rs vs
    vs = let az = ['a'..'z'] ; bs = [[]] ++ [ a:b | b<-bs, a<-az ] in drop 1 bs
    mkVars r v
      | isSymbT r = text v
      | isSymbN r = parens (text v <> comma <> text (v++"N"))
    genS zs = let go (ns,as) (r,v)
                    | isSymbT r = (ns, as ++ [v])
                    | isSymbN r = (ns++ [v++"N", ">>= Data.Vector.Fusion.Stream.Monadic.concatMap (\\", v, "->"], as ++ [v])
                  postAddBrackets = (++ (replicate (length . filter isSymbN . map fst $ zs) ')'))
              in  postAddBrackets
                  . concat
                  . intersperse " "
                  . uncurry (++)
                  . foldl' go ([],["Data.Vector.Fusion.Stream.Monadic.singleton $", (concat $ "_S" : f)])
                  $ zs

test = printDoc $ grammarHaskell asG where
  printDoc :: Doc -> IO ()
  printDoc d = displayIO stdout (renderPretty 0.8 160 $ d <> linebreak)

