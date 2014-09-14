
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | The Nussinov RNA secondary structure prediction problem.
--
-- NOTE This version uses two tables which has no useful properties, except
-- that we can test wether this correctly fuses and builds no black holes!

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
import           Data.List
import           Data.Vector.Fusion.Util
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Unboxed as VU
import           Text.Printf

import           ADP.Fusion
import           Data.PrimitiveArray as PA hiding (map)

import           FormalLanguage.CFG



-- | Define signature and grammar

[formalLanguage|
Grammar: Nussinov
N: X
N: Y
T: c
T: e
X -> unp <<< Y c
X -> jux <<< Y c Y c
X -> nil <<< e
Y -> unp <<< X c
Y -> jux <<< X c X c
Y -> nil <<< e
//
|]

makeAlgebraProductH ['h] ''SigNussinov


bpmax :: Monad m => SigNussinov m Int Int Char ()
bpmax = SigNussinov
  { unp = \ x c     -> x
  , jux = \ x c y d -> if c `pairs` d then x + y + 1 else -999999
  , nil = \ ()      -> 0
  , h   = SM.foldl' max 0
  }
{-# INLINE bpmax #-}

pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
  || c=='G' && d=='U'
  || c=='U' && d=='A'
  || c=='U' && d=='G'
{-# INLINE pairs #-}

pretty :: Monad m => SigNussinov m String (SM.Stream m String) Char ()
pretty = SigNussinov
  { unp = \ x c     -> x ++ "."
  , jux = \ x c y d -> x ++ "(" ++ y ++ ")"
  , nil = \ ()      -> ""
  , h   = return . id
  }
{-# INLINE pretty #-}

runNussinov :: Int -> String -> (Int,[String])
runNussinov k inp = (d, take k . S.toList . unId $ axiom b) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.s:.t) = mutateTablesDefault
             $ gNussinov bpmax
                 (ITbl EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-999999) []))
                 (ITbl EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-999999) []))
                 (chr i) Empty
                 :: Z:.ITbl Id Unboxed Subword Int:.ITbl Id Unboxed Subword Int
  d = let ITbl _ arr _ = t in arr PA.! subword 0 n
  !(Z:.b:.c) = gNussinov (bpmax <** pretty) (toBT s (undefined :: Id a -> Id a)) (toBT t (undefined :: Id a -> Id a)) (chr i) Empty

{-

type Arr  = PA.Unboxed Subword Int
type Arrs = Z:.Arr:.Arr

forward :: VU.Vector Char -> ST s Arrs -- (Unboxed (Z:.Subword) Int)
forward inp = do
  let n  = VU.length inp
  let c  = chr inp
  !s' <- PA.newWithM (subword 0 0) (subword 0 n) (-999999)
  !t' <- PA.newWithM (subword 0 0) (subword 0 n) (-999999)
  let s  = MTbl EmptyOk s'
  let t  = MTbl EmptyOk t'
  runFreezeMTbls $ gNussinov bpmax s t c Empty
{-# NOINLINE forward #-}

backtrack :: VU.Vector Char -> Arrs -> [String]
backtrack inp (Z:.s':.t') = unId . SM.toList . unId $ axiom g where
  c = chr inp
  (Z:.g:.h) = gNussinov (bpmax <** pretty) (BtTbl EmptyOk s') (BtTbl EmptyOk t') c Empty
{-# NOINLINE backtrack #-}

runNussinov :: Int -> String -> (Int,[String])
runNussinov k inp = (t PA.! (subword 0 n), take k b) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  (Z:.s:.t) = runST $ forward i
  b = backtrack i (Z:.s:.t)
{-# NOINLINE runNussinov #-}

-}

main = do
  ls <- lines <$> getContents
  forM_ ls $ \l -> do
    putStrLn l
    let (k,[x]) = runNussinov 1 l
    printf "%s %5d\n" x k

