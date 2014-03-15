{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The Nussinov RNA secondary structure prediction problem.

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
import qualified Data.Vector.Unboxed as VU
import           Text.Printf

import           ADP.Fusion
import           Data.Array.Repa.Index
import           Data.Array.Repa.Index.Subword
import           Data.PrimitiveArray as PA
import           Data.PrimitiveArray.Zero as PA

import           FormalLanguage.CFG



-- | Define signature and grammar

[formalLanguage|
Grammar: Nussinov
N: X
T: c
T: e
X -> unp <<< X c
X -> jux <<< X c X c
X -> nil <<< e
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

forward :: VU.Vector Char -> ST s (Unboxed (Z:.Subword) Int)
forward inp = do
  let n  = VU.length inp
  let c  = chr inp
  !t' <- PA.newWithM (Z:.subword 0 0) (Z:.subword 0 n) (-999999)
  let t  = mTblS EmptyOk t'
  fillTable $ gNussinov bpmax t c Empty
  PA.freeze t'
{-# NOINLINE forward #-}

fillTable (MTbl _ t,f) = do
  let (_,Z:.Subword (_:.n)) = boundsM t
  forM_ [n,n-1 .. 0] $ \i -> forM_ [i..n] $ \j ->
    (f $ subword i j) >>= PA.writeM t (Z:.subword i j)
{-# INLINE fillTable #-}

backtrack :: VU.Vector Char -> PA.Unboxed (Z:.Subword) Int -> [String]
backtrack inp t' = unId . SM.toList . unId . g $ subword 0 n where
  n = VU.length inp
  c = chr inp
  t = btTblS EmptyOk t' g
  (_,g) = gNussinov (bpmax <** pretty) t c Empty
{-# NOINLINE backtrack #-}

runNussinov :: Int -> String -> (Int,[String])
runNussinov k inp = (t PA.! (Z:.subword 0 n), take k b) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  t = runST $ forward i
  b = backtrack i t
{-# NOINLINE runNussinov #-}

main = do
  ls <- lines <$> getContents
  forM_ ls $ \l -> do
    putStrLn l
    let (k,[x]) = runNussinov 1 l
    printf "%s %5d\n" x k

