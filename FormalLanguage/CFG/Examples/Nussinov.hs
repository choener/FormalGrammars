
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

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
import           Data.PrimitiveArray as PA hiding (map)

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

type Arr  = PA.Unboxed Subword Int
type Arrs = Z:.Arr

forward :: VU.Vector Char -> ST s Arrs -- (Unboxed (Z:.Subword) Int)
forward inp = do
  let n  = VU.length inp
  let c  = chr inp
  !t' <- PA.newWithM (subword 0 0) (subword 0 n) (-999999)
  let t  = MTbl EmptyOk t'
  runFreezeMTbls $ gNussinov bpmax t c Empty
{-# NOINLINE forward #-}

{-
fillTable (Z:.(MTbl _ t,f)) = do
  let (_,Subword (_:.n)) = boundsM t
  forM_ [n,n-1 .. 0] $ \i -> forM_ [i..n] $ \j ->
    (f $ subword i j) >>= PA.writeM t (subword i j)
{-# INLINE fillTable #-}
-}

backtrack :: VU.Vector Char -> Arrs -> [String]
backtrack _ _ = undefined
{-
backtrack inp (Z:.t') = unId . SM.toList . unId . g $ subword 0 n where
  n = VU.length inp
  c = chr inp
  t = btTblS EmptyOk t' g
  (Z:.(_,g)) = gNussinov (bpmax <** pretty) t c Empty
  -}
{-# NOINLINE backtrack #-}

runNussinov :: Int -> String -> (Int,[String])
runNussinov k inp = (t PA.! (subword 0 n), take k b) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  (Z:.t) = runST $ forward i
  b = backtrack i (Z:.t)
{-# NOINLINE runNussinov #-}

main = do
  ls <- lines <$> getContents
  forM_ ls $ \l -> do
    putStrLn l
    let (k,[x]) = runNussinov 1 l
    printf "%s %5d\n" x k

