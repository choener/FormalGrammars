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
import           Data.Array.Repa.Index.Points
import           Data.PrimitiveArray as PA
import           Data.PrimitiveArray.Zero as PA

import           FormalLanguage.CFG



-- | Define signature and grammar

[formalLanguage|
Grammar: Global
N: X
T: c
T: e
E: z
[X,X] -> done  <<< [e,e]
[X,X] -> align <<< [X,X] [c,c]
[X,X] -> indel <<< [X,X] [z,c]
[X,X] -> delin <<< [X,X] [c,z]
//
|]


makeAlgebraProductH ['h] ''SigGlobal

score :: Monad m => SigGlobal m Int Int Char ()
score = SigGlobal
  { done  = \   (Z:.():.()) -> 0
  , align = \ x (Z:.a :.b ) -> if a==b then x+1 else -999999
  , indel = \ x (Z:.():.b ) -> x - 2
  , delin = \ x (Z:.a :.()) -> x - 2
  , h     = SM.foldl' max (-999999)
  }
{-# INLINE score #-}

forward :: VU.Vector Char -> VU.Vector Char -> ST s (Unboxed (Z:.PointL:.PointL) Int)
forward as bs = do
  let aL = VU.length as
  let bL = VU.length bs
  let aa = chr as
  let bb = chr bs
  !t' <- PA.newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 aL:.pointL 0 bL) (-999999)
  let t  = mTblD (Z:.EmptyOk:.EmptyOk) t'
  fillTable $ gGlobal score t aa bb Empty Empty
  PA.freeze t'
{-# NOINLINE forward #-}

fillTable (MTbl _ t,f) = do
  let (_,Z:.PointL (_:.aL):.PointL (_:.bL)) = boundsM t
  forM_ [0..aL] $ \a -> forM_ [0..bL] $ \b -> do
    let ix = Z:.pointL 0 a:.pointL 0 b
    (f $ ix) >>= PA.writeM t ix
{-# INLINE fillTable #-}

runGlobal :: Int -> String -> String -> (Int,[String])
runGlobal k as bs = (t PA.! (Z:.pointL 0 aL:.pointL 0 bL), take k b) where
  aa = VU.fromList as
  bb = VU.fromList bs
  aL = VU.length aa
  bL = VU.length bb
  t = runST $ forward aa bb
  b = [] -- backtrack i t
{-# NOINLINE runGlobal #-}

main = do
  ls <- lines <$> getContents
  let eats [] = return ()
      eats [x] = return ()
      eats (a:b:xs) = do
        putStrLn a
        putStrLn b
        let (k,ys) = runGlobal 1 a b
        printf "%5d\n" k
        eats xs
  eats ls


{-
pretty :: Monad m => SigNussinov m String (SM.Stream m String) Char ()
pretty = SigNussinov
  { unp = \ x c     -> x ++ "."
  , jux = \ x c y d -> x ++ "(" ++ y ++ ")"
  , nil = \ ()      -> ""
  , h   = return . id
  }
{-# INLINE pretty #-}

backtrack :: VU.Vector Char -> PA.Unboxed (Z:.Subword) Int -> [String]
backtrack inp t' = unId . SM.toList . unId . g $ subword 0 n where
  n = VU.length inp
  c = chr inp
  t = btTblS EmptyOk t' g
  (_,g) = gNussinov (bpmax <** pretty) t c Empty
{-# NOINLINE backtrack #-}

-}

