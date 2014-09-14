
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Needleman-Wunsch global alignment algorithm.

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

-- | 
--
-- NOTE The alignment needs to be reversed to print out.

pretty :: Monad m => SigGlobal m [String] (SM.Stream m [String]) Char ()
pretty = SigGlobal
  { done  = \       (Z:.():.()) -> [""   ,""   ]
  , align = \ [x,y] (Z:.a :.b ) -> [a  :x,b  :y]
  , indel = \ [x,y] (Z:.():.b ) -> ['-':x,b  :y]
  , delin = \ [x,y] (Z:.a :.()) -> [a  :x,'-':y]
  , h     = return . id
  }

runNeedlemanWunsch :: Int -> String -> String -> (Int,[[String]])
runNeedlemanWunsch k i1' i2' = (d, take k . S.toList . unId $ axiom b) where
  i1 = VU.fromList i1'
  i2 = VU.fromList i2'
  n1 = VU.length i1
  n2 = VU.length i2
  !(Z:.t) = mutateTablesDefault
          $ gGlobal score
              (ITbl (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2) (-999999) []))
              (chr i1) (chr i2)
              Empty Empty
              :: Z:.ITbl Id Unboxed (Z:.PointL:.PointL) Int
  d = let (ITbl _ arr _) = t in arr PA.! (Z:.pointL 0 n1:.pointL 0 n2)
  !(Z:.b) = gGlobal (score <** pretty) (toBT t (undefined :: Id a -> Id a)) (chr i1) (chr i2) Empty Empty

{-

type Arr  = PA.Unboxed (Z:.PointL:.PointL) Int
type Arrs = Z:.Arr

forward :: VU.Vector Char -> VU.Vector Char -> ST s Arrs
forward as bs = do
  let aL = VU.length as
  let bL = VU.length bs
  let aa = chr as
  let bb = chr bs
  !t' <- PA.newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 aL:.pointL 0 bL) (-999999)
  let t  = MTbl (Z:.EmptyOk:.EmptyOk) t'
  runFreezeMTbls $ gGlobal score t aa bb Empty Empty
{-# NOINLINE forward #-}

backtrack :: VU.Vector Char -> VU.Vector Char -> Arrs -> [[String]]
backtrack as bs (Z:.t') = map (map reverse) . unId . SM.toList . unId $ axiom g where -- . g $ Z:.pointL 0 aL:.pointL 0 bL where
  aa = chr as
  bb = chr bs
  t = BtTbl (Z:.EmptyOk:.EmptyOk) t'
  (Z:.g) = gGlobal (score <** pretty) t aa bb Empty Empty
{-# NOINLINE backtrack #-}

runGlobal :: Int -> String -> String -> (Int,[[String]])
runGlobal k as bs = (t PA.! (Z:.pointL 0 aL:.pointL 0 bL), take k b) where
  aa = VU.fromList as
  bb = VU.fromList bs
  aL = VU.length aa
  bL = VU.length bb
  (Z:.t) = runST $ forward aa bb
  b = backtrack aa bb (Z:.t)
{-# NOINLINE runGlobal #-}

-}

main = do
  ls <- lines <$> getContents
  let eats [] = return ()
      eats [x] = return ()
      eats (a:b:xs) = do
        putStrLn a
        putStrLn b
        let (k,ys) = runNeedlemanWunsch 1 a b
        forM_ ys $ \[y1,y2] -> printf "%s %5d\n%s\n" y1 k y2
        eats xs
  eats ls

