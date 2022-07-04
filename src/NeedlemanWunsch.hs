
-- | Needleman-Wunsch global alignment algorithm.

module Main where

import           Control.Applicative ()
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
import           Data.List (take)
import           Data.Vector.Fusion.Util
import           Data.Vector.Unboxed (Vector)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           System.Environment (getArgs)
import           Text.Printf

import           ADPfusion.PointL
import           Data.PrimitiveArray as PA hiding (map,toList)

import           FormalLanguage.CFG



-- | Define signature and grammar

[formalLanguage|
Verbose

Grammar: Global
N: X
T: c      -- NOTE that each tape with a 'c' uses its own type!
S: [X,X]
[X,X] -> done  <<< [e,e]
[X,X] -> align <<< [X,X] [c,c]
[X,X] -> indel <<< [X,X] [-,c]
[X,X] -> delin <<< [X,X] [c,-]
//

Outside: Labolg
Source: Global
//

Emit: Global
Emit: Labolg
|]


makeAlgebraProduct ''SigGlobal


-- |

score :: Monad m => SigGlobal m Int Int Char Char
score = SigGlobal
  { done  = \   (Z:.():.()) -> 0
  , align = \ x (Z:.a :.b ) -> if a==b then x+1 else -999999
  , indel = \ x (Z:.():.b ) -> x - 2
  , delin = \ x (Z:.a :.()) -> x - 2
  , h     = SM.foldl' max (-999999)
  }
{-# INLINE score #-}


-- | Pretty-print the optimal alignment.
--
-- NOTE The alignment needs to be reversed to print out. @x ++ [a]@ is
-- @O(n^2)@, while @reverse@ on the final string with @a:x@ is @O(n)@. For
-- long inputs (say 1000nt) prettyprinting will otherwise take *longer*
-- than scoring.

pretty :: Monad m => SigGlobal m (String,String) [(String,String)] Char Char
pretty = SigGlobal
  { done  = \       (Z:.():.()) -> (""   ,""   )
  , align = \ (x,y) (Z:.a :.b ) -> (a:x  , b:y )
  , indel = \ (x,y) (Z:.():.b ) -> ('-':x, b:y )
  , delin = \ (x,y) (Z:.a :.()) -> (a:x  ,'-':y)
  , h     = SM.toList
  }
{-# Inline pretty #-}

runNeedlemanWunschF :: Int -> String -> String -> (Int,[(String,String)],PerfCounter)
runNeedlemanWunschF k i1' i2' = (d, take k . unId $ axiom b, perf) where
  i1 = VU.fromList i1'
  i2 = VU.fromList i2'
  Mutated (Z:.t) perf eachPerf = runNeedlemanWunschForward i1 i2
  d = unId $ axiom t
  !(Z:.b) = gGlobal (score <|| pretty) (toBacktrack t (undefined :: Id a -> Id a)) (chr i1) (chr i2)
{-# NoInline runNeedlemanWunschF #-}

runNeedlemanWunschB :: Int -> String -> String -> (Int,[(String,String)],PerfCounter)
runNeedlemanWunschB k i1' i2' = (d, take k . unId $ axiom b, perf) where
  i1 = VU.fromList i1'
  i2 = VU.fromList i2'
  Mutated (Z:.t) perf eachPerf = runNeedlemanWunschBackward i1 i2
  d = unId $ axiom t
  !(Z:.b) = gGlobal (score <|| pretty) (toBacktrack t (undefined :: Id a -> Id a)) (chr i1) (chr i2)
{-# NoInline runNeedlemanWunschB #-}

-- | Decoupling the forward phase for CORE observation.

runNeedlemanWunschForward
  :: Vector Char
  -> Vector Char
  -> Mutated (Z:.TwITbl 0 0 Id (Dense VU.Vector) (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Int)
runNeedlemanWunschForward i1 i2 = runST $ do
  arr <- newWithPA (ZZ:..LtPointL n1:..LtPointL n2) (-999999)
  fillTables $ gGlobal score
    (ITbl @_ @_ @_ @_ @_ @_ (Z:.EmptyOk:.EmptyOk) arr)
    (chr i1) (chr i2)
  where n1 = VU.length i1
        n2 = VU.length i2
{-# NoInline runNeedlemanWunschForward #-}

-- | Decoupling the forward phase for CORE observation.

runNeedlemanWunschBackward
  :: Vector Char
  -> Vector Char
  -> Mutated (Z:.TwITbl 0 0 Id (Dense VU.Vector) (Z:.EmptyOk:.EmptyOk) (Z:.PointL O:.PointL O) Int)
runNeedlemanWunschBackward i1 i2 = runST $ do
  arr <- newWithPA (ZZ:..LtPointL n1:..LtPointL n2) (-999999)
  fillTables $ gLabolg score
    (ITbl @_ @_ @_ @_ @_ @_ (Z:.EmptyOk:.EmptyOk) arr)
    (chr i1) (chr i2)
  where n1 = VU.length i1
        n2 = VU.length i2
{-# NoInline runNeedlemanWunschBackward #-}



main = do
  as <- getArgs
  let k = if null as then 1 else read $ head as
  ls <- lines <$> getContents
  let eats [] = return ()
      eats [x] = return ()
      eats (a:b:xs) = do
        putStrLn a
        putStrLn b
        let (sF,ysF,perfF) = runNeedlemanWunschF k a b
        let (sB,ysB,perfB) = runNeedlemanWunschB k a b
        forM_ ysF $ \(y1,y2) -> printf "%s\n%s\n%5d (backward: %5d)\n" (reverse y1) (reverse y2) sF sB
        when (k==0) $ print sF
        print perfF
        print perfB
        eats xs
  eats ls

