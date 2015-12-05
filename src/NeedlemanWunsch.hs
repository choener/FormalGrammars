
-- | Needleman-Wunsch global alignment algorithm.

module Main where

import           Control.Applicative ()
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
import           Data.List (take)
import           Data.Vector.Fusion.Util
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed (Vector)
import           Text.Printf

import           ADP.Fusion
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

Emit: Global
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


-- | 
--
-- NOTE The alignment needs to be reversed to print out.

pretty :: Monad m => SigGlobal m (String,String) [(String,String)] Char Char
pretty = SigGlobal
  { done  = \       (Z:.():.()) -> ("","")
  , align = \ (x,y) (Z:.a :.b ) -> (x ++ [a] ,y ++ [b]) 
  , indel = \ (x,y) (Z:.():.b ) -> (x ++ "-" ,y ++ [b]) 
  , delin = \ (x,y) (Z:.a :.()) -> (x ++ [a] ,y ++ "-") 
  , h     = SM.toList
  }
{-# Inline pretty #-}

runNeedlemanWunsch :: Int -> String -> String -> (Int,[(String,String)])
runNeedlemanWunsch k i1' i2' = (d, take k . unId $ axiom b) where
  i1 = VU.fromList i1'
  i2 = VU.fromList i2'
  !(Z:.t) = runNeedlemanWunschForward i1 i2
  d = unId $ axiom t
  !(Z:.b) = gGlobal (score <|| pretty) (toBacktrack t (undefined :: Id a -> Id a)) (chr i1) (chr i2)
{-# NoInline runNeedlemanWunsch #-}

-- | Decoupling the forward phase for CORE observation.

runNeedlemanWunschForward :: Vector Char -> Vector Char -> Z:.(ITbl Id Unboxed (Z:.PointL I:.PointL I) Int)
runNeedlemanWunschForward i1 i2 = let n1 = VU.length i1; n2 = VU.length i2 in mutateTablesDefault $
  gGlobal score
    (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))
    (chr i1) (chr i2)
{-# NoInline runNeedlemanWunschForward #-}

main = do
  ls <- lines <$> getContents
  let eats [] = return ()
      eats [x] = return ()
      eats (a:b:xs) = do
        putStrLn a
        putStrLn b
        let (k,ys) = runNeedlemanWunsch 1 a b
        forM_ ys $ \(y1,y2) -> printf "%s %5d\n%s\n" y1 k y2
        eats xs
  eats ls

