
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
S: [X,X,X]
[X,X,X] -> done <<< [e,e,e]
[X,X,X] -> ccc  <<< [X,X,X] [c,c,c]
[X,X,X] -> ccm  <<< [X,X,X] [c,c,-]
[X,X,X] -> cmc  <<< [X,X,X] [c,-,c]
[X,X,X] -> mcc  <<< [X,X,X] [-,c,c]
[X,X,X] -> cmm  <<< [X,X,X] [c,-,-]
[X,X,X] -> mcm  <<< [X,X,X] [-,c,-]
[X,X,X] -> mmc  <<< [X,X,X] [-,-,c]
//

Emit: Global

-- SumOfPairs: Global has the following job: it takes the signature of the
-- given grammar (here Global) and provides a second signature SopGlobal
-- and a function of type @SopGlobal -> Global@. SopGlobal is special in
-- that we need functions for combining two '-' characters, the actual
-- 'sum' function in sum-of-pairs which is not really a monoid, since we
-- have to take in all arguments and combine correctly.
--
-- Example (a,b,'-') yields (a,b) , (a,'-') , (b,'-') and produces either
-- a score of a triple.
--
-- TODO use a function of arguments to Global directly?

-- SumOfPairs: Global
|]


makeAlgebraProduct ''SigGlobal



data SopGlobal' m s r x y z = SopGlobal'
  {
  }

-- |

score :: Monad m => SigGlobal m Int Int Char Char Char
score = SigGlobal
  { done  = \   (Z:.():.():.()) -> 0
  , ccc   = \ x (Z:.a :.b :.c ) -> if a==b && a==c then x+3 else -999999
  , ccm   = \ x (Z:.a :.b :.()) -> if a==b         then x+1 else -999999
  , cmc   = \ x (Z:.a :.():.c ) -> if a==c         then x+1 else -999999
  , mcc   = \ x (Z:.():.b :.c ) -> if b==c         then x+1 else -999999
  , cmm   = \ x (Z:.a :.():.()) -> x-2
  , mcm   = \ x (Z:.():.b :.()) -> x-2
  , mmc   = \ x (Z:.():.():.c ) -> x-2
  , h     = SM.foldl' max (-999999)
  }
{-# INLINE score #-}


-- | 

pretty :: Monad m => SigGlobal m (String,String,String) [(String,String,String)] Char Char Char
pretty = SigGlobal
  { done  = \         (Z:.():.():.()) -> ("","","")
  , ccc   = \ (x,y,z) (Z:.a :.b :.c ) -> (x ++ [a] ,y ++ [b], z ++ [c])
  , ccm   = \ (x,y,z) (Z:.a :.b :.()) -> (x ++ [a] ,y ++ [b], z ++ "-")
  , cmc   = \ (x,y,z) (Z:.a :.():.c ) -> (x ++ [a] ,y ++ "-", z ++ [c])
  , mcc   = \ (x,y,z) (Z:.():.b :.c ) -> (x ++ "-" ,y ++ [b], z ++ [c])
  , cmm   = \ (x,y,z) (Z:.a :.():.()) -> (x ++ [a] ,y ++ "-", z ++ "-")
  , mcm   = \ (x,y,z) (Z:.():.b :.()) -> (x ++ "-" ,y ++ [b], z ++ "-")
  , mmc   = \ (x,y,z) (Z:.():.():.c ) -> (x ++ "-" ,y ++ "-", z ++ [c])
  , h     = SM.toList
  }
{-# Inline pretty #-}

runNeedlemanWunsch :: Int -> String -> String -> String -> (Int,[(String,String,String)])
runNeedlemanWunsch k i1' i2' i3' = (d, take k . unId $ axiom b) where
  i1 = VU.fromList i1'
  i2 = VU.fromList i2'
  i3 = VU.fromList i3'
  !(Z:.t) = runNeedlemanWunschForward i1 i2 i3
  d = unId $ axiom t
  !(Z:.b) = gGlobal (score <|| pretty) (toBacktrack t (undefined :: Id a -> Id a)) (chr i1) (chr i2) (chr i3)
{-# NoInline runNeedlemanWunsch #-}

-- | Decoupling the forward phase for CORE observation.

runNeedlemanWunschForward
  :: Vector Char
  -> Vector Char
  -> Vector Char
  -> Z:.(TwITbl Id Unboxed (Z:.EmptyOk:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I:.PointL I) Int)
runNeedlemanWunschForward i1 i2 i3 = let n1 = VU.length i1; n2 = VU.length i2; n3 = VU.length i3 in mutateTablesDefault $
  gGlobal score
    (ITbl 0 0 (Z:.EmptyOk:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.PointL 0:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2:.PointL n3) (-999999) []))
    (chr i1) (chr i2) (chr i3)
{-# NoInline runNeedlemanWunschForward #-}

main = do
  ls <- lines <$> getContents
  let eats (a:b:c:xs) = do
        putStrLn a
        putStrLn b
        putStrLn c
        let (k,ys) = runNeedlemanWunsch 1 a b c
        forM_ ys $ \(y1,y2,y3) -> printf "%s %5d\n%s\n%s\n" y1 k y2 y3
        eats xs
      eats _ = return ()
  eats ls

