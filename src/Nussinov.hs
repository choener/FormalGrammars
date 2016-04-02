
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
import           Unsafe.Coerce (unsafeCoerce)

import           ADP.Fusion
import           Data.PrimitiveArray as PA hiding (map)

import           FormalLanguage



-- | Define signature and grammar

[formalLanguage|
Verbose

Grammar: Nussinov
N: X
T: c
S: X
X -> unp <<< X c
X -> jux <<< X c X c
X -> nil <<< e
//

Outside: Vonissun
Source:  Nussinov
//

Emit: Nussinov
Emit: Vonissun
|]



makeAlgebraProduct ''SigNussinov
makeAlgebraProduct ''SigVonissun

bpmax :: Monad m => SigNussinov m Int Int Char
bpmax = SigNussinov
  { nUnp = \ x c     -> x
  , nJux = \ x c y d -> if c `pairs` d then x + y + 1 else -999999
  , nNil = \ ()      -> 0
  , nH   = SM.foldl' max 0
  }
{-# INLINE bpmax #-}

bpmaxV :: Monad m => SigVonissun m Int Int Char
bpmaxV = undefined

pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
  || c=='G' && d=='U'
  || c=='U' && d=='A'
  || c=='U' && d=='G'
{-# INLINE pairs #-}

pretty :: Monad m => SigNussinov m String [String] Char
pretty = SigNussinov
  { nUnp = \ x c     -> x ++ "."
  , nJux = \ x c y d -> x ++ "(" ++ y ++ ")"
  , nNil = \ ()      -> ""
  , nH   = SM.toList
  }
{-# INLINE pretty #-}

prettyV :: Monad m => SigVonissun m String [String] Char
prettyV = undefined

runNussinov :: Int -> String -> (Int,[String]) -- ,Int,[String])
runNussinov k inp = (d, take k . unId $ axiom b) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.t) = mutateTablesDefault
          $ gNussinov bpmax
              (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-999999) []))
              (chr i)
              :: Z:.TwITbl Id Unboxed EmptyOk (Subword I) Int
  d = unId $ axiom t
  !(Z:.b) = gNussinov (bpmax <|| pretty) (toBacktrack t (undefined :: Id a -> Id a)) (chr i)
              :: Z:.TwITblBt Unboxed EmptyOk (Subword I) Int Id Id String
{-# NoInline runNussinov #-}

runVonissun :: Int -> String -> (Int,[String])
runVonissun k inp = (d, []) where -- take k . unId $ axiom b) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.t) = mutateTablesDefault
          $ gVonissun bpmaxV
              (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-999999) []))
              (undefined :: TwITbl Id Unboxed EmptyOk (Subword I) Int)
              (chr i)
              :: Z:.TwITbl Id Unboxed EmptyOk (Subword O) Int
  d = unId $ axiom t
--  !(Z:.b) = gVonissun (bpmaxV <|| prettyV) (toBacktrack t (undefined :: Id a -> Id a)) (undefined :: Backtrack (ITbl Id Unboxed Subword Int) Id Id String) (chr i)
{-# NoInline runVonissun #-}



main = do
  ls <- lines <$> getContents
  forM_ ls $ \l -> do
    putStrLn l
    let (k,[x]) = runNussinov 1 l
    printf "%s %5d\n" x k

