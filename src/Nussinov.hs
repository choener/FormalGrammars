
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
import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Text.Printf

import           ADP.Fusion
import           Data.PrimitiveArray as PA hiding (map)

import           FormalLanguage



-- | Define signature and grammar

[formalLanguage|
Grammar: Nussinov
N: X
T: c
S: X
X -> unp <<< X c
X -> jux <<< X c X c
X -> nil <<< e
//

Emit: Nussinov
|]



makeAlgebraProductH ['h] ''SigNussinov

bpmax :: Monad m => SigNussinov m Int Int Char
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

pretty :: Monad m => SigNussinov m String [String] Char
pretty = SigNussinov
  { unp = \ x c     -> x ++ "."
  , jux = \ x c y d -> x ++ "(" ++ y ++ ")"
  , nil = \ ()      -> ""
  , h   = SM.toList
  }
{-# INLINE pretty #-}

runNussinov :: Int -> String -> (Int,[String]) -- ,Int,[String])
runNussinov k inp = (d, take k . unId $ axiom b) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.t) = mutateTablesDefault
          $ gNussinov bpmax
              (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-999999) []))
              (chr i)
              :: Z:.ITbl Id Unboxed Subword Int
  d = unId $ axiom t
  !(Z:.b) = gNussinov (bpmax <|| pretty) (toBacktrack t (undefined :: Id a -> Id a)) (chr i)
{-# NoInline runNussinov #-}



main = do
  ls <- lines <$> getContents
  forM_ ls $ \l -> do
    putStrLn l
    let (k,[x]) = runNussinov 1 l
    printf "%s %5d\n" x k

