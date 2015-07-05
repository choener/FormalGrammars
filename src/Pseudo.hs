
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
Verbose

Grammar: PKN
N: S
N: <U,2>
N: <V,2>
T: c
S: S
S -> unp <<< S c
S -> jux <<< S c S c
S -> nil <<< e
S -> pse <<< U V U V

<U,U> -> pk1 <<< [S,-] [c,-] <U,U> [-,S] [-,c]
<U,U> -> nll <<< [e,e]

<V,V> -> pk2 <<< [S,-] [c,-] <V,V> [-,S] [-,c]
<V,V> -> nll <<< [e,e]
//
Emit: PKN
|]

makeAlgebraProduct ''SigPKN

{-
bpmax :: Monad m => SigPKN m Int Int Char
bpmax = SigPKN
  { unp = \ x c     -> x
  , jux = \ x c y d -> if c `pairs` d then x + y + 1 else -999999
  , pse = \ () () x y -> x + y
  , nil = \ ()      -> 0
  , pk1 = \ (Z:.x:.()) (Z:.a:.()) y (Z:.():.z) (Z:.():.b) -> if (a `pairs` b || a == 'N' && b == 'M') then x + y + z + 1 else -888888
  , pk2 = \ (Z:.x:.()) (Z:.a:.()) y (Z:.():.z) (Z:.():.b) -> if (a `pairs` b || a == 'N' && b == 'M') then x + y + z + 1 else -888888
  , nll = \ (Z:.():.()) -> 0
  , h   = SM.foldl' max (-999999)
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
-}

