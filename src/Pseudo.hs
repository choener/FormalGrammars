
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

