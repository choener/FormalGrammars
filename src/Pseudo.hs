
{-# Options_GHC -fdicts-cheap                  #-}
{-# Options_GHC -flate-dmd-anal                #-}
{-# Options_GHC -fno-liberate-case             #-}
{-# Options_GHC -fspec-constr-count=20000      #-}
{-# Options_GHC -fspec-constr-keen             #-}
{-# Options_GHC -fspec-constr-recursive=20000  #-}

-- Again, it is very important to have no full laziness. With this option we go from 80
-- Megacells/sec to 344 Megacells/sec.

{-# Options_GHC -fno-full-laziness             #-}

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
import           System.Environment
import           Debug.Trace (traceShow)

import           ADPfusion.Core
import           ADPfusion.PointL
import           Data.PrimitiveArray as PA hiding (map)

import           FormalLanguage



-- | Define signature and grammar

[formalLanguage|
Verbose

Grammar: PKN
N: S
N: <U,2>
T: c
S: S
S -> unp <<< S c
S -> nil <<< e
S -> pse <<< U U

-- because [S,S] is wrapped in square brackets, it is interpreted as a terminal stack, hence we can
-- access the individual "skalar" @S@ symbols, without it turning into an errorneous 2-dim syntactic
-- variable
<U,U> -> pk3 <<< [S,S] [c,c]
<U,U> -> nll <<< [e,e]

//
Emit: PKN
|]

makeAlgebraProduct ''SigPKN

bpmax :: Monad m => SigPKN m Int Int Char Char
--{{{
{-# INLINE bpmax #-}
bpmax = SigPKN
  { unp = \ x c     -> x
  , pse = \ () x    -> x
  , nil = \ ()      -> 1
  , pk3 = \ (Z:.s:.t) (Z:.a:.b) -> s + t
  , nll = \ (Z:.():.()) -> 1
  , h   = SM.foldl' (+) 0
  }
--}}}


runPseudoknot :: Int -> String -> (Int,[[String]],String,[String])
--{{{
runPseudoknot k inp = (d, take k bs, showPerfCounter perf,map showPerfCounter eachPerf) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  Mutated (Z:.s:.u) perf eachPerf = runInsideForward i
  d = unId $ axiom s
  bs = [] -- runInsideBacktrack i (Z:.t:.u:.v)
{-# NOINLINE runPseudoknot #-}
--}}}

runInsideForward
  :: VU.Vector Char
  -> Mutated (Z
                :.TwITbl 0 0 Id (Dense VU.Vector) EmptyOk               (PointL I)              Int
                :.TwITbl 0 0 Id (Dense VU.Vector) (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Int
             )
--{{{
{-# NoInline runInsideForward #-}
runInsideForward i = runST $ do
  let n = VU.length i
  arrS  <- newWithPA (LtPointL n)                   0
  arrUU <- newWithPA (ZZ:..LtPointL n:..LtPointL n) 0
  let guideIndex = Z:. BOI @0 (upperBound arrUU)
  fillTablesDim guideIndex $ gPKN bpmax
    (ITbl @_ @_ @_ @_ @_ @_ EmptyOk               arrS)
    (ITbl @_ @_ @_ @_ @_ @_ (Z:.EmptyOk:.EmptyOk) arrUU)
    (chr i) (chr i)
--}}}

--runInsideBacktrack :: VU.Vector Char -> Z:.X:.T:.T -> [[String]]
--runInsideBacktrack i (Z:.t:.u:.v) = unId $ axiom b
--  where !(Z:.b:._:._) = gPKN (bpmax <|| pretty)
--                          (toBacktrack t (undefined :: Id a -> Id a))
--                          (toBacktrack u (undefined :: Id a -> Id a))
--                          (toBacktrack v (undefined :: Id a -> Id a))
--                          (chr i)
--{-# NoInline runInsideBacktrack #-}

main :: IO ()
--{{{
main = do
  as <- getArgs
  let k = if null as then 1 else read $ head as
  ls <- lines <$> getContents
  forM_ ls $ \l -> do
    putStrLn l
    let (s,xs,sp,sps) = runPseudoknot k l
    print s
    print sp
    mapM_ (\[x] -> printf "%s %5d\n" x s) xs
--}}}

