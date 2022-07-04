
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

import           ADPfusion.Core
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

-- |
--
-- TODO It could be beneficial to introduce
-- @type Splitted = Either String (String,String)@
-- or something isomorphic. While [String] works, it allows for too many
-- possibilities here! ([] ist lightweight, on the other hand ...)

pretty :: Monad m => SigPKN m [String] [[String]] Char
pretty = SigPKN
  { unp = \ [x] c     -> [x ++ "."]
  , jux = \ [x] c [y] d -> [x ++ "(" ++ y ++ ")"]
  , pse = \ () () [x1,x2] [y1,y2] -> [x1 ++ y1 ++ x2 ++ y2]
  , nil = \ ()      -> [""]
  , pk1 = \ (Z:.[x]:.()) (Z:.a:.()) [y1,y2] (Z:.():.[z]) (Z:.():.b) -> [x ++ "[" ++ y1 , y2 ++ z ++ "]"]
  , pk2 = \ (Z:.[x]:.()) (Z:.a:.()) [y1,y2] (Z:.():.[z]) (Z:.():.b) -> [x ++ "{" ++ y1 , y2 ++ z ++ "}"]
  , nll = \ (Z:.():.()) -> ["",""]
  , h   = SM.toList
  }
{-# INLINE pretty #-}

-- |
--
-- @
-- [{]}(())
-- caguagcu
-- [ ]
--  { }
--     (())
-- @

runPseudoknot :: Int -> String -> (Int,[[String]])
runPseudoknot k inp = (d, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.t:.u:.v) = runInsideForward i
  d = unId $ axiom t
  bs = runInsideBacktrack i (Z:.t:.u:.v)
{-# NOINLINE runPseudoknot #-}

type X = ITbl Id Unboxed Subword Int
type T = ITbl Id Unboxed (Z:.Subword:.Subword) Int

runInsideForward :: VU.Vector Char -> Z:.X:.T:.T
runInsideForward i = mutateTablesDefault
                   $ gPKN bpmax
                        (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) (-666999) []))
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (-777999) []))
                        (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.subword 0 0:.subword 0 0) (Z:.subword 0 n:.subword 0 n) (-888999) []))
                        (chr i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack :: VU.Vector Char -> Z:.X:.T:.T -> [[String]]
runInsideBacktrack i (Z:.t:.u:.v) = unId $ axiom b
  where !(Z:.b:._:._) = gPKN (bpmax <|| pretty)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (toBacktrack u (undefined :: Id a -> Id a))
                          (toBacktrack v (undefined :: Id a -> Id a))
                          (chr i)
{-# NoInline runInsideBacktrack #-}

{-
main = do
  as <- getArgs
  let k = if null as then 1 else read $ head as
  ls <- lines <$> getContents
  forM_ ls $ \l -> do
    putStrLn l
    let (s,xs) = runPseudoknot k l
    print s
    mapM_ (\[x] -> printf "%s %5d\n" x s) xs

-}

