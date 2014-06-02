{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}

-- |
--
-- TODO we should check if it is possible to go a bit ``lower'' to the more raw
-- stuff, instead of trying to rebuild the top-level ADPfusion syntax. Thats
-- mostly for the RHS of rules.
--
-- TODO we should build the algebra product automatically (but that piece of TH
-- should go into ADPfusion)
--
-- TODO if the dimension of the grammar is 1, we should output specialized
-- code. Compare performance with and without an underlying @(Z:.a)@ in
-- ADPfusion.

module FormalLanguage.CFG.TH where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Lens hiding (Strict, (...))
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Array.Repa.Index
import           Data.Char (toUpper,toLower)
import           Data.Function (on)
import           Data.List (intersperse,nub,nubBy,groupBy)
import           Data.Maybe
import           Data.Vector.Fusion.Stream.Monadic (Stream)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Map as M
import qualified Data.Set as S
import           Text.Printf
import           GHC.Exts (the)

import           ADP.Fusion ( (%), (|||), (...), (<<<) )
import qualified ADP.Fusion.Multi as ADP
import           ADP.Fusion.None

import FormalLanguage.CFG.Grammar



-- * Local data ctors we use to build up signature and grammar

data TheF = TheF
  { _fName   :: Name
  , _fVar    :: Exp
  , _fStrict :: Strict
  , _fType   :: Type
  }
  deriving (Show)

makeLenses ''TheF

fVarStrictType :: Lens' TheF VarStrictType
fVarStrictType = lens get set where
  get :: TheF -> VarStrictType
  get f = (f^.fName, f^.fStrict, f^.fType)
  set :: TheF -> VarStrictType -> TheF
  set f (v,s,t) = f { _fName = v, _fStrict = s, _fType = t }

data TheN = TheN
  { _nName :: Name
  , _nVar  :: Exp
  , _nPat  :: Pat
  }
  deriving (Show)

makeLenses ''TheN

data TheSymb = TheSymb
  { _symbType :: Type
  , _symbVar  :: Exp
  }
  deriving (Show)

makeLenses ''TheSymb

data TheS = TheS
  { _sString :: String
  , _sName   :: Name
  , _sVarP   :: Pat
  , _sConT   :: Type
  }
  deriving (Show)

makeLenses ''TheS



-- * Builder functions

-- | Build the signature type and data constructor

genTheS s = do
  let n = "Sig" ++ s
  return $ TheS n (mkName n) (VarP . mkName . headLower $ n) (ConT . mkName $ n)

-- | associate each non-terminal with a new name for the variable in the grammar.

associateN :: Symb -> Q (Symb,TheN)
associateN s = do
  nn <- newName "n"
  return (s, TheN nn (VarE nn) (VarP nn))

-- | For each unique scalar terminal, we create a type

associateTerminalTypes :: String -> Q (String,Name)
associateTerminalTypes t = (t,) <$> newName t

-- | the objective function @h@ is always of the same type, we need to make
-- sure that stream payload and return here are different for things like
-- classified DP.

genHfun :: Name -> Name -> Name -> Q VarStrictType
genHfun m x r = do
  let n = "h"
  let strm = ConT ''Stream
  let args = AppT ArrowT . AppT (AppT strm (VarT m)) $ VarT x
  let rtrn = AppT (VarT m) (VarT r)
  return (mkName n, NotStrict, AppT args rtrn)

-- | Generate all the information for single terminals. Terminal symbols
-- are indexed by dimension.

associateTerminalNames :: [Symb] -> Q ([((String,Int),Name)])
associateTerminalNames xs = do
  let maxd = subtract 1 . the . map (length . view symb) $ xs
  ys <- forM [0 .. maxd] $ \d ->
    forM (filter isT . nub $ xs^..folded.symb.ix d) $ \s -> do
      nn <- newName $ (s^.tnName) ++ show d
      return ((s^.tnName,d), nn)
  return $ concat ys

-- | Determine the full type of a terminal symbol. Is actually in the @Q@ monad
-- so that we may use quasi-quoting.

termSymbType :: M.Map String Name -> Symb -> Q Type
termSymbType ttypes s
  | Symb [E]   <- s = [t| () |]
  | Symb [T t] <- s = varT $ ttypes M.! t
  | Symb xs    <- s = foldl (\acc z -> [t| $acc :. $(case z of {E -> [t| () |] ; T t -> varT $ ttypes M.! t} ) |] ) [t| Z |] $ xs

termSymbExp :: M.Map String Name -> M.Map (String,Int) Name -> Symb -> Q Exp
termSymbExp ttypes tnames s
  | Symb [E]   <- s = [| None |] -- whenever we encounter an epsilon, we choose 'None' to indicate that we simply jump over this position
  | Symb [T t] <- s = varE $ tnames M.! (t,0)
  | Symb xs    <- s = foldl (\acc (k,z) -> [| $acc ADP.:> $(case z of {E -> [| None |] ; T t -> varE $ tnames M.! (t,k)} ) |] ) [| ADP.M |] $ zip [0..] xs

associateTerminals :: Name -> M.Map String Name -> M.Map (String,Int) Name -> Symb -> Q (Symb,TheSymb)
associateTerminals eps ttypes tnames s = do
  stype <- termSymbType ttypes s
  sexp  <- termSymbExp  ttypes tnames s
  return (s, TheSymb stype sexp)

associateFunctions :: Name -> M.Map Symb TheSymb -> Rule -> Q ([String], TheF)
associateFunctions xname terminfo r = do
  let nm = mkName . headLower . concat . map headUpper $ r^.fun
  let as = map (AppT ArrowT . functionArgument xname terminfo) $ r^.rhs
  return (r^.fun, TheF nm (VarE nm) NotStrict (foldr AppT (VarT xname) as))

functionArgument :: Name -> M.Map Symb TheSymb -> Symb -> Type
functionArgument xname terminfo s
  | isSymbN s = VarT xname
  | isSymbT s = view symbType $ terminfo M.! s

-- | the new generator

newGen :: Grammar -> Q [Dec]
newGen g = do
  m <- newName "m"
  x <- newName "x"
  r <- newName "r"
  e <- newName "e" -- we need a single name for None's
  ix <- newName "ix" -- the index variable
  sg <- genTheS $ g^.name
  h  <- genHfun m x r
  ns <- M.fromList <$> (mapM associateN $ collectSymbN g)
  ttypes <- M.fromList <$> (mapM associateTerminalTypes $ g^..tsyms.folded.symb.folded.tnName)
  tnames <- M.fromList <$> (associateTerminalNames $ collectSymbT g)
  ts <- M.fromList <$> (mapM (associateTerminals e ttypes tnames) $ collectSymbT g)
  fs <- M.fromList <$> (mapM (associateFunctions x ts) . nubBy ((==) `on` _fun) $ g^..rules.folded)
  runIO . printf "The signature uses the following types variables (in order): Monad, Result, Collection, %s\n" . concat . intersperse ", " $ M.keys ttypes
  sig <- dataD (cxt [])
               (sg^.sName)
               (PlainTV m:PlainTV x:PlainTV r:(map PlainTV $ ttypes^..folded)) -- monad, non-terminal, non-term collection, and SCALAR terminal types
               [recC (sg^.sName) ((map return $ fs^..folded.fVarStrictType) ++ [return h])
               ]
               []
  runIO . printf "the grammar uses the following arguments: %s\n" . concat . intersperse ", " $ (map show $ M.keys ns) ++ (map show $ M.keys tnames)
  let graArgs =  (recP (sg^.sName) ((return (h^._1, VarP $ h^._1)):[return (n, VarP n) | n <- fs^..folded.fName])) -- bind algebra
              :  (map (return . view nPat) $ ns^..folded) -- bind non-terminal memo-tables
              ++ (map varP $ tnames^..folded)  -- bind terminal symbols
--  let graBody = normalB . tupE . map (genBodyPair h ix ns ts fs) . groupBy ((==)`on`_lhs) $ g^..rules.folded
  let graBody = normalB . foldl (\acc z -> [| $acc :. $(genBodyPair h ix ns ts fs z) |]) [|Z|] . groupBy ((==) `on` _lhs) $ g^..rules.folded
  gra <- funD (mkName $ "g" ++ g^.name) [clause graArgs graBody []]
  inl <- pragInlD (mkName $ "g" ++ g^.name) Inline FunLike AllPhases
  return [sig,gra,inl]

-- | The body is a series of pairs, built here

genBodyPair h ix ns ts fs rs = do
  let r = head rs
  let rhs = lamE [varP ix]
          $ appE ( uInfixE (foldl1 (\acc z -> uInfixE acc (varE '(|||)) z) . map (genBodyRhs ns ts fs) $ rs)
                           (varE '(...))
                           (varE $ h^._1) )
                 (varE ix)
  tupE [return . view nVar $ ns M.! (r^.lhs), rhs]

-- | the right-hand sides involved in each rule

genBodyRhs ns ts fs (Rule _ f rs) = appE (appE (varE '(<<<)) (return . view fVar $ fs M.! f))
                                  . foldl1 (\acc z -> uInfixE acc (varE '(%)) z) . map genS $ rs
  where genS s
          | isSymbT s = return . view symbVar $ ts M.! s
          | isSymbN s = return . view nVar    $ ns M.! s



-- * helper functions

headUpper [] = []
headUpper (x:xs) = toUpper x : xs

headLower [] = []
headLower (x:xs) = toLower x : xs

