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

module FormalLanguage.CFG.TH where

import           Data.Char (toUpper,toLower)
import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Lens hiding (Strict)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Array.Repa.Index
import           Data.Function (on)
import           Data.List (intersperse,nub,nubBy,groupBy)
import           Data.Maybe
import           Data.Vector.Fusion.Stream.Monadic (Stream)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Map as M
import qualified Data.Set as S

import           ADP.Fusion ( (%), (|||), (...), (<<<) )
import qualified ADP.Fusion.Multi as ADP

import FormalLanguage.CFG.Grammar



-- * Local data ctors we use to build up signature and grammar

data TheTT = TheTT
  { _ttType :: TyVarBndr
  , _ttName :: Name
  , _ttPat  :: Pat
  }
  deriving (Show)

makeLenses ''TheTT

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

data TheT = TheT
  { _tNames :: [Name]
  , _tVar   :: Exp
  , _tType  :: Type
  }
  deriving (Show)

makeLenses ''TheT

data TheS = TheS
  { _sString :: String
  , _sName   :: Name
  , _sVarP   :: Pat
  , _sConT   :: Type
  }

makeLenses ''TheS



-- * Builder functions

-- | Build the signature type and data constructor

genTheS s = do
  let n = "Sig" ++ s
  return $ TheS n (mkName n) (VarP . mkName . headLower $ n) (ConT . mkName $ n)

-- | the new generator

newGen :: Grammar -> Q [Dec]
newGen g = do
  m <- newName "m"
  x <- newName "x"
  r <- newName "r"
  ix <- newName "ix"
  ns <- M.fromList <$> (mapM genN $ collectSymbN g)
  tt <- M.fromList <$> (mapM genTT . nub $ g^..tsyms.folded.symb.folded.tnName)
  ts <- M.fromList <$> (mapM (genT tt) $ collectSymbT g)
  fs <- M.fromList <$> (mapM (genF x ts) . nubBy ((==) `on` _fun) $ g^..rules.folded)
  h  <- genHfun m x r
  sg <- genTheS $ g^.name
  runIO $ print fs
  sig <- dataD (cxt [])
               (sg^.sName)
               (PlainTV m:PlainTV x:PlainTV r:(tt^..folded.ttType))
               [recC (sg^.sName) ((map return $ fs^..folded.fVarStrictType) ++ [return h])
               ]
               []
  let graArgs =  (recP (sg^.sName) ((return (h^._1, VarP $ h^._1)):[return (n, VarP n) | n <- fs^..folded.fName]))
              :  (map (return . view nPat) $ ns^..folded)
              ++ (map (return . view ttPat) $ tt^..folded)
  let graBody = normalB . tupE . map (genBodyPair h ix ns ts fs) . groupBy ((==)`on`_lhs) $ g^..rules.folded
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
          | isSymbT s = return . view tVar $ ts M.! s
          | isSymbN s = return . view nVar $ ns M.! s

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

-- | Generate all the information for single terminals

genTT :: String -> Q (String,TheTT)
genTT t = do
  nn <- newName t
  return (t, TheTT (PlainTV nn) nn (VarP nn))

-- | Generate all the function information. Note that we do not create a new
-- name here, because users need to be able to easily identify all the
-- signature functions.

genF :: Name -> M.Map Symb TheT -> Rule -> Q ([String],TheF)
genF tyN theT r = do
  let nn = mkName . headLower . concat . map headUpper $ r^.fun
  let args = map (AppT ArrowT . genFArg tyN theT) $ r^.rhs
  return (r^.fun, TheF nn (VarE nn) NotStrict (foldr AppT (VarT tyN) args))

-- | builds up a function argument

genFArg :: Name -> M.Map Symb TheT -> Symb -> Type
genFArg tyN theT s
  | isSymbT s = view tType $ theT M.! s
  | isSymbN s = VarT tyN
  | otherwise = error $ "incompatible symbol: " ++ show s

-- | associate each non-terminal with a new name for the variable in the grammar

genN :: Symb -> Q (Symb,TheN)
genN s = do
  nn <- newName "n"
  return (s, TheN nn (VarE nn) (VarP nn))

-- | builds up a terminal symbol, in 1-dim stuff we just have the terminal
-- symbol; in multi-dim cases we build up using ADPfusion stuff.

genT :: M.Map String TheTT -> Symb -> Q (Symb,TheT)
genT tt s@(Symb [z]) = do
  let n = view ttName $ tt M.! (z^.tnName)
  return $ (s, TheT [n] (VarE n) (VarT n))
genT tt s@(Symb zs) = do
  let ns = map (view ttName . (tt M.!) . view tnName) zs
  k <- foldl (\acc z -> uInfixE acc (varE '(ADP.:!)) z) (varE 'T) . map varE $ ns
  let t = foldl (\l r -> AppT (AppT (ConT '(:.)) l) r) (ConT 'Z) (map VarT ns)
  return $ (s, TheT ns k t)



-- * helper functions

headUpper [] = []
headUpper (x:xs) = toUpper x : xs

headLower [] = []
headLower (x:xs) = toLower x : xs

