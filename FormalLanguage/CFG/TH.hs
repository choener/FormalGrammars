{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}

-- |
--
-- TODO we should check if it is possible to go a bit ``lower'' to the more raw
-- stuff, instead of trying to rebuild the top-level ADPfusion syntax. Thats
-- mostly for the RHS of rules.

module FormalLanguage.CFG.TH where

import           Data.Char (toUpper,toLower)
import           ADP.Fusion ( (%), (|||), (...), (<<<) )
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
import qualified ADP.Fusion.Multi as ADP
import qualified Data.Map as M
import qualified Data.Set as S

import FormalLanguage.CFG.Grammar

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

genTheS s = do
  let n = "Sig" ++ s
  return $ TheS n (mkName n) (VarP . mkName . headLower $ n) (ConT . mkName $ n)

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
  return [sig,gra]

genBodyPair h ix ns ts fs rs = do
  let r = head rs
  let rhs = lamE [varP ix]
          $ appE ( uInfixE (foldl1 (\acc z -> uInfixE acc (varE '(|||)) z) . map (genBodyRhs ns ts fs) $ rs)
                           (varE '(...))
                           (varE $ h^._1) )
                 (varE ix)
  tupE [return . view nVar $ ns M.! (r^.lhs), rhs]

genBodyRhs ns ts fs (Rule _ f rs) = appE (appE (varE '(<<<)) (return . view fVar $ fs M.! f))
                                  . foldl1 (\acc z -> uInfixE acc (varE '(%)) z) . map genS $ rs
  where genS s
          | isSymbT s = return . view tVar $ ts M.! s
          | isSymbN s = return . view nVar $ ns M.! s

genHfun :: Name -> Name -> Name -> Q VarStrictType
genHfun m x r = do
  let n = "h"
  let strm = ConT ''Stream
  let args = AppT ArrowT . AppT (AppT strm (VarT m)) $ VarT x
  let rtrn = AppT (VarT m) (VarT r)
  return (mkName n, NotStrict, AppT args rtrn)

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
  let args = map (AppT ArrowT . newGenFargs tyN theT) $ r^.rhs
  return (r^.fun, TheF nn (VarE nn) NotStrict (foldr AppT (VarT tyN) args))

newGenFargs :: Name -> M.Map Symb TheT -> Symb -> Type
newGenFargs tyN theT s
  | isSymbT s = view tType $ theT M.! s
  | isSymbN s = VarT tyN
  | otherwise = error $ "incompatible symbol: " ++ show s

headUpper [] = []
headUpper (x:xs) = toUpper x : xs

headLower [] = []
headLower (x:xs) = toLower x : xs

-- associate each non-terminal with a new name for the variable in the grammar

genN :: Symb -> Q (Symb,TheN)
genN s = do
  nn <- newName "n"
  return (s, TheN nn (VarE nn) (VarP nn))

-- |

genT :: M.Map String TheTT -> Symb -> Q (Symb,TheT)
genT tt s@(Symb [z]) = do
  let n = view ttName $ tt M.! (z^.tnName)
  return $ (s, TheT [n] (VarE n) (VarT n))
genT tt s@(Symb zs) = do
  let ns = map (view ttName . (tt M.!) . view tnName) zs
  k <- foldl (\acc z -> uInfixE acc (varE '(ADP.:!)) z) (varE 'T) . map varE $ ns
  let t = foldl (\l r -> AppT (AppT (ConT '(:.)) l) r) (ConT 'Z) (map VarT ns)
  return $ (s, TheT ns k t)













-- | Given a grammar, create the Signature of data type
--
-- @data Signature a b = Signature { ...}@
--
-- TODO we assume that all signature functions have same arity and type; a
-- function to check this is currently missing and needs to be added!
--
-- TODO need varnames for all NTs and Ts
--
-- TODO allow only one type of NTs (as in one type ctor)
--
-- TODO need monad vartype also (for opt)

genSignature :: Grammar -> Q Dec
genSignature g = do
  let ts = map (PlainTV . mkName . ("t"++)) $ g^..tsyms.folded.symb.folded.tnName
  let ns = map (PlainTV . mkName . genNname) $ g^..nsyms.folded -- PlainTV (mkName "xX")
  let ns = map (PlainTV . mkName) ["_m", "_x", "_r"]
  let fs = map genFname . nub $ g^..rules.folded
  let h  = genObjectiveFun
  s <- dataD (cxt []) (mkName "Signature") (ns++ts) [recC (mkName "Signature") (fs++[h])] []
  return s

-- | Generate the grammar.
--
-- TODO need to give signature as well

genGrammar :: Grammar -> Q Dec
genGrammar g = do
  ns <- mapM (\n -> newName (genNname n) >>= \z -> return (n,z)) $ g^..nsyms.folded
  ts <- mapM (\t -> newName           t  >>= \z -> return (t,z)) $ g^..tsyms.folded.symb.folded.tnName
  runIO $ print ts
  let bd = normalB $ tupE $ map (genPair ns ts) $ groupBy ((==) `on` _lhs) $ S.toList $ g^.rules
  -- TODO a Signature{..} to function generation; maybe we should call it SigGrammarName{..}
  -- TODO better way to capture all signature variables? (or maybe just hand them over), needed for GenPair
  let as = (recP (mkName "Signature") [return (mkName "h", VarP $ mkName "h")]) : (map varP $ map snd ns ++ map snd ts)
  f <- funD (mkName "grammar") [clause as bd [{-decQs-}]]
  return f

-- | Generate a pair (nonterminal, function)

genPair :: [(Symb,Name)] -> [(String,Name)] -> [Rule] -> ExpQ
genPair ns ts rs = do
  let l = fromJust $ lookup (head rs ^. lhs) ns
  ix <- newName "ix"
  let rhs = lamE [varP ix]
          $ appE ( uInfixE (foldl1 (\acc z -> uInfixE acc (varE '(|||)) z) . map (genRHSrule ns ts) $ rs)
                           (varE '(...))
                           (varE $ mkName "h") )
                 (varE ix)
  tupE [varE l, rhs]

errorS :: Show a => a -> b
errorS = error . show

genRHSrule ns ts (Rule _ f rs) = appE (varE 'errorS). foldl1 (\acc z -> uInfixE acc (varE '(%)) z) . map genS $ rs
  where
  genS s
    | isSymbT s = genT $ s^.symb -- error $ "T " ++ show s
    | isSymbN s = varE . fromJust . lookup s $ ns
    | otherwise = error $ "can not build TH expression for: " ++ show s
  genT [z] = varE . fromJust . lookup (z^.tnName) $ ts
  genT zs  = foldl (\acc z -> uInfixE acc (varE '(ADP.:!)) z) (varE 'T) . map (genT . (:[])) $ zs

-- |

genTname = PlainTV . mkName . ("t"++)

-- | Generate a non-terminal name. Can be used for the type ctor as well as for
-- the functions and grammar.

genNname :: Symb -> String
genNname s = ("n_"++) . concat . intersperse "_" $ s^..symb.folded.tnName

-- | Terminal names are composites either of @t@ or of @(Z:.t1:.t2:. ...)@. The
-- correct version is created here. We can not use this for the type ctor.

genTType :: Symb -> Type
genTType s
  | [z] <- s^.symb = VarT . mkName $ "t"++ z^.tnName
  | zs  <- s^.symb = foldl
                       (\l r -> AppT (AppT (ConT '(:.)) l) r)
                       (ConT 'Z)
                       (map (VarT . mkName . ("t"++)) $ (zs^..folded.tnName))

-- | 
--
-- TODO the return type is the type of the LHS

-- ( AppT ( AppT ArrowT (VarT a_7) )
--        ( AppT ( AppT ArrowT (VarT b_8) ) 
--               ( VarT c_9               )
--        )
-- ) 

genFname :: Rule -> Q (Name,Strict,Type)
genFname r = do
  let name   = ("f_"++) . concat . intersperse "_" $ r^.fun
  let rtrn = VarT . mkName $ "_x" -- . genNname $ r^.lhs
  let args   = map (AppT ArrowT . genArg) $ r^.rhs
  return (mkName name, NotStrict, foldr AppT rtrn args)

-- |

genObjectiveFun :: Q (Name,Strict,Type)
genObjectiveFun = do
  let name = "h"
  let mnd  = VarT . mkName $ "_m"
  let rtrn = AppT mnd $ VarT . mkName $ "_r"
  let strm = ConT . mkName $ "Stream"
  let args = AppT ArrowT . AppT (AppT strm mnd) . VarT . mkName $ "_x"
  return (mkName name, NotStrict, AppT args rtrn)

-- | Create the correct argument.
--
-- TODO make sure to handle multi-dim terms using Z:.

genArg :: Symb -> Type
genArg s
  | isSymbT s = genTType s
  | isSymbN s = VarT . mkName $ "_x" -- . genNname $ s
  | otherwise = error $ "incompatible symbol: " ++ show s

