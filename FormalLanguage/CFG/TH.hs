{-# LANGUAGE PatternGuards #-}

module FormalLanguage.CFG.TH where

import Control.Lens hiding (Strict)
import Data.List (intersperse,nub,nubBy,groupBy)
import Language.Haskell.TH
import Data.Vector.Fusion.Stream.Monadic (Stream)
import Control.Arrow
import Control.Applicative
import Control.Monad
import qualified Data.Set as S
import Data.Function (on)
import Control.Monad.Trans.Class
import Data.Maybe

import FormalLanguage.CFG.Grammar



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

genGrammar :: Grammar -> Q Dec
genGrammar g = do
  ns <- mapM (\n -> newName (genNname n) >>= \z -> return (n,z)) $ g^..nsyms.folded
  ts <- mapM (\t -> newName           t  >>= \z -> return (t,z)) $ g^..tsyms.folded.symb.folded.tnName
  runIO $ print ts
  let bd = normalB $ tupE $ map (genPair ns ts) $ groupBy ((==) `on` _lhs) $ S.toList $ g^.rules
  f <- funD (mkName "grammar") [clause (map varP $ map snd ns ++ map snd ts) bd [{-decQs-}]]
  return f

-- |

genPair :: [(Symb,Name)] -> [(String,Name)] -> [Rule] -> ExpQ
genPair ns ts rs = do
  let l = fromJust $ lookup (head rs ^. lhs) ns
  tupE [varE l, tupE []]

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
                       (\l r -> AppT (AppT (ConT . mkName $ ":.") l) r)
                       (ConT . mkName $ "Z")
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

