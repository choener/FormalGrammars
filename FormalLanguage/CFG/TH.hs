{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}

-- |
--
-- TODO we should check if it is possible to go a bit ``lower'' to the more raw
-- stuff, instead of trying to rebuild the top-level ADPfusion syntax. Thats
-- mostly for the RHS of rules.

module FormalLanguage.CFG.TH where

import Control.Lens hiding (Strict)
import Data.List (intersperse,nub,nubBy,groupBy)
import Language.Haskell.TH
import Data.Vector.Fusion.Stream.Monadic (Stream)
import Control.Arrow ()
import Control.Applicative
import Control.Monad
import qualified Data.Set as S
import Data.Function (on)
import Control.Monad.Trans.Class
import Data.Maybe
import Data.Array.Repa.Index
import ADP.Fusion ( (%), (|||), (...) )
import qualified ADP.Fusion.Multi as ADP

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

