{-# LANGUAGE PatternGuards #-}

module FormalLanguage.CFG.TH where

import Control.Lens hiding (Strict)
import Data.List (intersperse,nub)
import Language.Haskell.TH

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
  runIO $ print $ g^..tsyms.folded.symb.folded.tnName
  runIO $ print $ g^..nsyms
  let ns = map (PlainTV . mkName . ("t"++)) $ g^..tsyms.folded.symb.folded.tnName
  let ts = map (PlainTV . mkName . genNname) $ g^..nsyms.folded -- PlainTV (mkName "xX")
  let fs = map genFname . nub $ g^..rules.folded
  s <- dataD (cxt []) (mkName "Signature") (ns++ts) [recC (mkName "Signature") fs] []
  runIO $ print s
  return s

-- | Generate the grammar.

genGrammar :: Grammar -> Q Dec
genGrammar = do
  return undefined

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
  let rtrn = VarT . mkName . genNname $ r^.lhs
  let args   = map (AppT ArrowT . genArg) $ r^.rhs
  runIO $ print name
  runIO $ print rtrn
  runIO $ print args
  runIO $ print $ foldr AppT rtrn args
  return (mkName name, NotStrict, foldr AppT rtrn args)

-- | Create the correct argument.
--
-- TODO make sure to handle multi-dim terms using Z:.

genArg :: Symb -> Type
genArg s
  | isSymbT s = genTType s
  | isSymbN s = VarT . mkName . genNname $ s
  | otherwise = error $ "incompatible symbol: " ++ show s

