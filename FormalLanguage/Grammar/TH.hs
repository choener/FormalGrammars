
module FormalLanguage.Grammar.TH where

import Language.Haskell.TH
import Control.Lens hiding (Strict)
import Data.List (intersperse,nub)

import FormalLanguage.Grammar



-- | Given a grammar, create the Signature of data type
--
-- "data Signature a b = Signature { ...}"
--
-- TODO we assume that all signature functions have same arity and type; a
-- function to check this is currently missing and needs to be added!
--
-- TODO need varnames for all NTs and Ts

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

-- | generate a non-terminal name

genNname :: Symb -> String
genNname s = ("n_"++) . concat . intersperse "_" $ s^..symb.folded.tnName

-- | 
--
-- TODO the return type is the type of the LHS

genFname :: Rule -> Q (Name,Strict,Type)
genFname r = do
  let n = ("f_"++) . concat . intersperse "_" $ r^.fun
  return (mkName n, NotStrict, TupleT 0)
