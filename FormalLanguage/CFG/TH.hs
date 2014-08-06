
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import           Data.Char (toUpper,toLower)
import           Data.Function (on)
import           Data.List (intersperse,nub,nubBy,groupBy)
import           Data.Maybe
import           Data.Vector.Fusion.Stream.Monadic (Stream)
import           GHC.Exts (the)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax hiding (lift)
import qualified Data.Map as M
import qualified Data.Set as S
import           Text.Printf

import           Data.PrimitiveArray (Z(..), (:.)(..))
import           ADP.Fusion ( (%), (|||), (...), (<<<) )
import qualified ADP.Fusion.Multi as ADP
import           ADP.Fusion.None

import FormalLanguage.CFG.Grammar

{-
-}

import Control.Monad.State.Strict as M
import Data.Default



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

-- |
--
-- TODO add the generated name for the fully sated version in the grammar!

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
--  let graBody = normalB . foldl (\acc z -> [| $acc :. $(genBodyPair h ix ns ts fs z) |]) [|Z|] . groupBy ((==) `on` _lhs) $ g^..rules.folded
  let graBody = genFullBody h ix ns ts fs g
  gra <- funD (mkName $ "g" ++ g^.name) [clause graArgs graBody []]
  inl <- pragInlD (mkName $ "g" ++ g^.name) Inline FunLike AllPhases
  return [sig,gra,inl]

-- | With the new system, we need to bind non-terminals in a serier of
-- @let@'s. This requires: ...

genFullBody h ix ns ts fs g = do
  let gs = groupBy ((==) `on` _lhs) $ g^..rules.folded
  -- (i) create names for non-terminal, full bound
  nfb <- mapM (\(z:_) -> newName $ "nfb" ++ (z^.lhs.symb.folded.tnName)) gs
  runIO $ mapM_ print nfb
  -- (ii) pair those up with the non-terminals
  runIO $ print ns
  -- (iii) run something similar to genBodyPair
  -- (iv) profit
  return undefined

-- | The body is a series of pairs, built here
--
-- @h@ is the choice function; @ix@ the index variable; @ns@ the
-- non-terminals; @ts@ the terminals; @fs@ are the evaluation functions;
-- @rs@ are all the right-hand sides that have the same non-terminal symbol
-- on the left, i.e. @X -> y; X -> z@.

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



-- * @StateT CfgState Q@ monad and wrapper for TH-based grammar
-- construction.

-- | The state we carry around. Contains all the bound names, and lookup
-- tables for functions, terminals and syntactic variables.
--
-- NOTE the defaults all start out undefined, making sure anything invalid
-- explodes in our face.
--
-- TODO for TermTyNames, use @M.Map TN Name@ ?
--
-- TODO if we allow multiple different choice function, we'll have to
-- extend @_qChoiceFun@

data CfgState = CfgState
  { _qGrammar       :: Grammar  -- ^ the input grammar
  , _qGrammarName   :: Name     -- ^ the name for the body of the grammar
  , _qElemTyName    :: Name     -- ^ stream type name, as in @Stream m qElemTyName@
  , _qRetTyName     :: Name     -- ^ choice return type name, as in @h :: Stream m qElemTyName -> m qRetTyName@
  , _qMTyName       :: Name     -- ^ monad type name, as in @h :: Stream MTyName ...@
  , _qSigName       :: Name     -- ^ the name of the signature type and data constructor, both (signatures need to have a single data constructor)
  , _qTermTyNames   :: M.Map String Name  -- ^ the type name for each unique terminal symbol
  , _qSVarTyNames   :: M.Map Symb Name    -- ^ type variable names for syntactic variables (aka non-terminals)
  , _qSynBodyNames  :: M.Map Symb Name    -- ^ type variable names for the fully applied grammar body / where part
  , _qAttribFuns    :: M.Map [String] VarStrictType -- ^ map from the composed name to the template haskell attribute function @(Var,Strict,Type)@ (functions are currently stored as @[String]@ in @Grammar.hs@
  , _qChoiceFun     :: VarStrictType  -- ^ the choice function
  }

makeLenses ''CfgState

instance Default CfgState where
  def = CfgState
    { _qGrammar       = error "def / grammar"
    , _qGrammarName   = error "def / grammarname"
    , _qElemTyName    = error "def / elemty"
    , _qRetTyName     = error "def / retty"
    , _qMTyName       = error "def / mty"
    , _qSigName       = error "def / signame"
    , _qTermTyNames   = error "def / termtynames"
    , _qSVarTyNames   = error "def / svartynames"
    , _qSynBodyNames  = error "def / synbodynames"
    , _qAttribFuns    = error "def / attribfuns"
    , _qChoiceFun     = error "def / choicefun"
    }

type TQ z = StateT CfgState Q z

-- | Create the signature. Will also set the signature name.

signature :: TQ Dec
signature = do
  m         <- use qMTyName
  x         <- use qElemTyName
  r         <- use qRetTyName
  termNames <- use qTermTyNames
  sigName   <- (mkName . ("Sig" ++)) <$> use (qGrammar.name)
  fs        <- use qAttribFuns
  h         <- use qChoiceFun
  qSigName .= sigName
  lift $ dataD (cxt [])
               sigName
               (PlainTV m : PlainTV x : PlainTV r : (map PlainTV $ termNames^..folded))
               [recC sigName ((map return $ fs^..folded) ++ [return h])]
               []

-- | The grammar requires three types of arguments. First we need to bind
-- an algebra. Then we bind a list of non-terminals. Finally we bind a list
-- of terminals.

grammarArguments :: TQ [PatQ]
grammarArguments = do
  signame <- use qSigName
  h       <- use qChoiceFun
  fs      <- use qAttribFuns
  tnames  <- use qTermTyNames
  snames  <- use qSVarTyNames
  -- bind algebra
  let alg = recP signame [ fieldPat n (varP n) | (n,_,_) <- h:(fs^..folded) ]
  -- bind non-terminals
  let syn = [ varP s | s <- snames^..folded ]
  -- bind terminals
  let ter = [ varP t | t <- tnames^..folded ]
  return $ alg : syn ++ ter

-- | The grammar body is a series of @let@'s that bind the partially
-- applied syntactic variables to the right-hand sides. In each right-hand
-- side, the syntactic variable symbols are replaced by the left-bound
-- fully applied syntactic variables.
--
-- @
--  let s = s' (f >>> t ... h)
--      t = t' (f >>> s ... h)
--  in  Z:.s:.t
-- @

grammarBody :: TQ BodyQ
grammarBody = do
  return undefined

-- | Fully apply each partial syntactic variable to the corresponding
-- right-hand side. First, build up the map of fully applied names, then
-- associate each one.

grammarBodyWhere :: TQ [DecQ]
grammarBodyWhere = do
  synKeys   <- M.keys <$> use qSVarTyNames
  bodySynNames <- lift $ sequence [ (n,) <$> (newName $ concat k) | n <- synKeys, let k = n^..symb.folded.tnName ]
  qSynBodyNames .= M.fromList bodySynNames
  mapM grammarBodySyn bodySynNames

-- | Fully bind each 'Symb' (which is partially applied, coming in as an
-- argument in the grammar) to the correct right-hand side.

grammarBodySyn :: (Symb,Name) -> TQ DecQ
grammarBodySyn (s,n) = do
  hname <- use qChoiceName
  let rhs = appE ( uInfixE (foldl1 (\acc z -> uInfixE acc (varE '(|||)) z) . map grammarBodyRHS $ undefined)
                           (varE '(...))
                           (varE hname) )
  return $ valD (varP n) (normalB rhs) []

-- | Build up the rhs for each rule.
--
-- Requires using the fully bound syntactic variable name!

-- grammarBodyRHS :: ()
grammarBodyRHS = undefined

-- | Build the full grammar. Generate a name (the grammar name prefixed
-- with a @"g"@), the arguments, and the body of the grammar.

grammar :: TQ Dec
grammar = do
  gname <- (mkName . ("g" ++)) <$> use (qGrammar.name)
  qGrammarName .= gname
  args      <- grammarArguments
  bodyWhere <- grammarBodyWhere
  bodyNames <- use qSynBodyNames
  let body  =  normalB . foldl (\acc z -> [| $acc :. $z |]) [|Z|] . map varE $ bodyNames^..folded
  lift $ funD gname [clause args body bodyWhere]

-- | New entry point for generation of @Grammar@ and @Signature@ code. Will
-- also stuff the 'Grammar' into the state data. A bunch of TH names are
-- generated here and become part of the state, as they are used in
-- multiple places.
--
-- TODO _qTermTyNames: use collectSymbT ?
--
-- TODO call multiple times with different grammars will be a requirement
-- soon

newGen2 :: Grammar -> Q [Dec]
newGen2 g = do
  let _qGrammar = g
  _qMTyName     <- newName "m"
  _qElemTyName  <- newName "s"
  _qRetTyName   <- newName "r"
  _qTermTyNames <- M.fromList <$> (mapM (\t -> (t,) <$> newName t) $ g^..tsyms.folded.symb.folded.tnName)
  _qSVarTyNames <- M.fromList <$> (mapM (\n -> (n,) <$> newName (n^..symb.folded.tnName.folded)) $ collectSymbN g)
  evalStateT newGenM def{_qGrammar, _qMTyName, _qElemTyName, _qRetTyName, _qTermTyNames, _qSVarTyNames}

-- | Actually create signature, grammar, inline pragma.

newGenM :: TQ [Dec]
newGenM = do
  -- create signature
  sig <- signature
  -- create grammar
  gra <- grammar
  -- create inlining code
  inl <- use qGrammarName >>= \gname -> lift $ pragInlD gname Inline FunLike AllPhases
  return [sig,gra,inl]

