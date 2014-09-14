
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

-- | Template Haskell system for translating formal grammars into real
-- Haskell code based on ADPfusion.
--
-- If you want automatic algebra products, ADPfusion provides these.
-- @makeAlgebraProductH ['h] ''SigName@ where @SigName@ is the
-- auto-generted signature name, will generate the algebra product.
--
-- When will build the grammar, the types and variables are @newName@s
-- while attribute functions names are deterministic and potentially
-- non-unique.

module FormalLanguage.CFG.TH
  ( thCodeGen
  ) where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Exception (assert)
import           Control.Lens hiding (Strict, (...))
import           Control.Monad
import           Control.Monad.State.Strict as M
import           Control.Monad.Trans.Class
import           Data.Char (toUpper,toLower)
import           Data.Default
import           Data.Function (on)
import           Data.List (intersperse,nub,nubBy,groupBy)
import           Data.Maybe
import           Data.Vector.Fusion.Stream.Monadic (Stream)
import           GHC.Exts (the)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax hiding (lift)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Printf

import           ADP.Fusion ( (%), (|||), (...), (<<<) )
import           ADP.Fusion.None
import           Data.PrimitiveArray (Z(..), (:.)(..))
import qualified ADP.Fusion.Multi as ADP

import           FormalLanguage.CFG.Grammar
import           FormalLanguage.CFG.PrettyPrint.ANSI



-- * @StateT CfgState Q@ monad and wrapper for TH-based grammar
-- construction.

-- | The state we carry around. Contains all the bound names, and lookup
-- tables for functions, terminals and syntactic variables.
--
-- NOTE the defaults all start out undefined, making sure anything invalid
-- explodes in our face.
--
-- TODO if we allow multiple different choice function, we'll have to
-- extend @_qChoiceFun@

data CfgState = CfgState
  -- external stuff
  { _qGrammar             :: Grammar                      -- ^ the input grammar
  -- some basic names
  , _qElemTyName          :: Name                         -- ^ stream type name, as in @Stream m qElemTyName@
  , _qGrammarName         :: Name                         -- ^ the name for the body of the grammar
  , _qMTyName             :: Name                         -- ^ monad type name, as in @h :: Stream MTyName ...@
  , _qRetTyName           :: Name                         -- ^ choice return type name, as in @h :: Stream m qElemTyName -> m qRetTyName@
  , _qSigName             :: Name                         -- ^ the name of the signature type and data constructor, both (signatures need to have a single data constructor)
  -- attribute functions and choice; for now we allow only one choice
  -- function
  , _qAttribFuns          :: M.Map [String] VarStrictType -- ^ map from the composed name to the template haskell attribute function @(Var,Strict,Type)@ (functions are currently stored as @[String]@ in @Grammar.hs@
  , _qChoiceFun           :: VarStrictType                -- ^ the choice function
  -- syntactic variables
  , _qPartialSyntVarNames :: M.Map Symb Name              -- ^ syntactic-id to var name -- partially applied table / syntactic
  , _qFullSyntVarNames    :: M.Map Symb Name              -- ^ type variable names for the fully applied grammar body / where part
  -- everything on terminals
  , _qTermAtomVarNames    :: M.Map (String,Int) Name      -- ^ (Term-id,Dimension) to var name
  , _qTermAtomTyNames     :: M.Map String Name            -- ^ the type name for each unique terminal symbol (that is: the scalar terminals in each dimension)
  , _qTermSymbExp         :: M.Map Symb (Type,Exp)        -- ^ associate a terminal @Symb@ with a complete @Type@ and @Exp@
  }

makeLenses ''CfgState

instance Default CfgState where
  def = CfgState
    { _qGrammar             = error "def / grammar"
    , _qGrammarName         = error "def / grammarname"
    , _qElemTyName          = error "def / elemty"
    , _qRetTyName           = error "def / retty"
    , _qMTyName             = error "def / mty"
    , _qSigName             = error "def / signame"
    , _qTermAtomTyNames     = error "def / termtynames"
    , _qFullSyntVarNames    = error "def / synbodynames"
    , _qAttribFuns          = error "def / attribfuns"
    , _qChoiceFun           = error "def / choicefun"
    , _qTermSymbExp         = error "def / termsymbexp"
    , _qTermAtomVarNames    = error "def / termsingvarnames"
    , _qPartialSyntVarNames = error "def / partsyntvarnames"
    }

-- | The type of our little stateful @Q@ computations

type TQ z = StateT CfgState Q z



-- * TH functions

-- | Entry point for generation of @Grammar@ and @Signature@ code. Will
-- also stuff the 'Grammar' into the state data. A bunch of TH names are
-- generated here and become part of the state, as they are used in
-- multiple places.

thCodeGen :: Grammar -> Q [Dec]
thCodeGen g = do
  let _qGrammar = g
  _qMTyName             <- newName "m"
  _qElemTyName          <- newName "s"
  _qRetTyName           <- newName "r"
  _qTermAtomTyNames     <- M.fromList <$> (mapM (\t -> (t,) <$> newName ("t_" ++ t)) $ g^..tsyms.folded.symb.folded.tnName)
  _qPartialSyntVarNames <- M.fromList <$> (mapM (\n -> (n,) <$> newName ("s_" ++ n^..symb.folded.tnName.folded)) $ collectSymbN g)
  evalStateT codeGen def{_qGrammar, _qMTyName, _qElemTyName, _qRetTyName, _qTermAtomTyNames, _qPartialSyntVarNames}

-- | Actually create signature, grammar, inline pragma.

codeGen :: TQ [Dec]
codeGen = do
  -- build up the terminal symbol lookup
  qTermAtomVarNames <~ M.fromList <$> dimensionalTermSymbNames
  qTermSymbExp      <~ M.fromList <$> (mapM grammarTermExpression =<< collectSymbT <$> use qGrammar)
  -- create attribute function bindings (needed by signature and grammar)
  qAttribFuns <~ (use (qGrammar.rules) >>= (fmap M.fromList . mapM attributeFunctionType . S.toList))
  -- create choice function
  qChoiceFun <~ choiceFunction
  -- create signature
  sig <- signature
  -- create grammar
  gra <- grammar
  -- create inlining code
  inl <- use qGrammarName >>= \gname -> lift $ pragInlD gname Inline FunLike AllPhases
  return [sig,gra,inl]

-- | Create the signature. Will also set the signature name.

signature :: TQ Dec
signature = do
  m         <- use qMTyName
  x         <- use qElemTyName
  r         <- use qRetTyName
  termNames <- use qTermAtomTyNames
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
--
-- Once this function is called, it will print out the order of arguments!
--
-- TODO how about we wrap the non-terminals and terminals each in a tuple?

grammarArguments :: TQ [PatQ]
grammarArguments = do
  signame <- use qSigName
  h       <- use qChoiceFun
  fs      <- use qAttribFuns
  tavn    <- use qTermAtomVarNames
  psyn    <- use qPartialSyntVarNames
  -- bind algebra
  let alg = recP signame [ fieldPat n (varP n) | (n,_,_) <- h:(fs^..folded) ]
  -- bind partially applied non-terminals
  let syn = [ varP s | s <- psyn^..folded ]
  -- bind terminals
  let ter = [ varP t | t <- tavn^..folded ]
  --
  gname <- showName <$> use qGrammarName
  let ppSynt [x] = PP.red $ PP.text x
      ppSynt xs  = PP.list $ map (ppSynt . (:[])) xs
  let ppTerm (n,k) = PP.yellow . PP.text $ printf "%s,%d" n k
  let pp = PP.dullgreen $ PP.text (printf "%s $ALGEBRA" gname)
      --sy = PP.encloseSep (PP.text "   ") (PP.empty) (PP.text "  ") (map (\s -> ppSynt $ s^..symb.folded.tnName) $ M.keys psyn)
      sy = PP.encloseSep (PP.text "   ") (PP.empty) (PP.text "  ") (map symbolDoc $ M.keys psyn)
      te = PP.encloseSep (PP.text "   ") (PP.empty) (PP.text "  ") (map (\s -> ppTerm $ s)                      $ M.keys tavn)
  lift . runIO . printDoc $ pp PP.<> sy PP.<> te PP.<> PP.hardline
  --
  return $ alg : syn ++ ter

-- | Fully apply each partial syntactic variable to the corresponding
-- right-hand side. First, build up the map of fully applied names, then
-- associate each one.
--
-- @
--  Z:.s:.t
--    where s = s' (f >>> t ... h)
--          t = t' (f >>> s ... h)
-- @

grammarBodyWhere :: TQ [DecQ]
grammarBodyWhere = do
  ls <- (nub . map _lhs . S.elems) <$> use (qGrammar.rules)
  synKeys       <- (filter (`elem` ls) . M.keys) <$> use qPartialSyntVarNames
  bodySynNames  <- lift $ sequence [ (n,) <$> (newName $ "ss_" ++ concat k) | n <- synKeys, let k = n^..symb.folded.tnName ]
  qFullSyntVarNames .= M.fromList bodySynNames
  mapM grammarBodySyn bodySynNames

-- | Fully bind each 'Symb' (which is partially applied, coming in as an
-- argument in the grammar) to the correct right-hand side.

grammarBodySyn :: (Symb,Name) -> TQ DecQ
grammarBodySyn (s,n) = do
  hname <- use (qChoiceFun._1)
  partial <- use qPartialSyntVarNames
  ix <- lift $ newName "ix"
  -- all rules that have @s@ on the left-hand side
  fs <- (filter ((s==) . _lhs) . S.elems) <$> use (qGrammar.rules)
  rs <- mapM grammarBodyRHS fs
  let rhs = assert (not $ null rs) $
            appE ( uInfixE (foldl1 (\acc z -> uInfixE acc (varE '(|||)) z) rs)
                           (varE '(...))
                           (varE hname) )
                 (varE ix)
  return $ valD (varP n) (normalB $ appE (varE $ M.findWithDefault (error "grammarBodySyn") s partial) (lamE [varP ix] rhs)) []

-- | Build up the rhs for each rule.
--
-- Requires using the fully bound syntactic variable name!

grammarBodyRHS :: Rule -> TQ ExpQ
grammarBodyRHS (Rule _ f rs) = do
  -- bundle up terminals and non-terminals
  terms <- use qTermSymbExp
  synNames <- use qFullSyntVarNames -- just the name of the fully applied symbol
  let genSymbol s
        | isSymbT s = return . snd  $ M.findWithDefault (error "grammarBodyRHS") s terms
        | isSymbN s = return . VarE $ M.findWithDefault (error "grammarBodyRHS") s synNames
  let rhs = assert (not $ null rs) $ foldl1 (\acc z -> uInfixE acc (varE '(%)) z) . map genSymbol $ rs
  -- apply evaluation function
  Just (fname,_,_) <- use (qAttribFuns . at f)
  return $ appE (appE (varE '(<<<)) (varE fname)) rhs

-- | Terminal symbols are usually compound types, built up from different
-- terminals a la @M :> t1 :> t2 :> t3@. We here build up the type of these
-- and their expression.

grammarTermExpression :: Symb -> TQ (Symb, (Type,Exp))
grammarTermExpression s = do
  ttypes <- use qTermAtomTyNames
  tavn <- use qTermAtomVarNames
  let genType :: Symb -> TypeQ
      genType z
        | Symb Outside _ <- z = error $ printf "terminal symbol %s with OUTSIDE annotation!\n" (show z)
        | Symb _  [E]   <- z = [t| () |]
        | Symb _  [T t] <- z = varT $ ttypes M.! t
        | Symb io xs    <- z = foldl (\acc z -> [t| $acc :. $(genType $ Symb io [z]) |]) [t| Z |] xs
  let genExp :: Symb -> ExpQ
      genExp z
        | Symb Outside _ <- z = error $ printf "terminal symbol %s with OUTSIDE annotation!\n" (show z)
        | Symb _ [E]   <- z = [| None |]
        | Symb _ [T t] <- z = varE $ tavn M.! (t,0)
        | Symb _ xs    <- z = foldl (\acc (k,z) -> [| $acc ADP.:> $(case z of {E -> [| None |] ; T t -> varE $ tavn M.! (t,k)}) |]) [| ADP.M |] $ zip [0..] xs
  ty <- lift $ genType s
  ex <- lift $ genExp  s
  return (s, (ty,ex))

-- | Each terminal symbol is bound to some input. Since we might have the
-- same name in different dimensions, we now explicitly annotate with
-- a dimensional index. This means that each atomic terminal is annotated
-- with the corresponding dimension.

dimensionalTermSymbNames :: TQ [((String,Int),Name)]
dimensionalTermSymbNames = do
  g <- use qGrammar
  let xs = collectSymbT g
  let maxd = subtract 1 . the . map (length . view symb) $ xs
  ys <- forM [0..maxd] $ \d ->
        forM (filter isT . nub $ xs^..folded.symb.ix d) $ \s -> do
        ((s^.tnName,d),) <$> (lift $ newName $ ("lol" ++ s^.tnName) ++ show d)
  return $ concat ys

-- | Build the full grammar. Generate a name (the grammar name prefixed
-- with a @"g"@), the arguments, and the body of the grammar.

grammar :: TQ Dec
grammar = do
  gname <- (mkName . ("g" ++)) <$> use (qGrammar.name)
  qGrammarName .= gname
  args         <- grammarArguments
  bodyWhere    <- grammarBodyWhere
  bodyNames    <- use qFullSyntVarNames
  let body     =  normalB . foldl (\acc z -> [| $acc :. $z |]) [|Z|] . map varE $ bodyNames^..folded
  lift $ funD gname [clause args body bodyWhere]

-- | Given a rule, create the name and type for the attribute function
-- being used.
--
-- TODO we currently assume that should we ever have @f <<< a b@ and also
-- @f <<< c d@ then the types match. We should actually group up rules by
-- function name, then take the set of rules with same name and check if
-- the types will actually match! However, one could argue that this should
-- way earlier in the grammar parser and not here.

attributeFunctionType :: Rule -> TQ ([String],VarStrictType)
attributeFunctionType r = do
  let (f:fs) = r^.fun
  elemTyName <- use qElemTyName
  terminal   <- use qTermSymbExp
  let argument :: Symb -> Type
      argument s
        | isSymbN s = VarT elemTyName
        | isSymbT s = fst $ terminal  M.! s
  nm <- lift $ newName $ over _head toLower f ++ concatMap (over _head toUpper) fs
  let tp = foldr AppT (VarT elemTyName) $ map (AppT ArrowT . argument) $ r^.rhs
  return (f:fs, (nm,NotStrict,tp))

-- | Build the choice function. Basically @Stream m s -> m r@.

choiceFunction :: TQ VarStrictType
choiceFunction = do
  elemTyName <- use qElemTyName
  retTyName  <- use qRetTyName
  mTyName    <- use qMTyName
  let args = AppT ArrowT $ AppT (AppT (ConT ''Stream) (VarT mTyName)) (VarT elemTyName)
  let rtrn = AppT (VarT mTyName) (VarT retTyName)
  return (mkName "h", NotStrict, AppT args rtrn)

