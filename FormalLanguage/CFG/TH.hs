
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

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Exception (assert)
import Control.Lens hiding (Strict, (...), outside)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict as M
import Control.Monad.Trans.Class
import Data.Char (toUpper,toLower)
import Data.Default
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (intersperse,nub,nubBy,groupBy)
import Data.Maybe
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Vector.Fusion.Stream.Monadic (Stream)
import Debug.Trace
import GHC.Exts (the)
import Language.Haskell.TH hiding (dataD)
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified GHC.TypeLits as Kind
import Text.Printf

import ADPfusion.Core ( (%), (|||), (...), (<<<) )
import Data.PrimitiveArray (Z(..), (:.)(..))
import qualified ADPfusion.Core as ADP

import FormalLanguage.CFG.Grammar
import FormalLanguage.CFG.PrettyPrint.ANSI
import FormalLanguage.CFG.TH.Internal



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
  , _qAttribFuns          :: M.Map [AttributeFunction] VarStrictType -- ^ map from the composed name to the template haskell attribute function @(Var,Strict,Type)@ (functions are currently stored as @[String]@ in @Grammar.hs@
  , _qChoiceFun           :: VarStrictType                -- ^ the choice function
  -- syntactic variables
  , _qPartialSyntVarNames :: M.Map Symbol Name            -- ^ syntactic-id to var name -- partially applied table / syntactic
  , _qInsideSyntVarNames  :: M.Map Symbol Name            -- ^ for outside grammars, these are the var-names for inside syn-vars
  , _qFullSyntVarNames    :: M.Map Symbol Name            -- ^ type variable names for the fully applied grammar body / where part
  -- everything on terminals
  , _qTermAtomVarNames    :: M.Map (String,Int) Name      -- ^ (Term-id,Dimension) to var name
  , _qTermAtomTyNames     :: M.Map (String,Int) Name      -- ^ the type name for each unique terminal symbol (that is: the scalar terminals in each dimension)
  , _qTermSymbExp         :: M.Map Symbol (Type,Exp)      -- ^ associate a terminal @Symb@ with a complete @Type@ and @Exp@
  , _qPrefix              :: String                       -- ^ prefix for attribute functions
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
    , _qInsideSyntVarNames  = error "def / insidesyntvarnames"
    , _qPrefix              = error "def / prefix"
    }

-- | The type of our little stateful @Q@ computations

type TQ z = StateT CfgState Q z



-- * TH functions

-- | Entry point for generation of @Grammar@ and @Signature@ code. Will
-- also stuff the 'Grammar' into the state data. A bunch of TH names are
-- generated here and become part of the state, as they are used in
-- multiple places.

thCodeGen :: Int -> Grammar -> Q [Dec]
thCodeGen prefixLen g = do
  let _qGrammar = g
  _qMTyName             <- newName "m"
  _qElemTyName          <- newName "s"
  _qRetTyName           <- newName "r"
  _qTermAtomTyNames     <- M.fromList <$> (mapM (\(name,tape) -> ((name,tape),) <$> newNameTerm "t" name tape) $ terminalsWithTape g) -- g^..termvars.folded.name.getSteName)
  _qPartialSyntVarNames <- M.fromList <$> (mapM (\n -> (n,) <$> newName ("s_" ++ (n^..getSymbolList.folded.name.getSteName.folded))) $ uniqueSyntacticSymbols g)
  _qInsideSyntVarNames  <- M.fromList <$> (mapM (\n -> (n,) <$> newName ("i_" ++ (n^..getSymbolList.folded.name.getSteName.folded))) $ uniqueSynTermSymbols   g)
  let _qPrefix          =  over _head toLower $ take prefixLen (g^.grammarName)
  let ls = (nub . map _lhs . S.elems) $ g^.rules
  let synKeys  = (filter (`elem` ls) . M.keys) _qPartialSyntVarNames
  bodySynNames  <- sequence [ (n,) <$> (newName $ "ss_" ++ concat k) | n <- synKeys, let k = n^..getSymbolList.folded.name.getSteName ]
  let _qFullSyntVarNames = M.fromList bodySynNames
  -- TODO inside synvars in outside context
  evalStateT codeGen def{_qGrammar, _qMTyName, _qElemTyName, _qRetTyName, _qTermAtomTyNames, _qPartialSyntVarNames, _qInsideSyntVarNames, _qPrefix, _qFullSyntVarNames}

-- | Actually create signature, grammar, inline pragma.

codeGen :: TQ [Dec]
codeGen = do
  g ← use qGrammar
  -- build up the terminal symbol lookup
  qTermAtomVarNames <~ M.fromList <$> dimensionalTermSymbNames
  qTermSymbExp      <~ M.fromList <$> (mapM grammarTermExpression =<< uniqueTerminalSymbols <$> use qGrammar)
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
  return $ toList sig ++ [gra,inl]

-- | Create the signature. Will also set the signature name.
--
-- TODO check if signature has already been emitted (from inside, say). If so,
-- don't do anything. This goes by signature name.

signature :: TQ (Maybe Dec)
signature = do
  g ← use qGrammar
  let gName = case g^.outside of
                Inside → g^.grammarName
                Outside gI → gI^.grammarName
  -- we can not lookup signatures in the environment, because everything is emitted in one go...
  -- hence we query the environment if such a signature has already been emitted...
  -- lkupName ← lift $ lookupValueName ("Sig" ++ gName)
  -- lift . runIO $ print (gName, lkupName)
  -- case lkupName of
  --   Just theName → do
  --     qSigName .= theName
  --     return Nothing
  --   Nothing → do
  case g^.outside of
    Outside gI → do
      lift . runIO $ putStrLn "WARNING: using workaround for Inside/Outside sharing which REQUIRES emitting the inside grammar!"
      qSigName .= (mkName $ "Sig" ++ gI^.grammarName)
      return Nothing
    Inside → do
      gType     ← use (qGrammar.outside)
      m         <- use qMTyName
      x         <- use qElemTyName
      r         <- use qRetTyName
      termNames <- use qTermAtomTyNames
      sigName   <- (mkName . ("Sig" ++)) <$> use (qGrammar.grammarName)
      fs        <- use qAttribFuns
      h         <- use qChoiceFun
      qSigName .= sigName
      lift $ Just <$> dataD (cxt [])
                   sigName
                   (PlainTV m () : PlainTV x () : PlainTV r () : (map (`PlainTV` ()) $ termNames^..folded))
                   [recC sigName ((map return $ fs^..folded) ++ [return h])]

-- | The grammar requires three types of arguments. First we need to bind
-- an algebra. Then we bind a list of non-terminals. Finally we bind a list
-- of terminals.
--
-- Once this function is called, it will print out the order of arguments!
--
-- TODO how about we wrap the non-terminals and terminals each in a tuple?

grammarArguments :: TQ [PatQ]
grammarArguments = do
  g       <- use qGrammar
  signame <- use qSigName
  h       <- use qChoiceFun
  fs      <- use qAttribFuns
  tavn    <- use qTermAtomVarNames
  psyn    <- use qPartialSyntVarNames
  isyn    <- use qInsideSyntVarNames
  -- bind algebra
  let alg = recP signame [ fieldPat n (varP n) | (n,_,_) <- h:(fs^..folded) ]
  -- bind partially applied non-terminals
  let syn = [ bangP $ varP s | s <- psyn^..folded ]
  -- bind fully applied non-terminals
  let isn = [ bangP $ varP s | s <- isyn^..folded ]
  -- bind terminals
  let ter = [ bangP $ varP t | t <- tavn^..folded ]
  --
  gname <- showName <$> use qGrammarName
  let ppSynt [x] = annotate (color Red) $ pretty x
      ppSynt xs  = list $ map (ppSynt . (:[])) xs
      ppTerm (n,k) = {- annotate (color Yellow) . -} pretty $ (printf "%s,%d" n k :: String)
      pp = annotate (colorDull Green) $ pretty (printf "%s $ALGEBRA" gname :: String)
      sy = encloseSep "   " mempty "  " (runReader (mapM symbolDoc $ M.keys psyn) g)
      iy = if M.null isyn then "" else encloseSep "   " mempty "  " (runReader (mapM symbolDoc $ M.keys isyn) g)
      te = encloseSep "   " mempty "  " (map (\s -> ppTerm $ s)                      $ M.keys tavn)
  lift . runIO . printDoc $ pp <> sy <> iy <> te <> hardline
  return $ alg : syn ++ isn ++ ter

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
  {-
  ls <- (nub . map _lhs . S.elems) <$> use (qGrammar.rules)
  synKeys       <- (filter (`elem` ls) . M.keys) <$> use qPartialSyntVarNames
  bodySynNames  <- lift $ sequence [ (n,) <$> (newName $ "ss_" ++ concat k) | n <- synKeys, let k = n^..getSymbolList.folded.name.getSteName ]
  qFullSyntVarNames .= M.fromList bodySynNames
  -}
  bodySynNames <- M.toList <$> use qFullSyntVarNames
  -- TODO now we actually need to *ALSO* add symbols for the inside stuff,
  -- if this is an outside grammar.
  mapM grammarBodySyn bodySynNames

-- | Fully bind each 'Symb' (which is partially applied, coming in as an
-- argument in the grammar) to the correct right-hand side.

grammarBodySyn :: (Symbol,Name) -> TQ DecQ
grammarBodySyn (s,n) = do
  hname <- use (qChoiceFun._1)
  partial <- use qPartialSyntVarNames
  ix <- lift $ newName "ix"
  -- all rules that have @s@ on the left-hand side
  fs <- filter ((s==) . _lhs) . S.elems <$> use (qGrammar.rules)
  rs <- mapM grammarBodyRHS fs
  let rhs = assert (not $ null rs) $
            appE ( uInfixE (foldl1 (\acc z -> uInfixE acc [| (|||) |] z) rs)
                           [| (...) |]
                           (varE hname) )
                 (varE ix)
  let sname = M.findWithDefault (error $ "grammarBodySyn: name not found for: " ++ show s) s partial
  -- use @TW@ to combine the table @varE sname@ and the rule RHS @lamE...@
  let bdy = [| ADP.TW $(varE sname) $(lamE [varP ix] rhs) |]
  -- return $ valD (varP n) (normalB $ appE (varE $ M.findWithDefault (error "grammarBodySyn") s partial) (lamE [varP ix] rhs)) []
  return $ valD (varP n) (normalB bdy) []

-- | Build up the rhs for each rule.
--
-- Requires using the fully bound syntactic variable name!

grammarBodyRHS :: Rule -> TQ ExpQ
grammarBodyRHS (Rule _ f rs) = do
  -- bundle up terminals and non-terminals
  terms        <- use qTermSymbExp
  synNames     <- use qFullSyntVarNames -- just the name of the fully applied symbol
  synTermNames <- use qInsideSyntVarNames
  let fragmentSynVar :: Symbol -> Maybe Name
      fragmentSynVar s@(view getSymbolList -> [SynVar _ _ n k]) | n>1 && k<n = M.lookup (splitToFull s) synNames
      fragmentSynVar _ = Nothing
  let finalSynVar :: Symbol -> Maybe Name
      finalSynVar s@(view getSymbolList -> [SynVar _ _ n k]) | n>1 && k==n = M.lookup (splitToFull s) synNames
      finalSynVar _ = Nothing
  let genSymbol :: Symbol -> ExpQ
      -- | If, for whatever reason, we have an empty symbol
      genSymbol (_getSymbolList -> []) = error "empty genSymbol"
      -- | if we deal with terminals
      genSymbol ((`M.lookup` terms) -> Just (_,v)) = return v
      -- | if we deal with usual syntactic vars
      genSymbol ((`M.lookup` synNames) -> Just n) = varE n
      -- | usual syntactic terminals
      genSymbol ((`M.lookup` synTermNames) -> Just n) = varE n
      -- | if we deal with split synvars and have a fragment
      genSymbol (fragmentSynVar -> Just n) = let p = show n in [| ADP.split (ADP.Proxy :: ADP.Proxy ($(litT $ strTyLit p) :: Kind.Symbol)) (ADP.Proxy :: ADP.Proxy ADP.Fragment) $(varE n) |]
      -- | if we deal with split synvars and have a final split
      genSymbol (finalSynVar    -> Just n) = let p = show n in [| ADP.split (ADP.Proxy :: ADP.Proxy ($(litT $ strTyLit p) :: Kind.Symbol)) (ADP.Proxy :: ADP.Proxy ADP.Final   ) $(varE n) |]
      -- | single-tape synvars in a multi-tape setting
      genSymbol s
        | isSynStacked s = foldl go [|ADP.M|] $ _getSymbolList s
        where go acc Deletion = [| $(acc) ADP.:| ADP.Deletion |]
              go acc sv
                | Just n <- M.lookup (SynLike [sv]) synNames = [| $(acc) ADP.:| $(varE n) |]
                | otherwise = error $ "genSymbol:stacked: " ++ show (s,synTermNames)
      -- | catch-all error
      genSymbol s = error $ "genSymbol: " ++ show s
  let rhs = assert (not $ null rs) $ foldl1 (\acc z -> uInfixE acc (varE '(%)) z) . map genSymbol $ rs
  -- apply evaluation function
  Just (fname,_,_) <- use (qAttribFuns . at f)
  return $ appE (appE (varE '(<<<)) (varE $ fname)) rhs

-- | Terminal symbols are usually compound types, built up from different
-- terminals a la @M :| t1 :| t2 :| t3@. We here build up the type of these
-- and their expression.

grammarTermExpression :: Symbol -> TQ (Symbol, (Type,Exp))
grammarTermExpression s = do
  ttypes <- use qTermAtomTyNames
  tavn <- use qTermAtomVarNames
  elemTyName <- use qElemTyName
  synNames     <- use qFullSyntVarNames -- just the name of the fully applied symbol
  g <- use qGrammar
  let genType :: Int -> [SynTermEps] -> TypeQ
      genType tape z
        | [Deletion]      <- z = [t| () |]
        | [Epsilon _]     <- z = [t| () |]
        | [Term tnm tidx] <- z
        , Just v <- M.lookup (tnm^.getSteName,tape) ttypes = varT v -- single dimension only, set dim to 0
        | [Term tnm tidx] <- z = varT elemTyName
        | xs              <- z = foldl (\acc (tape',z) -> [t| $acc :. $(genType tape' [z]) |]) [t| Z |] (zip [0..] xs)
  let genSingleExp :: Int -> SynTermEps -> ExpQ
      genSingleExp _ Deletion = [| ADP.Deletion |]
      genSingleExp _ (Epsilon Global) = [| ADP.Epsilon @Global |]
      genSingleExp _ (Epsilon Local) = [| ADP.Epsilon @Local |]
      genSingleExp _ (((`M.lookup` synNames) . SynLike . (:[])) -> Just n) = error $ show n
      genSingleExp k (Term tnm tidx)
        | Just n <- M.lookup (tnm^.getSteName,k) tavn = varE n
        -- TODO this one is dangerous but currently necessary for split
        -- systems
        | Just n <- M.lookup (tnm^.getSteName,k) tavn = varE n
        | otherwise = error $ show ("genSingleExp:Term: ",k,tnm,tidx, tavn)
      genSingleExp _ err      = error $ "genSingleExp: " ++ show (s,err)
  let genExp :: [SynTermEps] -> ExpQ
      genExp z
        | [Deletion]      <- z = [| ADP.Deletion |] -- TODO ???
        | [Epsilon Global]      <- z = [| ADP.Epsilon @Global |]
        | [Epsilon Local]      <- z = [| ADP.Epsilon @Local |]
        | [Term tnm tidx] <- z
        , Just v <- M.lookup (tnm^.getSteName,0) tavn = varE v
        | xs              <- z = foldl (\acc (k,z) -> [| $acc ADP.:| $(genSingleExp k z) |])
                                        [| ADP.M |] $ zip [0..] xs
  ty <- lift . genType 0 $ s^.getSymbolList
  ex <- lift . genExp  $ s^.getSymbolList
  return (s, (ty,ex))

-- | Given a grammar, gives us each terminal on each tape.

terminalsWithTape :: Grammar -> [(String,Int)]
terminalsWithTape = map go . filter isTerm . uniqueTermsWithTape -- uniqueBindableTermsWithTape
  where go (t,d) = (t^.name.getSteName,d^.getTape)
        isTerm (Term{},_) = True
        isTerm _          = False

-- | Each terminal symbol is bound to some input. Since we might have the
-- same name in different dimensions, we now explicitly annotate with
-- a dimensional index. This means that each atomic terminal is annotated
-- with the corresponding dimension.

dimensionalTermSymbNames :: TQ [((String,Int),Name)]
dimensionalTermSymbNames = do
  g <- use qGrammar
  ys <- forM (terminalsWithTape g) $ \(name,tape) -> do
          ( (name,tape) , ) <$> (lift $ newNameTerm "term" name tape)
  return ys

--newNameTerm :: String -> Int -> TQ Name
newNameTerm prefix name tape = newName $ prefix ++ "_" ++ name ++ "_" ++ show tape ++ "_"

-- | Build the full grammar. Generate a name (the grammar name prefixed
-- with a @"g"@), the arguments, and the body of the grammar.

grammar :: TQ Dec
grammar = do
  gn <- (mkName . ("g" ++) . _grammarName) <$> use qGrammar
  qGrammarName .= gn
  args         <- grammarArguments
  bodyWhere    <- grammarBodyWhere
  bodyNames    <- use qFullSyntVarNames
  let body     =  normalB . foldl (\acc z -> [| $acc :. $z |]) [|Z|] . map varE $ bodyNames^..folded
  lift $ funD gn [clause args body bodyWhere]

-- | Given a rule, create the name and type for the attribute function
-- being used.
--
-- TODO we currently assume that should we ever have @f <<< a b@ and also
-- @f <<< c d@ then the types match. We should actually group up rules by
-- function name, then take the set of rules with same name and check if
-- the types will actually match! However, one could argue that this should
-- way earlier in the grammar parser and not here.
--
-- TODO currently using @mkName@ instead of @newName@. This allows us to
-- share the signature between grammars, but might be problematic if names
-- overlap... We should combine the two generators for @g@ and @gO@ into
-- one. Then, we should be able to re-use names.

attributeFunctionType :: Rule -> TQ ([AttributeFunction],VarStrictType)
attributeFunctionType r = do
  let (f:fs) = r^..attr.folded
  elemTyName <- use qElemTyName
  terminal   <- use qTermSymbExp
  let argument :: Symbol -> TypeQ
      argument s
        -- split stuff has @()@ arg type
        | isSyntactic s
        , (SynLike [SynVar _ _ n k]) <- s
        , n>1 && k<n   = [t| () |]
        | isSyntactic s  = varT elemTyName
        | isSynTerm   s  = varT elemTyName
        | isTerminal  s  = return . fst $ terminal  M.! s
        | isSynStacked s = let go :: TypeQ -> SynTermEps -> TypeQ
                               go t Deletion = [t| $t :. () |]
                               go t SynVar{} = [t| $(t) :. $(varT elemTyName) |]
                               go t sv = error $ show sv
                           in  foldl go [t|Z|] $ _getSymbolList s
        | otherwise     = error $ "argument: " ++ show s
  prefix <- use qPrefix
  let attrFun = over _head toLower (f^.getAttr) ++ concatMap (over _head toUpper) (fs^..folded.getAttr) -- TODO mkName ???
  nm <- lift $ (return . mkName) $ if null prefix
                                      then attrFun
                                      else prefix ++ over _head toUpper attrFun
  tp <- lift $ foldr appT (varT elemTyName) $ map (appT arrowT . argument) $ r^.rhs
  ns <- lift $ bang noSourceUnpackedness noSourceStrictness
  return (f:fs, (nm,ns,tp))

-- | Build the choice function. Basically @Stream m s -> m r@.

choiceFunction :: TQ VarStrictType
choiceFunction = do
  elemTyName <- use qElemTyName
  retTyName  <- use qRetTyName
  mTyName    <- use qMTyName
  let args = AppT ArrowT $ AppT (AppT (ConT ''Stream) (VarT mTyName)) (VarT elemTyName)
  let rtrn = AppT (VarT mTyName) (VarT retTyName)
  prefix <- use qPrefix
  let hFun = if null prefix then "h" else prefix ++ "H"
  ns <- lift $ bang noSourceUnpackedness noSourceStrictness
  return (mkName hFun, ns, AppT args rtrn)

