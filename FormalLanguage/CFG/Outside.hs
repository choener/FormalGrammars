
-- | This module provides the functionality for automatic calculation of
-- outside grammars from their inside progenitors.
--
-- TODO If we already have an inside rule: @S -> A | B | C@ with inside
-- syntactic variable @S@ whose sole
-- purpose is to collect results, than we don't need an extra symbol for
-- Outside. What happens if this is not the case?

module FormalLanguage.CFG.Outside where

import           Data.List (inits,tails,nub,sort)
import           Control.Lens hiding (Index,outside,indices)
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Maybe (catMaybes)
import           Data.Default
import qualified Data.Map as M

import FormalLanguage.CFG.Grammar



-- | Given an inside grammar, return @Just@ an outside grammar, otherwise
-- return @Nothing@.

outsideFromInside :: Grammar -> Maybe Grammar
outsideFromInside g
  | Outside _ <- g^.outside = Nothing
  -- TODO in theory, we should now check if we are at most context-free.
  -- (linear grammars are context-free as well).
  -- not $ isContextFree g = Nothing
  | otherwise = Just $ Grammar {..}
  where _outside     = Outside g
        _rules       = S.fromList $ epsrule : (concatMap genOutsideRules $ g^..rules.folded)
        _grammarName = "" -- will be set in the parser
        _params      = g^.params
        _indices     = g^.indices
        _synvars     = M.fromList $ [ (n,v) | v@(SynVar  n _ _ _) <- (_rules^..folded.lhs.getSymbolList.folded) ]
        _synterms    = M.fromList $ [ (n,v) | v@(SynTerm n _)     <- (_rules^..folded.rhs.folded.getSymbolList.folded) ]
        _termvars    = M.fromList $ [ (n,t) | t@(Term    n _)     <- (_rules^..folded.rhs.folded.getSymbolList.folded) ]
        _start       = case (findStartSymbols $ g^.rules) of
                         [s] -> s
                         xs  -> error $ "more than one epsilon rule in the source: " ++ show xs
        _write       = False
        epsfun       = case (filter (isEpsilon . head . _rhs) $ g^..rules.folded) of
                         [] -> error "grammar does not terminate with an epsilon"
                         (Rule _ f _ : _) -> f
        epsrule      = genEpsilonRule epsfun (g^.start)

-- | Given a single inside rule, create the outside rules.
--
-- TODO rules with only terminals on the RHS may need some consideration
-- (this INCLUDES epsilon rules!)
--
-- TODO How do I know what an epsilon rule is? I might actually have to say
-- in the formal language... actually this might work. say @e@ is a free
-- variable, but terminal: @X -> e@ has the epsilon form @X -> e \eps@
-- because there are only "non-epsilon" rhs terminals -- we don't know yet
-- that @e@ is epsilon. This generates the outside rule @S -> e X*@ which
-- is what we want, except for the superfluous @e@ on the rhs. Because this
-- generates an algebra type that is incompatible with the inside version,
-- users should not do this. We are probably save, if all rules FROM the
-- start symbol are of the form @S -> A | B | C@ and all terminal ending
-- rules are of the form @A -> \eps@ (i.e. rewrite @A -> c@ to @A -> c E@
-- and have @E -> eps@.

genOutsideRules :: Rule -> [Rule]
genOutsideRules (Rule l f rs) = catMaybes $ zipWith go (inits rs) (init $ tails rs)
  where go xs (h:ys)  -- @xs ++ [h] ++ ys@. We [h] the current element
          | isTerminal h = Nothing
          | otherwise  = Just $ Rule (outsideSymb h) (outsideFun f) (map toSynTerm xs ++ [outsideSymb l] ++ map toSynTerm ys)
        outsideFun  = id
        toSynTerm s
          -- TODO need to handle @SynVar n i s | s > 1@ !
          | isSyntactic s = over (getSymbolList . traverse) (\(SynVar n i s k) -> SynTerm n i) s
          | otherwise     = s

-- | Helper function that turns an inside symbol into an outside symbol.
-- Simply by attaching a @'@ (prime) symbol.

outsideSymb :: Symbol -> Symbol
outsideSymb = over (getSymbolList . traverse . name . getSteName) (++"'")

-- | 

genEpsilonRule :: [AttributeFunction] -> Symbol -> Rule
genEpsilonRule epsfun s = Rule (outsideSymb s) epsfun [(TermLike $ replicate (length $ s^.getSymbolList) $ Epsilon Global)]

-- | 

findStartSymbols :: Set Rule -> [Symbol]
findStartSymbols rs =  map (outsideSymb . _lhs) . filter (sing . _rhs) $ rs^..folded
  where sing [x] | isEpsilon x = True
        sing _                 = False

-- | If necessary add a special "start" rule to the set of rules.

-- | Take a grammar and transform it into an outside grammar. If the given
-- grammar is already in outside form, the grammar is returned as is.

toOutside :: Grammar -> Grammar
toOutside g
  | Outside _ <- g^.outside = g
  | Just o <- outsideFromInside g = o


{-

-- | Mechanically generate the @Outside@ grammar from a given @Inside@
-- grammar.
--
-- TODO clean up the resulting outside grammar where all symbols are killed
-- that are not needed. This means any syntactic variables from the inside
-- grammar, that are not used, are not retained. We need to consider
-- carefully if we should really do that, as we could just as well give all
-- symbols, making everything really mechanic in nature.

outsideFromInside :: Grammar -> Grammar
outsideFromInside g = Grammar term synv ins eps rls strt nm where
  term = g^.tsyms
  synv = S.fromList . filter (\(Symb io _) -> io==Outside) . filter isSymbN . nub $ (rls^..folded.lhs) ++ (rls^..folded.rhs.folded)
  ins  = S.fromList . filter (\(Symb io _) -> io==Inside ) . filter isSymbN . nub $ (rls^..folded.rhs.folded)
  eps  = g^.epsis
  rls  = S.fromList . concatMap (outsideRules g) $ g^..rules.folded
  strt = Nothing -- TODO the outside version of the inside start?
  nm   = (g^.name)

-- | Build the outside rules from inside ones.
--
-- TODO check wether the rule generation for the single terminal on the
-- right-hand side is correct.

outsideRules :: Grammar -> Rule -> [Rule]
outsideRules g (Rule l f [r]) | isSymbT r = [Rule (Symb Outside $ l^.symb) f [r]]
{-
outsideRules g (Rule l f [r]) | isSymbT r
  = let s = Symb Outside $ map (`N` Singular) n
        n = replicate (length $ l^.symb) "S"
    in  [Rule s f [Symb Outside $ l^.symb]]
-}
outsideRules g (Rule l f r) =
  [ Rule i' f (p ++ [i'] ++ s)
  | (p,i,s) <- zip3 (init $ inits r) r (tail $ tails r)
  , isSymbN i
  , let i' = Symb Outside $ i^.symb
  ]

-}

