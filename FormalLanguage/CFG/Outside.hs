
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides the functionality for automatic calculation of
-- outside grammars from their inside progenitors.
--
-- TODO If we already have an inside rule: @S -> A | B | C@ with inside
-- syntactic variable @S@ whose sole
-- purpose is to collect results, than we don't need an extra symbol for
-- Outside. What happens if this is not the case?

module FormalLanguage.CFG.Outside where

import           Data.List (inits,tails,nub)
import           Control.Lens hiding (Index,outside)
import qualified Data.Set as S
import           Data.Maybe (catMaybes)

import FormalLanguage.CFG.Grammar



-- | Given an inside grammar, return @Just@ an outside grammar, otherwise
-- return @Nothing@.

outsideFromInside :: Grammar -> Maybe Grammar
outsideFromInside g
  | g^.outside = Nothing
  -- TODO in theory, we should now check if we are at most context-free.
  -- (linear grammars are context-free as well).
  -- | not $ isContextFree g = Nothing
  | otherwise = Just $ Grammar {..}
  where _outside = True
        _rules   = S.fromList . concatMap genOutsideRules $ g^..rules.folded

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
  where go xs (h:ys)
          | isTerminal h = error "TODO these don't generate a rule -- unless we have an epsilon"
          | otherwise  = Just $ Rule (outsideSymb h) (outsideFun f) (xs ++ [outsideSymb l] ++ ys)
        outsideSymb s = undefined -- TODO simple lookup of inside name -> outside name!
        outsideFun  s = undefined -- this one should be @id@ if the algebra types are isomorphic ...

-- | If necessary add a special "start" rule to the set of rules.

-- | Take a grammar and transform it into an outside grammar. If the given
-- grammar is already in outside form, the grammar is returned as is.

toOutside :: Grammar -> Grammar
toOutside g
  | g^.outside = g
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

