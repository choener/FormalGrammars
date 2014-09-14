
-- | This module provides the functionality for automatic calculation of
-- outside grammars from their inside progenitors.

module FormalLanguage.CFG.Outside where

import           Data.List (inits,tails,nub)
import           Control.Lens
import qualified Data.Set as S

import FormalLanguage.CFG.Grammar



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
