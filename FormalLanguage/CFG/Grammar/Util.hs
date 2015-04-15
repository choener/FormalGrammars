
-- | Collection of small helper functions for grammars.

module FormalLanguage.CFG.Grammar.Util where

import Control.Lens hiding (Index,index)

import FormalLanguage.CFG.Grammar.Types


-- | @Term@, @Deletion@, and @Epsilon@ all count as terminal symbols.

isTerminal :: Symbol -> Bool
isTerminal = allOf folded (\case (SynVar _ _) -> False; _ -> True) . _getSymbolList



-- | Only @SynVar@s are non-terminal.

isSyntactic :: Symbol -> Bool
isSyntactic = allOf folded (\case (SynVar _ _) -> True; _ -> False) . _getSymbolList



-- | Epsilon-only symbols.

isEpsilon :: Symbol -> Bool
isEpsilon = allOf folded (\case Epsilon _ -> True; _ -> False) . _getSymbolList



-- | Dimension of the grammar. Rather costly, because we check for dimensional
-- consistency.

dim :: Grammar -> Int
dim g
  | null ls = error "no terminal symbol in grammar"
  | all (l==) ls = l
  | otherwise = error "inconsistent dimensionality"
  where ls@(l:_) = map (length . _getSymbolList) . filter isTerminal $ g^.rules.folded.rhs



-- |
--
-- TODO Currently a stub (original is in @.Grammar@ still. Want to have it
-- monadically, as the code is a mess.

normalizeStartEpsilon :: Grammar -> Grammar
normalizeStartEpsilon = error "normalizeStartEpsilon: (re-)write me"

