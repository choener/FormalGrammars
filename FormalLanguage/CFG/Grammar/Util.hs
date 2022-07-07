
-- | Collection of small helper functions for grammars.

module FormalLanguage.CFG.Grammar.Util where

import Control.Lens hiding (Index,index)
import Data.Tuple (swap)
import Data.List (sort,nub,genericReplicate)

import FormalLanguage.CFG.Grammar.Types



-- | @Term@, @Deletion@, and @Epsilon@ all count as terminal symbols.

isTerminal :: Symbol -> Bool
isTerminal = allOf folded (\case SynVar{} -> False; (SynTerm _ _) -> False; _ -> True) . _getSymbolList

-- | @Term@, and @Epsilon@ are terminal symbols that can be bound.

isBindableTerminal :: Symbol -> Bool
isBindableTerminal = allOf folded (\case (Term _ _) -> True; _ -> False) . _getSymbolList

-- | Only @SynVar@s are non-terminal.

isSyntactic :: Symbol -> Bool
isSyntactic (SynLike ss) = allOf folded (\case SynVar{} -> True; _ -> False) ss
isSyntactic _ = False

-- | special case of single-tape synvar in multi-tape setting

isSynStacked :: Symbol -> Bool
isSynStacked (TermLike ts) = allOf folded (\case SynVar{} -> True; Deletion -> True; _ -> False) ts
isSynStacked _ = False

-- | true if we have a split synvar

isAllSplit :: Symbol -> Bool
isAllSplit = allOf folded (\case (SynVar _ _ n _) -> n>1 ; _ -> False) . _getSymbolList

-- | Set all @splitK@ values to @0@ for lookups.

splitK0 :: Symbol -> Symbol
splitK0 = set (getSymbolList . traverse . splitK) 0

-- | Take a split symbol and rewrite as full.

splitToFull :: Symbol -> Symbol
splitToFull (SynLike [SynVar s i n k]) = SynLike . genericReplicate n $ SynVar s i n 0

-- | Is this a syntactic terminal symbol?

isSynTerm :: Symbol -> Bool
isSynTerm = allOf folded (\case (SynTerm _ _) -> True; _ -> False) . _getSymbolList

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
  where ls@(l:_) = map (length . _getSymbolList) $ g^.rules.folded.rhs

-- | Extract single-tape terminals together with their tape dimension.

uniqueTermsWithTape :: Grammar -> [(SynTermEps , Tape)]
uniqueTermsWithTape = uniqueSynTermEpsWithTape . uniqueTerminalSymbols

-- | Extract single-tape bindable terminals together with their tape dimension.

uniqueBindableTermsWithTape :: Grammar -> [(SynTermEps , Tape)]
uniqueBindableTermsWithTape = uniqueSynTermEpsWithTape . uniqueBindableTerminalSymbols

-- |

uniqueSynVarsWithTape :: Grammar -> [(SynTermEps, Tape)]
uniqueSynVarsWithTape = uniqueSynTermEpsWithTape . uniqueSyntacticSymbols

-- |

uniqueSynTermsWithTape :: Grammar -> [(SynTermEps, Tape)]
uniqueSynTermsWithTape = uniqueSynTermEpsWithTape . uniqueSynTermSymbols

-- |

uniqueSynTermEpsWithTape :: [Symbol] -> [(SynTermEps, Tape)]
uniqueSynTermEpsWithTape = nub . sort                             -- cleanup
                         . map swap                               -- swap index to second position
                         . concatMap (zip [0..] . _getSymbolList) -- combine single-tape STEs with tape indices

-- | Return the nub list of terminal symbols. This includes @Deletion@
-- symbols, and might not be what you want. Check
-- 'uniqueBindableTerminalSymbols' too!

uniqueTerminalSymbols :: Grammar -> [Symbol]
uniqueTerminalSymbols = nub . sort . filter isTerminal . toListOf (rules.folded.rhs.folded)

-- |

uniqueBindableTerminalSymbols :: Grammar -> [Symbol]
uniqueBindableTerminalSymbols = nub . sort . filter isBindableTerminal . toListOf (rules.folded.rhs.folded)

-- | Return the nub list of syntactic symbols.

uniqueSyntacticSymbols :: Grammar -> [Symbol]
uniqueSyntacticSymbols g = nub . sort . filter isSyntactic $ g^..rules.folded.lhs

-- | Return the nub list of syntactic terminals.

uniqueSynTermSymbols :: Grammar -> [Symbol]
uniqueSynTermSymbols = nub . sort . filter isSynTerm . toListOf (rules.folded.rhs.folded)

-- |
--
-- TODO Currently a stub (original is in @.Grammar@ still. Want to have it
-- monadically, as the code is a mess.

normalizeStartEpsilon :: Grammar -> Grammar
normalizeStartEpsilon = error "normalizeStartEpsilon: (re-)write me"



-- | Left-linear grammars have at most one non-terminal on the RHS. It is the
-- first symbol.

isLeftLinear :: Grammar -> Bool
isLeftLinear g = allOf folded isll $ g^.rules where
  isll :: Rule -> Bool
  isll (Rule l _ []) = isSyntactic l
  isll (Rule l _ rs) = isSyntactic l && (allOf folded (not . isSyntactic) $ tail rs) -- at most one non-terminal

isRightLinear :: Grammar -> Bool
isRightLinear g = allOf folded isrl $ g^.rules where
  isrl :: Rule -> Bool
  isrl (Rule l _ []) = isSyntactic l
  isrl (Rule l _ rs) = isSyntactic l && (allOf folded (not . isSyntactic) $ init rs)

isLinear :: Grammar -> Bool
isLinear g = allOf folded isl $ g^.rules where
  isl :: Rule -> Bool
  isl (Rule l _ []) = isSyntactic l
  isl (Rule l _ rs) = isSyntactic l && (1 >= (length . filter isSyntactic $ rs))

