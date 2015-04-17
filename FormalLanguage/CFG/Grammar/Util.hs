
-- | Collection of small helper functions for grammars.

module FormalLanguage.CFG.Grammar.Util where

import Control.Lens hiding (Index,index)
import Data.Tuple (swap)
import Data.List (sort,nub)

import FormalLanguage.CFG.Grammar.Types



-- | @Term@, @Deletion@, and @Epsilon@ all count as terminal symbols.

isTerminal :: Symbol -> Bool
isTerminal = allOf folded (\case (SynVar _ _) -> False; (SynTerm _ _) -> False; _ -> True) . _getSymbolList

-- | @Term@, and @Epsilon@ are terminal symbols that can be bound.

isBindableTerminal :: Symbol -> Bool
isBindableTerminal = allOf folded (\case (Term _ _) -> True; (Epsilon _) -> True; _ -> False) . _getSymbolList

-- | Only @SynVar@s are non-terminal.

isSyntactic :: Symbol -> Bool
isSyntactic = allOf folded (\case (SynVar _ _) -> True; _ -> False) . _getSymbolList

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
  where ls@(l:_) = map (length . _getSymbolList) . filter isTerminal $ g^.rules.folded.rhs

-- | Extract single-tape terminals together with their tape dimension.

uniqueTermsWithTape :: Grammar -> [(SynTermEps , Tape)]
uniqueTermsWithTape = nub . sort                              -- cleanup
                    . map swap                                -- swap the index to the second position
                    . concatMap (zip [0..] . _getSymbolList)  -- combine single-tape terminals with tape indices
                    . uniqueTerminalSymbols

-- | Extract single-tape bindable terminals together with their tape dimension.

uniqueBindableTermsWithTape :: Grammar -> [(SynTermEps , Tape)]
uniqueBindableTermsWithTape = nub . sort                              -- cleanup
                            . map swap                                -- swap the index to the second position
                            . concatMap (zip [0..] . _getSymbolList)  -- combine single-tape terminals with tape indices
                            . uniqueBindableTerminalSymbols

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

