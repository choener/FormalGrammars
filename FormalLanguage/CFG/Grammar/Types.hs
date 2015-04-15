
-- | The data types that define a CFG.

module FormalLanguage.CFG.Grammar.Types where

import           Control.Lens hiding (Index,index)
import           Data.String
import           Data.Default
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Encode the index of the syntactic or terminal variable.
--
-- In case of grammar-based indexing, keep @indexRange@ empty. The
-- @indexStep@ keeps track of any @+k@ / @-k@ given in the production
-- rules.
--
-- We allow indexing terminals now, too. When glueing together terminals,
-- one might want to be able to differentiate between terminals.

data Index = Index
  { _indexVar   :: String
  , _indexRange :: [String]
  , _indexStep  :: Int
  }
  -- TODO need a version, where we have figured out everything
  -- , i.e. replaced @i+2@ with, say, @1@ @(i==1+2 `mod` 3)@.
  -- Use the @_indexVar = j@ version in the set of syn-vars, but
  -- @_indexVar=x, x \in _indexRange@ in rules?
  deriving (Show,Eq,Ord)

makeLenses ''Index



-- | Newtype wrapper for symbol names.

newtype SymbolName = SymbolName { _getSymbolName :: String }
  deriving (Show,Eq,Ord,IsString)

makeLenses ''SymbolName



-- | The tape, a terminal operates on. Terminals on different tapes could
-- still have the same @SymbolName@ but different type and input!

newtype Tape = Tape { _getTape :: Int }
  deriving (Show,Eq,Ord)

makeLenses ''Tape



-- | Symbols, potentially with an index or more than one.

data SynTermEps
  -- | Syntactic variables.
  = SynVar
    { _name   :: SymbolName
    , _index  :: [Index]
    }
  -- syntactic terminals. Inside-synvars used in an outside context.
  | SynTerm
    { _name   :: SymbolName
    , _index  :: [Index]
    }
  -- | Regular old terminal symbol -- reads stuff from the input.
  | Term
    { _name   :: SymbolName
    , _index  :: [Index]
    , _tape   :: Tape
    }
  -- | This sym denotes the case, where we have an @Deletion@ terminal, i.e.
  -- something is matched to nothing. This is actually just a regular
  -- terminal symbol, we just treat it differently.
  | Deletion
  -- | Finally, a real epsilon. Again, these are somewhat regular terminal
  -- symbols, but it is important to be able to recognize these, when
  -- trying to create outside variants of our algorithms.
  | Epsilon
    { _name :: SymbolName
    }
  deriving (Show,Eq,Ord)

makeLenses ''SynTermEps
makePrisms ''SynTermEps



-- | The length of the list encodes the dimension of the symbol

newtype Symbol = Symbol { _getSymbolList :: [SynTermEps] }
  deriving (Show,Eq,Ord)

makeLenses ''Symbol

instance Monoid Symbol where
  (Symbol xs) `mappend` (Symbol ys) = Symbol $ xs ++ go (length xs) ys
    where go _ [] = []
          -- TODO the terminal case
          go k (y:ys) = y : go (k+1) ys
  mempty = Symbol []



-- | Production rules for at-most CFGs.

data Rule = Rule
  { _lhs  :: Symbol     -- ^ the left-hand side of the rule
  , _attr :: [String]   -- ^ the attribute for this rule
  , _rhs  :: [Symbol]   -- ^ the right-hand side with a collection of terminals and syntactic variables
  }
  deriving (Show,Eq,Ord)

makeLenses ''Rule

data Grammar = Grammar
  { _synvars      :: Map SymbolName SynTermEps          -- ^ regular syntactic variables, without dimension
  , _termsyns     :: Map SymbolName SynTermEps          -- ^ Terminal synvars are somewhat weird. They are used in Outside grammars, and hold previously calculated inside values.
  -- TODO store terminal symbols (i) without tape: once a symbol name is
  -- set, the symbol name denotes the same type over all tapes (ii) with
  -- tape; we can then have different types for the same name over
  -- different tapes. ... maybe (i) is better, giving the same meaning
  -- (type) to each use of the symbol. We can and do still bind the data
  -- differently for each tape.
  , _termvars     :: Map (SymbolName,Tape) SynTermEps   -- ^ regular terminal symbols
  , _epsvars      :: Map SymbolName SynTermEps          -- ^ terminal symbol names that denote @Epsilon@
  , _outside      :: Bool                               -- ^ Is this an outside grammar
  , _rules        :: Set Rule                           -- ^ set of production rules
  , _start        :: Symbol                             -- ^ start symbol
  , _params       :: Map String Index                   -- ^ any global variables
  , _grammarName  :: String                             -- ^ grammar name
  , _write        :: Bool                               -- ^ some grammar file requested this grammar to be expanded into code
  }
  deriving (Show)

instance Default Grammar where
  def = Grammar
    { _synvars      = M.empty
    , _termsyns     = M.empty
    , _termvars     = M.empty
    , _epsvars      = M.empty
    , _outside      = False
    , _rules        = S.empty
    , _start        = mempty
    , _params       = M.empty
    , _grammarName  = ""
    , _write        = False
    }

makeLenses ''Grammar

