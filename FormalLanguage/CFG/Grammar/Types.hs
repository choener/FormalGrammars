
-- | The data types that define a CFG.

module FormalLanguage.CFG.Grammar.Types
  ( module FormalLanguage.CFG.Grammar.Types
  , LocalGlobal(..)
  ) where

import           Control.Lens hiding (Index,index)
import           Data.Data (Data,Typeable)
import           Data.Default
import           Data.Map.Strict (Map)
import           Data.Semigroup
import           Data.Set (Set)
import           Data.String
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Text.Printf

import           ADPfusion.Core.Term.Epsilon (LocalGlobal(..))



newtype IndexName = IndexName { _getIndexName :: String }
  deriving stock (Show,Eq,Ord,Data,Typeable)
  deriving newtype (IsString)

makeLenses ''IndexName

data IOP
  = IPlus     -- in rules
  | IMinus    -- in rules
  | IEq       -- in rules
  | INone     -- grammar-global
  | ISymbol   -- when creating the symbol, here @=n@ says to use @[0..n-1]@
  deriving (Show,Eq,Ord,Data,Typeable)

-- | Encode the index of the syntactic or terminal variable.
--
-- In case of grammar-based indexing, keep @indexRange@ empty. The
-- @indexStep@ keeps track of any @+k@ / @-k@ given in the production
-- rules.
--
-- We allow indexing terminals now, too. When glueing together terminals,
-- one might want to be able to differentiate between terminals.

data Index = Index
  { _indexName  :: IndexName
  , _indexHere  :: Integer
  , _indexOp    :: IOP
  , _indexRange :: [Integer]
  , _indexStep  :: Integer
  }
  -- TODO need a version, where we have figured out everything
  -- , i.e. replaced @i+2@ with, say, @1@ @(i==1+2 `mod` 3)@.
  -- Use the @_indexVar = j@ version in the set of syn-vars, but
  -- @_indexVar=x, x \in _indexRange@ in rules?
  deriving (Show,Eq,Ord,Data,Typeable)

makeLenses ''Index



-- | Newtype wrapper for symbol names.

newtype SymbolName = SymbolName { _getSteName ∷ String }
  deriving stock (Eq,Ord,Data,Typeable)
  deriving newtype (IsString)

instance Show SymbolName where
  show = _getSteName

makeLenses ''SymbolName



-- | The tape, a terminal operates on. Terminals on different tapes could
-- still have the same @SymbolName@ but different type and input!

newtype Tape = Tape { _getTape :: Int }
  deriving stock (Show,Eq,Ord,Data,Typeable)
  deriving newtype (Enum,Num)

makeLenses ''Tape



-- | Symbols, potentially with an index or more than one.

data SynTermEps
  -- | Syntactic variables.
  = SynVar
    { _name   :: SymbolName
    , _index  :: [Index]
    , _splitN :: Integer
    , _splitK :: Integer
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
    }
  -- | This sym denotes the case, where we have an @Deletion@ terminal, i.e.
  -- something is matched to nothing. This is actually just a regular
  -- terminal symbol, we just treat it differently.
  | Deletion
  -- | Finally, a real epsilon. Again, these are somewhat regular terminal
  -- symbols, but it is important to be able to recognize these, when
  -- trying to create outside variants of our algorithms.
  | Epsilon LocalGlobal
  deriving (Eq,Ord,Data,Typeable)

instance Show SynTermEps where
  show (SynVar n i sn sk) = printf "SynVar %s %s %d %d" (show n) (show i) sn sk
  show (SynTerm n i) = printf "SynTerm %s %s" (show n) (show i)
  show (Term n i) = printf "Term %s %s" (show n) (show i)
  show Deletion = "Deletion"
  show (Epsilon lg) = printf "Epsilon %s" (show lg)

makeLenses ''SynTermEps
makePrisms ''SynTermEps



-- | The length of the list encodes the dimension of the symbol. Forms a monoid
-- over dimensional concatenation.
--
-- TODO This should split between symbol objects that are to be treated as "termina-style" and those
-- to be treated as "syntactic"!

data Symbol
  = SynLike { _getSymbolList :: [SynTermEps] }
  | TermLike { _getSymbolList :: [SynTermEps] }
  deriving stock (Eq,Ord,Data,Typeable)
--  deriving (Ord,Semigroup,Monoid)

instance Show Symbol where
  show (SynLike ss) = "<"++show ss++">"
  show (TermLike ss) = "["++show ss++"]"

makeLenses ''Symbol



-- | The name of an attribute function

newtype AttributeFunction = Attr { _getAttr :: String }
  deriving stock (Eq,Ord,Data,Typeable)
  deriving newtype (IsString)

instance Show AttributeFunction where
  show = _getAttr

makeLenses ''AttributeFunction



-- | Production rules for at-most CFGs.

data Rule = Rule
  { _lhs  :: Symbol               -- ^ the left-hand side of the rule
  , _attr :: [AttributeFunction]  -- ^ the attribute for this rule
  , _rhs  :: [Symbol]             -- ^ the right-hand side with a collection of terminals and syntactic variables
  }
  deriving (Eq,Ord,Data,Typeable)

instance Show Rule where
  show (Rule lhs attr rhs) = printf "%s -> %s <<< %s" (show lhs) (show attr) (show rhs)

makeLenses ''Rule



-- | Indicate wether we are a handwritten @Inside@ grammar, or an @Outside@
-- grammar derived @fromInside@.

data DerivedGrammar
  = Inside
  -- ^ Indicates being Inside
  | Outside { _fromInside ∷ Grammar }
  -- ^ Indicates being Outside, with original Inside
  deriving (Show,Data,Typeable)

instance Eq DerivedGrammar where
  Inside == Inside = True
  Inside == Outside _ = False
  Outside _ == Inside = False
  Outside x == Outside y = _grammarName x == _grammarName y

isOutside (Outside _) = True
isOutside _           = False

instance Default DerivedGrammar where
  def = Inside



-- | Complete descrition of a grammar. In principle it would be enough to hold
-- @_rules@ and the @_start@ symbol name. We also store dimensionless names for
-- syntactiv variables, and terminals. This makes certain checks easier or
-- possible.
--
-- We store all single-tape symbol names dimensionless. This means that, for
-- terminals, symbols with the same name have the same tape. This is slightly
-- inconvenient for special applications (say Protein-DNA alignment) but one
-- can easily rename terminals.
--
-- TODO better way to handle indexed symbols?
--
-- TODO include "String" name to handle sharing signatures (and thereby
-- algebras!). This makes sense only when sharing the more complex signature,
-- until I start allowing signature merges.

data Grammar = Grammar
  { _synvars      :: Map SymbolName SynTermEps
    -- ^ regular syntactic variables, without dimension
  , _synterms     :: Map SymbolName SynTermEps
    -- ^ Terminal synvars are somewhat weird. They are used in Outside
    -- grammars, and hold previously calculated inside values.
  , _termvars     :: Map SymbolName SynTermEps
    -- ^ regular terminal symbols
  , _outside      :: DerivedGrammar
    -- ^ Is this an automatically derived outside grammar, if so provide @fromInside@.
  , _rules        :: Set Rule
    -- ^ set of production rules
  , _start        :: Symbol
    -- ^ start symbol
  , _params       :: Map IndexName Index
    -- ^ any global variables
  , _indices      :: Map IndexName Index
    -- ^ active indices
  , _grammarName  :: String
    -- ^ grammar name
  , _write        :: Bool
    -- ^ some grammar file requested this grammar to be expanded into code
    --
    -- TODO remove, we have an emission queue
  }
  deriving (Show,Data,Typeable)

instance Default Grammar where
  def = Grammar
    { _synvars      = M.empty
    , _synterms     = M.empty
    , _termvars     = M.empty
    , _outside      = def
    , _rules        = S.empty
    , _start        = SynLike mempty
    , _params       = M.empty
    , _indices      = M.empty
    , _grammarName  = ""
    , _write        = False
    }

makeLenses ''DerivedGrammar
makeLenses ''Grammar

