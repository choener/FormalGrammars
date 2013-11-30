{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{- LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | We define a simple domain-specific language for context-free languages.

module FormalLanguage.Parser where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Identity
import           Control.Monad.State.Class (MonadState (..))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict hiding (get)
import           Data.Default
import           Data.Either
import           Data.List (partition,sort,nub)
import           Data.Maybe (catMaybes,isJust)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import           Text.Parser.Expression
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Printf
import           Text.Trifecta
import           Text.Trifecta.Delta
import           Text.Trifecta.Result
import           Data.Tuple (swap)

import Debug.Trace

import FormalLanguage.Grammar



data Enumerated
  = Sing
  | ZeroBased Integer
--  | Enum      [String]
  deriving (Show)

-- | The 

data GrammarState = GrammarState
  { _nsys         :: M.Map String Enumerated
  , _tsys         :: S.Set String
  , _esys         :: S.Set String
  , _grammarNames :: S.Set String
  }
  deriving (Show)

instance Default GrammarState where
  def = GrammarState
          { _nsys = def
          , _tsys = def
          , _esys = def
          , _grammarNames = def
          }

makeLenses ''GrammarState

-- | Parse a single grammar.

grammar :: Parse Grammar
grammar = do
  reserveGI "Grammar:"
  name :: String <- identGI
  _nsyms <- S.fromList . concat <$> many nts
  _tsyms <- S.fromList . concat <$> many ts
  _epsis <- S.fromList <$> many epsP
  _start <- try (Just <$> startSymbol) <|> pure Nothing
  _rules <- (S.fromList . concat) <$> some rule
  reserveGI "//"
  grammarNames <>= S.singleton name
  return Grammar { .. }

-- | Start symbol. Only a single symbol may be given
--
-- TODO for indexed symbols make sure we actually have one index to start with.

startSymbol :: Parse Symb
startSymbol = do
  reserveGI "S:"
  name :: String <- identGI
  -- TODO go and allow indexed NTs as start symbols, with one index given
  -- return $ nsym1 name Singular
  return $ Symb [N name Singular]

-- | The non-terminal declaration "NT: ..." returns a list of non-terms as
-- indexed non-terminals are expanded.

nts :: Parse [Symb]
nts = do
  reserveGI "N:"
  name   <- identGI
  enumed <- option Sing $ braces enumeration
  let zs = expandNT name enumed
  nsys <>= M.singleton name enumed
  return zs

-- | expand set of non-terminals based on type of enumerations

expandNT :: String -> Enumerated -> [Symb]
expandNT name = go where
  go Sing          = [Symb [N name Singular]]
  go (ZeroBased k) = [Symb [N name (IntBased   z k)] | z <- [0..(k-1)]]
  --go (Enum es)     = [Symb [N name (Enumerated z es        )] | z <- es        ]

-- | Figure out if we are dealing with indexed (enumerable) non-terminals

enumeration =   ZeroBased <$> natural
--            <|> Enum      <$> sepBy1 identGI (string ",")

-- | Parse declared terminal symbols.

ts :: Parse [Symb]
ts = do
  reserveGI "T:"
  n <- identGI
  let z = Symb [T n]
  tsys <>= S.singleton n
  return [z]

-- | Parse epsilon symbols

epsP :: Parse TN
epsP = do
  reserveGI "E:"
  e <- identGI
  esys <>= S.singleton e
  return $ E e

-- | Parse a single rule. Some rules come attached with an index. In that case,
-- each rule is inflated according to its modulus (or more general the set of
-- indices indicated.
--
-- TODO add @fun@ to each PR

rule :: P m => m [Rule] -- Parse [Rule]
rule = do
  lhs <- parsePreNN
  reserveGI "->"
  fun :: String <- identGI
  reserveGI "<<<"
  rhs <- runUnlined $ some (try (lift $ parsePreNN) <|> (lift $ parsePreTT))
  whiteSpace
  s <- get
  error $ show $ generateRules s lhs rhs
  return [] --  $ generateRules s lhs rhs

-- | Actually create a rule given both lhs and rhs. This means we need to
-- expand rules according to what we allow.
--
-- TODO need to handle epsilons correctly

generateRules :: GrammarState -> PreSymb -> [PreSymb] -> ()
generateRules s lhs rhs = error $ show (is) where
  -- gives (index,NT) list; from (NT,(index,integer)) list
  is = id $ (lhs : rhs) ^.. folded.folded._OnlyIndexedPreN
  {-
  js = sequence $ map (expandIndex $ s^.nsys) is
  expandIndex ns (i,n) =
    let expand Sing          = error "expanded index on singular"
        expand (ZeroBased z) = [0 .. (z-1)]
    in  map (i,) . expand $ ns M.! n
  buildRule xs = (buildSymb xs lhs, map (buildSymb xs) rhs)
  buildSymb xs (PreT ss) = (PreT ss)
  buildSymb xs (PreN ss) = undefined
  -}

data IndexedPreN
  = NotIndexed
  | IndexedPreN String Integer
  deriving (Show,Eq,Ord)

_IndexedPreN :: Prism' IndexedPreN (String,Integer)
_IndexedPreN = prism (uncurry IndexedPreN) $ \case (IndexedPreN s i) -> Right (s,i)
                                                   other             -> Left  other

data PreTNE
  = PreN String IndexedPreN
  | PreT String
  | PreE String
  deriving (Show,Eq,Ord)

_PreN :: Prism' PreTNE (String,IndexedPreN)
_PreN = prism (uncurry PreN) $ \case (PreN s i) -> Right (s,i)
                                     other      -> Left  other

_OnlyIndexedPreN :: Prism' PreTNE (String,IndexedPreN)
_OnlyIndexedPreN = prism (uncurry PreN) $ \case (PreN s (IndexedPreN t i)) -> Right (s, IndexedPreN t i)
                                                other                      -> Left  other

_PreT :: Prism' PreTNE String
_PreT = prism PreT $ \case (PreT s) -> Right s
                           other    -> Left  other

_PreE :: Prism' PreTNE String
_PreE = prism PreE $ \case (PreE s) -> Right s
                           other    -> Left  other

type PreSymb = [PreTNE]

parsePreN :: P m => m PreTNE
parsePreN = use nsys >>= \ks -> (PreN <$> (choice . map symbol . M.keys $ ks) <*> parseIndexedPreN)

parsePreT :: P m => m PreTNE
parsePreT = PreT <$> (use tsys >>= choice . map symbol . S.elems)

parsePreE :: P m => m PreTNE
parsePreE = PreE <$> (use esys >>= choice . map symbol . S.elems)

parseIndexedPreN :: P m => m IndexedPreN
parseIndexedPreN = option NotIndexed (try . braces $ IndexedPreN <$> identGI <*> option 0 integer)

parsePreNN :: P m => m [PreTNE]
parsePreNN = do
  ns <- (:[]) <$> parsePreN <|> list (try parsePreN <|> parsePreE)
  guard (notNullOf (folded._PreN) ns) <?> "no non-terminal encountered"
  return ns

parsePreTT :: P m => m [PreTNE]
parsePreTT = do
  ts <- (:[]) <$> parsePreT <|> list (try parsePreT <|> parsePreE)
  guard (notNullOf (folded._PreT) ts) <?> "no terminal encountered"
  return ts

-- | Parses a list of a la @[a,b,c]@

list = brackets . commaSep



-- * Monadic Parsing Machinery

-- | Parser with 'GrammarState'

newtype GrammarParser m a = GrammarP { runGrammarP :: StateT GrammarState m a }
  deriving  ( Monad
            , MonadPlus
            , Alternative
            , Applicative
            , Functor
            , MonadState GrammarState
            , TokenParsing
            , CharParsing
            , Parsing
            , MonadTrans
            )

-- | Functions that parse using the 'GrammarParser'

type Parse  a = ( Monad m
                , MonadPlus m
                , TokenParsing m
                ) => GrammarParser m a

-- | Parsing where we stop at a newline (which needs to be parsed explicitly)

type ParseU a = (Monad m
                , MonadPlus m
                , TokenParsing m
                ) => Unlined (GrammarParser m) a

type P m = ( Monad m
           , MonadPlus m
           , Alternative m
           , Parsing m
           , TokenParsing m
           , MonadState GrammarState m
           )

-- | grammar identifiers

grammarIdentifiers = set styleReserved rs emptyIdents where
  rs = H.fromList ["Grammar:", "N:", "T:", "E:"]

-- | partial binding of 'reserve' to idents

reserveGI = reserve grammarIdentifiers

identGI = ident grammarIdentifiers



--
-- test stuff
--

testGrammar = unlines
  [ "Grammar: Align"
  , "N: X{2}"
  , "N: Y{3}"
  , "T: a"
  , "E: epsilon"
  , "E: ε"
  , "S: X"
  , "[X{i},Y{j}] -> many <<< [X{i},Y{j}]"
--  , "X -> step  <<< X a"
--  , "X -> stand <<< X"
--  , "[X] -> oned <<< [X]"
--  , "X -> eps   <<< epsilon"
  , "//"
  ]

testParsing :: Result Grammar
testParsing = parseString
                ((evalStateT . runGrammarP) grammar def)
                (Directed (B.pack "testGrammar") 0 0 0 0)
                testGrammar

asG = let (Success g) = testParsing in g
