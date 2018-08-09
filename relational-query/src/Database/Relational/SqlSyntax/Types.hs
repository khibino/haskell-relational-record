{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- |
-- Module      : Database.Relational.SqlSyntax.Types
-- Copyright   : 2015-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines sub-query structure used in query products.
module Database.Relational.SqlSyntax.Types (
  -- * The SubQuery
  SubQuery (..),

  -- * Set operations
  Duplication (..), SetOp (..), BinOp (..),

  -- * Qualifiers for nested query
  Qualifier (..), Qualified (..), qualifier, unQualify, qualify,

  -- * Ordering types
  Order (..), Nulls (..), OrderColumn, OrderingTerm,

  -- * Aggregating types
  AggregateColumnRef,
  AggregateBitKey (..), AggregateSet (..), AggregateElem (..),

  AggregateKey (..),

  -- * Product tree type
  NodeAttr (..), ProductTree (..),
  Node (..), nodeAttr, nodeTree,
  JoinProduct,

  -- * Case
  CaseClause (..), WhenClauses(..),

  -- * Column, Tuple, Record and Projection
  Column (..), Tuple, tupleWidth,
  Record, untypeRecord, record, PI,
  recordWidth,
  typeFromRawColumns,
  typeFromScalarSubQuery,

  -- * Predicate to restrict Query result
  Predicate,

  nextIndexOf,
  previousIndexOf,
  setNextIndexOf,
  setPreviousIndexOf,
  )  where

import Prelude hiding (and, product)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Database.Relational.Internal.Config (Config)
import Database.Relational.Internal.String (StringSQL)
import Database.Relational.Internal.UntypedTable (Untyped)


-- | Result record duplication attribute
data Duplication = All | Distinct  deriving Show

-- | Set operators
data SetOp = Union | Except | Intersect  deriving Show

-- | Set binary operators
newtype BinOp = BinOp (SetOp, Duplication) deriving Show

-- | Order direction. Ascendant or Descendant.
data Order = Asc | Desc  deriving Show

-- | Order of null.
data Nulls =  NullsFirst | NullsLast deriving Show

-- | Type for order-by column
type OrderColumn = StringSQL

-- | Type for order-by term
type OrderingTerm = ((Order, Maybe Nulls), OrderColumn)

-- | Type for group-by term
type AggregateColumnRef = StringSQL

-- | Type for group key.
newtype AggregateBitKey = AggregateBitKey [AggregateColumnRef] deriving Show

-- | Type for grouping set
newtype AggregateSet = AggregateSet [AggregateElem] deriving Show

-- | Type for group-by tree
data AggregateElem = ColumnRef AggregateColumnRef
                   | Rollup [AggregateBitKey]
                   | Cube   [AggregateBitKey]
                   | GroupingSets [AggregateSet]
                   deriving Show

-- | Typeful aggregate element.
newtype AggregateKey a = AggregateKey (a, AggregateElem)

-- | Sub-query type
data SubQuery = Table Untyped
              | Flat Config
                Tuple Duplication JoinProduct [Tuple{-Predicate i j Flat-}]
                [OrderingTerm]
              | Aggregated Config
                Tuple Duplication JoinProduct [Tuple{-Predicate i j Flat-}]
                [AggregateElem] [Tuple{-Predicate i j Aggregated-}] [OrderingTerm]
              | Bin BinOp SubQuery SubQuery
              deriving Show

-- | Qualifier type.
newtype Qualifier = Qualifier Int  deriving Show

-- | Qualified query.
data Qualified a =
  Qualified Qualifier a
  deriving (Show, Functor, Foldable, Traversable)

-- | Get qualifier
qualifier :: Qualified a -> Qualifier
qualifier (Qualified q _) = q

-- | Unqualify.
unQualify :: Qualified a -> a
unQualify (Qualified _ a) = a

-- | Add qualifier
qualify :: Qualifier -> a -> Qualified a
qualify = Qualified


-- | node attribute for product.
data NodeAttr = Just' | Maybe deriving Show

type QS = Qualified SubQuery

-- | Product tree type. Product tree is constructed by left node and right node.
data ProductTree rs
  = Leaf QS
  | Join !(Node rs) !(Node rs) !rs
  deriving (Show, Functor)

-- | Product node. node attribute and product tree.
data Node rs = Node !NodeAttr !(ProductTree rs)  deriving (Show, Functor)

-- | Get node attribute.
nodeAttr :: Node rs -> NodeAttr
nodeAttr (Node a _) = a  where

-- | Get tree from node.
nodeTree :: Node rs -> ProductTree rs
nodeTree (Node _ t) = t

-- | Type for join product of query.
type JoinProduct = Maybe (ProductTree [Tuple{-Predicate i j Flat-}])

-- | when clauses
data WhenClauses =
  WhenClauses [(Tuple, Tuple)] Tuple
  deriving Show

-- | case clause
data CaseClause
  = CaseSearch WhenClauses
  | CaseSimple Tuple WhenClauses
  deriving Show

-- | Projected column structure unit with single column width
data Column
  = RawColumn StringSQL            -- ^ used in immediate value or unsafe operations
  | SubQueryRef (Qualified Int)    -- ^ normalized sub-query reference T<n> with Int index
  | Scalar SubQuery                -- ^ scalar sub-query
  | Case CaseClause Int            -- ^ <n>th column of case clause
  deriving Show

-- | Untyped projected tuple. Forgot record type.
type Tuple = [Column]

-- | Width of 'Tuple'.
tupleWidth :: Tuple -> Int
tupleWidth = length

-- | Phantom typed record. Projected into Haskell record type 't'.
-- TODO: Delete @i@ type parameter
-- TODO: pass xs and ys to directly. i.e. Record (ExRecord xs) c t
newtype Record i j c t =
  Record
  { untypeRecord :: Tuple {- ^ Discard record type -} }  deriving Show

-- | Type for predicate to restrict of query result.
type Predicate i j c = Record i j c (Maybe Bool)

-- | Type for projection function.
type PI i c a b = Record i i c a -> Record i i c b

-- | Unsafely type 'Tuple' value to 'Record' type.
record :: Tuple -> Record i j c t
record = Record

-- igrep TODO: Really impossible?
nextIndexOf :: recordOrRelation i j c r -> j
nextIndexOf = error "nextIndexOf: Impossible"

-- igrep TODO: Really impossible?
previousIndexOf :: recordOrRelation i j c r -> i
previousIndexOf = error "nextIndexOf: Impossible"

-- igrep TODO: Really impossible?
setNextIndexOf :: k -> recordOrRelation i j c r -> recordOrRelation i k c r
setNextIndexOf = error "nextIndexOf: Impossible"

-- igrep TODO: Really impossible?
setPreviousIndexOf :: j -> recordOrRelation i k c r -> recordOrRelation j k c r
setPreviousIndexOf = error "nextIndexOf: Impossible"


-- | Width of 'Record'.
recordWidth :: Record i j c r -> Int
recordWidth = length . untypeRecord

-- | Unsafely generate 'Record' from SQL string list.
typeFromRawColumns :: [StringSQL] -- ^ SQL string list specifies columns
                   -> Record i j c r  -- ^ Result 'Record'
typeFromRawColumns =  record . map RawColumn

-- | Unsafely generate 'Record' from scalar sub-query.
typeFromScalarSubQuery :: SubQuery -> Record i j c t
typeFromScalarSubQuery = record . (:[]) . Scalar
