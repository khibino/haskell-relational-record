{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- |
-- Module      : Database.Relational.SqlSyntax.Types
-- Copyright   : 2015-2018 Kei Hibino
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
  Record,
  placeholderOffsets,
  untypeRecord,
  record,
  PI,
  recordWidth,
  typeFromRawColumns,
  typeFromScalarSubQuery,

  -- * Manipulate records representing placeholders
  toPlaceholdersRecord,
  isPlaceholders,
  emptyPlaceholderOffsets,
  appendPlaceholderOffsetsOf,

  -- * Predicate to restrict Query result
  Predicate,
  )  where

import Prelude hiding (and, product)
import Data.DList (DList, fromList)
import Data.Foldable (Foldable)
import Data.Monoid ((<>))
import Data.Traversable (Traversable)

import Database.Relational.Internal.Config (Config)
import Database.Relational.Internal.ContextType (Flat, Aggregated)
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
type OrderColumn = Column

-- | Type for order-by term
type OrderingTerm = ((Order, Maybe Nulls), OrderColumn)

-- | Type for group-by term
type AggregateColumnRef = Column

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
newtype AggregateKey a = AggregateKey (a, AggregateElem) deriving Functor

-- | Sub-query type
data SubQuery = Table Untyped
              | Flat Config
                Tuple Duplication JoinProduct [Predicate Flat]
                [OrderingTerm]
              | Aggregated Config
                Tuple Duplication JoinProduct [Predicate Flat]
                [AggregateElem] [Predicate Aggregated] [OrderingTerm]
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
type JoinProduct = Maybe (ProductTree [Predicate Flat])

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
data Record c t =
  Record
  { placeholderOffsets :: !(DList Int)
  -- ^ If the record contains some placeholders,
  -- offsets of the values of the parameters are saved in this field.
  , untypeRecord :: !Tuple {- ^ Discard record type -}
  } deriving Show

-- | Type for predicate to restrict of query result.
type Predicate c = Record c (Maybe Bool)

-- | Type for projection function.
type PI c a b = Record c a -> Record c b

-- | Unsafely type 'Tuple' value to 'Record' type.
record :: DList Int -> Tuple -> Record c t
record = Record

-- | Width of 'Record'.
recordWidth :: Record c r -> Int
recordWidth = length . untypeRecord

-- | Unsafely generate 'Record' from SQL string list.
typeFromRawColumns :: DList Int
                   -> [StringSQL] -- ^ SQL string list specifies columns
                   -> Record c r  -- ^ Result 'Record'
typeFromRawColumns phs =  record phs . map RawColumn

toPlaceholdersRecord :: Record c r -> Record c r
toPlaceholdersRecord r = r { placeholderOffsets = fromList [0 .. tupleWidth (untypeRecord r) - 1] }

isPlaceholders :: Record c r -> Bool
isPlaceholders = (/= mempty) . placeholderOffsets

emptyPlaceholderOffsets :: Record c r -> Record c r
emptyPlaceholderOffsets r = r { placeholderOffsets = mempty }

appendPlaceholderOffsetsOf :: Record c1 a -> Record c2 b -> DList Int
appendPlaceholderOffsetsOf src1 src2 = placeholderOffsets src1 <> placeholderOffsets src2

-- | Unsafely generate 'Record' from scalar sub-query.
typeFromScalarSubQuery :: DList Int -> SubQuery -> Record c t
typeFromScalarSubQuery phs = record phs . (:[]) . Scalar
