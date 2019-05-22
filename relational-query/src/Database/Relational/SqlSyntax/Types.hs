{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

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
  TypedTuple (TypedTuple), untypeTuple,
  Record (Record), toTypedTuple, forciblyTypeTuple, mapTypedTuple, PI,

  -- * Predicate to restrict Query result
  Predicate,

  -- * Manipulate placeholders referred in the statement.
  PlaceholderOffsets,
  WithPlaceholderOffsetsT (WithPlaceholderOffsetsT),
  WithPlaceholderOffsets,
  SQLWithPlaceholderOffsets',
  SQLWithPlaceholderOffsets,

  appendPlaceholderOffsets,
  withPlaceholderOffsets,
  runWithPlaceholderOffsetsT,

  )  where

import Prelude hiding (and, product)
import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Writer (WriterT, runWriterT, writer, tell)
import Data.DList (DList)
import Data.Foldable (Foldable)
import Data.Functor.Identity (Identity)
import Data.Monoid (Monoid, mappend, mempty)
import Data.Semigroup (Semigroup, (<>))
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
                (WithPlaceholderOffsets Tuple) Duplication (WithPlaceholderOffsets JoinProduct) [WithPlaceholderOffsets Tuple]
                (WithPlaceholderOffsets [OrderingTerm])
              | Aggregated Config
                (WithPlaceholderOffsets Tuple) Duplication (WithPlaceholderOffsets JoinProduct) [WithPlaceholderOffsets Tuple]
                (WithPlaceholderOffsets [AggregateElem]) [WithPlaceholderOffsets Tuple] (WithPlaceholderOffsets [OrderingTerm])
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

-- | Product tree type. Product tree is constructed by left node and right node.
data ProductTree rs
  = Leaf (Bool, Qualified SubQuery)
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
type JoinProduct = Maybe (ProductTree [Tuple])

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
newtype TypedTuple c t =
  TypedTuple
  { untypeTuple :: Tuple {- ^ Discard record type -} }  deriving Show

newtype Record c t =
  Record
  { toTypedTuple :: WithPlaceholderOffsets (TypedTuple c t) } deriving Show

-- | Type for predicate to restrict of query result.
type Predicate c = Record c (Maybe Bool)

-- | Type for projection function.
type PI c a b = Record c a -> Record c b

mapTypedTuple :: (TypedTuple c t -> TypedTuple c' t') -> Record c t -> Record c' t'
mapTypedTuple f = Record . fmap f . toTypedTuple

type PlaceholderOffsets = DList Int

newtype WithPlaceholderOffsetsT m a =
  WithPlaceholderOffsetsT (WriterT PlaceholderOffsets m a)
  deriving (Show, Functor, Applicative, Monad, Foldable, Traversable, MonadTrans)

appendPlaceholderOffsets :: Monad m => PlaceholderOffsets -> WithPlaceholderOffsetsT m ()
appendPlaceholderOffsets = WithPlaceholderOffsetsT . tell

type WithPlaceholderOffsets = WithPlaceholderOffsetsT Identity

type SQLWithPlaceholderOffsets' = WithPlaceholderOffsets StringSQL

type SQLWithPlaceholderOffsets = WithPlaceholderOffsets String

-- I wish I could use DerivingVia...
instance Semigroup a => Semigroup (WithPlaceholderOffsets a) where
  ma <> mb = (<>) <$> ma <*> mb

instance Monoid a => Monoid (WithPlaceholderOffsets a) where
  mempty = withPlaceholderOffsets mempty mempty
  mappend ma mb = mappend <$> ma <*> mb

withPlaceholderOffsets :: PlaceholderOffsets -> a -> WithPlaceholderOffsets a
withPlaceholderOffsets phs x = WithPlaceholderOffsetsT $ writer (x, phs)

runWithPlaceholderOffsetsT :: WithPlaceholderOffsetsT m a -> m (a, PlaceholderOffsets)
runWithPlaceholderOffsetsT (WithPlaceholderOffsetsT act) = runWriterT act

-- | Unsafely type 'Tuple' value to 'TypedTuple' type.
forciblyTypeTuple :: Tuple -> TypedTuple c t
forciblyTypeTuple = TypedTuple
