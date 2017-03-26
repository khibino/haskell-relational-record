{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- |
-- Module      : Database.Relational.Query.Internal.Sub
-- Copyright   : 2015-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines sub-query structure used in query products.
module Database.Relational.Query.Internal.Sub
       ( SubQuery (..)
       , SetOp (..), BinOp (..), Qualifier (..)
       , Qualified (..), qualifier, unQualify, qualify

         -- * Product tree type
       , NodeAttr (..), ProductTree (..)
       , Node (..), nodeAttr, nodeTree
       , JoinProduct, QueryProductTree
       , ProductTreeBuilder, ProductBuilder

       , UntypedProjection, untypedProjectionWidth, ProjectionUnit (..)
       , Projection, untypeProjection, typedProjection, projectionWidth
       , projectFromColumns, projectFromScalarSubQuery

         -- * Query restriction
       , QueryRestriction
       )  where

import Prelude hiding (and, product)
import Data.DList (DList)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Database.Relational.Query.Internal.Config (Config)
import Database.Relational.Query.Internal.ContextType (Flat, Aggregated)
import Database.Relational.Query.Internal.SQL (StringSQL)
import Database.Relational.Query.Internal.BaseSQL (Duplication (..), OrderingTerm)
import Database.Relational.Query.Internal.GroupingSQL (AggregateElem)
import Database.Relational.Query.Internal.UntypedTable (Untyped)


-- | Set operators
data SetOp = Union | Except | Intersect  deriving Show

-- | Set binary operators
newtype BinOp = BinOp (SetOp, Duplication) deriving Show

-- | Sub-query type
data SubQuery = Table Untyped
              | Flat Config
                UntypedProjection Duplication JoinProduct (QueryRestriction Flat)
                [OrderingTerm]
              | Aggregated Config
                UntypedProjection Duplication JoinProduct (QueryRestriction Flat)
                [AggregateElem] (QueryRestriction Aggregated) [OrderingTerm]
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

type QueryRestrictionBuilder = DList (Projection Flat (Maybe Bool))

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

-- | Product tree with join restriction.
type QueryProductTree = ProductTree (QueryRestriction Flat)

-- | Product tree with join restriction builder.
type ProductTreeBuilder = ProductTree QueryRestrictionBuilder

-- | Product noe with join restriction builder.
type ProductBuilder = Node QueryRestrictionBuilder

-- | Type for join product of query.
type JoinProduct = Maybe QueryProductTree


-- | Projection structure unit with single column width
data ProjectionUnit
  = RawColumn StringSQL            -- ^ used in immediate value or unsafe operations
  | SubQueryRef (Qualified Int)    -- ^ normalized sub-query reference T<n> with Int index
  | Scalar SubQuery                -- ^ scalar sub-query
  deriving Show

-- | Untyped projection. Forgot record type.
type UntypedProjection = [ProjectionUnit]

-- | Width of 'UntypedProjection'.
untypedProjectionWidth :: UntypedProjection -> Int
untypedProjectionWidth = length

-- | Phantom typed projection. Projected into Haskell record type 't'.
newtype Projection c t =
  Projection
  { untypeProjection :: UntypedProjection {- ^ Discard projection value type -} }  deriving Show

-- | Unsafely type projection value.
typedProjection :: UntypedProjection -> Projection c t
typedProjection =  Projection

-- | Width of 'Projection'.
projectionWidth :: Projection c r -> Int
projectionWidth = length . untypeProjection

-- | Unsafely generate 'Projection' from SQL string list.
projectFromColumns :: [StringSQL]    -- ^ SQL string list specifies columns
                   -> Projection c r -- ^ Result 'Projection'
projectFromColumns =  typedProjection . map RawColumn

-- | Unsafely generate 'Projection' from scalar sub-query.
projectFromScalarSubQuery :: SubQuery -> Projection c t
projectFromScalarSubQuery = typedProjection . (:[]) . Scalar


-- | Type for restriction of query.
type QueryRestriction c = [Projection c (Maybe Bool)]
