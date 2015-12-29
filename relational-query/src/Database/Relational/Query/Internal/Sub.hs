-- |
-- Module      : Database.Relational.Query.Internal.Sub
-- Copyright   : 2015 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines sub-query structure used in query products.
module Database.Relational.Query.Internal.Sub
       ( SubQuery (..), UntypedProjection, ProjectionUnit (..)
       , JoinProduct, QueryProduct, QueryProductNode
       , SetOp (..), BinOp (..), Qualifier (..), Qualified (..)
       )  where

import Data.Array (Array)

import qualified Database.Relational.Query.Context as Context
import Database.Relational.Query.Internal.Product
  (ProductTree, Node)
import Database.Relational.Query.Component
  (ColumnSQL, Config,
   Duplication (..), QueryRestriction,
   AggregateElem, OrderingTerms)
import qualified Database.Relational.Query.Table as Table


data SetOp = Union | Except | Intersect  deriving Show

newtype BinOp = BinOp (SetOp, Duplication) deriving Show

-- | Sub-query type
data SubQuery = Table Table.Untyped
              | Flat Config
                UntypedProjection Duplication JoinProduct (QueryRestriction Context.Flat)
                OrderingTerms
              | Aggregated Config
                UntypedProjection Duplication JoinProduct (QueryRestriction Context.Flat)
                [AggregateElem] (QueryRestriction Context.Aggregated) OrderingTerms
              | Bin BinOp SubQuery SubQuery
              deriving Show

-- | Qualifier type.
newtype Qualifier = Qualifier Int  deriving Show

-- | Qualified query.
data Qualified a = Qualified a Qualifier  deriving Show

-- | 'Functor' instance of 'Qualified'
instance Functor Qualified where
  fmap f (Qualified a i) = Qualified (f a) i

-- | Projection structure unit
data ProjectionUnit = Columns (Array Int ColumnSQL)
                    | Normalized (Qualified Int)
                    | Scalar SubQuery
                    deriving Show

-- | Untyped projection. Forgot record type.
type UntypedProjection = [ProjectionUnit]

-- | Product tree specialized by 'SubQuery'.
type QueryProduct = ProductTree (Qualified SubQuery)
-- | Product node specialized by 'SubQuery'.
type QueryProductNode = Node (Qualified SubQuery)

-- | Type for join product of query.
type JoinProduct = Maybe QueryProduct
