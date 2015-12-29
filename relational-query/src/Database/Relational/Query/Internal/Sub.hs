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

       , Projection, untypeProjection, typedProjection

         -- * Query restriction
       , QueryRestriction, composeWhere, composeHaving
       )  where

import Data.Monoid (Monoid (..), (<>))
import Data.Array (Array)

import qualified Database.Relational.Query.Context as Context
import Database.Relational.Query.Expr (Expr, exprAnd)
import Database.Relational.Query.Expr.Unsafe (unsafeStringSql)
import Database.Relational.Query.Internal.SQL (StringSQL)
import Database.Relational.Query.Internal.Product
  (ProductTree, Node)
import Database.Relational.Query.Component
  (ColumnSQL, Config, Duplication (..),
   AggregateElem, OrderingTerms)
import qualified Database.Relational.Query.Table as Table

import Language.SQL.Keyword (Keyword(..))


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


-- | Phantom typed projection. Projected into Haskell record type 't'.
newtype Projection c t = Projection { untypeProjection :: UntypedProjection }  deriving Show

typedProjection :: UntypedProjection -> Projection c t
typedProjection =  Projection


-- | Type for restriction of query.
type QueryRestriction c = [Expr c Bool]

-- | Compose SQL String from 'QueryRestriction'.
composeRestrict :: Keyword -> QueryRestriction c -> StringSQL
composeRestrict k = d  where
  d    []    =  mempty
  d e@(_:_)  =  k <> unsafeStringSql (foldr1 exprAnd e)

-- | Compose WHERE clause from 'QueryRestriction'.
composeWhere :: QueryRestriction Context.Flat -> StringSQL
composeWhere =  composeRestrict WHERE

-- | Compose HAVING clause from 'QueryRestriction'.
composeHaving :: QueryRestriction Context.Aggregated -> StringSQL
composeHaving =  composeRestrict HAVING
