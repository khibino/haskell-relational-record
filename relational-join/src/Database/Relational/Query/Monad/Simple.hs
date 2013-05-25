{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Query.Monad.Simple (
  QuerySimple, SimpleQuery,

  simple,

  unsafeMergeAnotherOrderBys,

  toSQL,
  toSubQuery
  ) where

import Database.Relational.Query.Internal.Product (NodeAttr)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Ordering (Orderings, orderings, OrderedQuery)
import qualified Database.Relational.Query.Monad.Ordering as Ordering
import Database.Relational.Query.Monad.Core (QueryCore)
import qualified Database.Relational.Query.Monad.Core as Core

import Database.Relational.Query.Sub (SubQuery, subQuery)


type QuerySimple = Orderings Projection QueryCore
type SimpleQuery r = OrderedQuery Projection QueryCore r

simple :: QueryCore a -> QuerySimple a
simple =  orderings

unsafeMergeAnotherOrderBys :: NodeAttr -> QuerySimple (Projection r) -> QuerySimple (Projection r)
unsafeMergeAnotherOrderBys =  Ordering.unsafeMergeAnotherOrderBys

toSQL :: SimpleQuery r -> String
toSQL q = append sql  where
  ((sql, _), append) = Core.expandSQL . Ordering.appendOrderBys $ q

toSubQuery :: SimpleQuery r -> SubQuery
toSubQuery q = subQuery (append sql) (Projection.width pj)  where
  ((sql, pj), append) = Core.expandSQL . Ordering.appendOrderBys $ q
