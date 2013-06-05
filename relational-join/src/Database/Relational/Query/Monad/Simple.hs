{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Query.Monad.Simple (
  QuerySimple, SimpleQuery,

  simple,

  -- unsafeMergeAnotherOrderBys,

  toSQL,
  toSubQuery
  ) where

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Qualify (Qualify)
import Database.Relational.Query.Monad.Trans.Ordering (Orderings, orderings, OrderedQuery)
import qualified Database.Relational.Query.Monad.Trans.Ordering as Ordering
import Database.Relational.Query.Monad.Core (QueryCore)
import qualified Database.Relational.Query.Monad.Core as Core

import Database.Relational.Query.Sub (SubQuery, subQuery)


type QuerySimple = Orderings Projection QueryCore
type SimpleQuery r = OrderedQuery Projection QueryCore r

simple :: QueryCore a -> QuerySimple a
simple =  orderings

-- unsafeMergeAnotherOrderBys :: NodeAttr -> QuerySimple (Projection r) -> QuerySimple (Projection r)
-- unsafeMergeAnotherOrderBys =  Ordering.unsafeMergeAnotherOrderBys

expandSQL :: SimpleQuery r -> Qualify ((String, Projection r), String -> String)
expandSQL =  Core.expandSQL . Ordering.appendOrderBys

toSQL :: SimpleQuery r -> Qualify String
toSQL q = do
  ((sql, _), append) <- expandSQL q
  return $ append sql  where

toSubQuery :: SimpleQuery r -> Qualify SubQuery
toSubQuery q = do
  ((sql, pj), append) <- expandSQL q
  return $ subQuery (append sql) (Projection.width pj)
