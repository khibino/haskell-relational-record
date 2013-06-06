{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Relational.Query.Monad.Simple (
  QuerySimple, SimpleQuery,

  simple,

  toSQL,
  toSubQuery
  ) where

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Qualify (Qualify)
import Database.Relational.Query.Monad.Class (MonadQualify(..))
import Database.Relational.Query.Monad.Trans.Join (join')
import qualified Database.Relational.Query.Monad.Trans.Join as Join
import Database.Relational.Query.Monad.Trans.Ordering (Orderings, orderings, OrderedQuery)
import qualified Database.Relational.Query.Monad.Trans.Ordering as Ordering
import Database.Relational.Query.Monad.Core (QueryCore)

import Database.Relational.Query.Sub (SubQuery, subQuery)


type QuerySimple = Orderings Projection QueryCore
type SimpleQuery r = OrderedQuery Projection QueryCore r

simple :: Qualify a -> QuerySimple a
simple =  orderings . join'

instance MonadQualify Qualify (Orderings Projection QueryCore) where
  liftQualify = simple

expandSQL :: SimpleQuery r -> Qualify ((String, Projection r), String -> String)
expandSQL =  Join.expandSQL . Ordering.appendOrderBys

toSQL :: SimpleQuery r -> Qualify String
toSQL q = do
  ((sql, _), append) <- expandSQL q
  return $ append sql  where

toSubQuery :: SimpleQuery r -> Qualify SubQuery
toSubQuery q = do
  ((sql, pj), append) <- expandSQL q
  return $ subQuery (append sql) (Projection.width pj)
