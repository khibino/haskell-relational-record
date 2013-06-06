{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Relational.Query.Monad.Class (
  MonadQualify (..),
  MonadQuery (..), MonadAggregate (..)
  ) where

import Database.Relational.Query.Expr (Expr)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Aggregation (Aggregation)
import Database.Relational.Query.Sub (SubQuery, Qualified)

import Database.Relational.Query.Internal.Product (NodeAttr)

class (Functor m, Monad m) => MonadQuery m where
  on     :: Expr (Maybe Bool) -> m ()
  wheres :: Expr (Maybe Bool) -> m ()
  unsafeSubQuery :: NodeAttr -> Qualified SubQuery -> m (Projection r)
  -- unsafeMergeAnotherQuery :: NodeAttr -> m (Projection r) -> m (Projection r)

class (Functor q, Monad q, MonadQuery m) => MonadQualify q m where
  liftQualify :: q a -> m a

class MonadQuery m => MonadAggregate m where
  groupBy :: Projection r -> m (Aggregation r)
  having  :: Aggregation (Maybe Bool) -> m ()
