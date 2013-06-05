
module Database.Relational.Query.Monad.Class (
  MonadQuery (..), MonadAggregate (..)
  ) where

import Database.Relational.Query.Expr (Expr)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Aggregation (Aggregation)
import Database.Relational.Query.Sub (SubQuery)

import Database.Relational.Query.Internal.Product (NodeAttr)
import Database.Relational.Query.Monad.Qualify (Qualify)

class (Functor m, Monad m) => MonadQuery m where
  on     :: Expr (Maybe Bool) -> m ()
  wheres :: Expr (Maybe Bool) -> m ()
  unsafeSubQuery :: NodeAttr -> Qualify SubQuery -> m (Projection r)
  -- unsafeMergeAnotherQuery :: NodeAttr -> m (Projection r) -> m (Projection r)

class MonadQuery m => MonadAggregate m where
  groupBy :: Projection r -> m (Aggregation r)
  having  :: Aggregation (Maybe Bool) -> m ()
