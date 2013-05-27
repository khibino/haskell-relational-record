
module Database.Relational.Query.Monad.Class (
  MonadQuery (..), MonadAggregate (..)
  ) where

import Database.Relational.Query.Monad.Unsafe (UnsafeMonadQuery)
import Database.Relational.Query.Expr (Expr)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Aggregation (Aggregation)

class UnsafeMonadQuery m => MonadQuery m where
  on     :: Expr (Maybe Bool) -> m ()
  wheres :: Expr (Maybe Bool) -> m ()

class MonadQuery m => MonadAggregate m where
  groupBy :: Projection r -> m (Aggregation r)
  having  :: Aggregation (Maybe Bool) -> m ()
