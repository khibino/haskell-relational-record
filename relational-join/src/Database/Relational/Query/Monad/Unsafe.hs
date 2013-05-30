module Database.Relational.Query.Monad.Unsafe (
  UnsafeMonadQuery (..)
  ) where

import Database.Relational.Query.Internal.Product (NodeAttr)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Sub (SubQuery)
import Database.Relational.Query.Monad.Qualify (Qualify)

class (Functor m, Monad m) => UnsafeMonadQuery m where
  unsafeSubQuery          :: NodeAttr -> Qualify SubQuery -> m (Projection r)
  -- unsafeMergeAnotherQuery :: NodeAttr -> m (Projection r) -> m (Projection r)
