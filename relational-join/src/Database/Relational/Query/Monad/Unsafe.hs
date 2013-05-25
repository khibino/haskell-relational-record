
module Database.Relational.Query.Monad.Unsafe (
  UnsafeMonadQuery (..)
  ) where

import Database.Relational.Query.Internal.Product (NodeAttr)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Sub (SubQuery)

class (Functor m, Monad m) => UnsafeMonadQuery m where
  unsafeSubQuery          :: NodeAttr -> SubQuery -> m (Projection t)
  unsafeMergeAnotherQuery :: NodeAttr -> m (Projection r) -> m (Projection r)
