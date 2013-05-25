
module Database.Relational.Query.Monad.Unsafe (
  UnsafeMonadQuery (..)
  ) where

import Database.Relational.Query.Internal.Product (NodeAttr)
import Database.Relational.Query.Projection (Projection)

class Monad m => UnsafeMonadQuery m where
  unsafeMergeAnotherQuery :: NodeAttr -> m (Projection r) -> m (Projection r)
