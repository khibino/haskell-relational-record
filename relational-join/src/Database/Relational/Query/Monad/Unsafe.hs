
module Database.Relational.Query.Monad.Unsafe (
  UnsafeMonadQuery (..)
  ) where

import Database.Relational.Query.Internal.Product (NodeAttr)

class Monad m => UnsafeMonadQuery m where
  unsafeMergeAnotherQuery :: NodeAttr -> m a -> m a
