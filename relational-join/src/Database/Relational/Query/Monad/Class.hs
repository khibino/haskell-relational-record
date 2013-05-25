
module Database.Relational.Query.Monad.Class (
  MonadQuery (..)
  ) where

import Database.Relational.Query.Monad.Unsafe (UnsafeMonadQuery)
import Database.Relational.Query.Expr (Expr)

class UnsafeMonadQuery m => MonadQuery m where
  on     :: Expr Bool -> m ()
  wheres :: Expr Bool -> m ()
