
module Database.Relational.Query.Monad.Class (
  MonadQuery (..)
  ) where

import Database.Relational.Query.Expr (Expr)

class Monad m => MonadQuery m where
  on     :: Expr Bool -> m ()
  wheres :: Expr Bool -> m ()
