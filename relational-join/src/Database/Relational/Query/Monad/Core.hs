module Database.Relational.Query.Monad.Core (
  QueryCore,
  ) where

import Database.Relational.Query.Monad.Qualify (Qualify)
import Database.Relational.Query.Monad.Trans.Join (QueryJoin)


type QueryCore = QueryJoin Qualify
