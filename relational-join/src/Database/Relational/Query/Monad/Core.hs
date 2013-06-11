-- |
-- Module      : Database.Relational.Query.Monad.Core
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines core query type.
module Database.Relational.Query.Monad.Core (
  -- * Core query monad
  QueryCore
  ) where

import Database.Relational.Query.Monad.Qualify (Qualify)
import Database.Relational.Query.Monad.Trans.Join (QueryJoin)


-- | Core query monad type used by simple query and aggregated query.
type QueryCore = QueryJoin Qualify
