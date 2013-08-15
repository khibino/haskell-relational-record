-- |
-- Module      : Database.Relational.Query.Monad.Type
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines core query type.
module Database.Relational.Query.Monad.Type (
  -- * Core query monad
  QueryCore,
  ) where

import Database.Relational.Query.Monad.Qualify (Qualify)
import Database.Relational.Query.Monad.Trans.Join (QueryJoin)
import Database.Relational.Query.Monad.Trans.Restricting (Restrictings)


-- | Core query monad type used from simple query and aggregated query.
type QueryCore = Restrictings (QueryJoin Qualify)
