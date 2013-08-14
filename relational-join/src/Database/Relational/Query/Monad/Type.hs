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

  -- * Monad to restrict target records.
  SimpleRestrict
  ) where

import Data.Functor.Identity (Identity)

import Database.Relational.Query.Monad.Qualify (Qualify)
import Database.Relational.Query.Monad.Trans.Join (QueryJoin)
import Database.Relational.Query.Monad.Trans.Restrict (Restrict)


-- | Core query monad type used from simple query and aggregated query.
type QueryCore = Restrict (QueryJoin Qualify)

-- | Restrict only monad type used from update statement and delete statement.
type SimpleRestrict = Restrict Identity
