-- |
-- Module      : Database.Relational.Query.Monad.Register
-- Copyright   : 2015 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about assignment monad type to build insert statement.
module Database.Relational.Query.Monad.Register (
  -- * Monad to register target records.
  Register,

  extract,
  ) where

import Database.Relational.Query.Internal.BaseSQL (Assignment)
import Database.Relational.Query.Internal.Config (Config)
import Database.Relational.Query.Table (Table)
import Database.Relational.Query.Monad.BaseType (ConfigureQuery, configureQuery)
import Database.Relational.Query.Monad.Trans.Assigning (Assignings, extractAssignments)


-- | Target register monad type used from insert statement.
type Register r = Assignings r ConfigureQuery

-- | Run 'InsertStatement'.
extract :: Assignings r ConfigureQuery a -> Config -> (a, Table r -> [Assignment])
extract = configureQuery . extractAssignments
