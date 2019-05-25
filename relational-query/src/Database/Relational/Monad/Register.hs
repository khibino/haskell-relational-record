-- |
-- Module      : Database.Relational.Monad.Register
-- Copyright   : 2015-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about assignment monad type to build insert statement.
module Database.Relational.Monad.Register (
  -- * Monad to register target records.
  Register,

  extract,
  ) where

import Database.Relational.Internal.Config (Config)
import Database.Relational.SqlSyntax (Assignment)
import Database.Relational.Typed.Table (Table)

import Database.Relational.Monad.BaseType (ConfigureQuery, configureQuery)
import Database.Relational.Monad.Trans.Assigning (Assignings, extractAssignments)


-- | Target register monad type used from insert statement.
type Register r = Assignings r ConfigureQuery

-- | Run 'InsertStatement'.
extract :: Assignings r ConfigureQuery a -> Config -> (a, Table r -> [Assignment])
extract = configureQuery . extractAssignments
