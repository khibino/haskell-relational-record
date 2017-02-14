{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Query.Monad.Assign
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about restrict context with assignment monad type.
module Database.Relational.Query.Monad.Assign (
  -- * Monad to restrict target records with assignment.
  Assign, AssignStatement,
  -- updateStatement,
  extract,
  ) where

import Database.Relational.Query.Internal.BaseSQL (Assignment)
import Database.Relational.Query.Internal.Config (Config)
import Database.Relational.Query.Internal.Sub (QueryRestriction)

import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Table (Table)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Monad.Restrict (Restrict)
import qualified Database.Relational.Query.Monad.Restrict as Restrict
import Database.Relational.Query.Monad.Trans.Assigning (Assignings, extractAssignments)


-- | Target update monad type used from update statement and merge statement.
type Assign r = Assignings r Restrict

-- | AssignStatement type synonym.
--   Specifying assignments and restrictions like update statement.
--   Projection record type must be
--   the same as 'Target' type parameter 'r'.
type AssignStatement r a = Projection Flat r -> Assign r a

-- -- | 'return' of 'Update'
-- updateStatement :: a -> Assignings r (Restrictings Identity) a
-- updateStatement =  assignings . restrictings . Identity

-- | Run 'Assign'.
extract :: Assign r a -> Config -> ((a, Table r -> [Assignment]), QueryRestriction Flat)
extract =  Restrict.extract . extractAssignments
