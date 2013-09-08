-- |
-- Module      : Database.Relational.Query.Monad.Target
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about restrict context with assignment monad type.
module Database.Relational.Query.Monad.Target (
  -- * Monad to restrict target records with assignment.
  Target, TargetStatement,
  -- updateStatement,
  extract,
  expandPrepend
  ) where

import Database.Relational.Query.Sub (QueryRestriction, Assignments)
import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Table (Table)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Monad.Restrict (Restrict, expandWhere)
import qualified Database.Relational.Query.Monad.Restrict as Restrict
import Database.Relational.Query.Monad.Trans.Restricting (WherePrepend)
import Database.Relational.Query.Monad.Trans.Assigning
  (Assignings, SetPrepend, extractAssignments, extractSets)

-- | Target update monad type used from update statement and merge statement.
type Target r = Assignings r Restrict

-- | TargetStatement type synonym.
--   Table and projection record type must be
--   the same as 'Target' type parameter 'r'.
type TargetStatement r a = Table r -> Projection Flat r -> Target r a

-- -- | 'return' of 'Update'
-- updateStatement :: a -> Assignings r (Restrictings Identity) a
-- updateStatement =  assignings . restrictings . Identity

-- | Run 'Target'.
extract :: Target r a -> ((a, Assignments), QueryRestriction Flat)
extract =  Restrict.extract . extractAssignments

-- | Run 'Target' to get SQL WHERE clause.
expandPrepend :: Target r a -> ((a, SetPrepend), WherePrepend)
expandPrepend = expandWhere. extractSets
