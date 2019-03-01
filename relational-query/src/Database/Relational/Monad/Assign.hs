{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Assign
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about restrict context with assignment monad type.
module Database.Relational.Monad.Assign (
  -- * Monad to restrict target records with assignment.
  Assign, AssignStatement,
  -- updateStatement,
  extract,
  ) where

import Data.DList (DList)

import Database.Relational.Internal.Config (Config)
import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.SqlSyntax
  (Predicate, Record, Assignment)

import Database.Relational.Table (Table)
import Database.Relational.Monad.Restrict (Restrict)
import qualified Database.Relational.Monad.Restrict as Restrict
import Database.Relational.Monad.Trans.Assigning (Assignings, extractAssignments)
import Database.Relational.Monad.Trans.ReferredPlaceholders (ReferredPlaceholders, extractReferredPlaceholders)


-- | Target update monad type used from update statement and merge statement.
type Assign r = ReferredPlaceholders (Assignings r Restrict)

-- | AssignStatement type synonym.
--   Specifying assignments and restrictions like update statement.
--   Record type must be
--   the same as 'Target' type parameter 'r'.
type AssignStatement r a = Record Flat r -> Assign r a

-- -- | 'return' of 'Update'
-- updateStatement :: a -> Assignings r (Restrictings Identity) a
-- updateStatement =  assignings . restrictings . Identity

-- | Run 'Assign'.
extract :: Assign r a -> Config -> (((a, DList Int), Table r -> [Assignment]), [Predicate Flat])
extract =  Restrict.extract . extractAssignments . extractReferredPlaceholders
