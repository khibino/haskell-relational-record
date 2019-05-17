{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Database.Relational.Monad.Assign
-- Copyright   : 2013-2019 Kei Hibino
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
  extract,

  -- * API of context with assignments
  assignTo, (<-#),
  ) where

import Database.Relational.Internal.Config (Config)
import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.Projectable.Unsafe (ResultContext)
import Database.Relational.SqlSyntax
  (Record, Assignment, WithPlaceholderOffsets, Tuple)

import Database.Relational.Table (Table)
import Database.Relational.Monad.Restrict (Restrict)
import qualified Database.Relational.Monad.Restrict as Restrict
import Database.Relational.Monad.Trans.ReadPlaceholders (ReadPlaceholders, readPlaceholders)
import Database.Relational.Monad.Trans.Assigning (Assignings, AssignTarget, extractAssignments)
import qualified Database.Relational.Monad.Trans.Assigning as Trans


-- | Target update monad type used from update statement and merge statement.
type Assign r = Assignings r Restrict

-- | AssignStatement type synonym.
--   Specifying assignments and restrictions like update statement.
--   Record type must be
--   the same as 'Target' type parameter 'r'.
type AssignStatement p r a = Record Flat r -> ReadPlaceholders p (Assign r) a

-- | Run 'Assign'.
extract :: Assign r a -> Config -> ((a, Table r -> WithPlaceholderOffsets [Assignment]), [WithPlaceholderOffsets Tuple])
extract =  Restrict.extract . extractAssignments


assignTo :: (Monad m, ResultContext c Flat ~ Flat) => Record c v ->  AssignTarget r v -> ReadPlaceholders p (Assignings r m) ()
assignTo r = readPlaceholders . Trans.assignTo r

(<-#) :: (Monad m, ResultContext c Flat ~ Flat) => AssignTarget r v -> Record c v -> ReadPlaceholders p (Assignings r m) ()
(<-#) =  flip assignTo
