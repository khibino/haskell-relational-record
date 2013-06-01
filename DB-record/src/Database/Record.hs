-- |
-- Module      : Database.Record
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This is integrated module which contains
-- types to represent table constraints and
-- interfaces to bind between SQL database values and Haskell records.
module Database.Record (
  -- * Binding between SQL database values and Haskell records
  -- $bindSqlAndHaskellRecords
  -- * Constraints used by derivation rules
  -- $constraintsForDerivation
  -- * Modules which provide proof objects
  -- ** Table constraint
  module Database.Record.KeyConstraint,
  -- ** Convert between Haskell type and list of SQL type
  module Database.Record.Persistable,
  -- ** Convert from list of SQL type
  module Database.Record.FromSql,
  -- ** Convert into list of SQL type
  module Database.Record.ToSql
  ) where

{- $bindSqlAndHaskellRecords
-}

{- $constraintsForDerivation
-}

import Database.Record.KeyConstraint
  (KeyConstraint, HasKeyConstraint(..),
   Primary, PrimaryConstraint,
   Unique, UniqueConstraint, unique, derivedUniqueConstraint,
   NotNull, NotNullConstraint, notNull, derivedNotNullConstraint)
import Database.Record.Persistable
  (PersistableSqlType, PersistableType(..), sqlNullValue,
   PersistableSqlValue, PersistableValue(..), fromSql, toSql,
   PersistableRecordWidth, PersistableWidth(..),
   PersistableRecord, Persistable(..),
   derivedPersistableValueRecord)
import Database.Record.FromSql
  (RecordFromSql, FromSql(..), recordFromSql',
   runTakeRecord, takeRecord, runToRecord, toRecord)
import Database.Record.ToSql
  (RecordToSql, ToSql(..), recordToSql',
   runFromRecord, fromRecord,
   updateValuesByUnique, updateValuesByPrimary)
