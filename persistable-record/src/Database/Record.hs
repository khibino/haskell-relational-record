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
  -- * Concepts
  -- $concepts

  -- * Binding between SQL values and Haskell records
  -- $bindSqlAndHaskellRecords

  -- * Constraints used for 'RecordFromSql' inference
  -- $constraintsForInference

  -- * Modules which provide proof objects
  -- ** Table constraint specified by keys
  module Database.Record.KeyConstraint,
  -- ** Convert between Haskell type and list of SQL type
  module Database.Record.Persistable,
  -- ** Convert from list of SQL type
  module Database.Record.FromSql,
  -- ** Convert into list of SQL type
  module Database.Record.ToSql
  ) where

import Database.Record.KeyConstraint
  (ColumnConstraint, HasColumnConstraint(..),
   Primary, PrimaryColumnConstraint,
   Unique, UniqueColumnConstraint, uniqueColumn, derivedUniqueColumnConstraint,
   NotNull, NotNullColumnConstraint, notNullColumn, derivedNotNullColumnConstraint,
   KeyConstraint, HasKeyConstraint(..), PrimaryConstraint, UniqueConstraint,
   deriveComposite, unique, derivedCompositePrimary, derivedUniqueConstraint)
import Database.Record.Persistable
  (PersistableSqlType, PersistableType(..), sqlNullValue,
   PersistableSqlValue, PersistableValue(..), fromSql, toSql,
   PersistableRecordWidth, PersistableWidth(..), derivedWidth,
   PersistableRecord, Persistable(..),
   derivedPersistableValueRecord)
import Database.Record.FromSql
  (RecordFromSql, FromSql(..), recordFromSql',
   runTakeRecord, takeRecord, runToRecord, toRecord)
import Database.Record.ToSql
  (ToSqlM, RecordToSql, ToSql(..), recordToSql',
   runFromRecord, wrapToSql, putRecord, putEmpty, fromRecord,
   updateValuesByUnique, updateValuesByPrimary)

{- $concepts
On most drivers for SQL database,
we need to write or read untyped SQL value sequence
when accessing databases.

This library maps between list of untyped SQL type
and Haskell record type using type classes.
-}

{- $bindSqlAndHaskellRecords
You will need to implement instances of 'Persistable' class
to bind between SQL database values and Haskell records.
'Persistable' instance is source to derive 'FromSql' and 'ToSql'.

You can use Database.Record.TH module in this package
to generate instances from SQL database record column names and types.
-}

{- $constraintsForInference
You will need to implement instances of
'HasColumnConstraint' 'NotNull' which is a premise
to infer 'RecordFromSql' proof object using 'ToSql' 'q' ('Maybe' a) instance.
This proof object cat convert from SQL type into 'Maybe' typed record
when dealing with outer joined query.
-}
