{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Record.ToSql
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines interfaces
-- from Haskell type into list of SQL type.
module Database.Record.ToSql (
  -- * Conversion from record type into list of SQL type
  RecordToSql, runFromRecord,
  createRecordToSql,

  recordSerializer,

  (<&>),

  -- * Inference rules of 'RecordToSql' conversion
  ToSql (recordToSql), recordToSql',
  fromRecord,

  -- * Make parameter list for updating with key
  updateValuesByUnique',
  updateValuesByUnique,
  updateValuesByPrimary
  ) where

import Data.Array (listArray, (!))
import Data.Set (toList, fromList, (\\))

import Database.Record.Persistable
  (PersistableRecord, Persistable(persistable))
import Database.Record.KeyConstraint
  (Primary, Unique, KeyConstraint, HasKeyConstraint(keyConstraint), unique, indexes)
import qualified Database.Record.Persistable as Persistable


-- | Proof object type to convert from Haskell type 'a' into list of SQL type ['q'].
data RecordToSql q a = RecordToSql (a -> [q])

-- | Run 'RecordToSql' proof object. Convert from Haskell type 'a' into list of SQL type ['q'].
runFromRecord :: RecordToSql q a -- ^ Proof object which has capability to convert
              -> a               -- ^ Haskell type
              -> [q]             -- ^ list of SQL type
runFromRecord (RecordToSql f) = f

-- | Axiom of 'RecordToSql' for SQL type 'q' and Haksell type 'a'.
createRecordToSql :: (a -> [q])      -- ^ Convert function body
                  -> RecordToSql q a -- ^ Result proof object
createRecordToSql =  RecordToSql

-- | Derive 'RecordToSql' proof object from 'PersistableRecord'.
recordSerializer :: PersistableRecord q a -> RecordToSql q a
recordSerializer =  createRecordToSql . Persistable.fromRecord

-- | Inferred 'RecordToSql' proof object.
recordToSql' :: Persistable q a => RecordToSql q a
recordToSql' =  recordSerializer persistable

-- | Derivation rule of 'RecordToSql' proof object for Haskell tuple (,) type.
(<&>) :: RecordToSql q a -> RecordToSql q b -> RecordToSql q (a, b)
ra <&> rb = RecordToSql (\(a, b) -> runFromRecord ra a ++ runFromRecord rb b)

infixl 4 <&>


-- | Inference rule interface for 'RecordToSql' proof object.
class ToSql q a where
  -- | Infer 'RecordToSql' proof object.
  recordToSql :: RecordToSql q a

-- | Inference rule of 'RecordToSql' proof object which can convert
--   from Haskell tuple ('a', 'b') type into list of SQL type ['q'].
instance (ToSql q a, ToSql q b) => ToSql q (a, b) where
  recordToSql = recordToSql <&> recordToSql

-- | Inference rule of 'RecordToSql' proof object which can convert
--   from Haskell unit () type into /empty/ list of SQL type ['q'].
instance ToSql q () where
  recordToSql = recordToSql'

-- | Run inferred 'RecordToSql' proof object.
--   Convert from haskell type 'a' into list of SQL type ['q'].
fromRecord :: ToSql q a => a -> [q]
fromRecord =  runFromRecord recordToSql

-- | Unsafely specify key index to convert from Haskell type `ra`
--   into SQL value `q` list expected by update form like
--
-- /UPDATE <table> SET c0 = ?, c1 = ?, ..., cn = ? WHERE key0 = ? AND key1 = ? AND key2 = ? ... /
--
--   using 'RecordToSql' proof object.
unsafeUpdateValuesWithIndexes :: RecordToSql q ra
                              -> [Int]
                              -> ra
                              -> [q]
unsafeUpdateValuesWithIndexes pr key a =
  [ valsA ! i | i <- otherThanKey ++ key ]  where
    vals = runFromRecord pr a
    maxIx = length vals - 1
    valsA = listArray (0, maxIx) vals
    otherThanKey = toList $ fromList [0 .. maxIx] \\ fromList key

-- | Convert from Haskell type `ra` into SQL value `q` list expected by update form like
--
-- /UPDATE <table> SET c0 = ?, c1 = ?, ..., cn = ? WHERE key = ?/
--
--   using 'RecordToSql' proof object.
updateValuesByUnique' :: RecordToSql q ra
                      -> KeyConstraint Unique ra -- ^ Unique key table constraint proof object.
                      -> ra
                      -> [q]
updateValuesByUnique' pr uk = unsafeUpdateValuesWithIndexes pr (indexes uk)

-- | Convert like 'updateValuesByUnique'' using inferred 'RecordToSql' proof object.
updateValuesByUnique :: ToSql q ra
                     => KeyConstraint Unique ra -- ^ Unique key table constraint proof object.
                     -> ra
                     -> [q]
updateValuesByUnique = updateValuesByUnique' recordToSql

-- | Convert like 'updateValuesByUnique'' using inferred 'RecordToSql' and 'ColumnConstraint' proof objects.
updateValuesByPrimary :: (HasKeyConstraint Primary ra, ToSql q ra)
                      => ra -> [q]
updateValuesByPrimary =  updateValuesByUnique (unique keyConstraint)
