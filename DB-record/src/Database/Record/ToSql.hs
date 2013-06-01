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
-- from Haskell type into SQL value list.
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

import Database.Record.Persistable
  (PersistableRecord, Persistable(persistable))
import Database.Record.KeyConstraint
  (HasKeyConstraint(keyConstraint), KeyConstraint, Primary, Unique, unique, index)
import qualified Database.Record.Persistable as Persistable


-- | Proof object type to convert from Haskell type 'a' into list of SQL type ['q'].
data RecordToSql q a = RecordToSql (a -> [q])

-- | Run 'RecordToSql' proof object. Convert from Haskell type 'a' into list of SQL type ['q'].
runFromRecord :: RecordToSql q a -> a -> [q]
runFromRecord (RecordToSql f) = f

-- | Axiom of 'RecordToSql' for SQL type 'q' and Haksell type 'a'.
createRecordToSql :: (a -> [q]) -> RecordToSql q a
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

-- | Convert from Haskell type `ra` into SQL value `q` list expected by update form like
--
-- /UPDATE <table> SET c0 = ?, c1 = ?, ..., cn = ? WHERE key = ?/
--
--   using 'RecordToSql' proof object.
updateValuesByUnique' :: RecordToSql q ra
                      -> KeyConstraint Unique ra
                      -> ra
                      -> [q]
updateValuesByUnique' pr uk a = hd ++ tl ++ [key]  where
  (hd, key:tl) = splitAt (index uk) (runFromRecord pr a)

-- | Convert like 'updateValuesByUnique'' using inferred 'RecordToSql' proof object.
updateValuesByUnique :: ToSql q ra
                     => KeyConstraint Unique ra
                     -> ra
                     -> [q]
updateValuesByUnique = updateValuesByUnique' recordToSql

-- | Convert like 'updateValuesByUnique'' using inferred 'RecordToSql' and 'KeyConstraint' proof objects.
updateValuesByPrimary :: (HasKeyConstraint Primary a, ToSql q a) =>
                         a -> [q]
updateValuesByPrimary =  updateValuesByUnique (unique keyConstraint)
