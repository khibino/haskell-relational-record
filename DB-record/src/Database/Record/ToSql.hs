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
-- This module includes interfaces
-- from Haskell record into SQL value list.
module Database.Record.ToSql (
  RecordToSql, runFromRecord,
  createRecordToSql,

  recordSerializer,

  ToSql (recordToSql), recordToSql',
  fromRecord,

  updateValuesByUnique',
  updateValuesByUnique,
  updateValuesByPrimary
  ) where

import Database.Record.Persistable
  (PersistableRecord, Persistable(persistable))
import Database.Record.KeyConstraint
  (HasKeyConstraint(keyConstraint), KeyConstraint, Primary, Unique, unique, index)
import qualified Database.Record.Persistable as Persistable


-- | Proof object type to convert from haskell type `a` into sql value type `q` list.
data RecordToSql q a = RecordToSql (a -> [q])

-- | Run 'RecordToSql' proof object.
runFromRecord :: RecordToSql q a -> a -> [q]
runFromRecord (RecordToSql f) = f

-- | Construct function 'RecordToSql' proof object.
createRecordToSql :: (a -> [q]) -> RecordToSql q a
createRecordToSql =  RecordToSql


class ToSql q a where
  recordToSql :: RecordToSql q a

recordSerializer :: PersistableRecord q a -> RecordToSql q a
recordSerializer =  createRecordToSql . Persistable.fromRecord

(<&>) :: RecordToSql q a -> RecordToSql q b -> RecordToSql q (a, b)
ra <&> rb = RecordToSql (\(a, b) -> runFromRecord ra a ++ runFromRecord rb b)

instance (ToSql q a, ToSql q b) => ToSql q (a, b) where
  recordToSql = recordToSql <&> recordToSql

recordToSql' :: Persistable q a => RecordToSql q a
recordToSql' =  recordSerializer persistable

instance ToSql q () where
  recordToSql = recordToSql'

fromRecord :: ToSql q a => a -> [q]
fromRecord =  runFromRecord recordToSql

-- | SQL values expected by update form like
--
-- /UPDATE <table> SET c0 = ?, c1 = ?, ..., cn = ? WHERE key = ?/
updateValuesByUnique' :: RecordToSql q ra
                      -> KeyConstraint Unique ra
                      -> ra
                      -> [q]
updateValuesByUnique' pr uk a = hd ++ tl ++ [key]  where
  (hd, key:tl) = splitAt (index uk) (runFromRecord pr a)

updateValuesByUnique :: ToSql q ra
                     => KeyConstraint Unique ra
                     -> ra
                     -> [q]
updateValuesByUnique = updateValuesByUnique' recordToSql

updateValuesByPrimary :: (HasKeyConstraint Primary a, ToSql q a) =>
                         a -> [q]
updateValuesByPrimary =  updateValuesByUnique (unique keyConstraint)
