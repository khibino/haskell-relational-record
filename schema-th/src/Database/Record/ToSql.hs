{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Database.Record.ToSql
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Record.ToSql (
  RecordToSql, fromRecord,
  createRecordToSql,

  recordSerializer,

  ToSql (recordToSql), recordToSql',

  updateValuesByUnique,
  updateValuesByPrimary
  ) where

import Database.Record.Persistable
  (PersistableRecord, Persistable(persistable), Singleton)
import Database.Record.KeyConstraint
  (HasKeyConstraint(constraintKey), KeyConstraint, Primary, Unique, unique, index)
import qualified Database.Record.Persistable as Persistable


data RecordToSql q a =
  RecordToSql
  { fromRecord :: a -> [q] }

createRecordToSql :: (a -> [q]) -> RecordToSql q a
createRecordToSql =  RecordToSql


class ToSql q a where
  recordToSql :: RecordToSql q a

recordSerializer :: PersistableRecord q a -> RecordToSql q a
recordSerializer =  createRecordToSql . Persistable.fromRecord

instance Persistable q (Singleton a) => ToSql q (Singleton a) where
  recordToSql = recordSerializer persistable

(<&>) :: RecordToSql q a -> RecordToSql q b -> RecordToSql q (a, b)
ra <&> rb = RecordToSql (\(a, b) -> fromRecord ra a ++ fromRecord rb b)

instance (ToSql q a, ToSql q b) => ToSql q (a, b) where
  recordToSql = recordToSql <&> recordToSql

recordToSql' :: Persistable q a => RecordToSql q a
recordToSql' =  recordSerializer persistable


updateValuesByUnique :: RecordToSql q ra
              -> KeyConstraint Unique ra
              -> ra
              -> [q]
updateValuesByUnique pr uk a = hd ++ tl  where
  (hd, _uk:tl) = splitAt (index uk) (fromRecord pr a)

updateValuesByPrimary :: (HasKeyConstraint Primary a, ToSql q a) =>
                         a -> [q]
updateValuesByPrimary =  updateValuesByUnique recordToSql (unique constraintKey)
