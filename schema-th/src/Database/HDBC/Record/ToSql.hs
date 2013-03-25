{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Database.HDBC.Record.ToSql
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.Record.ToSql (
  RecordToSql, fromRecord,
  createRecordToSql,

  recordSerializer,

  ToSql (recordToSql), recordToSql',

  updateValuesByUnique,
  updateValuesByPrimary
  ) where

import Database.HDBC.Record.Persistable
  (PersistableRecord, Persistable(persistable), Singleton)
import Database.HDBC.Record.KeyConstraint
  (HasKeyConstraint(constraintKey), KeyConstraint, Primary, Unique, unique, index)
import qualified Database.HDBC.Record.Persistable as Persistable

import Database.HDBC (SqlValue)


data RecordToSql a =
  RecordToSql
  { fromRecord :: a -> [SqlValue] }

createRecordToSql :: (a -> [SqlValue]) -> RecordToSql a
createRecordToSql =  RecordToSql


class ToSql a where
  recordToSql :: RecordToSql a

recordSerializer :: PersistableRecord a -> RecordToSql a
recordSerializer =  createRecordToSql . Persistable.fromRecord

instance Persistable (Singleton a) => ToSql (Singleton a) where
  recordToSql = recordSerializer persistable

(<&>) :: RecordToSql a -> RecordToSql b -> RecordToSql (a, b)
ra <&> rb = RecordToSql (\(a, b) -> fromRecord ra a ++ fromRecord rb b)

instance (ToSql a, ToSql b) => ToSql (a, b) where
  recordToSql = recordToSql <&> recordToSql

recordToSql' :: Persistable a => RecordToSql a
recordToSql' =  recordSerializer persistable


updateValuesByUnique :: RecordToSql ra
              -> KeyConstraint Unique ra
              -> ra
              -> [SqlValue]
updateValuesByUnique pr uk a = hd ++ tl  where
  (hd, _uk:tl) = splitAt (index uk) (fromRecord pr a)

updateValuesByPrimary :: (HasKeyConstraint Primary a, ToSql a) =>
                           a -> [SqlValue]
updateValuesByPrimary =  updateValuesByUnique recordToSql (unique constraintKey)
