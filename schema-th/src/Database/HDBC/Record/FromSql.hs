{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Database.HDBC.Record.FromSql
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.Record.FromSql (
  RecordFromSql, runTakeRecord, runToRecord,
  createRecordFromSql,

  recordDeSerializer,

  (<&>),

  outer,

  FromSql (recordFromSql), recordFromSql',
  takeRecord, toRecord,
  ) where

import Database.HDBC.Record.Persistable
  (PersistableRecord, Singleton, Persistable(persistable))
import Database.HDBC.Record.KeyConstraint
  (HasKeyConstraint(constraintKey), KeyConstraint, NotNull, index)
import qualified Database.HDBC.Record.Persistable as Persistable

import Database.HDBC (SqlValue(SqlNull))
import Control.Monad (liftM, ap)
import Control.Applicative ((<$>), Applicative(pure, (<*>)))


newtype RecordFromSql a =
  RecordFromSql
  { runTakeRecord :: [SqlValue] -> (a, [SqlValue]) }

createRecordFromSql :: ([SqlValue] -> (a, [SqlValue])) -> RecordFromSql a
createRecordFromSql =  RecordFromSql

recordDeSerializer :: PersistableRecord a -> RecordFromSql a
recordDeSerializer =  createRecordFromSql . Persistable.takeRecord

runToRecord :: RecordFromSql a -> [SqlValue] -> a
runToRecord r = fst . runTakeRecord r


instance Monad RecordFromSql where
  return a = createRecordFromSql ((,) a)
  ma >>= fmb =
    createRecordFromSql (\vals ->
                           let (a, vals') = runTakeRecord ma vals
                           in  runTakeRecord (fmb a) vals')

instance Functor RecordFromSql where
  fmap = liftM

instance Applicative RecordFromSql where
  pure  = return
  (<*>) = ap

(<&>) :: RecordFromSql a -> RecordFromSql b -> RecordFromSql (a, b)
a <&> b = (,) <$> a <*> b

infixl 4 <&>


outer :: RecordFromSql a -> KeyConstraint NotNull a -> RecordFromSql (Maybe a)
outer rec pkey = createRecordFromSql mayToRec where
  mayToRec vals
    | vals !! index pkey /= SqlNull = (Just a,  vals')
    | otherwise                     = (Nothing, vals')  where
      (a, vals') = runTakeRecord rec vals


class FromSql a where
  recordFromSql :: RecordFromSql a

recordFromSql' :: Persistable a => RecordFromSql a
recordFromSql' =  recordDeSerializer persistable

instance Persistable (Singleton a) => FromSql (Singleton a) where
  recordFromSql = recordFromSql'

instance (FromSql a, FromSql b) => FromSql (a, b)  where
  recordFromSql = recordFromSql <&> recordFromSql

instance (HasKeyConstraint NotNull a, FromSql a) => FromSql (Maybe a) where
  recordFromSql = outer recordFromSql $ constraintKey

takeRecord :: FromSql a => [SqlValue] -> (a, [SqlValue])
takeRecord =  runTakeRecord recordFromSql

toRecord :: FromSql a => [SqlValue] -> a
toRecord = fst . takeRecord
