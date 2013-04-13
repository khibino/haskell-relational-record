{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Database.Record.FromSql
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Record.FromSql (
  RecordFromSql, runTakeRecord, runToRecord,
  createRecordFromSql,

  recordDeSerializer,

  (<&>),

  outer,

  FromSql (recordFromSql), recordFromSql',
  takeRecord, toRecord,
  ) where

import Database.Record.Persistable
  (PersistableRecord, Singleton,
   Persistable(persistable), PersistableNull)
import qualified Database.Record.Persistable as Persistable
import Database.Record.KeyConstraint
  (HasKeyConstraint(constraintKey), KeyConstraint, NotNull, index)

import Control.Monad (liftM, ap)
import Control.Applicative ((<$>), Applicative(pure, (<*>)))


newtype RecordFromSql q a =
  RecordFromSql
  { runTakeRecord :: [q] -> (a, [q]) }

createRecordFromSql :: ([q] -> (a, [q])) -> RecordFromSql q a
createRecordFromSql =  RecordFromSql

recordDeSerializer :: PersistableRecord q a -> RecordFromSql q a
recordDeSerializer =  createRecordFromSql . Persistable.takeRecord

runToRecord :: RecordFromSql q a -> [q] -> a
runToRecord r = fst . runTakeRecord r

instance Monad (RecordFromSql q) where
  return a = createRecordFromSql ((,) a)
  ma >>= fmb =
    createRecordFromSql
    (\vals -> let (a, vals') = runTakeRecord ma vals
              in  runTakeRecord (fmb a) vals')

instance Functor (RecordFromSql q) where
  fmap = liftM

instance Applicative (RecordFromSql q) where
  pure  = return
  (<*>) = ap

(<&>) :: RecordFromSql q a -> RecordFromSql q b -> RecordFromSql q (a, b)
a <&> b = (,) <$> a <*> b

infixl 4 <&>


outer :: PersistableNull q
      => RecordFromSql q a
      -> KeyConstraint NotNull a
      -> RecordFromSql q (Maybe a)
outer rec pkey = createRecordFromSql mayToRec where
  mayToRec vals
    | vals !! index pkey /= Persistable.sqlNullValue = (Just a,  vals')
    | otherwise                         = (Nothing, vals')  where
      (a, vals') = runTakeRecord rec vals


class FromSql q a where
  recordFromSql :: RecordFromSql q a

recordFromSql' :: Persistable q a => RecordFromSql q a
recordFromSql' =  recordDeSerializer persistable

instance Persistable q (Singleton a) => FromSql q (Singleton a)  where
  recordFromSql = recordFromSql'

instance (FromSql q a, FromSql q b) => FromSql q (a, b)  where
  recordFromSql = recordFromSql <&> recordFromSql

instance (HasKeyConstraint NotNull a, FromSql q a, PersistableNull q)
         => FromSql q (Maybe a)  where
  recordFromSql = outer recordFromSql $ constraintKey

takeRecord :: FromSql q a => [q] -> (a, [q])
takeRecord =  runTakeRecord recordFromSql

toRecord :: FromSql q a => [q] -> a
toRecord = fst . takeRecord
