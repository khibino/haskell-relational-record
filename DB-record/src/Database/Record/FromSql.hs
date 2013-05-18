{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

  maybeRecord,

  FromSql (recordFromSql), recordFromSql',
  takeRecord, toRecord,
  ) where

import Database.Record.Persistable
  (PersistableRecord,
   Persistable(persistable), PersistableType)
import qualified Database.Record.Persistable as Persistable
import Database.Record.KeyConstraint
  (HasKeyConstraint(keyConstraint), KeyConstraint, NotNull, index)

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


maybeRecord :: PersistableType q
            => RecordFromSql q a
            -> KeyConstraint NotNull a
            -> RecordFromSql q (Maybe a)
maybeRecord rec pkey = createRecordFromSql mayToRec where
  mayToRec vals
    | vals !! index pkey /= Persistable.sqlNullValue = (Just a,  vals')
    | otherwise                         = (Nothing, vals')  where
      (a, vals') = runTakeRecord rec vals


class FromSql q a where
  recordFromSql :: RecordFromSql q a

recordFromSql' :: Persistable q a => RecordFromSql q a
recordFromSql' =  recordDeSerializer persistable

instance (FromSql q a, FromSql q b) => FromSql q (a, b)  where
  recordFromSql = recordFromSql <&> recordFromSql

instance (HasKeyConstraint NotNull a, FromSql q a, PersistableType q)
         => FromSql q (Maybe a)  where
  recordFromSql = maybeRecord recordFromSql keyConstraint

instance FromSql q () where
  recordFromSql = recordFromSql'

takeRecord :: FromSql q a => [q] -> (a, [q])
takeRecord =  runTakeRecord recordFromSql

toRecord :: FromSql q a => [q] -> a
toRecord =  runToRecord recordFromSql
