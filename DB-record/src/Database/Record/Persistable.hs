{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Database.Record.Persistable
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Record.Persistable (
  Singleton(runSingleton), singleton,

  PersistableNullValue(runPersistableNullValue), persistableNullValue,

  PersistableRecordWidth(runPersistableRecordWidth), valueWidth,
  PersistableSqlValue, persistableSqlValue,

  PersistableRecord, persistableRecord,
  toRecord, fromRecord, width,

  persistableFromValue, persistableSingletonFromValue,

  PersistableNull(..), sqlNullValue,
  PersistableValue (..), fromSql, toSql,
  derivedPersistableRecord, derivedPersistableSingleton,
  PersistableWidth (..), persistableRecordWidth, singletonWidth, (<&>),
  Persistable (..), takeRecord
  ) where

-- Singleton value record.
newtype Singleton a = Singleton { runSingleton :: a }

singleton :: a -> Singleton a
singleton = Singleton


newtype PersistableNullValue q =
  PersistableNullValue
  { runPersistableNullValue :: q }

persistableNullValue :: q -> PersistableNullValue q
persistableNullValue =  PersistableNullValue


data PersistableSqlValue q a =
  PersistableSqlValue
  { toValue :: q -> a
  , fromValue   :: a -> q
  }

persistableSqlValue :: (q -> a) -> (a -> q) -> PersistableSqlValue q a
persistableSqlValue =  PersistableSqlValue


newtype PersistableRecordWidth a =
  PersistableRecordWidth { runPersistableRecordWidth :: Int }

persistableRecordWidth :: Int -> PersistableRecordWidth a
persistableRecordWidth =  PersistableRecordWidth

valueWidth :: PersistableRecordWidth a
valueWidth =  persistableRecordWidth 1

singletonWidth :: PersistableRecordWidth (Singleton a)
singletonWidth =  persistableRecordWidth 1

(<&>) :: PersistableRecordWidth a -> PersistableRecordWidth b -> PersistableRecordWidth (a, b)
a <&> b = PersistableRecordWidth $ runPersistableRecordWidth a + runPersistableRecordWidth b


data PersistableRecord q a =
  PersistableRecord
  { widthOfRecord :: PersistableRecordWidth a
  , toRecord      :: [q] -> a
  , fromRecord    :: a -> [q]
  }

width :: PersistableRecord q a -> Int
width =  runPersistableRecordWidth . widthOfRecord

persistableRecord :: PersistableRecordWidth a -> ([q] -> a) -> (a -> [q]) -> PersistableRecord q a
persistableRecord =  PersistableRecord

persistableFromValue :: PersistableRecordWidth a -> PersistableSqlValue q a -> PersistableRecord q a
persistableFromValue pw pv =
  persistableRecord pw (toValue pv . head) ((:[]) . fromValue pv)

persistableSingletonFromValue :: PersistableRecordWidth (Singleton a) -> PersistableSqlValue q a -> PersistableRecord q (Singleton a)
persistableSingletonFromValue pw pv =
  persistableRecord pw (singleton . toValue pv . head) ((:[]) . fromValue pv . runSingleton)


class Eq q => PersistableNull q where
  persistableNull :: PersistableNullValue q

sqlNullValue :: PersistableNull q => q
sqlNullValue =  runPersistableNullValue persistableNull


class PersistableWidth a where
  persistableWidth :: PersistableRecordWidth a

instance PersistableWidth (Singleton a) where
  persistableWidth = singletonWidth

instance (PersistableWidth a, PersistableWidth b) => PersistableWidth (a, b) where
  persistableWidth = persistableWidth <&> persistableWidth


class PersistableValue q a where
  persistableValue :: PersistableSqlValue q a

fromSql :: PersistableValue q a => q -> a
fromSql =  toValue persistableValue

toSql :: PersistableValue q a => a -> q
toSql =  fromValue persistableValue

derivedPersistableRecord :: (PersistableWidth a, PersistableValue q a) => PersistableRecord q a
derivedPersistableRecord =  persistableFromValue persistableWidth persistableValue

derivedPersistableSingleton :: (PersistableWidth (Singleton a), PersistableValue q a) => PersistableRecord q (Singleton a)
derivedPersistableSingleton =  persistableSingletonFromValue persistableWidth persistableValue


class PersistableWidth a => Persistable q a where
  persistable :: PersistableRecord q a


takeRecord :: PersistableRecord q a -> [q] -> (a, [q])
takeRecord rec vals = (toRecord rec va, vr) where
  (va, vr) = splitAt (width rec) vals
