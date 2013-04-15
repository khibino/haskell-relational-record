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
  PersistableSqlValue, persistableSqlValue,
  PersistableRecord, persistableRecord,
  toRecord, fromRecord, width,

  persistableSingletonFromValue,

  PersistableNull(..), sqlNullValue,
  PersistableValue (..), fromSql, toSql,
  Persistable (..), takeRecord
  ) where

-- Singleton value record.
newtype Singleton a = Singleton { runSingleton :: a }

newtype PersistableNullValue q =
  PersistableNullValue
  { runPersistableNullValue :: q }

data PersistableSqlValue q a =
  PersistableSqlValue
  { toValue :: q -> a
  , fromValue   :: a -> q
  }

data PersistableRecord q a =
  PersistableRecord
  { toRecord   :: [q] -> a
  , fromRecord :: a -> [q]
  , width      :: !Int
  }

singleton :: a -> Singleton a
singleton = Singleton

persistableNullValue :: q -> PersistableNullValue q
persistableNullValue =  PersistableNullValue

persistableSqlValue :: (q -> a) -> (a -> q) -> PersistableSqlValue q a
persistableSqlValue =  PersistableSqlValue

persistableRecord :: ([q] -> a) -> (a -> [q]) -> Int -> PersistableRecord q a
persistableRecord =  PersistableRecord

persistableSingletonFromValue :: PersistableSqlValue q a -> PersistableRecord q (Singleton a)
persistableSingletonFromValue pv =
  persistableRecord(singleton . toValue pv . head) ((:[]) . fromValue pv . runSingleton) 1

class Eq q => PersistableNull q where
  persistableNull :: PersistableNullValue q

sqlNullValue :: PersistableNull q => q
sqlNullValue =  runPersistableNullValue persistableNull

class PersistableValue q a where
  persistableValue :: PersistableSqlValue q a

fromSql :: PersistableValue q a => q -> a
fromSql =  toValue persistableValue

toSql :: PersistableValue q a => a -> q
toSql =  fromValue persistableValue

class Persistable q a where
  persistable :: PersistableRecord q a

takeRecord :: PersistableRecord q a -> [q] -> (a, [q])
takeRecord rec vals = (toRecord rec va, vr) where
  (va, vr) = splitAt (width rec) vals
