{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

  PersistableSqlType(runPersistableNullValue), persistableSqlTypeFromNull,

  PersistableRecordWidth(runPersistableRecordWidth),
  valueWidth, singletonWidth, (<&>), maybeWidth,
  PersistableSqlValue, persistableSqlValue,

  PersistableRecord, persistableRecord,
  toRecord, fromRecord, width,

  persistableFromValue, persistableSingletonFromValue,

  PersistableType(..), sqlNullValue,
  PersistableValue (..), fromSql, toSql,
  derivedPersistableValueRecord, derivedPersistableSingleton,
  PersistableWidth (..), persistableRecordWidth,
  Persistable (..), takeRecord
  ) where

-- | Singleton value record.
newtype Singleton a = Singleton { runSingleton :: a }

singleton :: a -> Singleton a
singleton = Singleton


-- | Proof object to specify 'q' is SQL type
newtype PersistableSqlType q =
  PersistableSqlType
  { runPersistableNullValue :: q }

-- | Proof for PersistableSqlType requires SQL null value
persistableSqlTypeFromNull :: q -> PersistableSqlType q
persistableSqlTypeFromNull =  PersistableSqlType


-- | Proof object to specify value type 'a' is convertible with SQL type 'q'
data PersistableSqlValue q a =
  PersistableSqlValue
  { toValue     :: q -> a
  , fromValue   :: a -> q
  }

-- | Proof for PersistableSqlValue q a
persistableSqlValue :: PersistableSqlType q -> (q -> a) -> (a -> q) -> PersistableSqlValue q a
persistableSqlValue =  const PersistableSqlValue


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

maybeWidth :: PersistableRecordWidth a -> PersistableRecordWidth (Maybe a)
maybeWidth =  PersistableRecordWidth . runPersistableRecordWidth

voidWidth :: PersistableRecordWidth ()
voidWidth =  persistableRecordWidth 0


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

persistableVoid :: PersistableRecord q ()
persistableVoid =  persistableRecord voidWidth (const ()) (const [])


class Eq q => PersistableType q where
  persistableType :: PersistableSqlType q

sqlNullValue :: PersistableType q => q
sqlNullValue =  runPersistableNullValue persistableType


class PersistableWidth a where
  persistableWidth :: PersistableRecordWidth a

instance PersistableWidth (Singleton a) where
  persistableWidth = singletonWidth

instance (PersistableWidth a, PersistableWidth b) => PersistableWidth (a, b) where
  persistableWidth = persistableWidth <&> persistableWidth

instance PersistableWidth a => PersistableWidth (Maybe a) where
  persistableWidth = maybeWidth persistableWidth

instance PersistableWidth () where
  persistableWidth = voidWidth


class PersistableType q => PersistableValue q a where
  persistableValue :: PersistableSqlValue q a

fromSql :: PersistableValue q a => q -> a
fromSql =  toValue persistableValue

toSql :: PersistableValue q a => a -> q
toSql =  fromValue persistableValue

derivedPersistableValueRecord :: (PersistableWidth a, PersistableValue q a) => PersistableRecord q a
derivedPersistableValueRecord =  persistableFromValue persistableWidth persistableValue

derivedPersistableSingleton :: (PersistableWidth (Singleton a), PersistableValue q a) => PersistableRecord q (Singleton a)
derivedPersistableSingleton =  persistableSingletonFromValue persistableWidth persistableValue


class PersistableWidth a => Persistable q a where
  persistable :: PersistableRecord q a

instance Persistable q () where
  persistable = persistableVoid


takeRecord :: PersistableRecord q a -> [q] -> (a, [q])
takeRecord rec vals = (toRecord rec va, vr) where
  (va, vr) = splitAt (width rec) vals
