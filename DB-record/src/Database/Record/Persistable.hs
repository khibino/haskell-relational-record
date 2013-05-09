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
  PersistableSqlType(runPersistableNullValue), persistableSqlTypeFromNull,

  PersistableRecordWidth(runPersistableRecordWidth),
  valueWidth, (<&>), maybeWidth,
  PersistableSqlValue, persistableSqlValue,
  toValue, fromValue,

  PersistableRecord, persistableRecord,
  toRecord, fromRecord, width,

  persistableFromValue,

  PersistableType(..), sqlNullValue,
  PersistableValue (..), fromSql, toSql,
  derivedPersistableValueRecord,
  PersistableWidth (..), persistableRecordWidth,
  Persistable (..), takeRecord
  ) where


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

persistableVoid :: PersistableRecord q ()
persistableVoid =  persistableRecord voidWidth (const ()) (const [])


class Eq q => PersistableType q where
  persistableType :: PersistableSqlType q

sqlNullValue :: PersistableType q => q
sqlNullValue =  runPersistableNullValue persistableType


class PersistableWidth a where
  persistableWidth :: PersistableRecordWidth a

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


class PersistableWidth a => Persistable q a where
  persistable :: PersistableRecord q a

instance Persistable q () where
  persistable = persistableVoid


takeRecord :: PersistableRecord q a -> [q] -> (a, [q])
takeRecord rec vals = (toRecord rec va, vr) where
  (va, vr) = splitAt (width rec) vals
