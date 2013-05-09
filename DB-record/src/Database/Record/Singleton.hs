{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Record.Singleton (
  Singleton, runSingleton, singleton,

  singletonWidth,
  persistableSingletonFromValue,
  derivedPersistableSingleton
  ) where

import Database.Record.Persistable
  (PersistableRecordWidth, persistableRecordWidth,
   PersistableSqlValue, toValue, fromValue,
   PersistableRecord, persistableRecord,
   PersistableWidth (persistableWidth),
   PersistableValue (persistableValue),
   Persistable)
import Database.Record.FromSql (FromSql(recordFromSql), recordFromSql')
import Database.Record.ToSql (ToSql(recordToSql), recordToSql')


-- | Singleton value record.
newtype Singleton a = Singleton { runSingleton :: a }

singleton :: a -> Singleton a
singleton = Singleton

singletonWidth :: PersistableRecordWidth (Singleton a)
singletonWidth =  persistableRecordWidth 1

instance PersistableWidth (Singleton a) where
  persistableWidth = singletonWidth

persistableSingletonFromValue :: PersistableRecordWidth (Singleton a) -> PersistableSqlValue q a -> PersistableRecord q (Singleton a)
persistableSingletonFromValue pw pv =
  persistableRecord pw (singleton . toValue pv . head) ((:[]) . fromValue pv . runSingleton)

derivedPersistableSingleton :: (PersistableWidth (Singleton a), PersistableValue q a) => PersistableRecord q (Singleton a)
derivedPersistableSingleton =  persistableSingletonFromValue persistableWidth persistableValue


instance Persistable q (Singleton a) => FromSql q (Singleton a)  where
  recordFromSql = recordFromSql'

instance Persistable q (Singleton a) => ToSql q (Singleton a) where
  recordToSql = recordToSql'
