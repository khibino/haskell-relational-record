{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Database.Record.Singleton
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines persistable instances for
-- general single field record.
module Database.Record.Singleton (
  -- * Singleton type constructor
  -- $singletonTypeConstructor
  Singleton, runSingleton, singleton,

  singletonWidth,
  persistableSingletonFromValue,
  derivedPersistableSingleton
  ) where

import Database.Record.Persistable
  (PersistableRecordWidth, unsafePersistableRecordWidth,
   PersistableSqlValue, toValue, fromValue,
   PersistableRecord, persistableRecord,
   PersistableWidth (persistableWidth),
   PersistableValue (persistableValue),
   Persistable)
import Database.Record.FromSql (FromSql(recordFromSql), recordFromSql')
import Database.Record.ToSql (ToSql(recordToSql), recordToSql')


{- $singletonTypeConstructor
Singleton type constructor is useful to avoid overlapped instances.
-}

-- | General single field record.
newtype Singleton a = Singleton
                      { -- | Unwrap from 'Singleton'.
                        runSingleton :: a
                      }

-- | Wrap into 'Singleton'.
singleton :: a -> Singleton a
singleton = Singleton

-- | Axiom of 'PersistableRecordWidth' proof object. Single field recrod width is 1.
singletonWidth :: PersistableRecordWidth (Singleton a)
singletonWidth =  unsafePersistableRecordWidth 1

-- | Derived 'PersistableRecordWidth' proof object for 'Singleton'.
instance PersistableWidth (Singleton a) where
  persistableWidth = singletonWidth

-- | Derivation rule of 'PersistableRecord' for 'Singleton'.
persistableSingletonFromValue :: PersistableRecordWidth (Singleton a) -> PersistableSqlValue q a -> PersistableRecord q (Singleton a)
persistableSingletonFromValue pw pv =
  persistableRecord pw (singleton . toValue pv . head) ((:[]) . fromValue pv . runSingleton)

-- | Derived 'PersistableRecord' of 'Singleton'
derivedPersistableSingleton :: (PersistableWidth (Singleton a), PersistableValue q a) => PersistableRecord q (Singleton a)
derivedPersistableSingleton =  persistableSingletonFromValue persistableWidth persistableValue


-- | Inference rule for 'RecordFromSql'.
instance Persistable q (Singleton a) => FromSql q (Singleton a)  where
  recordFromSql = recordFromSql'

-- | Inference rule for 'RecordToSql'.
instance Persistable q (Singleton a) => ToSql q (Singleton a) where
  recordToSql = recordToSql'
