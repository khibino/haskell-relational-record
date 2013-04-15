{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.HDBC.Record.Persistable (
  persistableSingleton
  ) where

import Database.Record.Persistable
  (Singleton, singleton, runSingleton,
   persistableNullValue, PersistableNull (..),
   PersistableSqlValue, -- persistableSqlValue,
   PersistableRecord, persistableRecordFromValue,
   PersistableValue (persistableValue),
   Persistable (persistable))
import qualified Database.Record.Persistable as Persistable

import Data.Convertible (Convertible)
import Database.HDBC (SqlValue(SqlNull), fromSql, toSql)

persistableSqlValue :: (Convertible SqlValue a, Convertible a SqlValue)
                       => PersistableSqlValue SqlValue (Singleton a)
persistableSqlValue =  Persistable.persistableSqlValue (singleton . fromSql) (toSql . runSingleton)

persistableSingleton :: (Convertible SqlValue a, Convertible a SqlValue)
                     => PersistableRecord SqlValue (Singleton a)
persistableSingleton =  persistableRecordFromValue persistableSqlValue

instance PersistableNull SqlValue  where
  persistableNull = persistableNullValue SqlNull

instance (Convertible SqlValue a, Convertible a SqlValue)
         => PersistableValue SqlValue (Singleton a)  where
  persistableValue = persistableSqlValue

instance (Convertible SqlValue a, Convertible a SqlValue)
         => Persistable SqlValue (Singleton a)  where
  persistable = persistableSingleton
