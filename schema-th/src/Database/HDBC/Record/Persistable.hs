{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.HDBC.Record.Persistable (
  persistableSingleton
  ) where

import Database.Record.Persistable
  (Singleton, singleton, runSingleton,
   persistableNullValue, PersistableNull (..),
   PersistableValue, persistableValue,
   PersistableRecord, persistableRecordFromValue,
   Persistable (persistable))

import Data.Convertible (Convertible)
import Database.HDBC (SqlValue(SqlNull), fromSql, toSql)

persistableSqlValue :: (Convertible SqlValue a, Convertible a SqlValue)
                       => PersistableValue SqlValue (Singleton a)
persistableSqlValue =  persistableValue (singleton . fromSql) (toSql . runSingleton)

persistableSingleton :: (Convertible SqlValue a, Convertible a SqlValue)
                     => PersistableRecord SqlValue (Singleton a)
persistableSingleton =  persistableRecordFromValue persistableSqlValue

instance PersistableNull SqlValue  where
  persistableNull = persistableNullValue SqlNull

instance (Convertible SqlValue a, Convertible a SqlValue)
         => Persistable SqlValue (Singleton a)  where
  persistable = persistableSingleton
