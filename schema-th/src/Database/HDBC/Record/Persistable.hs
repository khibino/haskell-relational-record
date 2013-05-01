{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.HDBC.Record.Persistable (
  persistableSingleton
  ) where

import Database.Record.Persistable
  (Singleton,
   persistableNullValue, PersistableNull (..),
   PersistableSqlValue, -- persistableSqlValue,
   PersistableRecord, persistableSingletonFromValue,
   PersistableValue (persistableValue),
   PersistableWidth (),
   Persistable (persistable))
import qualified Database.Record.Persistable as Persistable

import Data.Convertible (Convertible)
import Database.HDBC (SqlValue(SqlNull), fromSql, toSql)

persistableSqlValue :: (Convertible SqlValue a, Convertible a SqlValue)
                       => PersistableSqlValue SqlValue a
persistableSqlValue =  Persistable.persistableSqlValue fromSql toSql

persistableSingleton :: (Convertible SqlValue a, Convertible a SqlValue, PersistableWidth (Singleton a))
                     => PersistableRecord SqlValue (Singleton a)
persistableSingleton =  persistableSingletonFromValue persistableSqlValue

instance PersistableNull SqlValue  where
  persistableNull = persistableNullValue SqlNull

instance (Convertible SqlValue a, Convertible a SqlValue)
         => PersistableValue SqlValue a  where
  persistableValue = persistableSqlValue

instance (Convertible SqlValue a, Convertible a SqlValue, PersistableWidth (Singleton a))
         => Persistable SqlValue (Singleton a)  where
  persistable = persistableSingleton
