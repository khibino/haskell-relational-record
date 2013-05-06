{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.HDBC.Record.Persistable (
  ) where

import Database.Record.Persistable
  (Singleton,
   persistableNullValue, PersistableNull (..),
   PersistableSqlValue,
   PersistableWidth (),
   PersistableValue (persistableValue), derivedPersistableSingleton,
   Persistable (persistable))
import qualified Database.Record.Persistable as Persistable
import Database.HDBC.Record.TH (derivePersistableInstancesFromConvertibleSqlValues)

import Data.Convertible (Convertible)
import Database.HDBC (SqlValue(SqlNull), fromSql, toSql)

persistableSqlValue :: (Convertible SqlValue a, Convertible a SqlValue)
                       => PersistableSqlValue SqlValue a
persistableSqlValue =  Persistable.persistableSqlValue fromSql toSql

instance PersistableNull SqlValue  where
  persistableNull = persistableNullValue SqlNull

instance (Convertible SqlValue a, Convertible a SqlValue)
         => PersistableValue SqlValue a  where
  persistableValue = persistableSqlValue

$(derivePersistableInstancesFromConvertibleSqlValues)

instance (Convertible SqlValue a, Convertible a SqlValue, PersistableWidth (Singleton a))
         => Persistable SqlValue (Singleton a)  where
  persistable = derivedPersistableSingleton
