{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.HDBC.Record.Persistable (
  persistableSqlValue
  ) where

import Database.Record (PersistableSqlValue, PersistableType (..), PersistableValue (..))
import Database.Record.Persistable (unsafePersistableSqlTypeFromNull)
import qualified Database.Record.Persistable as Record
import Database.HDBC.Record.InternalTH (derivePersistableInstancesFromConvertibleSqlValues)

import Data.Convertible (Convertible)
import Database.HDBC (SqlValue(SqlNull), fromSql, toSql)

instance PersistableType SqlValue  where
  persistableType = unsafePersistableSqlTypeFromNull SqlNull

persistableSqlValue :: (Convertible SqlValue a, Convertible a SqlValue)
                       => PersistableSqlValue SqlValue a
persistableSqlValue =  Record.persistableSqlValue persistableType fromSql toSql

instance (Convertible SqlValue a, Convertible a SqlValue)
         => PersistableValue SqlValue a  where
  persistableValue = persistableSqlValue

$(derivePersistableInstancesFromConvertibleSqlValues)
