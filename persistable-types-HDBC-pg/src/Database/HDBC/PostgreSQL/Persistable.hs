{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.HDBC.PostgreSQL.Persistable () where

import Data.PostgreSQL.NetworkAddress (Inet, Cidr)
import Database.HDBC (SqlValue)
import Database.HDBC.Record.Persistable ()
import Database.Record.Persistable (PersistableWidth (..), unsafeValueWidth)
import Database.Record.FromSql (FromSql (..), valueFromSql)
import Database.Record.ToSql (ToSql (..), valueToSql)

import Database.HDBC.PostgreSQL.Instances ()


instance PersistableWidth Inet where
  persistableWidth = unsafeValueWidth

instance PersistableWidth Cidr where
  persistableWidth = unsafeValueWidth

instance FromSql SqlValue Inet where
  recordFromSql = valueFromSql

instance FromSql SqlValue Cidr where
  recordFromSql = valueFromSql

instance ToSql SqlValue Inet where
  recordToSql = valueToSql

instance ToSql SqlValue Cidr where
  recordToSql = valueToSql
