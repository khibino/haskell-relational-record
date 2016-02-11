{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.HDBC.PostgreSQL.Persistable () where

import Data.Convertible (convert)
import Data.PostgreSQL.NetworkAddress (Inet, Cidr)
import Database.HDBC (SqlValue)
import Database.HDBC.Record.Persistable ()
import Database.Record.Persistable (PersistableWidth (..), unsafeValueWidth)
import Database.Record.FromSql (FromSql (..), valueRecordFromSql)
import Database.Record.ToSql (ToSql (..), valueRecordToSql)

import Database.HDBC.PostgreSQL.Instances ()


instance PersistableWidth Inet where
  persistableWidth = unsafeValueWidth

instance PersistableWidth Cidr where
  persistableWidth = unsafeValueWidth

instance FromSql SqlValue Inet where
  recordFromSql = valueRecordFromSql convert

instance FromSql SqlValue Cidr where
  recordFromSql = valueRecordFromSql convert

instance ToSql SqlValue Inet where
  recordToSql = valueRecordToSql convert

instance ToSql SqlValue Cidr where
  recordToSql = valueRecordToSql convert
