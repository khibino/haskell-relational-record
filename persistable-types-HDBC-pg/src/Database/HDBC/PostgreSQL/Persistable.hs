{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Database.HDBC.PostgreSQL.Persistable
-- Copyright   : 2015-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines Persistable instances for PostgreSQL types
module Database.HDBC.PostgreSQL.Persistable () where

import Data.Convertible (convert)
import Data.PostgreSQL.NetworkAddress (Inet, Cidr)
import Database.HDBC (SqlValue)
import Database.Relational.HDBC.Persistable ()
import Database.Record.FromSql (FromSql (..), valueRecordFromSql)
import Database.Record.ToSql (ToSql (..), valueRecordToSql)
import Database.Record.TH (deriveNotNullType)

import Database.HDBC.PostgreSQL.Instances ()


$(deriveNotNullType [t| Inet |])

$(deriveNotNullType [t| Cidr |])

instance FromSql SqlValue Inet where
  recordFromSql = valueRecordFromSql convert

instance FromSql SqlValue Cidr where
  recordFromSql = valueRecordFromSql convert

instance ToSql SqlValue Inet where
  recordToSql = valueRecordToSql convert

instance ToSql SqlValue Cidr where
  recordToSql = valueRecordToSql convert
