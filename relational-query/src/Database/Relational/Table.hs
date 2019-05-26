-- |
-- Module      : Database.Relational.Table
-- Copyright   : 2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines interfaces about table metadata type.
module Database.Relational.Table (
  -- * Phantom typed table type
  Table, untype, tableName, shortName, width, tableColumns, index, table,

  -- * Table existence inference
  TableDerivable (..),

  -- * Deprecated
  name, columns,
  recordWidth, toSubQuery, toMaybe,
  ) where


import Database.Relational.Typed.Table
  (Table, untype, tableName, shortName, width, tableColumns, index, table,
   TableDerivable (..),)

-- required from deprecated definitions
import Database.Record.Persistable (PersistableRecordWidth)
import Database.Relational.Internal.String (showStringSQL)
import Database.Relational.SqlSyntax (SubQuery)
import Database.Relational.Typed.Table
  (name, columns,)
import qualified Database.Relational.Typed.Table as Typed

{-# DEPRECATED
    recordWidth, toSubQuery, toMaybe
    "low-level API, will be dropped in the future." #-}
recordWidth :: Table r -> PersistableRecordWidth r
recordWidth = Typed.recordWidth

toSubQuery :: Table r -> SubQuery
toSubQuery = Typed.toSubQuery

toMaybe :: Table r -> Table (Maybe r)
toMaybe t = table (tableName t) (map showStringSQL $ tableColumns t)
