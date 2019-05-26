{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.Relational.SimpleSql
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines functions to generate simple SQL strings.
module Database.Relational.SimpleSql (
  -- * Update SQL
  updateOtherThanKeySQL,

  -- * Insert SQL
  insertPrefixSQL,

  -- * Deprecated
  QuerySuffix,
  ) where

import Data.Array (listArray, (!))
import Data.Monoid (mconcat, (<>))

import Language.SQL.Keyword (Keyword(..), (.=.), (|*|))
import qualified Language.SQL.Keyword as SQL
import Database.Record.ToSql (untypedUpdateValuesIndex)

import Database.Relational.Internal.String
  (StringSQL, stringSQL, showStringSQL, rowConsStringSQL, )
import Database.Relational.Typed.Table (Table, tableName, tableColumns, recordWidth)
import Database.Relational.Typed.Record (recordColumns, unsafeRecordFromColumns)

import Database.Relational.Pi (Pi, expandIndexes')
import qualified Database.Relational.Record as Record


-- | Type for query suffix words
type QuerySuffix = [Keyword]
{-# DEPRECATED QuerySuffix "Use '[Keyword]' instead of this." #-}

-- | Generate update SQL by specified key and table.
--   Columns name list of table are also required.
updateSQL' :: String      -- ^ Table name
           -> [StringSQL] -- ^ Column name list to update
           -> [StringSQL] -- ^ Key column name list
           -> String      -- ^ Result SQL
updateSQL' table cols key =
  showStringSQL $ mconcat
  [UPDATE, stringSQL table, SET, SQL.fold (|*|) updAssigns,
   WHERE, SQL.fold SQL.and keyAssigns]
  where
    assigns cs = [ c .=. "?" | c <- cs ]
    updAssigns = assigns cols
    keyAssigns = assigns key

-- | Generate update SQL by specified key and table.
--   Columns name list of table are also required.
updateOtherThanKeySQL' :: String      -- ^ Table name
                       -> [StringSQL] -- ^ Column name list
                       -> [Int]       -- ^ Key column indexes
                       -> String      -- ^ Result SQL
updateOtherThanKeySQL' table cols ixs =
  updateSQL' table updColumns keyColumns
  where
    width' = length cols
    cols' = listArray (0, width' -1) cols
    otherThanKey = untypedUpdateValuesIndex ixs width'
    columns' is = [ cols' ! i | i <- is ]
    updColumns = columns' otherThanKey
    keyColumns = columns' ixs

-- | Generate update SQL specified by single key.
updateOtherThanKeySQL :: Table r -- ^ Table metadata
          -> Pi r p  -- ^ Key columns
          -> String  -- ^ Result SQL
updateOtherThanKeySQL tbl key =
  updateOtherThanKeySQL' (tableName tbl) (tableColumns tbl) (expandIndexes' (recordWidth tbl) key)

-- | Generate prefix string of insert SQL.
insertPrefixSQL :: Pi r r' -> Table r -> StringSQL
insertPrefixSQL pi' table =
  INSERT <> INTO <> stringSQL (tableName table) <> rowConsStringSQL cols  where
    cols = recordColumns . Record.wpi (recordWidth table) (unsafeRecordFromColumns $ tableColumns table) $ pi'
