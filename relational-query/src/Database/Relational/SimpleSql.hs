{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.Relational.SimpleSql
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines functions to generate simple SQL strings.
module Database.Relational.SimpleSql (
  -- * Query suffix
  QuerySuffix, showsQuerySuffix,

  -- * Update SQL
  updatePrefixSQL,
  updateOtherThanKeySQL,

  -- * Insert SQL
  insertPrefixSQL,

  -- * Delete SQL
  deletePrefixSQL
  ) where

import Data.Array (listArray, (!))
import Data.Monoid (mconcat, (<>))

import Language.SQL.Keyword (Keyword(..), (.=.), (|*|))
import qualified Language.SQL.Keyword as SQL
import Database.Record.ToSql (untypedUpdateValuesIndex)

import Database.Relational.Internal.String
  (StringSQL, stringSQL, showStringSQL, rowConsStringSQL, )

import Database.Relational.Pi (Pi, expandIndexes')
import Database.Relational.Table (Table, name, columns, recordWidth)
import qualified Database.Relational.Record as Record


-- | Type for query suffix words
type QuerySuffix = [Keyword]

-- | Expand query suffix words
showsQuerySuffix :: QuerySuffix -> StringSQL
showsQuerySuffix =  mconcat

-- | Generate prefix string of update SQL.
updatePrefixSQL :: Table r -> StringSQL
updatePrefixSQL table = UPDATE <> stringSQL (name table)

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
  updateOtherThanKeySQL' (name tbl) (columns tbl) (expandIndexes' (recordWidth tbl) key)

-- | Generate prefix string of insert SQL.
insertPrefixSQL :: Pi r r' -> Table r -> StringSQL
insertPrefixSQL pi' table =
  INSERT <> INTO <> stringSQL (name table) <> rowConsStringSQL cols  where
    cols = Record.columns . Record.wpi (recordWidth table) (Record.unsafeFromTable table) $ pi'

-- | Generate all column delete SQL by specified table. Untyped table version.
deletePrefixSQL' :: String -> StringSQL
deletePrefixSQL' table = DELETE <> FROM <> stringSQL table

-- | Generate all column delete SQL by specified table.
deletePrefixSQL :: Table r   -- ^ Table metadata
                -> StringSQL -- ^ Result SQL
deletePrefixSQL = deletePrefixSQL' . name
