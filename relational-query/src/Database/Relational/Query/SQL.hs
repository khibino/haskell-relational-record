{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.Relational.Query.SQL
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines functions to generate simple SQL strings.
module Database.Relational.Query.SQL (
  -- * Query suffix
  QuerySuffix, showsQuerySuffix,

  -- * Update SQL
  updatePrefixSQL,
  updateSQL',
  updateOtherThanKeySQL', updateOtherThanKeySQL,

  -- * Insert SQL
  insertPrefixSQL, insertSQL,

  -- * Delete SQL
  deletePrefixSQL', deletePrefixSQL
  ) where

import Data.Array (listArray, (!))
import Data.Monoid (mconcat, (<>))

import Language.SQL.Keyword (Keyword(..), (.=.), (|*|))
import qualified Language.SQL.Keyword as SQL
import Database.Record.ToSql (untypedUpdateValuesIndex)
import Database.Relational.Query.Internal.SQL (StringSQL, stringSQL, showStringSQL, rowStringSQL)
import Database.Relational.Query.Pi.Unsafe (Pi, unsafeExpandIndexes)
import Database.Relational.Query.Component (ColumnSQL, showsColumnSQL, showsColumnSQL)
import Database.Relational.Query.Table (Table, name, columns, width)


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
           -> [ColumnSQL] -- ^ Column name list to update
           -> [ColumnSQL] -- ^ Key column name list
           -> String      -- ^ Result SQL
updateSQL' table cols key =
  showStringSQL $ mconcat
  [UPDATE, stringSQL table, SET, SQL.fold (|*|) updAssigns,
   WHERE, SQL.fold SQL.and keyAssigns]
  where
    assigns cs = [ showsColumnSQL c .=. "?" | c <- cs ]
    updAssigns = assigns cols
    keyAssigns = assigns key

-- | Generate update SQL by specified key and table.
--   Columns name list of table are also required.
updateOtherThanKeySQL' :: String      -- ^ Table name
                       -> [ColumnSQL] -- ^ Column name list
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
  updateOtherThanKeySQL' (name tbl) (columns tbl) (unsafeExpandIndexes key)

-- | Generate prefix string of insert SQL.
insertPrefixSQL :: Table r -> StringSQL
insertPrefixSQL table =
  INSERT <> INTO <> stringSQL tn <> rowStringSQL [ showsColumnSQL c | c <- cols ]  where
    tn = name table
    cols = columns table

-- | Generate insert SQL.
insertSQL :: Table r -- ^ Table metadata
          -> String  -- ^ Result SQL
insertSQL tbl = showStringSQL $ insertPrefixSQL tbl <> VALUES <> rowStringSQL (replicate (width tbl) "?")

-- | Generate all column delete SQL by specified table. Untyped table version.
deletePrefixSQL' :: String -> StringSQL
deletePrefixSQL' table = DELETE <> FROM <> stringSQL table

-- | Generate all column delete SQL by specified table.
deletePrefixSQL :: Table r   -- ^ Table metadata
                -> StringSQL -- ^ Result SQL
deletePrefixSQL = deletePrefixSQL' . name
