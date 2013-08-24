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
  -- * Select SQL
  selectSeedSQL,

  -- * Update SQL
  updateSeedSQL,
  updateSQL',
  updateOtherThanKeySQL', updateOtherThanKeySQL,

  -- * Insert SQL
  insertSQL', insertSQL,

  -- * Delete SQL
  deleteSQL', deleteSQL
  ) where

import Data.Array (listArray, (!))

import Language.SQL.Keyword (Keyword(..), (.=.), unwordsSQL)
import qualified Language.SQL.Keyword as SQL
import Database.Record.ToSql (untypedUpdateValuesIndex)
import Database.Relational.Query.Pi.Unsafe (Pi, unsafeExpandIndexes)
import Database.Relational.Query.Sub (asColumnN)
import Database.Relational.Query.Table (Table, name, columns)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection


-- | Generate select SQL. Seed SQL string append to this.
selectSeedSQL :: Projection r -> ShowS
selectSeedSQL pj =
  (unwordsSQL [SELECT, columns' `SQL.sepBy` ", "] ++)
  where columns' = zipWith
                   (\f n -> SQL.word f `asColumnN` n)
                   (Projection.columns pj)
                   [(0 :: Int)..]

-- | Generate update SQL. Seed SQL string append to this.
updateSeedSQL :: Table r -> ShowS
updateSeedSQL table = (unwordsSQL [UPDATE, SQL.word $ name table] ++)

-- | Generate update SQL by specified key and table.
--   Columns name list of table are also required.
updateSQL' :: String   -- ^ Table name
           -> [String] -- ^ Column name list to update
           -> [String] -- ^ Key column name list
           -> String   -- ^ Result SQL
updateSQL' table cols key =
  SQL.unwordsSQL
  $ [UPDATE, SQL.word table, SET, updAssigns `SQL.sepBy` ", ",
     WHERE, keyAssigns `SQL.sepBy` " AND " ]
  where
    assigns cs = [ SQL.word c .=. "?" | c <- cs ]
    updAssigns = assigns cols
    keyAssigns = assigns key

-- | Generate update SQL by specified key and table.
--   Columns name list of table are also required.
updateOtherThanKeySQL' :: String   -- ^ Table name
           -> [String] -- ^ Column name list
           -> [Int]    -- ^ Key column indexes
           -> String   -- ^ Result SQL
updateOtherThanKeySQL' table cols ixs =
  updateSQL' table updColumns keyColumns
  where
    width = length cols
    cols' = listArray (0, width -1) cols
    otherThanKey = untypedUpdateValuesIndex ixs width
    columns' is = [ cols' ! i | i <- is ]
    updColumns = columns' otherThanKey
    keyColumns = columns' ixs

-- | Generate update SQL specified by single key.
updateOtherThanKeySQL :: Table r -- ^ Table metadata
          -> Pi r p  -- ^ Key columns
          -> String  -- ^ Result SQL
updateOtherThanKeySQL tbl key =
  updateOtherThanKeySQL' (name tbl) (columns tbl) (unsafeExpandIndexes key)

-- | Generate insert SQL.
insertSQL' :: String   -- ^ Table name
           -> [String] -- ^ Column name list
           -> String   -- ^ Result SQL
insertSQL' table cols =
  SQL.unwordsSQL
  $ [INSERT, INTO, SQL.word table, cols' `SQL.parenSepBy` ", ",
     VALUES, pfs `SQL.parenSepBy` ", "]
  where cols' = map SQL.word cols
        pfs      = replicate (length cols) "?"

-- | Generate insert SQL.
insertSQL :: Table r -- ^ Table metadata
          -> String  -- ^ Result SQL
insertSQL tbl = insertSQL' (name tbl) (columns tbl)

-- | Generate all column delete SQL by specified table. Untyped table version.
deleteSQL' :: String -> ShowS
deleteSQL' table = (SQL.unwordsSQL [DELETE, FROM, SQL.word table] ++)

-- | Generate all column delete SQL by specified table.
deleteSQL :: Table r -- ^ Table metadata
          -> ShowS   -- ^ Result SQL
deleteSQL = deleteSQL' . name
