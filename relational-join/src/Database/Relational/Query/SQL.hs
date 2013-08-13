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
  updateSQL', updateSQL,

  -- * Insert SQL
  insertSQL', insertSQL
  ) where

import Data.Array (listArray, (!))

import Language.SQL.Keyword (Keyword(..), (.=.), unwordsSQL)
import qualified Language.SQL.Keyword as SQL
import Database.Record.ToSql (untypedUpdateValuesIndex)
import Database.Relational.Query.Sub (asColumnN)
import Database.Relational.Query.Table (Table, name, columns)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection


-- | Generate select SQL. Append seed query string.
selectSeedSQL :: Projection r -> String
selectSeedSQL pj =
  unwordsSQL
  $ [SELECT, columns' `SQL.sepBy` ", "]
  where columns' = zipWith
                   (\f n -> SQL.word f `asColumnN` n)
                   (Projection.columns pj)
                   [(0 :: Int)..]

-- | Generate update SQL by specified key and table.
updateSQL' :: String   -- ^ Table name
           -> [String] -- ^ Column name list
           -> [Int]    -- ^ Key column indexes
           -> String   -- ^ Result SQL
updateSQL' table cols ixs =
  SQL.unwordsSQL
  $ [UPDATE, SQL.word table, SET, updAssigns `SQL.sepBy` ", ",
     WHERE, keyAssigns `SQL.sepBy` " AND " ]
  where
    width = length cols
    cols' = listArray (0, width -1) cols
    otherThanKey = untypedUpdateValuesIndex ixs width
    assigns is = [ SQL.word (cols' ! i) .=. "?" | i <- is ]
    updAssigns = assigns otherThanKey
    keyAssigns = assigns ixs

-- | Generate update SQL specified by single key.
updateSQL :: Table r -- ^ Table metadata
          -> [Int]   -- ^ Key column indexes
          -> String  -- ^ Result SQL
updateSQL tbl = updateSQL' (name tbl) (columns tbl)


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
