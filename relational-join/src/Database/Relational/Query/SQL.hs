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
  -- * Update SQL
  singleKeyUpdateSQL', singleKeyUpdateSQL,

  -- * Insert SQL
  insertSQL', insertSQL
  ) where

import Language.SQL.Keyword (Keyword(..), (.=.))
import qualified Language.SQL.Keyword as SQL
import Database.Relational.Query.Table (Table, name, columns)


-- | Generate update SQL specified by single key.
singleKeyUpdateSQL' :: String   -- ^ Table name
                    -> [String] -- ^ Column name list
                    -> String   -- ^ Key column name
                    -> String   -- ^ Result SQL
singleKeyUpdateSQL' table cols key =
  SQL.unwordsSQL
  $ [UPDATE, SQL.word table, SET, assignments `SQL.sepBy` ", ",
     WHERE, SQL.word key, "= ?"]
  where assignments = map (\f -> SQL.word f .=. "?") . filter (/= key) $ cols

-- | Generate update SQL specified by single key.
singleKeyUpdateSQL :: Table r -- ^ Table metadata
                   -> String  -- ^ Key column name
                   -> String  -- ^ Result SQL
singleKeyUpdateSQL tbl = singleKeyUpdateSQL' (name tbl) (columns tbl)


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
