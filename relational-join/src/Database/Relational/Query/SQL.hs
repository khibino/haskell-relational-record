{-# LANGUAGE OverloadedStrings #-}

module Database.Relational.Query.SQL (
  singleKeyUpdateSQL', singleKeyUpdateSQL,

  insertSQL', insertSQL
  ) where

import Language.SQL.Keyword (Keyword(..), (.=.))
import qualified Language.SQL.Keyword as SQL
import Database.Relational.Query.Table (Table, name, columns)


singleKeyUpdateSQL' :: String -> [String] -> String -> String
singleKeyUpdateSQL' table cols key =
  SQL.unwordsSQL
  $ [UPDATE, SQL.word table, SET, assignments `SQL.sepBy` ", ",
     WHERE, SQL.word key, "= ?"]
  where assignments = map (\f -> SQL.word f .=. "?") . filter (/= key) $ cols

singleKeyUpdateSQL :: Table r -> String -> String
singleKeyUpdateSQL tbl = singleKeyUpdateSQL' (name tbl) (columns tbl)


insertSQL' :: String -> [String] -> String
insertSQL' table cols =
  SQL.unwordsSQL
  $ [INSERT, INTO, SQL.word table, cols' `SQL.parenSepBy` ", ",
     VALUES, pfs `SQL.parenSepBy` ", "]
  where cols' = map SQL.word cols
        pfs      = replicate (length cols) "?"

insertSQL :: Table r -> String
insertSQL tbl = insertSQL' (name tbl) (columns tbl)
