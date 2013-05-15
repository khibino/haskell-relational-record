{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Relational.Schema.IBMDB2 (
  normalizeColumn, notNull, getType,

  columnsQuerySQL, primaryKeyQuerySQL
  ) where


import Data.Int (Int16, Int32, Int64)
import Data.Char (toLower)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Time (LocalTime, Day)
import Language.Haskell.TH (TypeQ)

import Database.Record.Instances ()

import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Type (unsafeTypedQuery, fromRelation)
import Database.Relational.Query
  (Query, PrimeRelation, inner, relation,
   wheres, (.=.), (!), placeholder, asc)

import Language.SQL.Keyword (Keyword(..))
import qualified Language.SQL.Keyword as SQL

import Control.Applicative ((<|>))

import Database.Relational.Schema.DB2Syscat.Columns (Columns, tableOfColumns, columns)
import qualified Database.Relational.Schema.DB2Syscat.Columns as Columns


mapFromSqlDefault :: Map String TypeQ
mapFromSqlDefault =
  fromList [("VARCHAR",   [t|String|]),
            ("CHAR",      [t|String|]),
            ("CHARACTER", [t|String|]),
            ("TIMESTAMP", [t|LocalTime|]),
            ("DATE",      [t|Day|]),
            ("SMALLINT",  [t|Int16|]),
            ("INTEGER",   [t|Int32|]),
            ("BIGINT",    [t|Int64|]),
            ("BLOB",      [t|String|]),
            ("CLOB",      [t|String|])]

normalizeColumn :: String -> String
normalizeColumn =  map toLower

notNull :: Columns -> Bool
notNull =  (== "N") . Columns.nulls

getType :: Map String TypeQ -> Columns -> Maybe (String, TypeQ)
getType mapFromSql rec = do
  typ <- (Map.lookup key mapFromSql
          <|>
          Map.lookup key mapFromSqlDefault)
  return (normalizeColumn $ Columns.colname rec, mayNull typ)
  where key = Columns.typename rec
        mayNull typ = if notNull rec
                      then typ
                      else [t| Maybe $(typ) |]

columnsRelationFromTable :: PrimeRelation (String, String) Columns
columnsRelationFromTable =  relation $ do
  c <- inner columns
  wheres $ c ! Columns.tabschema' .=. placeholder
  wheres $ c ! Columns.tabname'   .=. placeholder
  asc $ c ! Columns.colno'
  return c

columnsQuerySQL :: Query (String, String) Columns
columnsQuerySQL =  fromRelation columnsRelationFromTable


primaryKeyQuerySQL :: Query (String, String) String
primaryKeyQuerySQL =
  unsafeTypedQuery .
  SQL.unwordsSQL
  $ [SELECT, "key.colname",
     FROM,
     "SYSCAT.tabconst", AS, "const", ",",
     "SYSCAT.keycoluse", AS, "key", ",",
     SQL.word (Table.name tableOfColumns), AS, "col",
     WHERE,
     "const.tabschema = col.tabschema", AND,
     "const.tabname = col.tabname", AND,
     "key.colname = col.colname", AND,
     "const.constname = key.constname", AND,

     "col.nulls = 'N'", AND,
     "const.type = 'P'", AND, "const.enforced = 'Y'", AND,

     "const.tabschema = ?", AND, "const.tabname = ?"]
