{-# LANGUAGE TemplateHaskell #-}

module Database.Relational.Schema.SQLite3 (
  getType, normalizeColumn, normalizeType, notNull,
  tableInfoQuerySQL, indexListQuerySQL, indexInfoQuerySQL
  ) where

import qualified Data.Map as Map
import qualified Database.Relational.Schema.SQLite3Syscat.TableInfo as TableInfo

import Control.Arrow (first)
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map (Map)
import Database.Record.Instances ()
import Database.Relational.Query (Query)
import Database.Relational.Query.Type (unsafeTypedQuery)
import Database.Relational.Schema.SQLite3Syscat.IndexInfo
import Database.Relational.Schema.SQLite3Syscat.IndexList
import Database.Relational.Schema.SQLite3Syscat.TableInfo
import Language.Haskell.TH (TypeQ)

--{-# ANN module "HLint: ignore Redundant $" #-}

-- <https://www.sqlite.org/datatype3.html>
-- SQLite3 is dynamic typing,
-- so assign narrower constraints in this default mapping.
-- Using upper case typenames along with SQLite3 document.
mapFromSqlDefault :: Map String TypeQ
mapFromSqlDefault =
    Map.fromList [ ("INT",        [t|Int32|])
                 , ("INTEGER",    [t|Int32|])
                 , ("TINYINT",    [t|Int8|])
                 , ("SMALLINT",   [t|Int16|])
                 , ("MEDIUMINT",  [t|Int32|])
                 , ("BIGINT",     [t|Int64|])
                 , ("INT2",       [t|Int16|])
                 , ("INT8",       [t|Int64|])

                 , ("CHARACTER",  [t|String|])
                 , ("VARCHAR",    [t|String|])
                 , ("TEXT",       [t|String|])

                 , ("BLOB",       [t|ByteString|])

                 , ("REAL",       [t|Double|])
                 , ("DOUBLE",     [t|Double|])
                 , ("FLOAT",      [t|Float|])
                 ]

normalizeColumn :: String -> String
normalizeColumn = map toLower

normalizeType :: String -> String
normalizeType = map toUpper . takeWhile (not . flip elem " (")

notNull :: TableInfo -> Bool
notNull info = isTrue . TableInfo.notnull $ info
  where
    isTrue 0 = False
    isTrue _ = True

-- for backward compatibility
normalizeMap :: Map String TypeQ -> Map String TypeQ
normalizeMap = Map.fromList . map (first $ map toUpper) . Map.toList

getType :: Map String TypeQ -> TableInfo -> Maybe (String, TypeQ)
getType mapFromSql info = do
    typ <- Map.lookup key (normalizeMap {- for backward compatibility -} mapFromSql)
           <|>
           Map.lookup key mapFromSqlDefault
    return (normalizeColumn (TableInfo.name info), mayNull typ)
  where
    key = normalizeType . TableInfo.ctype $ info
    mayNull typ = if notNull info
                    then typ
                    else [t|Maybe $(typ)|]

tableInfoQuerySQL :: String -> String -> Query () TableInfo
tableInfoQuerySQL db tbl = unsafeTypedQuery $ "pragma " ++ db ++ ".table_info(" ++ tbl ++ ");"

indexListQuerySQL :: String -> String -> Query () IndexList
indexListQuerySQL db tbl = unsafeTypedQuery $ "pragma " ++ db ++ ".index_list(" ++ tbl ++ ");"

indexInfoQuerySQL :: String -> String -> Query () IndexInfo
indexInfoQuerySQL db idx = unsafeTypedQuery $ "pragma " ++ db ++ ".index_info(" ++ idx ++ ");"
