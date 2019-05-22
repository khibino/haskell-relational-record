{-# LANGUAGE TemplateHaskell #-}

module Database.Relational.Schema.SQLite3 (
  module Database.Relational.Schema.SQLite3.Config,

  getType, normalizeColumn, normalizeType, notNull,
  tableInfoQuerySQL, indexListQuerySQL, indexInfoQuerySQL
  ) where

import qualified Data.Map as Map
import qualified Database.Relational.Schema.SQLite3.TableInfo as TableInfo

import Language.Haskell.TH (TypeQ)
import Control.Arrow (first)
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map (Map)
import Data.Time (Day, LocalTime)
import Database.Relational (Query, unsafeTypedQuery, attachEmptyPlaceholderOffsets)

import Database.Relational.Schema.SQLite3.Config
import Database.Relational.Schema.SQLite3.IndexInfo
import Database.Relational.Schema.SQLite3.IndexList
import Database.Relational.Schema.SQLite3.TableInfo

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

                 , ("DATE",       [t|Day|])
                 , ("DATETIME",   [t|LocalTime|])
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
tableInfoQuerySQL db tbl = unsafeTypedQuery $ attachEmptyPlaceholderOffsets $ "pragma " ++ db ++ ".table_info(" ++ tbl ++ ");"

indexListQuerySQL :: String -> String -> Query () IndexList
indexListQuerySQL db tbl = unsafeTypedQuery $ attachEmptyPlaceholderOffsets $ "pragma " ++ db ++ ".index_list(" ++ tbl ++ ");"

indexInfoQuerySQL :: String -> String -> Query () IndexInfo
indexInfoQuerySQL db idx = unsafeTypedQuery $ attachEmptyPlaceholderOffsets $ "pragma " ++ db ++ ".index_info(" ++ idx ++ ");"
