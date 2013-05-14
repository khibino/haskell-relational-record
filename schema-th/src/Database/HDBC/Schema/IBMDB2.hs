{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Schema.IBMDB2
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.Schema.IBMDB2 (
  Columns, driverIBMDB2
  ) where

import Prelude hiding (length)

import qualified Data.List as List
import Data.Int (Int16, Int32, Int64)
import Data.Char (toUpper, toLower)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Time (LocalTime, Day)
import Language.Haskell.TH (TypeQ)
import qualified Language.Haskell.TH.Name.CamelCase as TH
import qualified Language.Haskell.TH.Name.Extra as TH

import Database.HDBC (IConnection, SqlValue)

import Database.HDBC.Record.Query (runQuery', listToUnique)
import Database.HDBC.Record.Persistable ()

import Database.Record.TH (derivingShow)
import Database.Record.TH (defineRecordWithSqlTypeDefaultFromDefined)

import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Type (unsafeTypedQuery, fromRelation)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)
import Database.Relational.Query
  (Query, PrimeRelation, inner, relation,
   wheres, (.=.), (!), placeholder, asc)

import Language.SQL.Keyword (Keyword(..))
import qualified Language.SQL.Keyword as SQL

import Database.HDBC.Schema.Driver
  (TypeMap, Driver, getFieldsWithMap, getPrimaryKey, emptyDriver)
  
import Database.Relational.Schema.DB2Syscat.Columns

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

normalizeField :: String -> String
normalizeField =  map toLower

notNull :: Columns -> Bool
notNull =  (== "N") . nulls

getType :: Map String TypeQ -> Columns -> (String, TypeQ)
getType mapFromSql rec =
  (normalizeField $ colname rec,
   mayNull $ mapFromSql Map.! typename rec)
  where mayNull typ = if notNull rec
                      then typ
                      else [t| Maybe $(typ) |]

columnsRelationFromTable :: PrimeRelation (String, String) Columns
columnsRelationFromTable =  relation $ do
  c <- inner columns
  wheres $ c ! tabschema' .=. placeholder
  wheres $ c ! tabname'   .=. placeholder
  asc $ c ! colno'
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


$(defineRecordWithSqlTypeDefaultFromDefined
  [t| SqlValue |] (Table.shortName tableOfColumns))

logPrefix :: String -> String
logPrefix =  ("IBMDB2: " ++)

putLog :: String -> IO ()
putLog =  putStrLn . logPrefix

compileErrorIO :: String -> IO a
compileErrorIO =  TH.compileErrorIO . logPrefix

getPrimaryKey' :: IConnection conn
              => conn
              -> String
              -> String
              -> IO (Maybe String)
getPrimaryKey' conn scm' tbl' = do
  let tbl = map toUpper tbl'
      scm = map toUpper scm'
  mayPrim <- runQuery' conn (scm, tbl) primaryKeyQuerySQL
             >>= listToUnique
  let mayPrimaryKey = normalizeField `fmap` mayPrim
  putLog $ "getPrimaryKey: primary key = " ++ show mayPrimaryKey

  return mayPrimaryKey

getFields' :: IConnection conn
          => TypeMap
          -> conn
          -> String
          -> String
          -> IO ([(String, TypeQ)], [Int])
getFields' tmap conn scm' tbl' = do
  let tbl = map toUpper tbl'
      scm = map toUpper scm'
      
  cols <- runQuery' conn (scm, tbl) columnsQuerySQL
  case cols of
    [] ->  compileErrorIO
           $ "getFields: No columns found: schema = " ++ scm ++ ", table = " ++ tbl
    _  ->  return ()

  let notNullIdxs = map fst . filter (notNull . snd) . zip [0..] $ cols
  putLog
    $  "getFields: num of columns = " ++ show (List.length cols)
    ++ ", not null columns = " ++ show notNullIdxs
  let mapFromSql = fromList tmap `Map.union` mapFromSqlDefault

  return $ (map (getType mapFromSql) cols, notNullIdxs)

driverIBMDB2 :: IConnection conn => Driver conn
driverIBMDB2 =
  emptyDriver { getFieldsWithMap = getFields' }
              { getPrimaryKey    = getPrimaryKey' }
