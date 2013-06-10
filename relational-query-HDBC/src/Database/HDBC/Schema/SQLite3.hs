{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Schema.SQLite3
-- Copyright   : 2013 Shohei Murayama
-- License     : BSD3
--
-- Maintainer  : shohei.murayama@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.Schema.SQLite3 (
  driverSQLite3
  ) where

import qualified Database.Relational.Query.Table as Table
import qualified Language.Haskell.TH.Lib.Extra as TH
import qualified Database.Relational.Schema.SQLite3Syscat.TableInfo as TableInfo

import Data.Map (fromList)
import Database.HDBC (IConnection, SqlValue)
import Database.HDBC.Record.Query (runQuery', listToUnique)
import Database.HDBC.Record.Persistable ()
import Database.HDBC.Schema.Driver (TypeMap, Driver, getFieldsWithMap, getPrimaryKey, emptyDriver)
import Database.Record.TH (defineRecordWithSqlTypeDefaultFromDefined)
import Database.Relational.Schema.SQLite3 (getType, normalizeColumn, normalizeType,
                                           notNull, tableInfoQuerySQL)
import Database.Relational.Schema.SQLite3Syscat.TableInfo (TableInfo(TableInfo))
import Language.Haskell.TH (TypeQ)

$(defineRecordWithSqlTypeDefaultFromDefined
  [t| SqlValue |] (Table.shortName TableInfo.tableOfTableInfo))

logPrefix :: String -> String
logPrefix = ("SQLite3: " ++)

putLog :: String -> IO ()
putLog = putStrLn . logPrefix

compileErrorIO :: String -> IO a
compileErrorIO = TH.compileErrorIO . logPrefix

getPrimaryKey' :: IConnection conn
               => conn
               -> String
               -> String
               -> IO (Maybe String)
getPrimaryKey' conn scm tbl = do
    ti <- runQuery' conn () (tableInfoQuerySQL scm tbl)
    mayPrim <- listToUnique . map  TableInfo.name . filter ((1 ==) . TableInfo.pk) $ ti
    return $ normalizeColumn `fmap` mayPrim 

getFields' :: IConnection conn
           => TypeMap
           -> conn
           -> String
           -> String
           -> IO ([(String, TypeQ)], [Int])
getFields' tmap conn scm tbl = do
    rows <- runQuery' conn () (tableInfoQuerySQL scm tbl)
    case rows of
      [] -> compileErrorIO
            $ "getFields: No columns found: schema = " ++ scm ++ ", table = " ++ tbl
      _  -> return ()
    let columnId = TableInfo.cid
    let notNullIdxs = map (fromIntegral . columnId) . filter notNull $ rows
    putLog
        $ "getFields: num of columns = " ++ show (length rows)
        ++ ", not null columns = " ++ show notNullIdxs
    let getType' ti = case getType (fromList tmap) ti of
          Nothing -> compileErrorIO
                     $ "Type mapping is not defined against SQLite3 type: "
                     ++ normalizeType (TableInfo.ctype ti)
          Just p  -> return p
    types <- mapM getType' rows
    return (types, notNullIdxs)

driverSQLite3 :: IConnection conn => Driver conn
driverSQLite3 =
    emptyDriver { getFieldsWithMap = getFields' }
                { getPrimaryKey    = getPrimaryKey' }
