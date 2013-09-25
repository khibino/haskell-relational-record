{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Schema.SQLServer
-- Copyright   : 2013 Shohei Murayama
-- License     : BSD3
--
-- Maintainer  : shohei.murayama@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.Schema.SQLServer (
  driverSQLServer,
  ) where

import qualified Database.Relational.Query.Table as Table
import qualified Database.Relational.Schema.SQLServerSyscat.Columns as Columns
import qualified Database.Relational.Schema.SQLServerSyscat.Types as Types
import qualified Language.Haskell.TH.Lib.Extra as TH

import Data.Map (fromList)
import Data.Maybe (catMaybes)
import Database.HDBC (IConnection, SqlValue)
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Record.Persistable ()
import Database.HDBC.Schema.Driver (TypeMap, Driver, getFieldsWithMap, getPrimaryKey, emptyDriver)
import Database.Record.TH (defineRecordWithSqlTypeDefaultFromDefined)
import Database.Relational.Schema.SQLServer (columnTypeQuerySQL, getType, normalizeColumn,
                                            notNull, primaryKeyQuerySQL)
import Database.Relational.Schema.SQLServerSyscat.Columns (Columns(Columns))
import Database.Relational.Schema.SQLServerSyscat.Types (Types(Types))
import Language.Haskell.TH (TypeQ)

$(defineRecordWithSqlTypeDefaultFromDefined
  [t| SqlValue |] (Table.shortName Columns.tableOfColumns))

$(defineRecordWithSqlTypeDefaultFromDefined
  [t| SqlValue |] (Table.shortName Types.tableOfTypes))

logPrefix :: String -> String
logPrefix = ("SQLServer: " ++)

putLog :: String -> IO ()
putLog = putStrLn . logPrefix

compileErrorIO :: String -> IO a
compileErrorIO = TH.compileErrorIO . logPrefix

getPrimaryKey' :: IConnection conn
               => conn
               -> String
               -> String
               -> IO [String]
getPrimaryKey' conn scm tbl = do
    prims <- catMaybes `fmap` runQuery' conn primaryKeyQuerySQL (scm,tbl)
    let primColumns = map normalizeColumn prims
    putLog $ "getPrimaryKey: keys=" ++ show primColumns
    return primColumns

getFields' :: IConnection conn
           => TypeMap
           -> conn
           -> String
           -> String
           -> IO ([(String, TypeQ)], [Int])
getFields' tmap conn scm tbl = do
    rows <- runQuery' conn columnTypeQuerySQL (scm, tbl)
    case rows of
      [] -> compileErrorIO
            $ "getFields: No columns found: schema = " ++ scm ++ ", table = " ++ tbl
      _  -> return ()
    let columnId ((cols,_),_) = Columns.columnId cols - 1
    let notNullIdxs = map (fromIntegral . columnId) . filter notNull $ rows
    putLog
        $ "getFields: num of columns = " ++ show (length rows)
        ++ ", not null columns = " ++ show notNullIdxs
    let getType' rec@((_,typs),typScms) = case getType (fromList tmap) rec of
          Nothing -> compileErrorIO
                     $ "Type mapping is not defined against SQLServer type: "
                     ++ typScms ++ "." ++ Types.name typs
          Just p  -> return p
    types <- mapM getType' rows
    return (types, notNullIdxs)

driverSQLServer :: IConnection conn => Driver conn
driverSQLServer =
    emptyDriver { getFieldsWithMap = getFields' }
                { getPrimaryKey    = getPrimaryKey' }
