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

import qualified Language.Haskell.TH.Lib.Extra as TH
import qualified Database.Relational.Schema.SQLite3Syscat.IndexInfo as IndexInfo
import qualified Database.Relational.Schema.SQLite3Syscat.IndexList as IndexList
import qualified Database.Relational.Schema.SQLite3Syscat.TableInfo as TableInfo

import Data.List (isPrefixOf, sort, sortBy)
import Data.Map (fromList)
import Database.HDBC (IConnection, SqlValue)
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Record.Persistable ()
import Database.HDBC.Schema.Driver (TypeMap, Driver, getFieldsWithMap, getPrimaryKey, emptyDriver)
import Database.Record.TH (makeRecordPersistableWithSqlTypeDefaultFromDefined)
import Database.Relational.Schema.SQLite3 (getType, indexInfoQuerySQL, indexListQuerySQL, normalizeColumn,
                                           normalizeType, notNull, tableInfoQuerySQL)
import Database.Relational.Schema.SQLite3Syscat.IndexInfo (IndexInfo)
import Database.Relational.Schema.SQLite3Syscat.IndexList (IndexList)
import Database.Relational.Schema.SQLite3Syscat.TableInfo (TableInfo)
import Language.Haskell.TH (TypeQ)

$(makeRecordPersistableWithSqlTypeDefaultFromDefined
  [t| SqlValue |] ''TableInfo)

$(makeRecordPersistableWithSqlTypeDefaultFromDefined
  [t| SqlValue |] ''IndexList)

$(makeRecordPersistableWithSqlTypeDefaultFromDefined
  [t| SqlValue |] ''IndexInfo)

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
               -> IO [String]
getPrimaryKey' conn scm tbl = do
    tblinfo <- runQuery' conn (tableInfoQuerySQL scm tbl) ()
    let primColumns = map (normalizeColumn . TableInfo.name)
                      . filter ((1 ==) . TableInfo.pk) $ tblinfo
    if length primColumns <= 1 then do
        putLog $ "getPrimaryKey: key=" ++ show primColumns
        return primColumns
     else do
        idxlist <- runQuery' conn (indexListQuerySQL scm tbl) ()
        let idxNames = filter (isPrefixOf "sqlite_autoindex_")
                       . map IndexList.name
                       . filter ((1 ==) . IndexList.unique) $ idxlist
        idxInfos <- mapM (\ixn -> runQuery' conn (indexInfoQuerySQL scm ixn) ()) idxNames
        let isPrimaryKey = (sort primColumns ==) . sort . map (normalizeColumn . IndexInfo.name)
        let idxInfo = concat . take 1 . filter isPrimaryKey $ idxInfos
        let comp x y = compare (IndexInfo.seqno x) (IndexInfo.seqno y)
        let primColumns' = map IndexInfo.name . sortBy comp $ idxInfo
        putLog $ "getPrimaryKey: keys=" ++ show primColumns'
        return primColumns'

getFields' :: IConnection conn
           => TypeMap
           -> conn
           -> String
           -> String
           -> IO ([(String, TypeQ)], [Int])
getFields' tmap conn scm tbl = do
    rows <- runQuery' conn (tableInfoQuerySQL scm tbl) ()
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
