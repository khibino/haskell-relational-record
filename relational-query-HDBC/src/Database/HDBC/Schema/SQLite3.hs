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

import qualified Database.Relational.Schema.SQLite3Syscat.IndexInfo as IndexInfo
import qualified Database.Relational.Schema.SQLite3Syscat.IndexList as IndexList
import qualified Database.Relational.Schema.SQLite3Syscat.TableInfo as TableInfo

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.List (isPrefixOf, sort, sortBy)
import Data.Map (fromList)
import Database.HDBC (IConnection, SqlValue)
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Record.Persistable ()
import Database.HDBC.Schema.Driver
  (TypeMap, LogChan, putVerbose, failWith, maybeIO,
   Driver, hoistMaybe, getFieldsWithMap, getPrimaryKey, emptyDriver)
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

putLog :: LogChan -> String -> IO ()
putLog lchan = putVerbose lchan . logPrefix

compileError :: LogChan -> String -> MaybeT IO a
compileError lchan = failWith lchan . logPrefix

getPrimaryKey' :: IConnection conn
               => conn
               -> LogChan
               -> String
               -> String
               -> IO [String]
getPrimaryKey' conn lchan scm tbl = do
    tblinfo <- runQuery' conn (tableInfoQuerySQL scm tbl) ()
    let primColumns = map (normalizeColumn . TableInfo.name)
                      . filter ((1 ==) . TableInfo.pk) $ tblinfo
    if length primColumns <= 1 then do
        putLog lchan $ "getPrimaryKey: key=" ++ show primColumns
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
        putLog lchan $ "getPrimaryKey: keys=" ++ show primColumns'
        return primColumns'

getColumns' :: IConnection conn
            => TypeMap
            -> conn
            -> LogChan
            -> String
            -> String
            -> IO ([(String, TypeQ)], [Int])
getColumns' tmap conn lchan scm tbl = maybeIO ([], []) id $ do
    rows <- lift $ runQuery' conn (tableInfoQuerySQL scm tbl) ()
    guard (not $ null rows) <|>
      compileError lchan
      ("getFields: No columns found: schema = " ++ scm ++ ", table = " ++ tbl)
    let columnId = TableInfo.cid
    let notNullIdxs = map (fromIntegral . columnId) . filter notNull $ rows
    lift . putLog lchan
        $ "getFields: num of columns = " ++ show (length rows)
        ++ ", not null columns = " ++ show notNullIdxs
    let getType' ti =
          hoistMaybe (getType (fromList tmap) ti) <|>
          compileError lchan
          ("Type mapping is not defined against SQLite3 type: "
           ++ normalizeType (TableInfo.ctype ti))
    types <- mapM getType' rows
    return (types, notNullIdxs)

-- | Driver implementation
driverSQLite3 :: IConnection conn => Driver conn
driverSQLite3 =
    emptyDriver { getFieldsWithMap = getColumns' }
                { getPrimaryKey    = getPrimaryKey' }
