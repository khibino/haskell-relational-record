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

import qualified Database.Relational.Schema.SQLServerSyscat.Columns as Columns
import qualified Database.Relational.Schema.SQLServerSyscat.Types as Types

import Control.Applicative ((<$>), (<|>))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Map (fromList)
import Data.Maybe (catMaybes)
import Database.HDBC (IConnection, SqlValue)
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Record.Persistable ()
import Database.HDBC.Schema.Driver
  (TypeMap, LogChan, putVerbose, failWith, maybeIO,
   Driver, hoistMaybe, getFieldsWithMap, getPrimaryKey, emptyDriver)
import Database.Record (FromSql, ToSql)
import Database.Relational.Schema.SQLServer (columnTypeQuerySQL, getType, normalizeColumn,
                                            notNull, primaryKeyQuerySQL)
import Database.Relational.Schema.SQLServerSyscat.Columns (Columns)
import Database.Relational.Schema.SQLServerSyscat.Types (Types)
import Language.Haskell.TH (TypeQ)


instance FromSql SqlValue Columns
instance ToSql SqlValue Columns

instance FromSql SqlValue Types
instance ToSql SqlValue Types

logPrefix :: String -> String
logPrefix = ("SQLServer: " ++)

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
    prims <- catMaybes <$> runQuery' conn primaryKeyQuerySQL (scm,tbl)
    let primColumns = map normalizeColumn prims
    putLog lchan $ "getPrimaryKey: keys=" ++ show primColumns
    return primColumns

getColumns' :: IConnection conn
            => TypeMap
            -> conn
            -> LogChan
            -> String
            -> String
            -> IO ([(String, TypeQ)], [Int])
getColumns' tmap conn lchan scm tbl = maybeIO ([], []) id $ do
    rows <- lift $ runQuery' conn columnTypeQuerySQL (scm, tbl)
    guard (not $ null rows) <|>
      compileError lchan
      ("getFields: No columns found: schema = " ++ scm ++ ", table = " ++ tbl)
    let columnId ((cols,_),_) = Columns.columnId cols - 1
    let notNullIdxs = map (fromIntegral . columnId) . filter notNull $ rows
    lift . putLog lchan
        $ "getFields: num of columns = " ++ show (length rows)
        ++ ", not null columns = " ++ show notNullIdxs
    let getType' rec'@((_,typs),typScms) =
          hoistMaybe (getType (fromList tmap) rec') <|>
          compileError lchan
          ("Type mapping is not defined against SQLServer type: "
           ++ typScms ++ "." ++ Types.name typs)
    types <- mapM getType' rows
    return (types, notNullIdxs)

-- | Driver implementation
driverSQLServer :: IConnection conn => Driver conn
driverSQLServer =
    emptyDriver { getFieldsWithMap = getColumns' }
                { getPrimaryKey    = getPrimaryKey' }
