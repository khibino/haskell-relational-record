{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Schema.Oracle
-- Copyright   : 2013 Shohei Yasutake, 2017-2019 Kei Hibiono
-- License     : BSD3
--
-- Maintainer  : amutake.s@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.Schema.Oracle
    ( driverOracle
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Char (toUpper)
import Data.Map (fromList)
import Data.Maybe (catMaybes)
import Language.Haskell.TH (TypeQ)

import Database.HDBC (IConnection, SqlValue)
import Database.Record (FromSql, ToSql)

import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Record.Persistable ()
import Database.HDBC.Schema.Driver
    ( TypeMap, LogChan, putVerbose, failWith, maybeIO, hoistMaybe,
      Driver, driverConfig, getFieldsWithMap, getPrimaryKey, emptyDriver
    )

import Database.Relational.Schema.Oracle
    ( normalizeColumn, notNull, getType
    , columnsQuerySQL, primaryKeyQuerySQL
    )
import Database.Relational.Schema.Oracle.TabColumns (DbaTabColumns)
import qualified Database.Relational.Schema.Oracle.TabColumns as Cols
import Database.Relational.Schema.Oracle (config)


instance FromSql SqlValue DbaTabColumns
instance ToSql SqlValue DbaTabColumns

logPrefix :: String -> String
logPrefix = ("Oracle: " ++)

putLog :: LogChan -> String -> IO ()
putLog lchan = putVerbose lchan . logPrefix

compileError :: LogChan -> String -> MaybeT IO a
compileError lchan = failWith lchan . logPrefix

getPrimaryKey' :: IConnection conn
               => conn
               -> LogChan
               -> String -- ^ owner name
               -> String -- ^ table name
               -> IO [String] -- ^ primary key names
getPrimaryKey' conn lchan owner' tbl' = do
    let owner = map toUpper owner'
        tbl = map toUpper tbl'
    prims <- map normalizeColumn . catMaybes <$>
        runQuery' conn primaryKeyQuerySQL (owner, tbl)
    putLog lchan $ "getPrimaryKey: keys = " ++ show prims
    return prims

getColumns' :: IConnection conn
            => TypeMap
            -> conn
            -> LogChan
            -> String
            -> String
            -> IO ([(String, TypeQ)], [Int])
getColumns' tmap conn lchan owner' tbl' = maybeIO ([], []) id $ do
    let owner = map toUpper owner'
        tbl = map toUpper tbl'
    cols <- lift $ runQuery' conn columnsQuerySQL (owner, tbl)
    guard (not $ null cols) <|>
        compileError lchan
        ("getFields: No columns found: owner = " ++ owner ++ ", table = " ++ tbl)
    let notNullIdxs = map fst . filter (notNull . snd) . zip [0..] $ cols
    lift . putLog lchan $
        "getFields: num of columns = " ++ show (length cols) ++
        ", not null columns = " ++ show notNullIdxs
    let getType' col =
          hoistMaybe (getType (fromList tmap) col) <|>
          compileError lchan
          ("Type mapping is not defined against Oracle DB type: " ++ show (Cols.dataType col))
    types <- mapM getType' cols
    return (types, notNullIdxs)

-- | Driver for Oracle DB
driverOracle :: IConnection conn => Driver conn
driverOracle =
    emptyDriver { getFieldsWithMap = getColumns' }
                { getPrimaryKey = getPrimaryKey' }
                { driverConfig  = config }
