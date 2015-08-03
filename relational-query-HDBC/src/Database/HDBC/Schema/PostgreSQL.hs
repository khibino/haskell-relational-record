{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Schema.PostgreSQL
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides driver implementation
-- to load PostgreSQL system catalog via HDBC.
module Database.HDBC.Schema.PostgreSQL (
  driverPostgreSQL
  ) where

import Language.Haskell.TH (TypeQ)

import Data.Char (toLower)
import Data.Map (fromList)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)

import Database.HDBC (IConnection, SqlValue)

import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Record.Persistable ()

import Database.Record.TH (makeRecordPersistableWithSqlTypeDefaultFromDefined)

import Database.Relational.Schema.PostgreSQL
  (normalizeColumn, notNull, getType, columnQuerySQL,
   primaryKeyLengthQuerySQL, primaryKeyQuerySQL)
import Database.Relational.Schema.PgCatalog.PgAttribute (PgAttribute)
import Database.Relational.Schema.PgCatalog.PgType (PgType)
import qualified Database.Relational.Schema.PgCatalog.PgType as Type

import Database.HDBC.Schema.Driver
  (TypeMap, LogChan, putVerbose, maybeIO,
   Driver, getFieldsWithMap, getPrimaryKey, emptyDriver)


$(makeRecordPersistableWithSqlTypeDefaultFromDefined
  [t| SqlValue |] ''PgAttribute)

$(makeRecordPersistableWithSqlTypeDefaultFromDefined
  [t| SqlValue |] ''PgType)

logPrefix :: String -> String
logPrefix =  ("PostgreSQL: " ++)

putLog :: LogChan -> String -> IO ()
putLog lchan = putVerbose lchan . logPrefix

compileErrorIO :: String -> IO a
compileErrorIO =  fail . logPrefix

getPrimaryKey' :: IConnection conn
              => conn
              -> LogChan
              -> String
              -> String
              -> IO [String]
getPrimaryKey' conn lchan scm' tbl' = do
  let scm = map toLower scm'
      tbl = map toLower tbl'
  mayKeyLen <- runQuery' conn primaryKeyLengthQuerySQL (scm, tbl)
  case mayKeyLen of
    []        -> do
      putLog lchan   "getPrimaryKey: Primary key not found."
      return []
    [keyLen]  -> do
      primCols <- runQuery' conn (primaryKeyQuerySQL keyLen) (scm, tbl)
      let primaryKeyCols = normalizeColumn `fmap` primCols
      putLog lchan $ "getPrimaryKey: primary key = " ++ show primaryKeyCols
      return primaryKeyCols
    _:_:_     -> do
      putLog lchan   "getPrimaryKey: Fail to detect primary key. Something wrong."
      return []

getColumns' :: IConnection conn
          => TypeMap
          -> conn
          -> LogChan
          -> String
          -> String
          -> IO ([(String, TypeQ)], [Int])
getColumns' tmap conn lchan scm' tbl' = maybeIO ([], []) id $ do
  let scm = map toLower scm'
      tbl = map toLower tbl'
  cols <- lift $ runQuery' conn columnQuerySQL (scm, tbl)
  lift . when (null cols) . compileErrorIO
    $ "getFields: No columns found: schema = " ++ scm ++ ", table = " ++ tbl

  let notNullIdxs = map fst . filter (notNull . snd) . zip [0..] $ cols
  lift . putLog lchan
    $  "getFields: num of columns = " ++ show (length cols)
    ++ ", not null columns = " ++ show notNullIdxs
  let getType' col = case getType (fromList tmap) col of
        Nothing -> compileErrorIO
                   $ "Type mapping is not defined against PostgreSQL type: " ++ Type.typname (snd col)
        Just p  -> return p

  types <- lift $ mapM getType' cols
  return (types, notNullIdxs)

-- | Driver implementation
driverPostgreSQL :: IConnection conn => Driver conn
driverPostgreSQL =
  emptyDriver { getFieldsWithMap = getColumns' }
              { getPrimaryKey    = getPrimaryKey' }
