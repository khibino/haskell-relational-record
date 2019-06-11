{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Schema.HDBC.PostgreSQL
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides driver implementation
-- to load PostgreSQL system catalog via HDBC.
module Database.Schema.HDBC.PostgreSQL (
  driverPostgreSQL
  ) where

import Language.Haskell.TH (TypeQ)

import Data.Char (toLower)
import Data.Map (fromList)
import Control.Applicative ((<$>), (<|>))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)

import Database.HDBC (IConnection, SqlValue)

import Database.Relational.HDBC.Query (runQuery')
import Database.Relational.HDBC.Persistable ()

import Database.Record (FromSql, ToSql)

import Database.Relational.Schema.PostgreSQL
  (normalizeColumn, notNull, getType, columnQuerySQL,
   primaryKeyLengthQuerySQL, primaryKeyQuerySQL)
import Database.Relational.Schema.PostgreSQL.PgAttribute (PgAttribute)
import Database.Relational.Schema.PostgreSQL.PgType (PgType)
import qualified Database.Relational.Schema.PostgreSQL.PgType as Type
import Database.Relational.Schema.PostgreSQL (config)

import Database.Schema.HDBC.Driver
  (TypeMap, LogChan, putVerbose, failWith, maybeIO, hoistMaybe,
   Driver, driverConfig, getFieldsWithMap, getPrimaryKey, emptyDriver)


instance FromSql SqlValue PgAttribute
instance ToSql SqlValue PgAttribute

instance FromSql SqlValue PgType
instance ToSql SqlValue PgType

logPrefix :: String -> String
logPrefix =  ("PostgreSQL: " ++)

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
getPrimaryKey' conn lchan scm' tbl' = do
  let scm = map toLower scm'
      tbl = map toLower tbl'
  mayKeyLen <- runQuery' conn primaryKeyLengthQuerySQL (scm, tbl)
  case mayKeyLen of
    []        ->
      return []
    [keyLen]  -> do
      primCols <- runQuery' conn (primaryKeyQuerySQL keyLen) (scm, tbl)
      let primaryKeyCols = normalizeColumn <$> primCols
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
  guard (not $ null cols) <|>
    compileError lchan ("getFields: No columns found: schema = " ++ scm ++ ", table = " ++ tbl)

  let notNullIdxs = map fst . filter (notNull . snd) . zip [0..] $ cols
  lift . putLog lchan
    $  "getFields: num of columns = " ++ show (length cols)
    ++ ", not null columns = " ++ show notNullIdxs
  let getType' col =
        hoistMaybe (getType (fromList tmap) col) <|>
        compileError lchan ("Type mapping is not defined against PostgreSQL type: " ++ Type.typname (snd col))

  types <- mapM getType' cols
  return (types, notNullIdxs)

-- | Driver implementation
driverPostgreSQL :: IConnection conn => Driver conn
driverPostgreSQL =
  emptyDriver { getFieldsWithMap = getColumns' }
              { getPrimaryKey    = getPrimaryKey' }
              { driverConfig     = config }
