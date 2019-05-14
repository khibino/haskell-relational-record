{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Schema.IBMDB2
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides driver implementation
-- to load IBM-DB2 system catalog via HDBC.
module Database.HDBC.Schema.IBMDB2 (
  driverIBMDB2
  ) where

import Prelude hiding (length)

import Language.Haskell.TH (TypeQ)

import qualified Data.List as List
import Data.Char (toUpper)
import Data.Map (fromList)
import Control.Applicative ((<$>), (<|>))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)

import Database.HDBC (IConnection, SqlValue)

import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Record.Persistable ()

import Database.Record (FromSql, ToSql)

import Database.Relational.Schema.IBMDB2
  (normalizeColumn, notNull, getType, columnsQuerySQL, primaryKeyQuerySQL)
import Database.Relational.Schema.IBMDB2.Columns (Columns)
import qualified Database.Relational.Schema.IBMDB2.Columns as Columns
import Database.Relational.Schema.IBMDB2 (config)

import Database.HDBC.Schema.Driver
  (TypeMap, LogChan, putVerbose, failWith, maybeIO, hoistMaybe,
   Driver, driverConfig, getFieldsWithMap, getPrimaryKey, emptyDriver)


instance FromSql SqlValue Columns
instance ToSql SqlValue Columns

logPrefix :: String -> String
logPrefix =  ("IBMDB2: " ++)

putLog :: LogChan -> String -> IO ()
putLog lchan =  putVerbose lchan . logPrefix

compileError :: LogChan -> String -> MaybeT IO a
compileError lchan = failWith lchan . logPrefix

getPrimaryKey' :: IConnection conn
               => conn
               -> LogChan
               -> String
               -> String
               -> IO [String]
getPrimaryKey' conn lchan scm' tbl' = do
  let tbl = map toUpper tbl'
      scm = map toUpper scm'
  primCols <- runQuery' conn primaryKeyQuerySQL (scm, tbl)
  let primaryKeyCols = normalizeColumn <$> primCols
  putLog lchan $ "getPrimaryKey: primary key = " ++ show primaryKeyCols

  return primaryKeyCols

getColumns' :: IConnection conn
            => TypeMap
            -> conn
            -> LogChan
            -> String
            -> String
            -> IO ([(String, TypeQ)], [Int])
getColumns' tmap conn lchan scm' tbl' = maybeIO ([], []) id $ do
  let tbl = map toUpper tbl'
      scm = map toUpper scm'

  cols <- lift $ runQuery' conn columnsQuerySQL (scm, tbl)
  guard (not $ null cols) <|>
    compileError lchan ("getFields: No columns found: schema = " ++ scm ++ ", table = " ++ tbl)

  let notNullIdxs = map fst . filter (notNull . snd) . zip [0..] $ cols
  lift . putLog lchan
    $  "getFields: num of columns = " ++ show (List.length cols)
    ++ ", not null columns = " ++ show notNullIdxs
  let getType' col =
        hoistMaybe (getType (fromList tmap) col) <|>
        compileError lchan ("Type mapping is not defined against DB2 type: " ++ Columns.typename col)

  types <- mapM getType' cols
  return (types, notNullIdxs)

-- | Driver implementation
driverIBMDB2 :: IConnection conn => Driver conn
driverIBMDB2 =
  emptyDriver { getFieldsWithMap = getColumns' }
              { getPrimaryKey    = getPrimaryKey' }
              { driverConfig     = config }
