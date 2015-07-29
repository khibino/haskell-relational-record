{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Schema.IBMDB2
-- Copyright   : 2013 Kei Hibino
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
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)

import Database.HDBC (IConnection, SqlValue)

import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Record.Persistable ()

import Database.Record.TH (makeRecordPersistableWithSqlTypeDefaultFromDefined)

import Database.Relational.Schema.IBMDB2
  (normalizeColumn, notNull, getType, columnsQuerySQL, primaryKeyQuerySQL)
import Database.Relational.Schema.DB2Syscat.Columns (Columns)
import qualified Database.Relational.Schema.DB2Syscat.Columns as Columns

import Database.HDBC.Schema.Driver
  (TypeMap, LogChan, putVerbose, maybeIO,
   Driver, getFieldsWithMap, getPrimaryKey, emptyDriver)


-- Specify type constructor and data constructor from same table name.
$(makeRecordPersistableWithSqlTypeDefaultFromDefined
  [t| SqlValue |] ''Columns)

logPrefix :: String -> String
logPrefix =  ("IBMDB2: " ++)

putLog :: LogChan -> String -> IO ()
putLog lchan =  putVerbose lchan . logPrefix

compileErrorIO :: String -> IO a
compileErrorIO =  fail . logPrefix

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
  let primaryKeyCols = normalizeColumn `fmap` primCols
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
  lift . when (null cols) . compileErrorIO
    $ "getFields: No columns found: schema = " ++ scm ++ ", table = " ++ tbl

  let notNullIdxs = map fst . filter (notNull . snd) . zip [0..] $ cols
  lift . putLog lchan
    $  "getFields: num of columns = " ++ show (List.length cols)
    ++ ", not null columns = " ++ show notNullIdxs
  let getType' col = case getType (fromList tmap) col of
        Nothing -> compileErrorIO
                   $ "Type mapping is not defined against DB2 type: " ++ Columns.typename col
        Just p  -> return p

  types <- lift $ mapM getType' cols
  return (types, notNullIdxs)

-- | Driver implementation
driverIBMDB2 :: IConnection conn => Driver conn
driverIBMDB2 =
  emptyDriver { getFieldsWithMap = getColumns' }
              { getPrimaryKey    = getPrimaryKey' }
