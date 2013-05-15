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
module Database.HDBC.Schema.IBMDB2 (
  driverIBMDB2
  ) where

import Prelude hiding (length)

import qualified Data.List as List
import Data.Char (toUpper)
import Data.Map (fromList)
import Language.Haskell.TH (TypeQ)
import qualified Language.Haskell.TH.Name.Extra as TH

import Database.HDBC (IConnection, SqlValue)

import Database.HDBC.Record.Query (runQuery', listToUnique)
import Database.HDBC.Record.Persistable ()

import Database.Record.TH (defineRecordWithSqlTypeDefaultFromDefined)

import qualified Database.Relational.Query.Table as Table

import Database.Relational.Schema.IBMDB2
  (normalizeColumn, notNull, getType, columnsQuerySQL, primaryKeyQuerySQL)

import Database.HDBC.Schema.Driver
  (TypeMap, Driver, getFieldsWithMap, getPrimaryKey, emptyDriver)
  
import Database.Relational.Schema.DB2Syscat.Columns (Columns(Columns), tableOfColumns)
import qualified Database.Relational.Schema.DB2Syscat.Columns as Columns


-- Specify type constructor and data constructor from same table name.
$(defineRecordWithSqlTypeDefaultFromDefined
  [t| SqlValue |] (Table.shortName tableOfColumns))

logPrefix :: String -> String
logPrefix =  ("IBMDB2: " ++)

putLog :: String -> IO ()
putLog =  putStrLn . logPrefix

compileErrorIO :: String -> IO a
compileErrorIO =  TH.compileErrorIO . logPrefix

getPrimaryKey' :: IConnection conn
              => conn
              -> String
              -> String
              -> IO (Maybe String)
getPrimaryKey' conn scm' tbl' = do
  let tbl = map toUpper tbl'
      scm = map toUpper scm'
  mayPrim <- runQuery' conn (scm, tbl) primaryKeyQuerySQL
             >>= listToUnique
  let mayPrimaryKey = normalizeColumn `fmap` mayPrim
  putLog $ "getPrimaryKey: primary key = " ++ show mayPrimaryKey

  return mayPrimaryKey

getFields' :: IConnection conn
          => TypeMap
          -> conn
          -> String
          -> String
          -> IO ([(String, TypeQ)], [Int])
getFields' tmap conn scm' tbl' = do
  let tbl = map toUpper tbl'
      scm = map toUpper scm'
      
  cols <- runQuery' conn (scm, tbl) columnsQuerySQL
  case cols of
    [] ->  compileErrorIO
           $ "getFields: No columns found: schema = " ++ scm ++ ", table = " ++ tbl
    _  ->  return ()

  let notNullIdxs = map fst . filter (notNull . snd) . zip [0..] $ cols
  putLog
    $  "getFields: num of columns = " ++ show (List.length cols)
    ++ ", not null columns = " ++ show notNullIdxs
  let getType' col = case getType (fromList tmap) col of
        Nothing -> compileErrorIO
                   $ "Type mapping is not defined against DB2 type: " ++ Columns.typename col
        Just p  -> return p

  types <- mapM getType' cols
  return (types, notNullIdxs)

driverIBMDB2 :: IConnection conn => Driver conn
driverIBMDB2 =
  emptyDriver { getFieldsWithMap = getFields' }
              { getPrimaryKey    = getPrimaryKey' }
