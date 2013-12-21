{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.HDBC.Schema.MySQL (
  driverMySQL
  ) where

import           Prelude                            hiding (length)
import           Language.Haskell.TH                (TypeQ)
import qualified Language.Haskell.TH.Lib.Extra      as TH
import qualified Data.List                          as List
import           Data.Map                           (fromList)

import Database.HDBC                                (IConnection, SqlValue)
import Database.HDBC.Record.Query                   (runQuery')
import Database.HDBC.Record.Persistable             ()
import Database.HDBC.Schema.Driver                  (TypeMap, Driver, getFieldsWithMap, getPrimaryKey, emptyDriver)

import           Database.Record.TH                 (defineRecordWithSqlTypeDefaultFromDefined)
import qualified Database.Relational.Query.Table    as Table
import           Database.Relational.Schema.MySQL   ( normalizeColumn
                                                    , notNull 
                                                    , getType
                                                    , columnsQuerySQL
                                                    , primaryKeyQuerySQL
                                                    )

import           Database.Relational.Schema.MySQLInfo.Columns (Columns(Columns), tableOfColumns)
import qualified Database.Relational.Schema.MySQLInfo.Columns as Columns

$(defineRecordWithSqlTypeDefaultFromDefined [t| SqlValue |] (Table.shortName tableOfColumns))

logPrefix :: String -> String
logPrefix = ("MySQL: " ++)

putLog :: String -> IO ()
putLog = putStrLn . logPrefix

compileErrorIO :: String -> IO a
compileErrorIO =  TH.compileErrorIO . logPrefix

getPrimaryKey' :: IConnection conn
               => conn
               -> String
               -> String
               -> IO [String]
getPrimaryKey' conn scm tbl = do
    primCols <- runQuery' conn primaryKeyQuerySQL (scm, tbl)
    let primaryKeyCols = normalizeColumn `fmap` primCols
    putLog $ "getPrimaryKey: primary key = " ++ show primaryKeyCols
    return primaryKeyCols

getFields' :: IConnection conn
          => TypeMap
          -> conn
          -> String
          -> String
          -> IO ([(String, TypeQ)], [Int])
getFields' tmap conn scm tbl = do
    cols <- runQuery' conn columnsQuerySQL (scm, tbl)
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
                     $ "Type mapping is not defined against MySQL type: " ++ Columns.dataType col
          Just p  -> return p

    types <- mapM getType' cols
    return (types, notNullIdxs)

-- | Driver implementation
driverMySQL :: IConnection conn => Driver conn
driverMySQL =
    emptyDriver { getFieldsWithMap = getFields' }
                { getPrimaryKey    = getPrimaryKey' }
