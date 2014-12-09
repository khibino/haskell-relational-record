{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Database.HDBC.Schema.Oracle
    ( driverOracle
    ) where

import Control.Applicative ((<$>))
import Data.Char (toUpper)
import Data.Map (fromList)
import Data.Maybe (catMaybes)
import Language.Haskell.TH (TypeQ)
import qualified Language.Haskell.TH.Lib.Extra as TH

import Database.HDBC (IConnection, SqlValue)
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC.Record.Persistable ()
import Database.Record.TH (makeRecordPersistableWithSqlTypeDefaultFromDefined)
import Database.HDBC.Schema.Driver
    ( TypeMap, Driver, getFieldsWithMap, getPrimaryKey, emptyDriver
    )

import Database.Relational.Schema.Oracle
    ( normalizeColumn, notNull, getType
    , columnsQuerySQL, primaryKeyQuerySQL
    )
import Database.Relational.Schema.OracleDataDictionary.TabColumns (DbaTabColumns)
import qualified Database.Relational.Schema.OracleDataDictionary.TabColumns as Cols

$(makeRecordPersistableWithSqlTypeDefaultFromDefined
    [t|SqlValue|]
    ''DbaTabColumns)

logPrefix :: String -> String
logPrefix = ("Oracle: " ++)

putLog :: String -> IO ()
putLog = putStrLn . logPrefix

compileErrorIO :: String -> IO a
compileErrorIO = TH.compileErrorIO . logPrefix

getPrimaryKey' :: IConnection conn
               => conn
               -> String -- ^ owner name
               -> String -- ^ table name
               -> IO [String] -- ^ primary key names
getPrimaryKey' conn owner' tbl' = do
    let owner = map toUpper owner'
        tbl = map toUpper tbl'
    prims <- map normalizeColumn . catMaybes <$>
        runQuery' conn primaryKeyQuerySQL (owner, tbl)
    putLog $ "getPrimaryKey: keys = " ++ show prims
    return prims

getFields' :: IConnection conn
           => TypeMap
           -> conn
           -> String
           -> String
           -> IO ([(String, TypeQ)], [Int])
getFields' tmap conn owner' tbl' = do
    let owner = map toUpper owner'
        tbl = map toUpper tbl'
    cols <- runQuery' conn columnsQuerySQL (owner, tbl)
    case cols of
        [] -> compileErrorIO $
            "getFields: No columns found: owner = " ++ owner ++ ", table = " ++ tbl
        _ -> return ()
    let notNullIdxs = map fst . filter (notNull . snd) . zip [0..] $ cols
    putLog $
        "getFields: num of columns = " ++ show (length cols) ++
        ", not null columns = " ++ show notNullIdxs
    let getType' col = case getType (fromList tmap) col of
            Nothing -> compileErrorIO $
                "Type mapping is not defined against Oracle DB type: " ++
                show (Cols.dataType col)
            Just p -> return p
    types <- mapM getType' cols
    return (types, notNullIdxs)

-- | Driver for Oracle DB
driverOracle :: IConnection conn => Driver conn
driverOracle =
    emptyDriver { getFieldsWithMap = getFields' }
                { getPrimaryKey = getPrimaryKey' }
