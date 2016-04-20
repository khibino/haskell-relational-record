{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Schema.MySQL
-- Copyright   : 2013 Sho KURODA
-- License     : BSD3
--
-- Maintainer  : krdlab@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.Schema.MySQL
    (
      driverMySQL
    )
    where

import           Prelude                            hiding (length)
import           Language.Haskell.TH                (TypeQ)
import           Control.Applicative                ((<$>), (<|>))
import           Control.Monad                      (guard)
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.Trans.Maybe          (MaybeT)
import           Data.Char                          (toUpper)
import qualified Data.List                          as List
import           Data.Map                           (fromList)

import           Database.HDBC                      (IConnection, SqlValue)
import           Database.HDBC.Record.Query         (runQuery')
import           Database.HDBC.Record.Persistable   ()
import           Database.HDBC.Schema.Driver        ( TypeMap
                                                    , LogChan
                                                    , putVerbose
                                                    , failWith
                                                    , maybeIO
                                                    , hoistMaybe
                                                    , Driver
                                                    , getFieldsWithMap
                                                    , getPrimaryKey
                                                    , emptyDriver
                                                    )
import           Database.Record.TH                 (makeRecordPersistableWithSqlTypeDefaultFromDefined)
import           Database.Relational.Schema.MySQL   ( normalizeColumn
                                                    , notNull
                                                    , getType
                                                    , columnsQuerySQL
                                                    , primaryKeyQuerySQL
                                                    )

import           Database.Relational.Schema.MySQLInfo.Columns (Columns)
import qualified Database.Relational.Schema.MySQLInfo.Columns as Columns

$(makeRecordPersistableWithSqlTypeDefaultFromDefined [t| SqlValue |] ''Columns)

logPrefix :: String -> String
logPrefix = ("MySQL: " ++)

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
getColumns' tmap conn lchan scm tbl = maybeIO ([], []) id $ do
    cols <- lift $ runQuery' conn columnsQuerySQL (scm, tbl)
    guard (not $ null cols) <|>
      compileError lchan
      ("getFields: No columns found: schema = " ++ scm
       ++ ", table = " ++ tbl)
    let notNullIdxs = map fst . filter (notNull . snd) . zip [0..] $ cols
    lift . putLog lchan
      $  "getFields: num of columns = " ++ show (List.length cols)
      ++ ", not null columns = " ++ show notNullIdxs
    types <- mapM getType' cols
    return (types, notNullIdxs)
    where
        getType' col =
            hoistMaybe (getType (fromList tmap) col) <|>
            compileError lchan
            ("Type mapping is not defined against MySQL type: "
             ++ (map toUpper (Columns.columnType col)))

-- | Driver implementation
driverMySQL :: IConnection conn => Driver conn
driverMySQL =
    emptyDriver { getFieldsWithMap = getColumns' }
                { getPrimaryKey    = getPrimaryKey' }
