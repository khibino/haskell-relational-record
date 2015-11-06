{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.HDBC.Schema.MySQL
    (
      driverMySQL
    )
    where

import           Prelude                            hiding (length)
import           Language.Haskell.TH                (TypeQ)
import           Control.Applicative                ((<|>))
import           Control.Monad                      (guard)
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.Trans.Maybe          (MaybeT)
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

compileErrorIO :: LogChan -> String -> MaybeT IO a
compileErrorIO lchan = failWith lchan . logPrefix

getPrimaryKey' :: IConnection conn
               => conn
               -> LogChan
               -> String
               -> String
               -> IO [String]
getPrimaryKey' conn lchan scm tbl = do
    primCols <- runQuery' conn primaryKeyQuerySQL (scm, tbl)
    let primaryKeyCols = normalizeColumn `fmap` primCols
    putLog lchan $ "getPrimaryKey: primary key = " ++ show primaryKeyCols
    return primaryKeyCols

getFields' :: IConnection conn
           => TypeMap
           -> conn
           -> LogChan
           -> String
           -> String
           -> IO ([(String, TypeQ)], [Int])
getFields' tmap conn lchan scm tbl = maybeIO ([], []) id $ do
    cols <- lift $ runQuery' conn columnsQuerySQL (scm, tbl)
    guard (not $ null cols) <|>
      compileErrorIO lchan
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
            compileErrorIO lchan
            ("Type mapping is not defined against MySQL type: "
             ++ Columns.dataType col)

-- | Driver implementation
driverMySQL :: IConnection conn => Driver conn
driverMySQL =
    emptyDriver { getFieldsWithMap = getFields' }
                { getPrimaryKey    = getPrimaryKey' }
