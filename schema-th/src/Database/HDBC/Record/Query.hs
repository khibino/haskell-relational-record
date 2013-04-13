{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Database.HDBC.Record.Query
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.Record.Query (
  Query (untypeQuery), typedQuery,

  PreparedQuery, prepare,

  BoundStatement, bindTo', bindTo,

  ExecutedStatement, executed, result, execute,

  fetchAll, fetchAll',
  listToUnique, fetchUnique, fetchUnique',

  runStatement, runStatement',
  runPreparedQuery, runPreparedQuery',
  runQuery, runQuery'
  ) where

import Data.Maybe (listToMaybe)
import Database.HDBC (IConnection, Statement, SqlValue)
import qualified Database.HDBC as HDBC

import Database.Record.ToSql (RecordToSql(fromRecord), ToSql(recordToSql))
import Database.Record.FromSql (RecordFromSql, runToRecord, FromSql(recordFromSql))

newtype Query p a = Query { untypeQuery :: String }

typedQuery :: String -> Query p a
typedQuery =  Query

instance Show (Query p a) where
  show = untypeQuery

newtype PreparedQuery p a = PreparedQuery { prepared :: Statement }

data BoundStatement a =
  BoundStatement
  { bound  :: Statement
  , params :: [SqlValue]
  }

data ExecutedStatement a =
  ExecutedStatement
  { executed :: Statement
  , result   :: Integer
  }

prepare :: IConnection conn => conn -> Query p a -> IO (PreparedQuery p a)
prepare conn = fmap PreparedQuery . HDBC.prepare conn . untypeQuery

bindTo' :: RecordToSql SqlValue p -> p -> PreparedQuery p a -> BoundStatement a
bindTo' toSql p q = BoundStatement { bound = prepared q, params = fromRecord toSql p }

bindTo :: ToSql SqlValue p => p -> PreparedQuery p a -> BoundStatement a
bindTo =  bindTo' recordToSql

execute :: BoundStatement a -> IO (ExecutedStatement a)
execute bs = do
  let stmt = bound bs
  n <- HDBC.execute stmt (params bs)
  return $ ExecutedStatement stmt n

fetchRecordsExplicit :: (Statement -> IO [[SqlValue]])
                     -> RecordFromSql SqlValue a
                     -> ExecutedStatement a
                     -> IO [a]
fetchRecordsExplicit fetch fromSql es = do
  rows <- fetch (executed es)
  return $ map (runToRecord fromSql) rows

fetchAll :: FromSql SqlValue a => ExecutedStatement a -> IO [a]
fetchAll =  fetchRecordsExplicit HDBC.fetchAllRows recordFromSql

fetchAll' :: FromSql SqlValue a => ExecutedStatement a -> IO [a]
fetchAll' =  fetchRecordsExplicit HDBC.fetchAllRows' recordFromSql

fetchUnique :: FromSql SqlValue a => ExecutedStatement a -> IO (Maybe a)
fetchUnique =  fmap listToMaybe . fetchAll

listToUnique :: [a] -> IO (Maybe a)
listToUnique =  d  where
  d []      = return Nothing
  d [r]     = return $ Just r
  d (_:_:_) = ioError . userError $ "listToUnique': more than one record found."

fetchUnique' :: FromSql SqlValue a => ExecutedStatement a -> IO (Maybe a)
fetchUnique' es = do
  fetchAll es >>= listToUnique

runStatement :: FromSql SqlValue a => BoundStatement a -> IO [a]
runStatement =  (>>= fetchAll) . execute

runStatement' :: FromSql SqlValue a => BoundStatement a -> IO [a]
runStatement' =  (>>= fetchAll') . execute

runPreparedQuery :: (ToSql SqlValue p, FromSql SqlValue a)
                 => p
                 -> PreparedQuery p a
                 -> IO [a]
runPreparedQuery p = runStatement . (p `bindTo`)

runPreparedQuery' :: (ToSql SqlValue p, FromSql SqlValue a)
                  => p
                  -> PreparedQuery p a
                  -> IO [a]
runPreparedQuery' p = runStatement' . (p `bindTo`)

runQuery :: (IConnection conn, ToSql SqlValue p, FromSql SqlValue a)
         => conn
         -> p
         -> Query p a
         -> IO [a]
runQuery conn p = (>>= runPreparedQuery p) . prepare conn

runQuery' :: (IConnection conn, ToSql SqlValue p, FromSql SqlValue a)
          => conn
          -> p
          -> Query p a
          -> IO [a]
runQuery' conn p = (>>= runPreparedQuery' p) . prepare conn
