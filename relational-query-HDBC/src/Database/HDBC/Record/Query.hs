{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Database.HDBC.Record.Query
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed 'Query' running sequence
-- which intermediate structres are typed.
module Database.HDBC.Record.Query (
  PreparedQuery, prepare,

  BoundStatement, bindTo', bindTo,

  ExecutedStatement, executed, result, execute,

  fetch, fetchAll, fetchAll',
  listToUnique, fetchUnique, fetchUnique',

  runStatement, runStatement',
  runPreparedQuery, runPreparedQuery',
  runQuery, runQuery'
  ) where

import Data.Maybe (listToMaybe)
import Database.HDBC (IConnection, Statement, SqlValue)
import qualified Database.HDBC as HDBC

import Database.Relational.Query (Query, untypeQuery)

import Database.Record
  (RecordToSql, ToSql(recordToSql), runFromRecord,
   RecordFromSql, FromSql(recordFromSql), runToRecord)


-- | Typed prepared statement type.
newtype PreparedQuery p a = PreparedQuery { prepared :: Statement }

-- | Typed prepared statement which has bound placeholder parameters.
data BoundStatement a =
  BoundStatement
  { bound  :: Statement
  , params :: [SqlValue]
  }

-- | Typed executed statement.
data ExecutedStatement a =
  ExecutedStatement
  { executed :: Statement
  , result   :: Integer
  }

-- | Typed prepare operation.
prepare :: IConnection conn => conn -> Query p a -> IO (PreparedQuery p a)
prepare conn = fmap PreparedQuery . HDBC.prepare conn . untypeQuery

-- | Typed operation to bind parameters.
bindTo' :: RecordToSql SqlValue p -> p -> PreparedQuery p a -> BoundStatement a
bindTo' toSql p q = BoundStatement { bound = prepared q, params = runFromRecord toSql p }

-- | Typed operation to bind parameters. Infered 'RecordToSql' is used.
bindTo :: ToSql SqlValue p => p -> PreparedQuery p a -> BoundStatement a
bindTo =  bindTo' recordToSql

-- | Typed execute operation.
execute :: BoundStatement a -> IO (ExecutedStatement a)
execute bs = do
  let stmt = bound bs
  n <- HDBC.execute stmt (params bs)
  return $ ExecutedStatement stmt n

-- | Polymorphic fetch operation
fetchRecordsExplicit :: Functor f
                     => (Statement -> IO (f [SqlValue]) )
                     -> RecordFromSql SqlValue a
                     -> ExecutedStatement a
                     -> IO (f a)
fetchRecordsExplicit fetchs fromSql es = do
  rows <- fetchs (executed es)
  return $ fmap (runToRecord fromSql) rows

-- | Fetch a record.
fetch :: FromSql SqlValue a => ExecutedStatement a -> IO (Maybe a)
fetch =  fetchRecordsExplicit HDBC.fetchRow recordFromSql

-- | Fetch all records. Lazy read version.
fetchAll :: FromSql SqlValue a => ExecutedStatement a -> IO [a]
fetchAll =  fetchRecordsExplicit HDBC.fetchAllRows recordFromSql

-- | Fetch all records. Strict version.
fetchAll' :: FromSql SqlValue a => ExecutedStatement a -> IO [a]
fetchAll' =  fetchRecordsExplicit HDBC.fetchAllRows' recordFromSql

-- | Fetch expecting result records is unique.
fetchUnique :: FromSql SqlValue a => ExecutedStatement a -> IO (Maybe a)
fetchUnique =  fmap listToMaybe . fetchAll

-- | Fetch expecting result records is unique.
listToUnique :: [a] -> IO (Maybe a)
listToUnique =  d  where
  d []      = return Nothing
  d [r]     = return $ Just r
  d (_:_:_) = fail "listToUnique': more than one record found."

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
