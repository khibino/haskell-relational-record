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
  PreparedQuery, prepare, prepareQuery,

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
  (ToSql, RecordFromSql, FromSql(recordFromSql), runToRecord)

import Database.HDBC.Record.Statement
  (unsafePrepare, PreparedStatement,
   bindTo, BoundStatement,
   execute, ExecutedStatement, executed)


-- | Typed prepared query type.
type PreparedQuery p a = PreparedStatement p a

-- | Typed prepare query operation.
prepare :: IConnection conn
        => conn                   -- ^ Database connection
        -> Query p a              -- ^ Typed query
        -> IO (PreparedQuery p a) -- ^ Result typed prepared query with parameter type 'p' and result type 'a'
prepare conn = unsafePrepare conn . untypeQuery

-- | Same as 'prepare'.
prepareQuery :: IConnection conn
             => conn                   -- ^ Database connection
             -> Query p a              -- ^ Typed query
             -> IO (PreparedQuery p a) -- ^ Result typed prepared query with parameter type 'p' and result type 'a'
prepareQuery = prepare

-- | Polymorphic fetch operation.
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

-- | Lazily Fetch all records.
fetchAll :: FromSql SqlValue a => ExecutedStatement a -> IO [a]
fetchAll =  fetchRecordsExplicit HDBC.fetchAllRows recordFromSql

-- | Strict version of 'fetchAll'.
fetchAll' :: FromSql SqlValue a => ExecutedStatement a -> IO [a]
fetchAll' =  fetchRecordsExplicit HDBC.fetchAllRows' recordFromSql

-- | Fetch all records but get only first record.
--   Expecting result records is unique.
fetchUnique :: FromSql SqlValue a => ExecutedStatement a -> IO (Maybe a)
fetchUnique es = do
  recs <- fetchAll es
  let z' = listToMaybe recs
  z <- z' `seq` return z'
  HDBC.finish $ executed es
  return z

-- | Fetch expecting result records is unique.
listToUnique :: [a] -> IO (Maybe a)
listToUnique =  d  where
  d []      = return Nothing
  d [r]     = return $ Just r
  d (_:_:_) = fail "fetchUnique': more than one record found."

-- | Fetch all records but get only first record.
--   Expecting result records is unique.
--   Error when records count is more than one.
fetchUnique' :: FromSql SqlValue a => ExecutedStatement a -> IO (Maybe a)
fetchUnique' es = do
  recs <- fetchAll es
  z <- listToUnique recs
  HDBC.finish $ executed es
  return z

-- | Execute statement and lazily fetch all records.
runStatement :: FromSql SqlValue a => BoundStatement a -> IO [a]
runStatement =  (>>= fetchAll) . execute

-- | Strict version of 'runStatement'.
runStatement' :: FromSql SqlValue a => BoundStatement a -> IO [a]
runStatement' =  (>>= fetchAll') . execute

-- | Bind parameters, execute statement and lazily fetch all records.
runPreparedQuery :: (ToSql SqlValue p, FromSql SqlValue a)
                 => p                 -- ^ Parameter type
                 -> PreparedQuery p a -- ^ Statement to bind to
                 -> IO [a]            -- ^ Action to get records
runPreparedQuery p = runStatement . (p `bindTo`)

-- | Strict version of 'runPreparedQuery'.
runPreparedQuery' :: (ToSql SqlValue p, FromSql SqlValue a)
                  => p                 -- ^ Parameter type
                  -> PreparedQuery p a -- ^ Statement to bind to
                  -> IO [a]            -- ^ Action to get records
runPreparedQuery' p = runStatement' . (p `bindTo`)

-- | Prepare SQL, bind parameters, execute statement and lazily fetch all records.
runQuery :: (IConnection conn, ToSql SqlValue p, FromSql SqlValue a)
         => conn      -- ^ Database connection
         -> p         -- ^ Parameter type
         -> Query p a -- ^ Query to get record type 'a' requires parameter 'p'
         -> IO [a]    -- ^ Action to get records
runQuery conn p = (>>= runPreparedQuery p) . prepare conn

-- | Strict version of 'runQuery'.
runQuery' :: (IConnection conn, ToSql SqlValue p, FromSql SqlValue a)
          => conn      -- ^ Database connection
          -> p         -- ^ Parameter type
          -> Query p a -- ^ Query to get record type 'a' requires parameter 'p'
          -> IO [a]    -- ^ Action to get records
runQuery' conn p = (>>= runPreparedQuery' p) . prepare conn
