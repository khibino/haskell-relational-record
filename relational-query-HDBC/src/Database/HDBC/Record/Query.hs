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
-- which intermediate structures are typed.
module Database.HDBC.Record.Query (
  PreparedQuery, prepare, prepareQuery, withPrepareQuery,

  fetch, fetchAll, fetchAll',
  listToUnique, fetchUnique, fetchUnique',

  runStatement, runStatement',
  runPreparedQuery, runPreparedQuery',
  runQuery, runQuery'
  ) where

import Data.Maybe (listToMaybe)
import Database.HDBC (IConnection, Statement, SqlValue)
import qualified Database.HDBC as HDBC

import Database.Relational (Query, untypeQuery)
import Database.Record (ToSql, FromSql, toRecord)

import Database.HDBC.Record.Statement
  (unsafePrepare, withUnsafePrepare, PreparedStatement,
   bind, BoundStatement,
   executeBound, ExecutedStatement, executed)


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

-- | Bracketed prepare operation.
withPrepareQuery :: IConnection conn
                 => conn                        -- ^ Database connection
                 -> Query p a                   -- ^ Typed query
                 -> (PreparedQuery p a -> IO b) -- ^ Body action to use prepared statement
                 -> IO b                        -- ^ Result action
withPrepareQuery conn = withUnsafePrepare conn . untypeQuery

-- | Polymorphic fetch operation.
fetchRecords :: (Functor f, FromSql SqlValue a)
             => (Statement -> IO (f [SqlValue]) )
             -> ExecutedStatement a
             -> IO (f a)
fetchRecords fetchs es = do
  rows <- fetchs (executed es)
  return $ fmap toRecord rows

-- | Fetch a record.
fetch :: FromSql SqlValue a => ExecutedStatement a -> IO (Maybe a)
fetch =  fetchRecords HDBC.fetchRow

-- | Lazily Fetch all records.
fetchAll :: FromSql SqlValue a => ExecutedStatement a -> IO [a]
fetchAll =  fetchRecords HDBC.fetchAllRows

-- | Strict version of 'fetchAll'.
fetchAll' :: FromSql SqlValue a => ExecutedStatement a -> IO [a]
fetchAll' =  fetchRecords HDBC.fetchAllRows'

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
runStatement =  (>>= fetchAll) . executeBound

-- | Strict version of 'runStatement'.
runStatement' :: FromSql SqlValue a => BoundStatement a -> IO [a]
runStatement' =  (>>= fetchAll') . executeBound

-- | Bind parameters, execute statement and lazily fetch all records.
runPreparedQuery :: (ToSql SqlValue p, FromSql SqlValue a)
                 => PreparedQuery p a -- ^ Statement to bind to
                 -> p                 -- ^ Parameter type
                 -> IO [a]            -- ^ Action to get records
runPreparedQuery ps = runStatement . bind ps

-- | Strict version of 'runPreparedQuery'.
runPreparedQuery' :: (ToSql SqlValue p, FromSql SqlValue a)
                  => PreparedQuery p a -- ^ Statement to bind to
                  -> p                 -- ^ Parameter type
                  -> IO [a]            -- ^ Action to get records
runPreparedQuery' ps = runStatement' . bind ps

-- | Prepare SQL, bind parameters, execute statement and lazily fetch all records.
runQuery :: (IConnection conn, ToSql SqlValue p, FromSql SqlValue a)
         => conn      -- ^ Database connection
         -> Query p a -- ^ Query to get record type 'a' requires parameter 'p'
         -> p         -- ^ Parameter type
         -> IO [a]    -- ^ Action to get records
runQuery conn q p = prepare conn q >>= (`runPreparedQuery` p)

-- | Strict version of 'runQuery'.
runQuery' :: (IConnection conn, ToSql SqlValue p, FromSql SqlValue a)
          => conn      -- ^ Database connection
          -> Query p a -- ^ Query to get record type 'a' requires parameter 'p'
          -> p         -- ^ Parameter type
          -> IO [a]    -- ^ Action to get records
runQuery' conn q p = withPrepareQuery conn q (`runPreparedQuery'` p)
