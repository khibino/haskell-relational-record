{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.HDBC.Query
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed 'Query' running sequence
-- which intermediate structures are typed.
module Database.Relational.HDBC.Query (
  -- * Prepare
  PreparedQuery, prepare, prepareQuery, withPrepareQuery,

  -- * Fetch strictly
  fetch, fetchAll',
  listToUnique, fetchUnique, fetchUnique',

  runStatement',
  runPreparedQuery',
  runQuery',

  -- * Fetch loop
  foldlFetch, forFetch,

  -- * Fetch with Lazy-IO
  -- $fetchWithLazyIO
  fetchAll,
  runStatement,
  runPreparedQuery,
  runQuery,
  ) where

import Control.Applicative ((<$>), pure)
import Data.Monoid (mempty, (<>))
import Data.Maybe (listToMaybe)
import Data.DList (toList)
import Database.HDBC (IConnection, Statement, SqlValue)
import qualified Database.HDBC as HDBC

import Database.Relational (Query, untypeQuery)
import Database.Record (ToSql, FromSql, toRecord)

import Database.Relational.HDBC.Statement
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
--   PreparedStatement is released on closing connection,
--   so connection pooling cases often cause resource leaks.
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

{- $fetchWithLazyIO
__CAUTION!!__

/Lazy-IO/ APIs may be harmful in complex transaction with RDBMs interfaces
which require sequential ordered calls of low-level APIs.
 -}

-- | Fetch a record.
fetch :: FromSql SqlValue a => ExecutedStatement a -> IO (Maybe a)
fetch =  fetchRecords HDBC.fetchRow

-- | /Lazy-IO/ version of 'fetchAll''.
fetchAll :: FromSql SqlValue a => ExecutedStatement a -> IO [a]
fetchAll =  fetchRecords HDBC.fetchAllRows

-- | Strictly fetch all records.
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

-- | Fetch fold-left loop convenient for
--   the sequence of cursor-solid lock actions.
--   Each action is executed after each fetch.
foldlFetch :: FromSql SqlValue a
           => (b -> a -> IO b)    -- ^ action executed after each fetch
           -> b                   -- ^ zero element of result
           -> ExecutedStatement a -- ^ statement to fetch from
           -> IO b
foldlFetch f z st =
    go z
  where
    go ac = do
      let step = (go =<<) . f ac
      maybe (return ac) step =<< fetch st

-- | Fetch loop convenient for
--   the sequence of cursor-solid lock actions.
--   Each action is executed after each fetch.
forFetch :: FromSql SqlValue a
         => ExecutedStatement a -- ^ statement to fetch from
         -> (a -> IO b)         -- ^ action executed after each fetch
         -> IO [b]
forFetch st action =
  toList <$>
  foldlFetch (\ac x -> ((ac <>) . pure) <$> action x) mempty st

-- | /Lazy-IO/ version of 'runStatement''.
runStatement :: FromSql SqlValue a => BoundStatement a -> IO [a]
runStatement =  (>>= fetchAll) . executeBound

-- | Execute a parameter-bounded statement and strictly fetch all records.
runStatement' :: FromSql SqlValue a => BoundStatement a -> IO [a]
runStatement' =  (>>= fetchAll') . executeBound

-- | /Lazy-IO/ version of 'runPreparedQuery''.
runPreparedQuery :: (ToSql SqlValue p, FromSql SqlValue a)
                 => PreparedQuery p a -- ^ Statement to bind to
                 -> p                 -- ^ Parameter type
                 -> IO [a]            -- ^ Action to get records
runPreparedQuery ps = runStatement . bind ps

-- | Bind parameters, execute statement and strictly fetch all records.
runPreparedQuery' :: (ToSql SqlValue p, FromSql SqlValue a)
                  => PreparedQuery p a -- ^ Statement to bind to
                  -> p                 -- ^ Parameter type
                  -> IO [a]            -- ^ Action to get records
runPreparedQuery' ps = runStatement' . bind ps

-- | /Lazy-IO/ version of 'runQuery''.
runQuery :: (IConnection conn, ToSql SqlValue p, FromSql SqlValue a)
         => conn      -- ^ Database connection
         -> Query p a -- ^ Query to get record type 'a' requires parameter 'p'
         -> p         -- ^ Parameter type
         -> IO [a]    -- ^ Action to get records
runQuery conn q p = prepare conn q >>= (`runPreparedQuery` p)

-- | Prepare SQL, bind parameters, execute statement and strictly fetch all records.
runQuery' :: (IConnection conn, ToSql SqlValue p, FromSql SqlValue a)
          => conn      -- ^ Database connection
          -> Query p a -- ^ Query to get record type 'a' requires parameter 'p'
          -> p         -- ^ Parameter type
          -> IO [a]    -- ^ Action to get records
runQuery' conn q p = withPrepareQuery conn q (`runPreparedQuery'` p)
