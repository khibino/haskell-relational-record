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
newtype PreparedQuery p a =
  PreparedQuery {
    -- | Untyped prepared statement before executed.
    prepared :: Statement
    }

-- | Typed prepared statement which has bound placeholder parameters.
data BoundStatement a =
  BoundStatement
  {
    -- | Untyped prepared statement before executed.
    bound  :: Statement
    -- | Bound parameters.
  , params :: [SqlValue]
  }

-- | Typed executed statement.
data ExecutedStatement a =
  ExecutedStatement
  { -- | Untyped executed statement.
    executed :: Statement
    -- | Result of HDBC execute.
  , result   :: Integer
  }

-- | Typed prepare operation.
prepare :: IConnection conn
        => conn                   -- ^ Database connection
        -> Query p a              -- ^ Typed query
        -> IO (PreparedQuery p a) -- ^ Result typed prepared query with parameter type 'p' and result type 'a'
prepare conn = fmap PreparedQuery . HDBC.prepare conn . untypeQuery

-- | Typed operation to bind parameters.
bindTo' :: RecordToSql SqlValue p -- ^ Proof object to convert from parameter type 'p' into 'SqlValue' list.
        -> p                      -- ^ Parameter to bind
        -> PreparedQuery p a      -- ^ Prepared query to bind to
        -> BoundStatement a       -- ^ Result parameter bound statement
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
