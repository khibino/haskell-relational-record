{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.HDBC.Record.InsertQuery
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed 'InsertQuery' running sequence
-- which intermediate structres are typed.
module Database.HDBC.Record.InsertQuery (
  PreparedInsertQuery, prepare, prepareInsertQuery, withPrepareInsertQuery,

  runPreparedInsertQuery, runInsertQuery
  ) where

import Database.HDBC (IConnection, SqlValue)

import Database.Relational.Query (InsertQuery)
import Database.Record (ToSql)

import Database.HDBC.Record.Statement
  (prepareNoFetch, withPrepareNoFetch, PreparedStatement, runPreparedNoFetch, runNoFetch)

-- | Typed prepared insert query type.
type PreparedInsertQuery p = PreparedStatement p ()

-- | Typed prepare insert-query operation.
prepare :: IConnection conn
        => conn
        -> InsertQuery p
        -> IO (PreparedInsertQuery p)
prepare = prepareNoFetch

-- | Same as 'prepare'.
prepareInsertQuery :: IConnection conn
                   => conn
                   -> InsertQuery p
                   -> IO (PreparedInsertQuery p)
prepareInsertQuery = prepare

-- | Bracketed prepare operation.
withPrepareInsertQuery :: IConnection conn
                       => conn
                       -> InsertQuery p
                       -> (PreparedInsertQuery p -> IO a)
                       -> IO a
withPrepareInsertQuery = withPrepareNoFetch

-- | Bind parameters, execute statement and get execution result.
runPreparedInsertQuery :: ToSql SqlValue p
                       => PreparedInsertQuery p
                       -> p
                       -> IO Integer
runPreparedInsertQuery =  runPreparedNoFetch

-- | Prepare insert statement, bind parameters,
--   execute statement and get execution result.
runInsertQuery :: (IConnection conn, ToSql SqlValue p)
               => conn
               -> InsertQuery p
               -> p
               -> IO Integer
runInsertQuery =  runNoFetch
