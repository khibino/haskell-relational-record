{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.HDBC.InsertQuery
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed 'InsertQuery' running sequence
-- which intermediate structures are typed.
module Database.Relational.HDBC.InsertQuery (
  PreparedInsertQuery, prepare, prepareInsertQuery, withPrepareInsertQuery,

  runPreparedInsertQuery, runInsertQuery
  ) where

import Database.HDBC (IConnection, SqlValue)

import Database.Relational (InsertQuery)
import Database.Record (ToSql)

import Database.Relational.HDBC.Statement
  (prepareNoFetch, withPrepareNoFetch, PreparedStatement, executeNoFetch, runNoFetch)

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
runPreparedInsertQuery =  executeNoFetch

-- | Prepare insert statement, bind parameters,
--   execute statement and get execution result.
runInsertQuery :: (IConnection conn, ToSql SqlValue p)
               => conn
               -> InsertQuery p
               -> p
               -> IO Integer
runInsertQuery =  runNoFetch
