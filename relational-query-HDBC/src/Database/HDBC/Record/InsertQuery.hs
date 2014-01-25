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
  PreparedInsertQuery, prepare, prepareInsertQuery,

  runPreparedInsertQuery, runInsertQuery
  ) where

import Database.HDBC (IConnection, SqlValue)

import Database.Relational.Query (InsertQuery, untypeInsertQuery)
import Database.Record (ToSql)

import Database.HDBC.Record.Statement
  (unsafePrepare, PreparedStatement, runPreparedNoFetch)

-- | Typed prepared insert query type.
type PreparedInsertQuery p = PreparedStatement p ()

-- | Typed prepare insert-query operation.
prepare :: IConnection conn
        => conn
        -> InsertQuery p
        -> IO (PreparedInsertQuery p)
prepare conn = unsafePrepare conn . untypeInsertQuery

-- | Same as 'prepare'.
prepareInsertQuery :: IConnection conn
                   => conn
                   -> InsertQuery p
                   -> IO (PreparedInsertQuery p)
prepareInsertQuery = prepare

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
runInsertQuery conn q p = prepareInsertQuery conn q >>= (`runPreparedInsertQuery` p)
