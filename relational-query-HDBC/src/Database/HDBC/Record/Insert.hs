{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.HDBC.Record.Insert
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed 'Insert' running sequence
-- which intermediate structres are typed.
module Database.HDBC.Record.Insert (
  PreparedInsert, prepare, prepareInsert,
  BoundInsert, ExecutedInsert, executeInsert,

  runPreparedInsert, runInsert
  ) where

import Database.HDBC (IConnection, SqlValue)

import Database.Relational.Query (Insert, untypeInsert)
import Database.Record (ToSql)

import Database.HDBC.Record.Statement
  (unsafePrepare, PreparedStatement,
   BoundStatement,
   executeNoFetch, ExecutedStatement,
   runPreparedNoFetch)


-- | Typed prepared insert type.
type PreparedInsert a = PreparedStatement a ()

-- | Typed prepared insert which has bound placeholder parameters.
type BoundInsert = BoundStatement ()

-- | Typed executed insert.
type ExecutedInsert = ExecutedStatement ()

-- | Typed prepare insert operation.
prepare :: IConnection conn
        => conn
        -> Insert a
        -> IO (PreparedInsert a)
prepare conn = unsafePrepare conn . untypeInsert

-- | Same as 'prepare'.
prepareInsert :: IConnection conn
              => conn
              -> Insert a
              -> IO (PreparedInsert a)
prepareInsert = prepare

-- | Typed execute insert operation.
executeInsert :: BoundInsert -> IO Integer
executeInsert =  executeNoFetch

-- | Bind parameters, execute statement and get execution result.
runPreparedInsert :: ToSql SqlValue a
                  => a
                  -> PreparedInsert a
                  -> IO Integer
runPreparedInsert =  runPreparedNoFetch

-- | Prepare insert statement, bind parameters,
--   execute statement and get execution result.
runInsert :: (IConnection conn, ToSql SqlValue a)
          => conn
          -> a
          -> Insert a
          -> IO Integer
runInsert conn p = (>>= runPreparedInsert p) . prepareInsert conn
