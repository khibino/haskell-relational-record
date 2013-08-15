{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.HDBC.Record.Delete
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed 'Delete' running sequence
-- which intermediate structres are typed.
module Database.HDBC.Record.Delete (
  PreparedDelete, prepare, prepareDelete,
  BoundDelete, ExecutedDelete, executeDelete,

  runPreparedDelete, runDelete
  ) where

import Database.HDBC (IConnection, SqlValue)

import Database.Relational.Query (Delete, untypeDelete)
import Database.Record (ToSql)

import Database.HDBC.Record.Statement
  (unsafePrepare, PreparedStatement,
   BoundStatement,
   executeNoFetch, ExecutedStatement,
   runPreparedNoFetch)


-- | Typed prepared insert type.
type PreparedDelete p = PreparedStatement p ()

-- | Typed prepared insert which has bound placeholder parameters.
type BoundDelete = BoundStatement ()

-- | Typed executed insert.
type ExecutedDelete = ExecutedStatement ()

-- | Typed prepare insert operation.
prepare :: IConnection conn
        => conn
        -> Delete p
        -> IO (PreparedDelete p)
prepare conn = unsafePrepare conn . untypeDelete

-- | Same as 'prepare'.
prepareDelete :: IConnection conn
              => conn
              -> Delete p
              -> IO (PreparedDelete p)
prepareDelete = prepare

-- | Typed execute insert operation.
executeDelete :: BoundDelete -> IO Integer
executeDelete =  executeNoFetch

-- | Bind parameters, execute statement and get execution result.
runPreparedDelete :: ToSql SqlValue p
                  => p
                  -> PreparedDelete p
                  -> IO Integer
runPreparedDelete =  runPreparedNoFetch

-- | Prepare insert statement, bind parameters,
--   execute statement and get execution result.
runDelete :: (IConnection conn, ToSql SqlValue p)
          => conn
          -> p
          -> Delete p
          -> IO Integer
runDelete conn p = (>>= runPreparedDelete p) . prepareDelete conn
