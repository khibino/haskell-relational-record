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

  runPreparedDelete, runDelete
  ) where

import Database.HDBC (IConnection, SqlValue)

import Database.Relational.Query (Delete)
import Database.Record (ToSql)

import Database.HDBC.Record.Statement
  (prepareNoFetch, PreparedStatement, runPreparedNoFetch, runNoFetch)


-- | Typed prepared delete type.
type PreparedDelete p = PreparedStatement p ()

-- | Typed prepare delete operation.
prepare :: IConnection conn
        => conn
        -> Delete p
        -> IO (PreparedDelete p)
prepare =  prepareNoFetch

-- | Same as 'prepare'.
prepareDelete :: IConnection conn
              => conn
              -> Delete p
              -> IO (PreparedDelete p)
prepareDelete = prepare

-- | Bind parameters, execute statement and get execution result.
runPreparedDelete :: ToSql SqlValue p
                  => PreparedDelete p
                  -> p
                  -> IO Integer
runPreparedDelete =  runPreparedNoFetch

-- | Prepare delete statement, bind parameters,
--   execute statement and get execution result.
runDelete :: (IConnection conn, ToSql SqlValue p)
          => conn
          -> Delete p
          -> p
          -> IO Integer
runDelete =  runNoFetch
