{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.HDBC.Delete
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed 'Delete' running sequence
-- which intermediate structures are typed.
module Database.Relational.HDBC.Delete (
  PreparedDelete, prepare, prepareDelete, withPrepareDelete,

  runPreparedDelete, runDelete
  ) where

import Database.HDBC (IConnection, SqlValue)

import Database.Relational (Delete)
import Database.Record (ToSql)

import Database.Relational.HDBC.Statement
  (prepareNoFetch, withPrepareNoFetch, PreparedStatement, executeNoFetch, runNoFetch)


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

-- | Bracketed prepare operation.
withPrepareDelete :: IConnection conn
              => conn
              -> Delete p
              -> (PreparedDelete p -> IO a)
              -> IO a
withPrepareDelete = withPrepareNoFetch

-- | Bind parameters, execute statement and get execution result.
runPreparedDelete :: ToSql SqlValue p
                  => PreparedDelete p
                  -> p
                  -> IO Integer
runPreparedDelete =  executeNoFetch

-- | Prepare delete statement, bind parameters,
--   execute statement and get execution result.
runDelete :: (IConnection conn, ToSql SqlValue p)
          => conn
          -> Delete p
          -> p
          -> IO Integer
runDelete =  runNoFetch
