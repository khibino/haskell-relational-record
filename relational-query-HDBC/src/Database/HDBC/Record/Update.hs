{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.HDBC.Record.Update
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed 'Update' running sequence
-- which intermediate structres are typed.
module Database.HDBC.Record.Update (
  PreparedUpdate, prepare, prepareUpdate,

  runPreparedUpdate, runUpdate
  ) where

import Database.HDBC (IConnection, SqlValue)

import Database.Relational.Query (Update, untypeUpdate)
import Database.Record (ToSql)

import Database.HDBC.Record.Statement
  (unsafePrepare, PreparedStatement, runPreparedNoFetch)


-- | Typed prepared update type.
type PreparedUpdate p = PreparedStatement p ()

-- | Typed prepare update operation.
prepare :: IConnection conn
        => conn
        -> Update p a
        -> IO (PreparedUpdate (a, p))
prepare conn = unsafePrepare conn . untypeUpdate

-- | Same as 'prepare'.
prepareUpdate :: IConnection conn
              => conn
              -> Update p a
              -> IO (PreparedUpdate (a, p))
prepareUpdate = prepare

-- | Bind parameters, execute statement and get execution result.
runPreparedUpdate :: (ToSql SqlValue a, ToSql SqlValue p)
                  => a
                  -> p
                  -> PreparedUpdate (a, p)
                  -> IO Integer
runPreparedUpdate = curry runPreparedNoFetch

-- | Prepare update statement, bind parameters,
--   execute statement and get execution result.
runUpdate :: (IConnection conn, ToSql SqlValue a, ToSql SqlValue p)
          => conn
          -> a
          -> p
          -> Update p a
          -> IO Integer
runUpdate conn a p = (>>= runPreparedUpdate a p) . prepareUpdate conn
