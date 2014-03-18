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
        -> Update p
        -> IO (PreparedUpdate p)
prepare conn = unsafePrepare conn . untypeUpdate

-- | Same as 'prepare'.
prepareUpdate :: IConnection conn
              => conn
              -> Update p
              -> IO (PreparedUpdate p)
prepareUpdate =  prepare

-- | Bind parameters, execute statement and get execution result.
runPreparedUpdate :: ToSql SqlValue p
                  => PreparedUpdate p
                  -> p
                  -> IO Integer
runPreparedUpdate = runPreparedNoFetch

-- | Prepare update statement, bind parameters,
--   execute statement and get execution result.
runUpdate :: (IConnection conn, ToSql SqlValue p)
          => conn
          -> Update p
          -> p
          -> IO Integer
runUpdate conn q p = prepareUpdate conn q >>= (`runPreparedUpdate` p)
