{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.HDBC.Record.KeyUpdate
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed 'KeyUpdate' running sequence
-- which intermediate structres are typed.
module Database.HDBC.Record.KeyUpdate (
  PreparedKeyUpdate, prepare, prepareKeyUpdate,

  runPreparedKeyUpdate, runKeyUpdate
  ) where

import Database.HDBC (IConnection, SqlValue, Statement)
import qualified Database.HDBC as HDBC

import Database.Relational.Query
  (KeyUpdate, untypeKeyUpdate, updateValuesWithKey, Pi)
import qualified Database.Relational.Query as Query
import Database.Record (ToSql)

import Database.HDBC.Record.Statement
  (BoundStatement (BoundStatement, bound, params), executeNoFetch)


-- | Typed prepared update type.
data PreparedKeyUpdate p a =
  PreparedKeyUpdate
  {
    -- | Key to specify update target records.
    updateKey         :: Pi a p
    -- | Untyped prepared statement before executed.
  , preparedKeyUpdate :: Statement
  }

-- | Typed prepare update operation.
prepare :: IConnection conn
        => conn
        -> KeyUpdate p a
        -> IO (PreparedKeyUpdate p a)
prepare conn ku = fmap (PreparedKeyUpdate key) . HDBC.prepare conn $ sql  where
  sql = untypeKeyUpdate ku
  key = Query.updateKey ku

-- | Same as 'prepare'.
prepareKeyUpdate :: IConnection conn
              => conn
              -> KeyUpdate p a
              -> IO (PreparedKeyUpdate p a)
prepareKeyUpdate = prepare

bindToKeyUpdate :: ToSql SqlValue a
                   => a
                   -> PreparedKeyUpdate p a
                   -> BoundStatement ()
bindToKeyUpdate a pre =
  BoundStatement { bound = preparedKeyUpdate pre, params = updateValuesWithKey key a }
  where key = updateKey pre

-- | Bind parameters, execute statement and get execution result.
runPreparedKeyUpdate :: ToSql SqlValue a
                     => a
                     -> PreparedKeyUpdate p a
                     -> IO Integer
runPreparedKeyUpdate a = executeNoFetch . bindToKeyUpdate a

-- | Prepare insert statement, bind parameters,
--   execute statement and get execution result.
runKeyUpdate :: (IConnection conn, ToSql SqlValue a)
             => conn
             -> a
             -> KeyUpdate p a
             -> IO Integer
runKeyUpdate conn a = (>>= runPreparedKeyUpdate a) . prepareKeyUpdate conn
