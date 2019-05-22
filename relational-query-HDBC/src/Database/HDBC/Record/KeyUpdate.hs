{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.HDBC.Record.KeyUpdate
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed 'KeyUpdate' running sequence
-- which intermediate structures are typed.
module Database.HDBC.Record.KeyUpdate (
  PreparedKeyUpdate,

  prepare, prepareKeyUpdate, withPrepareKeyUpdate,

  bindKeyUpdate,

  runPreparedKeyUpdate, runKeyUpdate
  ) where

import Control.Exception (bracket)
import Data.Traversable (traverse)
import Database.HDBC (IConnection, SqlValue, Statement)
import qualified Database.HDBC as HDBC

import Database.Relational
  (KeyUpdate, untypeKeyUpdate, updateValuesWithKey, Pi,
   WithPlaceholderOffsets, placeholderOffsets, detachPlaceholderOffsets, sortByPlaceholderOffsets, )
import qualified Database.Relational as DSL
import Database.Record (ToSql)

import Database.HDBC.Record.Statement
  (BoundStatement (BoundStatement, bound, params), executeBoundNoFetch)


-- | Typed prepared key-update type.
data PreparedKeyUpdate p a =
  PreparedKeyUpdate
  {
    -- | Key to specify update target records.
    updateKey         :: Pi a p
    -- | Untyped prepared statement before executed.
  , preparedKeyUpdate :: WithPlaceholderOffsets Statement
  }

-- | Typed prepare key-update operation.
prepare :: IConnection conn
        => conn
        -> KeyUpdate p a
        -> IO (PreparedKeyUpdate p a)
prepare conn ku = fmap (PreparedKeyUpdate key) . traverse (HDBC.prepare conn) $ sql where
  sql = untypeKeyUpdate ku
  key = DSL.updateKey ku

-- | Same as 'prepare'.
prepareKeyUpdate :: IConnection conn
                 => conn
                 -> KeyUpdate p a
                 -> IO (PreparedKeyUpdate p a)
prepareKeyUpdate =  prepare

-- | Bracketed prepare operation.
withPrepareKeyUpdate :: IConnection conn
                     => conn
                     -> KeyUpdate p a
                     -> (PreparedKeyUpdate p a -> IO b)
                     -> IO b
withPrepareKeyUpdate conn ku body =
    bracket (traverse (HDBC.prepare conn) sql) (HDBC.finish . detachPlaceholderOffsets)
    $ body . PreparedKeyUpdate key
  where
    sql = untypeKeyUpdate ku
    key = DSL.updateKey ku

-- | Typed operation to bind parameters for 'PreparedKeyUpdate' type.
bindKeyUpdate :: ToSql SqlValue a
              => PreparedKeyUpdate p a
              -> a
              -> BoundStatement ()
bindKeyUpdate pre a =
  BoundStatement { bound = st, params = ps }
  where
    key = updateKey pre
    stphs = preparedKeyUpdate pre 
    st = detachPlaceholderOffsets stphs
    phs = placeholderOffsets stphs
    ps  = sortByPlaceholderOffsets phs $ updateValuesWithKey key a

-- | Bind parameters, execute statement and get execution result.
runPreparedKeyUpdate :: ToSql SqlValue a
                     => PreparedKeyUpdate p a
                     -> a
                     -> IO Integer
runPreparedKeyUpdate pre = executeBoundNoFetch . bindKeyUpdate pre

-- | Prepare insert statement, bind parameters,
--   execute statement and get execution result.
runKeyUpdate :: (IConnection conn, ToSql SqlValue a)
             => conn
             -> KeyUpdate p a
             -> a
             -> IO Integer
runKeyUpdate conn q a = withPrepareKeyUpdate conn q (`runPreparedKeyUpdate` a)
