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

  runPreparedInsert, runInsert, mapInsert
  ) where

import Database.HDBC (IConnection, SqlValue)

import Database.Relational.Query (Insert)
import Database.Record (ToSql)

import Database.HDBC.Record.Statement
  (prepareNoFetch, PreparedStatement, runPreparedNoFetch, runNoFetch, mapNoFetch)


-- | Typed prepared insert type.
type PreparedInsert a = PreparedStatement a ()

-- | Typed prepare insert operation.
prepare :: IConnection conn
        => conn
        -> Insert a
        -> IO (PreparedInsert a)
prepare =  prepareNoFetch

-- | Same as 'prepare'.
prepareInsert :: IConnection conn
              => conn
              -> Insert a
              -> IO (PreparedInsert a)
prepareInsert = prepare

-- | Bind parameters, execute statement and get execution result.
runPreparedInsert :: ToSql SqlValue a
                  => PreparedInsert a
                  -> a
                  -> IO Integer
runPreparedInsert =  runPreparedNoFetch

-- | Prepare insert statement, bind parameters,
--   execute statement and get execution result.
runInsert :: (IConnection conn, ToSql SqlValue a)
          => conn
          -> Insert a
          -> a
          -> IO Integer
runInsert =  runNoFetch

-- | Prepare and insert each record.
mapInsert :: (IConnection conn, ToSql SqlValue a)
          => conn
          -> Insert a
          -> [a]
          -> IO [Integer]
mapInsert = mapNoFetch
