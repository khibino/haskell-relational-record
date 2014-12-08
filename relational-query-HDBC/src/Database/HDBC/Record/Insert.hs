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

  runPreparedInsert, runInsert, mapInsert,

  chunksInsertActions, chunksInsert,
  ) where

import Database.HDBC (IConnection, SqlValue)

import Database.Relational.Query (Insert (..))
import Database.Record (ToSql, fromRecord)

import Database.HDBC.Record.Statement
  (prepareNoFetch, unsafePrepare, PreparedStatement, untypePrepared, BoundStatement (..),
   runPreparedNoFetch, runNoFetch, mapNoFetch, executeNoFetch)


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


-- | Unsafely bind chunk of records.
chunkBind :: ToSql SqlValue p => PreparedStatement [p] () -> [p] -> BoundStatement ()
chunkBind q ps = BoundStatement { bound = untypePrepared q, params =  ps >>= fromRecord }

chunks :: Int -> [a] -> [Either [a] [a]]
chunks n = rec'  where
  rec' xs
    | null tl    =  [ if length c == n
                      then Right c
                      else Left  c ]
    | otherwise  =  Right c : rec' tl  where
      (c, tl) = splitAt n xs

-- | Prepare and insert with chunk insert statement. Result is insert action list.
chunksInsertActions :: (IConnection conn, ToSql SqlValue a)
                    => conn
                    -> Insert a
                    -> [a]
                    -> IO [ IO [Integer] ]
chunksInsertActions conn i0 rs = do
  ins    <- unsafePrepare conn $ untypeInsert i0
  iChunk <- unsafePrepare conn $ untypeChunkInsert i0
  let insert (Right c) = do
        rv <- executeNoFetch $ chunkBind iChunk c
        return [rv]
      insert (Left  c) =
        mapM (runPreparedInsert ins) c
  return . map insert $ chunks (chunkSizeOfInsert i0) rs

-- | Prepare and insert with chunk insert statement.
chunksInsert :: (IConnection conn, ToSql SqlValue a) => conn -> Insert a -> [a] -> IO [[Integer]]
chunksInsert conn ins rs = do
  as <- chunksInsertActions conn ins rs
  sequence as
