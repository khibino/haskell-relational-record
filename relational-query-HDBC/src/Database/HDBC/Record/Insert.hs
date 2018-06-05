{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.HDBC.Record.Insert
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed 'Insert' running sequence
-- which intermediate structures are typed.
module Database.HDBC.Record.Insert (
  PreparedInsert, prepare, prepareInsert,

  runPreparedInsert, runInsert, mapInsert,

  chunksInsert,
  ) where

import Database.HDBC (IConnection, SqlValue)

import Database.Relational (Insert (..), untypeChunkInsert, chunkSizeOfInsert)
import Database.Record (ToSql, fromRecord)

import Database.HDBC.Record.Statement
  (prepareNoFetch, withPrepareNoFetch, withUnsafePrepare, PreparedStatement, untypePrepared,
   BoundStatement (..), runPreparedNoFetch, runNoFetch, mapNoFetch, executeBoundNoFetch)


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

withPrepareChunksInsert :: (IConnection conn, ToSql SqlValue a)
                        => conn
                        -> Insert a
                        -> (PreparedInsert a -> PreparedStatement [p] () -> Int -> IO b)
                        -> IO b
withPrepareChunksInsert conn i0 body =
  withPrepareNoFetch conn i0
  (\ins -> withUnsafePrepare conn (untypeChunkInsert i0)
           (\iChunk -> body ins iChunk $ chunkSizeOfInsert i0)  )

-- Prepare and insert with chunk insert statement.
chunksInsertActions :: ToSql SqlValue a
                    => [a]
                    -> PreparedInsert a
                    -> PreparedStatement [a] ()
                    -> Int
                    -> IO [[Integer]]
chunksInsertActions rs ins iChunk size =
    mapM insert $ chunks size rs
  where
    insert (Right c) = do
      rv <- executeBoundNoFetch $ chunkBind iChunk c
      rv `seq` return [rv]
    insert (Left  c) =
      mapM (runPreparedInsert ins) c

-- | Prepare and insert with chunk insert statement.
chunksInsert :: (IConnection conn, ToSql SqlValue a) => conn -> Insert a -> [a] -> IO [[Integer]]
chunksInsert conn ins rs =
  withPrepareChunksInsert conn ins $ chunksInsertActions rs
