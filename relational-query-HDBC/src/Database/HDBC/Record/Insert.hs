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

  bulkInsert,
  bulkInsert',
  bulkInsertInterleave,

  chunksInsert,
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import System.IO.Unsafe (unsafeInterleaveIO)
import Database.HDBC (IConnection, SqlValue)

import Database.Relational (Insert (..), untypeChunkInsert, chunkSizeOfInsert)
import Database.Record (ToSql, fromRecord)

import Database.HDBC.Record.Statement
  (prepareNoFetch, withPrepareNoFetch, withUnsafePrepare, PreparedStatement, untypePrepared,
   BoundStatement (..), executeNoFetch, runNoFetch, mapNoFetch, executeBoundNoFetch)


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
runPreparedInsert =  executeNoFetch

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

withPrepareChunksInsert :: (IConnection conn, ToSql SqlValue a)
                        => conn
                        -> Insert a
                        -> (PreparedInsert a -> PreparedStatement [p] () -> Int -> IO b)
                        -> IO b
withPrepareChunksInsert conn i0 body =
  withPrepareNoFetch conn i0
  (\ins -> withUnsafePrepare conn (untypeChunkInsert i0)
           (\iChunk -> body ins iChunk $ chunkSizeOfInsert i0)  )

chunks :: Int -> [a] -> ([[a]], [a])
chunks n = rec'  where
  rec' xs
    | null tl    =  if length c == n
                    then ([c], [])
                    else ( [], c)
    | otherwise  =  (c : cs, ys)  where
      (c, tl) = splitAt n xs
      (cs, ys) = rec' tl

lazyMapIO :: (a -> IO b) -> [a] -> IO [b]
lazyMapIO _  []     =  return []
lazyMapIO f (x:xs)  =  unsafeInterleaveIO $ (:) <$> f x <*> lazyMapIO f xs

chunksLazyAction :: ToSql SqlValue a
                 => [a]
                 -> PreparedInsert a
                 -> PreparedStatement [a] ()
                 -> Int
                 -> IO ([Integer], [Integer])
chunksLazyAction rs ins iChunk size =
    (,)
    <$> lazyMapIO (executeBoundNoFetch . chunkBind iChunk) cs
    <*> (unsafeInterleaveIO $ mapM (runPreparedInsert ins) xs)
  where
    (cs, xs) = chunks size rs

-- | Prepare and insert using chunk insert statement, with the Lazy-IO results of insert statements.
bulkInsertInterleave :: (IConnection conn, ToSql SqlValue a)
                     => conn
                     -> Insert a
                     -> [a]
                     -> IO ([Integer], [Integer])
bulkInsertInterleave conn ins =
  withPrepareChunksInsert conn ins . chunksLazyAction

chunksAction :: ToSql SqlValue a
             => [a]
             -> PreparedInsert a
             -> PreparedStatement [a] ()
             -> Int
             -> IO ()
chunksAction rs ins iChunk size = do
    (zs, os)  <-  chunksLazyAction rs ins iChunk size
    unless (all (== fromIntegral size) zs)
      $ fail "chunksAction: chunks: unexpected result size!"
    unless (all (== 1) os)
      $ fail "chunksAction: tails: unexpected result size!"

-- | Prepare and insert using chunk insert statement.
bulkInsert :: (IConnection conn, ToSql SqlValue a)
           => conn
           -> Insert a
           -> [a]
           -> IO ()
bulkInsert conn ins =
  withPrepareChunksInsert conn ins . chunksAction

-- | Prepare and insert using chunk insert statement, with the results of insert statements.
bulkInsert' :: (IConnection conn, ToSql SqlValue a)
             => conn
             -> Insert a
             -> [a]
             -> IO ([Integer], [Integer])
bulkInsert' conn ins rs = do
  p@(zs, os) <- withPrepareChunksInsert conn ins $ chunksLazyAction rs
  let zl = length zs
      ol = length os
  zl `seq` ol `seq` return p

{-# DEPRECATED chunksInsert "use bulkInsert' instead of this." #-}
-- | Deprecated. Use bulkInsert' instead of this. Prepare and insert using chunk insert statement.
chunksInsert :: (IConnection conn, ToSql SqlValue a)
             => conn
             -> Insert a
             -> [a]
             -> IO [[Integer]]
chunksInsert conn ins rs = do
  (zs, os) <- bulkInsert' conn ins rs
  return $ map (: []) zs ++ [os]
