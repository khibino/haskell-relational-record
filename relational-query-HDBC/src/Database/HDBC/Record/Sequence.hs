{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.HDBC.Record.Sequence
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides operations for sequence tables of relational-query with HDBC.
module Database.HDBC.Record.Sequence (
  -- * Get pool of sequence numbers
  getPool, getSeq, getAutoPool,
  poolFromSeq, autoPoolFromSeq,

  -- * Deprecated
  pool, autoPool,
  unsafePool, unsafeAutoPool,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when, void)
import Data.Maybe (listToMaybe)
import System.IO.Unsafe (unsafeInterleaveIO)
import Database.HDBC (IConnection, SqlValue, commit)
import Database.HDBC.Session (withConnectionIO)

import Language.SQL.Keyword (Keyword (FOR, UPDATE))
import Database.Record (FromSql, ToSql, PersistableWidth)
import Database.Relational
  (relationalQuery', LiteralSQL, Relation, relationFromTable,
   seqFromRelation, seqTable, tableName, updateNumber)
import Database.HDBC.Record.Persistable ()
import Database.HDBC.Record.Statement (bind, executeBound)
import Database.HDBC.Record.Query (prepareQuery, fetch)
import Database.HDBC.Record.Update (runUpdate)

import Database.Relational (Sequence (..), Binding, Number, unsafeSpecifyNumber)


-- | Get a sized pool of sequence number from sequence table corresponding proper Table 'r'
getPool :: (FromSql SqlValue s, ToSql SqlValue i,
            PersistableWidth i, LiteralSQL i,
            Bounded i, Integral i, Show i, IConnection conn,
            Binding r s i)
        => IO conn       -- ^ action to connect to DBMS
        -> i             -- ^ pool size
        -> Relation () r -- ^ table relation corresponding sequence table
        -> IO [i]        -- ^ action to get pool
getPool connAct sz = seqPool connAct sz . seqFromRelation

getSeq :: (FromSql SqlValue s, ToSql SqlValue i,
            PersistableWidth i, LiteralSQL i,
            Bounded i, Integral i, Show i, IConnection conn,
            Binding r s i)
       => IO conn       -- ^ action to connect to DBMS
       -> Relation () r -- ^ table relation corresponding sequence table
       -> IO i          -- ^ action to get pool
getSeq connAct rel =
    maybe (fail $ "Sequence.getSeq: fail to get seq from seq-table: " ++ n) return . listToMaybe =<<
    getPool connAct 1 rel
  where
    n = tableName . seqTable $ seqFromRelation rel

-- | Get a lazy-IO pool of sequence number from sequence table corresponding proper Table 'r'
getAutoPool :: (FromSql SqlValue s,
                ToSql SqlValue i, LiteralSQL i,
                Bounded i, Integral i, Show i, IConnection conn,
                Binding r s i)
            => IO conn       -- ^ action to connect to DBMS
            -> i             -- ^ buffer size
            -> Relation () r -- ^ table relation corresponding sequence table
            -> IO [i]        -- ^ action to get lazy-IO pool
getAutoPool connAct sz = unsafeAutoPool connAct sz . seqFromRelation


-- | 'Number' result version of 'getPool'.
pool :: (FromSql SqlValue s, ToSql SqlValue i,
         PersistableWidth i, LiteralSQL i,
         Bounded i, Integral i, Show i, IConnection conn,
         Binding r s i)
     => IO conn
     -> i
     -> Relation () r
     -> IO [Number r i]
pool connAct sz =
  (map unsafeSpecifyNumber <$>)
  . seqPool connAct sz
  . seqFromRelation
{-# WARNING pool "Number will be dropped in the future. use getPool instead of this." #-}

-- | 'Number' result version of 'getAutoPool'.
autoPool :: (FromSql SqlValue s,
             ToSql SqlValue i, LiteralSQL i,
             Bounded i, Integral i, Show i, IConnection conn,
             Binding r s i)
         => IO conn
         -> i
         -> Relation () r
         -> IO [Number r i]
autoPool connAct sz =
  (map unsafeSpecifyNumber <$>)
  . unsafeAutoPool connAct sz
  . seqFromRelation
{-# WARNING autoPool "Number will be dropped in the future. use getAutoPool instead of this." #-}

-----

-- | Get a sized pool of sequence number from sequence table directly.
poolFromSeq :: (FromSql SqlValue s, PersistableWidth s,
                ToSql SqlValue i, LiteralSQL i,
                Bounded i, Integral i, Show i, IConnection conn)
            => IO conn      -- ^ action to connect to DBMS
            -> i            -- ^ pool size
            -> Sequence s i -- ^ sequence table to get pool from
            -> IO [i]       -- ^ action to get pool
poolFromSeq = seqPool

-- | Get a lazy-IO pool of sequence number from sequence table directly.
autoPoolFromSeq :: (FromSql SqlValue s, PersistableWidth s,
                   ToSql SqlValue i, LiteralSQL i,
                   Bounded i, Integral i, Show i, IConnection conn)
                => IO conn      -- ^ action to connect to DBMS
                -> i            -- ^ buffer size
                -> Sequence s i -- ^ sequence table to get pool from
                -> IO [i]       -- ^ action to get lazy-IO pool
autoPoolFromSeq connAct sz seqt = loop  where
  loop = unsafeInterleaveIO $ do
    hd <- seqPool connAct sz seqt
    (hd ++) <$> loop


-- | Depredated. use poolFromSeq instead of this.
unsafePool :: (FromSql SqlValue s, PersistableWidth s,
               ToSql SqlValue i, LiteralSQL i,
               Bounded i, Integral i, Show i, IConnection conn)
           => IO conn
           -> i
           -> Sequence s i
           -> IO [i]
unsafePool = seqPool
{-# DEPRECATED unsafePool "use poolFromSeq instead of this." #-}

-- | Deprecated. use autoPoolFromSeq instead of this.
unsafeAutoPool :: (FromSql SqlValue s, PersistableWidth s,
                   ToSql SqlValue i, LiteralSQL i,
                   Bounded i, Integral i, Show i, IConnection conn)
               => IO conn
               -> i
               -> Sequence s i
               -> IO [i]
unsafeAutoPool = autoPoolFromSeq
{-# DEPRECATED unsafeAutoPool "use autoPoolFromSeq instead of this." #-}

seqPool :: (FromSql SqlValue s, PersistableWidth s,
            ToSql SqlValue i, LiteralSQL i,
            Bounded i, Integral i, Show i, IConnection conn)
        => IO conn
        -> i
        -> Sequence s i
        -> IO [i]
seqPool connAct sz seqt = withConnectionIO connAct $ \conn -> do
  let t      = seqTable seqt
      name   = tableName t
  pq    <- prepareQuery conn $ relationalQuery' (relationFromTable t) [FOR, UPDATE]

  es    <- executeBound $ pq `bind` ()
  seq0  <- maybe
           (fail $ "No record found in sequence table: " ++ name)
           (return . seqExtract seqt)
           =<< fetch es
  when (maxBound - seq0 < sz) . fail
    $ "Not enough size in sequence table: "
    ++ name ++ ": " ++ show (maxBound - seq0) ++ " < " ++ show sz

  let seq1 = seq0 + sz
  void $ runUpdate conn (updateNumber seq1 seqt) ()
  maybe (return ()) (const . fail $ "More than two record found in seq table: " ++ name) =<< fetch es

  commit conn
  return [seq0 + 1 .. seq1]
