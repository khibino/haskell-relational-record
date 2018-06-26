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
  pool, autoPool,

  unsafePool, unsafeAutoPool,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when, void)
import System.IO.Unsafe (unsafeInterleaveIO)
import Database.HDBC (IConnection, SqlValue, commit)
import Database.HDBC.Session (withConnectionIO)

import Language.SQL.Keyword (Keyword (FOR, UPDATE))
import Database.Record (FromSql, ToSql, PersistableWidth)
import Database.Relational
  (relationalQuery', LiteralSQL, Relation, )
import qualified Database.Relational as Relation
import qualified Database.Relational.Table as Table
import Database.HDBC.Record.Persistable ()
import Database.HDBC.Record.Statement (bind, executeBound)
import Database.HDBC.Record.Query (prepareQuery, fetch)
import Database.HDBC.Record.Update (runUpdate)

import Database.Relational (Sequence (..), Binding, Number, )
import qualified Database.Relational as Relational


-- | Unsafely get a raw sequence number pool of specified size
unsafePool :: (FromSql SqlValue s, PersistableWidth s,
               ToSql SqlValue i, LiteralSQL i,
               Bounded i, Integral i, Show i, IConnection conn)
           => IO conn
           -> i
           -> Sequence s i
           -> IO [i]
unsafePool connAct sz seqt = withConnectionIO connAct $ \conn -> do
  let t      = seqTable seqt
      name   = Table.name t
  pq    <- prepareQuery conn $ relationalQuery' (Relation.table t) [FOR, UPDATE]

  es    <- executeBound $ pq `bind` ()
  seq0  <- maybe
           (fail $ "No record found in sequence table: " ++ name)
           (return . seqExtract seqt)
           =<< fetch es
  when (maxBound - seq0 < sz) . fail
    $ "Not enough size in sequence table: "
    ++ name ++ ": " ++ show (maxBound - seq0) ++ " < " ++ show sz

  let seq1 = seq0 + sz
  void $ runUpdate conn (Relational.updateNumber seq1 seqt) ()
  maybe (return ()) (const . fail $ "More than two record found in seq table: " ++ name) =<< fetch es

  commit conn
  return [seq0 + 1 .. seq1]

-- | Unsafely get a raw lazy pool of sequence number
unsafeAutoPool :: (FromSql SqlValue s, PersistableWidth s,
                   ToSql SqlValue i, LiteralSQL i,
                   Bounded i, Integral i, Show i, IConnection conn)
               => IO conn
               -> i
               -> Sequence s i
               -> IO [i]
unsafeAutoPool connAct sz seqt = loop  where
  loop = unsafeInterleaveIO $ do
    hd <- unsafePool connAct sz seqt
    (hd ++) <$> loop


-- | Get a sized sequence number pool corresponding proper table 'r'
pool :: (FromSql SqlValue s, ToSql SqlValue i,
         PersistableWidth i, LiteralSQL i,
         Bounded i, Integral i, Show i, IConnection conn,
         Binding r s i)
     => IO conn
     -> i
     -> Relation () r
     -> IO [Number r i]
pool connAct sz =
  (map Relational.unsafeSpecifyNumber <$>)
  . unsafePool connAct sz
  . Relational.fromRelation

-- | Get a lazy pool corresponding proper table 'r'
autoPool :: (FromSql SqlValue s,
             ToSql SqlValue i, LiteralSQL i,
             Bounded i, Integral i, Show i, IConnection conn,
             Binding r s i)
         => IO conn
         -> i
         -> Relation () r
         -> IO [Number r i]
autoPool connAct sz =
  (map Relational.unsafeSpecifyNumber <$>)
  . unsafeAutoPool connAct sz
  . Relational.fromRelation
