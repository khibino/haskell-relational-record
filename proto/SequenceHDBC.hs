{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : SequenceHDBC
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides sequence operations of relational-record for HDBC.
module SequenceHDBC (
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
  (relationalQuery', ShowConstantTermsSQL, Relation, )
import qualified Database.Relational as Relation
import qualified Database.Relational.Table as Table
import Database.HDBC.Record.Persistable ()
import Database.HDBC.Record.Statement (bind, executeBound)
import Database.HDBC.Record.Query (prepareQuery, fetch)
import Database.HDBC.Record.Update (runUpdate)

import Sequence (Sequence, Binding, Number, )
import qualified Sequence


unsafePool :: (FromSql SqlValue s, PersistableWidth s,
               ToSql SqlValue i, ShowConstantTermsSQL i,
               Bounded i, Integral i, Show i, IConnection conn)
           => IO conn
           -> i
           -> Sequence s i
           -> IO [i]
unsafePool connAct sz seqt = withConnectionIO connAct $ \conn -> do
  let t      = Sequence.table seqt
      name   = Table.name t
  pq    <- prepareQuery conn $ relationalQuery' (Relation.table t) [FOR, UPDATE]

  es    <- executeBound $ pq `bind` ()
  seq0  <- maybe
           (fail $ "No record found in sequence table: " ++ name)
           (return . Sequence.extract seqt)
           =<< fetch es
  when (maxBound - seq0 < sz) . fail
    $ "Not enough size in sequence table: "
    ++ name ++ ": " ++ show (maxBound - seq0) ++ " < " ++ show sz

  let seq1 = seq0 + sz
  void $ runUpdate conn (Sequence.updateNumber seq1 seqt) ()
  maybe (return ()) (const . fail $ "More than two record found in seq table: " ++ name) =<< fetch es

  commit conn
  return [seq0 + 1 .. seq1]

unsafeAutoPool :: (FromSql SqlValue s, PersistableWidth s,
                   ToSql SqlValue i, ShowConstantTermsSQL i,
                   Bounded i, Integral i, Show i, IConnection conn)
               => IO conn
               -> i
               -> Sequence s i
               -> IO [i]
unsafeAutoPool connAct sz seqt = loop  where
  loop = unsafeInterleaveIO $ do
    hd <- unsafePool connAct sz seqt
    (hd ++) <$> loop


pool :: (FromSql SqlValue s, ToSql SqlValue i,
         PersistableWidth i, ShowConstantTermsSQL i,
         Bounded i, Integral i, Show i, IConnection conn,
         Binding r s i)
     => IO conn
     -> i
     -> Relation () r
     -> IO [Number r i]
pool connAct sz =
  (map Sequence.unsafeSpecifyNumber <$>)
  . unsafePool connAct sz
  . Sequence.fromRelation

autoPool :: (FromSql SqlValue s,
             ToSql SqlValue i, ShowConstantTermsSQL i,
             Bounded i, Integral i, Show i, IConnection conn,
             Binding r s i)
         => IO conn
         -> i
         -> Relation () r
         -> IO [Number r i]
autoPool connAct sz =
  (map Sequence.unsafeSpecifyNumber <$>)
  . unsafeAutoPool connAct sz
  . Sequence.fromRelation
