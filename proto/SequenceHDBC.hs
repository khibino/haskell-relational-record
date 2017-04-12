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
  pool', pool, autoPool,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when, void)
import System.IO.Unsafe (unsafeInterleaveIO)
import Database.HDBC (IConnection, SqlValue, commit)
import Database.HDBC.Session (withConnectionIO)

import Language.SQL.Keyword (Keyword (FOR, UPDATE))
import Database.Record (FromSql, ToSql, PersistableWidth)
import Database.Relational.Query
  (relationalQuery', ShowConstantTermsSQL, Relation, )
import qualified Database.Relational.Query as Relation
import qualified Database.Relational.Query.Table as Table
import Database.HDBC.Record.Persistable ()
import Database.HDBC.Record.Statement (bind, execute)
import Database.HDBC.Record.Query (prepareQuery, fetch)
import Database.HDBC.Record.Update (runUpdate)

import Sequence (Sequence, SequenceFromTable, Number, )
import qualified Sequence


unsafePool :: (FromSql SqlValue s, ToSql SqlValue i,
               PersistableWidth i, ShowConstantTermsSQL i,
               Bounded i, Integral i, Show i, IConnection conn)
           => conn
           -> i
           -> Sequence s i
           -> IO [i]
unsafePool conn sz seqt = do
  let t      = Sequence.table seqt
      name   = Table.name t
  pq    <- prepareQuery conn $ relationalQuery' (Relation.table t) [FOR, UPDATE]

  es    <- execute $ pq `bind` ()
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

pool' :: (FromSql SqlValue s, ToSql SqlValue i,
          PersistableWidth i, ShowConstantTermsSQL i,
          Bounded i, Integral i, Show i, IConnection conn,
          SequenceFromTable r s i)
      => conn
      -> i
      -> Relation () r
      -> IO [Number r i]
pool' conn sz = (map Sequence.unsafeSpecifyNumber <$>) . unsafePool conn sz . Sequence.fromRelation

pool :: (FromSql SqlValue s, ToSql SqlValue i,
         PersistableWidth i, ShowConstantTermsSQL i,
         Bounded i, Integral i, Show i, IConnection conn,
         SequenceFromTable r s i)
     => IO conn
     -> i
     -> Relation () r
     -> IO [Number r i]
pool connAct sz rel = withConnectionIO connAct $ \conn -> pool' conn sz rel

autoPool :: (FromSql SqlValue s, ToSql SqlValue i,
             PersistableWidth i, ShowConstantTermsSQL i,
             Bounded i, Integral i, Show i, IConnection conn,
             SequenceFromTable r s i)
         => IO conn
         -> i
         -> Relation () r
         -> IO [Number r i]
autoPool connAct sz rel = loop  where
  loop = unsafeInterleaveIO $ do
    hd <- pool connAct sz rel
    tl <- loop
    return $ hd ++ tl
