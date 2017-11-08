{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Sequence
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides sequence model for relational-record.
module Sequence (
  Sequence (..), relation,
  unsafeSpecifySequence,

  SequenceDerivable (..),

  bindTripleFromPi, primaryBindTriple,
  BindTableToSequence (..), fromRelation,

  Number, unsafeSpecifyNumber, unsafeExtractNumber,
  ($$!), ($$),

  updateNumber,
  ) where

import Prelude hiding (seq)

import Database.Record (PersistableWidth)
import Database.Relational
  (ShowConstantTermsSQL, updateTarget', Update, typedUpdate,
   Pi, TableDerivable, derivedTable, tableOf, Table, Relation,
   (<-#), (.<=.), (!), wheres, unitPlaceHolder, value,
   HasConstraintKey, constraintKey, projectionKey, Primary, Key, )
import qualified Database.Relational as Relational


data Sequence s i =
  Sequence
  { table :: Table s
  , extract :: s -> i
  , key :: Pi s i
  }

-- | Unsafely specify sequence table.
unsafeSpecifySequence :: TableDerivable s => (s -> i) -> Pi s i -> Sequence s i
unsafeSpecifySequence = Sequence derivedTable

relation :: TableDerivable s => Sequence s i -> Relation () s
relation = Relational.table . table

class TableDerivable s => SequenceDerivable s i | s -> i where
  derivedSequence :: Sequence s i

bindTripleFromPi :: (TableDerivable r, SequenceDerivable s i)
                 => Pi r i
                 -> (Table r, Pi r i, Sequence s i)
bindTripleFromPi pi' = (derivedTable, pi', derivedSequence)

primaryBindTriple :: (TableDerivable r, SequenceDerivable s i, HasConstraintKey Primary r i)
                  => (Table r, Pi r i, Sequence s i)
primaryBindTriple = bindTripleFromPi $ projectionKey primaryKey
  where
    primaryKey :: HasConstraintKey Primary r i => Key Primary r i
    primaryKey = constraintKey

class (TableDerivable r, SequenceDerivable s i)
      => BindTableToSequence r s i | r -> s  where
  bindTriple :: (Table r, Pi r i, Sequence s i)

fromRelation :: BindTableToSequence r s i
             => Relation () r
             -> Sequence s i
fromRelation rel = s
  where
    (t, _, s) = bindTriple
    _t = t `asTypeOf` tableOf rel

newtype Number r i = Number i deriving (Eq, Ord, Show)

-- | Unsafely specify sequence number.
unsafeSpecifyNumber :: BindTableToSequence r s i => i -> Number r i
unsafeSpecifyNumber = Number

unsafeExtractNumber :: Number r i -> i
unsafeExtractNumber (Number i) = i

-- | Unsafely apply sequence number.
($$!) :: (i -> r) -> Number r i -> r
($$!) = (. unsafeExtractNumber)

-- | Unsafely apply sequence number. Only safe to build corresponding record type.
($$) :: BindTableToSequence r s i => (i -> r) -> Number r i -> r
($$) = ($$!)

{-
updateNumber :: PersistableWidth p => Sequence r p -> Update (p, p)
updateNumber seqt = typedUpdate (table seqt) . updateTarget' $ \ proj -> do
  (phv', ()) <- placeholder (\ph -> key seqt <-# ph)
  (phx', ()) <- placeholder (\ph -> wheres $ proj ! key seqt .<=. ph)
  return $ (,) |$| phv' |*| phx'
 -}

updateNumber :: (PersistableWidth s, Integral i, ShowConstantTermsSQL i)
             => i            -- ^ sequence number to set. expect not SQL injectable.
             -> Sequence s i -- ^ sequence table
             -> Update ()
updateNumber i seqt = typedUpdate (table seqt) . updateTarget' $ \ proj -> do
  let iv = value i
  key seqt <-# iv
  wheres $ proj ! key seqt .<=. iv -- fool proof
  return unitPlaceHolder
