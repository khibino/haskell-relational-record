{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

-- |
-- Module      : Database.Relational.Sequence
-- Copyright   : 2017-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides structures about sequence tables.
module Database.Relational.Sequence (
  Sequence, seqTable, seqExtract, seqKey, seqRelation,
  unsafeSpecifySequence,

  SeqBinding, boundTable, boundKey, boundSequence,
  unsafeSpecifyBinding, primaryBinding,

  SequenceDerivable (..),

  Binding (..), fromRelation,

  Number, unsafeSpecifyNumber, extractNumber,
  ($$!), ($$),

  updateNumber,
  ) where

import Prelude hiding (seq)

import Database.Record (PersistableWidth)
import Database.Relational.Monad.Class (wheres)
import Database.Relational.Monad.BaseType (Relation)
import Database.Relational.Monad.Trans.Assigning ((<-#))
import Database.Relational.Table (TableDerivable, derivedTable, Table)
import Database.Relational.Pi (Pi)
import Database.Relational.Constraint
  (HasConstraintKey (..), Key, Primary, projectionKey)
import Database.Relational.Projectable ((.<=.), value, unitPlaceHolder, (!))
import Database.Relational.ProjectableClass (ShowConstantTermsSQL)
import Database.Relational.Relation (tableOf)
import qualified Database.Relational.Relation as Relation
import Database.Relational.Effect (updateTarget')
import Database.Relational.Type (Update, typedUpdate)


-- | Basic record to express sequence-table.
--   actual sequence-table is a table which has only one column
--   of integer type.
data Sequence s i =
  Sequence
  { seqTable    :: Table s   -- ^ actual sequence-table
  , seqExtract  :: s -> i    -- ^ sequence number selector for sequence record
  , seqKey      :: Pi s i    -- ^ sequence number projection for sequence record
  }

-- | Unsafely specify sequence table.
unsafeSpecifySequence :: TableDerivable s => (s -> i) -> Pi s i -> Sequence s i
unsafeSpecifySequence = Sequence derivedTable

-- | Infer 'Relation' of sequence table
seqRelation :: TableDerivable s => Sequence s i -> Relation () s
seqRelation = Relation.table . seqTable

-- | 'Sequence' derivation rule
class TableDerivable s => SequenceDerivable s i | s -> i where
  derivedSequence :: Sequence s i

-- | Record to express binding between normal-table and sequence-table.
data SeqBinding r s i =
  SeqBinding
  { boundTable     :: Table r       -- ^ normal-table bound to sequence-table
  , boundKey       :: Pi r i        -- ^ sequence key projection for bound record
  , boundSequence  :: Sequence s i  -- ^ sequence table record
  }

-- | Unsafely specify binding between normal-table and sequence-table.
unsafeSpecifyBinding :: (TableDerivable r, SequenceDerivable s i)
                     => Pi r i -> SeqBinding r s i
unsafeSpecifyBinding k = SeqBinding derivedTable k derivedSequence

-- | Derive binding using primary key.
primaryBinding :: (TableDerivable r, SequenceDerivable s i,
                   HasConstraintKey Primary r i)
               => SeqBinding r s i
primaryBinding = unsafeSpecifyBinding $ primaryKey constraintKey
  where
    primaryKey :: Key Primary r ct -> Pi r ct
    primaryKey = projectionKey

-- | Derivation rule for binding between 'Table' and 'Sequence'
class (TableDerivable r, SequenceDerivable s i)
      => Binding r s i | r -> s  where
  binding :: SeqBinding r s i

  default binding :: HasConstraintKey Primary r i => SeqBinding r s i
  binding = primaryBinding

fromTable :: Binding r s i => Table r -> Sequence s i
fromTable = const derivedSequence

-- | Derive 'Sequence' from corresponding 'Relation'
fromRelation :: Binding r s i
             => Relation () r
             -> Sequence s i
fromRelation = fromTable . tableOf

-- | Sequence number type for record type 'r'
newtype Number r i = Number i deriving (Eq, Ord, Show)

-- | Unsafely specify sequence number.
unsafeSpecifyNumber :: Binding r s i => i -> Number r i
unsafeSpecifyNumber = Number

-- | Get untyped sequence number.
extractNumber :: Number r i -> i
extractNumber (Number i) = i

-- | Unsafely apply sequence number.
($$!) :: (i -> r)    -- ^ sequence number should be passed to proper field of record
      -> Number r i
      -> r
($$!) = (. extractNumber)

-- | Unsafely apply sequence number. Only safe to build corresponding record type.
($$) :: Binding r s i
     => (i -> r)   -- ^ sequence number should be passed to proper field of record
     -> Number r i
     -> r
($$) = ($$!)

{-
updateNumber :: PersistableWidth p => Sequence r p -> Update (p, p)
updateNumber seqt = typedUpdate (table seqt) . updateTarget' $ \ proj -> do
  (phv', ()) <- placeholder (\ph -> key seqt <-# ph)
  (phx', ()) <- placeholder (\ph -> wheres $ proj ! key seqt .<=. ph)
  return $ (,) |$| phv' |*| phx'
 -}

-- | Update statement for sequence table
updateNumber :: (PersistableWidth s, Integral i, ShowConstantTermsSQL i)
             => i            -- ^ sequence number to set. expect not SQL injectable.
             -> Sequence s i -- ^ sequence table
             -> Update ()
updateNumber i seqt = typedUpdate (seqTable seqt) . updateTarget' $ \ proj -> do
  let iv = value i
  seqKey seqt <-# iv
  wheres $ proj ! seqKey seqt .<=. iv -- fool proof
  return unitPlaceHolder
