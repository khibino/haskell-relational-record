{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Record.KeyConstraint
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides proof object definitions
-- of table constraint specifiey by keys.
module Database.Record.KeyConstraint (
  -- * Constraint specified by keys
  ColumnConstraint, index, unsafeSpecifyColumnConstraint,

  Unique, UniqueColumnConstraint,
  NotNull, NotNullColumnConstraint,

  Primary, PrimaryColumnConstraint,

  KeyConstraint, indexes, unsafeSpecifyKeyConstraint,

  UniqueConstraint, PrimaryConstraint,

  -- * Deriviations
  uniqueColumn, notNullColumn,

  leftColumnConstraint,

  unsafeSpecifyNotNullValue,

  deriveComposite,

  unique,

  -- * Inferences
  HasColumnConstraint (columnConstraint),

  derivedUniqueColumnConstraint,
  derivedNotNullColumnConstraint,

  HasKeyConstraint (keyConstraint),

  derivedCompositePrimary,
  derivedUniqueConstraint
  ) where


-- | Proof object to specify table constraint
--   for table record type 'r' and constraint 'c'
--   specified by a single column.
newtype ColumnConstraint c r = ColumnConstraint Int

-- | Index of key which specifies table constraint.
index :: ColumnConstraint c r -> Int
index (ColumnConstraint i) = i

-- | Constraint type. Unique key.
data Unique

-- | Constraint type. Not-null key.
data NotNull

-- | Constraint type. Primary key.
data Primary

-- | Specialized unique constraint.
type UniqueColumnConstraint  = ColumnConstraint Unique

-- | Specialized not-null constraint.
type NotNullColumnConstraint = ColumnConstraint NotNull

-- | Specialized primary constraint.
type PrimaryColumnConstraint = ColumnConstraint Primary

-- | Unsafely generate 'ColumnConstraint' proof object using specified key index.
unsafeSpecifyColumnConstraint :: Int               -- ^ Key index which specify this constraint
                           -> ColumnConstraint c r -- ^ Result constraint proof object
unsafeSpecifyColumnConstraint =  ColumnConstraint

-- | Derivation rule for 'UniqueColumnConstraint'. Derive Unique from Primary.
uniqueColumn :: PrimaryColumnConstraint r -> UniqueColumnConstraint r
uniqueColumn =  unsafeSpecifyColumnConstraint . index

-- | Derivation rule for 'NotNullColumnConstraint'. Derive NotNull from Primary.
notNullColumn :: PrimaryColumnConstraint r -> NotNullColumnConstraint r
notNullColumn =  unsafeSpecifyColumnConstraint . index


-- | Derivation rule of 'ColumnConstraint' 'NotNull' for tuple (,) type.
leftColumnConstraint :: ColumnConstraint NotNull a -> ColumnConstraint NotNull (a, b)
leftColumnConstraint pa = ColumnConstraint (index pa)

-- | Interface of inference rule for 'ColumnConstraint' proof object.
class HasColumnConstraint c a where
  -- | Infer 'ColumnConstraint' proof object.
  columnConstraint :: ColumnConstraint c a

-- | Inference rule of 'ColumnConstraint' 'NotNull' for tuple (,) type.
instance HasColumnConstraint NotNull a => HasColumnConstraint NotNull (a, b) where
  columnConstraint = leftColumnConstraint columnConstraint

-- | Inferred 'UniqueColumnConstraint' proof object.
--   Record type 'r' has unique key which is derived 'r' has primary key.
derivedUniqueColumnConstraint :: HasColumnConstraint Primary r => UniqueColumnConstraint r
derivedUniqueColumnConstraint =  uniqueColumn columnConstraint

-- | Inferred 'NotNullColumnConstraint' proof object.
--   Record type 'r' has not-null key which is derived 'r' has primary key.
derivedNotNullColumnConstraint :: HasColumnConstraint Primary r => NotNullColumnConstraint r
derivedNotNullColumnConstraint =  notNullColumn columnConstraint


-- | Unsafely generate 'NotNullColumnConstraint' proof object of single column value.
unsafeSpecifyNotNullValue :: NotNullColumnConstraint a
unsafeSpecifyNotNullValue =  unsafeSpecifyColumnConstraint 0


-- | Proof object to specify table constraint
--   for table record type 'r' and constraint 'c'.
--   Constraint is specified by composite key.
newtype KeyConstraint c r = KeyConstraint [Int]

-- | Index of key which specifies table constraint.
indexes :: KeyConstraint c r -> [Int]
indexes (KeyConstraint is) = is

-- | Unsafely generate 'KeyConstraint' proof object using specified key indexes.
unsafeSpecifyKeyConstraint :: [Int]             -- ^ Key index which specify this constraint
                           -> KeyConstraint c r -- ^ Result constraint proof object
unsafeSpecifyKeyConstraint =  KeyConstraint

-- | Derivation rule for 'KeyConstraint'. Derive from 'ColumnConstraint'.
deriveComposite :: ColumnConstraint c r -> KeyConstraint c r
deriveComposite =  unsafeSpecifyKeyConstraint . (:[]) . index

-- | Specialized unique constraint.
type UniqueConstraint  = KeyConstraint Unique

-- | Specialized primary constraint.
type PrimaryConstraint = KeyConstraint Primary

-- | Derivation rule for 'UniqueConstraint'.
unique :: PrimaryConstraint r -> UniqueConstraint r
unique =  unsafeSpecifyKeyConstraint . indexes

-- | Interface of inference rule for 'KeyConstraint' proof object.
class HasKeyConstraint c a where
  -- | Infer 'ColumnConstraint' proof object.
  keyConstraint :: KeyConstraint c a

-- | Inferred 'KeyConstraint' proof object.
--   Record type 'r' has composite key which is derived 'r' has single column key.
derivedCompositeConstraint :: HasColumnConstraint c r => KeyConstraint c r
derivedCompositeConstraint =  deriveComposite columnConstraint

-- | Inferred 'PrimaryConstraint' proof object.
--   Record type 'r' has composite primary key which is derived 'r' has single column primary key.
derivedCompositePrimary :: HasColumnConstraint Primary r => PrimaryConstraint r
derivedCompositePrimary =  derivedCompositeConstraint

-- | Inferred 'UniqueConstraint' proof object.
--   Record type 'r' has unique key which is derived 'r' has primary key.
derivedUniqueConstraint :: HasKeyConstraint Primary r => UniqueConstraint r
derivedUniqueConstraint =  unique keyConstraint
