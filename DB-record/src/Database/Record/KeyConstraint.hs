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
  unique, notNull,

  -- * Deriviations
  leftColumnConstraint,
  HasColumnConstraint (keyConstraint),

  derivedUniqueColumnConstraint,
  derivedNotNullColumnConstraint,

  unsafeSpecifyNotNullValue
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

-- | Derivation rule for 'UniqueColumnConstraint'.
unique :: PrimaryColumnConstraint r -> UniqueColumnConstraint r
unique =  unsafeSpecifyColumnConstraint . index

-- | Derivation rule for 'NotNullColumnConstraint'.
notNull :: PrimaryColumnConstraint r -> NotNullColumnConstraint r
notNull =  unsafeSpecifyColumnConstraint . index


-- | Derivation rule of 'ColumnConstraint' 'NotNull' for tuple (,) type.
leftColumnConstraint :: ColumnConstraint NotNull a -> ColumnConstraint NotNull (a, b)
leftColumnConstraint pa = ColumnConstraint (index pa)

-- | Interface of inference rule for 'ColumnConstraint' proof object.
class HasColumnConstraint c a where
  -- | Infer 'ColumnConstraint' proof object.
  keyConstraint :: ColumnConstraint c a

-- | Inference rule of 'ColumnConstraint' 'NotNull' for tuple (,) type.
instance HasColumnConstraint NotNull a => HasColumnConstraint NotNull (a, b) where
  keyConstraint = leftColumnConstraint keyConstraint

-- | Inferred 'UniqueColumnConstraint' proof object.
--   Record type 'r' has unique key which is derived 'r' has primary key.
derivedUniqueColumnConstraint :: HasColumnConstraint Primary r => UniqueColumnConstraint r
derivedUniqueColumnConstraint =  unique keyConstraint

-- | Inferred 'NotNullColumnConstraint' proof object.
--   Record type 'r' has not-null key which is derived 'r' has primary key.
derivedNotNullColumnConstraint :: HasColumnConstraint Primary r => NotNullColumnConstraint r
derivedNotNullColumnConstraint =  notNull keyConstraint


-- | Unsafely generate 'NotNullColumnConstraint' proof object of single column value.
unsafeSpecifyNotNullValue :: NotNullColumnConstraint a
unsafeSpecifyNotNullValue =  unsafeSpecifyColumnConstraint 0
