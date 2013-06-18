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
  -- * Constraint specifiey by keys
  SingleKeyConstraint, index, unsafeSpecifySingleKeyConstraint,

  Unique, UniqueColumnConstraint,
  NotNull, NotNullColumnConstraint,

  Primary, PrimaryColumnConstraint,
  unique, notNull,

  -- * Deriviations
  leftSingleKeyConstraint,
  HasSingleKeyConstraint (keyConstraint),

  derivedUniqueColumnConstraint,
  derivedNotNullColumnConstraint,

  unsafeSpecifyNotNullValue
  ) where


-- | Proof object to specify table constraint
--   for table record type 'r' and constraint 'c'.
newtype SingleKeyConstraint c r = SingleKeyConstraint Int

-- | Index of key which specifies table constraint.
index :: SingleKeyConstraint c r -> Int
index (SingleKeyConstraint i) = i

-- | Constraint type. Unique key.
data Unique

-- | Constraint type. Not-null key.
data NotNull

-- | Constraint type. Primary key.
data Primary

-- | Specialized unique constraint.
type UniqueColumnConstraint  = SingleKeyConstraint Unique

-- | Specialized not-null constraint.
type NotNullColumnConstraint = SingleKeyConstraint NotNull

-- | Specialized primary constraint.
type PrimaryColumnConstraint = SingleKeyConstraint Primary

-- | Unsafely generate 'SingleKeyConstraint' proof object using specified key index.
unsafeSpecifySingleKeyConstraint :: Int               -- ^ Key index which specify this constraint
                           -> SingleKeyConstraint c r -- ^ Result constraint proof object
unsafeSpecifySingleKeyConstraint =  SingleKeyConstraint

-- | Derivation rule for 'UniqueColumnConstraint'.
unique :: PrimaryColumnConstraint r -> UniqueColumnConstraint r
unique =  unsafeSpecifySingleKeyConstraint . index

-- | Derivation rule for 'NotNullColumnConstraint'.
notNull :: PrimaryColumnConstraint r -> NotNullColumnConstraint r
notNull =  unsafeSpecifySingleKeyConstraint . index


-- | Derivation rule of 'SingleKeyConstraint' for tuple (,) type.
leftSingleKeyConstraint :: SingleKeyConstraint k a -> SingleKeyConstraint k (a, b)
leftSingleKeyConstraint pa = SingleKeyConstraint (index pa)

-- | Interface of inference rule for 'SingleKeyConstraint' proof object.
class HasSingleKeyConstraint c a where
  -- | Infer 'SingleKeyConstraint' proof object.
  keyConstraint :: SingleKeyConstraint c a

-- | Inference rule of 'SingleKeyConstraint' for tuple (,) type.
instance HasSingleKeyConstraint c a => HasSingleKeyConstraint c (a, b) where
  keyConstraint = leftSingleKeyConstraint keyConstraint

-- | Inferred 'UniqueColumnConstraint' proof object.
--   Record type 'r' has unique key which is derived 'r' has primary key.
derivedUniqueColumnConstraint :: HasSingleKeyConstraint Primary r => UniqueColumnConstraint r
derivedUniqueColumnConstraint =  unique keyConstraint

-- | Inferred 'NotNullColumnConstraint' proof object.
--   Record type 'r' has not-null key which is derived 'r' has primary key.
derivedNotNullColumnConstraint :: HasSingleKeyConstraint Primary r => NotNullColumnConstraint r
derivedNotNullColumnConstraint =  notNull keyConstraint


-- | Unsafely generate 'NotNullColumnConstraint' proof object of single column value.
unsafeSpecifyNotNullValue :: NotNullColumnConstraint a
unsafeSpecifyNotNullValue =  unsafeSpecifySingleKeyConstraint 0
