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

  Unique, UniqueConstraint,
  NotNull, NotNullConstraint,

  Primary, PrimaryConstraint,
  unique, notNull,

  -- * Deriviations
  leftSingleKeyConstraint,
  HasSingleKeyConstraint (keyConstraint),

  derivedUniqueConstraint,
  derivedNotNullConstraint,

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
type UniqueConstraint  = SingleKeyConstraint Unique

-- | Specialized not-null constraint.
type NotNullConstraint = SingleKeyConstraint NotNull

-- | Specialized primary constraint.
type PrimaryConstraint = SingleKeyConstraint Primary

-- | Unsafely generate 'SingleKeyConstraint' proof object using specified key index.
unsafeSpecifySingleKeyConstraint :: Int               -- ^ Key index which specify this constraint
                           -> SingleKeyConstraint c r -- ^ Result constraint proof object
unsafeSpecifySingleKeyConstraint =  SingleKeyConstraint

-- | Derivation rule for 'UniqueConstraint'.
unique :: PrimaryConstraint r -> UniqueConstraint r
unique =  unsafeSpecifySingleKeyConstraint . index

-- | Derivation rule for 'NotNullConstraint'.
notNull :: PrimaryConstraint r -> NotNullConstraint r
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

-- | Inferred 'UniqueConstraint' proof object.
--   Record type 'r' has unique key which is derived 'r' has primary key.
derivedUniqueConstraint :: HasSingleKeyConstraint Primary r => UniqueConstraint r
derivedUniqueConstraint =  unique keyConstraint

-- | Inferred 'NotNullConstraint' proof object.
--   Record type 'r' has not-null key which is derived 'r' has primary key.
derivedNotNullConstraint :: HasSingleKeyConstraint Primary r => NotNullConstraint r
derivedNotNullConstraint =  notNull keyConstraint


-- | Unsafely generate 'NotNullConstraint' proof object of single column value.
unsafeSpecifyNotNullValue :: NotNullConstraint a
unsafeSpecifyNotNullValue =  unsafeSpecifySingleKeyConstraint 0
