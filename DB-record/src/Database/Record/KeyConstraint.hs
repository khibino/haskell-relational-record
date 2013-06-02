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
  KeyConstraint, index, specifyKeyConstraint,

  Unique, UniqueConstraint,
  NotNull, NotNullConstraint,

  Primary, PrimaryConstraint,
  unique, notNull,

  -- * Deriviations
  leftKeyConstraint,
  HasKeyConstraint (keyConstraint),

  derivedUniqueConstraint,
  derivedNotNullConstraint,

  specifyNotNullValue
  ) where


-- | Proof object to specify table constraint
--   for table record type 'r' and constraint 'c'.
newtype KeyConstraint c r = KeyConstraint Int

-- | Index of key which specifies table constraint.
index :: KeyConstraint c r -> Int
index (KeyConstraint i) = i

-- | Constraint type. Unique key.
data Unique

-- | Constraint type. Not-null key.
data NotNull

-- | Constraint type. Primary key.
data Primary

-- | Specialized unique constraint.
type UniqueConstraint  = KeyConstraint Unique

-- | Specialized not-null constraint.
type NotNullConstraint = KeyConstraint NotNull

-- | Specialized primary constraint.
type PrimaryConstraint = KeyConstraint Primary

-- | Unsafely generate proof object using specified key index.
specifyKeyConstraint :: Int -> KeyConstraint c r
specifyKeyConstraint =  KeyConstraint

-- | Derivation rule for 'UniqueConstraint'.
unique :: PrimaryConstraint r -> UniqueConstraint r
unique =  specifyKeyConstraint . index

-- | Derivation rule for 'NotNullConstraint'.
notNull :: PrimaryConstraint r -> NotNullConstraint r
notNull =  specifyKeyConstraint . index


-- | Derivation rule of 'KeyConstraint' for tuple (,) type.
leftKeyConstraint :: KeyConstraint k a -> KeyConstraint k (a, b)
leftKeyConstraint pa = KeyConstraint (index pa)

-- | Interface of inference rule for 'KeyConstraint' proof object.
class HasKeyConstraint c a where
  keyConstraint :: KeyConstraint c a

-- | Inference rule of 'KeyConstraint' for tuple (,) type.
instance HasKeyConstraint c a => HasKeyConstraint c (a, b) where
  keyConstraint = leftKeyConstraint keyConstraint

-- | Inferred 'UniqueConstraint' proof object.
--   Record type 'r' has unique key which is derived 'r' has primary key.
derivedUniqueConstraint :: HasKeyConstraint Primary r => UniqueConstraint r
derivedUniqueConstraint =  unique keyConstraint

-- | Inferred 'NotNullConstraint' proof object.
--   Record type 'r' has not-null key which is derived 'r' has primary key.
derivedNotNullConstraint :: HasKeyConstraint Primary r => NotNullConstraint r
derivedNotNullConstraint =  notNull keyConstraint


-- | 'NotNullConstraint' proof object of single column value. This is unsafe.
specifyNotNullValue :: NotNullConstraint a
specifyNotNullValue =  specifyKeyConstraint 0
