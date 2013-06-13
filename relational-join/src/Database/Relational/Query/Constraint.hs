{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Database.Relational.Query.Constraint
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides proof object definitions of constraint key.
-- Difference between this module and "Database.Record.KeyConstraint" is
-- typed constraint key column definition is included in this module.
module Database.Relational.Query.Constraint (
  -- * Constraint Key proof object
  Key, index, unsafeDefineConstraintKey,
  tableConstraint, projectionKey,

  unsafeReturnKey, -- unsafeAppendConstraint,

  -- * Derivation rules
  uniqueKey, notNullKey,

  -- * Inference rules
  HasConstraintKey (..),

  -- * Constraint types
  Primary, Unique, NotNull
  ) where


import Database.Relational.Query.Pi (Pi)
import qualified Database.Relational.Query.Pi.Unsafe as UnsafePi
import Database.Record.KeyConstraint
  (SingleKeyConstraint, unsafeSpecifySingleKeyConstraint,
   Primary, Unique, NotNull)

import qualified Database.Record.KeyConstraint as C
import Database.Record (PersistableWidth)


-- | Constraint Key proof object. Constraint type 'c', record type 'r' and column type 'ft'.
newtype Key c r ct = Key Int

-- | Index of key which specifies constraint key.
index :: Key c r ct -> Int
index (Key i) = i

-- | Unsafely generate constraint 'Key' proof object using specified key index.
unsafeDefineConstraintKey :: Int        -- ^ Key index which specify this constraint key
                          -> Key c r ct -- ^ Result constraint key proof object
unsafeDefineConstraintKey =  Key

-- | Get table constraint 'KeyConstraint' proof object from constraint 'Key'.
tableConstraint :: Key c r ct -> SingleKeyConstraint c r
tableConstraint =  unsafeSpecifySingleKeyConstraint . index

-- | Get projection path proof object from constraint 'Key'.
projectionKey :: PersistableWidth ct => Key c r ct -> Pi r ct
projectionKey =  UnsafePi.definePi . index

-- | Unsafe. Make constraint key to add column phantom type
unsafeReturnKey :: SingleKeyConstraint c r -> Key c r ct
unsafeReturnKey =  unsafeDefineConstraintKey . C.index

-- -- | Unsafe. Make constraint key to add constraint phantom type
-- unsafeAppendConstraint :: Pi r ct -> Key c r ct
-- unsafeAppendConstraint =  unsafeDefineConstraintKey . leafIndex


-- | Map from table constraint into constraint 'Key'.
mapConstraint :: (SingleKeyConstraint c0 r -> SingleKeyConstraint c1 r)
              -> Key c0 r ct
              -> Key c1 r ct
mapConstraint f = unsafeReturnKey . f . tableConstraint

-- | Derive 'Unique' constraint 'Key' from 'Primary' constraint 'Key'
uniqueKey :: Key Primary r ct -> Key Unique r ct
uniqueKey  = mapConstraint C.unique

-- | Derive 'NotNull' constraint 'Key' from 'Primary' constraint 'Key'
notNullKey :: Key Primary r ct -> Key NotNull r ct
notNullKey =  mapConstraint C.notNull


-- | Constraint 'Key' inference interface.
class HasConstraintKey c r ct  where
  -- | Infer constraint key.
  constraintKey :: Key c r ct

-- | Inference rule of 'Unique' constraint 'Key'.
instance HasConstraintKey Primary r ct
         => HasConstraintKey Unique r ct  where
  constraintKey = uniqueKey constraintKey

-- | Inference rule of 'NotNull' constraint 'Key'.
instance HasConstraintKey Primary r ct
         => HasConstraintKey NotNull r ct  where
  constraintKey = notNullKey constraintKey
