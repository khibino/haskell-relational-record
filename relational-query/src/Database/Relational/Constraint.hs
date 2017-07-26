{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Constraint
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides proof object definitions of constraint key.
-- Difference between this module and "Database.Record.KeyConstraint" is
-- typed constraint key column definition is included in this module.
module Database.Relational.Constraint (
  -- * Constraint Key proof object
  Key, indexes, unsafeDefineConstraintKey,
  tableConstraint, projectionKey,

  -- unsafeReturnKey, -- unsafeAppendConstraint,

  -- * Derivation rules
  uniqueKey, -- notNullKey,

  -- * Inference rules
  HasConstraintKey (..),
  derivedUniqueKey, -- derivedNotNullKey,

  -- * Constraint types
  Primary, Unique, NotNull
  ) where


import Database.Record (PersistableRecordWidth, PersistableWidth (persistableWidth))
import Database.Record.KeyConstraint
  (KeyConstraint, unsafeSpecifyKeyConstraint,
   Primary, Unique, NotNull)
import qualified Database.Record.KeyConstraint as C

import Database.Relational.Pi (Pi)
import qualified Database.Relational.Pi.Unsafe as UnsafePi


-- | Constraint Key proof object. Constraint type 'c', record type 'r' and columns type 'ct'.
data Key c r ct = Key [Int] (PersistableRecordWidth ct)

-- | Index of key which specifies constraint key.
indexes :: Key c r ct -> [Int]
indexes (Key is _) = is

-- | Width of key.
width :: Key c r ct -> PersistableRecordWidth ct
width (Key _ w) = w

-- | Unsafely generate constraint 'Key' proof object using specified key index.
unsafeDefineConstraintKey :: PersistableWidth ct
                          => [Int]      -- ^ Key indexes which specify this constraint key
                          -> Key c r ct -- ^ Result constraint key proof object
unsafeDefineConstraintKey ixs = Key ixs persistableWidth

-- | Get table constraint 'KeyConstraint' proof object from constraint 'Key'.
tableConstraint :: Key c r ct -> KeyConstraint c r
tableConstraint =  unsafeSpecifyKeyConstraint . indexes

-- | Get projection path proof object from constraint 'Key'.
projectionKey :: Key c r ct -> Pi r ct
projectionKey k =  UnsafePi.defineDirectPi' w ixs  where
  ixs = indexes k
  w   = width k

-- | Unsafe. Make constraint key to add column phantom type
unsafeReturnKey :: PersistableWidth ct
                => KeyConstraint c r -> Key c r ct
unsafeReturnKey =  unsafeDefineConstraintKey . C.indexes

-- -- | Unsafe. Make constraint key to add constraint phantom type
-- unsafeAppendConstraint :: Pi r ct -> Key c r ct
-- unsafeAppendConstraint =  unsafeDefineConstraintKey . leafIndex


-- | Map from table constraint into constraint 'Key'.
mapConstraint :: PersistableWidth ct
              => (KeyConstraint c0 r -> KeyConstraint c1 r)
              -> Key c0 r ct
              -> Key c1 r ct
mapConstraint f = unsafeReturnKey . f . tableConstraint

-- | Derive 'Unique' constraint 'Key' from 'Primary' constraint 'Key'
uniqueKey :: PersistableWidth ct
          => Key Primary r ct -> Key Unique r ct
uniqueKey  = mapConstraint C.unique

-- -- | Derive 'NotNull' constraint 'Key' from 'Primary' constraint 'Key'
-- notNullKey :: Key Primary r ct -> Key NotNull r ct
-- notNullKey =  mapConstraint C.notNull


-- | Constraint 'Key' inference interface.
class PersistableWidth ct => HasConstraintKey c r ct  where
  -- | Infer constraint key.
  constraintKey :: Key c r ct

-- | Inferred 'Unique' constraint 'Key'.
--   Record type 'r' has unique key which type is 'ct' derived from primay key.
derivedUniqueKey :: HasConstraintKey Primary r ct => Key Unique r ct
derivedUniqueKey =  uniqueKey constraintKey

-- -- | Inferred 'NotNull' constraint 'Key'.
-- --   Record type 'r' has not-null key which type is 'ct' derived from primay key.
-- derivedNotNullKey :: HasConstraintKey Primary r ct => Key NotNull r ct
-- derivedNotNullKey =  notNullKey constraintKey
