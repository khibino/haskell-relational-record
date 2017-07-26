{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Derives
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines typed SQLs derived from type informations.
module Database.Relational.Derives (
  -- * Query derivation
  specifiedKey,

  unique,

  primary', primary,

  -- * Update derivation
  updateByConstraintKey,
  primaryUpdate,

  updateValuesWithKey,

  -- * Derived objects from table
  derivedUniqueRelation
  ) where

import Database.Record (PersistableWidth, ToSql (recordToSql))
import Database.Record.ToSql (unsafeUpdateValuesWithIndexes)

import Database.Relational.Table (Table, TableDerivable)
import Database.Relational.Pi.Unsafe (Pi, unsafeExpandIndexes)
import Database.Relational.Projection (Projection)
import qualified Database.Relational.Projection as Projection
import Database.Relational.Projectable (placeholder, (.=.))
import Database.Relational.ProjectableExtended ((!))
import Database.Relational.Monad.Class (wheres)
import Database.Relational.Monad.BaseType (Relation, relationWidth)
import Database.Relational.Relation
  (derivedRelation, relation, relation', query, UniqueRelation, unsafeUnique)
import Database.Relational.Constraint
   (Key, Primary, Unique, projectionKey, uniqueKey,
    HasConstraintKey(constraintKey))
import qualified Database.Relational.Constraint as Constraint
import Database.Relational.Type (KeyUpdate, typedKeyUpdate)


-- | Query restricted with specified key.
specifiedKey :: PersistableWidth p
      => Pi a p        -- ^ Unique key proof object which record type is 'a' and key type is 'p'.
      -> Relation () a -- ^ 'Relation' to add restriction.
      -> Relation p a  -- ^ Result restricted 'Relation'
specifiedKey key rel = relation' $ do
  q <- query rel
  (param, ()) <- placeholder (\ph -> wheres $ Projection.wpi (relationWidth rel) q key .=. ph)
  return (param, q)

-- | Query restricted with specified unique key.
unique :: PersistableWidth p
       => Key Unique a p -- ^ Unique key proof object which record type is 'a' and key type is 'p'.
       -> Relation () a  -- ^ 'Relation' to add restriction.
       -> Relation p a   -- ^ Result restricted 'Relation'
unique =  specifiedKey . projectionKey

-- | Query restricted with specified primary key.
primary' :: PersistableWidth p
         => Key Primary a p -- ^ Primary key proof object which record type is 'a' and key type is 'p'.
         -> Relation () a   -- ^ 'Relation' to add restriction.
         -> Relation p a    -- ^ Result restricted 'Relation'
primary' =  specifiedKey . projectionKey

-- | Query restricted with inferred primary key.
primary :: HasConstraintKey Primary a p
        => Relation () a -- ^ 'Relation' to add restriction.
        -> Relation p a  -- ^ Result restricted 'Relation'
primary =  primary' constraintKey


-- | Convert from Haskell type `r` into SQL value `q` list expected by update form like
--
-- /UPDATE <table> SET c0 = ?, c1 = ?, ..., cn = ? WHERE key0 = ? AND key1 = ? AND key2 = ? ... /
--
--   using derived 'RecordToSql' proof object.
updateValuesWithKey :: ToSql q r
                    => Pi r p
                    -> r
                    -> [q]
updateValuesWithKey =  unsafeUpdateValuesWithIndexes recordToSql . unsafeExpandIndexes

-- | Typed 'KeyUpdate' using specified constraint key.
updateByConstraintKey :: Table r       -- ^ 'Table' to update
                      -> Key c r p     -- ^ Key with constraint 'c', record type 'r' and columns type 'p'
                      -> KeyUpdate p r -- ^ Result typed 'Update'
updateByConstraintKey table' = typedKeyUpdate table' . Constraint.projectionKey

-- | Typed 'KeyUpdate' using inferred primary key.
primaryUpdate :: (HasConstraintKey Primary r p)
              => Table r       -- ^ 'Table' to update
              -> KeyUpdate p r -- ^ Result typed 'Update'
primaryUpdate table' = updateByConstraintKey table' (uniqueKey constraintKey)

-- | 'UniqueRelation' inferred from table.
derivedUniqueRelation :: TableDerivable r
                      => Key Unique r k        -- ^ Unique key proof object which record type is 'a' and key type is 'p'.
                      -> Projection c k        -- ^ Unique key value to specify.
                      -> UniqueRelation () c r -- ^ Result restricted 'Relation'
derivedUniqueRelation uk kp = unsafeUnique . relation $ do
  r <- query derivedRelation
  wheres $ r ! projectionKey uk .=. Projection.unsafeChangeContext kp
  return r
