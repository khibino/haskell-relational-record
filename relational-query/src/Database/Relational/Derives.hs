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

  uniqueSelect,
  primarySelect,

  -- * Update derivation
  updateByConstraintKey,
  primaryUpdate,

  updateValuesWithKey,

  -- * Derived objects from table
  derivedUniqueRelation,

  -- * Deprecated
  unique,
  primary', primary,
  ) where

import Database.Record (PersistableWidth, ToSql)
import Database.Record.ToSql (unsafeUpdateValuesWithIndexes)

import Database.Relational.SqlSyntax (Record)

import Database.Relational.Table (Table, TableDerivable)
import Database.Relational.Pi (Pi, expandIndexes)
import qualified Database.Relational.Record as Record
import Database.Relational.Projectable ((.=.), (!))
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
             => Pi a p        -- ^ Projection path
             -> Relation () a -- ^ 'Relation' to add restriction.
             -> Relation p a  -- ^ Result restricted 'Relation'
specifiedKey key rel = relation' $ \ph -> do
  q <- query rel
  wheres $ Record.wpi (relationWidth rel Record.pempty) q key .=. Record.toFlat ph
  return q

-- | Query restricted with specified unique key.
uniqueSelect :: PersistableWidth p
             => Key Unique a p -- ^ Unique key proof object which record type is 'a' and key type is 'p'.
             -> Relation () a  -- ^ 'Relation' to add restriction.
             -> Relation p a   -- ^ Result restricted 'Relation'
uniqueSelect =  specifiedKey . projectionKey

{-# DEPRECATED unique "use `uniqueSelect` instead of this." #-}
-- | Deprecated.
unique :: PersistableWidth p
       => Key Unique a p
       -> Relation () a
       -> Relation p a
unique = uniqueSelect

{-# DEPRECATED primary' "use `primarySelect` instead of this." #-}
-- | Deprecated.
primary' :: PersistableWidth p
         => Key Primary a p -- ^ Primary key proof object which record type is 'a' and key type is 'p'.
         -> Relation () a   -- ^ 'Relation' to add restriction.
         -> Relation p a    -- ^ Result restricted 'Relation'
primary' =  specifiedKey . projectionKey

-- | Query restricted with inferred primary key.
primarySelect :: HasConstraintKey Primary a p
              => Relation () a -- ^ 'Relation' to add restriction.
              -> Relation p a  -- ^ Result restricted 'Relation'
primarySelect =  primary' constraintKey

{-# DEPRECATED primary "use `primarySelect` instead of this." #-}
-- | Deprecated.
primary :: HasConstraintKey Primary a p
        => Relation () a
        -> Relation p a
primary = primarySelect

-- | Convert from Haskell type `r` into SQL value `q` list expected by update form like
--
-- /UPDATE <table> SET c0 = ?, c1 = ?, ..., cn = ? WHERE key0 = ? AND key1 = ? AND key2 = ? ... /
--
--   using derived 'RecordToSql' proof object.
updateValuesWithKey :: ToSql q r
                    => Pi r p
                    -> r
                    -> [q]
updateValuesWithKey =  unsafeUpdateValuesWithIndexes . expandIndexes

-- | Typed 'KeyUpdate' using specified constraint key.
updateByConstraintKey :: PersistableWidth p
                      => Table r       -- ^ 'Table' to update
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
                      -> Record c k            -- ^ Unique key value to specify.
                      -> UniqueRelation () c r -- ^ Result restricted 'Relation'
derivedUniqueRelation uk kp = unsafeUnique . relation $ do
  r <- query derivedRelation
  wheres $ r ! projectionKey uk .=. Record.unsafeChangeContext kp
  return r
