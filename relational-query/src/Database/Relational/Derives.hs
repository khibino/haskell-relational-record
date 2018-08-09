{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}

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

import Database.Relational.ExtensibleRecord
import Database.Relational.ReboundSyntax
import Database.Relational.Table (Table, TableDerivable)
import Database.Relational.Pi.Unsafe (Pi, unsafeExpandIndexes)
import qualified Database.Relational.Record as Record
import Database.Relational.Projectable (placeholder, (.=.), (!))
import Database.Relational.Monad.Trans.Placeholders (wheres)
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
             -> Relation (ExRecord '[]) (ExRecord ys) () a -- ^ 'Relation' to add restriction.
             -> Relation (ExRecord '[]) (ExRecord ys) p a  -- ^ Result restricted 'Relation'
specifiedKey key rel = relation' $ do
  q <- query rel
  -- igrep TODO
  (param, ()) <- undefined -- placeholder (\ph -> wheres $ Record.wpi (relationWidth rel) q key .=. ph)
  ireturn (param, q)

-- | Query restricted with specified unique key.
uniqueSelect :: PersistableWidth p
             => Key Unique a p -- ^ Unique key proof object which record type is 'a' and key type is 'p'.
             -> Relation (ExRecord '[]) (ExRecord ys) () a  -- ^ 'Relation' to add restriction.
             -> Relation (ExRecord '[]) (ExRecord ys) p a   -- ^ Result restricted 'Relation'
uniqueSelect =  specifiedKey . projectionKey

{-# DEPRECATED unique "use `uniqueSelect` instead of this." #-}
-- | Deprecated.
unique :: PersistableWidth p
       => Key Unique a p
       -> Relation (ExRecord '[]) (ExRecord ys) () a
       -> Relation (ExRecord '[]) (ExRecord ys) p a
unique = uniqueSelect

{-# DEPRECATED primary' "use `primarySelect` instead of this." #-}
-- | Deprecated.
primary' :: PersistableWidth p
         => Key Primary a p -- ^ Primary key proof object which record type is 'a' and key type is 'p'.
         -> Relation (ExRecord '[]) (ExRecord ys) () a   -- ^ 'Relation' to add restriction.
         -> Relation (ExRecord '[]) (ExRecord ys) p a    -- ^ Result restricted 'Relation'
primary' =  specifiedKey . projectionKey

-- | Query restricted with inferred primary key.
primarySelect :: HasConstraintKey Primary a p
              => Relation (ExRecord '[]) (ExRecord ys) () a -- ^ 'Relation' to add restriction.
              -> Relation (ExRecord '[]) (ExRecord ys) p a  -- ^ Result restricted 'Relation'
primarySelect =  primary' constraintKey

{-# DEPRECATED primary "use `primarySelect` instead of this." #-}
-- | Deprecated.
primary :: HasConstraintKey Primary a p
        => Relation (ExRecord '[]) (ExRecord ys) () a
        -> Relation (ExRecord '[]) (ExRecord ys) p a
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
updateValuesWithKey =  unsafeUpdateValuesWithIndexes . unsafeExpandIndexes

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
                      => Key Unique r k            -- ^ Unique key proof object which record type is 'a' and key type is 'p'.
                      -> Record (ExRecord '[]) (ExRecord '[]) c k            -- ^ Unique key value to specify.
                      -> UniqueRelation (ExRecord '[]) (ExRecord '[]) () c r -- ^ Result restricted 'Relation'
                      -- -> Record (ExRecord '[]) (ExRecord ys) c k            -- ^ Unique key value to specify.
                      -- -> UniqueRelation (ExRecord '[]) (ExRecord '[]) () c r -- ^ Result restricted 'Relation'
derivedUniqueRelation uk kp = unsafeUnique . relation $ do
  r <- query derivedRelation
  -- igrep TODO: abolish `Pi` for narrowing record.
  wheres $ r ! projectionKey uk .=. Record.unsafeChangeContext kp
  ireturn r
