{-# LANGUAGE FlexibleContexts, Rank2Types #-}

-- |
-- Module      : Database.Relational.Query.Derives
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines typed SQLs derived from type informations.
module Database.Relational.Query.Derives (
  -- * Query derivation
  specifiedKey,

  unique,

  primary', primary,

  -- * Update derivation
  updateBySpecifiedKey,
  updateByConstraintKey,
  primaryUpdate,

  updateValuesWithKey,

  -- * Derived objects from table
  TableDerivation (..),
  specifyTableDerivation', specifyTableDerivation,

  TableDerivable (..),
  derivedTable, derivedRelation, derivedInsert, DerivedInsertQuery, derivedInsertQuery,

  derivedUniqueRelation
  ) where

import Database.Record (PersistableWidth, ToSql (recordToSql))
import Database.Record.ToSql (unsafeUpdateValuesWithIndexes)
import Database.Relational.Query.Table (Table)
import Database.Relational.Query.Pi.Unsafe (Pi, unsafeExpandIndexes)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable (placeholder, (.=.))
import Database.Relational.Query.ProjectableExtended ((!))
import Database.Relational.Query.Monad.Class (wheres)
import Database.Relational.Query.Relation
  (Relation, relation, relation', query, table, UniqueRelation, unsafeUnique)
import Database.Relational.Query.Constraint
   (Key, Primary, Unique, projectionKey, uniqueKey,
    HasConstraintKey(constraintKey))
import qualified Database.Relational.Query.Constraint as Constraint
import Database.Relational.Query.Type
  (KeyUpdate, typedKeyUpdate, Insert, typedInsert, InsertQuery, typedInsertQuery)


-- | Query restricted with specified key.
specifiedKey :: PersistableWidth p
      => Pi a p        -- ^ Unique key proof object which record type is 'a' and key type is 'p'.
      -> Relation () a -- ^ 'Relation' to add restriction.
      -> Relation p a  -- ^ Result restricted 'Relation'
specifiedKey key rel = relation' $ do
  q <- query rel
  (param, ()) <- placeholder (\ph -> wheres $ q ! key .=. ph)
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

-- | Query restricted with infered primary key.
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

-- | Typed 'KeyUpdate' using specified key.
updateBySpecifiedKey :: Table r       -- ^ 'Table' to update
                     -> Pi r p        -- ^ Key with record type 'r' and columns type 'p'
                     -> KeyUpdate p r -- ^ Result typed 'Update'
updateBySpecifiedKey table' = typedKeyUpdate table'

-- | Typed 'KeyUpdate' using specified constraint key.
updateByConstraintKey :: Table r       -- ^ 'Table' to update
                      -> Key c r p     -- ^ Key with constraint 'c', record type 'r' and columns type 'p'
                      -> KeyUpdate p r -- ^ Result typed 'Update'
updateByConstraintKey table' = updateBySpecifiedKey table' . Constraint.projectionKey

-- | Typed 'KeyUpdate' using infered primary key.
primaryUpdate :: (HasConstraintKey Primary r p)
              => Table r       -- ^ 'Table' to update
              -> KeyUpdate p r -- ^ Result typed 'Update'
primaryUpdate table' = updateByConstraintKey table' (uniqueKey constraintKey)


-- | Type for insert qeury.
type DerivedInsertQuery r = forall p . Relation p r -> InsertQuery p

-- | Capabilities derived from table.
data TableDerivation r =
  TableDerivation
  { derivedTable'       :: Table r
  , derivedRelation'    :: Relation () r
  , derivedInsert'      :: Insert r
  , derivedInsertQuery' :: DerivedInsertQuery r
  }

-- | Specify properties derived from table.
specifyTableDerivation' :: Table r
                        -> Relation () r
                        -> Insert r
                        -> DerivedInsertQuery r
                        -> TableDerivation r
specifyTableDerivation' =  TableDerivation

-- | Specify properties derived from table.
specifyTableDerivation :: Table r -> TableDerivation r
specifyTableDerivation t = specifyTableDerivation' t (table t) (typedInsert t) (typedInsertQuery t)

-- | Inference rule for 'TableDerivation'.
class TableDerivable r where
  tableDerivation :: TableDerivation r

-- | Infered 'Table'.
derivedTable :: TableDerivable r => Table r
derivedTable =  derivedTable' tableDerivation

-- | Infered 'Relation'.
derivedRelation :: TableDerivable r => Relation () r
derivedRelation =  derivedRelation' tableDerivation

-- | Infered 'Insert'.
derivedInsert :: TableDerivable r => Insert r
derivedInsert =  derivedInsert' tableDerivation

-- | Infered 'Insert'.
derivedInsertQuery :: TableDerivable r => DerivedInsertQuery r
derivedInsertQuery =  derivedInsertQuery' tableDerivation

-- | 'UniqueRelation' infered from table.
derivedUniqueRelation :: TableDerivable r
                      => Key Unique r k        -- ^ Unique key proof object which record type is 'a' and key type is 'p'.
                      -> Projection c k        -- ^ Unique key value to specify.
                      -> UniqueRelation () c r -- ^ Result restricted 'Relation'
derivedUniqueRelation uk kp = unsafeUnique . relation $ do
  r <- query derivedRelation
  wheres $ r ! projectionKey uk .=. Projection.unsafeChangeContext kp
  return r
