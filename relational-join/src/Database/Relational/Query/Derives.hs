{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Query.Type
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines typed SQLs derived from type informations.
module Database.Relational.Query.Derives (
  unique,

  primary', primary,

  updateByConstraintKey,
  primaryUpdate
  ) where

import Database.Record (PersistableWidth)
import Database.Relational.Query.Table (Table)
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Projectable (placeholder, (.=.))
import Database.Relational.Query.ProjectableExtended ((!))
import Database.Relational.Query.Monad.Class (wheres)
import Database.Relational.Query.Relation (Relation, relation', query)
import Database.Relational.Query.Constraint
   (Key, Primary, Unique, projectionKey, uniqueKey,
    HasConstraintKey(constraintKey))
import qualified Database.Relational.Query.Constraint as Constraint
import Database.Relational.Query.Type (Update, typedSingleKeyUpdate)


-- | Query restricted with specified unique key.
unique :: PersistableWidth p
       => Key Unique a p -- ^ Unique key proof object which record type is 'a' and key type is 'p'.
       -> Relation () a  -- ^ 'Relation' to add restriction.
       -> Relation p a   -- ^ Result restricted 'Relation'
unique uk rel = relation' $ do
  q <- query rel
  (param, ()) <- placeholder (\ph -> wheres $ q ! projectionKey uk .=. ph)
  return (param, q)

-- | Query restricted with specified primary key.
primary' :: PersistableWidth p
         => Key Primary a p -- ^ Primary key proof object which record type is 'a' and key type is 'p'.
         -> Relation () a   -- ^ 'Relation' to add restriction.
         -> Relation p a    -- ^ Result restricted 'Relation'
primary' pc = unique $ Constraint.uniqueKey pc

-- | Query restricted with infered primary key.
primary :: (PersistableWidth p, HasConstraintKey Primary a p)
        => Relation () a -- ^ 'Relation' to add restriction.
        -> Relation p a  -- ^ Result restricted 'Relation'
primary = primary' constraintKey


-- | Typed 'Update' using specified key.
updateByConstraintKey :: PersistableWidth p
                      => Table r    -- ^ 'Table' to update
                      -> Key c r p  -- ^ Key with constraint 'c', record type 'r' and column type 'p'
                      -> Update p r -- ^ Result typed 'Update'
updateByConstraintKey table key =
  typedSingleKeyUpdate table (table `Table.index` Constraint.index key)

-- | Typed 'Update' using infered primary key.
primaryUpdate :: (PersistableWidth p, HasConstraintKey Primary r p)
              => Table r   -- ^ 'Table' to update
                 -> Update p r -- ^ Result typed 'Update'
primaryUpdate table = updateByConstraintKey table (uniqueKey constraintKey)
