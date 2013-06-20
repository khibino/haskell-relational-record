{-# LANGUAGE FlexibleContexts #-}

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
  updateByConstraintKey,
  primaryUpdate
  ) where

import Database.Record (PersistableWidth)
import Database.Relational.Query.Table (Table)
import Database.Relational.Query.Pi (Pi)
import Database.Relational.Query.Projectable (placeholder, (.=.))
import Database.Relational.Query.ProjectableExtended ((!))
import Database.Relational.Query.Monad.Class (wheres)
import Database.Relational.Query.Relation (Relation, relation', query)
import Database.Relational.Query.Constraint
   (Key, Primary, Unique, projectionKey, uniqueKey,
    HasConstraintKey(constraintKey))
import qualified Database.Relational.Query.Constraint as Constraint
import Database.Relational.Query.Type (Update, typedUpdate)


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


-- | Typed 'Update' using specified key.
updateByConstraintKey :: Table r    -- ^ 'Table' to update
                      -> Key c r p  -- ^ Key with constraint 'c', record type 'r' and column type 'p'
                      -> Update p r -- ^ Result typed 'Update'
updateByConstraintKey table key = typedUpdate table (Constraint.indexes key)

-- | Typed 'Update' using infered primary key.
primaryUpdate :: (HasConstraintKey Primary r p)
              => Table r            -- ^ 'Table' to update
              -> Update p r -- ^ Result typed 'Update'
primaryUpdate table = updateByConstraintKey table (uniqueKey constraintKey)
