{-# LANGUAGE FlexibleContexts #-}

module Database.Relational.Query.Derives (
  unique,

  primary', primary
  ) where

import Database.Record (PersistableWidth)
import Database.Relational.Query.Relation (Relation, PrimeRelation)
import Database.Relational.Query.Expr ((.=.))
import Database.Relational.Query.Projection (placeholder)
import Database.Relational.Query.Join (relation, inner, wheres, (!))
import Database.Relational.Query.Constraint
   (Key, Primary, Unique, projectionKey,
    HasConstraintKey(constraintKey))
import qualified Database.Relational.Query.Constraint as Constraint

unique :: PersistableWidth p
       => Key Unique a p
       -> Relation a
       -> PrimeRelation p a
unique uk rel = relation $ do
  q <- inner rel
  wheres $ q ! projectionKey uk .=. placeholder
  return q

primary' :: PersistableWidth p
         => Key Primary a p
         -> Relation a
         -> PrimeRelation p a
primary' pc = unique $ Constraint.uniqueKey pc

primary :: (PersistableWidth p, HasConstraintKey Primary a p)
        => Relation a
        -> PrimeRelation p a
primary = primary' constraintKey
