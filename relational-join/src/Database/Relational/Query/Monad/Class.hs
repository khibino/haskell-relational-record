{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Query.Monad.Class
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines query building interface classes.
module Database.Relational.Query.Monad.Class (
  -- * Query interface classes
  MonadQualify (..),
  MonadQuery (..), MonadAggregate (..),

  on', wheres', having'
  ) where

import Database.Relational.Query.Expr (Expr)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Aggregation (Aggregation)
import Database.Relational.Query.Projectable (expr)
import Database.Relational.Query.Sub (SubQuery, Qualified)

import Database.Relational.Query.Internal.Product (NodeAttr)

-- | Query building interface.
class (Functor m, Monad m) => MonadQuery m where
  -- | Add restriction to last join.
  on :: Expr Projection (Maybe Bool) -- ^ 'Expr' 'Projection' which represent restriction
     -> m ()                         -- ^ Restricted query context
  -- | Add restriction to this query.
  wheres :: Expr Projection (Maybe Bool) -- ^ 'Expr' 'Projection' which represent restriction
         -> m ()              -- ^ Restricted query context
  -- | Unsafely join subquery with this query.
  unsafeSubQuery :: NodeAttr           -- ^ Attribute maybe or just
                 -> Qualified SubQuery -- ^ 'SubQuery' to join
                 -> m (Projection r)   -- ^ Result joined context and 'SubQuery' result projection.
  -- unsafeMergeAnotherQuery :: NodeAttr -> m (Projection r) -> m (Projection r)

-- | Lift interface from base qualify monad.
class (Functor q, Monad q, MonadQuery m) => MonadQualify q m where
  -- | Lift from qualify monad 'q' into 'MonadQuery' m.
  --   Qualify monad qualifies table form 'SubQuery'.
  liftQualify :: q a -> m a

-- | Aggregated query building interface extends 'MonadQuery'.
class MonadQuery m => MonadAggregate m where
  -- | Add /group by/ term into context and get aggregated projection.
  groupBy :: Projection r      -- ^ Projection to add into group by
          -> m (Aggregation r) -- ^ Result context and aggregated projection
  -- | Add restriction to this aggregated query.
  having :: Expr Aggregation (Maybe Bool) -- ^ 'Expr' 'Aggregation' which represent restriction
         -> m ()                          -- ^ Restricted query context

on' :: MonadQuery m => Projection (Maybe Bool) -> m ()
on' =  on . expr

wheres' :: MonadQuery m => Projection (Maybe Bool) -> m ()
wheres' =  wheres . expr

having' :: MonadAggregate m => Aggregation (Maybe Bool) -> m ()
having' =  having . expr
