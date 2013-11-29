{-# LANGUAGE FlexibleContexts #-}
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
  MonadQualify (..), MonadRestrict (..),
  MonadQuery (..), MonadAggregate (..),

  onE, on, wheresE, wheres,
  groupBy, havingE, having
  ) where

import Database.Relational.Query.Context (Flat, Aggregated)
import Database.Relational.Query.Expr (Expr)
import Database.Relational.Query.Projection (Projection, AggregatedElements, unsafeToAggregatedElements)
import Database.Relational.Query.Projectable (expr)
import Database.Relational.Query.Sub (SubQuery, Qualified)

import Database.Relational.Query.Internal.Product (NodeAttr)

-- | Restrict context interface
class (Functor m, Monad m) => MonadRestrict c m where
  -- | Add restriction to this context.
  restrictContext :: Expr c (Maybe Bool) -- ^ 'Expr' 'Projection' which represent restriction
                  -> m ()                         -- ^ Restricted query context

-- | Query building interface.
class (Functor m, Monad m) => MonadQuery m where
  -- | Add restriction to last join.
  restrictJoin :: Expr Flat (Maybe Bool) -- ^ 'Expr' 'Projection' which represent restriction
               -> m ()                   -- ^ Restricted query context
  -- -- | Add restriction to this query.
  -- restrictQuery :: Expr Projection (Maybe Bool) -- ^ 'Expr' 'Projection' which represent restriction
  --               -> m ()                         -- ^ Restricted query context
  -- | Unsafely join subquery with this query.
  unsafeSubQuery :: NodeAttr              -- ^ Attribute maybe or just
                 -> Qualified SubQuery    -- ^ 'SubQuery' to join
                 -> m (Projection Flat r) -- ^ Result joined context and 'SubQuery' result projection.
  -- unsafeMergeAnotherQuery :: NodeAttr -> m (Projection r) -> m (Projection r)

-- | Lift interface from base qualify monad.
class (Functor q, Monad q, MonadQuery m) => MonadQualify q m where
  -- | Lift from qualify monad 'q' into 'MonadQuery' m.
  --   Qualify monad qualifies table form 'SubQuery'.
  liftQualify :: q a -> m a

-- | Aggregated query building interface extends 'MonadQuery'.
class MonadQuery m => MonadAggregate m where
  -- | Add /group by/ term into context and get aggregated projection.
  aggregateElement :: AggregatedElements a -- ^ Projection to add into group by
                   -> m a                  -- ^ Result context and aggregated projection

-- | Add restriction to last join.
onE :: MonadQuery m => Expr Flat (Maybe Bool) -> m ()
onE =  restrictJoin

-- | Add restriction to last join. Projection type version.
on :: MonadQuery m => Projection Flat (Maybe Bool) -> m ()
on =  restrictJoin . expr

-- | Add restriction to this query.
wheresE :: MonadRestrict Flat m => Expr Flat (Maybe Bool) -> m ()
wheresE =  restrictContext

-- | Add restriction to this query. Projection type version.
wheres :: MonadRestrict Flat m => Projection Flat (Maybe Bool) -> m ()
wheres =  restrictContext . expr

groupBy' :: MonadAggregate m
         => AggregatedElements a
         -> m a
groupBy' =  aggregateElement

-- | Add /GROUP BY/ term into context and get aggregated projection.
groupBy :: MonadAggregate m
        => Projection Flat r      -- ^ Projection to add into group by
        -> m (Projection Aggregated r) -- ^ Result context and aggregated projection
groupBy =  groupBy' . unsafeToAggregatedElements

-- | Add restriction to this aggregated query.
havingE :: MonadRestrict Aggregated m => Expr Aggregated (Maybe Bool) -> m ()
havingE =  restrictContext

-- | Add restriction to this aggregated query. Aggregated Projection type version.
having :: MonadRestrict Aggregated m => Projection Aggregated (Maybe Bool) -> m ()
having =  restrictContext . expr
