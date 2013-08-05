{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Query.Monad.Trans.Aggregate
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift
-- from 'MonadQuery' into Aggregated query.
module Database.Relational.Query.Monad.Trans.Aggregate (
  -- * Transformer into aggregated query
  Aggregatings, aggregate,

  -- * Result group by SQLs
  appendGroupBys
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, runStateT, modify)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (second)

import Database.Relational.Query.Expr (Expr)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Aggregation (Aggregation)
import qualified Database.Relational.Query.Aggregation as Aggregation

import Database.Relational.Query.Monad.Trans.AggregateState
  (AggregatingContext, primeAggregatingContext, addGroupBy, composeGroupBys)
import qualified Database.Relational.Query.Monad.Trans.AggregateState as State

import Database.Relational.Query.Monad.Class
  (MonadRestrict(..), MonadQuery(..), MonadAggregate(..))


-- | 'StateT' type to accumulate aggregating context.
newtype Aggregatings m a =
  Aggregatings { aggregatingState :: StateT AggregatingContext m a }
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Run 'Aggregatings' to expand context state.
runAggregating :: Aggregatings m a          -- ^ Context to expand
               -> AggregatingContext        -- ^ Initial context
               -> m (a, AggregatingContext) -- ^ Expanded result
runAggregating =  runStateT . aggregatingState

-- | Run 'Aggregatings' with primary empty context to expand context state.
runAggregatingPrime :: Aggregatings m a          -- ^ Context to expand
                    -> m (a, AggregatingContext) -- ^ Expanded result
runAggregatingPrime =  (`runAggregating` primeAggregatingContext)

-- | Lift to 'Aggregatings'.
aggregate :: Monad m => m a -> Aggregatings m a
aggregate =  lift

-- | Aggregated 'MonadRestrict'.
instance MonadRestrict m => MonadRestrict (Aggregatings m) where
  restrictContext =  aggregate . restrictContext

-- | Aggregated 'MonadQuery'.
instance MonadQuery m => MonadQuery (Aggregatings m) where
  restrictJoin  =  aggregate . restrictJoin
  unsafeSubQuery na = aggregate . unsafeSubQuery na

-- | Unsafely update aggregating context.
updateAggregatingContext :: Monad m => (AggregatingContext -> AggregatingContext) -> Aggregatings m ()
updateAggregatingContext =  Aggregatings . modify

-- | Unsafely add not-typeful aggregating terms.
addGroupBys' :: Monad m => [String] -> Aggregatings m ()
addGroupBys' gbs = updateAggregatingContext (\c -> foldl (flip addGroupBy) c gbs)

-- | Add restrictions for aggregated query.
addRestriction :: MonadQuery m
               => Expr Aggregation (Maybe Bool) -- ^ Restriction to add
               -> Aggregatings m ()             -- ^ Result restricted context
addRestriction =  updateAggregatingContext . State.addRestriction

-- | Add aggregating terms.
addGroupBys :: MonadQuery m
            => Projection r                   -- ^ Group-by term to add
            -> Aggregatings m (Aggregation r) -- ^ Result aggregated context
addGroupBys p = do
  addGroupBys' . Projection.columns $ p
  return $ Aggregation.unsafeFromProjection p

-- | Aggregated query instance.
instance MonadQuery m => MonadAggregate (Aggregatings m) where
  aggregateKey = addGroupBys
  restrictAggregatedQuery = addRestriction


-- | Get group-by appending function from 'AggregatingContext'.
appendGroupBys' :: AggregatingContext -> String -> String
appendGroupBys' c = (++ d (composeGroupBys c))  where
  d "" = ""
  d s  = ' ' : s

-- | Run 'Aggregatings' to get query result and group-by appending function.
appendGroupBys :: MonadQuery m
               => Aggregatings m a        -- ^ 'Aggregatings' to run
               -> m (a, String -> String) -- ^ Query result and group-by appending function.
appendGroupBys q = second appendGroupBys' <$> runAggregatingPrime q
