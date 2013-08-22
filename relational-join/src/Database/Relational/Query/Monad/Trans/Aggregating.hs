{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Query.Monad.Trans.Aggregating
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift
-- from 'MonadQuery' into Aggregated query.
module Database.Relational.Query.Monad.Trans.Aggregating (
  -- * Transformer into aggregated query
  Aggregatings, aggregatings,

  -- * Result group by SQLs
  GroupBysPrepend, extractGroupBys, prependGroupBys
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, runStateT, modify)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (second, (>>>))

import Database.Relational.Query.Monad.Trans.StatePrepend (Prepend, prepend, liftToString)
import Database.Relational.Query.Monad.Trans.AggregatingState
  (AggregatingContext, primeAggregatingContext, addGroupBy, composeGroupBys)
import qualified Database.Relational.Query.Monad.Trans.AggregatingState as State
import Database.Relational.Query.Expr (Expr)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Aggregation (Aggregation)
import qualified Database.Relational.Query.Aggregation as Aggregation

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
aggregatings :: Monad m => m a -> Aggregatings m a
aggregatings =  lift

-- | Aggregated 'MonadRestrict'.
instance MonadRestrict m => MonadRestrict (Aggregatings m) where
  restrictContext =  aggregatings . restrictContext

-- | Aggregated 'MonadQuery'.
instance MonadQuery m => MonadQuery (Aggregatings m) where
  restrictJoin  =  aggregatings . restrictJoin
  unsafeSubQuery na = aggregatings . unsafeSubQuery na

-- | Unsafely update aggregating context.
updateAggregatingContext :: Monad m => (AggregatingContext -> AggregatingContext) -> Aggregatings m ()
updateAggregatingContext =  Aggregatings . modify

-- | Unsafely add not-typeful aggregating terms.
addGroupBys' :: Monad m => [String] -> Aggregatings m ()
addGroupBys' gbs = updateAggregatingContext . foldr (>>>) id $ map addGroupBy gbs

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

-- | GROUP BY terms prepending function.
type GroupBysPrepend = Prepend AggregatingContext

-- | Run 'Aggregatings' to get GROUP BY terms prepending function.
extractGroupBys :: MonadQuery m
                => Aggregatings m a      -- ^ 'Aggregatings' to run
                -> m (a, GroupBysPrepend) -- ^ GROUP BY terms prepending function.
extractGroupBys q = second (liftToString composeGroupBys) <$> runAggregatingPrime q

-- | Run GROUP BY terms prepend.
prependGroupBys :: GroupBysPrepend -> String -> String
prependGroupBys =  prepend
