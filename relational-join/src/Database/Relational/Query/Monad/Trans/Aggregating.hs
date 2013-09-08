{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

  -- * Result
  extractAggregateTerms
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, runStateT, modify)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (second, (>>>))

import Database.Relational.Query.Context (Flat, Aggregated)
import Database.Relational.Query.Sub (AggregateTerm, AggregateTerms)
import Database.Relational.Query.Monad.Trans.AggregatingState
  (AggregatingContext, primeAggregatingContext, aggregateTerms, addGroupBy)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

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
instance MonadRestrict c m => MonadRestrict c (Aggregatings m) where
  restrictContext =  aggregatings . restrictContext

-- | Aggregated 'MonadQuery'.
instance MonadQuery m => MonadQuery (Aggregatings m) where
  restrictJoin  =  aggregatings . restrictJoin
  unsafeSubQuery na = aggregatings . unsafeSubQuery na

-- | Unsafely update aggregating context.
updateAggregatingContext :: Monad m => (AggregatingContext -> AggregatingContext) -> Aggregatings m ()
updateAggregatingContext =  Aggregatings . modify

-- | Unsafely add not-typeful aggregating terms.
addGroupBys' :: Monad m => [AggregateTerm] -> Aggregatings m ()
addGroupBys' gbs = updateAggregatingContext . foldr (>>>) id $ map addGroupBy gbs

-- | Add aggregating terms.
addGroupBys :: MonadQuery m
            => Projection Flat r              -- ^ Group-by term to add
            -> Aggregatings m (Projection Aggregated r) -- ^ Result aggregated context
addGroupBys p = do
  addGroupBys' . Projection.columns $ p
  return $ Projection.unsafeToAggregated p

-- | Aggregated query instance.
instance MonadQuery m => MonadAggregate (Aggregatings m) where
  aggregateKey = addGroupBys

-- | Run 'Aggregatings' to get 'AggregateTerms'.
extractAggregateTerms :: (Monad m, Functor m) => Aggregatings m a -> m (a, AggregateTerms)
extractAggregateTerms q = second aggregateTerms <$> runAggregatingPrime q
