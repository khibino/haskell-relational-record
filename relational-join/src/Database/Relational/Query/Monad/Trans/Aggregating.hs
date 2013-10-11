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

import Database.Relational.Query.Context (Flat, Aggregated, Group, Cube)
import Database.Relational.Query.Component (AggregateTerm, AggregateTerms)
import Database.Relational.Query.Monad.Trans.AggregatingState
  (AggregatingContext, primeAggregatingContext, aggregateTerms, addGroupBy)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Class
  (MonadRestrict(..), MonadQuery(..), MonadAggregate(..), MonadCube(..))


-- | 'StateT' type to accumulate aggregating context.
newtype Aggregatings ac m a =
  Aggregatings { aggregatingState :: StateT AggregatingContext m a }
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Run 'Aggregatings' to expand context state.
runAggregating :: Aggregatings ac m a          -- ^ Context to expand
               -> AggregatingContext        -- ^ Initial context
               -> m (a, AggregatingContext) -- ^ Expanded result
runAggregating =  runStateT . aggregatingState

-- | Run 'Aggregatings' with primary empty context to expand context state.
runAggregatingPrime :: Aggregatings ac m a          -- ^ Context to expand
                    -> m (a, AggregatingContext) -- ^ Expanded result
runAggregatingPrime =  (`runAggregating` primeAggregatingContext)

-- | Lift to 'Aggregatings'.
aggregatings :: Monad m => m a -> Aggregatings ac m a
aggregatings =  lift

-- | Aggregated 'MonadRestrict'.
instance MonadRestrict c m => MonadRestrict c (Aggregatings Group m) where
  restrictContext =  aggregatings . restrictContext

-- | Aggregated 'MonadRestrict'. Has group by cube context.
instance MonadRestrict c m => MonadRestrict c (Aggregatings Cube m) where
  restrictContext =  aggregatings . restrictContext

-- | Aggregated 'MonadQuery'.
instance MonadQuery m => MonadQuery (Aggregatings Group m) where
  restrictJoin  =  aggregatings . restrictJoin
  unsafeSubQuery na = aggregatings . unsafeSubQuery na

-- | Aggregated 'MonadQuery'. Has group by cube context.
instance MonadQuery m => MonadQuery (Aggregatings Cube m) where
  restrictJoin  =  aggregatings . restrictJoin
  unsafeSubQuery na = aggregatings . unsafeSubQuery na

-- | Unsafely update aggregating context.
updateAggregatingContext :: Monad m => (AggregatingContext -> AggregatingContext) -> Aggregatings ac m ()
updateAggregatingContext =  Aggregatings . modify

-- | Unsafely add not-typeful aggregating terms.
addAggregating' :: Monad m => [AggregateTerm] -> Aggregatings ac m ()
addAggregating' gbs = updateAggregatingContext . foldr (>>>) id $ map addGroupBy gbs

aggregateWithProjection :: Monad m => Projection pc r -> Aggregatings ac m ()
aggregateWithProjection =  addAggregating' . Projection.columns

-- | Add aggregating terms.
addGroupBys :: MonadQuery m
            => Projection Flat r              -- ^ Group-by term to add
            -> Aggregatings ac m (Projection Aggregated r) -- ^ Result aggregated context
addGroupBys p = do
  aggregateWithProjection p
  return $ Projection.unsafeToAggregated p

-- | Aggregated query instance.
instance MonadQuery m => MonadAggregate (Aggregatings Group m) where
  aggregateKey = addGroupBys

-- | Add group-by-cube terms
addGroupByCubes :: MonadQuery m
                => Projection Flat r                                     -- ^ Cube term to add
                -> Aggregatings Cube m (Projection Aggregated (Maybe r)) -- ^ Result aggregated context
addGroupByCubes p = do
  aggregateWithProjection p
  return . Projection.unsafeToAggregated $ Projection.just p

-- | Aggregated query instance.
instance MonadQuery m => MonadCube (Aggregatings Cube m) where
  cubeKey = addGroupByCubes

-- | Run 'Aggregatings' to get 'AggregateTerms'.
extractAggregateTerms :: (Monad m, Functor m) => Aggregatings ac m a -> m (a, AggregateTerms)
extractAggregateTerms q = second aggregateTerms <$> runAggregatingPrime q
