module Database.Relational.Query.Monad.Aggregate (
  aggregate,

  appendGroupBys,

  QueryAggregate,
  AggregatedQuery,

  toSQL,

  toSubQuery
  ) where

import Control.Monad (liftM, ap)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, runStateT, modify)
import Control.Applicative (Applicative (pure, (<*>)), (<$>))
import Control.Arrow (second)

import Database.Relational.Query.Expr (Expr)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable (projectAggregation)
import Database.Relational.Query.Aggregation (Aggregation)
import qualified Database.Relational.Query.Aggregation as Aggregation
import Database.Relational.Query.Sub (SubQuery, subQuery)

import Database.Relational.Query.Internal.AggregatingContext (AggregatingContext, primeAggregatingContext)
import qualified Database.Relational.Query.Internal.AggregatingContext as Context

import Database.Relational.Query.Monad.Unsafe (UnsafeMonadQuery(unsafeSubQuery))
import Database.Relational.Query.Monad.Class (MonadQuery, MonadAggregate)
import qualified Database.Relational.Query.Monad.Class as MonadQuery
import Database.Relational.Query.Monad.Ordering (Orderings, OrderedQuery)
import qualified Database.Relational.Query.Monad.Ordering as Ordering
import Database.Relational.Query.Monad.Core (QueryCore)
import qualified Database.Relational.Query.Monad.Core as Core

newtype Aggregatings m a =
  Aggregatings { aggregatingState :: StateT AggregatingContext m a }

runAggregating :: Aggregatings m a -> AggregatingContext -> m (a, AggregatingContext)
runAggregating =  runStateT . aggregatingState

runAggregatingPrime :: Aggregatings m a -> m (a, AggregatingContext)
runAggregatingPrime =  (`runAggregating` primeAggregatingContext)

instance MonadTrans Aggregatings where
  lift = Aggregatings . lift

aggregate :: Monad m => m a -> Aggregatings m a
aggregate =  lift

instance Monad m => Monad (Aggregatings m) where
  return     = Aggregatings . return
  q0 >>= f   = Aggregatings $ aggregatingState q0 >>= aggregatingState . f

instance Monad m => Functor (Aggregatings m) where
  fmap = liftM

instance Monad m => Applicative (Aggregatings m) where
  pure  = return
  (<*>) = ap

instance UnsafeMonadQuery m => UnsafeMonadQuery (Aggregatings m) where
  unsafeSubQuery na = aggregate . unsafeSubQuery na

instance MonadQuery m => MonadQuery (Aggregatings m) where
  on     =  aggregate . MonadQuery.on
  wheres =  aggregate . MonadQuery.wheres

updateAggregatingContext :: Monad m => (AggregatingContext -> AggregatingContext) -> Aggregatings m ()
updateAggregatingContext =  Aggregatings . modify

addGroupBys' :: Monad m => [String] -> Aggregatings m ()
addGroupBys' gbs = updateAggregatingContext (\c -> foldl (flip Context.addGroupBy) c gbs)

addRestriction' :: Monad m => Expr (Maybe Bool) -> Aggregatings m ()
addRestriction' =  updateAggregatingContext . Context.addRestriction

addGroupBys :: MonadQuery m => Projection r -> Aggregatings m (Aggregation r)
addGroupBys p = do
  addGroupBys' . Projection.columns $ p
  return $ Aggregation.unsafeFromProjection p

addRestriction :: MonadQuery m => Aggregation (Maybe Bool) -> Aggregatings m ()
addRestriction =  addRestriction' . projectAggregation

instance MonadQuery m => MonadAggregate (Aggregatings m) where
  groupBy = addGroupBys
  having  = addRestriction


appendGroupBys' :: AggregatingContext -> String -> String
appendGroupBys' c = (++ d (Context.composeGroupBys c))  where
  d "" = ""
  d s  = ' ' : s

appendGroupBys :: MonadQuery m => Aggregatings m a -> m (a, String -> String)
appendGroupBys q = second appendGroupBys' `liftM` runAggregatingPrime q


type QueryAggregate    = Orderings Aggregation (Aggregatings QueryCore)
type AggregatedQuery r = OrderedQuery Aggregation (Aggregatings QueryCore) r

expandSQL :: AggregatedQuery r -> ((String, Projection r), (String -> String, String -> String))
expandSQL q = Core.expandSQL $ assoc <$> appendGroupBys (Ordering.appendOrderBys q)  where
  assoc ((a, b), c) = (Aggregation.projection a, (b, c))

toSQL :: AggregatedQuery r -> String
toSQL q =  appOrd $ appGrp sql  where
  ((sql, _pj), (appOrd, appGrp)) = expandSQL q

toSubQuery :: AggregatedQuery r -> SubQuery
toSubQuery q = subQuery (appOrd $ appGrp sql) (Projection.width pj)  where
  ((sql, pj), (appOrd, appGrp))  = expandSQL q
