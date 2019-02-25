{-# LANGUAGE FlexibleContexts #-}

-- | Monadic actions wrapped to record 'referredPlaceholders' of 'Record'
module Database.Relational.Monad.ReferPlaceholders
  ( query' , queryMaybe'
  , on
  , wheres, having
  , groupBy, groupBy', partitionBy
  , orderBy, orderBy', asc, desc
  , assignTo , (<-#)
  ) where


import Database.Relational.SqlSyntax
  (Predicate, Record, AggregateKey, Order (..), Nulls (..), emptyPlaceholderOffsets, aggregateKeyRecord)
import Database.Relational.Internal.ContextType (Flat, Aggregated)
import Database.Relational.Monad.BaseType
  (Relation, ConfigureQuery, untypeRelation, unsafeTypeRelation)
import Database.Relational.Monad.Class
import Database.Relational.Monad.Trans.Assigning
import Database.Relational.Monad.Trans.Ordering
import Database.Relational.Monad.Trans.ReferredPlaceholders
import Database.Relational.Projectable (PlaceHolders)

query' :: MonadQuery m => Relation p r -> ReferredPlaceholders m (PlaceHolders p, Record Flat r)
query' = appendingPlaceholdersOfRelation queryNoPh'

queryMaybe' :: MonadQuery m => Relation p r -> ReferredPlaceholders m (PlaceHolders p, Record Flat (Maybe r))
queryMaybe' = appendingPlaceholdersOfRelation queryMaybeNoPh'

-- | Add restriction to last join. Record type version.
on :: MonadQuery m => Predicate Flat -> ReferredPlaceholders m ()
on =  appendingPlaceholdersOfRecord restrictJoinNoPh

-- | Add restriction to this not aggregated query.
wheres :: MonadRestrict Flat m => Predicate Flat -> ReferredPlaceholders m ()
wheres =  appendingPlaceholdersOfRecord restrictNoPh

-- | Add restriction to this aggregated query. Aggregated Record type version.
having :: MonadRestrict Aggregated m => Predicate Aggregated -> ReferredPlaceholders m ()
having =  appendingPlaceholdersOfRecord restrictNoPh

groupBy
  :: MonadAggregate m
  => Record Flat r
  -- ^ Record to add into group by
  -> ReferredPlaceholders m (Record Aggregated r)
  -- ^ Result context and aggregated record
groupBy = appendingPlaceholdersOfRecord groupByNoPh

groupBy'
  :: MonadAggregate m
  => AggregateKey (Record Aggregated r)
  -- ^ Key to aggretate for non-traditional group-by interface
  -> ReferredPlaceholders m (Record Aggregated r)
  -- ^ Result context and aggregated record
groupBy' key = do
  let r = aggregateKeyRecord key
  appendPlaceholdersOfRecord r
  referredPlaceholders $ groupByNoPh' (emptyPlaceholderOffsets <$> key)

-- | Add /PARTITION BY/ term into context.
partitionBy :: MonadPartition c m => Record c r -> ReferredPlaceholders m ()
partitionBy = appendingPlaceholdersOfRecord partitionByNoPh

-- | Add ordering terms with null ordering.
orderBy'
  :: Monad m
  => Record c t                              -- ^ Ordering terms to add
  -> Order                                   -- ^ Order direction
  -> Nulls                                   -- ^ Order of null
  -> ReferredPlaceholders (Orderings c m) () -- ^ Result context with ordering
orderBy' p o n = appendingPlaceholdersOfRecord (\r -> orderByNoPh' r o n) p

-- | Add ordering terms.
orderBy
  :: Monad m
  => Record c t                              -- ^ Ordering terms to add
  -> Order                                   -- ^ Order direction
  -> ReferredPlaceholders (Orderings c m) () -- ^ Result context with ordering
orderBy p o = appendingPlaceholdersOfRecord (`orderByNoPh` o) p

-- | Add ascendant ordering term.
asc
  :: Monad m
  => Record c t                              -- ^ Ordering terms to add
  -> ReferredPlaceholders (Orderings c m) () -- ^ Result context with ordering
asc = appendingPlaceholdersOfRecord ascNoPh

-- | Add descendant ordering term.
desc
  :: Monad m
  => Record c t                              -- ^ Ordering terms to add
  -> ReferredPlaceholders (Orderings c m) () -- ^ Result context with ordering
desc = appendingPlaceholdersOfRecord descNoPh

-- | Add an assignment.
assignTo :: Monad m => Record Flat v ->  AssignTarget r v -> ReferredPlaceholders (Assignings r m) ()
assignTo vp target = appendingPlaceholdersOfRecord (`assignToNoPh` target) vp

-- | Add and assginment.
(<-#) :: Monad m => AssignTarget r v -> Record Flat v -> ReferredPlaceholders (Assignings r m) ()
(<-#) =  flip assignTo

infix 4 <-#

appendingPlaceholdersOfRecord :: Monad m => (Record c a -> m b) -> Record c a -> ReferredPlaceholders m b
appendingPlaceholdersOfRecord act r = do
  appendPlaceholdersOfRecord r
  referredPlaceholders (act $ emptyPlaceholderOffsets r)

appendingPlaceholdersOfRelation
  :: MonadQualify ConfigureQuery m => (Relation p a -> m b) -> Relation p a -> ReferredPlaceholders m b
appendingPlaceholdersOfRelation act r = do
  (phs, subq) <- liftQualify $ untypeRelation r
  appendPlaceholderOffsets phs
  let r' = unsafeTypeRelation $ return (mempty, subq)
  referredPlaceholders (act r')
