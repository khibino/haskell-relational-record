{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Database.Relational.Monad.Trans.Placeholders where


import           Control.Monad.Indexed
import           Control.Monad.Indexed.State
import           Control.Monad.Indexed.Trans
import           Database.Relational.ExtensibleRecord
import           Database.Relational.SqlSyntax.Types
import           Database.Relational.SqlSyntax.Aggregate (aggregateKeyRecord)
import           Database.Relational.Monad.Class
import           Database.Relational.Internal.ContextType (Flat, Aggregated)
import           Database.Relational.Projectable (PlaceHolders)
import           Database.Relational.Monad.BaseType (Relation)


newtype Placeholders m i j a =
  Placeholders { unPlaceholders :: IxStateT m i j a }
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, IxMonadTrans, Functor)

deriving instance Monad m => Applicative (Placeholders m i i)

deriving instance Monad m => Monad (Placeholders m i i)

instance (Functor q, Monad q, Functor m, Monad m, MonadQualify q m) => MonadQualify q (Placeholders m i i) where
  liftQualify = placeholders . liftQualify

instance MonadQuery m => MonadQuery (Placeholders m i i) where
  setDuplication                         = placeholders . setDuplication
  restrictJoin                           = placeholders . restrictJoin
  queryWithoutUpdatingPlaceholders'      = placeholders . queryWithoutUpdatingPlaceholders'
  queryMaybeWithoutUpdatingPlaceholders' = placeholders . queryMaybeWithoutUpdatingPlaceholders'

instance MonadPartition c m => MonadPartition c (Placeholders m i i) where
  partitionByWithoutUpdatingPlaceholders = placeholders . partitionByWithoutUpdatingPlaceholders

placeholders :: Monad m => m a -> Placeholders m i i a
placeholders = ilift

addPlaceholders
  :: Monad m
  => recordOrRelation (ExRecord ys) (ExRecord zs) c r
  -> Placeholders m (ExRecord xs) (ExRecord (xs ++ zs)) ()
addPlaceholders rr =
  Placeholders $ imodify (\xs -> xs `happend` nextIndexOf rr)


-- igrep TODO: j should be `Data.Extensible.Record '[]`
extractPlaceholders :: Monad m => Placeholders m i j a -> m a
extractPlaceholders  (Placeholders a) = fst <$> runIxStateT a (error "TODO Pass a Data.Extensible.Record")


-- igrep TODO: rename into addingPlaceholders
addingPlaceholders
  :: Monad m
  => recordOrRelation (ExRecord ys) (ExRecord zs) c r
  -> m a
  -> Placeholders m (ExRecord xs) (ExRecord (xs ++ zs)) a
addingPlaceholders rec action =
  addPlaceholders rec >>>= const (placeholders action)


forceSetPlaceholders :: Monad m => recordOrRelation i j c r -> Placeholders m i j ()
forceSetPlaceholders = Placeholders . iput . nextIndexOf


forceSettingPlaceholders
  :: Monad m
  => recordOrRelation i j c r
  -> m a
  -> Placeholders m i j a
forceSettingPlaceholders rec action =
  forceSetPlaceholders rec >>>= const (placeholders action)


-- | Add restriction to last join. Record type version.
on :: MonadQuery m => Predicate (ExRecord '[]) (ExRecord ys) Flat -> Placeholders m (ExRecord xs) (ExRecord (xs ++ ys)) ()
on pd = addingPlaceholders pd (restrictJoin pd)

query'
  :: MonadQuery m
  => Relation (ExRecord ys) (ExRecord zs) p r
  -> Placeholders m (ExRecord xs) (ExRecord (xs ++ zs)) (PlaceHolders p, Record (ExRecord '[]) (ExRecord '[]) Flat r)
query' rel =
  addingPlaceholders rel (queryWithoutUpdatingPlaceholders' rel)


queryMaybe'
  :: MonadQuery m
  => Relation (ExRecord ys) (ExRecord zs) p r
  -> Placeholders m (ExRecord xs) (ExRecord (xs ++ zs)) (PlaceHolders p, Record (ExRecord '[]) (ExRecord '[]) Flat (Maybe r))
queryMaybe' rel =
  addingPlaceholders rel (queryMaybeWithoutUpdatingPlaceholders' rel)


-- | Add restriction to this not aggregated query.
wheres :: (MonadRestrict Flat m) => Predicate (ExRecord '[]) (ExRecord ys) Flat -> Placeholders m (ExRecord xs) (ExRecord (xs ++ ys)) ()
wheres pd =
  addingPlaceholders pd (restrict pd)


-- | Add restriction to this aggregated query. Aggregated Record type version.
having :: MonadRestrict Aggregated m => Predicate (ExRecord '[]) (ExRecord ys) Aggregated -> Placeholders m (ExRecord xs) (ExRecord (xs ++ ys)) ()
having pd =
  addingPlaceholders pd (restrict pd)


groupBy
  :: MonadAggregate m
  => Record (ExRecord '[]) (ExRecord ys) Flat r
  -> Placeholders m (ExRecord xs) (ExRecord (xs ++ ys)) (Record (ExRecord '[]) (ExRecord '[]) Aggregated r)
groupBy rec =
  addingPlaceholders rec (groupByWithoutUpdatingPlaceholders rec)


groupBy'
  :: MonadAggregate m
  => AggregateKey (Record (ExRecord '[]) (ExRecord ys) Aggregated r)
  -> Placeholders m (ExRecord xs) (ExRecord (xs ++ ys)) (Record (ExRecord '[]) (ExRecord '[]) Aggregated r)
groupBy' key =
  addingPlaceholders (aggregateKeyRecord key) (groupByWithoutUpdatingPlaceholders' key)


partitionBy :: MonadPartition c m => Record (ExRecord '[]) (ExRecord ys) c r -> Placeholders m (ExRecord xs) (ExRecord (xs ++ ys)) ()
partitionBy rec =
  addingPlaceholders rec (partitionByWithoutUpdatingPlaceholders rec)
