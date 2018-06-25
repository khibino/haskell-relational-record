{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Experiment where


import           Control.Arrow ((***))
import           Control.Monad.Indexed
import           Control.Monad.Indexed.State
import           Control.Monad.Indexed.Trans
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Writer.Lazy (runWriterT, execWriterT, WriterT, tell)
import           Data.DList (DList)
import qualified Data.DList as DL
import           Data.Extensible
import           Data.Extensible.HList
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Functor.Const
import           Data.Proxy
import           Data.Type.Equality (type (==))
import qualified Database.Relational as HRR
import qualified Database.Relational.Record as HRR
import           Database.Relational.Monad.Class
import           Database.Relational.Monad.Type
import           Database.Relational.Monad.Trans.Ordering
import           Database.Relational.SqlSyntax.Query
import           GHC.OverloadedLabels
import           Prelude hiding ((>>=), (>>))
import qualified Prelude ((>>=), (>>))



newtype PlaceHolderValues = PlaceHolderValues { placeHolderValuesAsDList :: DList String } deriving (Eq, Show, Monoid)

-- Effに書き換えるのは難しい？
newtype Placeholders m p1 p2 a =
  Placeholders { unPlaceholders :: (IxStateT (WriterT PlaceHolderValues m) p1 p2 a) }
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, Functor)

instance Monad m => Applicative (Placeholders m p p)

instance Monad m => Monad (Placeholders m p p)

instance IxMonadTrans Placeholders where
  ilift = Placeholders . ilift . lift

liftPlaceholders :: Monad m => m a -> Placeholders m p p a
liftPlaceholders = ilift


instance HRR.MonadRestrict rc m => HRR.MonadRestrict rc (Placeholders m p p) where
  restrict = liftPlaceholders . restrict


instance HRR.MonadQualify q m => HRR.MonadQualify q (Placeholders m p p) where
  liftQualify = liftPlaceholders . liftQualify


instance HRR.MonadQuery m => HRR.MonadQuery (Placeholders m p p) where
  setDuplication = liftPlaceholders . setDuplication
  restrictJoin   = liftPlaceholders . restrictJoin
  query'         = liftPlaceholders . query'
  queryMaybe'    = liftPlaceholders . queryMaybe'


instance HRR.MonadAggregate m => HRR.MonadAggregate (Placeholders m p p) where
  groupBy  = liftPlaceholders . groupBy
  groupBy' = liftPlaceholders . groupBy'


instance HRR.MonadPartition c m => HRR.MonadPartition c (Placeholders m p p) where
  partitionBy = liftPlaceholders . partitionBy

type QueryM p1 p2 a = Placeholders HRR.QuerySimple p1 p2 a


-- TODO: convert to type HDBC can handle
runQueryM
  :: HRR.Config
  -> Record xs -- ^ Parameter record
  -> QueryM (Record xs) (Record '[]) (HRR.Record HRR.Flat r)
  -> (String, [String]) -- ^ Built SQL and parameters converted into string
runQueryM cfg params action = (show . HRR.relationalQuery . HRR.unsafeTypeRelation *** DL.toList . placeHolderValuesAsDList) sq
 where
  sq :: (HRR.ConfigureQuery HRR.SubQuery, PlaceHolderValues)
  sq = (`HRR.configureQuery` cfg) $ do
    ((((((hrrRec, _emptyRec), phvs), ot), rs), jp), da) <- extractCore $ extractOrderingTerms $ runWriterT $ runIxStateT (unPlaceholders action) params
    c <- HRR.askConfig
    return (return $ flatSubQuery c (HRR.untype hrrRec) da jp rs ot, phvs)

  (>>=) :: Monad m => m a -> (a -> m b) -> m b
  (>>=) = (Prelude.>>=)

  (>>) :: Monad m => m a -> m b -> m b
  (>>) = (Prelude.>>)

{-
-- | Simple (not-aggregated) query monad type.
type QuerySimple = Orderings Flat QueryCore

-- | Simple (not-aggregated) query type. 'SimpleQuery'' p r == 'QuerySimple' ('PlaceHolders' p, 'Record' r).
type SimpleQuery p r = OrderedQuery Flat QueryCore p r

-- | Type to accumulate ordering context.
--   Type 'c' is ordering term record context type.
newtype Orderings c m a =
  Orderings (WriterT (DList OrderingTerm) m a)
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | OrderedQuery monad type with placeholder type 'p'. Record must be the same as 'Orderings' context type parameter 'c'.
type OrderedQuery c m p r = Orderings c m (PlaceHolders p, Record c r)

-- | Run 'Orderings' to get 'OrderingTerms'
extractOrderingTerms :: (Monad m, Functor m) => Orderings c m a -> m (a, [OrderingTerm])
extractOrderingTerms (Orderings oc) = second toList <$> runWriterT oc

-- | Lift to 'Orderings'.
orderings :: Monad m => m a -> Orderings c m a
orderings =  lift

extract :: SimpleQuery p r
        -> ConfigureQuery (((((PlaceHolders p, Record Flat r), [OrderingTerm]), [Predicate Flat]),
                           JoinProduct), Duplication)
extract =  extractCore . extractOrderingTerms

-- | Run 'SimpleQuery' to get 'SubQuery' with 'Qualify' computation.
toSubQuery :: SimpleQuery p r        -- ^ 'SimpleQuery'' to run
           -> ConfigureQuery SubQuery -- ^ Result 'SubQuery' with 'Qualify' computation
toSubQuery q = do
   (((((_ph, pj), ot), rs), pd), da) <- extract q
   c <- askConfig
   return $ flatSubQuery c (Record.untype pj) da pd rs ot


-- | Finalize 'QuerySimple' monad and generate 'Relation' with place-holder parameter 'p'.
relation' :: SimpleQuery p r -> Relation p r
relation' =  unsafeTypeRelation . Simple.toSubQuery

-- | Finalize 'QuerySimple' monad and generate 'Relation'.
relation :: QuerySimple (Record Flat r) -> Relation () r
relation =  relation' . addUnitPH

-- | Relation type with place-holder parameter 'p' and query result type 'r'.
newtype Relation p r = SubQuery (ConfigureQuery SubQuery)

-- | Unsafely type qualified subquery into record typed relation type.
unsafeTypeRelation :: ConfigureQuery SubQuery -> Relation p r
unsafeTypeRelation = SubQuery

-- | Sub-query Qualify monad from relation.
untypeRelation :: Relation p r -> ConfigureQuery SubQuery
untypeRelation (SubQuery qsub) = qsub

-- | Generate SQL string from 'Relation' with configuration.
sqlFromRelationWith :: Relation p r -> Config -> StringSQL
sqlFromRelationWith =  configureQuery . (showSQL <$>) . untypeRelation


-- | Query type with place-holder parameter 'p' and query result type 'a'.
newtype Query p a = Query { untypeQuery :: String }

-- | Unsafely make typed 'Query' from SQL string.
unsafeTypedQuery :: String    -- ^ Query SQL to type
                 -> Query p a -- ^ Typed result
unsafeTypedQuery =  Query

-- | Show query SQL string
instance Show (Query p a) where
  show = untypeQuery

-- | From 'Relation' into untyped SQL query string.
relationalQuerySQL :: Config -> Relation p r -> QuerySuffix -> String
relationalQuerySQL config rel qsuf = showStringSQL $ sqlFromRelationWith rel config <> showsQuerySuffix qsuf

-- | From 'Relation' into typed 'Query' with suffix SQL words.
relationalQuery' :: Relation p r -> QuerySuffix -> Query p r
relationalQuery' rel qsuf = unsafeTypedQuery $ relationalQuerySQL defaultConfig rel qsuf

-- | From 'Relation' into typed 'Query'.
relationalQuery :: Relation p r -> Query p r
relationalQuery =  (`relationalQuery'` [])
-}


singlePlaceholderValue :: Show a => a -> PlaceHolderValues
singlePlaceholderValue = PlaceHolderValues . DL.singleton . show


unsafeAppendValue
  :: (Monad m, Show a) => a -> Placeholders m (Record xs) (Record xs) ()
unsafeAppendValue = Placeholders . ilift . tell . singlePlaceholderValue

{-
-- | Provide scoped placeholder from width and return its parameter object.
pwPlaceholder :: SqlContext c
              => PersistableRecordWidth a
              -> (Record c a -> b)
              -> (PlaceHolders a, b)
pwPlaceholder pw f = (PlaceHolders, f $ projectPlaceHolder pw)
  where
    projectPlaceHolder :: SqlContext c
                       => PersistableRecordWidth a
                       -> Record c a
    projectPlaceHolder = unsafeProjectSqlTerms . (`replicate` "?") . runPersistableRecordWidth

-- | Provide scoped placeholder and return its parameter object.
placeholder' :: (PersistableWidth t, SqlContext c) => (Record c t -> a) ->  (PlaceHolders t, a)
placeholder' = pwPlaceholder persistableWidth

-- | Provide scoped placeholder and return its parameter object. Monadic version.
placeholder :: (PersistableWidth t, SqlContext c, Monad m) => (Record c t -> m a) -> m (PlaceHolders t, a)
placeholder f = do
  let (ph, ma) = placeholder' f
  a <- ma
  return (ph, a)
-}


ph
  :: forall key v xs ys m
   . (Monad m, Show v, HWithout (key >: v) xs ys, Associate key v xs)
  => FieldName key
  -> Placeholders m (Record xs) (Record ys) ()
ph field = do
  rec <- Placeholders iget
  -- TODO append "?"
  unsafeAppendValue $ rec ^. itemAssoc (Proxy :: Proxy key)
  Placeholders $ imodify $ without field
 where
  (>>=)
    :: forall im i j k a b . IxMonad im => im i j a -> (a -> im j k b) -> im i k b
  (>>=) = (>>>=)

  (>>) :: forall im i j k a b . IxMonad im => im i j a -> im j k b -> im i k b
  m >> k = m >>= \_ -> k

class HEq (x :: k) (y :: k) (b :: Bool) | x y -> b
instance ((Proxy x == Proxy y) ~ b) => HEq x y b

class ConsFalse (b :: Bool) (x :: k) (xs :: [k]) (r :: [k]) | b x xs -> r, r b -> xs, x xs r -> b where
  hconsFalse :: Proxy b -> h x -> HList h xs -> HList h r

instance ConsFalse 'False x xs (x ': xs) where
  hconsFalse _ = HCons

instance ConsFalse 'True x xs xs where
  hconsFalse _ _ = id


class HWithout (x :: k) (xs :: [k]) (ys :: [k]) | x xs -> ys where
  hwithout :: Proxy x -> HList h xs -> HList h ys

instance HWithout x '[] '[] where
  hwithout _ HNil = HNil

instance (HEq x y b, ConsFalse b x ys zs, HWithout y xs ys) => HWithout y (x ': xs) zs where
  hwithout p (HCons x xs) = hconsFalse (Proxy :: Proxy b) x (hwithout p xs)


without
  :: forall k v xs ys h
   . (HWithout (k >: v) xs ys, Associate k v xs)
  => FieldName k
  -> RecordOf h xs
  -> RecordOf h ys
without _ = fromHList . hwithout (Proxy :: Proxy (k >: v)) . toHList


-- | Copied from https://hackage.haskell.org/package/microlens-0.4.9.1/docs/Lens-Micro.html#t:Getting
type Getting r s a = (a -> Const r a) -> s -> Const r s

-- | Copied from https://hackage.haskell.org/package/microlens-0.4.9.1/docs/src/Lens-Micro.html#%5E.
(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}
infixl 8 ^.
