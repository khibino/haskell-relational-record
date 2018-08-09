{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Database.Relational.Set
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines set operations on monadic Relation operations.
module Database.Relational.Set (
  -- * Direct style join
  JoinRestriction,
  inner', left', right', full',
  inner, left, right, full,
  on',

  -- * Relation append
  union, except, intersect,
  unionAll, exceptAll, intersectAll,

  union', except', intersect',
  unionAll', exceptAll', intersectAll',
  ) where

import Data.Functor.ProductIsomorphic ((|$|), (|*|))

import Database.Relational.ExtensibleRecord
import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.SqlSyntax
  (Duplication (Distinct, All), SubQuery, Predicate, Record, )
import qualified Database.Relational.SqlSyntax as Syntax

import Database.Relational.Monad.BaseType
  (Relation, unsafeTypeRelation, untypeRelation, )
import Database.Relational.Monad.Trans.Placeholders (query', queryMaybe', on)
import Database.Relational.Monad.Simple (QuerySimple)
import Database.Relational.Projectable (PlaceHolders)
import Database.Relational.Relation (relation', relation, query, queryMaybe, )
import Database.Relational.ReboundSyntax (ireturn, IxMonad)
import qualified Database.Relational.ReboundSyntax as ReboundSyntax

import Prelude hiding ((>>=), (>>))
import qualified Prelude


-- | Restriction predicate function type for direct style join operator,
--   used on predicates of direct join style as follows.
--
-- @
--   do xy <- query $
--            relX `inner` relY `on'` [ \x y -> ... ] -- this lambda form has JoinRestriction type
--      ...
-- @
type JoinRestriction xs ys zs a b =
  Record (ExRecord '[]) (ExRecord xs) Flat a
  -> Record (ExRecord '[]) (ExRecord ys) Flat b
  -> Predicate (ExRecord '[]) (ExRecord (xs ++ ys ++ zs)) Flat

-- | Basic direct join operation with place-holder parameters.
join' :: (Relation (ExRecord xs_) (ExRecord ysa) pa a -> QuerySimple (ExRecord xsa) (ExRecord (xsa ++ ysa)) (PlaceHolders pa, Record (ExRecord '[]) (ExRecord '[]) Flat a'))
      -> (Relation (ExRecord ys_) (ExRecord ysb) pb b -> QuerySimple (ExRecord (xsa ++ ysa)) (ExRecord ((xsa ++ ysa) ++ ysb)) (PlaceHolders pb, Record (ExRecord '[]) (ExRecord '[]) Flat b'))
      -> Relation (ExRecord xs_) (ExRecord ysa) pa a
      -> Relation (ExRecord ys_) (ExRecord ysb) pb b
      -> JoinRestriction '[] '[] zs a' b'
      -> Relation (ExRecord xsa) (ExRecord (((xsa ++ ysa) ++ ysb) ++ zs)) (pa, pb) (a', b')
join' qL qR r0 r1 r = relation' $ do
  (ph0, pj0) <- qL r0
  (ph1, pj1) <- qR r1
  on $ r pj0 pj1
  ireturn ((,) |$| ph0 |*| ph1, (,) |$| pj0 |*| pj1)
 where
  (>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
  (>>=) = (ReboundSyntax.>>=)

  (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
  (>>) = (ReboundSyntax.>>)

-- | Direct inner join with place-holder parameters.
inner' :: Relation (ExRecord xs_) (ExRecord ysa) pa a            -- ^ Left query to join
       -> Relation (ExRecord ys_) (ExRecord ysb) pb b            -- ^ Right query to join
       -> JoinRestriction '[] '[] zs a b    -- ^ Join restrictions
       -> Relation (ExRecord xsa) (ExRecord (((xsa ++ ysa) ++ ysb) ++ zs)) (pa, pb) (a, b) -- ^ Result joined relation
inner' =  join' query' query'

-- | Direct left outer join with place-holder parameters.
left' :: Relation (ExRecord xs_) (ExRecord ysa) pa a            -- ^ Left query to join
      -> Relation (ExRecord ys_) (ExRecord ysb) pb b            -- ^ Right query to join
      -> JoinRestriction '[] '[] zs a (Maybe b)    -- ^ Join restrictions
      -> Relation (ExRecord xsa) (ExRecord (((xsa ++ ysa) ++ ysb) ++ zs)) (pa, pb) (a, Maybe b) -- ^ Result joined relation
left'  =  join' query' queryMaybe'

-- | Direct right outer join with place-holder parameters.
right' :: Relation (ExRecord xs_) (ExRecord ysa) pa a            -- ^ Left query to join
       -> Relation (ExRecord ys_) (ExRecord ysb) pb b            -- ^ Right query to join
       -> JoinRestriction '[] '[] zs (Maybe a) b    -- ^ Join restrictions
       -> Relation (ExRecord xsa) (ExRecord (((xsa ++ ysa) ++ ysb) ++ zs)) (pa, pb) (Maybe a, b) -- ^ Result joined relation
right' = join' queryMaybe' query'

-- | Direct full outer join with place-holder parameters.
full' :: Relation (ExRecord xs_) (ExRecord ysa) pa a            -- ^ Left query to join
      -> Relation (ExRecord ys_) (ExRecord ysb) pb b            -- ^ Right query to join
      -> JoinRestriction '[] '[] zs (Maybe a) (Maybe b)    -- ^ Join restrictions
      -> Relation (ExRecord xsa) (ExRecord (((xsa ++ ysa) ++ ysb) ++ zs)) (pa, pb) (Maybe a, Maybe b) -- ^ Result joined relation
full'  = join' queryMaybe' queryMaybe'

-- | Basic direct join operation.
join_ :: (Relation (ExRecord xs_) (ExRecord ysa) pa a -> QuerySimple (ExRecord xsa) (ExRecord (xsa ++ ysa)) (Record (ExRecord '[]) (ExRecord '[]) Flat a'))
      -> (Relation (ExRecord ys_) (ExRecord ysb) pb b -> QuerySimple (ExRecord (xsa ++ ysa)) (ExRecord ((xsa ++ ysa) ++ ysb)) (Record (ExRecord '[]) (ExRecord '[]) Flat b'))
      -> Relation (ExRecord xs_) (ExRecord ysa) pa a
      -> Relation (ExRecord ys_) (ExRecord ysb) pb b
      -> JoinRestriction '[] '[] zs a' b'
      -> Relation (ExRecord xsa) (ExRecord (((xsa ++ ysa) ++ ysb) ++ zs)) () (a', b')
join_ qL qR r0 r1 r = relation $ do
  pj0 <- qL r0
  pj1 <- qR r1
  on $ r pj0 pj1
  ireturn $ (,) |$| pj0 |*| pj1
 where
  (>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
  (>>=) = (ReboundSyntax.>>=)

  (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
  (>>) = (ReboundSyntax.>>)

-- | Direct inner join.
inner :: Relation (ExRecord xs_) (ExRecord ysa) () a -- ^ Left query to join
      -> Relation (ExRecord xs_) (ExRecord ysb) () b -- ^ Right query to join
      -> JoinRestriction '[] '[] zs a b -- ^ Join restrictions
      -> Relation (ExRecord xsa) (ExRecord (((xsa ++ ysa) ++ ysb) ++ zs)) () (a, b) -- ^ Result joined relation
inner =  join_ query query

-- | Direct left outer join.
left :: Relation (ExRecord xsa) (ExRecord ysa) () a -- ^ Left query to join
     -> Relation (ExRecord (xsa ++ ysa)) (ExRecord ysb) () b -- ^ Right query to join
     -> JoinRestriction '[] '[] zs a (Maybe b) -- ^ Join restrictions
     -> Relation (ExRecord xsa) (ExRecord (((xsa ++ ysa) ++ ysb) ++ zs)) () (a, Maybe b) -- ^ Result joined relation
left  =  join_ query queryMaybe

-- | Direct right outer join.
right :: Relation (ExRecord xs_) (ExRecord ysa) () a -- ^ Left query to join
      -> Relation (ExRecord xs_) (ExRecord ysb) () b -- ^ Right query to join
      -> JoinRestriction '[] '[] zs (Maybe a) b -- ^ Join restrictions
      -> Relation (ExRecord xsa) (ExRecord (((xsa ++ ysa) ++ ysb) ++ zs)) () (Maybe a, b) -- ^ Result joined relation
right =  join_ queryMaybe query

-- | Direct full outer join.
full :: Relation (ExRecord xs_) (ExRecord ysa) () a -- ^ Left query to join
     -> Relation (ExRecord xs_) (ExRecord ysb) () b -- ^ Right query to join
     -> JoinRestriction '[] '[] zs (Maybe a) (Maybe b) -- ^ Join restrictions
     -> Relation (ExRecord xsa) (ExRecord (((xsa ++ ysa) ++ ysb) ++ zs)) () (Maybe a, Maybe b) -- ^ Result joined relation
full  =  join_ queryMaybe queryMaybe

-- | Apply restriction for direct join style.
on' :: (JoinRestriction xs ys zs a b -> Relation (ExRecord '[]) (ExRecord zs) pc (a, b))
    -> JoinRestriction xs ys zs a b
    -> Relation (ExRecord '[]) (ExRecord zs) pc (a, b)
on' =  ($)

infixl 8 `inner'`, `left'`, `right'`, `full'`, `inner`, `left`, `right`, `full`, `on'`

unsafeLiftAppend :: (SubQuery -> SubQuery -> SubQuery)
           -> Relation i j p a
           -> Relation k l q a
           -> Relation m n r a
unsafeLiftAppend op a0 a1 = unsafeTypeRelation $ do
  s0 <- untypeRelation a0
  s1 <- untypeRelation a1
  return $ s0 `op` s1
 where
  (>>=) :: Monad m => m a -> (a -> m b) -> m b
  (>>=) = (Prelude.>>=)

liftAppend :: (SubQuery -> SubQuery -> SubQuery)
           -> Relation i j () a
           -> Relation i j () a
           -> Relation i j () a
liftAppend = unsafeLiftAppend

-- | Union of two relations.
union     :: Relation i j () a -> Relation i j () a -> Relation i j () a
union     =  liftAppend $ Syntax.union Distinct

-- | Union of two relations. Not distinct.
unionAll  :: Relation i j () a -> Relation i j () a -> Relation i j () a
unionAll  =  liftAppend $ Syntax.union All

-- | Subtraction of two relations.
except    :: Relation i j () a -> Relation i j () a -> Relation i j () a
except    =  liftAppend $ Syntax.except Distinct

-- | Subtraction of two relations. Not distinct.
exceptAll :: Relation i j () a -> Relation i j () a -> Relation i j () a
exceptAll =  liftAppend $ Syntax.except All

-- | Intersection of two relations.
intersect :: Relation i j () a -> Relation i j () a -> Relation i j () a
intersect =  liftAppend $ Syntax.intersect Distinct

-- | Intersection of two relations. Not distinct.
intersectAll :: Relation i j () a -> Relation i j () a -> Relation i j () a
intersectAll =  liftAppend $ Syntax.intersect All

liftAppend' :: (SubQuery -> SubQuery -> SubQuery)
            -> Relation i j p a
            -> Relation i j q a
            -> Relation i j (p, q) a
liftAppend' = unsafeLiftAppend

-- | Union of two relations with place-holder parameters.
-- igrep TODO: Relation i j p a -> Relation i j j k q a -> Relation i k a?
union'     :: Relation i j p a -> Relation i j q a -> Relation i j (p, q) a
union'     =  liftAppend' $ Syntax.union Distinct

-- | Union of two relations with place-holder parameters. Not distinct.
unionAll' :: Relation i j p a -> Relation i j q a -> Relation i j (p, q) a
unionAll'  =  liftAppend' $ Syntax.union All

-- | Subtraction of two relations with place-holder parameters.
except'    :: Relation i j p a -> Relation i j q a -> Relation i j (p, q) a
except'    =  liftAppend' $ Syntax.except Distinct

-- | Subtraction of two relations with place-holder parameters. Not distinct.
exceptAll' :: Relation i j p a -> Relation i j q a -> Relation i j (p, q) a
exceptAll' =  liftAppend' $ Syntax.except All

-- | Intersection of two relations with place-holder parameters.
intersect' :: Relation i j p a -> Relation i j q a -> Relation i j (p, q) a
intersect' =  liftAppend' $ Syntax.intersect Distinct

-- | Intersection of two relations with place-holder parameters. Not distinct.
intersectAll' :: Relation i j p a -> Relation i j q a -> Relation i j (p, q) a
intersectAll' =  liftAppend' $ Syntax.intersect All

infixl 7 `union`, `except`, `unionAll`, `exceptAll`
infixl 8 `intersect`, `intersectAll`
infixl 7 `union'`, `except'`, `unionAll'`, `exceptAll'`
infixl 8 `intersect'`, `intersectAll'`
