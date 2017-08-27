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

import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.SqlSyntax
  (Duplication (Distinct, All), SubQuery, Predicate, Record, )
import qualified Database.Relational.SqlSyntax as Syntax

import Database.Relational.Monad.BaseType
  (Relation, unsafeTypeRelation, untypeRelation, )
import Database.Relational.Monad.Class (MonadQuery (query', queryMaybe'), on)
import Database.Relational.Monad.Simple (QuerySimple)
import Database.Relational.Projectable (PlaceHolders, projectZip)
import Database.Relational.Relation (relation', relation, query, queryMaybe, )


-- | Restriction predicate function type for direct style join operator,
--   used on predicates of direct join style as follows.
--
-- @
--   do xy <- query $
--            relX `inner` relY `on'` [ \x y -> ... ] -- this lambda form has JoinRestriction type
--      ...
-- @
type JoinRestriction a b = Record Flat a -> Record Flat b -> Predicate Flat

-- | Basic direct join operation with place-holder parameters.
join' :: (qa -> QuerySimple (PlaceHolders pa, Record Flat a))
      -> (qb -> QuerySimple (PlaceHolders pb, Record Flat b))
      -> qa
      -> qb
      -> [JoinRestriction a b]
      -> Relation (pa, pb) (a, b)
join' qL qR r0 r1 rs = relation' $ do
  (ph0, pj0) <- qL r0
  (ph1, pj1) <- qR r1
  sequence_ [ on $ f pj0 pj1 | f <- rs ]
  return (ph0 `projectZip` ph1, pj0 `projectZip` pj1)

-- | Direct inner join with place-holder parameters.
inner' :: Relation pa a            -- ^ Left query to join
       -> Relation pb b            -- ^ Right query to join
       -> [JoinRestriction a b]    -- ^ Join restrictions
       -> Relation (pa, pb) (a, b) -- ^ Result joined relation
inner' =  join' query' query'

-- | Direct left outer join with place-holder parameters.
left' :: Relation pa a                  -- ^ Left query to join
      -> Relation pb b                  -- ^ Right query to join
      -> [JoinRestriction a (Maybe b)]  -- ^ Join restrictions
      -> Relation (pa, pb) (a, Maybe b) -- ^ Result joined relation
left'  =  join' query' queryMaybe'

-- | Direct right outer join with place-holder parameters.
right' :: Relation pa a                 -- ^ Left query to join
       -> Relation pb b                 -- ^ Right query to join
       -> [JoinRestriction (Maybe a) b] -- ^ Join restrictions
       -> Relation (pa, pb)(Maybe a, b) -- ^ Result joined relation
right' =  join' queryMaybe' query'

-- | Direct full outer join with place-holder parameters.
full' :: Relation pa a                         -- ^ Left query to join
      -> Relation pb b                         -- ^ Right query to join
      -> [JoinRestriction (Maybe a) (Maybe b)] -- ^ Join restrictions
      -> Relation (pa, pb) (Maybe a, Maybe b)  -- ^ Result joined relation
full'  =  join' queryMaybe' queryMaybe'

-- | Basic direct join operation.
join_ :: (qa -> QuerySimple (Record Flat a))
      -> (qb -> QuerySimple (Record Flat b))
      -> qa
      -> qb
      -> [JoinRestriction a b]
      -> Relation () (a, b)
join_ qL qR r0 r1 rs = relation $ do
  pj0 <- qL r0
  pj1 <- qR r1
  sequence_ [ on $ f pj0 pj1 | f <- rs ]
  return $ pj0 `projectZip` pj1

-- | Direct inner join.
inner :: Relation () a         -- ^ Left query to join
      -> Relation () b         -- ^ Right query to join
      -> [JoinRestriction a b] -- ^ Join restrictions
      -> Relation () (a, b)    -- ^ Result joined relation
inner =  join_ query query

-- | Direct left outer join.
left :: Relation () a                 -- ^ Left query to join
     -> Relation () b                 -- ^ Right query to join
     -> [JoinRestriction a (Maybe b)] -- ^ Join restrictions
     -> Relation () (a, Maybe b)      -- ^ Result joined relation
left  =  join_ query queryMaybe

-- | Direct right outer join.
right :: Relation () a                 -- ^ Left query to join
      -> Relation () b                 -- ^ Right query to join
      -> [JoinRestriction (Maybe a) b] -- ^ Join restrictions
      -> Relation () (Maybe a, b)      -- ^ Result joined relation
right =  join_ queryMaybe query

-- | Direct full outer join.
full :: Relation () a                         -- ^ Left query to join
     -> Relation () b                         -- ^ Right query to join
     -> [JoinRestriction (Maybe a) (Maybe b)] -- ^ Join restrictions
     -> Relation () (Maybe a, Maybe b)        -- ^ Result joined relation
full  =  join_ queryMaybe queryMaybe

-- | Apply restriction for direct join style.
on' :: ([JoinRestriction a b] -> Relation pc (a, b))
    -> [JoinRestriction a b]
    -> Relation pc (a, b)
on' =  ($)

infixl 8 `inner'`, `left'`, `right'`, `full'`, `inner`, `left`, `right`, `full`, `on'`

unsafeLiftAppend :: (SubQuery -> SubQuery -> SubQuery)
           -> Relation p a
           -> Relation q a
           -> Relation r a
unsafeLiftAppend op a0 a1 = unsafeTypeRelation $ do
  s0 <- untypeRelation a0
  s1 <- untypeRelation a1
  return $ s0 `op` s1

liftAppend :: (SubQuery -> SubQuery -> SubQuery)
           -> Relation () a
           -> Relation () a
           -> Relation () a
liftAppend = unsafeLiftAppend

-- | Union of two relations.
union     :: Relation () a -> Relation () a -> Relation () a
union     =  liftAppend $ Syntax.union Distinct

-- | Union of two relations. Not distinct.
unionAll  :: Relation () a -> Relation () a -> Relation () a
unionAll  =  liftAppend $ Syntax.union All

-- | Subtraction of two relations.
except    :: Relation () a -> Relation () a -> Relation () a
except    =  liftAppend $ Syntax.except Distinct

-- | Subtraction of two relations. Not distinct.
exceptAll :: Relation () a -> Relation () a -> Relation () a
exceptAll =  liftAppend $ Syntax.except All

-- | Intersection of two relations.
intersect :: Relation () a -> Relation () a -> Relation () a
intersect =  liftAppend $ Syntax.intersect Distinct

-- | Intersection of two relations. Not distinct.
intersectAll :: Relation () a -> Relation () a -> Relation () a
intersectAll =  liftAppend $ Syntax.intersect All

liftAppend' :: (SubQuery -> SubQuery -> SubQuery)
            -> Relation p a
            -> Relation q a
            -> Relation (p, q) a
liftAppend' = unsafeLiftAppend

-- | Union of two relations with place-holder parameters.
union'     :: Relation p a -> Relation q a -> Relation (p, q) a
union'     =  liftAppend' $ Syntax.union Distinct

-- | Union of two relations with place-holder parameters. Not distinct.
unionAll' :: Relation p a -> Relation q a -> Relation (p, q) a
unionAll'  =  liftAppend' $ Syntax.union All

-- | Subtraction of two relations with place-holder parameters.
except'    :: Relation p a -> Relation q a -> Relation (p, q) a
except'    =  liftAppend' $ Syntax.except Distinct

-- | Subtraction of two relations with place-holder parameters. Not distinct.
exceptAll' :: Relation p a -> Relation q a -> Relation (p, q) a
exceptAll' =  liftAppend' $ Syntax.except All

-- | Intersection of two relations with place-holder parameters.
intersect' :: Relation p a -> Relation q a -> Relation (p, q) a
intersect' =  liftAppend' $ Syntax.intersect Distinct

-- | Intersection of two relations with place-holder parameters. Not distinct.
intersectAll' :: Relation p a -> Relation q a -> Relation (p, q) a
intersectAll' =  liftAppend' $ Syntax.intersect All

infixl 7 `union`, `except`, `unionAll`, `exceptAll`
infixl 8 `intersect`, `intersectAll`
infixl 7 `union'`, `except'`, `unionAll'`, `exceptAll'`
infixl 8 `intersect'`, `intersectAll'`
