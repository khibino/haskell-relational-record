
module Database.Relational.Query.Relation (
  table,
  relation, relation',

  query, query', queryMaybe, queryMaybe', from,

  PrimeRelation, Relation,

  inner', left', right', full',
  inner, left, right, full,

  sqlFromRelation,

  subQueryFromRelation,

  nested, width
  ) where

import Database.Relational.Query.Monad.Simple (QuerySimple)
import qualified Database.Relational.Query.Monad.Ordering as QueryOrdering
import qualified Database.Relational.Query.Monad.Simple as QuerySimple
import qualified Database.Relational.Query.Monad.Core   as QueryCore

import Database.Relational.Query.Table (Table)

import Database.Relational.Query.Internal.Product (NodeAttr(Just', Maybe))

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable
  (PlaceHolders, addPlaceHolders, projectZip)
import Database.Relational.Query.ProjectableExtended
  (ProjectableGeneralizedZip (generalizedZip))

import Database.Relational.Query.Sub (SubQuery)
import qualified Database.Relational.Query.Sub as SubQuery


data PrimeRelation p r = SubQuery SubQuery
                       | PrimeRelation (QuerySimple (Projection r))

type Relation r = PrimeRelation () r


table :: Table r -> Relation r
table =  SubQuery . SubQuery.fromTable


queryWithAttr :: NodeAttr -> PrimeRelation p r -> QuerySimple (PlaceHolders p, Projection r)
queryWithAttr attr = addPlaceHolders . d where
  d (SubQuery sub)    = QuerySimple.simple $ QueryCore.unsafeSubQueryWithAttr attr sub
  d (PrimeRelation q) = QuerySimple.unsafeMergeAnotherOrderBys attr q

query' :: PrimeRelation p r -> QuerySimple (PlaceHolders p, Projection r)
query' =  queryWithAttr Just'

query :: Relation r -> QuerySimple (Projection r)
query =  fmap snd . query'

queryMaybe' :: PrimeRelation p r -> QuerySimple (PlaceHolders p, Projection (Maybe r))
queryMaybe' pr =  do
  (ph, pj) <- queryWithAttr Maybe pr
  return (ph, Projection.just pj)

queryMaybe :: PrimeRelation p r -> QuerySimple (Projection (Maybe r))
queryMaybe =  fmap snd . queryMaybe'

relation :: QuerySimple (Projection r) -> PrimeRelation p r
relation =  PrimeRelation

relation' :: QuerySimple (PlaceHolders p, Projection r) -> PrimeRelation p r
relation' =  PrimeRelation . fmap snd

from :: Table r -> Relation r
from =  table


join' :: ProjectableGeneralizedZip pa pb pc
      => (qa -> QuerySimple (PlaceHolders pa, Projection a))
      -> (qb -> QuerySimple (PlaceHolders pb, Projection b))
      -> qa
      -> qb
      -> PrimeRelation pc (a, b)
join' qL qR r0 r1 = relation' $ do
  (ph0, pj0) <- qL r0
  (ph1, pj1) <- qR r1
  return $ (ph0 `generalizedZip` ph1, pj0 `projectZip` pj1)

inner' :: ProjectableGeneralizedZip pa pb pc
       => PrimeRelation pa a
       -> PrimeRelation pb b
       -> PrimeRelation pc (a, b)
inner' =  join' query' query'

left'  :: ProjectableGeneralizedZip pa pb pc
       => PrimeRelation pa a
       -> PrimeRelation pb b
       -> PrimeRelation pc (a, Maybe b)
left'  =  join' query' queryMaybe'

right' :: ProjectableGeneralizedZip pa pb pc
       => PrimeRelation pa a
       -> PrimeRelation pb b
       -> PrimeRelation pc(Maybe a, b)
right' =  join' queryMaybe' query'

full'  :: ProjectableGeneralizedZip pa pb pc
       => PrimeRelation pa a
       -> PrimeRelation pb b
       -> PrimeRelation pc (Maybe a, Maybe b)
full'  =  join' queryMaybe' queryMaybe'

join :: (qa -> QuerySimple (Projection a))
     -> (qb -> QuerySimple (Projection b))
     -> qa
     -> qb
     -> Relation (a, b)
join qL qR r0 r1 = relation $ do
  pj0 <- qL r0
  pj1 <- qR r1
  return $ pj0 `projectZip` pj1

inner :: Relation a
      -> Relation b
      -> Relation (a, b)
inner =  join query query

left  :: Relation a
      -> Relation b
      -> Relation (a, Maybe b)
left  =  join query queryMaybe

right :: Relation a
      -> Relation b
      -> Relation (Maybe a, b)
right =  join queryMaybe query

full  :: Relation a
      -> Relation b
      -> Relation (Maybe a, Maybe b)
full  =  join queryMaybe queryMaybe

infix 8 `inner'`, `left'`, `right'`, `full'`, `inner`, `left`, `right`, `full`


sqlFromRelation :: PrimeRelation p r -> String
sqlFromRelation =  d  where
  d (SubQuery sub)     = SubQuery.toSQL sub
  d (PrimeRelation qp) = QuerySimple.toSQL qp

instance Show (PrimeRelation p r) where
  show = sqlFromRelation

subQueryFromRelation :: PrimeRelation p r -> SubQuery
subQueryFromRelation =  d  where
  d (SubQuery sub)     = sub
  d (PrimeRelation qp) = QuerySimple.toSubQuery qp

width :: PrimeRelation p r -> Int
width =  SubQuery.width . subQueryFromRelation

nested :: PrimeRelation p r -> PrimeRelation p r
nested =  SubQuery . subQueryFromRelation
