{-# LANGUAGE FlexibleContexts #-}

module ArrowQuery (
  module Database.Relational.Query,

  query, queryMaybe, query', queryMaybe', wheres, having, groupBy, placeholder,

  relation, relation', aggregateRelation, aggregateRelation',

  QuerySimple, QueryAggregate,
  ) where

import Control.Arrow

import Database.Record
import Database.Relational.Query hiding
  (query, queryMaybe, query', queryMaybe', wheres, having, groupBy, placeholder,
   relation, relation', aggregateRelation, aggregateRelation',
   QuerySimple, QueryAggregate)
import qualified Database.Relational.Query as Query


type QuerySimple    = Kleisli Query.QuerySimple
type QueryAggregate = Kleisli Query.QueryAggregate


query :: MonadQualify ConfigureQuery m =>
         Kleisli m (Query.Relation () r) (Projection Flat r)
query = Kleisli Query.query

queryMaybe :: MonadQualify ConfigureQuery m =>
              Kleisli m (Query.Relation () r) (Projection Flat (Maybe r))
queryMaybe = Kleisli Query.queryMaybe

query' :: MonadQualify ConfigureQuery m =>
         Kleisli m (Query.Relation p r) (PlaceHolders p, Projection Flat r)
query' = Kleisli Query.query'

queryMaybe' :: MonadQualify ConfigureQuery m =>
               Kleisli m (Query.Relation p r) (PlaceHolders p, Projection Flat (Maybe r))
queryMaybe' = Kleisli Query.queryMaybe'

wheres :: MonadRestrict Flat m =>
          Kleisli m (Projection Flat (Maybe Bool)) ()
wheres =  Kleisli Query.wheres

having :: MonadRestrict Aggregated m =>
           Kleisli m (Projection Aggregated (Maybe Bool)) ()
having =  Kleisli Query.having

groupBy :: MonadAggregate m =>
           Kleisli m (Projection Flat r) (Projection Aggregated r)
groupBy =  Kleisli Query.groupBy

placeholder :: (PersistableWidth t, SqlProjectable p, Monad m) =>
               Kleisli m (Kleisli m (p t) a) (PlaceHolders t, a)
placeholder =  Kleisli $ Query.placeholder . runKleisli

relation :: QuerySimple () (Projection Flat r) -> Relation () r
relation =  Query.relation . (`runKleisli` ())

relation' :: QuerySimple () (PlaceHolders p, Projection Flat r)
          -> Relation p r
relation' =  Query.relation' . (`runKleisli` ())

aggregateRelation :: QueryAggregate () (Projection Aggregated r)
                   -> Relation () r
aggregateRelation =  Query.aggregateRelation . (`runKleisli` ())

aggregateRelation' :: QueryAggregate () (PlaceHolders p, Projection Aggregated r)
                   -> Relation p r
aggregateRelation' =  Query.aggregateRelation' . (`runKleisli` ())
