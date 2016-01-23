{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Query.Monad.Class
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines query building interface classes.
module Database.Relational.Query.Monad.Class
       ( -- * Query interface classes
         MonadQualify (..), MonadRestrict (..),
         MonadQuery (..), MonadAggregate (..), MonadPartition (..),

         all', distinct,
         on, wheres, having,
       ) where

import Database.Relational.Query.Context (Flat, Aggregated)
import Database.Relational.Query.Component (Duplication (..), AggregateKey)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Projectable (PlaceHolders)
import Database.Relational.Query.Monad.BaseType (ConfigureQuery, Relation)


-- | Restrict context interface
class (Functor m, Monad m) => MonadRestrict c m where
  -- | Add restriction to this context.
  restrict :: Projection c (Maybe Bool) -- ^ 'Projection' which represent restriction
           -> m ()                      -- ^ Restricted query context

-- | Query building interface.
class (Functor m, Monad m, MonadQualify ConfigureQuery m) => MonadQuery m where
  -- | Specify duplication.
  setDuplication :: Duplication -> m ()
  -- | Add restriction to last join.
  restrictJoin :: Projection Flat (Maybe Bool) -- ^ 'Projection' which represent restriction
               -> m ()                         -- ^ Restricted query context
  {- Haddock BUG? -}
  -- | Join sub-query with place-holder parameter 'p'. query result is not 'Maybe'.
  query' :: Relation p r
         -> m (PlaceHolders p, Projection Flat r)
  -- | Join sub-query with place-holder parameter 'p'. Query result is 'Maybe'.
  queryMaybe' :: Relation p r
              -> m (PlaceHolders p, Projection Flat (Maybe r))

-- | Lift interface from base qualify monad.
class (Functor q, Monad q, Functor m, Monad m) => MonadQualify q m where
  -- | Lift from qualify monad 'q' into 'MonadQuery' m.
  --   Qualify monad qualifies table form 'SubQuery'.
  liftQualify :: q a -> m a

instance (Functor q, Monad q) => MonadQualify q q where
  liftQualify = id

-- | Aggregated query building interface extends 'MonadQuery'.
class MonadQuery m => MonadAggregate m where
  -- | Add /GROUP BY/ term into context and get aggregated projection.
  groupBy :: Projection Flat r           -- ^ Projection to add into group by
          -> m (Projection Aggregated r) -- ^ Result context and aggregated projection
  -- | Add /GROUP BY/ term into context and get aggregated projection. Non-traditional group-by version.
  groupBy' :: AggregateKey (Projection Aggregated r)  -- ^ Key to aggretate for non-traditional group-by interface
           -> m (Projection Aggregated r)             -- ^ Result context and aggregated projection

-- | Window specification building interface.
class Monad m => MonadPartition c m where
  -- | Add /PARTITION BY/ term into context.
  partitionBy :: Projection c r -> m ()

-- | Specify ALL attribute to query context.
all' :: MonadQuery m => m ()
all' =  setDuplication All

-- | Specify DISTINCT attribute to query context.
distinct :: MonadQuery m => m ()
distinct =  setDuplication Distinct

-- | Add restriction to last join. Projection type version.
on :: MonadQuery m => Projection Flat (Maybe Bool) -> m ()
on =  restrictJoin

-- | Add restriction to this not aggregated query.
wheres :: MonadRestrict Flat m => Projection Flat (Maybe Bool) -> m ()
wheres =  restrict

-- | Add restriction to this aggregated query. Aggregated Projection type version.
having :: MonadRestrict Aggregated m => Projection Aggregated (Maybe Bool) -> m ()
having =  restrict
