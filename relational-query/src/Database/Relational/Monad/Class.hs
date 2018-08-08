{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Class
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines query building interface classes.
module Database.Relational.Monad.Class
       ( -- * Query interface classes
         MonadPlaceholder(..), MonadQualify (..), MonadRestrict (..),
         MonadQuery (..), MonadAggregate (..), MonadPartition (..),

         all', distinct,
         on, wheres, having,
       ) where

import Control.Monad.Indexed.Trans (IxMonadTrans)

import Database.Relational.Internal.ContextType (Flat, Aggregated)
import Database.Relational.SqlSyntax
  (Duplication (..), Predicate, Record, AggregateKey)

import Database.Relational.Projectable (PlaceHolders)
import Database.Relational.Monad.BaseType (ConfigureQuery, Relation)


class IxMonadTrans t => MonadPlaceholder t where
  dropPlaceholder :: Monad m => Record i j c r -> t m i j ()

-- | Restrict context interface
-- igrep TODO: Add index parameter to the monad
class (Functor m, Monad m) => MonadRestrict c m where
  -- | Add restriction to this context.
  restrict :: Predicate i j c -- ^ 'Record' which represent restriction
           -> m ()        -- ^ Restricted query context

-- | Query building interface.
class (Functor m, Monad m, MonadQualify ConfigureQuery m) => MonadQuery m where
  -- | Specify duplication.
  setDuplication :: Duplication -> m ()
  -- | Add restriction to last join.
  -- but actually required?
  -- igrep TODO: Add index parameter to the monad
  restrictJoin :: Predicate i j Flat -- ^ 'Record' which represent restriction
               -> m ()           -- ^ Restricted query context
  {- Haddock BUG? -}
  -- | Join sub-query with place-holder parameter 'p'. query result is not 'Maybe'.
  query' :: Relation p r
         -> m (PlaceHolders p, Record i j Flat r)
  -- | Join sub-query with place-holder parameter 'p'. Query result is 'Maybe'.
  queryMaybe' :: Relation p r
              -> m (PlaceHolders p, Record i j Flat (Maybe r))

-- | Lift interface from base qualify monad.
class (Functor q, Monad q, Functor m, Monad m) => MonadQualify q m where
  -- | Lift from qualify monad 'q' into 'MonadQuery' m.
  --   Qualify monad qualifies table form 'SubQuery'.
  liftQualify :: q a -> m a

instance (Functor q, Monad q) => MonadQualify q q where
  liftQualify = id

-- | Aggregated query building interface extends 'MonadQuery'.
class MonadQuery m => MonadAggregate m where
  -- | Add /GROUP BY/ term into context and get aggregated record.
  -- igrep TODO: Add index parameter to the monad
  groupBy :: Record i j Flat r           -- ^ Record to add into group by
          -> m (Record i j Aggregated r) -- ^ Result context and aggregated record
  -- | Add /GROUP BY/ term into context and get aggregated record. Non-traditional group-by version.
  groupBy' :: AggregateKey (Record i j Aggregated r)  -- ^ Key to aggretate for non-traditional group-by interface
           -> m (Record i j Aggregated r)             -- ^ Result context and aggregated record

-- | Window specification building interface.
class Monad m => MonadPartition c m where
  -- | Add /PARTITION BY/ term into context.
  -- igrep TODO: Add index parameter to the monad
  partitionBy :: Record i j c r -> m ()

-- | Specify ALL attribute to query context.
all' :: MonadQuery m => m ()
all' =  setDuplication All

-- | Specify DISTINCT attribute to query context.
distinct :: MonadQuery m => m ()
distinct =  setDuplication Distinct

-- | Add restriction to last join. Record type version.
on :: MonadQuery m => Predicate i j Flat -> m ()
on =  restrictJoin

-- | Add restriction to this not aggregated query.
wheres :: MonadRestrict Flat m => Predicate i j Flat -> m ()
wheres =  restrict

-- | Add restriction to this aggregated query. Aggregated Record type version.
having :: MonadRestrict Aggregated m => Predicate i j Aggregated -> m ()
having =  restrict
