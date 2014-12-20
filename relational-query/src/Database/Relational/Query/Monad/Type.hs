-- |
-- Module      : Database.Relational.Query.Monad.Type
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines core query type.
module Database.Relational.Query.Monad.Type (
  -- * Core query monad
  ConfigureQuery, configureQuery, qualifyQuery, askConfig, QueryCore, extractCore,
  OrderedQuery, OrderedQuery',
  ) where

import Data.Functor.Identity (Identity, runIdentity)

import Database.Relational.Query.Component (Config, Duplication, QueryRestriction)
import Database.Relational.Query.Sub (Qualified, JoinProduct)
import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Projectable (PlaceHolders)
import qualified Database.Relational.Query.Monad.Trans.Qualify as Qualify
import Database.Relational.Query.Monad.Trans.Qualify (Qualify, qualify, evalQualifyPrime)
import Database.Relational.Query.Monad.Trans.Config (QueryConfig, runQueryConfig, askQueryConfig)
import Database.Relational.Query.Monad.Trans.Join (QueryJoin, extractProduct)
import Database.Relational.Query.Monad.Trans.Restricting (Restrictings, extractRestrict)
import Database.Relational.Query.Monad.Trans.Ordering (Orderings)

-- | Thin monad type for untyped structure.
type ConfigureQuery = Qualify (QueryConfig Identity)

-- | Run 'ConfigureQuery' monad with initial state to get only result.
configureQuery :: ConfigureQuery q -> Config -> q
configureQuery cq c = runIdentity $ runQueryConfig (evalQualifyPrime cq) c

-- | Get qualifyed table form query.
qualifyQuery :: a -> ConfigureQuery (Qualified a)
qualifyQuery =  Qualify.qualifyQuery

-- | Read configuration.
askConfig :: ConfigureQuery Config
askConfig =  qualify askQueryConfig

-- | Core query monad type used from flat(not-aggregated) query and aggregated query.
type QueryCore = Restrictings Flat (QueryJoin ConfigureQuery)

-- | Extract 'QueryCore' computation.
extractCore :: QueryCore a
            -> ConfigureQuery (((a, QueryRestriction Flat), JoinProduct), Duplication)
extractCore =  extractProduct . extractRestrict

-- | OrderedQuery monad type. Projection must be the same as 'Orderings' context type parameter 'c'.
type OrderedQuery c m r = Orderings c m (Projection c r)

-- | OrderedQuery' monad type with placeholder type 'p'. Projection must be the same as 'Orderings' context type parameter 'c'.
type OrderedQuery' c m p r = Orderings c m (PlaceHolders p, Projection c r)
