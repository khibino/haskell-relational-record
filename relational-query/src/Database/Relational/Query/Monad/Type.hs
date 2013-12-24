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
  ConfigureQuery, configureQuery, qualifyQuery, QueryCore, extractCore
  ) where

import Database.Relational.Query.Component (Config, Duplication, QueryRestriction)
import Database.Relational.Query.Sub (Qualified, JoinProduct)
import Database.Relational.Query.Context (Flat)
import qualified Database.Relational.Query.Monad.Qualify as Qualify
import Database.Relational.Query.Monad.Qualify (Qualify, evalQualifyPrime)
import Database.Relational.Query.Monad.Trans.Config (QueryConfig, runQueryConfig, config)
import Database.Relational.Query.Monad.Trans.Join (QueryJoin, extractProduct)
import Database.Relational.Query.Monad.Trans.Restricting (Restrictings, extractRestrict)


-- | Thin monad type for untyped structure.
type ConfigureQuery = QueryConfig Qualify

-- | Run 'ConfigureQuery' monad with initial state to get only result.
configureQuery :: ConfigureQuery c -> Config -> c
configureQuery c = evalQualifyPrime . runQueryConfig c

-- | Get qualifyed table form query.
qualifyQuery :: a -> ConfigureQuery (Qualified a)
qualifyQuery =  config . Qualify.qualifyQuery

-- | Core query monad type used from flat(not-aggregated) query and aggregated query.
type QueryCore = Restrictings Flat (QueryJoin ConfigureQuery)

-- | Extract 'QueryCore' computation.
extractCore :: QueryCore a
            -> ConfigureQuery ((a, QueryRestriction Flat), (JoinProduct, Duplication))
extractCore =  extractProduct . extractRestrict
