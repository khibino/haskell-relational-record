-- |
-- Module      : Database.Relational.Query.Monad.BaseType
-- Copyright   : 2015 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines base monad type to build queries.
module Database.Relational.Query.Monad.BaseType
       ( -- * Base monad type to build queries
         ConfigureQuery, configureQuery,
         qualifyQuery, askConfig
       ) where

import Data.Functor.Identity (Identity, runIdentity)

import Database.Relational.Query.Component (Config)
import Database.Relational.Query.Sub (Qualified)
import qualified Database.Relational.Query.Monad.Trans.Qualify as Qualify
import Database.Relational.Query.Monad.Trans.Qualify (Qualify, qualify, evalQualifyPrime)
import Database.Relational.Query.Monad.Trans.Config (QueryConfig, runQueryConfig, askQueryConfig)

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
