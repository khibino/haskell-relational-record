-- |
-- Module      : Database.Relational.Query.Monad.BaseType
-- Copyright   : 2015-2017 Kei Hibino
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
         qualifyQuery, askConfig,

         -- * Relation type
         Relation, unsafeTypeRelation, untypeRelation,

         dump,
         sqlFromRelationWith, sqlFromRelation,

         rightPh, leftPh,
       ) where

import Data.Functor.Identity (Identity, runIdentity)
import Control.Applicative ((<$>))

import Database.Relational.Query.Internal.Config (Config, defaultConfig)
import Database.Relational.Query.Internal.SQL (StringSQL, showStringSQL)
import Database.Relational.Query.Sub (Qualified, SubQuery, showSQL)
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


-- | Relation type with place-holder parameter 'p' and query result type 'r'.
newtype Relation p r = SubQuery (ConfigureQuery SubQuery)

-- | Unsafely type qualified subquery into record typed relation type.
unsafeTypeRelation :: ConfigureQuery SubQuery -> Relation p r
unsafeTypeRelation = SubQuery

-- | Sub-query Qualify monad from relation.
untypeRelation :: Relation p r -> ConfigureQuery SubQuery
untypeRelation (SubQuery qsub) = qsub

unsafeCastPlaceHolder :: Relation a r -> Relation b r
unsafeCastPlaceHolder (SubQuery qsub) = SubQuery qsub

-- | Simplify placeholder type applying left identity element.
rightPh :: Relation ((), p) r -> Relation p r
rightPh =  unsafeCastPlaceHolder

-- | Simplify placeholder type applying right identity element.
leftPh :: Relation (p, ()) r -> Relation p r
leftPh =  unsafeCastPlaceHolder

-- | Generate SQL string from 'Relation' with configuration.
sqlFromRelationWith :: Relation p r -> Config -> StringSQL
sqlFromRelationWith =  configureQuery . (showSQL <$>) . untypeRelation

-- | SQL string from 'Relation'.
sqlFromRelation :: Relation p r -> StringSQL
sqlFromRelation =  (`sqlFromRelationWith` defaultConfig)

-- | Dump internal structure tree.
dump :: Relation p r -> String
dump =  show . (`configureQuery` defaultConfig) . untypeRelation

instance Show (Relation p r) where
  show = showStringSQL . sqlFromRelation
