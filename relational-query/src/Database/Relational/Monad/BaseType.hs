{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      : Database.Relational.Monad.BaseType
-- Copyright   : 2015-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines base monad type to build queries.
module Database.Relational.Monad.BaseType
       ( -- * Base monad type to build queries
         ConfigureQuery, configureQuery,
         qualifyQuery, askConfig,

         -- * Relation type
         Relation, unsafeTypeRelation, untypeRelation, untypeRelationNoPlaceholders, relationWidth,

         -- * Object to represent the SQL built by the monads.
         WithPlaceholderOffsets (..),
         SQLWithPlaceholderOffsets,
         SQLWithPlaceholderOffsets',
         attachEmptyPlaceholderOffsets,

         dump,
         sqlFromRelationWith, sqlFromRelation,

         rightPh, leftPh,
       ) where

import Data.Functor.Identity (Identity, runIdentity)
import Control.Applicative ((<$>))
import Control.Arrow

import Database.Record.Persistable (PersistableRecordWidth, unsafePersistableRecordWidth)

import Database.Relational.Internal.String (StringSQL, showStringSQL)
import Database.Relational.Internal.Config (Config, defaultConfig)
import Database.Relational.SqlSyntax (Qualified, SubQuery, PlaceholderOffsets, showSQL, width)

import qualified Database.Relational.Monad.Trans.Qualify as Qualify
import Database.Relational.Monad.Trans.Qualify (Qualify, qualify, evalQualifyPrime)
import Database.Relational.Monad.Trans.Config (QueryConfig, runQueryConfig, askQueryConfig)

data WithPlaceholderOffsets a = WithPlaceholderOffsets
  { placeholderOffsets :: PlaceholderOffsets
  , detachPlaceholderOffsets :: a
  } deriving (Show, Functor)

type SQLWithPlaceholderOffsets' = WithPlaceholderOffsets StringSQL

type SQLWithPlaceholderOffsets = WithPlaceholderOffsets String

attachEmptyPlaceholderOffsets :: a -> WithPlaceholderOffsets a
attachEmptyPlaceholderOffsets = WithPlaceholderOffsets mempty

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
newtype Relation p r = SubQuery (ConfigureQuery (PlaceholderOffsets, SubQuery))

-- | Unsafely type qualified subquery into record typed relation type.
unsafeTypeRelation :: ConfigureQuery (PlaceholderOffsets, SubQuery) -> Relation p r
unsafeTypeRelation = SubQuery

-- | Sub-query Qualify monad from relation.
untypeRelation :: Relation p r -> ConfigureQuery (PlaceholderOffsets, SubQuery)
untypeRelation (SubQuery qps) = qps

untypeRelationNoPlaceholders :: Relation p r -> ConfigureQuery SubQuery
untypeRelationNoPlaceholders (SubQuery qps) = snd <$> qps

-- | 'PersistableRecordWidth' of 'Relation' type.
relationWidth :: Relation p r ->  PersistableRecordWidth r
relationWidth rel =
  unsafePersistableRecordWidth . width $ configureQuery (untypeRelationNoPlaceholders rel) defaultConfig
  ---                               Assume that width is independent from Config structure

unsafeCastPlaceHolder :: Relation a r -> Relation b r
unsafeCastPlaceHolder (SubQuery qps) = SubQuery qps

-- | Simplify placeholder type applying left identity element.
rightPh :: Relation ((), p) r -> Relation p r
rightPh =  unsafeCastPlaceHolder

-- | Simplify placeholder type applying right identity element.
leftPh :: Relation (p, ()) r -> Relation p r
leftPh =  unsafeCastPlaceHolder

-- | Generate SQL string from 'Relation' with configuration.
sqlFromRelationWith :: Relation p r -> Config -> SQLWithPlaceholderOffsets'
sqlFromRelationWith rel cfg =
  uncurry WithPlaceholderOffsets . (`configureQuery` cfg) . (second showSQL <$>) $ untypeRelation rel

-- | SQL string from 'Relation'.
sqlFromRelation :: Relation p r -> SQLWithPlaceholderOffsets'
sqlFromRelation =  (`sqlFromRelationWith` defaultConfig)

-- | Dump internal structure tree.
dump :: Relation p r -> String
dump =  show . (`configureQuery` defaultConfig) . untypeRelationNoPlaceholders

instance Show (Relation p r) where
  show = showStringSQL . detachPlaceholderOffsets . sqlFromRelation
