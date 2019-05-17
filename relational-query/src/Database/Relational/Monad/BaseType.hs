{-# LANGUAGE OverloadedStrings #-}

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
         Relation, unsafeTypeRelation, untypeRelation, relationWidth,

         -- * Generate a record representing placeholders
         defaultPlaceholders, pwPlaceholders,

         dump,
         sqlFromRelationWith, sqlFromRelation,
       ) where

import Data.Functor.Identity (Identity, runIdentity)
import Data.DList (fromList)
import Control.Arrow ((&&&))

import Database.Record.Persistable
  (PersistableRecordWidth, PersistableWidth, persistableWidth, unsafePersistableRecordWidth, runPersistableRecordWidth)
import Database.Relational.Projectable.Unsafe (unsafeProjectSqlTermsWithPlaceholders)
import Database.Relational.Projectable.Instances ()

import Database.Relational.Internal.String (showStringSQL)
import Database.Relational.Internal.Config (Config, defaultConfig)
import Database.Relational.Internal.ContextType (PureOperand)
import Database.Relational.SqlSyntax
  (Qualified, SubQuery, Record, SQLWithPlaceholderOffsets',
   showSQL, width, collectPlaceholderOffsets, detachPlaceholderOffsets, withPlaceholderOffsets)

import qualified Database.Relational.Monad.Trans.Qualify as Qualify
import Database.Relational.Monad.Trans.ReadPlaceholders.Type
  (ReadPlaceholders, runReadPlaceholders)
import Database.Relational.Monad.Trans.Qualify (Qualify, qualify, evalQualifyPrime)
import Database.Relational.Monad.Trans.Config (QueryConfig, runQueryConfig, askQueryConfig)

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
newtype Relation p r = SubQuery (ReadPlaceholders p ConfigureQuery SubQuery)

-- | Unsafely type qualified subquery into record typed relation type.
unsafeTypeRelation :: ReadPlaceholders p ConfigureQuery SubQuery -> Relation p r
unsafeTypeRelation = SubQuery

-- | Sub-query Qualify monad from relation.
untypeRelation :: Relation p r -> Record PureOperand p -> ConfigureQuery SubQuery
untypeRelation (SubQuery qsub) = runReadPlaceholders qsub

-- | 'PersistableRecordWidth' of 'Relation' type.
relationWidth :: Relation p r -> Record PureOperand p -> PersistableRecordWidth r
relationWidth rel phs =
  unsafePersistableRecordWidth
    . width
    . (`configureQuery` defaultConfig) --- Assume that width is independent from Config structure
    $ (untypeRelation rel phs)

-- | Generate SQL string from 'Relation' with configuration.
sqlFromRelationWith :: Relation p r -> Record PureOperand p -> Config -> SQLWithPlaceholderOffsets'
sqlFromRelationWith r p c =
  uncurry withPlaceholderOffsets
    . (collectPlaceholderOffsets &&& showSQL)
    . (`configureQuery` c)
    $ untypeRelation r p

-- | SQL string from 'Relation'.
sqlFromRelation :: Relation p r -> Record PureOperand p -> SQLWithPlaceholderOffsets'
sqlFromRelation r p =  sqlFromRelationWith r p defaultConfig

-- | Dump internal structure tree.
dump :: PersistableWidth p => Relation p r -> String
dump =  show . (`configureQuery` defaultConfig) . (`untypeRelation` defaultPlaceholders)

instance PersistableWidth p => Show (Relation p r) where
  show = showStringSQL . detachPlaceholderOffsets . (`sqlFromRelation` defaultPlaceholders)

defaultPlaceholders :: PersistableWidth t => Record PureOperand t
defaultPlaceholders = pwPlaceholders persistableWidth

pwPlaceholders :: PersistableRecordWidth a
               -> Record PureOperand a
pwPlaceholders pw =
  unsafeProjectSqlTermsWithPlaceholders . withPlaceholderOffsets phs $ replicate w "?"
 where
  w = runPersistableRecordWidth pw
  phs = fromList [0 .. (w - 1)]
