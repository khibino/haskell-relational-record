-- |
-- Module      : Database.Relational
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module is integrated module of Query.
module Database.Relational (
  module Database.Relational.Table,
  module Database.Relational.SimpleSql,
  module Database.Relational.Pi,
  module Database.Relational.Constraint,
  module Database.Relational.Context,
  module Database.Relational.Config,
  module Database.Relational.SqlSyntax,
  module Database.Relational.Record,
  module Database.Relational.ProjectableClass,
  module Database.Relational.Projectable,
  module Database.Relational.TupleInstances,
  module Database.Relational.Monad.BaseType,
  module Database.Relational.Monad.Class,
  module Database.Relational.Monad.Trans.Ordering,
  module Database.Relational.Monad.Trans.Aggregating,
  module Database.Relational.Monad.Trans.Assigning,
  module Database.Relational.Monad.Type,
  module Database.Relational.Monad.Simple,
  module Database.Relational.Monad.Aggregate,
  module Database.Relational.Monad.Restrict,
  module Database.Relational.Monad.Unique,
  module Database.Relational.Monad.Assign,
  module Database.Relational.Monad.Register,
  module Database.Relational.Relation,
  module Database.Relational.Set,
  module Database.Relational.Sequence,
  module Database.Relational.Scalar,
  module Database.Relational.SQL,
  module Database.Relational.Effect,
  module Database.Relational.Derives
  ) where

import Database.Relational.Table (Table, TableDerivable (..))
import Database.Relational.SimpleSql (QuerySuffix, updateOtherThanKeySQL)
import Database.Relational.Pure ()
import Database.Relational.Pi
import Database.Relational.Constraint
  (Key, tableConstraint, projectionKey,
   uniqueKey, -- notNullKey,
   HasConstraintKey(constraintKey),
   derivedUniqueKey, -- derivedNotNullKey,
   Primary, Unique, NotNull)
import Database.Relational.Context
import Database.Relational.Config
import Database.Relational.SqlSyntax
  (Order (..), Nulls (..), AggregateKey, SubQuery, unitSQL, queryWidth)
import Database.Relational.Record (Record, Predicate, PI, RecordList, list)
import Database.Relational.ProjectableClass
import Database.Relational.Projectable
import Database.Relational.TupleInstances
import Database.Relational.Monad.BaseType
import Database.Relational.Monad.Class
  (MonadQualify,
   MonadRestrict, wheres, having, restrict,
   MonadAggregate, groupBy, groupBy',
   MonadQuery, query', queryMaybe',
   MonadPartition, partitionBy,
   distinct, all', on)
import Database.Relational.Monad.Trans.Ordering
  (Orderings, orderBy', orderBy, asc, desc)
import Database.Relational.Monad.Trans.Aggregating
  (key, key', set, bkey, rollup, cube, groupingSets)
import Database.Relational.Monad.Trans.Assigning (assignTo, (<-#))
import Database.Relational.Monad.Type
import Database.Relational.Monad.Simple (QuerySimple, SimpleQuery)
import Database.Relational.Monad.Aggregate
  (QueryAggregate, AggregatedQuery, Window, over)
import Database.Relational.Monad.Restrict (Restrict)
import Database.Relational.Monad.Unique (QueryUnique)
import Database.Relational.Monad.Assign (Assign)
import Database.Relational.Monad.Register (Register)
import Database.Relational.Relation
import Database.Relational.Set
import Database.Relational.Sequence
import Database.Relational.Scalar (ScalarDegree)
import Database.Relational.SQL hiding
  (unsafeTypedKeyUpdate, unsafeTypedDelete,
   unsafeTypedInsert, unsafeTypedInsertQuery, )
import Database.Relational.Effect
  (Restriction, restriction, restriction',

   UpdateTarget, updateTarget',
   liftTargetAllColumn, liftTargetAllColumn',

   InsertTarget, insertTarget', piRegister,

   sqlChunkFromInsertTarget,
   sqlFromInsertTarget,
   sqlChunksFromRecordList,

   insertTarget, updateTarget,
   updateTargetAllColumn, updateTargetAllColumn',
   sqlWhereFromRestriction,
   sqlFromUpdateTarget,
  )
import Database.Relational.Derives

import Database.Record.Instances ()

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
