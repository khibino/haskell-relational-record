-- |
-- Module      : Database.Relational.Query
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module is integrated module of Query.
module Database.Relational.Query (
  module Database.Relational.Query.Table,
  module Database.Relational.Query.SQL,
  module Database.Relational.Query.Pi,
  module Database.Relational.Query.Constraint,
  module Database.Relational.Query.Expr,
  module Database.Relational.Query.Sub,
  module Database.Relational.Query.Projection,
  module Database.Relational.Query.Aggregation,
  module Database.Relational.Query.Projectable,
  module Database.Relational.Query.ProjectableExtended,
  module Database.Relational.Query.Monad.Class,
  module Database.Relational.Query.Monad.Trans.Ordering,
  module Database.Relational.Query.Monad.Core,
  module Database.Relational.Query.Monad.Simple,
  module Database.Relational.Query.Monad.Aggregate,
  module Database.Relational.Query.Relation,
  module Database.Relational.Query.Type,
  module Database.Relational.Query.Derives
  ) where

import Database.Relational.Query.Table (Table)
import Database.Relational.Query.SQL (singleKeyUpdateSQL, insertSQL)
import Database.Relational.Query.Pi
import Database.Relational.Query.Constraint
  (Key, tableConstraint, projectionKey,
   uniqueKey, -- notNullKey,
   HasConstraintKey(constraintKey),
   derivedUniqueKey, -- derivedNotNullKey,
   Primary, Unique, NotNull)
import Database.Relational.Query.Expr hiding (flattenMaybe, just)
import Database.Relational.Query.Sub (SubQuery, unitSQL, queryWidth)
import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Aggregation (Aggregation)
import Database.Relational.Query.Projectable
import Database.Relational.Query.ProjectableExtended
import Database.Relational.Query.Monad.Class (on, wheres, groupBy, having)
import Database.Relational.Query.Monad.Trans.Ordering (asc, desc)
import Database.Relational.Query.Monad.Core (QueryCore)
import Database.Relational.Query.Monad.Simple (QuerySimple, SimpleQuery)
import Database.Relational.Query.Monad.Aggregate (QueryAggregate, AggregatedQuery)
import Database.Relational.Query.Relation
import Database.Relational.Query.Type
  (Query, untypeQuery, fromRelation,
   Update, untypeUpdate, typedSingleKeyUpdate,
   Insert, untypeInsert, typedInsert)
import Database.Relational.Query.Derives
