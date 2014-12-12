-- |
-- Module      : Database.Relational.Query.Documentation
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module is documentation module for relational record.
module Database.Relational.Query.Documentation (
  -- * Concepts
  -- $concepts

  -- ** Relational Query Building DSL
  -- $queryBuildingDSL

  -- *** Monadic Query Context Building
  -- $monadicContext

  -- **** Monadic Operators
  -- $monadicOperator
  query,
  queryMaybe,

  on,
  wheres,

  groupBy,

  having,

  -- **** Direct Join Operators
  -- $directJoin
  inner, left, right, full,
  on',

  -- **** Finalize Context
  -- $finalize
  relation,
  aggregateRelation,

  -- *** Projection
  -- $projection

  -- **** Projection Type
  -- $projectionType
  Projection,

  Flat, Aggregated,

  Exists, OverWindow,

  -- **** Projection Path
  -- $projectionPath
  (!),

  -- **** Projection Operators
  -- $projectionOperators
  (.=.), (.<.), (.<=.), (.>.), (.>=.), (.<>.),
  casesOrElse, case',
  in', and', or',

  isNothing, isJust, fromMaybe,

  not', exists,

  (.||.),
  (.+.), (.-.), (./.), (.*.), negate', fromIntegral', showNum,

  -- *** Aggregate and Window Functions
  -- $aggregateFunctions
  sum', avg,
  max', min',
  every, any', some',

  rank, denseRank, rowNumber, percentRank, cumeDist,

  -- *** Set Operators
  -- $setOperators
  union, except, intersect,

  -- *** Maybe Projections
  -- $maybeProjection
  (?+?),
  negateMaybe,
  sumMaybe,

  -- *** Placeholders
  -- $placeholder
  query',
  left',
  relation',
  union',

  -- ** Database Operations
  -- $databaseOperations

  -- *** Generalized Statement
  -- $generalizedStmt
  prepareNoFetch,
  bind,
  execute,
  executeNoFetch,

  -- *** Select
  -- $select
  prepareQuery,
  fetch,
  runQuery,

  -- *** Insert Values
  -- $insertValue
  prepareInsert,
  runInsert,

  -- *** Insert Select Results
  -- $insertSelect
  prepareInsertQuery,
  runInsertQuery,

  -- *** Update
  -- $update
  prepareUpdate,
  runUpdate,

  -- *** Delete
  -- $delete
  prepareDelete,
  runDelete,

  -- *** Update by Key
  -- $keyUpdate
  prepareKeyUpdate,
  bindKeyUpdate,
  runKeyUpdate,

  ) where

import Database.Relational.Query
import Database.HDBC.Record

{- $concepts
User inferface of Relational Record has main two part of modules.

[@"Database.Relational.Query"@] Relational Query Building DSL

[@"Database.HDBC.Record"@] Database Operation Actions
 -}

{- $queryBuildingDSL
Relatoinal Query ("Database.Relational.Query") module
defines Typed DSL to build complex SQL query.
 -}

{- $monadicContext
This DSL accumulates query structures into monadic context.
 -}

{- $monadicOperator
Some operators are defined to build query structures in monadic context.

'query' and 'queryMaybe' operators grow query product of monadic context like join operation of SQL.
'on' operator appends a new condition into recent join product condition.

'groupBy' operator aggregates flat projection value.

'wheres' and 'having' operators appends a new condition into whole query condition.
'having' only accepts aggregated projection value.
 -}

{- $directJoin
Not monadic style join is supported by some direct join operators.

'inner', 'left', 'right', 'full' operators can construct join products directly like SQL.
'on'' operator specifies condition of join product.
 -}

{- $finalize
Several operators are defined to finalize query monadic context.

'relation' operator finalizes flat (not aggregated) query monadic context,
and 'aggregateRelation' operator finalizes aggregated query monadic context.
 -}

{- $projection
This DSL project SQL value type to haskell type.
 -}

{- $projectionType
'Projection' /c/ /t/ is SQL value type projection to haskell type with context type /c/ correspond haskell type /t/.

'Flat' is not aggregated query context type,
'Aggregated' is aggregated query context type,
'OverWindow' is window function context type, and so on.
 -}

{- $projectionPath
 -}

{- $projectionOperators
Some operators are defined to caluculate projected values.

For example,
'.=.' operator is equal compare operation of projected value correspond to SQL =,
'.+.' operator is plus operation of projected value coresspond to SQL +, and so on.
 -}

{- $aggregateFunctions
Typed aggregate operators are defined.
Aggregated value types is distinguished with Flat value types.

For example,
'sum'' operator is aggregate function of projected flat (not aggregated) value
correspond to SQL SUM(...),
'rank' operator is window function of projected value coresspond to SQL RANK(), and so on.

 -}

{- $setOperators
Several operators are defined to manipulate relation set.

'union' operator makes union relation set of two relation set correspond to SQL UNION.
'except' operator makes difference relation set of two relation set correspond to SQL EXCEPT.
'intersect' operator makes intersection relation set of two relation set correspond to SQL INTERSECT.
-}

{- $maybeProjection
Operators of projection and aggregation are also provided Maybe type versions.

For example,
'?+?' operator is maybe version of '.+.',
'nagateMaybe' operator is maybe version of 'nagate',
'sumMaybe' operator is maybe version of 'sum'', and so on.
 -}

{- $placeholder
Some operators are defined to realize type safe placeholder.
 -}


{- $databaseOperations
Some actions are defined for database site effects.
 -}

{- $generalizedStmt
Actions to manage generalized SQL statements.
 -}

{- $select
Actions to manage SELECT statements.
 -}

{- $insertValue
Actions to manage INSERT ... VALUES ... statements.
 -}

{- $insertSelect
Actions to manage INSERT ... SELECT ... statements.
 -}

{- $update
Actions to manage UPDATE statements.
 -}

{- $delete
Actions to manage DELETE statements.
 -}

{- $keyUpdate
Actions to manage UPDATE statements which updates columns other than specified key of the records selected by specified key.
 -}
