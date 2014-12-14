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

  -- * Relational Query Building DSL
  -- $queryBuildingDSL

  -- ** Monadic Query Context Building
  -- $monadicContext

  -- *** Monadic Operators
  -- $monadicOperator
  query,
  queryMaybe,

  on,
  wheres,

  groupBy,

  having,

  (<-#),

  -- *** Direct Join Operators
  -- $directJoin
  inner, left, right, full,
  on',

  -- *** Finalize Context
  -- $finalize
  Relation,
  relation,
  aggregateRelation,

  UpdateTarget, updateTarget,
  Restriction, restriction,

  -- ** Projection
  -- $projection

  -- *** Projection Type
  -- $projectionType
  Projection,

  Flat, Aggregated,

  Exists, OverWindow,

  -- *** Projection Path
  -- $projectionPath
  Pi, (!), (<.>),

  -- *** Projection Operators
  -- $projectionOperators
  (.=.), (.<.), (.<=.), (.>.), (.>=.), (.<>.),
  casesOrElse, case',
  in', and', or',

  isNothing, isJust, fromMaybe,

  not', exists,

  (.||.),
  (.+.), (.-.), (./.), (.*.), negate', fromIntegral', showNum,

  -- ** Aggregate and Window Functions
  -- $aggregateFunctions
  sum', avg,
  max', min',
  every, any', some',

  rank, denseRank, rowNumber, percentRank, cumeDist,

  -- ** Set Operators
  -- $setOperators
  union, except, intersect,

  -- ** Maybe Projections
  -- $maybeProjection
  (?!), (?!?),
  (<?.>), (<?.?>),

  (?+?),
  negateMaybe,
  sumMaybe,

  -- ** Placeholders
  -- $placeholder
  query',
  left',
  relation',
  updateTarget',
  restriction',
  union',

  -- ** Database Statements
  -- $databaseStatements
  relationalQuery,
  typedInsert,
  typedUpdate,
  typedDelete,
  typedKeyUpdate,

  -- * Database Operations
  -- $databaseOperations

  -- ** Generalized Statement
  -- $generalizedStmt
  prepareNoFetch,
  bind,
  execute,
  executeNoFetch,

  -- ** Select
  -- $select
  prepareQuery,
  fetch,
  runQuery,

  -- ** Insert Values
  -- $insertValue
  prepareInsert,
  runInsert,

  -- ** Insert Select Results
  -- $insertSelect
  prepareInsertQuery,
  runInsertQuery,

  -- ** Update
  -- $update
  prepareUpdate,
  runUpdate,

  -- ** Delete
  -- $delete
  prepareDelete,
  runDelete,

  -- ** Update by Key
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
On building query, query structures can be accumulated in monadic context.
 -}

{- $monadicOperator
Some operators are defined to build query structures in monadic context.

'query' and 'queryMaybe' operators grow query product of monadic context like join operation of SQL.
'on' operator appends a new condition into recent join product condition.

'groupBy' operator aggregates flat projection value.

'wheres' and 'having' operators appends a new condition into whole query condition.
'having' only accepts aggregated projection value.

'(<-#)' operator assigns update target column and projection value to build update statement structure.
 -}

{- $directJoin
Not monadic style join is supported by some direct join operators.

'inner', 'left', 'right', 'full' operators can construct join products directly like SQL.
'inner' operator is INNER JOIN of SQL, 'left' operator is LEFT OUTER JOIN of SQL, and so on.
'on'' operator specifies condition of join product.
 -}

{- $finalize
Several operators are defined to make 'Relation' type with finalizing query monadic context.

'relation' operator finalizes flat (not aggregated) query monadic context,
and 'aggregateRelation' operator finalizes aggregated query monadic context.
Both operator convert monadic context into 'Relation' type,
and finalized 'Relation' can be reused as joining and sub-quering in another queries.

'updateTarget' operator finalize monadic context into 'UpdateTarget' type
which can be used as update statement.

'restriction' operator finalize monadic context into 'Restriction' type
which can be used as delete statement.
 -}

{- $projection
SQL expression can be projected to haskell phantom type in this DSL.
 -}

{- $projectionType
'Projection' /c/ /t/ is SQL value type projection to haskell type with context type /c/ correspond haskell type /t/.

'Flat' is not aggregated query context type,
'Aggregated' is aggregated query context type,
'OverWindow' is window function context type, and so on.

Module "Database.Relational.Query.Context" contains documentation of other context types.
 -}

{- $projectionPath
'!' operator is projected value selector using projection path type 'Pi' /r0/ /r1/.
'Pi' /r0/ /r1/ is projection path type selecting column type /r1/ from record type /r0/.
'<.>' operator makes composed projection path from two projection paths.
 -}

{- $projectionOperators
Some operators are defined to caluculate projected values.

For example,
'.=.' operator is equal compare operation of projected value correspond to SQL =,
'.+.' operator is plus operation of projected value coresspond to SQL +, and so on.

Module "Database.Relational.Query.Projectable" contains documentation of other projection operators.
 -}

{- $aggregateFunctions
Typed aggregate function operators are defined.
Aggregated value types is distinguished with Flat value types.

For example,
'sum'' operator is aggregate function of projected flat (not aggregated) value
correspond to SQL SUM(...),
'rank' operator is window function of projected value coresspond to SQL RANK(), and so on.

Module "Database.Relational.Query.Projectable" contains documentation of
other aggregate function operators and window function operators.
 -}

{- $setOperators
Several operators are defined to manipulate relation set.

'union' operator makes union relation set of two relation set correspond to SQL UNION.
'except' operator makes difference relation set of two relation set correspond to SQL EXCEPT.
'intersect' operator makes intersection relation set of two relation set correspond to SQL INTERSECT.
-}

{- $maybeProjection
'Maybe' type flavor of operators against projection path, projection and aggregation are also provided.

For example,
'?!' operator is maybe flavor of '!',
'<?.>' operator is maybe flavor of '<.>'.
'?!?' opeartor and '<?.?>' operator 'join' two 'Maybe' phantom functors.

'?+?' operator is maybe flavor of '.+.',
'nagateMaybe' operator is maybe flavor of 'nagate',
'sumMaybe' operator is maybe flavor of 'sum''.

Module "Database.Relational.Query.Projectable" and "Database.Relational.Query.ProjectableExtended"
contain documentation of other 'Maybe' flavor projection operators.
 -}

{- $placeholder
Placeholder flavor of operators against query operation and set operation are also provided, to realize type safe placeholder.

'query'', 'left'', 'relation'', 'updateTarget'', 'restriction'', and 'union''
operator are placeholder flavor 'query', 'left', 'relation', 'updateTarget', 'restriction' and 'union'.

Module "Database.Relational.Query.Relation" and "Database.Relational.Query.Effect"
contains documentation of other placeholder flavor operators.
-}

{- $databaseStatements
Some functions are defined to expand query structure
into flat SQL statements to be used by database operation.

'relationalQuery' function converts 'Relation' type info flat SQL query like SELECT statement.

'typedInsert' function converts 'Pi' key type info flat SQL INSERT statement.

'typedUpdate' function converts 'UpdateTarget' type into flat SQL UPDATE statement.

'typedDelete' function converts 'Restriction' into flat SQL DELETE statement.

'typedKeyUpdate' function converts 'Pi' key type info flat SQL UPDATE statement.
-}


{- $databaseOperations
Some HDBC actions are defined for database side effects.
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
