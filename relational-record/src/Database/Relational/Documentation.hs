-- |
-- Module      : Database.Relational.Documentation
-- Copyright   : 2014-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module is documentation module for relational-record.
-- The project page of relational-record is
-- http://khibino.github.io/haskell-relational-record/ .
module Database.Relational.Documentation (
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

  distinct,
  all',

  (<-#),

  -- *** Direct Join Operators
  -- $directJoin
  inner, left, right, full,
  on',

  JoinRestriction,

  -- *** Finalize Context
  -- $finalize
  Relation,
  relation,
  aggregateRelation,

  UpdateTarget, updateTarget,
  Restriction, restriction,

  -- ** Record
  -- $record

  -- *** Record Type
  -- $recordType
  Record,

  Flat, Aggregated,

  Exists, OverWindow,

  -- *** Projection Path
  -- $projectionPath
  Pi, (!), (<.>),

  -- *** Overloaded Projection
  -- $overloadedProjection

  -- *** Record Operators
  -- $recordOperators
  ShowConstantTermsSQL,

  value, values,

  (.=.), (.<.), (.<=.), (.>.), (.>=.), (.<>.),

  and', or', in',

  (.||.), like, like',
  (.+.), (.-.), (.*.), (./.),

  isNothing, isJust, fromMaybe,

  not', exists,

  negate', fromIntegral', showNum,

  casesOrElse, case',

  -- ** Aggregate and Window Functions
  -- $aggregateFunctions
  count,
  sum', avg,
  max', min',
  every, any', some',

  over,

  rank, denseRank, rowNumber, percentRank, cumeDist,

  -- ** Set Operators
  -- $setOperators
  union, except, intersect,

  -- ** Maybe Records
  -- $maybeRecord
  just, flattenMaybe,

  (?!), (?!?),
  (<?.>), (<?.?>),

  (?+?),
  negateMaybe,
  sumMaybe,

  -- ** Placeholders
  -- $placeholders
  placeholder,

  query',
  left',
  relation',
  updateTarget',
  restriction',
  union',

  -- ** Record Mapping
  -- $recordMapping
  ProductConstructor,
  ProductIsoFunctor (..),
  ProductIsoApplicative (..),
  (><),

  -- ** Database Statements
  -- $databaseStatements
  relationalQuery,
  typedInsert,
  typedInsertQuery,
  typedUpdate,
  typedDelete,
  typedKeyUpdate,

  derivedInsert,
  derivedInsertQuery,
  derivedUpdate,
  derivedDelete,

  -- * Database Operations
  -- $databaseOperations

  -- ** Conversion interfaces to communicate with database
  -- $conversionInterfaces
  FromSql, ToSql,
  RecordFromSql, RecordToSql,

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
  runQuery',

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

import Data.Functor.ProductIsomorphic
import Database.Relational
import Database.Record (RecordFromSql, FromSql, RecordToSql, ToSql)
import Database.HDBC.Record

{- $concepts
User interface of Relational Record has main two part of modules.

[@"Database.Relational"@] Relational Query Building DSL

[@"Database.Record" and "Database.HDBC.Record"@] Database Operation Actions
 -}

{- $queryBuildingDSL
Relational Query ("Database.Relational") module
defines Typed DSL to build complex SQL query.
 -}

{- $monadicContext
On building query, query structures can be accumulated in monadic context.
 -}

{- $monadicOperator
Some operators are defined to build query structures in monadic context.

'query' and 'queryMaybe' operators grow query product of monadic context like join operation of SQL.
'on' operator appends a new condition into recent join product condition.

'groupBy' operator aggregates flat record value, and can be used only in 'MonadAggregate' context.

'wheres' and 'having' operators appends a new condition into whole query condition.
'having' only accepts aggregated record value, and can be used only in 'MonadRestrict' 'Aggregated' context.

'distinct' operator and 'all'' operator specify SELECT DISTINCT or SELECT ALL, the last specified in monad is used.

'<-#' operator assigns update target column and record value to build update statement structure.
 -}

{- $directJoin
Not monadic style join is supported by some direct join operators.

'inner', 'left', 'right', 'full' operators can construct join products directly like SQL.
'inner' operator is INNER JOIN of SQL, 'left' operator is LEFT OUTER JOIN of SQL, and so on.
'on'' operator specifies condition of join product.
'JoinRestriction' is the type of lambda form which expresses condition of join product.
 -}

{- $finalize
Several operators are defined to make 'Relation' type with finalizing query monadic context.

'relation' operator finalizes flat (not aggregated) query monadic context,
and 'aggregateRelation' operator finalizes aggregated query monadic context.
Both operator convert monadic context into 'Relation' type,
and finalized 'Relation' can be reused as joining and sub-querying in another queries.

'updateTarget' operator finalize monadic context into 'UpdateTarget' type
which can be used as update statement.

'restriction' operator finalize monadic context into 'Restriction' type
which can be used as delete statement.
 -}

{- $record
SQL expression corresponds to Haskell record phantom type in this DSL.
 -}

{- $recordType
'Record' /c/ /a/ is projected SQL value type corresponding to Haskell record type /a/ with context type /c/.

'Flat' is not aggregated query context type,
'Aggregated' is aggregated query context type,
'OverWindow' is window function context type, and so on.

Module "Database.Relational.Context" contains documentation of other context types.
 -}

{- $projectionPath
'!' operator is record value selector using projection path type 'Pi' /r0/ /r1/.
'Pi' /r0/ /r1/ is projection path type selecting column type /r1/ from record type /r0/.
'<.>' operator makes composed projection path from two projection paths.
'fst'' and 'snd'' are projection paths for pair type.
 -}

{- $overloadedProjection
On newer or equal GHC 8.0, overloaded projections are supported.
So you can use projections like below:

@
   a ! \#foo .=. b ! \#bar
@

instead of:

@
   a ! A.foo' .=. b ! B.bar'
@

Funtion application style is also available:

@
   \#foo a .=. \#bar b
@

@ \#fst @ and @ \#snd @ are overloaded-projection for pair type.
 -}

{- $recordOperators
Some operators are defined to calculate record values.

For example,
'value' operator lifts from Haskell value into 'Record' corresponding SQL row value,
which conversion is implicitly specified by 'ShowConstantTermsSQL' class.
Generic programming with default signature is available to define instances of 'ShowConstantTermsSQL'.

'values' operator converts from Haskell list value into 'RecordList', corresponding SQL set value,
'.=.' operator is equal compare operation of record value correspond to SQL =,
'.+.' operator is plus operation of record value correspond to SQL +, and so on.

Module "Database.Relational.Projectable" contains documentation of other record operators.
 -}

{- $aggregateFunctions
Typed aggregate function operators are defined.
Aggregated value types is distinguished with Flat value types.

For example,
'sum'' operator is aggregate function of flat (not aggregated) record value
correspond to SQL SUM(...),
'rank' operator is window function of record value correspond to SQL RANK(), and so on.

To convert window function result into normal record, use the 'over' operator with built 'Window' monad.

Module "Database.Relational.Projectable" contains documentation of
other aggregate function operators and window function operators.
 -}

{- $setOperators
Several operators are defined to manipulate relation set.

'union' operator makes union relation set of two relation set correspond to SQL UNION.
'except' operator makes difference relation set of two relation set correspond to SQL EXCEPT.
'intersect' operator makes intersection relation set of two relation set correspond to SQL INTERSECT.
-}

{- $maybeRecord
Some operators are provided to manage records with 'Maybe' phantom type.

'just' operator creates 'Maybe' typed record,
'flattenMaybe' operator joins nested 'Maybe' typed record.

'Maybe' type flavor of operators against projection path, record and aggregation are also provided.

For example,
'?!' operator is maybe flavor of '!',
'<?.>' operator is maybe flavor of '<.>'.
'?!?' operator and '<?.?>' operator 'join' two 'Maybe' phantom functors.

'?' is same as '?!', which is assumed to use with overloaded-projection like @ (? \#foo) @.
'??' is same as '?!?', which is assumed to use with overloaded-projection like @ (?? \#foo) @.

'?+?' operator is maybe flavor of '.+.',
'negateMaybe' operator is maybe flavor of 'negate'',
'sumMaybe' operator is maybe flavor of 'sum''.

Module "Database.Relational.Projectable" and "Database.Relational.ProjectableExtended"
contain documentation of other 'Maybe' flavor operators.
 -}

{- $placeholders
'placeholders' operator takes
a lambda-form which argument is 'Record' typed placeholders and its scope is restricted by that lambda-form
and then creates dummy value with 'Placeholders' typed which propagate placeholder type information into 'Relation' layer.

Placeholders' flavor of operators against query operation and set operation are also provided, to realize type safe placeholders.

'query'', 'left'', 'relation'', 'updateTarget'', 'restriction'', and 'union''
operator are placeholders' flavor 'query', 'left', 'relation', 'updateTarget', 'restriction' and 'union'.

Module "Database.Relational.Relation" and "Database.Relational.Effect"
contains documentation of other placeholders' flavor operators.
-}

{- $recordMapping
Applicative style record mapping is supported, for 'Record', 'Pi' and 'PlaceHolders'.
'|$|' operator can be used on 'ProductIsoFunctor' context, and
'|*|' operator can be used on 'ProductIsoApplicative' context with 'ProductConstructor',
like /Foo |$| record1 |*| record2 |*| record3/
, /Foo |$| placeholders1 |*| placeholders2 |*| placeholders3/, and so on.

'><' operator constructs pair result. /x >< y/ is the same as /(,) |$| x |*| y/.
 -}

{- $databaseStatements
Some functions are defined to expand query structure
into flat SQL statements to be used by database operation.

'relationalQuery' function converts 'Relation' type info flat SQL query like SELECT statement.

'typedInsert' function converts 'Pi' key type info flat SQL INSERT statement.

'typedInsertQuery' function converts 'Pi' key type and 'Relation' type info flat SQL INSERT ... SELECT ... statement.

'typedUpdate' function converts 'UpdateTarget' type into flat SQL UPDATE statement.

'typedDelete' function converts 'Restriction' into flat SQL DELETE statement.

'typedKeyUpdate' function converts 'Pi' key type info flat SQL UPDATE statement.

Some handy table type inferred functions are provided,
'derivedInsert', 'derivedInsertQuery', 'derivedUpdate' and 'derivedDelete'.
 -}


{- $databaseOperations
Some HDBC actions are defined for database side effects.
 -}

{- $conversionInterfaces
Some record conversion interfaces are defined to communicate with database.

The conversions are implicitly specified by 'FromSql' class and 'ToSql' class.
Generic programming with default signature is available to define instances of 'FromSql' and 'ToSql'.

The explicit definitions correnponsing those classes are 'RecordFromSql' and 'RecordToSql'.

-}

{- $generalizedStmt
Actions to manage generalized SQL statements.
 -}

{- $select
Actions to manage SELECT statements.

'runQuery' function is lazy-read and 'runQuery'' function is strict version,
please use carefully.
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
