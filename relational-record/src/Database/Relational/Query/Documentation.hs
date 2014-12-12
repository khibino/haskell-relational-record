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
  on,
  wheres,

  groupBy,

  having,

  -- **** Direct Join Operators
  -- $directJoin

  -- **** Finalize Context
  -- $finalize
  relation,
  aggregateRelation,

  -- *** Projection Operators
  -- $projectionOperators
  (.=.), (.<>.),

  -- *** Aggregate Functions
  -- $aggregateFunctions
  sum',

  -- *** Set Operators
  -- $setOperators
  union,

  -- ** Database Operations
  -- $databaseOperations
  bind,

  ) where

import Database.Relational.Query
import Database.HDBC.Record

{- $concepts
User inferface of Relational Record has main two part of modules.
 * Relational Query Building DSL - "Database.Relational.Query"
 * Database Operation  - "Database.HDBC.Record"
 -}

{- $queryBuildingDSL
Relatoinal Query ("Database.Relational.Query") module
defines Typed DSL to build complex SQL query.
 -}

{- $monadicContext
This DSL accumulates query structures into monadic context.
 -}

{- $monadicOperator
Some operators are defined to update query structures in monadic context.
 -}

{- $directJoin
Not monadic style join is supported by some direct join operators.
 -}

{- $finalize
Several operators are defined to finalize flat and aggregated monadic context.
 -}

{- $projectionOperators
Some operators are defined to caluculate projections.
 -}

{- $aggregateFunctions
Typed aggregate operators are defined.
Aggregated value types is distinguished with Flat value types.
 -}

{- $setOperators
Several operators are defined to manipulate relation set.
 -}

{- $databaseOperations
Some actions are defined for database site effects.
 -}
