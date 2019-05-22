{-# LANGUAGE EmptyDataDecls #-}

-- |
-- Module      : Database.Relational.Internal.ContextType
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines query context tag types.
module Database.Relational.Internal.ContextType (
  Flat, Aggregated, Exists, OverWindow, PureOperand,

  Set, SetList, Power,
  ) where

-- | Type tag for flat (not-aggregated) query
data Flat

-- | Type tag for aggregated query
data Aggregated

-- | Type tag for exists predicate
data Exists

-- | Type tag for window function building
data OverWindow

-- | Type tag for records all of whom values are placeholders (denoted as "?" in the generated SQL) or literal value.
data PureOperand


-- | Type tag for normal aggregatings set
data Set

-- | Type tag for aggregatings GROUPING SETS
data SetList

-- | Type tag for aggregatings power set
data Power
