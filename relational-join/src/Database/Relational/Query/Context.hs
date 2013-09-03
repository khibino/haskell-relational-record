{-# LANGUAGE EmptyDataDecls #-}

-- |
-- Module      : Database.Relational.Query.Context
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines query context tag types.
module Database.Relational.Query.Context (
  Flat, Aggregated, Exists
  ) where

-- | Type tag for flat (not-aggregated) query
data Flat

-- | Type tag for aggregated query
data Aggregated

-- | Type tag for exists predicate
data Exists
