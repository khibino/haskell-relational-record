{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Projectable.Instances
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides instances between projected terms and SQL terms.
module Database.Relational.Projectable.Instances () where

import Data.Functor.ProductIsomorphic
  (ProductIsoFunctor, (|$|), ProductIsoApplicative, pureP, (|*|),
   ProductIsoEmpty, pureE, peRight, peLeft, )

import Database.Relational.Internal.ContextType
  (Flat, Aggregated, OverWindow)
import qualified Database.Relational.Record as Record
import Database.Relational.Projectable.Unsafe
  (SqlContext (..), OperatorContext, AggregatedContext, PlaceHolders (..))

-- context

-- | Unsafely make 'Record' from SQL terms.
instance SqlContext Flat where
  unsafeProjectSqlTermsWithPlaceholders = Record.unsafeFromSqlTerms

-- | Unsafely make 'Record' from SQL terms.
instance SqlContext Aggregated where
  unsafeProjectSqlTermsWithPlaceholders = Record.unsafeFromSqlTerms

-- | Unsafely make 'Record' from SQL terms.
instance SqlContext OverWindow where
  unsafeProjectSqlTermsWithPlaceholders = Record.unsafeFromSqlTerms

-- | full SQL expression is availabe in Flat context
instance OperatorContext Flat
-- | full SQL expression is availabe in Aggregated context
instance OperatorContext Aggregated

-- | 'Aggregated' context is aggregated context
instance AggregatedContext Aggregated
-- | 'OverWindow' context is aggregated context
instance AggregatedContext OverWindow

-- placeholders

-- | Zipping except for identity element laws against placeholder parameter type.
instance ProductIsoEmpty PlaceHolders () where
  pureE     = PlaceHolders
  peRight _ = PlaceHolders
  peLeft  _ = PlaceHolders

-- | Compose seed of record type 'PlaceHolders'.
instance ProductIsoFunctor PlaceHolders where
  _ |$| PlaceHolders = PlaceHolders

-- | Compose record type 'PlaceHolders' using applicative style.
instance ProductIsoApplicative PlaceHolders where
  pureP _     = PlaceHolders
  _pf |*| _pa = PlaceHolders
