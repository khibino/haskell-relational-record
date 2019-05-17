{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

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

import Database.Relational.Internal.ContextType
  (Flat, Aggregated, OverWindow, PureOperand)
import qualified Database.Relational.Record as Record
import Database.Relational.Projectable.Unsafe
  (SqlContext (..), OperatorContext, AggregatedContext, ResultContext,)

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

-- | Unsafely make 'Record' from SQL terms.
instance SqlContext PureOperand where
  unsafeProjectSqlTermsWithPlaceholders = Record.unsafeFromSqlTerms

-- | full SQL expression is availabe in Flat context
instance OperatorContext Flat
-- | full SQL expression is availabe in Aggregated context
instance OperatorContext Aggregated
-- | full SQL expression is availabe in PureOperand context
instance OperatorContext PureOperand

-- | 'Aggregated' context is aggregated context
instance AggregatedContext Aggregated
-- | 'OverWindow' context is aggregated context
instance AggregatedContext OverWindow

type instance ResultContext Flat Flat = Flat
type instance ResultContext Flat PureOperand = Flat
type instance ResultContext PureOperand Flat = Flat

type instance ResultContext Aggregated Aggregated = Aggregated
type instance ResultContext Aggregated PureOperand = Aggregated
type instance ResultContext PureOperand Aggregated = Aggregated
