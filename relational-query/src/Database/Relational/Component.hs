-- |
-- Module      : Database.Relational.Component
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides untyped components for query.
module Database.Relational.Component
       ( -- * Configuration type for query
         module Database.Relational.Internal.Config,

         -- * Types for aggregation
         AggregateKey,

         -- * Types for ordering
         Order (..), Nulls (..),
       ) where

import Database.Relational.Internal.Config
  (NameConfig (..),
   ProductUnitSupport (..), SchemaNameMode (..), IdentifierQuotation (..),
   Config (..), defaultConfig,)
import Database.Relational.Internal.BaseSQL (Order (..), Nulls (..),)
import Database.Relational.Internal.GroupingSQL (AggregateKey)
