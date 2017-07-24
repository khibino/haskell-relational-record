-- |
-- Module      : Database.Relational.Query.Component
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides untyped components for query.
module Database.Relational.Query.Component
       ( -- * Configuration type for query
         module Database.Relational.Query.Internal.Config,

         -- * Types for aggregation
         AggregateKey,

         -- * Types for ordering
         Order (..), Nulls (..),
       ) where

import Database.Relational.Query.Internal.Config
  (NameConfig (..),
   ProductUnitSupport (..), SchemaNameMode (..), IdentifierQuotation (..),
   Config (..), defaultConfig,)
import Database.Relational.Query.Internal.BaseSQL (Order (..), Nulls (..),)
import Database.Relational.Query.Internal.GroupingSQL (AggregateKey)
