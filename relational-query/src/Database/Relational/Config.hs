-- |
-- Module      : Database.Relational.Config
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides untyped components for query.
module Database.Relational.Config
       ( -- * Configuration type for query
         module Database.Relational.Internal.Config,
       ) where

import Database.Relational.Internal.Config
  (NameConfig (..),
   ProductUnitSupport (..), SchemaNameMode (..), IdentifierQuotation (..),
   Config (..), defaultConfig,)
