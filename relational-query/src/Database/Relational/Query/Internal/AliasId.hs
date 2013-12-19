-- |
-- Module      : Database.Relational.Query.Internal.AliasId
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines qualifier for SQL table form.
-- Contains internal structure and unsafe interfaces.
module Database.Relational.Query.Internal.AliasId (
  -- * Alias identifier definition
  AliasId (AliasId), primeAlias, newAliasId,

  unsafeExtractAliasId,
  ) where

-- | Alias id definition
newtype AliasId = AliasId Int deriving (Show, Eq)


-- | Unsafely get 'AliasId' internal 'Int' value.
unsafeExtractAliasId :: AliasId -> Int
unsafeExtractAliasId (AliasId i) = i

-- | Initial value of 'AliasId'
primeAlias :: AliasId
primeAlias =  AliasId 0

-- | New unique 'AliasId'.
newAliasId :: AliasId -> AliasId
newAliasId (AliasId i) = AliasId $ i + 1
