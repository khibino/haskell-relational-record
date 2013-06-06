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

  -- * Qualified SQL expression
  columnN, aliasName, (<.>), columnFromId,
  asColumnN,
  ) where

import qualified Language.SQL.Keyword as SQL


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

-- | Column name of projection index.
columnN :: Int -> String
columnN i = 'f' : show i

-- | Alias expression string from AliasId.
aliasName :: AliasId -> String
aliasName (AliasId i) = 'T' : show i

-- | Binary operator to qualify.
(<.>) :: String -> String -> String
t <.> n = t ++ '.' : n

-- | Qualified expression from 'AliasId' and projection index.
columnFromId :: AliasId -> Int -> String
columnFromId a i = aliasName a <.> columnN i

-- | Renamed column in SQL expression.
asColumnN :: SQL.Keyword -> Int -> SQL.Keyword
f `asColumnN` n = f `SQL.as` SQL.word (columnN  n)
