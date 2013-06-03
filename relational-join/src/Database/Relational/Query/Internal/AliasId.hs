-- |
-- Module      : Database.Relational.Query.AliasId
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
  AliasId (AliasId), primAlias, newAliasId,

  -- * Qualified SQL expression
  columnN, aliasName, (<.>), columnFromId,
  asColumnN,

  -- * Qualified type
  Qualified, unQualify, qualifyAlias, qualify,

  qualifiedSQLas
  ) where

import qualified Language.SQL.Keyword as SQL
import qualified Language.SQL.Keyword.ConcatString as SQLs


-- | Alias id definition
newtype AliasId = AliasId Int deriving (Show, Eq)

-- | Initial value of 'AliasId'
primAlias :: AliasId
primAlias =  AliasId 0


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


-- | Qualified query by AliasId
data Qualified q = Qualified  q !AliasId

-- | Unqualify.
unQualify :: Qualified q -> q
unQualify (Qualified fr _)   = fr

-- | Get 'AliasId' qualifier
qualifyAlias :: Qualified q -> AliasId
qualifyAlias (Qualified _ i) = i

-- | Add AliasId as qualifier.
qualify :: fr -> AliasId -> Qualified fr
qualify =  Qualified

-- | 'Functor' instance of 'Qualified'
instance Functor Qualified where
  fmap f (Qualified a i) = Qualified (f a) i

-- | From 'Qualified' SQL string into 'String'.
qualifiedSQLas :: Qualified String -> String
qualifiedSQLas q =
  unQualify q
  `SQLs.as`
  (aliasName $ qualifyAlias q)
