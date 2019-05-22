-- |
-- Module      : Database.Relational.SqlSyntax.Query
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides building and expanding operations of SQL query tree.
module Database.Relational.SqlSyntax.Query (
  flatSubQuery, aggregatedSubQuery,
  union, except, intersect,
  ) where

import Database.Relational.Internal.Config (Config)
import Database.Relational.SqlSyntax.Types
  (Duplication (..), SetOp (..), BinOp (..),
   OrderingTerm, AggregateElem,
   JoinProduct, SubQuery (..),
   Tuple, Guard, )


-- | Unsafely generate flat 'SubQuery' from untyped components.
flatSubQuery :: Config
             -> Tuple
             -> Duplication
             -> JoinProduct
             -> [Guard]
             -> [OrderingTerm]
             -> SubQuery
flatSubQuery = Flat

-- | Unsafely generate aggregated 'SubQuery' from untyped components.
aggregatedSubQuery :: Config
                   -> Tuple
                   -> Duplication
                   -> JoinProduct
                   -> [Guard]
                   -> [AggregateElem]
                   -> [Guard]
                   -> [OrderingTerm]
                   -> SubQuery
aggregatedSubQuery = Aggregated

setBin :: SetOp -> Duplication -> SubQuery -> SubQuery -> SubQuery
setBin op = Bin . BinOp . (,) op

-- | Union binary operator on 'SubQuery'
union     :: Duplication -> SubQuery -> SubQuery -> SubQuery
union     =  setBin Union

-- | Except binary operator on 'SubQuery'
except    :: Duplication -> SubQuery -> SubQuery -> SubQuery
except    =  setBin Except

-- | Intersect binary operator on 'SubQuery'
intersect :: Duplication -> SubQuery -> SubQuery -> SubQuery
intersect =  setBin Intersect
