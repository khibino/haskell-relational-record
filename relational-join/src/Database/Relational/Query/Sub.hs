-- |
-- Module      : Database.Relational.Query.Sub
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines sub-query structure used in query products.
module Database.Relational.Query.Sub (
  -- * Sub-query
  SubQuery, fromTable, subQuery, toSQL, unitSQL, width,

  -- * Qualified Sub-query
  queryWidth, qualifiedForm,

  -- * Sub-query columns
  column, columnExpr
  ) where

import Database.Relational.Query.Table (Table, (!))
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Internal.AliasId
  (aliasName, (<.>),
   Qualified, unQualify, qualifyAlias, qualifiedSQLas)
import qualified Database.Relational.Query.Internal.AliasId as AliasId
import Database.Relational.Query.Expr.Unsafe (Expr(Expr))


-- | Sub-query type
data SubQuery = Table Table.Untyped
              | SubQuery
                { sql'   :: String
                , width' :: !Int
                }

-- | 'SubQuery' from 'Table'.
fromTable :: Table r -> SubQuery
fromTable =  Table . Table.unType

-- | 'SubQuery' from SQL.
subQuery :: String -> Int -> SubQuery
subQuery =  SubQuery

-- | Width of 'SubQuery'.
width :: SubQuery -> Int
width =  d  where
  d (Table u)                 = Table.width' u
  d (SubQuery { width' = w }) = w

-- | SQL string for nested-query and toplevel-SQL.
toSQLs :: SubQuery -> (String, String)
toSQLs =  d  where
  d (Table u)               = (Table.name' u, Table.fromTableToSql u)
  d (SubQuery { sql' = q }) = ('(' : q ++ [')'], q)

-- | SQL string for nested-qeury.
unitSQL :: SubQuery -> String
unitSQL =  fst . toSQLs

-- | SQL string for toplevel-SQL.
toSQL :: SubQuery -> String
toSQL =  snd . toSQLs

-- | Width of 'Qualified' 'SubQUery'.
queryWidth :: Qualified SubQuery -> Int
queryWidth =  width . unQualify

-- | Get column SQL string of 'SubQuery'.
column :: Qualified SubQuery -> Int -> String
column q =  d (unQualify q)  where
  a = qualifyAlias q
  d (Table u)      i = (aliasName a <.> (u ! i))
  d (SubQuery _ _) i = (a `AliasId.columnFromId` i)

-- | Get column SQL expression of 'SubQuery'.
columnExpr :: Qualified SubQuery -> Int -> Expr ft
columnExpr q i =  Expr $ column q i

-- | Get qualified SQL string, like (SELECT ...) AS T0
qualifiedForm :: Qualified SubQuery -> String
qualifiedForm =  qualifiedSQLas . fmap (unitSQL)

-- | Show 'SubQuery'.
instance Show SubQuery where
  show = toSQL
