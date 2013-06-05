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
  Qualifier (Qualifier),
  Qualified, qualifier, unQualify, qualify,
  queryWidth, qualifiedForm,

  -- * Sub-query columns
  column, columnExpr
  ) where

import Database.Relational.Query.Table (Table, (!))
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Expr.Unsafe (Expr(Expr))

import qualified Language.SQL.Keyword.ConcatString as SQLs


-- | Sub-query type
data SubQuery = Table Table.Untyped
              | SubQuery
                { sql'   :: String
                , width' :: !Int
                }

-- | 'SubQuery' from 'Table'.
fromTable :: Table r -- ^ Typed 'Table' metadata
          -> SubQuery -- ^ Result 'SubQuery'
fromTable =  Table . Table.unType

-- | Unsafely generate 'SubQuery' from SQL.
subQuery :: String -- ^ SQL string
            -> Int -- ^ Width of 'SubQuery'
            -> SubQuery -- ^ Result 'SubQuery'
subQuery =  SubQuery

-- | Width of 'SubQuery'.
width :: SubQuery -> Int
width =  d  where
  d (Table u)                 = Table.width' u
  d (SubQuery { width' = w }) = w

-- | SQL string for nested-query and toplevel-SQL.
toSQLs :: SubQuery
       -> (String, String) -- ^ subquery SQL and top-level SQL
toSQLs =  d  where
  d (Table u)               = (Table.name' u, Table.fromTableToSql u)
  d (SubQuery { sql' = q }) = ('(' : q ++ [')'], q)

-- | SQL string for nested-qeury.
unitSQL :: SubQuery -> String
unitSQL =  fst . toSQLs

-- | SQL string for toplevel-SQL.
toSQL :: SubQuery -> String
toSQL =  snd . toSQLs

newtype Qualifier = Qualifier Int

data Qualified a = Qualified a Qualifier

-- | 'Functor' instance of 'Qualified'
instance Functor Qualified where
  fmap f (Qualified a i) = Qualified (f a) i

qualifier :: Qualified a -> Qualifier
qualifier (Qualified _ i) = i

unQualify :: Qualified a -> a
unQualify (Qualified a _) = a

qualify :: a -> Qualifier -> Qualified a
qualify =  Qualified

columnN :: Int -> String
columnN i = 'f' : show i

showQualifier :: Qualifier -> String
showQualifier (Qualifier i) = 'T' : show i

(<.>) :: Qualifier -> String -> String
i <.> n =  showQualifier i ++ '.' : n

columnFromId :: Qualifier -> Int -> String
columnFromId qi i = qi <.> columnN i

-- | From 'Qualified' SQL string into 'String'.
qualifiedSQLas :: Qualified String -> String
qualifiedSQLas q =
  unQualify q
  `SQLs.as`
  (showQualifier $ qualifier q)

-- | Width of 'Qualified' 'SubQUery'.
queryWidth :: Qualified SubQuery -> Int
queryWidth =  width . unQualify

-- | Get column SQL string of 'SubQuery'.
column :: Qualified SubQuery -> Int -> String
column qs =  d (unQualify qs)  where
  q = qualifier qs
  d (Table u)      i = (q <.> (u ! i))
  d (SubQuery _ _) i = (q `columnFromId` i)

-- | Get column SQL expression of 'SubQuery'.
columnExpr :: Qualified SubQuery -> Int -> Expr ft
columnExpr q i =  Expr $ column q i

-- | Get qualified SQL string, like (SELECT ...) AS T0
qualifiedForm :: Qualified SubQuery -> String
qualifiedForm =  qualifiedSQLas . fmap (unitSQL)

-- | Show 'SubQuery'.
instance Show SubQuery where
  show = toSQL
