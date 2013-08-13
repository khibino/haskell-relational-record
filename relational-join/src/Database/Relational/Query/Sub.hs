{-# LANGUAGE OverloadedStrings #-}

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
  SubQuery, fromTable, subQuery, union, except, intersect,
  toSQL, unitSQL, width,

  -- * Qualified Sub-query
  Qualifier (Qualifier),
  Qualified, qualifier, unQualify, qualify,
  queryWidth, qualifiedForm,

  asColumnN,

  -- * Sub-query columns
  column
  ) where

import Data.List (intercalate)

import Database.Relational.Query.Table (Table, (!))
import qualified Database.Relational.Query.Table as Table

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL
import qualified Language.SQL.Keyword.ConcatString as SQLs


data BinOp = Union | Except | Intersect

keywordBinOp :: BinOp -> Keyword
keywordBinOp = d  where
  d Union     = UNION
  d Except    = EXCEPT
  d Intersect = INTERSECT

-- | Sub-query type
data SubQuery = Table Table.Untyped
              | SubQuery
                { sql'   :: String
                , width' :: !Int
                }
              | Bin BinOp SubQuery SubQuery

-- | 'SubQuery' from 'Table'.
fromTable :: Table r -- ^ Typed 'Table' metadata
          -> SubQuery -- ^ Result 'SubQuery'
fromTable =  Table . Table.unType

-- | Unsafely generate 'SubQuery' from SQL.
subQuery :: String -- ^ SQL string
            -> Int -- ^ Width of 'SubQuery'
            -> SubQuery -- ^ Result 'SubQuery'
subQuery =  SubQuery

-- | Binary operator on 'SubQuery'
binSubQuery :: BinOp -> SubQuery -> SubQuery -> SubQuery
binSubQuery op a b = Bin op (hideTable a) (hideTable b)

-- | Union binary operator on 'SubQuery'
union     :: SubQuery -> SubQuery -> SubQuery
union     =  binSubQuery Union

-- | Except binary operator on 'SubQuery'
except    :: SubQuery -> SubQuery -> SubQuery
except    =  binSubQuery Except

-- | Intersect binary operator on 'SubQuery'
intersect :: SubQuery -> SubQuery -> SubQuery
intersect =  binSubQuery Intersect

-- | Unify projection field name.
hideTable :: SubQuery -> SubQuery
hideTable = d  where
  d (Table t)          = subQuery sql (Table.width' t)  where
    columns' = zipWith
               (\f n -> SQL.word f `asColumnN` n)
               (Table.columns' t)
               [(0 :: Int)..]
    sql = unwordsSQL
          $ [SELECT, columns' `SQL.sepBy` ", ",
             FROM, SQL.word . Table.name' $ t]

  d sub@(SubQuery _ _) = sub
  d sub@(Bin _ _ _)    = sub

-- | Width of 'SubQuery'.
width :: SubQuery -> Int
width =  d  where
  d (Table u)                 = Table.width' u
  d (SubQuery { width' = w }) = w
  d (Bin _ l _)               = width l

-- | SQL to query table
fromTableToSql :: Table.Untyped -> String
fromTableToSql t =
  unwordsSQL
  $ [SELECT, SQL.word $ ", " `intercalate` Table.columns' t,
     FROM, SQL.word $ Table.name' t]

-- | SQL string for nested-query and toplevel-SQL.
toSQLs :: SubQuery
       -> (String, String) -- ^ subquery SQL and top-level SQL
toSQLs =  d  where
  d (Table u)               = (Table.name' u, fromTableToSql u)
  d (SubQuery { sql' = q }) = ('(' : q ++ [')'], q)
  d (Bin op l r)            = ('(' : q ++ [')'], q)  where
    q = unwords [unitSQL l, SQL.wordShow $ keywordBinOp op, unitSQL r]

-- | SQL string for nested-qeury.
unitSQL :: SubQuery -> String
unitSQL =  fst . toSQLs

-- | SQL string for toplevel-SQL.
toSQL :: SubQuery -> String
toSQL =  snd . toSQLs

-- | Qualifier type.
newtype Qualifier = Qualifier Int

-- | Qualified query.
data Qualified a = Qualified a Qualifier

-- | 'Functor' instance of 'Qualified'
instance Functor Qualified where
  fmap f (Qualified a i) = Qualified (f a) i

-- | Get qualifier
qualifier :: Qualified a -> Qualifier
qualifier (Qualified _ i) = i

-- | Unqualify.
unQualify :: Qualified a -> a
unQualify (Qualified a _) = a

-- | Add qualifier
qualify :: a -> Qualifier -> Qualified a
qualify =  Qualified

-- | Column name of projection index.
columnN :: Int -> String
columnN i = 'f' : show i

-- | Renamed column in SQL expression.
asColumnN :: SQL.Keyword -> Int -> SQL.Keyword
f `asColumnN` n = f `SQL.as` SQL.word (columnN  n)

-- | Alias string from qualifier
showQualifier :: Qualifier -> String
showQualifier (Qualifier i) = 'T' : show i

-- | Binary operator to qualify.
(<.>) :: Qualifier -> String -> String
i <.> n =  showQualifier i ++ '.' : n

-- | Qualified expression from qualifier and projection index.
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
  d (Bin _ _ _)    i = (q `columnFromId` i)

-- | Get qualified SQL string, like (SELECT ...) AS T0
qualifiedForm :: Qualified SubQuery -> String
qualifiedForm =  qualifiedSQLas . fmap (unitSQL)

-- | Show 'SubQuery'.
instance Show SubQuery where
  show = toSQL
