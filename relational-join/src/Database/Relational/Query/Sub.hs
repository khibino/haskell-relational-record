
module Database.Relational.Query.Sub (
  SubQuery, fromTable, subQuery, unitSQL, width,

  queryWidth,

  column, columnExpr, qualifiedForm
  ) where

import Database.Relational.Query.Table (Table, (!))
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.AliasId
  (aliasName, (<.>),
   Qualified, unQualify, qualifyAlias)
import qualified Database.Relational.Query.AliasId as AliasId
import Database.Relational.Query.Expr.Unsafe (Expr(Expr))

import qualified Language.SQL.Keyword as SQL


data SubQuery = Table Table.Untyped
              | SubQuery
                { sql'   :: String
                , width' :: !Int
                }

fromTable :: Table r -> SubQuery
fromTable =  Table . Table.unType

subQuery :: String -> Int -> SubQuery
subQuery =  SubQuery

width :: SubQuery -> Int
width =  d  where
  d (Table u)                 = Table.width' u
  d (SubQuery { width' = w }) = w

toSQLs :: SubQuery -> (String, String)
toSQLs =  d  where
  d (Table u)               = let n = Table.name' u in (n, n)
  d (SubQuery { sql' = q }) = ('(' : q ++ [')'], q)

unitSQL :: SubQuery -> String
unitSQL =  fst . toSQLs

toSQL :: SubQuery -> String
toSQL =  snd . toSQLs

queryWidth :: Qualified SubQuery -> Int
queryWidth =  width . unQualify

column :: Qualified SubQuery -> Int -> String
column q =  d (unQualify q)  where
  a = qualifyAlias q
  d (Table u)      i = (aliasName a <.> (u ! i))
  d (SubQuery _ _) i = (a `AliasId.columnFromId` i)

columnExpr :: Qualified SubQuery -> Int -> Expr ft
columnExpr q i =  Expr $ column q i

qualifiedForm :: Qualified SubQuery -> SQL.Keyword
qualifiedForm q = SQL.word (unitSQL (unQualify q))
                  `SQL.as`
                  SQL.word (aliasName . qualifyAlias $ q)

instance Show SubQuery where
  show = toSQL
