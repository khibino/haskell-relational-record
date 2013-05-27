{-# LANGUAGE OverloadedStrings #-}

module Database.Relational.Query.Internal.AggregatingContext (
  AggregatingContext,

  primeAggregatingContext,

  addGroupBy, addRestriction,

  composeGroupBys
  ) where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid ((<>))
import Control.Applicative (pure)

import Database.Relational.Query.Expr (Expr, showExpr)
import qualified Database.Relational.Query.Projectable as Projectable

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL
import qualified Language.SQL.Keyword.ConcatString as SQLs

type GroupByTerm = String
type GroupBys = DList GroupByTerm

data AggregatingContext =
  AggregatingContext
  { groupByTerms :: GroupBys
  , restriction  :: Maybe (Expr Bool)
  }

primeAggregatingContext :: AggregatingContext
primeAggregatingContext =  AggregatingContext DList.empty Nothing

addGroupBy :: String -> AggregatingContext -> AggregatingContext
addGroupBy t c =  c { groupByTerms = groupByTerms c <> pure t }

addRestriction :: Expr Bool -> AggregatingContext -> AggregatingContext
addRestriction e1 ctx =
  ctx { restriction = Just . uf . restriction $ ctx }
  where uf  Nothing = e1
        uf (Just e0) = e0 `Projectable.and` e1

composeGroupBys :: AggregatingContext -> String
composeGroupBys ac = unwords [unwordsSQL groupBys, unwordsSQL havings]
  where groupBys
          | null gs   = []
          | otherwise = [GROUP, BY, SQL.word . concat $ gs `SQLs.sepBy` ", "]
        gs = DList.toList (groupByTerms ac)
        havings = maybe [] (\e -> [HAVING, SQL.word . showExpr $ e]) $ restriction ac
