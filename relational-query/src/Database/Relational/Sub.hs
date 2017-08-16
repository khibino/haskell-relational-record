{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.Relational.Sub
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines sub-query structure used in query products.
module Database.Relational.Sub (
  -- * Sub-query
  SubQuery, fromTable, flatSubQuery, aggregatedSubQuery,
  union, except, intersect,
  showSQL, toSQL, unitSQL, width,

  -- * Qualified Sub-query
  Qualifier (Qualifier),
  Qualified,
  queryWidth,

  -- * Sub-query columns
  column,

  -- * Tuple and Record
  tupleFromJoinedSubQuery,

  recordRawColumns,

  -- * Product of sub-queries
  JoinProduct, NodeAttr (..),
  ProductBuilder,

  -- * Query restriction
  QueryRestriction,
  composeWhere, composeHaving
  ) where

import Control.Applicative ((<$>))
import Data.Monoid (mempty, (<>), mconcat)
import Data.Traversable (traverse)

import Language.SQL.Keyword (Keyword(..), (|*|))
import qualified Language.SQL.Keyword as SQL

import qualified Database.Relational.Context as Context
import Database.Relational.Internal.SQL
  (StringSQL, stringSQL, rowStringSQL, showStringSQL, )
import Database.Relational.SqlSyntax
  (Config (productUnitSupport), ProductUnitSupport (PUSupported, PUNotSupported),
   Duplication (..), OrderingTerm)
import Database.Relational.Internal.BaseSQL
  (showsDuplication, composeOrderBy, )
import Database.Relational.Internal.GroupingSQL
  (AggregateElem, composeGroupBy, )
import Database.Relational.SqlSyntax.Types
  (SubQuery (..), Record,
   CaseClause(..), WhenClauses (..),
   Tuple, Column (..),
   JoinProduct, QueryProductTree, ProductBuilder,
   NodeAttr (Just', Maybe), ProductTree (Leaf, Join),
   SetOp (..), BinOp (..), Qualifier (..), Qualified (..),
   QueryRestriction)
import qualified Database.Relational.SqlSyntax.Types as Syntax
import Database.Relational.Internal.UntypedTable ((!))
import qualified Database.Relational.Internal.UntypedTable as UntypedTable

import Database.Relational.Table (Table)
import qualified Database.Relational.Table as Table
import Database.Relational.ProjectableClass (showConstantTermsSQL)
import Database.Relational.Pure ()


showsSetOp' :: SetOp -> StringSQL
showsSetOp' =  d  where
  d Union     = UNION
  d Except    = EXCEPT
  d Intersect = INTERSECT

showsSetOp :: SetOp -> Duplication -> StringSQL
showsSetOp op dup0 = showsSetOp' op <> mayDup dup0  where
  mayDup dup@All  = showsDuplication dup
  mayDup Distinct = mempty

-- | 'SubQuery' from 'Table'.
fromTable :: Table r  -- ^ Typed 'Table' metadata
          -> SubQuery -- ^ Result 'SubQuery'
fromTable =  Table . Table.unType

-- | Unsafely generate flat 'SubQuery' from untyped components.
flatSubQuery :: Config
             -> Tuple
             -> Duplication
             -> JoinProduct
             -> QueryRestriction Context.Flat
             -> [OrderingTerm]
             -> SubQuery
flatSubQuery = Flat

-- | Unsafely generate aggregated 'SubQuery' from untyped components.
aggregatedSubQuery :: Config
                   -> Tuple
                   -> Duplication
                   -> JoinProduct
                   -> QueryRestriction Context.Flat
                   -> [AggregateElem]
                   -> QueryRestriction Context.Aggregated
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

-- | Width of 'SubQuery'.
width :: SubQuery -> Int
width =  d  where
  d (Table u)                     = UntypedTable.width' u
  d (Bin _ l _)                   = width l
  d (Flat _ up _ _ _ _)           = Syntax.tupleWidth up
  d (Aggregated _ up _ _ _ _ _ _) = Syntax.tupleWidth up

-- | SQL to query table.
fromTableToSQL :: UntypedTable.Untyped -> StringSQL
fromTableToSQL t =
  SELECT <> SQL.fold (|*|) (UntypedTable.columns' t) <>
  FROM <> stringSQL (UntypedTable.name' t)

-- | Generate normalized column SQL from table.
fromTableToNormalizedSQL :: UntypedTable.Untyped -> StringSQL
fromTableToNormalizedSQL t = SELECT <> SQL.fold (|*|) columns' <>
                             FROM <> stringSQL (UntypedTable.name' t)  where
  columns' = zipWith asColumnN
             (UntypedTable.columns' t)
             [(0 :: Int)..]

-- | Normalized column SQL
normalizedSQL :: SubQuery -> StringSQL
normalizedSQL =  d  where
  d (Table t)                 =  fromTableToNormalizedSQL t
  d sub@(Bin {})              =  showUnitSQL sub
  d sub@(Flat _ _ _ _ _ ots)
    | null ots                =  showSQL sub
    | otherwise               =  showUnitSQL sub
  d sub@(Aggregated _ _ _ _ _ _ _ ots)
    | null ots                =  showSQL sub
    | otherwise               =  showUnitSQL sub

selectPrefixSQL :: Tuple -> Duplication -> StringSQL
selectPrefixSQL up da = SELECT <> showsDuplication da <>
                        SQL.fold (|*|) columns'  where
  columns' = zipWith asColumnN
             (map showColumn up)
             [(0 :: Int)..]

-- | SQL string for nested-query and toplevel-SQL.
toSQLs :: SubQuery
       -> (StringSQL, StringSQL) -- ^ sub-query SQL and top-level SQL
toSQLs =  d  where
  d (Table u)               = (stringSQL $ UntypedTable.name' u, fromTableToSQL u)
  d (Bin (BinOp (op, da)) l r) = (SQL.paren q, q)  where
    q = mconcat [normalizedSQL l, showsSetOp op da, normalizedSQL r]
  d (Flat cf up da pd rs od)   = (SQL.paren q, q)  where
    q = selectPrefixSQL up da <> showsJoinProduct (productUnitSupport cf) pd <> composeWhere rs
        <> composeOrderBy od
  d (Aggregated cf up da pd rs ag grs od) = (SQL.paren q, q)  where
    q = selectPrefixSQL up da <> showsJoinProduct (productUnitSupport cf) pd <> composeWhere rs
        <> composeGroupBy ag <> composeHaving grs <> composeOrderBy od

showUnitSQL :: SubQuery -> StringSQL
showUnitSQL =  fst . toSQLs

-- | SQL string for nested-qeury.
unitSQL :: SubQuery -> String
unitSQL =  showStringSQL . showUnitSQL

-- | SQL StringSQL for toplevel-SQL.
showSQL :: SubQuery -> StringSQL
showSQL = snd . toSQLs

-- | SQL string for toplevel-SQL.
toSQL :: SubQuery -> String
toSQL =  showStringSQL . showSQL

columnN :: Int -> StringSQL
columnN i = stringSQL $ 'f' : show i

asColumnN :: StringSQL -> Int -> StringSQL
c `asColumnN` n =c `SQL.as` columnN n

-- | Alias string from qualifier
showQualifier :: Qualifier -> StringSQL
showQualifier (Qualifier i) = stringSQL $ 'T' : show i

-- | Binary operator to qualify.
(<.>) :: Qualifier -> StringSQL -> StringSQL
i <.> n = showQualifier i SQL.<.> n

-- | Qualified expression from qualifier and projection index.
columnFromId :: Qualifier -> Int -> StringSQL
columnFromId qi i = qi <.> columnN i

-- | From 'Qualified' SQL string into qualified formed 'String'
--  like (SELECT ...) AS T<n>
qualifiedSQLas :: Qualified StringSQL -> StringSQL
qualifiedSQLas q = Syntax.unQualify q <> showQualifier (Syntax.qualifier q)

-- | Width of 'Qualified' 'SubQUery'.
queryWidth :: Qualified SubQuery -> Int
queryWidth =  width . Syntax.unQualify

-- | Get column SQL string of 'SubQuery'.
column :: Qualified SubQuery -> Int -> StringSQL
column qs =  d (Syntax.unQualify qs)  where
  q = Syntax.qualifier qs
  d (Table u)           i           = q <.> (u ! i)
  d (Bin {})            i           = q `columnFromId` i
  d (Flat _ up _ _ _ _) i           = showTupleIndex up i
  d (Aggregated _ up _ _ _ _ _ _) i = showTupleIndex up i

-- | Make untyped tuple from joined sub-query.
tupleFromJoinedSubQuery :: Qualified SubQuery -> Tuple
tupleFromJoinedSubQuery qs = d $ Syntax.unQualify qs  where
  normalized = SubQueryRef <$> traverse (\q -> [0 .. width q - 1]) qs
  d (Table _)               =  map RawColumn . map (column qs)
                               $ take (queryWidth qs) [0..]
  d (Bin {})                =  normalized
  d (Flat {})               =  normalized
  d (Aggregated {})         =  normalized

-- | index result of each when clause and else clause.
indexWhensClause :: WhenClauses -> Int -> StringSQL
indexWhensClause (WhenClauses ps e) i =
    mconcat [ when' p r | (p, r)  <-  ps] <> else' <> SQL.END
  where
    when' p r = SQL.WHEN <> rowStringSQL (map showColumn p) <>
                SQL.THEN <> showTupleIndex r i
    else'     = SQL.ELSE <> showTupleIndex e i

-- | index result of each when clause and else clause.
caseClause :: CaseClause -> Int -> StringSQL
caseClause c i = d c  where
  d (CaseSearch wcl)    = SQL.CASE <> indexWhensClause wcl i
  d (CaseSimple m wcl)  = SQL.CASE <> rowStringSQL (map showColumn m) <> indexWhensClause wcl i

-- | Convert from typed' Column' into column string expression.
showColumn :: Column -> StringSQL
showColumn = d  where
  d (RawColumn e)     = e
  d (SubQueryRef qi)  = Syntax.qualifier qi `columnFromId` Syntax.unQualify qi
  d (Scalar sub)      = showUnitSQL sub
  d (Case c i)        = caseClause c i

-- | Get column SQL string of 'Tuple'.
showTupleIndex :: Tuple     -- ^ Source 'Tuple'
               -> Int       -- ^ Column index
               -> StringSQL -- ^ Result SQL string
showTupleIndex up i
  | 0 <= i && i < Syntax.tupleWidth up  =
    showColumn $ up !! i
  | otherwise                                         =
    error $ "showTupleIndex: index out of bounds: " ++ show i

-- | Get column SQL string list of record.
recordRawColumns :: Record c r  -- ^ Source 'Record'
                 -> [StringSQL] -- ^ Result SQL string list
recordRawColumns = map showColumn . Syntax.untypeRecord


-- | Show product tree of query into SQL. StringSQL result.
showsQueryProduct :: QueryProductTree -> StringSQL
showsQueryProduct =  rec  where
  joinType Just' Just' = INNER
  joinType Just' Maybe = LEFT
  joinType Maybe Just' = RIGHT
  joinType Maybe Maybe = FULL
  urec n = case Syntax.nodeTree n of
    p@(Leaf _)     -> rec p
    p@(Join {})    -> SQL.paren (rec p)
  rec (Leaf q)               = qualifiedSQLas $ fmap showUnitSQL q
  rec (Join left' right' rs) =
    mconcat
    [urec left',
     joinType (Syntax.nodeAttr left') (Syntax.nodeAttr right'), JOIN,
     urec right',
     ON, foldr1 SQL.and $ ps ++ concat [ showConstantTermsSQL True | null ps ] ]
    where ps = [ rowStringSQL $ recordRawColumns p | p <- rs ]

-- | Shows join product of query.
showsJoinProduct :: ProductUnitSupport -> JoinProduct -> StringSQL
showsJoinProduct ups =  maybe (up ups) from  where
  from qp = FROM <> showsQueryProduct qp
  up PUSupported    = mempty
  up PUNotSupported = error "relation: Unit product support mode is disabled!"


-- | Compose SQL String from 'QueryRestriction'.
composeRestrict :: Keyword -> QueryRestriction c -> StringSQL
composeRestrict k = d  where
  d     []    =  mempty
  d ps@(_:_)  =  k <> foldr1 SQL.and [ rowStringSQL $ recordRawColumns p | p <- ps ]

-- | Compose WHERE clause from 'QueryRestriction'.
composeWhere :: QueryRestriction Context.Flat -> StringSQL
composeWhere =  composeRestrict WHERE

-- | Compose HAVING clause from 'QueryRestriction'.
composeHaving :: QueryRestriction Context.Aggregated -> StringSQL
composeHaving =  composeRestrict HAVING
