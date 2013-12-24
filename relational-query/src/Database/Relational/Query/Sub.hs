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
  SubQuery, fromTable, flatSubQuery, aggregatedSubQuery,
  union, except, intersect,
  showSQL, toSQL, unitSQL, width,

  -- * Qualified Sub-query
  Qualifier (Qualifier),
  Qualified, qualifier, unQualify, qualify,
  queryWidth,

  -- * Sub-query columns
  column,

  -- * Untyped projection
  ProjectionUnit, UntypedProjection,

  untypedProjectionFromColumns, untypedProjectionFromSubQuery,
  widthOfUntypedProjection, columnsOfUntypedProjection,


  -- * Product of sub-queries
  QueryProduct, QueryProductNode, JoinProduct,
  ) where

import Data.Maybe (fromMaybe)
import Data.Array (Array, listArray)
import qualified Data.Array as Array

import qualified Database.Relational.Query.Context as Context
import Database.Relational.Query.Expr (valueExpr)
import Database.Relational.Query.Expr.Unsafe (showExpr)
import Database.Relational.Query.Internal.Product
  (NodeAttr(Just', Maybe), ProductTree (Leaf, Join),
   Node, nodeAttr, nodeTree)
import Database.Relational.Query.Component
  (ColumnSQL, columnSQL, sqlWordFromColumn, showsColumnSQL,
   Config, UnitProductSupport (UPSupported, UPNotSupported),
   Duplication, showsDuplication, QueryRestriction, composeWhere, composeHaving,
   AggregateElem, composeGroupBy, OrderingTerms, composeOrderBy)
import Database.Relational.Query.Table (Table, (!))
import qualified Database.Relational.Query.Table as Table

import Database.Relational.Query.Internal.String
  (showUnwordsSQL, showWordSQL, showWordSQL', showUnwords, showSepBy, showSpace, showComma, showParen')
import Language.SQL.Keyword (Keyword(..))
import qualified Language.SQL.Keyword as SQL
import qualified Language.SQL.Keyword.ConcatString as SQLs


data SetOp = Union | Except | Intersect  deriving Show

newtype BinOp = BinOp (SetOp, Duplication) deriving Show

showsSetOp :: SetOp -> ShowS
showsSetOp =  showWordSQL . d  where
  d Union     = UNION
  d Except    = EXCEPT
  d Intersect = INTERSECT

-- | Sub-query type
data SubQuery = Table Table.Untyped
              | Flat Config
                UntypedProjection Duplication JoinProduct (QueryRestriction Context.Flat)
                OrderingTerms
              | Aggregated Config
                UntypedProjection Duplication JoinProduct (QueryRestriction Context.Flat)
                [AggregateElem] (QueryRestriction Context.Aggregated) OrderingTerms
              | Bin BinOp SubQuery SubQuery
              deriving Show

-- | 'SubQuery' from 'Table'.
fromTable :: Table r  -- ^ Typed 'Table' metadata
          -> SubQuery -- ^ Result 'SubQuery'
fromTable =  Table . Table.unType

-- | Unsafely generate flat 'SubQuery' from untyped components.
flatSubQuery :: Config
             -> UntypedProjection
             -> Duplication
             -> JoinProduct
             -> QueryRestriction Context.Flat
             -> OrderingTerms
             -> SubQuery
flatSubQuery = Flat

-- | Unsafely generate aggregated 'SubQuery' from untyped components.
aggregatedSubQuery :: Config
                   -> UntypedProjection
                   -> Duplication
                   -> JoinProduct
                   -> QueryRestriction Context.Flat
                   -> [AggregateElem]
                   -> QueryRestriction Context.Aggregated
                   -> OrderingTerms
                   -> SubQuery
aggregatedSubQuery = Aggregated

setBin :: SetOp -> Duplication -> SubQuery -> SubQuery -> SubQuery
setBin op = Bin . BinOp . ((,) op)

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
  d (Table u)                     = Table.width' u
  d (Bin _ l _)                   = width l
  d (Flat _ up _ _ _ _)           = widthOfUntypedProjection up
  d (Aggregated _ up _ _ _ _ _ _) = widthOfUntypedProjection up

-- | SQL to query table.
fromTableToSQL :: Table.Untyped -> ShowS
fromTableToSQL t =
  showUnwordsSQL
  $ [SELECT, map sqlWordFromColumn (Table.columns' t) `SQL.sepBy` ", ",
     FROM, SQL.word $ Table.name' t]

-- | Generate normalized column SQL from table.
fromTableToNormalizedSQL :: Table.Untyped -> ShowS
fromTableToNormalizedSQL t = showWordSQL' SELECT . (columns' `showSepBy` showComma)
                             . showWordSQL' FROM . showString (Table.name' t)  where
  columns' = zipWith
             (\f n -> showsColumnSQL $ f `asColumnN` n)
             (Table.columns' t)
             [(0 :: Int)..]

-- | Normalized column SQL
normalizedSQL :: SubQuery -> ShowS
normalizedSQL =  d  where
  d (Table t)                        = fromTableToNormalizedSQL t
  d sub@(Bin _ _ _)                  = showUnitSQL sub
  d sub@(Flat _ _ _ _ _ _)           = showUnitSQL sub
  d sub@(Aggregated _ _ _ _ _ _ _ _) = showUnitSQL sub

selectPrefixSQL :: UntypedProjection -> Duplication -> ShowS
selectPrefixSQL up da = showWordSQL' SELECT . showsDuplication da
                        . showSpace . (columns' `showSepBy` showComma)  where
  columns' = zipWith
             (\f n -> showsColumnSQL $ f `asColumnN` n)
             (columnsOfUntypedProjection up)
             [(0 :: Int)..]

-- | SQL string for nested-query and toplevel-SQL.
toSQLs :: SubQuery
       -> (ShowS, ShowS) -- ^ subquery SQL and top-level SQL
toSQLs =  d  where
  d (Table u)               = (showString $ Table.name' u, fromTableToSQL u)
  d (Bin (BinOp (op, da)) l r) = (showParen' q, q)  where
    q = showUnwords [normalizedSQL l, showsSetOp op, showsDuplication da, normalizedSQL r]
  d (Flat cf up da pd rs od)   = (showParen' q, q)  where
    q = selectPrefixSQL up da . showsJoinProduct cf pd . composeWhere rs
        . composeOrderBy od
  d (Aggregated cf up da pd rs ag grs od) = (showParen' q, q)  where
    q = selectPrefixSQL up da . showsJoinProduct cf pd . composeWhere rs
        . composeGroupBy ag . composeHaving grs . composeOrderBy od

showUnitSQL :: SubQuery -> ShowS
showUnitSQL =  fst . toSQLs

-- | SQL string for nested-qeury.
unitSQL :: SubQuery -> String
unitSQL =  ($ "") . showUnitSQL

-- | SQL ShowS for toplevel-SQL.
showSQL :: SubQuery -> ShowS
showSQL = snd . toSQLs

-- | SQL string for toplevel-SQL.
toSQL :: SubQuery -> String
toSQL =  ($ "") . showSQL

-- | Qualifier type.
newtype Qualifier = Qualifier Int  deriving Show

-- | Qualified query.
data Qualified a = Qualified a Qualifier  deriving Show

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
columnN :: Int -> ColumnSQL
columnN i = columnSQL $ 'f' : show i

asColumnN :: ColumnSQL -> Int -> ColumnSQL
c `asColumnN` n = do
  c' <- c
  cn <- columnN n
  return $ c' `SQLs.as` cn

-- | Alias string from qualifier
showQualifier :: Qualifier -> String
showQualifier (Qualifier i) = 'T' : show i

-- | Binary operator to qualify.
(<.>) :: Qualifier -> ColumnSQL -> ColumnSQL
i <.> n = do
  n' <- n
  return $ showQualifier i ++ '.' : n'

-- | Qualified expression from qualifier and projection index.
columnFromId :: Qualifier -> Int -> ColumnSQL
columnFromId qi i = qi <.> columnN i

-- | From 'Qualified' SQL string into 'String'.
qualifiedSQLas :: Qualified ShowS -> ShowS
qualifiedSQLas q =
  showUnwords
  [unQualify q, (showQualifier (qualifier q) ++)]

-- | Width of 'Qualified' 'SubQUery'.
queryWidth :: Qualified SubQuery -> Int
queryWidth =  width . unQualify

-- | Get column SQL string of 'SubQuery'.
column :: Qualified SubQuery -> Int -> ColumnSQL
column qs =  d (unQualify qs)  where
  q = qualifier qs
  d (Table u)         i             = q <.> (u ! i)
  d (Bin _ _ _)       i             = q `columnFromId` i
  d (Flat _ up _ _ _ _) i           = columnOfUntypedProjection up i
  d (Aggregated _ up _ _ _ _ _ _) i = columnOfUntypedProjection up i

-- | Get qualified SQL string, like (SELECT ...) AS T0
qualifiedForm :: Qualified SubQuery -> ShowS
qualifiedForm =  qualifiedSQLas . fmap showUnitSQL


-- | Projection structure unit
data ProjectionUnit = Columns (Array Int ColumnSQL)
                    | Normalized (Qualified Int)
                    deriving Show

projectionUnitFromColumns :: [ColumnSQL] -> ProjectionUnit
projectionUnitFromColumns cs = Columns $ listArray (0, length cs - 1) cs

-- | Untyped projection. Forgot record type.
type UntypedProjection = [ProjectionUnit]

unitUntypedProjection :: ProjectionUnit -> UntypedProjection
unitUntypedProjection =  (:[])

-- | Make untyped projection from columns.
untypedProjectionFromColumns :: [ColumnSQL] -> UntypedProjection
untypedProjectionFromColumns =  unitUntypedProjection . projectionUnitFromColumns

-- | Make untyped projection from sub query.
untypedProjectionFromSubQuery :: Qualified SubQuery -> UntypedProjection
untypedProjectionFromSubQuery qs = d $ unQualify qs  where  --  unitUntypedProjection . Sub
  normalized = unitUntypedProjection . Normalized $ fmap width qs
  d (Table _)                    = untypedProjectionFromColumns . map (column qs)
                                   $ take (queryWidth qs) [0..]
  d (Bin _ _ _)                  = normalized
  d (Flat _ _ _ _ _ _)           = normalized
  d (Aggregated _ _ _ _ _ _ _ _) = normalized

-- | ProjectionUnit width.
widthOfProjectionUnit :: ProjectionUnit -> Int
widthOfProjectionUnit =  d  where
  d (Columns a)     = mx - mn + 1 where (mn, mx) = Array.bounds a
  d (Normalized qw) = unQualify qw

-- | Get column of ProjectionUnit.
columnOfProjectionUnit :: ProjectionUnit -> Int -> ColumnSQL
columnOfProjectionUnit =  d  where
  d (Columns a) i | mn <= i && i <= mx = a Array.! i
                  | otherwise          = error $ "index out of bounds (unit): " ++ show i
    where (mn, mx) = Array.bounds a
  d (Normalized qw) i | i < w          = qualifier qw `columnFromId` i
                      | otherwise      = error $ "index out of bounds (normalized unit): " ++ show i
    where w = unQualify qw

-- | Width of 'UntypedProjection'.
widthOfUntypedProjection :: UntypedProjection -> Int
widthOfUntypedProjection =  sum . map widthOfProjectionUnit

-- | Get column SQL string of 'UntypedProjection'.
columnOfUntypedProjection :: UntypedProjection -- ^ Source 'Projection'
                          -> Int               -- ^ Column index
                          -> ColumnSQL         -- ^ Result SQL string
columnOfUntypedProjection up i' = rec up i' where
  rec []       _        = error $ "index out of bounds: " ++ show i'
  rec (u : us) i
    | i < widthOfProjectionUnit u = columnOfProjectionUnit u i
    | i < 0             = error $ "index out of bounds: " ++ show i
    | otherwise         = rec us (i - widthOfProjectionUnit u)

-- | Get column SQL string list of projection.
columnsOfUntypedProjection :: UntypedProjection -- ^ Source 'Projection'
                           -> [ColumnSQL]       -- ^ Result SQL string list
columnsOfUntypedProjection p = map (\n -> columnOfUntypedProjection p n) . take w $ [0 .. ]
  where w = widthOfUntypedProjection p


-- | Product tree specialized by 'SubQuery'.
type QueryProduct = ProductTree (Qualified SubQuery)
-- | Product node specialized by 'SubQuery'.
type QueryProductNode = Node (Qualified SubQuery)

-- | Show product tree of query into SQL. ShowS result.
showsQueryProduct :: QueryProduct -> ShowS
showsQueryProduct =  rec  where
  joinType Just' Just' = INNER
  joinType Just' Maybe = LEFT
  joinType Maybe Just' = RIGHT
  joinType Maybe Maybe = FULL
  urec n = case nodeTree n of
    p@(Leaf _)     -> rec p
    p@(Join _ _ _) -> showParen True (rec p)
  rec (Leaf q)               = qualifiedForm q
  rec (Join left' right' rs) =
    showUnwords
    [urec left',
     showUnwordsSQL [joinType (nodeAttr left') (nodeAttr right'), JOIN],
     urec right',
     showWordSQL ON,
     showString . showExpr
     . fromMaybe (valueExpr True) {- or error on compile -}  $ rs]

-- | Type for join product of query.
type JoinProduct = Maybe QueryProduct

-- | Shows join product of query.
showsJoinProduct :: UnitProductSupport -> JoinProduct -> ShowS
showsJoinProduct ups =  maybe (up ups) from  where
  from qp = showSpace . showWordSQL' FROM . showsQueryProduct qp
  up UPSupported    = id
  up UPNotSupported = error "relation: Unit product support mode is disabled!"
