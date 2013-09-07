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
  column,

  -- * Untyped projection
  ProjectionUnit, UntypedProjection,

  untypedProjectionFromColumns, untypedProjectionFromSubQuery,
  widthOfUntypedProjection, columnsOfUntypedProjection,


  -- * Configuration type for query
  Config, defaultConfig,
  UnitProductSupport (..),

  -- * Product of sub-queries
  QueryProduct, QueryProductNode,

  queryProductSQL,

  JoinProduct,

  -- * Query restriction
  QueryRestriction,

  -- * Types for aggregation
  AggregateTerm, AggregateTerms,

  -- * Types for ordering
  Order (..), order, OrderColumn, OrderingTerm, OrderingTerms
  ) where

import Data.Maybe (fromMaybe)
import Data.Array (Array, listArray)
import qualified Data.Array as Array

import qualified Database.Relational.Query.Context as Context
import Database.Relational.Query.Expr (Expr, valueExpr)
import Database.Relational.Query.Expr.Unsafe (showExpr)
import Database.Relational.Query.Internal.Product
  (NodeAttr(Just', Maybe), ProductTree (Leaf, Join),
   Node, nodeAttr, nodeTree)
import Database.Relational.Query.Table
  (ColumnSQL, columnSQL, sqlWordFromColumn, stringFromColumnSQL, Table, (!))
import qualified Database.Relational.Query.Table as Table

import Database.Relational.Query.Internal.String
  (showUnwordsSQL, showWordSQL, showWordSQL', showUnwords, showSpace, paren)
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
fromTable :: Table r  -- ^ Typed 'Table' metadata
          -> SubQuery -- ^ Result 'SubQuery'
fromTable =  Table . Table.unType

-- | Unsafely generate 'SubQuery' from SQL.
subQuery :: String   -- ^ SQL string
         -> Int      -- ^ Width of 'SubQuery'
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
               (\f n -> sqlWordFromColumn f `asColumnN` n)
               (Table.columns' t)
               [(0 :: Int)..]
    sql = unwordsSQL
          $ [SELECT, columns' `SQL.sepBy` ", ",
             FROM, SQL.word . Table.name' $ t]

  d sub@(SubQuery _ _)   = sub
  d sub@(Bin _ _ _)      = sub

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
  $ [SELECT, map sqlWordFromColumn (Table.columns' t) `SQL.sepBy` ", ",
     FROM, SQL.word $ Table.name' t]

-- | Generate select SQL. Seed SQL string append to this.
_selectPrefixSQL :: UntypedProjection -> ShowS
_selectPrefixSQL up =
  (unwordsSQL [SELECT, columns' `SQL.sepBy` ", "] ++)
  where columns' = zipWith
                   (\f n -> sqlWordFromColumn f `asColumnN` n)
                   (columnsOfUntypedProjection up)
                   [(0 :: Int)..]

-- | SQL string for nested-query and toplevel-SQL.
toSQLs :: SubQuery
       -> (String, String) -- ^ subquery SQL and top-level SQL
toSQLs =  d  where
  d (Table u)               = (Table.name' u, fromTableToSql u)
  d (SubQuery { sql' = q }) = (paren q, q)
  d (Bin op l r)            = (paren q, q)  where
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
columnN :: Int -> ColumnSQL
columnN i = columnSQL $ 'f' : show i

-- | Renamed column in SQL expression.
asColumnN :: SQL.Keyword -> Int -> SQL.Keyword
f `asColumnN` n = f `SQL.as` sqlWordFromColumn (columnN  n)

-- | Alias string from qualifier
showQualifier :: Qualifier -> String
showQualifier (Qualifier i) = 'T' : show i

-- | Binary operator to qualify.
(<.>) :: Qualifier -> ColumnSQL -> ColumnSQL
i <.> n = columnSQL $ showQualifier i ++ '.' : stringFromColumnSQL n

-- | Qualified expression from qualifier and projection index.
columnFromId :: Qualifier -> Int -> ColumnSQL
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
column :: Qualified SubQuery -> Int -> ColumnSQL
column qs =  d (unQualify qs)  where
  q = qualifier qs
  d (Table u)      i = (q <.> (u ! i))
  d (SubQuery _ _) i = (q `columnFromId` i)
  d (Bin _ _ _)    i = (q `columnFromId` i)

-- | Get qualified SQL string, like (SELECT ...) AS T0
qualifiedForm :: Qualified SubQuery -> String
qualifiedForm =  qualifiedSQLas . fmap unitSQL

-- | Show 'SubQuery'.
instance Show SubQuery where
  show = toSQL


-- | Projection structure unit
data ProjectionUnit = Columns (Array Int ColumnSQL)
                    | Normalized (Qualified Int)

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
  d (Table _)      = untypedProjectionFromColumns . map (column qs)
                     $ take (queryWidth qs) [0..]
  d (SubQuery _ _) = normalized
  d (Bin _ _ _)    = normalized

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


-- | Configuration type.
type Config = UnitProductSupport

-- | Default configuration.
defaultConfig :: Config
defaultConfig =  UPSupported

-- | Unit product is supported or not.
data UnitProductSupport = UPSupported | UPNotSupported


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
  rec (Leaf q)               = showString $ qualifiedForm q
  rec (Join left' right' rs) =
    showUnwords
    [urec left',
     showUnwordsSQL [joinType (nodeAttr left') (nodeAttr right'), JOIN],
     urec right',
     showWordSQL ON,
     showString . showExpr
     . fromMaybe (valueExpr True) {- or error on compile -}  $ rs]

-- | Show product tree of query into SQL.
queryProductSQL :: QueryProduct -> String
queryProductSQL =  ($ "") . showsQueryProduct

-- | Type for join product of query.
type JoinProduct = Maybe QueryProduct

-- | Shows join product of query.
_showsJoinProduct :: UnitProductSupport -> JoinProduct -> ShowS
_showsJoinProduct ups =  maybe (up ups) from  where
  from qp = showSpace . showWordSQL' FROM . showsQueryProduct qp
  up UPSupported    = id
  up UPNotSupported = error "relation: Unit product support mode is disabled!"

-- | Type for restriction of query.
type QueryRestriction c = Maybe (Expr c Bool)

-- | Compose SQL String from 'Expr' object.
_composeRestrict :: Keyword -> QueryRestriction c -> ShowS
_composeRestrict k = maybe id (\e -> showSpace . showUnwordsSQL [k, SQL.word . showExpr $ e])


-- | Type for group-by term
type AggregateTerm = ColumnSQL

-- | Type for group-by terms
type AggregateTerms = [AggregateTerm]

_composeGroupBys :: AggregateTerms -> ShowS
_composeGroupBys as = groupBys where
  groupBys
    | null gs   = id
    | otherwise = showSpace . showUnwordsSQL [GROUP, BY, gs `SQL.sepBy` ", "]
  gs = map sqlWordFromColumn as


-- | Order direction. Ascendant or Descendant.
data Order = Asc | Desc

-- | Get SQL keyword from order attribute.
order :: Order -> Keyword
order Asc  = ASC
order Desc = DESC

-- | Type for order-by column
type OrderColumn = ColumnSQL

-- | Type for order-by term
type OrderingTerm = (Order, OrderColumn)

-- | Type for order-by terms
type OrderingTerms = [OrderingTerm]

_composeOrderByes :: OrderingTerms -> ShowS
_composeOrderByes ots = orders  where
  orderList = foldr (\ (o, e) r -> [sqlWordFromColumn e, order o] `SQL.sepBy` " "  : r) [] ots
  orders | null orderList = id
         | otherwise      = showSpace . showUnwordsSQL [ORDER, BY, orderList `SQL.sepBy` ", "]
