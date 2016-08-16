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

  -- * Projection
  Projection, ProjectionUnit, UntypedProjection,

  untypedProjectionFromColumns, untypedProjectionFromJoinedSubQuery, untypedProjectionFromScalarSubQuery,
  widthOfUntypedProjection, columnsOfUntypedProjection,

  projectionColumns, unsafeProjectionStringSql, unsafeProjectFromColumns,

  -- * Product of sub-queries
  QueryProduct, QueryProductNode, JoinProduct, NodeAttr,
  nodeTree,

  -- * Query restriction
  QueryRestriction,
  composeWhere, composeHaving
  ) where

import Data.Array (listArray)
import qualified Data.Array as Array
import Data.Monoid (mempty, (<>), mconcat)
import Data.DList (toList)

import qualified Database.Relational.Query.Context as Context
import Database.Relational.Query.Internal.SQL (StringSQL, stringSQL, rowStringSQL, showStringSQL)
import Database.Relational.Query.Internal.Sub
  (SubQuery (..), Projection, untypeProjection, typedProjection,
   UntypedProjection, ProjectionUnit (..),
   JoinProduct, QueryProduct, QueryProductNode,
   NodeAttr (Just', Maybe), ProductTree (Leaf, Join), Node (Node),
   SetOp (..), BinOp (..), Qualifier (..), Qualified (..),
   QueryRestriction)
import Database.Relational.Query.Component
  (ColumnSQL, columnSQL', showsColumnSQL,
   Config (productUnitSupport), ProductUnitSupport (PUSupported, PUNotSupported),
   Duplication (..), showsDuplication,
   AggregateElem, composeGroupBy, OrderingTerms, composeOrderBy)
import Database.Relational.Query.Table (Table, (!))
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Pure (showConstantTermsSQL')

import Language.SQL.Keyword (Keyword(..), (|*|))
import qualified Language.SQL.Keyword as SQL


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
  d (Table u)                     = Table.width' u
  d (Bin _ l _)                   = width l
  d (Flat _ up _ _ _ _)           = widthOfUntypedProjection up
  d (Aggregated _ up _ _ _ _ _ _) = widthOfUntypedProjection up

-- | SQL to query table.
fromTableToSQL :: Table.Untyped -> StringSQL
fromTableToSQL t =
  SELECT <> SQL.fold (|*|) [showsColumnSQL c | c <- Table.columns' t] <>
  FROM <> stringSQL (Table.name' t)

-- | Generate normalized column SQL from table.
fromTableToNormalizedSQL :: Table.Untyped -> StringSQL
fromTableToNormalizedSQL t = SELECT <> SQL.fold (|*|) columns' <>
                             FROM <> stringSQL (Table.name' t)  where
  columns' = zipWith asColumnN
             (Table.columns' t)
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

selectPrefixSQL :: UntypedProjection -> Duplication -> StringSQL
selectPrefixSQL up da = SELECT <> showsDuplication da <>
                        SQL.fold (|*|) columns'  where
  columns' = zipWith asColumnN
             (columnsOfUntypedProjection up)
             [(0 :: Int)..]

-- | SQL string for nested-query and toplevel-SQL.
toSQLs :: SubQuery
       -> (StringSQL, StringSQL) -- ^ sub-query SQL and top-level SQL
toSQLs =  d  where
  d (Table u)               = (stringSQL $ Table.name' u, fromTableToSQL u)
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

-- | Get qualifier
qualifier :: Qualified a -> Qualifier
qualifier (Qualified _ i) = i

-- | Unqualify.
unQualify :: Qualified a -> a
unQualify (Qualified a _) = a

-- | Add qualifier
qualify :: a -> Qualifier -> Qualified a
qualify =  Qualified

columnN :: Int -> StringSQL
columnN i = stringSQL $ 'f' : show i

asColumnN :: ColumnSQL -> Int -> StringSQL
c `asColumnN` n = showsColumnSQL c `SQL.as` columnN n

-- | Alias string from qualifier
showQualifier :: Qualifier -> StringSQL
showQualifier (Qualifier i) = stringSQL $ 'T' : show i

-- | Binary operator to qualify.
(<.>) :: Qualifier -> ColumnSQL -> ColumnSQL
i <.> n = fmap (showQualifier i SQL.<.>) n

-- | Qualified expression from qualifier and projection index.
columnFromId :: Qualifier -> Int -> ColumnSQL
columnFromId qi i = qi <.> columnSQL' (columnN i)

-- | From 'Qualified' SQL string into 'String'.
qualifiedSQLas :: Qualified StringSQL -> StringSQL
qualifiedSQLas q = unQualify q <> showQualifier (qualifier q)

-- | Width of 'Qualified' 'SubQUery'.
queryWidth :: Qualified SubQuery -> Int
queryWidth =  width . unQualify

-- | Get column SQL string of 'SubQuery'.
column :: Qualified SubQuery -> Int -> ColumnSQL
column qs =  d (unQualify qs)  where
  q = qualifier qs
  d (Table u)           i           = q <.> (u ! i)
  d (Bin {})            i           = q `columnFromId` i
  d (Flat _ up _ _ _ _) i           = columnOfUntypedProjection up i
  d (Aggregated _ up _ _ _ _ _ _) i = columnOfUntypedProjection up i

-- | Get qualified SQL string, like (SELECT ...) AS T0
qualifiedForm :: Qualified SubQuery -> StringSQL
qualifiedForm =  qualifiedSQLas . fmap showUnitSQL


projectionUnitFromColumns :: [ColumnSQL] -> ProjectionUnit
projectionUnitFromColumns cs = Columns $ listArray (0, length cs - 1) cs

projectionUnitFromScalarSubQuery :: SubQuery -> ProjectionUnit
projectionUnitFromScalarSubQuery =  Scalar

unitUntypedProjection :: ProjectionUnit -> UntypedProjection
unitUntypedProjection =  (:[])

-- | Make untyped projection from columns.
untypedProjectionFromColumns :: [ColumnSQL] -> UntypedProjection
untypedProjectionFromColumns =  unitUntypedProjection . projectionUnitFromColumns

-- | Make untyped projection from scalar sub-query.
untypedProjectionFromScalarSubQuery :: SubQuery -> UntypedProjection
untypedProjectionFromScalarSubQuery =  unitUntypedProjection . projectionUnitFromScalarSubQuery

-- | Make untyped projection from joined sub-query.
untypedProjectionFromJoinedSubQuery :: Qualified SubQuery -> UntypedProjection
untypedProjectionFromJoinedSubQuery qs = d $ unQualify qs  where  --  unitUntypedProjection . Sub
  normalized = unitUntypedProjection . Normalized $ fmap width qs
  d (Table _)               =  untypedProjectionFromColumns . map (column qs)
                               $ take (queryWidth qs) [0..]
  d (Bin {})                =  normalized
  d (Flat {})               =  normalized
  d (Aggregated {})         =  normalized

-- | ProjectionUnit width.
widthOfProjectionUnit :: ProjectionUnit -> Int
widthOfProjectionUnit =  d  where
  d (Columns a)     = mx - mn + 1 where (mn, mx) = Array.bounds a
  d (Normalized qw) = unQualify qw
  d (Scalar _)      = 1

-- | Get column of ProjectionUnit.
columnOfProjectionUnit :: ProjectionUnit -> Int -> ColumnSQL
columnOfProjectionUnit =  d  where
  d (Columns a) i | mn <= i && i <= mx = a Array.! i
                  | otherwise          = error $ "index out of bounds (unit): " ++ show i
    where (mn, mx) = Array.bounds a
  d (Normalized qw) i | i < w          = qualifier qw `columnFromId` i
                      | otherwise      = error $ "index out of bounds (normalized unit): " ++ show i
    where w = unQualify qw
  d (Scalar sub)    0                  = columnSQL' $ showUnitSQL sub
  d (Scalar _)      i                  = error $ "index out of bounds (scalar unit): " ++ show i

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
columnsOfUntypedProjection p = map (columnOfUntypedProjection p) . take w $ [0 .. ]
  where w = widthOfUntypedProjection p

-- | Get column SQL string list of projection.
projectionColumns :: Projection c r -- ^ Source 'Projection'
                  -> [ColumnSQL]    -- ^ Result SQL string list
projectionColumns =  columnsOfUntypedProjection . untypeProjection

-- | Unsafely get SQL term from 'Proejction'.
unsafeProjectionStringSql :: Projection c r -> StringSQL
unsafeProjectionStringSql =  rowStringSQL . map showsColumnSQL . projectionColumns

-- | Unsafely generate 'Projection' from SQL string list.
unsafeProjectFromColumns :: [ColumnSQL]    -- ^ SQL string list specifies columns
                         -> Projection c r -- ^ Result 'Projection'
unsafeProjectFromColumns =  typedProjection . untypedProjectionFromColumns


-- | Get node attribute.
nodeAttr :: Node -> NodeAttr
nodeAttr (Node a _) = a  where

-- | Get tree from node.
nodeTree :: Node -> ProductTree
nodeTree (Node _ t) = t

-- | Show product tree of query into SQL. StringSQL result.
showsQueryProduct :: QueryProduct -> StringSQL
showsQueryProduct =  rec  where
  joinType Just' Just' = INNER
  joinType Just' Maybe = LEFT
  joinType Maybe Just' = RIGHT
  joinType Maybe Maybe = FULL
  urec n = case nodeTree n of
    p@(Leaf _)     -> rec p
    p@(Join {})    -> SQL.paren (rec p)
  rec (Leaf q)               = qualifiedForm q
  rec (Join left' right' rs) =
    mconcat
    [urec left',
     joinType (nodeAttr left') (nodeAttr right'), JOIN,
     urec right',
     ON, foldr1 SQL.and $ ps ++ concat [ showConstantTermsSQL' True | null ps ] ]
    where ps = [ unsafeProjectionStringSql p | p <- toList rs ]

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
  d ps@(_:_)  =  k <> foldr1 SQL.and [ unsafeProjectionStringSql p | p <- ps ]

-- | Compose WHERE clause from 'QueryRestriction'.
composeWhere :: QueryRestriction Context.Flat -> StringSQL
composeWhere =  composeRestrict WHERE

-- | Compose HAVING clause from 'QueryRestriction'.
composeHaving :: QueryRestriction Context.Aggregated -> StringSQL
composeHaving =  composeRestrict HAVING
