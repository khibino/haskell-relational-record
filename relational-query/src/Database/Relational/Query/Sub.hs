{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.Relational.Query.Sub
-- Copyright   : 2013-2017 Kei Hibino
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
  Qualified,
  queryWidth,

  -- deprecated interfaces
  qualifier, unQualify, qualify,

  -- * Sub-query columns
  column,

  -- * Projection
  Projection, ProjectionUnit, UntypedProjection,

  untypedProjectionFromJoinedSubQuery, untypedProjectionFromScalarSubQuery,

  projectionColumns, unsafeProjectionStringSql,

  -- deprecated interfaces
  untypedProjectionFromColumns,
  unsafeProjectFromColumns,
  widthOfUntypedProjection, columnsOfUntypedProjection,

  -- * Product of sub-queries
  JoinProduct, NodeAttr,
  nodeTree,
  ProductBuilder,

  -- * Query restriction
  QueryRestriction,
  composeWhere, composeHaving
  ) where

import Control.Applicative ((<$>))
import Data.Monoid (mempty, (<>), mconcat)
import Data.Traversable (traverse)

import qualified Database.Relational.Query.Context as Context
import Database.Relational.Query.Internal.SQL (StringSQL, stringSQL, rowStringSQL, showStringSQL)
import Database.Relational.Query.Internal.Sub
  (SubQuery (..), Projection, untypeProjection,
   UntypedProjection, untypedProjectionWidth, ProjectionUnit (..),
   JoinProduct, QueryProductTree, ProductBuilder,
   NodeAttr (Just', Maybe), ProductTree (Leaf, Join), Node (Node),
   SetOp (..), BinOp (..), Qualifier (..), Qualified (..),
   QueryRestriction)
import qualified Database.Relational.Query.Internal.Sub as I
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
  d (Flat _ up _ _ _ _)           = untypedProjectionWidth up
  d (Aggregated _ up _ _ _ _ _ _) = untypedProjectionWidth up

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
             (map columnOfProjectionUnit up)
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

{-# DEPRECATED qualifier "prepare to drop public interface. use Database.Relational.Query.Internal.Sub.qualifier." #-}
-- | Get qualifier
qualifier :: Qualified a -> Qualifier
qualifier = I.qualifier

{-# DEPRECATED unQualify "prepare to drop public interface. use Database.Relational.Query.Internal.Sub.unQualify." #-}
-- | Unqualify.
unQualify :: Qualified a -> a
unQualify = I.unQualify

{-# DEPRECATED qualify "prepare to drop public interface. use Database.Relational.Query.Internal.Sub.qualify." #-}
-- | Add qualifier
qualify :: a -> Qualifier -> Qualified a
qualify a q = I.qualify q a

columnN :: Int -> StringSQL
columnN i = stringSQL $ 'f' : show i

asColumnN :: ColumnSQL -> Int -> StringSQL
c `asColumnN` n = showsColumnSQL c `SQL.as` columnN n

-- | Alias string from qualifier
showQualifier :: Qualifier -> StringSQL
showQualifier (Qualifier i) = stringSQL $ 'T' : show i

-- | Binary operator to qualify.
(<.>) :: Qualifier -> ColumnSQL -> ColumnSQL
i <.> n = (showQualifier i SQL.<.>) <$> n

-- | Qualified expression from qualifier and projection index.
columnFromId :: Qualifier -> Int -> ColumnSQL
columnFromId qi i = qi <.> columnSQL' (columnN i)

-- | From 'Qualified' SQL string into qualified formed 'String'
--  like (SELECT ...) AS T<n>
qualifiedSQLas :: Qualified StringSQL -> StringSQL
qualifiedSQLas q = I.unQualify q <> showQualifier (I.qualifier q)

-- | Width of 'Qualified' 'SubQUery'.
queryWidth :: Qualified SubQuery -> Int
queryWidth =  width . I.unQualify

-- | Get column SQL string of 'SubQuery'.
column :: Qualified SubQuery -> Int -> ColumnSQL
column qs =  d (I.unQualify qs)  where
  q = I.qualifier qs
  d (Table u)           i           = q <.> (u ! i)
  d (Bin {})            i           = q `columnFromId` i
  d (Flat _ up _ _ _ _) i           = columnOfUntypedProjection up i
  d (Aggregated _ up _ _ _ _ _ _) i = columnOfUntypedProjection up i


{-# DEPRECATED untypedProjectionFromColumns "prepare to drop public interface. use (map RawColumn)." #-}
-- | Make untyped projection from columns.
untypedProjectionFromColumns :: [ColumnSQL] -> UntypedProjection
untypedProjectionFromColumns =  map RawColumn

-- | Make untyped projection from scalar sub-query.
untypedProjectionFromScalarSubQuery :: SubQuery -> UntypedProjection
untypedProjectionFromScalarSubQuery = (:[]) . Scalar

-- | Make untyped projection from joined sub-query.
untypedProjectionFromJoinedSubQuery :: Qualified SubQuery -> UntypedProjection
untypedProjectionFromJoinedSubQuery qs = d $ I.unQualify qs  where
  normalized = SubQueryRef <$> traverse (\q -> [0 .. width q - 1]) qs
  d (Table _)               =  map RawColumn . map (column qs)
                               $ take (queryWidth qs) [0..]
  d (Bin {})                =  normalized
  d (Flat {})               =  normalized
  d (Aggregated {})         =  normalized

-- | Convert from ProjectionUnit into column.
columnOfProjectionUnit :: ProjectionUnit -> ColumnSQL
columnOfProjectionUnit = d  where
  d (RawColumn e)     = e
  d (SubQueryRef qi)  = I.qualifier qi `columnFromId` I.unQualify qi
  d (Scalar sub)      = columnSQL' $ showUnitSQL sub

{-# DEPRECATED widthOfUntypedProjection "prepare to drop public interface. use untypedProjectionWidth internally." #-}
-- | Width of 'UntypedProjection'.
widthOfUntypedProjection :: UntypedProjection -> Int
widthOfUntypedProjection = untypedProjectionWidth

-- | Get column SQL string of 'UntypedProjection'.
columnOfUntypedProjection :: UntypedProjection -- ^ Source 'Projection'
                          -> Int               -- ^ Column index
                          -> ColumnSQL         -- ^ Result SQL string
columnOfUntypedProjection up i
  | 0 <= i && i < untypedProjectionWidth up    =  columnOfProjectionUnit $ up !! i
  | otherwise                                  =  error $ "columnOfUntypedProjection: index out of bounds: " ++ show i

{-# DEPRECATED columnsOfUntypedProjection "prepare to drop unused interface." #-}
-- | Get column SQL string list of projection.
columnsOfUntypedProjection :: UntypedProjection -- ^ Source 'Projection'
                           -> [ColumnSQL]       -- ^ Result SQL string list
columnsOfUntypedProjection = map columnOfProjectionUnit

-- | Get column SQL string list of projection.
projectionColumns :: Projection c r -- ^ Source 'Projection'
                  -> [ColumnSQL]    -- ^ Result SQL string list
projectionColumns = map columnOfProjectionUnit . untypeProjection

-- | Unsafely get SQL term from 'Proejction'.
unsafeProjectionStringSql :: Projection c r -> StringSQL
unsafeProjectionStringSql =  rowStringSQL . map showsColumnSQL . projectionColumns

{-# DEPRECATED unsafeProjectFromColumns "prepare to drop unused interface. use Database.Relational.Query.Internal.Sub.projectFromColumns. " #-}
-- | Unsafely generate 'Projection' from SQL string list.
unsafeProjectFromColumns :: [ColumnSQL]    -- ^ SQL string list specifies columns
                         -> Projection c r -- ^ Result 'Projection'
unsafeProjectFromColumns = I.projectFromColumns


-- | Get node attribute.
nodeAttr :: Node rs -> NodeAttr
nodeAttr (Node a _) = a  where

-- | Get tree from node.
nodeTree :: Node rs -> ProductTree rs
nodeTree (Node _ t) = t

-- | Show product tree of query into SQL. StringSQL result.
showsQueryProduct :: QueryProductTree -> StringSQL
showsQueryProduct =  rec  where
  joinType Just' Just' = INNER
  joinType Just' Maybe = LEFT
  joinType Maybe Just' = RIGHT
  joinType Maybe Maybe = FULL
  urec n = case nodeTree n of
    p@(Leaf _)     -> rec p
    p@(Join {})    -> SQL.paren (rec p)
  rec (Leaf q)               = qualifiedSQLas $ fmap showUnitSQL q
  rec (Join left' right' rs) =
    mconcat
    [urec left',
     joinType (nodeAttr left') (nodeAttr right'), JOIN,
     urec right',
     ON, foldr1 SQL.and $ ps ++ concat [ showConstantTermsSQL' True | null ps ] ]
    where ps = [ unsafeProjectionStringSql p | p <- rs ]

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
