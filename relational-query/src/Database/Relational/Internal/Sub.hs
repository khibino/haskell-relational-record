{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- |
-- Module      : Database.Relational.Internal.Sub
-- Copyright   : 2015-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines sub-query structure used in query products.
module Database.Relational.Internal.Sub
       ( SubQuery (..)
       , SetOp (..), BinOp (..), Qualifier (..)
       , Qualified (..), qualifier, unQualify, qualify

         -- * Product tree type
       , NodeAttr (..), ProductTree (..)
       , Node (..), nodeAttr, nodeTree
       , JoinProduct, QueryProductTree
       , ProductTreeBuilder, ProductBuilder

       , CaseClause (..), WhenClauses(..)
       , caseSearch, case'

       , Tuple, UntypedProjection
       , untypedProjectionWidth
       , Column (..), ProjectionUnit
       , Projection, untypeProjection, typedProjection, projectionWidth
       , projectFromColumns, projectFromScalarSubQuery

         -- * Query restriction
       , QueryRestriction
       )  where

import Prelude hiding (and, product)
import Data.DList (DList)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Database.Relational.Internal.Config (Config)
import Database.Relational.Internal.ContextType (Flat, Aggregated)
import Database.Relational.Internal.SQL (StringSQL)
import Database.Relational.Internal.BaseSQL (Duplication (..), OrderingTerm)
import Database.Relational.Internal.GroupingSQL (AggregateElem)
import Database.Relational.Internal.UntypedTable (Untyped)


-- | Set operators
data SetOp = Union | Except | Intersect  deriving Show

-- | Set binary operators
newtype BinOp = BinOp (SetOp, Duplication) deriving Show

-- | Sub-query type
data SubQuery = Table Untyped
              | Flat Config
                Tuple Duplication JoinProduct (QueryRestriction Flat)
                [OrderingTerm]
              | Aggregated Config
                Tuple Duplication JoinProduct (QueryRestriction Flat)
                [AggregateElem] (QueryRestriction Aggregated) [OrderingTerm]
              | Bin BinOp SubQuery SubQuery
              deriving Show

-- | Qualifier type.
newtype Qualifier = Qualifier Int  deriving Show

-- | Qualified query.
data Qualified a =
  Qualified Qualifier a
  deriving (Show, Functor, Foldable, Traversable)

-- | Get qualifier
qualifier :: Qualified a -> Qualifier
qualifier (Qualified q _) = q

-- | Unqualify.
unQualify :: Qualified a -> a
unQualify (Qualified _ a) = a

-- | Add qualifier
qualify :: Qualifier -> a -> Qualified a
qualify = Qualified


-- | node attribute for product.
data NodeAttr = Just' | Maybe deriving Show

type QS = Qualified SubQuery

type QueryRestrictionBuilder = DList (Projection Flat (Maybe Bool))

-- | Product tree type. Product tree is constructed by left node and right node.
data ProductTree rs
  = Leaf QS
  | Join !(Node rs) !(Node rs) !rs
  deriving (Show, Functor)

-- | Product node. node attribute and product tree.
data Node rs = Node !NodeAttr !(ProductTree rs)  deriving (Show, Functor)

-- | Get node attribute.
nodeAttr :: Node rs -> NodeAttr
nodeAttr (Node a _) = a  where

-- | Get tree from node.
nodeTree :: Node rs -> ProductTree rs
nodeTree (Node _ t) = t

-- | Product tree with join restriction.
type QueryProductTree = ProductTree (QueryRestriction Flat)

-- | Product tree with join restriction builder.
type ProductTreeBuilder = ProductTree QueryRestrictionBuilder

-- | Product noe with join restriction builder.
type ProductBuilder = Node QueryRestrictionBuilder

-- | Type for join product of query.
type JoinProduct = Maybe QueryProductTree

-- | when clauses
data WhenClauses =
  WhenClauses [(Tuple, Tuple)] Tuple
  deriving Show

-- | case clause
data CaseClause
  = CaseSearch WhenClauses
  | CaseSimple Tuple WhenClauses
  deriving Show

-- | Projection structure unit with single column width
data Column
  = RawColumn StringSQL            -- ^ used in immediate value or unsafe operations
  | SubQueryRef (Qualified Int)    -- ^ normalized sub-query reference T<n> with Int index
  | Scalar SubQuery                -- ^ scalar sub-query
  | Case CaseClause Int            -- ^ <n>th column of case clause
  deriving Show

{-# DEPRECATED ProjectionUnit "Replaced by Column." #-}
type ProjectionUnit = Column

{-# DEPRECATED UntypedProjection "Replaced by Tuple." #-}
type UntypedProjection = [Column]

-- | Untyped projected tuple. Forgot record type.
type Tuple = [Column]

-- | Width of 'Tuple'.
untypedProjectionWidth :: Tuple -> Int
untypedProjectionWidth = length

-- | Phantom typed projection. Projected into Haskell record type 't'.
newtype Projection c t =
  Projection
  { untypeProjection :: Tuple {- ^ Discard projection value type -} }  deriving Show

-- | Unsafely type projection value.
typedProjection :: Tuple -> Projection c t
typedProjection =  Projection

-- | Width of 'Projection'.
projectionWidth :: Projection c r -> Int
projectionWidth = length . untypeProjection

-- | Unsafely generate 'Projection' from SQL string list.
projectFromColumns :: [StringSQL]    -- ^ SQL string list specifies columns
                   -> Projection c r -- ^ Result 'Projection'
projectFromColumns =  typedProjection . map RawColumn

-- | Unsafely generate 'Projection' from scalar sub-query.
projectFromScalarSubQuery :: SubQuery -> Projection c t
projectFromScalarSubQuery = typedProjection . (:[]) . Scalar

whenClauses :: String                             -- ^ Error tag
            -> [(Projection c a, Projection c b)] -- ^ Each when clauses
            -> Projection c b                     -- ^ Else result projection
            -> WhenClauses                        -- ^ Result clause
whenClauses eTag ws0 e = d ws0
  where
    d []       = error $ eTag ++ ": Empty when clauses!"
    d ws@(_:_) =
      WhenClauses [ (untypeProjection p, untypeProjection r) | (p, r) <- ws ]
      $ untypeProjection e

-- | Search case operator correnponding SQL search /CASE/.
--   Like, /CASE WHEN p0 THEN a WHEN p1 THEN b ... ELSE c END/
caseSearch :: [(Projection c (Maybe Bool), Projection c a)] -- ^ Each when clauses
           -> Projection c a                                -- ^ Else result projection
           -> Projection c a                                -- ^ Result projection
caseSearch ws e =
    typedProjection [ Case c i | i <- [0 .. projectionWidth e - 1] ]
  where
    c = CaseSearch $ whenClauses "caseSearch" ws e

-- | Simple case operator correnponding SQL simple /CASE/.
--   Like, /CASE x WHEN v THEN a WHEN w THEN b ... ELSE c END/
case' :: Projection c a                     -- ^ Projection value to match
      -> [(Projection c a, Projection c b)] -- ^ Each when clauses
      -> Projection c b                     -- ^ Else result projection
      -> Projection c b                     -- ^ Result projection
case' v ws e =
    typedProjection [ Case c i | i <- [0 .. projectionWidth e - 1] ]
  where
    c = CaseSimple (untypeProjection v) $ whenClauses "case'" ws e


-- | Type for restriction of query.
type QueryRestriction c = [Projection c (Maybe Bool)]
