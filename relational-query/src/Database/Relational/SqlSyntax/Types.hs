{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- |
-- Module      : Database.Relational.SqlSyntax.Types
-- Copyright   : 2015-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines sub-query structure used in query products.
module Database.Relational.SqlSyntax.Types
       ( SubQuery (..)

       , Duplication (..), SetOp (..), BinOp (..)
       , Qualifier (..), Qualified (..), qualifier, unQualify, qualify

       , Order (..), Nulls (..), OrderColumn, OrderingTerm

         -- * Product tree type
       , NodeAttr (..), ProductTree (..)
       , Node (..), nodeAttr, nodeTree
       , JoinProduct

       , CaseClause (..), WhenClauses(..)

       , Tuple, tupleWidth
       , Column (..)
       , Record, untypeRecord, record
       , recordWidth
       , typeFromRawColumns
       , typeFromScalarSubQuery

         -- * Query restriction
       , QueryRestriction, QueryRestrictionBuilder

       )  where

import Prelude hiding (and, product)
import Data.DList (DList)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Database.Relational.Internal.Config (Config)
import Database.Relational.Internal.ContextType (Flat, Aggregated)
import Database.Relational.Internal.SQL (StringSQL)
import Database.Relational.Internal.UntypedTable (Untyped)
import Database.Relational.SqlSyntax.Aggregate (AggregateElem)


-- | Result record duplication attribute
data Duplication = All | Distinct  deriving Show

-- | Set operators
data SetOp = Union | Except | Intersect  deriving Show

-- | Set binary operators
newtype BinOp = BinOp (SetOp, Duplication) deriving Show

-- | Order direction. Ascendant or Descendant.
data Order = Asc | Desc  deriving Show

-- | Order of null.
data Nulls =  NullsFirst | NullsLast deriving Show

-- | Type for order-by column
type OrderColumn = StringSQL

-- | Type for order-by term
type OrderingTerm = ((Order, Maybe Nulls), OrderColumn)

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

-- | Type for join product of query.
type JoinProduct = Maybe (ProductTree (QueryRestriction Flat))

-- | when clauses
data WhenClauses =
  WhenClauses [(Tuple, Tuple)] Tuple
  deriving Show

-- | case clause
data CaseClause
  = CaseSearch WhenClauses
  | CaseSimple Tuple WhenClauses
  deriving Show

-- | Projected column structure unit with single column width
data Column
  = RawColumn StringSQL            -- ^ used in immediate value or unsafe operations
  | SubQueryRef (Qualified Int)    -- ^ normalized sub-query reference T<n> with Int index
  | Scalar SubQuery                -- ^ scalar sub-query
  | Case CaseClause Int            -- ^ <n>th column of case clause
  deriving Show

-- | Untyped projected tuple. Forgot record type.
type Tuple = [Column]

-- | Width of 'Tuple'.
tupleWidth :: Tuple -> Int
tupleWidth = length

-- | Phantom typed record. Projected into Haskell record type 't'.
newtype Record c t =
  Record
  { untypeRecord :: Tuple {- ^ Discard record type -} }  deriving Show

-- | Type for restriction of query.
type QueryRestriction c = [Record c (Maybe Bool)]

-- | Builder type to build 'QueryRestriction'.
type QueryRestrictionBuilder = DList (Record Flat (Maybe Bool))

-- | Unsafely type 'Tuple' value to 'Record' type.
record :: Tuple -> Record c t
record = Record

-- | Width of 'Record'.
recordWidth :: Record c r -> Int
recordWidth = length . untypeRecord

-- | Unsafely generate 'Record' from SQL string list.
typeFromRawColumns :: [StringSQL] -- ^ SQL string list specifies columns
                   -> Record c r  -- ^ Result 'Record'
typeFromRawColumns =  record . map RawColumn

-- | Unsafely generate 'Record' from scalar sub-query.
typeFromScalarSubQuery :: SubQuery -> Record c t
typeFromScalarSubQuery = record . (:[]) . Scalar
