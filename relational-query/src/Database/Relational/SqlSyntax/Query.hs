-- |
-- Module      : Database.Relational.SqlSyntax.Query
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides building and expanding operations of SQL query tree.
module Database.Relational.SqlSyntax.Query (
  flatSubQuery, aggregatedSubQuery,
  union, except, intersect,
  caseSearch, case',
  ) where

import Data.Monoid ((<>))
import Database.Relational.Internal.Config (Config)
import Database.Relational.Internal.ContextType (Flat, Aggregated)
import Database.Relational.SqlSyntax.Types
  (Duplication (..), SetOp (..), BinOp (..),
   OrderingTerm, AggregateElem,
   JoinProduct, Predicate, WhenClauses (..), CaseClause (..), SubQuery (..),
   Column (..), Tuple, Record, record, untypeRecord, recordWidth, placeholderOffsets, )


-- | Unsafely generate flat 'SubQuery' from untyped components.
flatSubQuery :: Config
             -> Tuple
             -> Duplication
             -> JoinProduct
             -> [Predicate Flat]
             -> [OrderingTerm]
             -> SubQuery
flatSubQuery = Flat

-- | Unsafely generate aggregated 'SubQuery' from untyped components.
aggregatedSubQuery :: Config
                   -> Tuple
                   -> Duplication
                   -> JoinProduct
                   -> [Predicate Flat]
                   -> [AggregateElem]
                   -> [Predicate Aggregated]
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


whenClauses :: String                     -- ^ Error tag
            -> [(Record c a, Record c b)] -- ^ Each when clauses
            -> Record c b                 -- ^ Else result record
            -> WhenClauses                -- ^ Result clause
whenClauses eTag ws0 e = d ws0
  where
    d []       = error $ eTag ++ ": Empty when clauses!"
    d ws@(_:_) =
      WhenClauses [ (untypeRecord p, untypeRecord r) | (p, r) <- ws ]
      $ untypeRecord e

-- | Search case operator correnponding SQL search /CASE/.
--   Like, /CASE WHEN p0 THEN a WHEN p1 THEN b ... ELSE c END/
caseSearch :: [(Predicate c, Record c a)] -- ^ Each when clauses
           -> Record c a                  -- ^ Else result record
           -> Record c a                  -- ^ Result record
caseSearch ws e =
    record
      (ws' <> placeholderOffsets e)
      [ Case c i | i <- [0 .. recordWidth e - 1] ]
  where
    c = CaseSearch $ whenClauses "caseSearch" ws e
    ws' = foldMap (\(wa, wb) -> placeholderOffsets wa <> placeholderOffsets wb) ws

-- | Simple case operator correnponding SQL simple /CASE/.
--   Like, /CASE x WHEN v THEN a WHEN w THEN b ... ELSE c END/
case' :: Record c a                 -- ^ Record value to match
      -> [(Record c a, Record c b)] -- ^ Each when clauses
      -> Record c b                 -- ^ Else result record
      -> Record c b                 -- ^ Result record
case' v ws e =
    record
      (placeholderOffsets v <> ws' <> placeholderOffsets e)
      [ Case c i | i <- [0 .. recordWidth e - 1] ]
  where
    c = CaseSimple (untypeRecord v) $ whenClauses "case'" ws e
    ws' = foldMap (\(wa, wb) -> placeholderOffsets wa <> placeholderOffsets wb) ws
