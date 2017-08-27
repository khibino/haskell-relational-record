-- |
-- Module      : Database.Relational.SqlSyntax.Query
-- Copyright   : 2013-2017 Kei Hibino
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

  composeOrderBy,
  ) where

import Data.Monoid (mempty, (<>))

import Language.SQL.Keyword (Keyword(..), (|*|))
import qualified Language.SQL.Keyword as SQL

import Database.Relational.Internal.Config (Config)
import Database.Relational.Internal.ContextType (Flat, Aggregated)
import Database.Relational.Internal.String (StringSQL)
import Database.Relational.SqlSyntax.Types
  (Duplication (..), SetOp (..), BinOp (..),
   Order (..), Nulls (..), OrderingTerm, AggregateElem,
   JoinProduct, Predicate, WhenClauses (..), CaseClause (..), SubQuery (..),
   Column (..), Tuple, Record, record, untypeRecord, recordWidth, )


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
    record [ Case c i | i <- [0 .. recordWidth e - 1] ]
  where
    c = CaseSearch $ whenClauses "caseSearch" ws e

-- | Simple case operator correnponding SQL simple /CASE/.
--   Like, /CASE x WHEN v THEN a WHEN w THEN b ... ELSE c END/
case' :: Record c a                 -- ^ Record value to match
      -> [(Record c a, Record c b)] -- ^ Each when clauses
      -> Record c b                 -- ^ Else result record
      -> Record c b                 -- ^ Result record
case' v ws e =
    record [ Case c i | i <- [0 .. recordWidth e - 1] ]
  where
    c = CaseSimple (untypeRecord v) $ whenClauses "case'" ws e


-- | Compose ORDER BY clause from OrderingTerms
composeOrderBy :: [OrderingTerm] -> StringSQL
composeOrderBy =  d where
  d []       = mempty
  d ts@(_:_) = ORDER <> BY <> SQL.fold (|*|) (map showsOt ts)
  showsOt ((o, mn), e) = e <> order o <> maybe mempty ((NULLS <>) . nulls) mn
  order Asc  = ASC
  order Desc = DESC
  nulls NullsFirst = FIRST
  nulls NullsLast  = LAST
