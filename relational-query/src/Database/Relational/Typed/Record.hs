{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Typed.Record
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines typed record structure and interfaces.
module Database.Relational.Typed.Record (
  -- * Record
  Record, untypeRecord, record, PI,
  recordWidth, recordColumns,

  unsafeRecordFromColumns,
  unsafeRecordFromScalarQuery,
  unsafeRecordFromQualifiedQuery,

  -- * Cast phantom types
  flattenMaybe, just,
  unsafeToAggregated, unsafeToFlat, unsafeChangeContext,

  -- * Predicate to restrict Query result
  Predicate,

  -- * case records
  caseSearch, case',
) where

import Data.Functor.ProductIsomorphic
  (ProductIsoFunctor, (|$|), ProductIsoApplicative, pureP, (|*|),
   ProductIsoEmpty, pureE, peRight, peLeft, )

import Database.Relational.Internal.ContextType (Aggregated, Flat)
import Database.Relational.Internal.String (StringSQL)
import Database.Relational.SqlSyntax.Fold (showColumn, tupleFromJoinedSubQuery)
import Database.Relational.SqlSyntax.Types
  (Qualified, SubQuery, Tuple, Column (..),
   WhenClauses (..), CaseClause (..),)


-- | Phantom typed record. Projected into Haskell record type 't'.
newtype Record c t =
  Record
  { untypeRecord :: Tuple {- ^ Discard record type -} }  deriving Show

-- | Type for predicate to restrict of query result.
type Predicate c = Record c (Maybe Bool)

-- | Type for projection function.
type PI c a b = Record c a -> Record c b

-- | Unsafely type 'Tuple' value to 'Record' type.
record :: Tuple -> Record c t
record = Record

-- | Width of 'Record'.
recordWidth :: Record c r -> Int
recordWidth = length . untypeRecord

-- | Get column SQL string list of record.
recordColumns :: Record c r  -- ^ Source 'Record'
              -> [StringSQL] -- ^ Result SQL string list
recordColumns = map showColumn . untypeRecord

-- | Unsafely generate 'Record' from SQL string list.
unsafeRecordFromColumns :: [StringSQL] -- ^ SQL string list specifies columns
                        -> Record c r  -- ^ Result 'Record'
unsafeRecordFromColumns =  record . map RawColumn

-- | Unsafely generate 'Record' from scalar sub-query.
unsafeRecordFromScalarQuery :: SubQuery -> Record c t
unsafeRecordFromScalarQuery = record . (:[]) . Scalar

-- | Unsafely generate  'Record' from qualified (joined) sub-query.
unsafeRecordFromQualifiedQuery :: Qualified SubQuery -> Record c t
unsafeRecordFromQualifiedQuery = record . tupleFromJoinedSubQuery

-----

-- | Composite nested 'Maybe' on record phantom type.
flattenMaybe :: Record c (Maybe (Maybe a)) -> Record c (Maybe a)
flattenMaybe = record . untypeRecord

-- | Cast into 'Maybe' on record phantom type.
just :: Record c r -> Record c (Maybe r)
just = record . untypeRecord

-- | Unsafely cast context type tag.
unsafeChangeContext :: Record c r -> Record c' r
unsafeChangeContext = record . untypeRecord

-- | Unsafely lift to aggregated context.
unsafeToAggregated :: Record Flat r -> Record Aggregated r
unsafeToAggregated = record . untypeRecord

-- | Unsafely down to flat context.
unsafeToFlat :: Record Aggregated r -> Record Flat r
unsafeToFlat = record . untypeRecord

-----

-- | Map 'Record' which result type is record.
instance ProductIsoFunctor (Record c) where
  _ |$| p = record $ untypeRecord p

-- | Compose 'Record' using applicative style.
instance ProductIsoApplicative (Record c) where
  pureP _ = record []
  pf |*| pa = record $ untypeRecord pf ++ untypeRecord pa

instance ProductIsoEmpty (Record c) () where
  pureE   = pureP ()
  peRight = record . untypeRecord
  peLeft  = record . untypeRecord

-----

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
