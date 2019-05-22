-- |
-- Module      : Database.Relational.SqlSyntax.Record
-- Copyright   : 2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines typed record structure.
module Database.Relational.SqlSyntax.Record (
  -- * Record
  Record, untypeRecord, record, PI,
  recordWidth,
  typeFromRawColumns,
  typeFromScalarSubQuery,

  -- * Predicate to restrict Query result
  Predicate,
) where

import Database.Relational.Internal.String (StringSQL)
import Database.Relational.SqlSyntax.Types (SubQuery, Tuple, Column (..))


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

-- | Unsafely generate 'Record' from SQL string list.
typeFromRawColumns :: [StringSQL] -- ^ SQL string list specifies columns
                   -> Record c r  -- ^ Result 'Record'
typeFromRawColumns =  record . map RawColumn

-- | Unsafely generate 'Record' from scalar sub-query.
typeFromScalarSubQuery :: SubQuery -> Record c t
typeFromScalarSubQuery = record . (:[]) . Scalar
