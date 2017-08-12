{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Projection
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines query projection type structure and interfaces.
module Database.Relational.Projection (
  -- * Record data structure and interface
  Record,

  width,
  columns,
  untype,

  unsafeFromSqlTerms,
  unsafeFromQualifiedSubQuery,
  unsafeFromScalarSubQuery,
  unsafeFromTable,

  unsafeStringSql,

  -- * Projections
  pi, piMaybe, piMaybe',
  wpi,

  flattenMaybe, just,

  unsafeToAggregated, unsafeToFlat, unsafeChangeContext,
  unsafeStringSqlNotNullMaybe,

  pfmap, pap,

  -- * List Projection
  ListProjection, list, unsafeListFromSubQuery,
  unsafeStringSqlList
  ) where

import Prelude hiding (pi)

import qualified Language.SQL.Keyword as SQL

import Database.Record (HasColumnConstraint, NotNull, NotNullColumnConstraint, PersistableWidth, persistableWidth)
import Database.Record.Persistable (PersistableRecordWidth)
import qualified Database.Record.KeyConstraint as KeyConstraint

import Database.Relational.Internal.SQL (StringSQL, listStringSQL, rowStringSQL)
import Database.Relational.Internal.Sub
  (SubQuery, Qualified, Tuple, Record)
import qualified Database.Relational.Internal.Sub as Internal

import Database.Relational.ProjectableClass
  (ProductConstructor (..), ProjectableFunctor (..), ProjectableApplicative (..), )
import Database.Relational.Context (Aggregated, Flat)
import Database.Relational.Table (Table)
import qualified Database.Relational.Table as Table
import Database.Relational.Pi (Pi)
import qualified Database.Relational.Pi.Unsafe as UnsafePi
import Database.Relational.Sub
  (recordRawColumns, tupleFromJoinedSubQuery, )
import qualified Database.Relational.Sub as SubQuery


-- | Unsafely get SQL term from 'Record'.
unsafeStringSql :: Record c r -> StringSQL
unsafeStringSql = rowStringSQL . recordRawColumns

-- | Get column SQL string list of projection.
columns :: Record c r  -- ^ Source 'Record'
        -> [StringSQL] -- ^ Result SQL string list
columns = recordRawColumns

-- | Width of 'Record'.
width :: Record c r -> Int
width = Internal.recordWidth

-- | Unsafely get untyped projection.
untype :: Record c r -> Tuple
untype = Internal.untypeRecord


-- | Unsafely generate  'Record' from qualified (joined) sub-query.
unsafeFromQualifiedSubQuery :: Qualified SubQuery -> Record c t
unsafeFromQualifiedSubQuery = Internal.record . tupleFromJoinedSubQuery

-- | Unsafely generate 'Record' from scalar sub-query.
unsafeFromScalarSubQuery :: SubQuery -> Record c t
unsafeFromScalarSubQuery = Internal.typeFromScalarSubQuery

-- | Unsafely generate unqualified 'Record' from 'Table'.
unsafeFromTable :: Table r
                -> Record c r
unsafeFromTable = Internal.typeFromRawColumns . Table.columns

-- | Unsafely generate 'Record' from SQL expression strings.
unsafeFromSqlTerms :: [StringSQL] -> Record c t
unsafeFromSqlTerms = Internal.typeFromRawColumns


-- | Unsafely trace projection path.
unsafeProject :: PersistableRecordWidth a -> Record c a' -> Pi a b -> Record c b'
unsafeProject w p pi' =
  Internal.typeFromRawColumns
  . (UnsafePi.pi w pi')
  . columns $ p

-- | Trace projection path to get narrower 'Record'.
wpi :: PersistableRecordWidth a
    -> Record c a -- ^ Source 'Record'
    -> Pi a b     -- ^ Projection path
    -> Record c b -- ^ Narrower 'Record'
wpi =  unsafeProject

-- | Trace projection path to get narrower 'Record'.
pi :: PersistableWidth a
   => Record c a -- ^ Source 'Record'
   -> Pi a b     -- ^ Record path
   -> Record c b -- ^ Narrower 'Record'
pi =  unsafeProject persistableWidth

-- | Trace projection path to get narrower 'Record'. From 'Maybe' type to 'Maybe' type.
piMaybe :: PersistableWidth a
        => Record c (Maybe a) -- ^ Source 'Record'. 'Maybe' type
        -> Pi a b             -- ^ Projection path
        -> Record c (Maybe b) -- ^ Narrower 'Record'. 'Maybe' type result
piMaybe = unsafeProject persistableWidth

-- | Trace projection path to get narrower 'Record'. From 'Maybe' type to 'Maybe' type.
--   Leaf type of projection path is 'Maybe'.
piMaybe' :: PersistableWidth a
         => Record c (Maybe a) -- ^ Source 'Record'. 'Maybe' type
         -> Pi a (Maybe b)     -- ^ Projection path. 'Maybe' type leaf
         -> Record c (Maybe b) -- ^ Narrower 'Record'. 'Maybe' type result
piMaybe' = unsafeProject persistableWidth

unsafeCast :: Record c r -> Record c r'
unsafeCast = Internal.record . Internal.untypeRecord

-- | Composite nested 'Maybe' on projection phantom type.
flattenMaybe :: Record c (Maybe (Maybe a)) -> Record c (Maybe a)
flattenMaybe =  unsafeCast

-- | Cast into 'Maybe' on projection phantom type.
just :: Record c r -> Record c (Maybe r)
just =  unsafeCast

-- | Unsafely cast context type tag.
unsafeChangeContext :: Record c r -> Record c' r
unsafeChangeContext = Internal.record . Internal.untypeRecord

-- | Unsafely lift to aggregated context.
unsafeToAggregated :: Record Flat r -> Record Aggregated r
unsafeToAggregated =  unsafeChangeContext

-- | Unsafely down to flat context.
unsafeToFlat :: Record Aggregated r -> Record Flat r
unsafeToFlat =  unsafeChangeContext

notNullMaybeConstraint :: HasColumnConstraint NotNull r => Record c (Maybe r) -> NotNullColumnConstraint r
notNullMaybeConstraint =  const KeyConstraint.columnConstraint

-- | Unsafely get SQL string expression of not null key projection.
unsafeStringSqlNotNullMaybe :: HasColumnConstraint NotNull r => Record c (Maybe r) -> StringSQL
unsafeStringSqlNotNullMaybe p = (!!  KeyConstraint.index (notNullMaybeConstraint p)) . columns $ p

-- | Projectable fmap of 'Record' type.
pfmap :: ProductConstructor (a -> b)
      => (a -> b) -> Record c a -> Record c b
_ `pfmap` p = unsafeCast p

-- | Projectable ap of 'Record' type.
pap :: Record c (a -> b) -> Record c a -> Record c b
pf `pap` pa = Internal.record $ Internal.untypeRecord pf ++ Internal.untypeRecord pa

-- | Compose seed of record type 'Record'.
instance ProjectableFunctor (Record c) where
  (|$|) = pfmap

-- | Compose record type 'Record' using applicative style.
instance ProjectableApplicative (Record c) where
  (|*|) = pap

-- | Projection type for row list.
data ListProjection p t = List [p t]
                        | Sub SubQuery

-- | Make row list projection from 'Projection' list.
list :: [p t] -> ListProjection p t
list =  List

-- | Make row list projection from 'SubQuery'.
unsafeListFromSubQuery :: SubQuery -> ListProjection p t
unsafeListFromSubQuery =  Sub

-- | Map projection show operatoions and concatinate to single SQL expression.
unsafeStringSqlList :: (p t -> StringSQL) -> ListProjection p t -> StringSQL
unsafeStringSqlList sf = d  where
  d (List ps) = listStringSQL $ map sf ps
  d (Sub sub) = SQL.paren $ SubQuery.showSQL sub
