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
  -- * Projection data structure and interface
  Projection,

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
  (SubQuery, Qualified, Tuple, Record, Projection)
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


-- | Unsafely get SQL term from 'Proejction'.
unsafeStringSql :: Projection c r -> StringSQL
unsafeStringSql = rowStringSQL . recordRawColumns

-- | Get column SQL string list of projection.
columns :: Projection c r -- ^ Source 'Projection'
        -> [StringSQL]    -- ^ Result SQL string list
columns = recordRawColumns

-- | Width of 'Projection'.
width :: Projection c r -> Int
width = Internal.recordWidth

-- | Unsafely get untyped projection.
untype :: Projection c r -> Tuple
untype = Internal.untypeRecord


-- | Unsafely generate  'Projection' from qualified (joined) sub-query.
unsafeFromQualifiedSubQuery :: Qualified SubQuery -> Projection c t
unsafeFromQualifiedSubQuery = Internal.record . tupleFromJoinedSubQuery

-- | Unsafely generate 'Projection' from scalar sub-query.
unsafeFromScalarSubQuery :: SubQuery -> Projection c t
unsafeFromScalarSubQuery = Internal.typeFromScalarSubQuery

-- | Unsafely generate unqualified 'Projection' from 'Table'.
unsafeFromTable :: Table r
                -> Projection c r
unsafeFromTable = Internal.typeFromRawColumns . Table.columns

-- | Unsafely generate 'Projection' from SQL expression strings.
unsafeFromSqlTerms :: [StringSQL] -> Projection c t
unsafeFromSqlTerms = Internal.typeFromRawColumns


-- | Unsafely trace projection path.
unsafeProject :: PersistableRecordWidth a -> Projection c a' -> Pi a b -> Projection c b'
unsafeProject w p pi' =
  Internal.typeFromRawColumns
  . (UnsafePi.pi w pi')
  . columns $ p

-- | Trace projection path to get narrower 'Projection'.
wpi :: PersistableRecordWidth a
    -> Projection c a -- ^ Source 'Projection'
    -> Pi a b         -- ^ Projection path
    -> Projection c b -- ^ Narrower 'Projection'
wpi =  unsafeProject

-- | Trace projection path to get narrower 'Projection'.
pi :: PersistableWidth a
   => Projection c a -- ^ Source 'Projection'
   -> Pi a b         -- ^ Projection path
   -> Projection c b -- ^ Narrower 'Projection'
pi =  unsafeProject persistableWidth

-- | Trace projection path to get narrower 'Projection'. From 'Maybe' type to 'Maybe' type.
piMaybe :: PersistableWidth a
        => Projection c (Maybe a) -- ^ Source 'Projection'. 'Maybe' type
        -> Pi a b                 -- ^ Projection path
        -> Projection c (Maybe b) -- ^ Narrower 'Projection'. 'Maybe' type result
piMaybe = unsafeProject persistableWidth

-- | Trace projection path to get narrower 'Projection'. From 'Maybe' type to 'Maybe' type.
--   Leaf type of projection path is 'Maybe'.
piMaybe' :: PersistableWidth a
         => Projection c (Maybe a) -- ^ Source 'Projection'. 'Maybe' type
         -> Pi a (Maybe b)         -- ^ Projection path. 'Maybe' type leaf
         -> Projection c (Maybe b) -- ^ Narrower 'Projection'. 'Maybe' type result
piMaybe' = unsafeProject persistableWidth

unsafeCast :: Projection c r -> Projection c r'
unsafeCast = Internal.record . Internal.untypeRecord

-- | Composite nested 'Maybe' on projection phantom type.
flattenMaybe :: Projection c (Maybe (Maybe a)) -> Projection c (Maybe a)
flattenMaybe =  unsafeCast

-- | Cast into 'Maybe' on projection phantom type.
just :: Projection c r -> Projection c (Maybe r)
just =  unsafeCast

-- | Unsafely cast context type tag.
unsafeChangeContext :: Projection c r -> Projection c' r
unsafeChangeContext = Internal.record . Internal.untypeRecord

-- | Unsafely lift to aggregated context.
unsafeToAggregated :: Projection Flat r -> Projection Aggregated r
unsafeToAggregated =  unsafeChangeContext

-- | Unsafely down to flat context.
unsafeToFlat :: Projection Aggregated r -> Projection Flat r
unsafeToFlat =  unsafeChangeContext

notNullMaybeConstraint :: HasColumnConstraint NotNull r => Projection c (Maybe r) -> NotNullColumnConstraint r
notNullMaybeConstraint =  const KeyConstraint.columnConstraint

-- | Unsafely get SQL string expression of not null key projection.
unsafeStringSqlNotNullMaybe :: HasColumnConstraint NotNull r => Projection c (Maybe r) -> StringSQL
unsafeStringSqlNotNullMaybe p = (!!  KeyConstraint.index (notNullMaybeConstraint p)) . columns $ p

-- | Projectable fmap of 'Projection' type.
pfmap :: ProductConstructor (a -> b)
      => (a -> b) -> Projection c a -> Projection c b
_ `pfmap` p = unsafeCast p

-- | Projectable ap of 'Projection' type.
pap :: Projection c (a -> b) -> Projection c a -> Projection c b
pf `pap` pa = Internal.record $ Internal.untypeRecord pf ++ Internal.untypeRecord pa

-- | Compose seed of record type 'Projection'.
instance ProjectableFunctor (Record c) where
  (|$|) = pfmap

-- | Compose record type 'Projection' using applicative style.
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
