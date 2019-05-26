{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Record
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines interfaces of projected record type.
module Database.Relational.Record (
  -- * Record
  Record, Predicate, PI,
  recordColumns,

  -- * Projections
  pi, piMaybe, piMaybe',
  wpi,

  unsafeStringSqlNotNullMaybe,

  -- * List of Record
  RecordList, list,

  -- * Deprecated
  width, columns, untype,

  unsafeFromSqlTerms,
  unsafeFromQualifiedSubQuery,
  unsafeFromScalarSubQuery,
  unsafeFromTable,

  unsafeStringSql,

  flattenMaybe, just,

  unsafeToAggregated, unsafeToFlat, unsafeChangeContext,

  unsafeListFromSubQuery,
  unsafeStringSqlList
  ) where

import Prelude hiding (pi)

import Database.Record (HasColumnConstraint, NotNull, NotNullColumnConstraint, PersistableWidth, persistableWidth)
import Database.Record.Persistable (PersistableRecordWidth)
import qualified Database.Record.KeyConstraint as KeyConstraint

import Database.Relational.Internal.ContextType (Flat, Aggregated)
import Database.Relational.Internal.String (StringSQL)
import Database.Relational.SqlSyntax (Tuple)
import Database.Relational.Typed.Record
  (Record, recordColumns, unsafeRecordFromColumns, Predicate, PI,
   RecordList, list)

-- required from deprecated definitions
import Database.Relational.Internal.String (rowStringSQL)
import Database.Relational.SqlSyntax (Qualified, SubQuery)
import qualified Database.Relational.Typed.Record as Typed
import Database.Relational.Typed.Table (Table, tableColumns)
-- required from deprecated definitions

import Database.Relational.Pi (Pi)
import qualified Database.Relational.Pi.Unsafe as UnsafePi


-- | Unsafely trace projection path.
unsafeProject :: PersistableRecordWidth a -> Record c a' -> Pi a b -> Record c b'
unsafeProject w p pi' =
  unsafeRecordFromColumns
  . (UnsafePi.pi w pi')
  . recordColumns $ p

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


notNullMaybeConstraint :: HasColumnConstraint NotNull r => Record c (Maybe r) -> NotNullColumnConstraint r
notNullMaybeConstraint =  const KeyConstraint.columnConstraint

-- | Unsafely get SQL string expression of not null key record.
unsafeStringSqlNotNullMaybe :: HasColumnConstraint NotNull r => Record c (Maybe r) -> StringSQL
unsafeStringSqlNotNullMaybe p = (!!  KeyConstraint.index (notNullMaybeConstraint p)) . recordColumns $ p


-----

width :: Record c r -> Int
width = Typed.recordWidth
{-# DEPRECATED width "use length . recordColumns instead of this." #-}

columns :: Record c r -> [StringSQL]
columns = recordColumns
{-# DEPRECATED columns "use recordColumns instead of this." #-}

{-# DEPRECATED
    untype, unsafeFromQualifiedSubQuery, unsafeFromScalarSubQuery
    "low-level API. dropped in the future." #-}
untype :: Record c t -> Tuple
untype = Typed.untypeRecord

unsafeFromQualifiedSubQuery :: Qualified SubQuery -> Record c t
unsafeFromQualifiedSubQuery = Typed.unsafeRecordFromQualifiedQuery

unsafeFromScalarSubQuery :: SubQuery -> Record c t
unsafeFromScalarSubQuery = Typed.unsafeRecordFromScalarQuery

unsafeFromSqlTerms :: [StringSQL] -> Record c r
unsafeFromSqlTerms = unsafeRecordFromColumns
{-# DEPRECATED unsafeFromSqlTerms "use unsafeProjectSqlTerms instead of this." #-}

unsafeFromTable :: Table r -> Record c r
unsafeFromTable = unsafeRecordFromColumns . tableColumns
{-# DEPRECATED unsafeFromTable "use `unsafeProjectSqlTerms . tableColumns` instead of this." #-}

unsafeStringSql :: Record c r -> StringSQL
unsafeStringSql = rowStringSQL . recordColumns
{-# DEPRECATED unsafeStringSql "use `fromString . showRecordSql` instead of this." #-}

flattenMaybe :: Record c (Maybe (Maybe a)) -> Record c (Maybe a)
flattenMaybe = Typed.flattenMaybe
{-# DEPRECATED flattenMaybe "import from Database.Relational." #-}

just :: Record c r -> Record c (Maybe r)
just = Typed.just
{-# DEPRECATED just "import from Database.Relational." #-}

{-# DEPRECATED
    unsafeToAggregated, unsafeToFlat, unsafeChangeContext
    "use unsafeProejctSql . recordColumns instead of this." #-}
unsafeToAggregated :: Record Flat r -> Record Aggregated r
unsafeToAggregated = Typed.unsafeToAggregated

unsafeToFlat :: Record Aggregated r -> Record Flat r
unsafeToFlat = Typed.unsafeToFlat

unsafeChangeContext :: Record c r -> Record c' r
unsafeChangeContext = Typed.unsafeChangeContext

{-# DEPRECATED
    unsafeListFromSubQuery, unsafeStringSqlList
    "low-level API. dropped in the future." #-}
unsafeListFromSubQuery :: SubQuery -> RecordList p t
unsafeListFromSubQuery = Typed.unsafeListFromSubQuery

unsafeStringSqlList :: (p t -> StringSQL) -> RecordList p t -> StringSQL
unsafeStringSqlList = Typed.unsafeStringSqlList
