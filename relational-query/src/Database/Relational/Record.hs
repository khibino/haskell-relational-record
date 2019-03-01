{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Record
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines interfaces of projected record type.
module Database.Relational.Record (
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

  -- * List of Record
  RecordList, list, unsafeListFromSubQuery,
  unsafeStringSqlList,
  collectPlaceholders,
  ) where

import Prelude hiding (pi)
import Data.DList (fromList)
import Data.Functor.ProductIsomorphic
  (ProductIsoFunctor, (|$|), ProductIsoApplicative, pureP, (|*|),
   ProductIsoEmpty, pureE, peRight, peLeft, )

import qualified Language.SQL.Keyword as SQL

import Database.Record (HasColumnConstraint, NotNull, NotNullColumnConstraint, PersistableWidth, persistableWidth)
import Database.Record.Persistable (PersistableRecordWidth)
import qualified Database.Record.KeyConstraint as KeyConstraint

import Database.Relational.Internal.ContextType (Aggregated, Flat)
import Database.Relational.Internal.String (StringSQL, listStringSQL, rowStringSQL)
import Database.Relational.SqlSyntax
  (SubQuery, Qualified, Tuple, Record, PlaceholderOffsets,
   recordRawColumns, tupleFromJoinedSubQuery,)
import qualified Database.Relational.SqlSyntax as Syntax

import Database.Relational.Table (Table)
import qualified Database.Relational.Table as Table
import Database.Relational.Pi (Pi)
import qualified Database.Relational.Pi.Unsafe as UnsafePi


-- | Unsafely get SQL term from 'Record'.
unsafeStringSql :: Record c r -> StringSQL
unsafeStringSql = rowStringSQL . recordRawColumns

-- | Get column SQL string list of record.
columns :: Record c r  -- ^ Source 'Record'
        -> [StringSQL] -- ^ Result SQL string list
columns = recordRawColumns

-- | Width of 'Record'.
width :: Record c r -> Int
width = Syntax.recordWidth

-- | Get untyped tuple.
untype :: Record c r -> Tuple
untype = Syntax.untypeRecord


-- | Unsafely generate  'Record' from qualified (joined) sub-query.
unsafeFromQualifiedSubQuery :: PlaceholderOffsets ->  Qualified SubQuery -> Record c t
unsafeFromQualifiedSubQuery phs = Syntax.record phs . tupleFromJoinedSubQuery

-- | Unsafely generate 'Record' from scalar sub-query.
unsafeFromScalarSubQuery :: PlaceholderOffsets ->  SubQuery -> Record c t
unsafeFromScalarSubQuery = Syntax.typeFromScalarSubQuery

-- | Unsafely generate unqualified 'Record' from 'Table'.
unsafeFromTable :: Table r
                -> Record c r
unsafeFromTable = Syntax.typeFromRawColumns mempty . Table.columns

-- | Unsafely generate 'Record' from SQL expression strings.
unsafeFromSqlTerms :: PlaceholderOffsets ->  [StringSQL] -> Record c t
unsafeFromSqlTerms = Syntax.typeFromRawColumns


-- | Unsafely trace projection path.
unsafeProject :: PersistableRecordWidth a -> Record c a' -> Pi a b -> Record c b'
unsafeProject w p pi' =
  Syntax.typeFromRawColumns phs
  . UnsafePi.pi w pi'
  . columns $ p
  where
    phs =
      if Syntax.isPlaceholders p
        then fromList $ UnsafePi.unsafeExpandIndexes' w pi'
        else mempty

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
unsafeCast r =
  Syntax.record  (Syntax.placeholderOffsets r) $ Syntax.untypeRecord r

-- | Composite nested 'Maybe' on record phantom type.
flattenMaybe :: Record c (Maybe (Maybe a)) -> Record c (Maybe a)
flattenMaybe =  unsafeCast

-- | Cast into 'Maybe' on record phantom type.
just :: Record c r -> Record c (Maybe r)
just =  unsafeCast

-- | Unsafely cast context type tag.
unsafeChangeContext :: Record c r -> Record c' r
unsafeChangeContext r =
  Syntax.record (Syntax.placeholderOffsets r) $ Syntax.untypeRecord r

-- | Unsafely lift to aggregated context.
unsafeToAggregated :: Record Flat r -> Record Aggregated r
unsafeToAggregated =  unsafeChangeContext

-- | Unsafely down to flat context.
unsafeToFlat :: Record Aggregated r -> Record Flat r
unsafeToFlat =  unsafeChangeContext

notNullMaybeConstraint :: HasColumnConstraint NotNull r => Record c (Maybe r) -> NotNullColumnConstraint r
notNullMaybeConstraint =  const KeyConstraint.columnConstraint

-- | Unsafely get SQL string expression of not null key record.
unsafeStringSqlNotNullMaybe :: HasColumnConstraint NotNull r => Record c (Maybe r) -> StringSQL
unsafeStringSqlNotNullMaybe p = (!!  KeyConstraint.index (notNullMaybeConstraint p)) . columns $ p

pempty :: Record c ()
pempty = Syntax.record mempty []

-- | Map 'Record' which result type is record.
instance ProductIsoFunctor (Record c) where
  _ |$| p = unsafeCast p

-- | Compose 'Record' using applicative style.
instance ProductIsoApplicative (Record c) where
  pureP _ = unsafeCast pempty
  pf |*| pa =
    Syntax.record
      (pf `Syntax.appendPlaceholderOffsetsOf`  pa)
      (Syntax.untypeRecord pf ++ Syntax.untypeRecord pa)

instance ProductIsoEmpty (Record c) () where
  pureE   = pureP ()
  peRight = unsafeCast
  peLeft  = unsafeCast

-- | Projected record list type for row list.
data RecordList p t = List [p t]
                    | Sub (PlaceholderOffsets) SubQuery

-- | Make projected record list from 'Record' list.
list :: [p t] -> RecordList p t
list =  List

-- | Make projected record list from 'SubQuery'.
unsafeListFromSubQuery :: PlaceholderOffsets -> SubQuery -> RecordList p t
unsafeListFromSubQuery =  Sub

-- | Map record show operatoions and concatinate to single SQL expression.
unsafeStringSqlList :: (p t -> StringSQL) -> RecordList p t -> StringSQL
unsafeStringSqlList sf = d  where
  d (List ps) = listStringSQL $ map sf ps
  d (Sub _ sub) = SQL.paren $ Syntax.showSQL sub

collectPlaceholders :: RecordList (Record c) t -> PlaceholderOffsets
collectPlaceholders (List rs) = foldMap Syntax.placeholderOffsets rs
collectPlaceholders (Sub phs _subq) = phs
