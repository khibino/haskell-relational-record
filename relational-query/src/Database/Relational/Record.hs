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
  columnsWithPlaceholders,
  untype,
  pempty,

  unsafeFromSqlTerms,
  unsafeFromQualifiedSubQuery,
  unsafeFromScalarSubQuery,
  unsafeFromTable,

  unsafeStringSql,
  unsafeStringSqlWithPlaceholders,

  -- * Projections
  pi, piMaybe, piMaybe',
  wpi,

  flattenMaybe, just,

  unsafeToAggregated, unsafeToFlat, unsafeChangeContext,
  toAggregated, toFlat, toSomeOperatorContext,
  unsafeStringSqlNotNullMaybe,

  -- * List of Record
  RecordList, list, unsafeListFromSubQuery,
  unsafeStringSqlList
  ) where

import Prelude hiding (pi)
import qualified Data.DList as DList
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mempty)
import Data.Functor.ProductIsomorphic
  (ProductIsoFunctor, (|$|), ProductIsoApplicative, pureP, (|*|),
   ProductIsoEmpty, pureE, peRight, peLeft, )
import Data.Traversable (traverse)

import qualified Language.SQL.Keyword as SQL

import Database.Record (HasColumnConstraint, NotNull, NotNullColumnConstraint, PersistableWidth, persistableWidth)
import Database.Record.Persistable (PersistableRecordWidth)
import qualified Database.Record.KeyConstraint as KeyConstraint

import Database.Relational.Internal.ContextType (Aggregated, Flat, PureOperand)
import Database.Relational.Internal.String (StringSQL, listStringSQL, rowStringSQL)
import Database.Relational.SqlSyntax
  (SubQuery, Qualified, Tuple, Record,
   typedTupleRawColumns, tupleFromJoinedSubQuery,)
import qualified Database.Relational.SqlSyntax as Syntax

import Database.Relational.Table (Table)
import qualified Database.Relational.Table as Table
import Database.Relational.Pi (Pi)
import qualified Database.Relational.Pi.Unsafe as UnsafePi
import Database.Relational.Projectable.Unsafe (OperatorContext)


-- | Unsafely get SQL term from 'Record'.
unsafeStringSql :: Record c r -> StringSQL
unsafeStringSql =
  rowStringSQL . typedTupleRawColumns . Syntax.detachPlaceholderOffsets . Syntax.toTypedTuple

unsafeStringSqlWithPlaceholders :: Record c r -> Syntax.SQLWithPlaceholderOffsets'
unsafeStringSqlWithPlaceholders =
  fmap (rowStringSQL . typedTupleRawColumns) . Syntax.toTypedTuple

-- | Get column SQL string list of record.
columns :: Record c r  -- ^ Source 'Record'
        -> [StringSQL] -- ^ Result SQL string list
columns =
  Syntax.typedTupleRawColumns . Syntax.detachPlaceholderOffsets . Syntax.toTypedTuple

columnsWithPlaceholders :: Record c r -> Syntax.WithPlaceholderOffsets [StringSQL]
columnsWithPlaceholders = fmap Syntax.typedTupleRawColumns . Syntax.toTypedTuple

-- | Width of 'Record'.
width :: Record c r -> Int
width = Syntax.recordWidth

-- | Get untyped tuple.
untype :: Record c r -> Tuple
untype = Syntax.untypeRecord

-- | Unsafely generate  'Record' from qualified (joined) sub-query.
unsafeFromQualifiedSubQuery :: Qualified SubQuery -> Record c t
unsafeFromQualifiedSubQuery =
  Syntax.unsafeRecordFromTupleWithPlaceholderOffsets . tupleFromJoinedSubQuery

-- | Unsafely generate 'Record' from scalar sub-query.
unsafeFromScalarSubQuery :: SubQuery -> Record c t
unsafeFromScalarSubQuery sq =
  Syntax.record (Syntax.collectPlaceholderOffsets sq) . (:[]) $ Syntax.Scalar sq

-- | Unsafely generate unqualified 'Record' from 'Table'.
unsafeFromTable :: Table r
                -> Record c r
unsafeFromTable = Syntax.typeFromRawColumns mempty . Table.columns

-- | Unsafely generate 'Record' from SQL expression strings.
unsafeFromSqlTerms :: Syntax.WithPlaceholderOffsets [StringSQL] -> Record c t
unsafeFromSqlTerms = uncurry (flip Syntax.typeFromRawColumns) . Syntax.tupleFromPlaceholderOffsets

-- | Unsafely trace projection path.
unsafeProject :: PersistableRecordWidth a -> Record c a' -> Pi a b -> Record c b'
unsafeProject w p pi' =
  Syntax.typeFromRawColumns phs
    . (UnsafePi.pi w pi')
    $ columns p
 where
  phs = if Syntax.isPlaceholdersRecord p
          then DList.fromList $ UnsafePi.expandIndexes' w pi'
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
unsafeCast = Syntax.mapTypedTuple (Syntax.forciblyTypeTuple . Syntax.untypeTuple)

-- | Composite nested 'Maybe' on record phantom type.
flattenMaybe :: Record c (Maybe (Maybe a)) -> Record c (Maybe a)
flattenMaybe =  unsafeCast

-- | Cast into 'Maybe' on record phantom type.
just :: Record c r -> Record c (Maybe r)
just =  unsafeCast

-- | Unsafely cast context type tag.
unsafeChangeContext :: Record c r -> Record c' r
unsafeChangeContext = Syntax.mapTypedTuple (Syntax.forciblyTypeTuple . Syntax.untypeTuple)

-- | Unsafely lift to aggregated context.
unsafeToAggregated :: Record Flat r -> Record Aggregated r
unsafeToAggregated =  unsafeChangeContext

-- | Unsafely down to flat context.
unsafeToFlat :: Record Aggregated r -> Record Flat r
unsafeToFlat =  unsafeChangeContext

-- | Convert pure operand context into aggregated context.
toAggregated :: Record PureOperand r -> Record Aggregated r
toAggregated =  unsafeChangeContext

-- | Convert pure operand context into flat context.
toFlat :: Record PureOperand r -> Record Flat r
toFlat =  unsafeChangeContext

-- | Convert pure operand context into some operator context.
toSomeOperatorContext :: OperatorContext c => Record PureOperand r -> Record c r
toSomeOperatorContext =  unsafeChangeContext

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
  pff |*| pfa = Syntax.Record (Syntax.forciblyTypeTuple <$> csphs)
   where
    csphs = (++) <$> Syntax.untypeRecordWithPlaceholderOffsets pff <*> Syntax.untypeRecordWithPlaceholderOffsets pfa

instance ProductIsoEmpty (Record c) () where
  pureE   = pureP ()
  peRight = unsafeCast
  peLeft  = unsafeCast

-- | Projected record list type for row list.
data RecordList p t = List [p t]
                    | Sub SubQuery

-- | Make projected record list from 'Record' list.
list :: [p t] -> RecordList p t
list =  List

-- | Make projected record list from 'SubQuery'.
unsafeListFromSubQuery :: SubQuery -> RecordList p t
unsafeListFromSubQuery =  Sub

-- | Map record show operatoions and concatinate to single SQL expression.
unsafeStringSqlList :: (p t -> Syntax.SQLWithPlaceholderOffsets') -> RecordList p t -> Syntax.SQLWithPlaceholderOffsets'
unsafeStringSqlList sf = d  where
  d (List ps) = listStringSQL <$> traverse sf ps
  d (Sub sub) = Syntax.withPlaceholderOffsets (Syntax.collectPlaceholderOffsets sub) . SQL.paren $ Syntax.showSQL sub
