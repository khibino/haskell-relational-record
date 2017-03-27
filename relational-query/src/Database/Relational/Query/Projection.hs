{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Query.Projection
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines query projection type structure and interfaces.
module Database.Relational.Query.Projection (
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

import Database.Record (HasColumnConstraint, NotNull, NotNullColumnConstraint)
import qualified Database.Record.KeyConstraint as KeyConstraint

import Database.Relational.Query.Internal.SQL (StringSQL, listStringSQL, )
import Database.Relational.Query.Internal.Sub
  (SubQuery, Qualified, UntypedProjection,
   Projection, untypeProjection, typedProjection, projectionWidth)
import qualified Database.Relational.Query.Internal.Sub as Internal
import Database.Relational.Query.Internal.ProjectableClass
  (ProjectableFunctor (..), ProjectableApplicative (..), )

import Database.Relational.Query.Context (Aggregated, Flat)
import Database.Relational.Query.Table (Table)
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Pure (ProductConstructor (..))
import Database.Relational.Query.Pi (Pi)
import qualified Database.Relational.Query.Pi.Unsafe as UnsafePi
import Database.Relational.Query.Sub
  (projectionColumns,
   untypedProjectionFromJoinedSubQuery,
   unsafeProjectionStringSql)
import qualified Database.Relational.Query.Sub as SubQuery


-- | Unsafely get SQL term from 'Proejction'.
unsafeStringSql :: Projection c r -> StringSQL
unsafeStringSql = unsafeProjectionStringSql

-- | Get column SQL string list of projection.
columns :: Projection c r -- ^ Source 'Projection'
        -> [StringSQL]    -- ^ Result SQL string list
columns = projectionColumns

-- | Width of 'Projection'.
width :: Projection c r -> Int
width = projectionWidth

-- | Unsafely get untyped projection.
untype :: Projection c r -> UntypedProjection
untype =  untypeProjection


-- | Unsafely generate  'Projection' from qualified (joined) sub-query.
unsafeFromQualifiedSubQuery :: Qualified SubQuery -> Projection c t
unsafeFromQualifiedSubQuery =  typedProjection . untypedProjectionFromJoinedSubQuery

-- | Unsafely generate 'Projection' from scalar sub-query.
unsafeFromScalarSubQuery :: SubQuery -> Projection c t
unsafeFromScalarSubQuery = Internal.projectFromScalarSubQuery

-- | Unsafely generate unqualified 'Projection' from 'Table'.
unsafeFromTable :: Table r
                -> Projection c r
unsafeFromTable = Internal.projectFromColumns . Table.columns

-- | Unsafely generate 'Projection' from SQL expression strings.
unsafeFromSqlTerms :: [StringSQL] -> Projection c t
unsafeFromSqlTerms = Internal.projectFromColumns


-- | Unsafely trace projection path.
unsafeProject :: Projection c a' -> Pi a b -> Projection c b'
unsafeProject p pi' =
  Internal.projectFromColumns
  . (`UnsafePi.pi` pi')
  . columns $ p

-- | Trace projection path to get narrower 'Projection'.
pi :: Projection c a -- ^ Source 'Projection'
   -> Pi a b         -- ^ Projection path
   -> Projection c b -- ^ Narrower 'Projection'
pi =  unsafeProject

-- | Trace projection path to get narrower 'Projection'. From 'Maybe' type to 'Maybe' type.
piMaybe :: Projection c (Maybe a) -- ^ Source 'Projection'. 'Maybe' type
        -> Pi a b                 -- ^ Projection path
        -> Projection c (Maybe b) -- ^ Narrower 'Projection'. 'Maybe' type result
piMaybe =  unsafeProject

-- | Trace projection path to get narrower 'Projection'. From 'Maybe' type to 'Maybe' type.
--   Leaf type of projection path is 'Maybe'.
piMaybe' :: Projection c (Maybe a) -- ^ Source 'Projection'. 'Maybe' type
         -> Pi a (Maybe b)         -- ^ Projection path. 'Maybe' type leaf
         -> Projection c (Maybe b) -- ^ Narrower 'Projection'. 'Maybe' type result
piMaybe' =  unsafeProject

unsafeCast :: Projection c r -> Projection c r'
unsafeCast =  typedProjection . untypeProjection

-- | Composite nested 'Maybe' on projection phantom type.
flattenMaybe :: Projection c (Maybe (Maybe a)) -> Projection c (Maybe a)
flattenMaybe =  unsafeCast

-- | Cast into 'Maybe' on projection phantom type.
just :: Projection c r -> Projection c (Maybe r)
just =  unsafeCast

-- | Unsafely cast context type tag.
unsafeChangeContext :: Projection c r -> Projection c' r
unsafeChangeContext =  typedProjection . untypeProjection

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
pf `pap` pa = typedProjection $ untypeProjection pf ++ untypeProjection pa

-- | Compose seed of record type 'Projection'.
instance ProjectableFunctor (Projection c) where
  (|$|) = pfmap

-- | Compose record type 'Projection' using applicative style.
instance ProjectableApplicative (Projection c) where
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
