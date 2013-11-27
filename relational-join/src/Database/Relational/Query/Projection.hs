-- |
-- Module      : Database.Relational.Query.Projection
-- Copyright   : 2013 Kei Hibino
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

  unsafeFromColumns,
  unsafeFromQualifiedSubQuery,
  unsafeFromTable,

  -- * Projections
  compose,

  pi, piMaybe, piMaybe',

  flattenMaybe, just,

  unsafeToAggregated, unsafeToFlat,

  -- * List Projection
  ListProjection, list, unsafeListProjectionFromSubQuery,
  unsafeShowSqlListProjection,

  -- * Aggregated element
  AggregatedElements, aggregatedElements, runAggregatedElements,
  unsafeToAggregatedElements
  ) where

import Prelude hiding (pi)

import Database.Relational.Query.Internal.String (paren, sqlRowListString)
import Database.Relational.Query.Context (Aggregated, Flat)
import Database.Relational.Query.Component (ColumnSQL, AggregateElem, aggregateTerm)
import Database.Relational.Query.Table (Table)
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Pi (Pi)
import qualified Database.Relational.Query.Pi.Unsafe as UnsafePi
import Database.Relational.Query.Sub
  (SubQuery, Qualified, ProjectionUnit,
   UntypedProjection, widthOfUntypedProjection, columnsOfUntypedProjection,
   untypedProjectionFromColumns, untypedProjectionFromSubQuery)
import qualified Database.Relational.Query.Sub as SubQuery


-- | Phantom typed projection. Projected into Haskell record type 't'.
newtype Projection c t = Projection { untypeProjection :: UntypedProjection }

typedProjection :: UntypedProjection -> Projection c t
typedProjection =  Projection

units :: Projection c t -> [ProjectionUnit]
units =  untypeProjection

fromUnits :: [ProjectionUnit] -> Projection c t
fromUnits =  typedProjection

-- | Width of 'Projection'.
width :: Projection c r -> Int
width =  widthOfUntypedProjection . untypeProjection

-- | Get column SQL string list of projection.
columns :: Projection c r -- ^ Source 'Projection'
        -> [ColumnSQL]    -- ^ Result SQL string list
columns =  columnsOfUntypedProjection . untypeProjection

-- | Unsafely get untyped projection.
untype :: Projection c r -> UntypedProjection
untype =  untypeProjection


-- | Unsafely generate 'Projection' from SQL string list.
unsafeFromColumns :: [ColumnSQL]    -- ^ SQL string list specifies columns
                  -> Projection c r -- ^ Result 'Projection'
unsafeFromColumns =  typedProjection . untypedProjectionFromColumns

-- | Unsafely generate  'Projection' from qualified subquery.
unsafeFromQualifiedSubQuery :: Qualified SubQuery -> Projection c t
unsafeFromQualifiedSubQuery =  typedProjection . untypedProjectionFromSubQuery

-- | Unsafely generate unqualified 'Projection' from 'Table'.
unsafeFromTable :: Table r
                -> Projection c r
unsafeFromTable =  unsafeFromColumns . Table.columns

-- | Concatenate 'Projection'.
compose :: Projection c a -> Projection c b -> Projection c (pair a b)
compose a b = fromUnits $ units a ++ units b


-- | Unsafely trace projection path.
unsafeProject :: Projection c a' -> Pi a b -> Projection c b'
unsafeProject p pi' =
  unsafeFromColumns
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

unsafeChangeContext :: Projection c r -> Projection c' r
unsafeChangeContext =  typedProjection . untypeProjection

-- | Unsafely lift to aggregated context.
unsafeToAggregated :: Projection Flat r -> Projection Aggregated r
unsafeToAggregated =  unsafeChangeContext

-- | Unsafely down to flat context.
unsafeToFlat :: Projection Aggregated r -> Projection Flat r
unsafeToFlat =  unsafeChangeContext


-- | Projection type for row list.
data ListProjection p t = List [p t]
                        | Sub SubQuery

-- | Make row list projection from 'Projection' list.
list :: [p t] -> ListProjection p t
list =  List

-- | Make row list projection from 'SubQuery'.
unsafeListProjectionFromSubQuery :: SubQuery -> ListProjection p t
unsafeListProjectionFromSubQuery =  Sub

-- | Map projection show operatoions and concatinate to single SQL expression.
unsafeShowSqlListProjection :: (p t -> String) -> ListProjection p t -> String
unsafeShowSqlListProjection sf = d  where
  d (List ps) = sqlRowListString $ map sf ps
  d (Sub sub) = paren $ SubQuery.toSQL sub


-- | Aggregated elements context and result.
newtype AggregatedElements a = AggregatedElements (a, [AggregateElem])

-- | Pack aggregated elements.
aggregatedElements :: (a, [AggregateElem]) -> AggregatedElements a
aggregatedElements =  AggregatedElements

-- | Unpack aggregated elements.
runAggregatedElements :: AggregatedElements a -> (a, [AggregateElem])
runAggregatedElements (AggregatedElements p) = p

-- | Unsafely make 'AggregatedElements' from flat 'Projection'.
unsafeToAggregatedElements :: Projection Flat r -> AggregatedElements (Projection Aggregated r)
unsafeToAggregatedElements p =
  aggregatedElements (unsafeToAggregated p, map aggregateTerm . columns $ p)
