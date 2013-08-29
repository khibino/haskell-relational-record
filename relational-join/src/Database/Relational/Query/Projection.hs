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

  unsafeFromColumns,
  unsafeFromQualifiedSubQuery,
  unsafeFromTable,

  -- * Projections
  compose,

  pi, piMaybe, piMaybe',

  flattenMaybe, just,

  unsafeToAggregated, unsafeToFlat,

  unsafeShowSqlProjection
  ) where

import Prelude hiding (pi)

import Database.Relational.Query.Internal.String (sqlRowString)
import Database.Relational.Query.Context (Aggregated, Flat)
import Database.Relational.Query.Table (Table, ColumnSQL, stringFromColumnSQL)
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Pi (Pi)
import qualified Database.Relational.Query.Pi.Unsafe as UnsafePi
import Database.Relational.Query.Sub
  (SubQuery, Qualified,
   ProjectionUnit, widthOfProjectionUnit, columnOfProjectionUnit,
   UntypedProjection, untypedProjectionFromColumns, untypedProjectionFromSubQuery)


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
width =  sum . map widthOfProjectionUnit . units  where

-- | Get column SQL string of 'Projection'.
column :: Projection c r -- ^ Source 'Projection'
       -> Int            -- ^ Column index
       -> ColumnSQL      -- ^ Result SQL string
column =  d  where
  d proj i' = rec (units proj) i'  where
    rec []       _        = error $ "index out of bounds: " ++ show i'
    rec (u : us) i
      | i < widthOfProjectionUnit u = columnOfProjectionUnit u i
      | i < 0             = error $ "index out of bounds: " ++ show i
      | otherwise         = rec us (i - widthOfProjectionUnit u)

-- | Get column SQL string list of projection.
columns :: Projection c r -- ^ Source 'Projection'
        -> [ColumnSQL]    -- ^ Result SQL string list
columns p = map (\n -> column p n) . take w $ [0 .. ]
  where w = width p


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

-- | Unsafely get SQL term from 'Proejction'.
unsafeShowSqlProjection :: Projection c r -> String
unsafeShowSqlProjection =  sqlRowString . map stringFromColumnSQL . columns
