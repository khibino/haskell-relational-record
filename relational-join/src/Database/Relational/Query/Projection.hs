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

  flattenMaybe, just
  ) where

import Prelude hiding (pi)

import Data.Array (Array, listArray)
import qualified Data.Array as Array

import Database.Relational.Query.Table (Table)
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Pi (Pi)
import qualified Database.Relational.Query.Pi.Unsafe as UnsafePi
import Database.Relational.Query.Sub (SubQuery, queryWidth, Qualified)
import qualified Database.Relational.Query.Sub as SubQuery


-- | Projection structure unit
data ProjectionUnit = Columns (Array Int String)
                    | Sub (Qualified SubQuery)

-- | Untyped projection. Forgot record type.
newtype Untyped = Composed { projectionUnits :: [ProjectionUnit] }

-- | Phantom typed projection. Projected into Haskell record type 't'.
newtype Projection t = Projection { untypeProjection :: Untyped }

typedProjection :: Untyped -> Projection t
typedProjection =  Projection

units :: Projection t -> [ProjectionUnit]
units =  projectionUnits . untypeProjection

fromUnits :: [ProjectionUnit] -> Projection t
fromUnits =  typedProjection . Composed

-- | ProjectionUnit width.
widthOfUnit :: ProjectionUnit -> Int
widthOfUnit =  d  where
  d (Columns a) = mx - mn + 1 where (mn, mx) = Array.bounds a
  d (Sub sq)    = queryWidth sq

-- | Get column of ProjectionUnit.
columnOfUnit :: ProjectionUnit -> Int -> String
columnOfUnit =  d  where
  d (Columns a) i | mn <= i && i <= mx = a Array.! i
                  | otherwise          = error $ "index out of bounds (unit): " ++ show i
    where (mn, mx) = Array.bounds a
  d (Sub sq) i = SubQuery.column sq i

-- | Width of 'Projection'.
width :: Projection r -> Int
width =  sum . map widthOfUnit . units  where

-- | Get column SQL string of 'Projection'.
column :: Projection r -- ^ Source 'Projection'
       -> Int          -- ^ Column index
       -> String       -- ^ Result SQL string
column =  d  where
  d proj i' = rec (units proj) i'  where
    rec []       _       = error $ "index out of bounds: " ++ show i'
    rec (u : us) i
      | i < widthOfUnit u = columnOfUnit u i
      | i < 0             = error $ "index out of bounds: " ++ show i
      | otherwise         = rec us (i - widthOfUnit u)

-- | Get column SQL string list of projection.
columns :: Projection r -- ^ Source 'Projection'
        -> [String]     -- ^ Result SQL string list
columns p = map (\n -> column p n) . take w $ [0 .. ]
  where w = width p


-- | Unsafely generate 'Projection' from ProjectionUnit.
unsafeFromUnit :: ProjectionUnit -> Projection t
unsafeFromUnit =  fromUnits . (:[])

-- | Unsafely generate 'Projection' from SQL string list.
unsafeFromColumns :: [String]     -- ^ SQL string list specifies columns
                  -> Projection r -- ^ Result 'Projection'
unsafeFromColumns fs = unsafeFromUnit . Columns $ listArray (0, length fs - 1) fs

-- | Unsafely generate  'Projection' from qualified subquery.
unsafeFromQualifiedSubQuery :: Qualified SubQuery -> Projection t
unsafeFromQualifiedSubQuery =  unsafeFromUnit . Sub

-- | Unsafely generate unqualified 'Projection' from 'Table'.
unsafeFromTable :: Table r
                -> Projection r
unsafeFromTable =  unsafeFromColumns . Table.columns

-- | Concatenate 'Projection'.
compose :: Projection a -> Projection b -> Projection (c a b)
compose a b = fromUnits $ units a ++ units b


-- | Unsafely trace projection path.
unsafeProject :: Projection a' -> Pi a b -> Projection b'
unsafeProject p pi' =
  unsafeFromColumns
  . (`UnsafePi.pi` pi')
  . columns $ p

-- | Trace projection path to get narrower 'Projection'.
pi :: Projection a -- ^ Source 'Projection'
   -> Pi a b       -- ^ Projection path
   -> Projection b -- ^ Narrower 'Projection'
pi =  unsafeProject

-- | Trace projection path to get narrower 'Projection'. From 'Maybe' type to 'Maybe' type.
piMaybe :: Projection (Maybe a) -- ^ Source 'Projection'. 'Maybe' type
        -> Pi a b               -- ^ Projection path
        -> Projection (Maybe b) -- ^ Narrower 'Projection'. 'Maybe' type result
piMaybe =  unsafeProject

-- | Trace projection path to get narrower 'Projection'. From 'Maybe' type to 'Maybe' type.
--   Leaf type of projection path is 'Maybe'.
piMaybe' :: Projection (Maybe a) -- ^ Source 'Projection'. 'Maybe' type
         -> Pi a (Maybe b)       -- ^ Projection path. 'Maybe' type leaf
         -> Projection (Maybe b) -- ^ Narrower 'Projection'. 'Maybe' type result
piMaybe' =  unsafeProject

-- | Composite nested 'Maybe' on projection phantom type.
flattenMaybe :: Projection (Maybe (Maybe a)) -> Projection (Maybe a)
flattenMaybe =  typedProjection . untypeProjection

-- | Cast into 'Maybe' on projection phantom type.
just :: Projection r -> Projection (Maybe r)
just =  typedProjection . untypeProjection
