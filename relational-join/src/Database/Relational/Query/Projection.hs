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
  fromQualifiedSubQuery,

  -- * Projections
  compose,

  pi, piMaybe, piMaybe',

  flattenMaybe, just
  ) where

import Prelude hiding (pi)

import Data.Array (Array, listArray)
import qualified Data.Array as Array

import Database.Record
  (PersistableWidth, persistableWidth, PersistableRecordWidth)
import Database.Record.Persistable (runPersistableRecordWidth)

import Database.Relational.Query.Pi (Pi)
import qualified Database.Relational.Query.Pi as Pi
import Database.Relational.Query.Sub (SubQuery, queryWidth, Qualified)
import qualified Database.Relational.Query.Sub as SubQuery


-- | Projection structure unit
data ProjectionUnit = Columns (Array Int String)
                    | Sub (Qualified SubQuery)

-- | Phantom typed projection. Projected into Haskell record type 't'.
data Projection t = Composed [ProjectionUnit]

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
width =  d  where
  d (Composed prod) = sum . map widthOfUnit $ prod

-- | Get column SQL string of 'Projection'.
column :: Projection r -- ^ Source 'Projection'
       -> Int          -- ^ Column index
       -> String       -- ^ Result SQL string
column =  d  where
  d (Composed us') i' = rec us' i'  where
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
unsafeFromUnit =  Composed . (:[])

-- | Unsafely generate 'Projection' from SQL string list.
unsafeFromColumns :: [String]     -- ^ SQL string list specifies columns
                  -> Projection r -- ^ Result 'Projection'
unsafeFromColumns fs = unsafeFromUnit . Columns $ listArray (0, length fs - 1) fs

-- | Unsafely generate  'Projection' from qualified subquery.
fromQualifiedSubQuery :: Qualified SubQuery -> Projection t
fromQualifiedSubQuery =  unsafeFromUnit . Sub

-- | Concatenate 'Projection'.
compose :: Projection a -> Projection b -> Projection (c a b)
compose (Composed a) (Composed b) = Composed $ a ++ b


-- | Unsafely trace projection path.
unsafeProject :: PersistableRecordWidth b -> Projection a' -> Pi a b -> Projection b'
unsafeProject pr p pi' =
  unsafeFromColumns
  . take (runPersistableRecordWidth pr) . drop (Pi.leafIndex pi')
  . columns $ p

-- | Trace projection path to get narrower 'Projection'.
pi :: PersistableWidth b
   => Projection a -- ^ Source 'Projection'
   -> Pi a b       -- ^ Projection path
   -> Projection b -- ^ Narrower 'Projection'
pi =  unsafeProject persistableWidth

-- | Trace projection path to get narrower 'Projection'. From 'Maybe' type to 'Maybe' type.
piMaybe :: PersistableWidth b
        => Projection (Maybe a) -- ^ Source 'Projection'. 'Maybe' type
        -> Pi a b               -- ^ Projection path
        -> Projection (Maybe b) -- ^ Narrower 'Projection'. 'Maybe' type result
piMaybe =  unsafeProject persistableWidth

-- | Trace projection path to get narrower 'Projection'. From 'Maybe' type to 'Maybe' type.
--   Projection path's leaf is 'Maybe' case.
piMaybe' :: PersistableWidth b
         => Projection (Maybe a) -- ^ Source 'Projection'. 'Maybe' type
         -> Pi a (Maybe b)       -- ^ Projection path. 'Maybe' type leaf
         -> Projection (Maybe b) -- ^ Narrower 'Projection'. 'Maybe' type result
piMaybe' =  unsafeProject persistableWidth

-- | Composite nested 'Maybe' on projection phantom type.
flattenMaybe :: Projection (Maybe (Maybe a)) -> Projection (Maybe a)
flattenMaybe (Composed pus) = Composed pus

-- | Cast into 'Maybe' on projection phantom type.
just :: Projection r -> Projection (Maybe r)
just (Composed us) = Composed us
