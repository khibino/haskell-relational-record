module Database.Relational.Query.Projection (
  Projection, outer,

  width,

  columns,

  compose, fromQualifiedSubQuery,

  toExpr,

  pi, piMaybe, Projectable (project),

  value,

  valueTrue, valueFalse,

  SqlProjectable (unsafeSqlValue),
  valueNull, placeholder
  ) where

import Prelude hiding ((!!), pi)

import Data.Array (Array, listArray)
import qualified Data.Array as Array
import Data.List (intercalate)

import Database.Record.Persistable
  (PersistableRecordWidth, runPersistableRecordWidth,
   PersistableWidth, persistableWidth)

import Database.Relational.Query.Pi (Pi)
import qualified Database.Relational.Query.Pi as Pi
import Database.Relational.Query.Expr (Expr, ShowConstantSQL (showConstantSQL))
import qualified Database.Relational.Query.Expr.Unsafe as UnsafeExpr
import Database.Relational.Query.AliasId (Qualified)
import Database.Relational.Query.Sub (SubQuery, queryWidth)
import qualified Database.Relational.Query.Sub as SubQuery


data ProjectionUnit = Columns (Array Int String)
                    | Sub (Qualified SubQuery)

data Projection t = Composed [ProjectionUnit]

outer :: Projection r -> Projection (Maybe r)
outer =  d  where
  d (Composed qs) = Composed qs

widthOfUnit :: ProjectionUnit -> Int
widthOfUnit =  d  where
  d (Columns a) = mx - mn + 1 where (mn, mx) = Array.bounds a
  d (Sub sq)    = queryWidth sq

columnOfUnit :: ProjectionUnit -> Int -> String
columnOfUnit =  d  where
  d (Columns a) i | mn <= i && i <= mx = a Array.! i
                  | otherwise          = error $ "index out of bounds (unit): " ++ show i
    where (mn, mx) = Array.bounds a
  d (Sub sq) i = SubQuery.column sq i

width :: Projection r -> Int
width =  d  where
  d (Composed prod) = sum . map widthOfUnit $ prod

column :: Projection r -> Int -> String
column =  d  where
  d (Composed us') i' = rec us' i'  where
    rec []       _       = error $ "index out of bounds: " ++ show i'
    rec (u : us) i
      | i < widthOfUnit u = columnOfUnit u i
      | i < 0             = error $ "index out of bounds: " ++ show i
      | otherwise         = rec us (i - widthOfUnit u)

columns :: Projection r -> [String]
columns p = map (\n -> column p n) . take w $ [0 .. ]
  where w = width p


unsafeFromUnit :: ProjectionUnit -> Projection t
unsafeFromUnit =  Composed . (:[])

unsafeFromColumns :: [String] -> Projection t
unsafeFromColumns fs = unsafeFromUnit . Columns $ listArray (0, length fs - 1) fs

compose :: Projection a -> Projection b -> Projection (c a b)
compose (Composed a) (Composed b) = Composed $ a ++ b

fromQualifiedSubQuery :: Qualified SubQuery -> Projection t
fromQualifiedSubQuery =  unsafeFromUnit . Sub

toExpr :: Projection t -> Expr t
toExpr =  UnsafeExpr.Expr . d . columns  where
  d ([])  = error $ "expr: no columns."
  d ([c]) = c
  d (cs)  = '(' : intercalate ", " cs ++ [')']


unsafeProject :: PersistableRecordWidth b -> Projection a' -> Pi a b -> Projection b'
unsafeProject pr p pi' =
  unsafeFromColumns
  . take (runPersistableRecordWidth pr) . drop (Pi.leafIndex pi')
  . columns $ p

pi :: PersistableWidth b => Projection a -> Pi a b -> Projection b
pi =  unsafeProject persistableWidth

piMaybe :: PersistableWidth b => Projection (Maybe a) -> Pi a b -> Projection (Maybe b)
piMaybe =  unsafeProject persistableWidth

class Projectable p where
  project :: Projection a -> p a

instance Projectable Projection where
  project = id

instance Projectable Expr where
  project = toExpr

unsafeSqlProjection :: String -> Projection t
unsafeSqlProjection =  unsafeFromColumns . (:[])


class SqlProjectable p where
  unsafeSqlValue :: String -> p t

instance SqlProjectable Projection where
  unsafeSqlValue = unsafeSqlProjection

instance SqlProjectable Expr where
  unsafeSqlValue = UnsafeExpr.Expr

valueNull :: SqlProjectable p => p (Maybe a)
valueNull =  unsafeSqlValue "NULL"

placeholder :: SqlProjectable p => p t
placeholder =  unsafeSqlValue "?"

value :: (ShowConstantSQL t, SqlProjectable p) => t -> p t
value =  unsafeSqlValue . showConstantSQL

valueTrue :: SqlProjectable p => p Bool
valueTrue =  value True

valueFalse :: SqlProjectable p => p Bool
valueFalse =  value False
