module Database.Relational.Query.Projection (
  Projection, outer,

  width,

  (!), (!?),
  columns,

  compose, fromQualifiedSubQuery, fromExpr
  ) where

import Database.Relational.Query.Pi (Pi)
import qualified Database.Relational.Query.Pi as Pi
import Database.Relational.Query.Expr (Expr, showExpr)
import qualified Database.Relational.Query.Expr.Unsafe as UnsafeExpr
import Database.Relational.Query.AliasId (Qualified)
import Database.Relational.Query.Sub (SubQuery, queryWidth)
import qualified Database.Relational.Query.Sub as SubQuery


data ProjectionUnit = Expr String
                    | Sub (Qualified SubQuery)

data Projection t = Composed [ProjectionUnit]

outer :: Projection r -> Projection (Maybe r)
outer =  d  where
  d (Composed qs) = Composed qs

widthOfUnit :: ProjectionUnit -> Int
widthOfUnit =  d  where
  d (Expr _) = 1
  d (Sub sq) = queryWidth sq

columnOfUnit :: ProjectionUnit -> Int -> String
columnOfUnit =  d  where
  d (Expr s) 0 = s
  d (Expr _) i = error $ "index out of bounds (unit): " ++ show i
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


fromUnit :: ProjectionUnit -> Projection t
fromUnit =  Composed . (:[])

compose :: Projection a -> Projection b -> Projection (c a b)
compose (Composed a) (Composed b) = Composed $ a ++ b

fromQualifiedSubQuery :: Qualified SubQuery -> Projection t
fromQualifiedSubQuery =  fromUnit . Sub

fromExpr :: Expr ft -> Projection ft
fromExpr =  fromUnit . Expr . showExpr

unsafeColumnExpr :: Projection r' -> Pi r ft -> Expr ft'
unsafeColumnExpr p ps = UnsafeExpr.Expr $ column p (Pi.leafIndex ps)

columnExpr :: Projection r -> Pi r ft -> Expr ft
columnExpr p ps = unsafeColumnExpr p ps

columnMaybeExpr :: Projection (Maybe r) -> Pi r ft -> Expr (Maybe ft)
columnMaybeExpr p ps = unsafeColumnExpr p ps

(!) :: Projection r -> Pi r ft -> Expr ft
(!) =  columnExpr

(!?) :: Projection (Maybe r) -> Pi r ft -> Expr (Maybe ft)
(!?) =  columnMaybeExpr
