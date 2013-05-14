module Database.Relational.Query.Table (
  Untyped, name', width', columns', (!),

  Table, unType, name, shortName, width, columns, index, table, outer,
  ) where

import Data.Array (Array, listArray, elems)
import qualified Data.Array as Array

data Untyped = Untyped
               { name'       :: String
               , width'      :: Int
               , columnArray :: Array Int String
               }

columns' :: Untyped -> [String]
columns' =  elems . columnArray

(!) :: Untyped -> Int -> String
t ! i = columnArray t Array.! i


newtype Table r = Table { unType :: Untyped }

name :: Table r -> String
name   = name'   . unType

shortName :: Table r -> String
shortName =  tail . dropWhile (/= '.') . name

width :: Table r -> Int
width  = width'  . unType

columns :: Table r -> [String]
columns =  columns' . unType

index :: Table r -> Int -> String
index =  (!) . unType

outer :: Table r -> Table (Maybe r)
outer (Table t) = (Table t)

table :: String -> [String] -> Table r
table n f = Table $ Untyped n w fa  where
  w  = length f
  fa = listArray (0, w - 1) f
