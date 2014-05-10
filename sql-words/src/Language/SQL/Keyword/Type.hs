-- |
-- Module      : Language.SQL.Keyword.Type
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- SQL keyword representation using Haskell data constructors.
module Language.SQL.Keyword.Type (
  Keyword (..),

  word,
  wordShow, unwordsSQL,

  unsafeString, integer,

  stringMap
  ) where

import Data.String (IsString(fromString))
import Data.List (find)


newtype DString = DString (String -> String)

dString :: String -> DString
dString =  DString . (++)

showDString :: DString -> String
showDString (DString f) = f []

instance Show DString where
  show = showDString

instance Read DString where
  readsPrec _ s = [(dString s, [])]

-- | Type represent SQL keywords.
data Keyword = SELECT | ALL | DISTINCT | ON
             | GROUP | COUNT | SUM | AVG | MAX | MIN | EVERY | ANY | SOME
             | ORDER | BY | ASC | DESC
             | CUBE | ROLLUP | GROUPING | SETS | HAVING
             | FOR

             | LIMIT
             | FETCH | FIRST | NEXT | ROW | ROWS | ONLY

             | UNION | EXCEPT | INTERSECT

             | DELETE | USING | RETURNING

             | FROM | AS | WITH
             | JOIN | INNER | LEFT | RIGHT | FULL | NATURAL | OUTER

             | UPDATE | SET | DEFAULT

             | WHERE

             | INSERT | INTO | VALUES

             | MERGE

             | OVER | PARTITION
             | DENSE_RANK | RANK | ROW_NUMBER
             | PERCENT_RANK | CUME_DIST
             | LAG | LEAD | FIRST_VALUE | LAST_VALUE

             | CASE | END | WHEN | ELSE | THEN

             | LIKE | SIMILAR
             | AND | OR | NOT
             | EXISTS

             | IS | NULL | IN

             | DATE | TIME | TIMESTAMP | INTERVAL

             | Sequence DString
             deriving (Read, Show)

             {-
                  | (:?)
                  | (:+) | (:-) | (:*) | (:/)
                  | OPEN | CLOSE
             -}


-- | Make 'Keyword' from String
word :: String -> Keyword
word =  Sequence . dString

-- | 'Keyword' type with OverloadedString extension,
--   can be involved same list with string literals.
--
-- > selectFoo = [SELECT, "a, b, c", FROM, "foo"]
--
instance IsString Keyword where
  fromString s' = found (find ((== "") . snd) (reads s')) s'  where
   found  Nothing      s = word s
   found (Just (w, _)) _ = w

-- | Show 'Keyword'
wordShow :: Keyword -> String
wordShow =  d  where
  d (Sequence s)   = showDString s
  d w              = show w

-- | Concatinate keywords into 'String' like unwords
unwordsSQL :: [Keyword] -> String
unwordsSQL =  unwords . map wordShow

-- | Make SQL string expression. No escape logic, so this is unsafe function.
unsafeString :: String -> Keyword
unsafeString =  word . ('\'' :) . (++ "'")

-- | Make SQL integer expression
integer :: (Integral a, Show a) => a -> Keyword
integer =  word . show

-- | Map 'String' function into 'Keyword' function
stringMap :: (String -> String) -> Keyword -> Keyword
stringMap f = word . f . wordShow
