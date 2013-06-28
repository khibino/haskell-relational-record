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


-- | Type represent SQL keywords.
data Keyword = SELECT | ALL | DISTINCT | ON
             | GROUP | COUNT | SUM | AVG | MAX | MIN | EVERY | ANY | SOME
             | ORDER | BY | ASC | DESC | HAVING
             | FETCH | FIRST | NEXT | ROW | ROWS | ONLY

             | UNION | EXCEPT | INTERSECT

             | DELETE | USING | RETURNING

             | FROM | AS | WITH
             | JOIN | INNER | LEFT | RIGHT | FULL | NATURAL | OUTER

             | UPDATE | SET | DEFAULT

             | WHERE

             | INSERT | INTO | VALUES

             | CASE | END | WHEN | ELSE | THEN

             | LIKE
             | AND | OR | NOT

             | IS | NULL | IN

             | Sequence String
             deriving (Read, Show)

             {-
                  | (:?)
                  | (:+) | (:-) | (:*) | (:/)
                  | OPEN | CLOSE
             -}


-- | 'Keyword' type with OverloadedString extension,
--   can be involved same list with string literals.
--
-- > selectFoo = [SELECT, "a, b, c", FROM, "foo"]
--
instance IsString Keyword where
  fromString s' = found (find ((== "") . snd) (reads s')) s'  where
   found  Nothing      s = Sequence s
   found (Just (w, _)) _ = w

-- | Make 'Keyword' from String
word :: String -> Keyword
word =  Sequence

-- | Show 'Keyword'
wordShow :: Keyword -> String
wordShow =  d  where
  d (Sequence s)   = s
  d w              = show w

-- | Concatinate keywords into 'String' like unwords
unwordsSQL :: [Keyword] -> String
unwordsSQL =  unwords . map wordShow

-- | Make SQL string expression. No escape logic, so this is unsafe function.
unsafeString :: String -> Keyword
unsafeString =  Sequence . ('\'' :) . (++ "'")

-- | Make SQL integer expression
integer :: (Integral a, Show a) => a -> Keyword
integer =  Sequence . show

-- | Map 'String' function into 'Keyword' function
stringMap :: (String -> String) -> Keyword -> Keyword
stringMap f = word . f . wordShow
