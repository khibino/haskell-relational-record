-- |
-- Module      : Language.SQL.Keyword.Internal.Type
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines package internal types.
module Language.SQL.Keyword.Internal.Type (
  -- * SQL keyword type interface.
  Keyword (..), word, wordShow,

  -- * Low-level diff string interface.
  fromDString, toDString,
  DString, dString, showDString, isEmptyDString
  ) where

import Data.String (IsString(..))
import Data.List (find)
import Data.Monoid (Monoid (..), (<>))


-- | Diff String type for low-cost concatination.
newtype DString = DString (String -> String)

-- | Make 'DString' from 'String'
dString :: String -> DString
dString =  DString . (++)

-- | Show 'DString' into 'String'
showDString :: DString -> String
showDString (DString f) = f []

-- | 'DString' is empty or not.
isEmptyDString :: DString -> Bool
isEmptyDString = null . showDString

instance Eq DString where
  x == y = showDString x == showDString y

instance Show DString where
  show = showDString

instance Read DString where
  readsPrec _ s = [(dString s, [])]

instance Monoid DString where
  mempty  = DString id
  DString f `mappend` DString g = DString $ f . g

dspace :: DString
dspace =  dString " "

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

             | DATE | TIME | TIMESTAMP | TIMESTAMPTZ | INTERVAL

             | Sequence !DString
             deriving (Read, Show)

             {-
                  | (:?)
                  | (:+) | (:-) | (:*) | (:/)
                  | OPEN | CLOSE
             -}


-- | Wrap 'DString' into 'Keyword'
fromDString :: DString -> Keyword
fromDString =  Sequence

-- | Unwrap 'Keyword' into 'DString'
toDString :: Keyword -> DString
toDString = d  where
  d (Sequence ds) = ds
  d  w            = dString $ show w

-- | Make 'Keyword' from String
word :: String -> Keyword
word =  fromDString . dString

-- | 'Keyword' type with OverloadedString extension,
--   can be involved same list with string literals.
--
-- > selectFoo = [SELECT, "a, b, c", FROM, "foo"]
--
instance IsString Keyword where
  fromString s' = found (find ((== "") . snd) (reads s')) s'  where
   found  Nothing      s = word s
   found (Just (w, _)) _ = w

-- | 'Keyword' default concatination separate by space.
instance Monoid Keyword where
  mempty  = fromDString mempty
  a `mappend` b = fromDString $ toDString a `append'` toDString b  where
    append' p q
      | isEmptyDString p = q
      | isEmptyDString q = p
      | otherwise        = p <> dspace <> q


-- | Show 'Keyword'
wordShow :: Keyword -> String
wordShow =  d  where
  d (Sequence s)   = showDString s
  d w              = show w

instance Eq Keyword where
  x == y = wordShow x == wordShow y
