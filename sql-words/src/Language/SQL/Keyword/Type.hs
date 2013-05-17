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

  string, integer
  ) where

import Prelude hiding (and, or)
import Data.String (IsString(fromString))
import Data.List (find)


data Keyword = SELECT | ALL | DISTINCT | ON
             | GROUP | COUNT | SUM | AVG | MAX | MIN
             | ORDER | BY | ASC | DESC
             | FETCH | FIRST | NEXT | ROW | ROWS | ONLY

             | DELETE | USING | RETURNING

             | FROM | AS | WITH
             | JOIN | INNER | LEFT | RIGHT | FULL | NATURAL | OUTER

             | UPDATE | SET | DEFAULT

             | WHERE

             | INSERT | INTO | VALUES

             | CASE | END | WHEN | ELSE | THEN

             | LIKE
             -- | (:?)
             -- | (:+) | (:-) | (:*) | (:/)
             | AND | OR | NOT

             | IS | NULL | IN

             -- | OPEN | CLOSE

             | Sequence String
             deriving (Read, Show)


instance IsString Keyword where
  fromString s' = found (find ((== "") . snd) (reads s')) s'  where
   found  Nothing      s = Sequence s
   found (Just (w, _)) _ = w

word :: String -> Keyword
word =  Sequence

wordShow :: Keyword -> String
wordShow =  d  where
  d (Sequence s)   = s
  d w              = show w

unwordsSQL :: [Keyword] -> String
unwordsSQL =  unwords . map wordShow

string :: String -> Keyword
string =  Sequence . ('\'' :) . (++ "'")

integer :: (Integral a, Show a) => a -> Keyword
integer =  Sequence . show
