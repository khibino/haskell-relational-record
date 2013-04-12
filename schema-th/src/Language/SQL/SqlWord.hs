{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Language.SQL.SqlWord
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Language.SQL.SqlWord (
  SqlWord (..),

  word,
  wordShow, unwordsSQL,

  sepBy, parenSepBy, defineBinOp,
  as, (<.>),

  (<=>), (<<>>), and, or,

  stringMap
  ) where

import Prelude hiding (and, or)
import Data.String (IsString(fromString))
import Data.List (find, intersperse)


data SqlWord = SELECT | ALL | DISTINCT | ON
             | GROUP | COUNT | SUM | AVG | MAX | MIN
             | ORDER | BY | ASC | DESC
             | FETCH | FIRST | NEXT | ROW | ROWS | ONLY

             | DELETE | USING | RETURNING

             | FROM | AS | WITH
             | JOIN | LEFT | RIGHT | NATURAL | OUTER

             | UPDATE | SET | DEFAULT

             | WHERE

             | INSERT | INTO | VALUES

             | CASE | END | WHEN | ELSE | THEN

             | LIKE
             -- | (:?)
             -- | (:=) | (:<) | (:<=)| (:>) | (:>=) | (:<>)
             -- | (:+) | (:-) | (:*) | (:/) | (:||)
             | AND | OR | NOT

             | IS | NULL

             -- | OPEN | CLOSE

             | Sequence String
             deriving (Read, Show)


instance IsString SqlWord where
  fromString s' = found (find ((== "") . snd) (reads s')) s'  where
   found  Nothing      s = Sequence s
   found (Just (w, _)) _ = w

word :: String -> SqlWord
word =  Sequence

wordShow :: SqlWord -> String
wordShow =  d  where
  d (Sequence s)   = s
  d w              = show w

sepBy' :: [SqlWord] -> SqlWord -> [String]
ws `sepBy'` d =  map wordShow . intersperse d $ ws

unwordsSQL :: [SqlWord] -> String
unwordsSQL =  unwords . map wordShow

-- unwords' :: [SqlWord] -> SqlWord
-- unwords' =  word . unwordsSQL

concat' :: [String] -> SqlWord
concat' =  word . concat

sepBy :: [SqlWord] -> SqlWord -> SqlWord
ws `sepBy` d = concat' $ ws `sepBy'` d

parenSepBy :: [SqlWord] -> SqlWord -> SqlWord
ws `parenSepBy` d = concat' $ "(" : (ws `sepBy'` d) ++ [")"]

defineBinOp' :: SqlWord -> SqlWord -> SqlWord -> SqlWord
defineBinOp' op a b = concat' $ [a, b] `sepBy'` op

defineBinOp :: SqlWord -> SqlWord -> SqlWord -> SqlWord
defineBinOp op a b = word . unwords $ [a, b] `sepBy'` op

as :: SqlWord -> SqlWord -> SqlWord
as =  defineBinOp AS

(<.>) :: SqlWord -> SqlWord -> SqlWord
(<.>) =  defineBinOp' "."

(<=>) :: SqlWord -> SqlWord -> SqlWord
(<=>) =  defineBinOp "="

(<<>>) :: SqlWord -> SqlWord -> SqlWord
(<<>>) =  defineBinOp "<>"

and :: SqlWord -> SqlWord -> SqlWord
and =  defineBinOp AND

or :: SqlWord -> SqlWord -> SqlWord
or =  defineBinOp OR

infixl 4 `and`
infixl 3 `or`
infixl 2 <=>, <<>>


stringMap :: (String -> String) -> SqlWord -> SqlWord
stringMap f = word . f . wordShow
