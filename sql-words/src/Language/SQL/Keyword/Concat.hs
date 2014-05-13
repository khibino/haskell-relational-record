{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Language.SQL.Keyword.Concat
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Concatinations on 'Keyword' types
module Language.SQL.Keyword.Concat (
  -- * List concatination functions
  -- $listConcatination
  unwords',

  sepBy, parenSepBy,

  -- * Binary operators
  -- $binaryOperators
  defineBinOp,

  strBinOp,

  as, (<.>), (|*|),

  (.||.),
  (.=.), (.<.), (.<=.), (.>.), (.>=.), (.<>.),
  and, or, in',

  fold,

  -- * Unary operator
  defineUniOp, paren,
  strUniOp
  ) where

import Prelude hiding (and, or, not)
import Data.List (intersperse)
import Data.Monoid (mempty, mconcat)

import Language.SQL.Keyword.Internal.Type (Keyword (..), word, wordShow, toDString, fromDString)


{- $listConcatination
Functions to concatinate 'Keyword' list.
-}

-- | Separate 'Keyword' list with delimiter 'Keyword' and map to 'String' list.
sepBy' :: [Keyword] -> Keyword -> [String]
ws `sepBy'` d =  map wordShow . intersperse d $ ws

-- | Concatinate 'Keyword' list like unwords on 'String' list.
unwords' :: [Keyword] -> Keyword
unwords' =  mconcat

-- | Concatinate 'String' list into one 'Keyword'.
concatStr :: [String] -> Keyword
concatStr =  word . concat

-- | Separate 'Keyword' list with delimiter 'Keyword' and concatinate into one 'Keyword'.
sepBy :: [Keyword] -> Keyword -> Keyword
ws `sepBy` d = concatStr $ ws `sepBy'` d

-- | Do 'sepBy' and enclose by paren
parenSepBy :: [Keyword] -> Keyword -> Keyword
ws `parenSepBy` d = concatStr $ "(" : (ws `sepBy'` d) ++ [")"]

{- $binaryOperators
Binary operators on SQL. Result is concatinated into one 'Keyword'.
-}

concat' :: [Keyword] -> Keyword
concat' =  fromDString . mconcat . map toDString

-- | Define binary operator on 'Keyword' type.
--   Result is not delimited by whitespace like concat on 'String' list.
defineBinOp' :: Keyword -> Keyword -> Keyword -> Keyword
defineBinOp' op a b = concat' [a, op, b]

-- | Define binary operator on 'Keyword' type.
--   Result is delimited by whitespace like unwords on 'String' list.
defineBinOp :: Keyword -> Keyword -> Keyword -> Keyword
defineBinOp op a b = mconcat [a, op, b]

-- | Binary operator to create qualified name on SQL.
(<.>) :: Keyword -> Keyword -> Keyword
(<.>)  =  defineBinOp' "."

-- | Binary operator to create comma separated words.
(|*|) :: Keyword -> Keyword -> Keyword
(|*|)  =  defineBinOp' ", "

-- | Binary operator for SQL string expression concatination.
(.||.) :: Keyword -> Keyword -> Keyword
(.||.) =  defineBinOp "||"

-- | Binary eq operator for SQL expression.
(.=.) :: Keyword -> Keyword -> Keyword
(.=.)  =  defineBinOp "="

-- | Binary not eq operator for SQL expression.
(.<>.) :: Keyword -> Keyword -> Keyword
(.<>.) =  defineBinOp "<>"

-- | Binary lt operator for SQL expression.
(.<.) :: Keyword -> Keyword -> Keyword
(.<.)  =  defineBinOp "<"

-- | Binary le operator for SQL expression.
(.<=.) :: Keyword -> Keyword -> Keyword
(.<=.) =  defineBinOp "<="

-- | Binary gt operator for SQL expression.
(.>.) :: Keyword -> Keyword -> Keyword
(.>.)  =  defineBinOp ">"

-- | Binary ge operator for SQL expression.
(.>=.) :: Keyword -> Keyword -> Keyword
(.>=.) =  defineBinOp ">="

-- | Binary operator for SQL name alias.
as :: Keyword -> Keyword -> Keyword
as     =  defineBinOp AS

-- | Binary `AND` operator for SQL boolean expression.
and :: Keyword -> Keyword -> Keyword
and    =  defineBinOp AND

-- | Binary `OR` operator for SQL boolean expression.
or :: Keyword -> Keyword -> Keyword
or     =  defineBinOp OR

-- | Fold operation using binary operator with empty result of zero length case.
fold :: (Keyword -> Keyword -> Keyword) -- ^ Binary operator used in fold
     -> [Keyword]                       -- ^ List to fold
     -> Keyword                         -- ^ Result
fold op =  d  where
  d []       = mempty
  d xs@(_:_) = foldr1 op xs

-- | Define unary operator on 'Keyword' type represeted by specified 'Keyword'.
--   Result is delimited by whitespace like unwords on 'String' list.
defineUniOp :: Keyword -> Keyword -> Keyword
defineUniOp op e = mconcat [op, e]

-- | Uni operator to create Parend words.
paren :: Keyword -> Keyword
paren w = concat' ["(", w, ")"]

-- | Binary `IN` operator for SQL.
in' :: Keyword -> Keyword -> Keyword
in'    =  defineBinOp IN

infixr 5 .||.
infixr 4 .=., .<., .<=., .>., .>=., .<>.
infix  4 `in'`
infixr 3 `and`
infixr 2 `or`
infixr 1 |*|

strUniOp :: (Keyword -> Keyword) -> String -> String
strUniOp u = wordShow . u . word

strBinOp :: (Keyword -> Keyword -> Keyword) -> String -> String -> String
strBinOp op a b = wordShow $ op (word a) (word b)
