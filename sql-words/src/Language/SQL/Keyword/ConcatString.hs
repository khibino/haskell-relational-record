{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Language.SQL.Keyword.ConcatString
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
--  'String' type version of "Language.SQL.Keyword.Concat"
module Language.SQL.Keyword.ConcatString (
  -- * List concatination functions
  sepBy,

  -- * Binary operators
  -- $binaryOperators
  defineBinOp,

  as,

  (.||.),
  (.=.), (.<.), (.<=.), (.>.), (.>=.), (.<>.),
  and, or, not, in'

  ) where

import Prelude hiding (and, or, not)
import Data.List (intersperse)

import Language.SQL.Keyword.Type (Keyword (..), wordShow)


-- | Separate 'String' list with delimiter 'Keyword' and map to 'String' list.
sepBy :: [String] -> Keyword -> [String]
ws `sepBy` d = intersperse (wordShow d) $ ws

{- $binaryOperators
Binary operators on SQL. Result is concatinated into one 'String'.
-}

-- | Define binary operator on 'String' type represeted by specified 'Keyword'.
--   Result is delimited by whitespace like unwords on 'String' list.
defineBinOp :: Keyword -> String -> String -> String
defineBinOp op a b = unwords $ [a, b] `sepBy` op

-- | Binary operator for SQL string expression concatination.
(.||.) :: String -> String -> String
(.||.) =  defineBinOp "||"

-- | Binary eq operator for SQL expression.
(.=.) :: String -> String -> String
(.=.)  =  defineBinOp "="

-- | Binary not eq operator for SQL expression.
(.<>.) :: String -> String -> String
(.<>.) =  defineBinOp "<>"

-- | Binary lt operator for SQL expression.
(.<.) :: String -> String -> String
(.<.)  =  defineBinOp "<"

-- | Binary le operator for SQL expression.
(.<=.) :: String -> String -> String
(.<=.) =  defineBinOp "<="

-- | Binary gt operator for SQL expression.
(.>.) :: String -> String -> String
(.>.)  =  defineBinOp ">"

-- | Binary ge operator for SQL expression.
(.>=.) :: String -> String -> String
(.>=.) =  defineBinOp ">="

-- | Binary operator for SQL name alias.
as :: String -> String -> String
as     =  defineBinOp AS

-- | Binary `AND` operator for SQL boolean expression.
and :: String -> String -> String
and    =  defineBinOp AND

-- | Binary `OR` operator for SQL boolean expression.
or :: String -> String -> String
or     =  defineBinOp OR

-- | Uni `NOT` operator for SQL boolean expression.
not :: String -> String
not e = unwords [wordShow NOT, e]

-- | Binary `IN` operator for SQL.
in' :: String -> String -> String
in'    =  defineBinOp IN

infixr 5 .||.
infixr 4 .=., .<., .<=., .>., .>=., .<>.
infix  4 `in'`
infixr 3 `and`
infixr 2 `or`
