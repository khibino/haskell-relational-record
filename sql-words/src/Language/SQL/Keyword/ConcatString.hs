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
  sepBy, defineBinOp,

  as,

  (.||.),
  (.=.), (.<.), (.<=.), (.>.), (.>=.), (.<>.),
  and, or, in'

  ) where

import Prelude hiding (and, or)
import Data.List (intersperse)

import Language.SQL.Keyword.Type (Keyword (..), wordShow)


sepBy :: [String] -> Keyword -> [String]
ws `sepBy` d = intersperse (wordShow d) $ ws

defineBinOp :: Keyword -> String -> String -> String
defineBinOp op a b = unwords $ [a, b] `sepBy` op


(.||.) =  defineBinOp "||"
(.=.)  =  defineBinOp "="
(.<.)  =  defineBinOp "<"
(.<=.) =  defineBinOp "<="
(.>.)  =  defineBinOp ">"
(.>=.) =  defineBinOp ">="
(.<>.) =  defineBinOp "<>"
as     =  defineBinOp AS
and    =  defineBinOp AND
or     =  defineBinOp OR
in'    =  defineBinOp IN

(.||.), (.=.), (.<.), (.<=.), (.>.), (.>=.), (.<>.), as, and, or, in'
  :: String -> String -> String

infixr 5 .||.
infixr 4 .=., .<., .<=., .>., .>=., .<>.
infix  4 `in'`
infixr 3 `and`
infixr 2 `or`
