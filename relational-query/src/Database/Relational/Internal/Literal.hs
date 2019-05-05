-- |
-- Module      : Database.Relational.Internal.Literal
-- Copyright   : 2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides definitions to lift haskell expressions
-- into SQL expressions.
module Database.Relational.Internal.Literal (
  stringExpr,
  bool, integral, timestamp,
  ) where

import Data.Monoid ((<>))
import Data.Time (FormatTime, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)

import Language.SQL.Keyword (Keyword)

import Database.Relational.Internal.String (StringSQL, stringSQL)


-- | Escape 'String' for constant SQL string expression.
escapeStringToSqlExpr :: String -> String
escapeStringToSqlExpr = rec where
  rec ""        = ""
  rec ('\'':cs) = '\'' : '\'' : rec cs
  rec (c:cs)    = c : rec cs

-- | From 'String' into constant SQL string expression.
stringExpr :: String -> StringSQL
stringExpr = stringSQL . ('\'' :) . (++ "'") . escapeStringToSqlExpr

-- | SQL expressions for Bool type.
bool :: Bool -> StringSQL
bool =
    stringSQL . d
  where
    d True  = "(0=0)"
    d False = "(0=1)"

-- | Constant integral SQL term.
integral :: (Show a, Integral a) => a -> StringSQL
integral = stringSQL . show

timestamp :: FormatTime t => Keyword -> String -> t -> StringSQL
timestamp kw fmt t = kw <> stringExpr (formatTime defaultTimeLocale fmt t)
