-- |
-- Module      : Database.Relational.Query.Expr.Unsafe
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines phantom typed SQL expression object.
-- Contains internal structure and unsafe interfaces.
module Database.Relational.Query.Expr.Unsafe (
  -- * Typed SQL Expression
  Expr(Expr), showExpr
  ) where

-- | Phantom typed SQL expression object.
newtype Expr a = Expr (String)

-- | Get SQL expression from typed object.
showExpr :: Expr t -> String
showExpr (Expr s) = s

-- | Show expression.
instance Show (Expr a) where
  show = showExpr
