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
  Expr(Expr), unsafeStringSql, showExpr
  ) where

import Database.Relational.Query.Internal.SQL (StringSQL, showStringSQL)

-- | Phantom typed SQL expression object. Project from projection type 'p'.
newtype Expr p a = Expr StringSQL

-- | Get SQL expression from typed object.
unsafeStringSql :: Expr p t -> StringSQL
unsafeStringSql (Expr s) = s

-- | Get SQL string from typed object.
showExpr :: Expr p t -> String
showExpr =  showStringSQL . unsafeStringSql

-- | Show expression.
instance Show (Expr p a) where
  show = showExpr
