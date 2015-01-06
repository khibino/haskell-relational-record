{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Query.Expr
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines phantom typed SQL expression object.
-- Contains normal interfaces.
module Database.Relational.Query.Expr (
  -- * Typed SQL Expression
  Expr,

  valueExpr,

  -- * Type conversion
  just, fromJust,

  exprAnd
  ) where

import Prelude hiding (and, or)

import Database.Relational.Query.Expr.Unsafe (Expr(Expr), sqlExpr)
import Database.Relational.Query.Pure (ShowConstantTermsSQL, showConstantTermsSQL')
import Database.Relational.Query.Internal.SQL (rowStringSQL)

import qualified Language.SQL.Keyword as SQL


-- | Typed constant SQL expression from Haskell value.
valueExpr :: ShowConstantTermsSQL ft => ft -> Expr p ft
valueExpr =  Expr . rowStringSQL . showConstantTermsSQL'

-- | Unsafely cast phantom type.
unsafeCastExpr :: Expr p a -> Expr p b
unsafeCastExpr =  Expr . sqlExpr

-- | Convert phantom type into 'Maybe'.
just :: Expr p ft -> Expr p (Maybe ft)
just =  unsafeCastExpr

-- | Allowed only for having or where 'Expr'.
--   So NULL expression result will be possible.
--   Behavior around boolean is strongly dependent on RDBMS impelemetations.
fromJust :: Expr p (Maybe ft) -> Expr p ft
fromJust =  unsafeCastExpr

-- | AND operator for 'Expr'.
exprAnd :: Expr p Bool -> Expr p Bool -> Expr p Bool
exprAnd a b = Expr . SQL.paren $ SQL.and (sqlExpr a) (sqlExpr b)
