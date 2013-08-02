-- |
-- Module      : Database.Relational.Query.Internal.Context
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides context definition for
-- "Database.Relational.Query.Monad.Trans.Join" and
-- "Database.Relational.Query.Monad.Trans.Ordering".
module Database.Relational.Query.Internal.RestrictContext (
  -- * Context of restriction
  RestrictContext,

  primeRestrictContext,

  addRestriction,

  composeWheres
  ) where

import Database.Relational.Query.Expr (Expr, fromTriBool, exprAnd)
import Database.Relational.Query.Expr.Unsafe (showExpr)

import Database.Relational.Query.Projection (Projection)

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL


-- | Context type for Restrict.
data RestrictContext =
  RestrictContext
  { restriction :: Maybe (Expr Projection Bool) }

-- | Initial 'RestrictContext'.
primeRestrictContext :: RestrictContext
primeRestrictContext =  RestrictContext Nothing

-- | Add restriction of 'RestrictContext'.
addRestriction :: Expr Projection (Maybe Bool) -> RestrictContext -> RestrictContext
addRestriction e1 ctx =
  ctx { restriction = Just . uf . restriction $ ctx }
  where uf  Nothing  = fromTriBool e1
        uf (Just e0) = e0 `exprAnd` fromTriBool e1

-- | Compose SQL String from QueryJoin monad object.
composeWheres' :: Maybe (Expr Projection Bool) -> String
composeWheres' =  maybe [] (\e -> unwordsSQL [WHERE, SQL.word . showExpr $ e])

-- | Compose SQL String from QueryJoin monad object.
composeWheres :: RestrictContext -> String
composeWheres = composeWheres' . restriction
