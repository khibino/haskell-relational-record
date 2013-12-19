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

  -- * Constant SQL Expression
  ShowConstantSQL (showConstantSQL),

  valueExpr,

  -- * Type conversion
  just, fromJust,

  exprAnd
  ) where

import Prelude hiding (and, or)
import Data.Int (Int16, Int32, Int64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T

import Database.Relational.Query.Expr.Unsafe (Expr(Expr), showExpr)

import Database.Relational.Query.Internal.String (paren)
import qualified Language.SQL.Keyword as SQL
import qualified Language.SQL.Keyword.ConcatString as SQLs


-- | Constant integral SQL expression.
intExprSQL :: (Show a, Integral a) => a -> String
intExprSQL =  show

-- | Escape 'String' for constant SQL string expression.
escapeStringToSqlExpr :: String -> String
escapeStringToSqlExpr =  rec  where
  rec ""        = ""
  rec ('\'':cs) = '\'' : '\'' : rec cs
  rec (c:cs)    = c : rec cs

-- | From 'String' into constant SQL string expression.
stringExprSQL :: String -> String
stringExprSQL =  ('\'':) . (++ "'") . escapeStringToSqlExpr

-- | Interface for constant SQL expression.
class ShowConstantSQL a where
  -- | Make constant SQL expression 'String' from Haskell type 'a'.
  showConstantSQL :: a -> String

-- | Constant SQL expression of 'Int16'.
instance ShowConstantSQL Int16 where
  showConstantSQL = intExprSQL

-- | Constant SQL expression of 'Int32'.
instance ShowConstantSQL Int32 where
  showConstantSQL = intExprSQL

-- | Constant SQL expression of 'Int64'.
instance ShowConstantSQL Int64 where
  showConstantSQL = intExprSQL

-- | Constant SQL expression of 'String'.
instance ShowConstantSQL String where
  showConstantSQL = stringExprSQL

-- | Constant SQL expression of 'ByteString'.
instance ShowConstantSQL ByteString where
  showConstantSQL = stringExprSQL . BS.unpack

-- | Constant SQL expression of 'Text'.
instance ShowConstantSQL Text where
  showConstantSQL = stringExprSQL . T.unpack

-- | Constant SQL expression of 'Char'.
instance ShowConstantSQL Char where
  showConstantSQL = stringExprSQL . (:"")

-- | Constant SQL expression of 'Bool'.
instance ShowConstantSQL Bool where
  showConstantSQL = d  where
    d True  = "(0=0)"
    d False = "(0=1)"

-- | Inference rule for Constant SQL expression of 'Maybe' type.
instance ShowConstantSQL a => ShowConstantSQL (Maybe a) where
  showConstantSQL = d  where
    d (Just a)  = showConstantSQL a
    d (Nothing) = "NULL"


-- | Typed constant SQL expression from Haskell value.
valueExpr :: ShowConstantSQL ft => ft -> Expr p ft
valueExpr =  Expr . showConstantSQL

-- | Unsafely cast phantom type.
unsafeCastExpr :: Expr p a -> Expr p b
unsafeCastExpr =  Expr . showExpr

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
exprAnd a b = Expr . paren $ SQLs.defineBinOp SQL.AND (showExpr a) (showExpr b)
