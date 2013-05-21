{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Query.Expr (
  Expr(showExpr),

  ShowConstantSQL (showConstantSQL),

  valueExpr,

  just, flattenMaybe
  ) where

import Prelude hiding (and, or)
import Data.Int (Int16, Int32, Int64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T

import Database.Relational.Query.Expr.Unsafe (Expr(Expr, showExpr))


intExprSQL :: (Show a, Integral a) => a -> String
intExprSQL =  show

escapeStringToSqlExpr :: String -> String
escapeStringToSqlExpr =  rec  where
  rec ""        = ""
  rec ('\'':cs) = '\'' : '\'' : rec cs
  rec (c:cs)    = c : rec cs

stringExprSQL :: String -> String
stringExprSQL =  ('\'':) . (++ "'") . escapeStringToSqlExpr

class ShowConstantSQL a where
  showConstantSQL :: a -> String

instance ShowConstantSQL Int16 where
  showConstantSQL = intExprSQL

instance ShowConstantSQL Int32 where
  showConstantSQL = intExprSQL

instance ShowConstantSQL Int64 where
  showConstantSQL = intExprSQL

instance ShowConstantSQL String where
  showConstantSQL = stringExprSQL

instance ShowConstantSQL ByteString where
  showConstantSQL = stringExprSQL . BS.unpack

instance ShowConstantSQL Text where
  showConstantSQL = stringExprSQL . T.unpack

instance ShowConstantSQL Char where
  showConstantSQL = stringExprSQL . (:"")

instance ShowConstantSQL Bool where
  showConstantSQL = d  where
    d True  = "(0=0)"
    d False = "(0=1)"

instance ShowConstantSQL a => ShowConstantSQL (Maybe a) where
  showConstantSQL = d  where
    d (Just a)  = showConstantSQL a
    d (Nothing) = "NULL"


valueExpr :: ShowConstantSQL ft => ft -> Expr ft
valueExpr =  Expr . showConstantSQL

just :: Expr ft -> Expr (Maybe ft)
just =  Expr . showExpr

flattenMaybe :: Expr (Maybe (Maybe ft)) -> Expr (Maybe ft)
flattenMaybe =  Expr . showExpr
