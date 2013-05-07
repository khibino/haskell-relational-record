{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Query.Expr (
  Expr(showExpr),

  ShowConstantSQL (showConstantSQL),

  valueExpr,

  just, unsafeFromJust,

  (.=.), (.<>.), (.>.), (.<.), and, or
  ) where

import Prelude hiding (and, or)
import Data.Int (Int16, Int32, Int64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T

import qualified Language.SQL.Keyword as SQL

import Database.Relational.Query.Expr.Unsafe (Expr(Expr, showExpr), compareBinOp)


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

unsafeFromJust :: Expr (Maybe ft) -> Expr ft
unsafeFromJust =  Expr . showExpr


(.=.) :: Expr ft -> Expr ft -> Expr Bool
(.=.) =  compareBinOp (SQL..=.)

(.<>.) :: Expr ft -> Expr ft -> Expr Bool
(.<>.) =  compareBinOp (SQL..<>.)

(.>.) :: Expr ft -> Expr ft -> Expr Bool
(.>.) =  compareBinOp (SQL.defineBinOp (SQL.word ">"))

(.<.) :: Expr ft -> Expr ft -> Expr Bool
(.<.) =  compareBinOp (SQL.defineBinOp (SQL.word "<"))

and :: Expr Bool ->  Expr Bool ->  Expr Bool
and =  compareBinOp SQL.and

or :: Expr Bool ->  Expr Bool ->  Expr Bool
or =  compareBinOp SQL.or

infixr 4 .=., .<>.
infixr 3 `and`
infixr 2 `or`
