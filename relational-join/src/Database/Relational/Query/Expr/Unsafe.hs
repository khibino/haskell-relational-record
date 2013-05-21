
module Database.Relational.Query.Expr.Unsafe (
  Expr(Expr), showExpr
  ) where

newtype Expr a = Expr (String)

showExpr :: Expr t -> String
showExpr (Expr s) = s

instance Show (Expr a) where
  show = showExpr
