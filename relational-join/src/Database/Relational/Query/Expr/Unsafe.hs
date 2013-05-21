
module Database.Relational.Query.Expr.Unsafe (
  Expr(Expr, showExpr),
  ) where

newtype Expr a = Expr { showExpr :: String }

instance Show (Expr a) where
  show = showExpr
