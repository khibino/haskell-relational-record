
module Database.Relational.Query.Expr.Unsafe (
  Expr(Expr, showExpr),

  UExpr, compareBinOp, binOp
  ) where

import qualified Language.SQL.Keyword as SQL

import Database.Record.Persistable (Singleton)

newtype Expr a = Expr { showExpr :: String }

instance Show (Expr a) where
  show = showExpr

paren :: String -> String
paren = ('(' :) . (++[')'])

binOp :: (SQL.Keyword -> SQL.Keyword -> SQL.Keyword)
      -> Expr ft -> Expr ft -> Expr a
binOp op a b = Expr . paren . SQL.wordShow
               $ op (wordExpr a) (wordExpr b)
  where wordExpr = SQL.word . showExpr

type UExpr a = Expr (Singleton a)

compareBinOp :: (SQL.Keyword -> SQL.Keyword -> SQL.Keyword)
             -> Expr ft -> Expr ft -> UExpr Bool
compareBinOp =  binOp
