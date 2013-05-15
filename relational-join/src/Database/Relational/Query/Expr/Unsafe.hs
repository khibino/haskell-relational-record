
module Database.Relational.Query.Expr.Unsafe (
  Expr(Expr, showExpr),

  compareBinOp, numBinOp, binOp
  ) where

import qualified Language.SQL.Keyword as SQL

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

compareBinOp :: (SQL.Keyword -> SQL.Keyword -> SQL.Keyword)
             -> Expr ft -> Expr ft -> Expr Bool
compareBinOp =  binOp

numBinOp :: Num a
         => (SQL.Keyword -> SQL.Keyword -> SQL.Keyword)
         -> Expr a -> Expr a -> Expr a
numBinOp =  binOp
