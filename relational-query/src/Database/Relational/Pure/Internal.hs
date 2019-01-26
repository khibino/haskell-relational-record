module Database.Relational.Pure.Internal
    ( intTermsSQL
    , escapeStringToSqlExpr
    , stringExprSQL
    , stringTermsSQL
    , constantTimeTerms
    ) where

import Control.Applicative (pure)
import Data.DList (DList)
import Data.Monoid ((<>))
import Data.Time (FormatTime, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Database.Relational.Internal.String (StringSQL, stringSQL)
import Language.SQL.Keyword (Keyword (..))

-- | Constant integral SQL terms.
intTermsSQL :: (Show a, Integral a) => a -> DList StringSQL
intTermsSQL = pure . stringSQL . show

-- | Escape 'String' for constant SQL string expression.
escapeStringToSqlExpr :: String -> String
escapeStringToSqlExpr = rec where
  rec ""        = ""
  rec ('\'':cs) = '\'' : '\'' : rec cs
  rec (c:cs)    = c : rec cs

-- | From 'String' into constant SQL string expression.
stringExprSQL :: String -> StringSQL
stringExprSQL = stringSQL . ('\'' :) . (++ "'") . escapeStringToSqlExpr

stringTermsSQL :: String -> DList StringSQL
stringTermsSQL = pure . stringExprSQL

constantTimeTerms :: FormatTime t => Keyword -> String -> t -> DList StringSQL
constantTimeTerms kw fmt t =
    pure $ kw <> stringExprSQL (formatTime defaultTimeLocale fmt t)
