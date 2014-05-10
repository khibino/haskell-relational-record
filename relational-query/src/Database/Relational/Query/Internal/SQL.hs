-- |
-- Module      : Database.Relational.Query.Internal.SQL
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides SQL string wrap interfaces.
module Database.Relational.Query.Internal.SQL (
  StringSQL, stringSQL, showStringSQL
  ) where

-- | String wrap type for SQL strings.
type StringSQL = ShowS

-- | 'StringSQL' from 'String'.
stringSQL :: String -> StringSQL
stringSQL =  (++)

-- | 'StringSQL' to 'String'.
showStringSQL :: StringSQL -> String
showStringSQL =  ($ [])
