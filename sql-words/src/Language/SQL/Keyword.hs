-- |
-- Module      : Language.SQL.Keyword
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- SQL keyword representation using Haskell data constructors.
-- Integrated module.
module Language.SQL.Keyword (
  -- * Module which includes keyword type definition
  module Language.SQL.Keyword.Type,
  -- * Module which includes functions to concatinate keywords
  module Language.SQL.Keyword.Concat
  ) where

import Language.SQL.Keyword.Type
import Language.SQL.Keyword.Concat
