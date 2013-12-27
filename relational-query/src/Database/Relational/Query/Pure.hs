
-- |
-- Module      : Database.Relational.Query.Pure
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines interfaces between haskell pure values
-- and query internal projection values.
module Database.Relational.Query.Pure (

  -- * Interface to specify record constructors.
  ProductConstructor (..)
  ) where

-- | Specify tuple like record constructors which are allowed to define 'ProjectableFunctor'.
class ProductConstructor r where
  productConstructor :: r
