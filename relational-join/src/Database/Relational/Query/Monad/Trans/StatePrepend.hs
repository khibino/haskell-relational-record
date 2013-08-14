-- |
-- Module      : Database.Relational.Query.Monad.Trans.StatePrepend
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides functions to generate strings
-- from individual transformers' state and string prepend seed.
module Database.Relational.Query.Monad.Trans.StatePrepend (
  -- * Type tagged string prepend function types
  Prepend (prepend),
  liftToString
  ) where

-- | Type tagged string prepend function.
newtype Prepend c = Prepend { prepend :: ShowS }

-- | Lift from to-string into to-prepend function.
liftToString :: (c -> String) -> c -> Prepend c
liftToString =  fmap $ \s -> Prepend (d s ++)  where
  d "" = ""
  d s  = ' ' : s
