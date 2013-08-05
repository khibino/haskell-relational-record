-- |
-- Module      : Database.Relational.Query.Monad.Trans.StateAppend
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides type tagged string append function types.
module Database.Relational.Query.Monad.Trans.StateAppend (
  Append (append),
  liftToString
  ) where

-- | Type tagged string append function.
newtype Append c = Append { append :: String -> String }

-- | Lift from to-string into to-append function.
liftToString :: (c -> String) -> c -> Append c
liftToString =  fmap $ \s -> Append (++ d s)  where
  d "" = ""
  d s  = ' ' : s
