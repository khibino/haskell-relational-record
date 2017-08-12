-- |
-- Module      : Database.Relational.Compat
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains backword compatibility definitions of
-- relational-query <= 0.9.*
module Database.Relational.Compat (
  Projection,
  ) where

import Database.Relational

{-# DEPRECATED Projection "Replaced by Record type" #-}
-- | old 'Projection' type replaced by 'Record' type.
type Projection = Record
