-- |
-- Module      : Database.Relational.Query.TH
-- Copyright   : 2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains backword compatibility interface of
-- relational-query <= 0.9.*
module Database.Relational.Query.TH
  {-# DEPRECATED "Should switch namespace to Database.Relational.TH" #-}
  ( module Database.Relational.TH
  ) where

import Database.Relational.TH
