-- |
-- Module      : Database.Relational.Query
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains backword compatibility interface of
-- relational-query <= 0.9.*
module Database.Relational.Query
       {-# DEPRECATED "Should switch namespace to Database.Relational" #-}
       ( module Database.Relational,
         module Database.Relational.Compat,
       )  where

import Database.Relational
import Database.Relational.Compat
