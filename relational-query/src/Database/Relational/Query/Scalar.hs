{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Query.Scalar
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines type classes and templates for scalar queries.
module Database.Relational.Query.Scalar (
  -- * Single degree constraint
  ScalarDegree, defineScalarDegree
  ) where

import Language.Haskell.TH (Q, TypeQ, Dec)

import Database.Record (PersistableWidth)


-- | Constraint which represents scalar degree.
class PersistableWidth ct => ScalarDegree ct

instance ScalarDegree ct => ScalarDegree (Maybe ct)

-- | 'ScalarDegree' instance templates.
defineScalarDegree :: TypeQ -> Q [Dec]
defineScalarDegree typeCon = do
  [d| instance ScalarDegree $(typeCon) |]
