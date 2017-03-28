{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Query.ProjectableClass
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides interfaces to preserve constraints of
-- direct product projections.
module Database.Relational.Query.ProjectableClass (
  -- * Interface to specify record constructors.
  ProductConstructor (..),

  -- * ProjectableFunctor and ProjectableApplicative
  ProjectableFunctor (..), ProjectableApplicative (..), ipfmap,
  ) where

-- | Specify tuple like record constructors which are allowed to define 'ProjectableFunctor'.
class ProductConstructor r where
  -- | The constructor which has type 'r'.
  productConstructor :: r

-- | Weaken functor on projections.
class ProjectableFunctor p where
  -- | Method like 'fmap'.
  (|$|) :: ProductConstructor (a -> b) => (a -> b) -> p a -> p b

-- | Same as '|$|' other than using inferred record constructor.
ipfmap :: (ProjectableFunctor p, ProductConstructor (a -> b))
       => p a -> p b
ipfmap =  (|$|) productConstructor

-- | Weaken applicative functor on projections.
class ProjectableFunctor p => ProjectableApplicative p where
  -- | Method like '<*>'.
  (|*|) :: p (a -> b) -> p a -> p b

infixl 4 |$|, |*|
