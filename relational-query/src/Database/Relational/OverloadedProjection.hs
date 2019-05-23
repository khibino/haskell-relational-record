{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

#if __GLASGOW_HASKELL__ >= 800
-- |
-- Module      : Database.Relational.OverloadedProjection
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides interfaces of overloaded projections.
module Database.Relational.OverloadedProjection (
  HasProjection (..),
  ) where

import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol)

import Database.Record (PersistableWidth)
import Database.Relational.Typed.Record (PI)

import Database.Relational.Pi (Pi)
import Database.Relational.Projectable ((!))


data PiLabel (l :: Symbol) = GetPi

-- | Projection interface to implement Pi with row polymorphism.
class HasProjection l a b | l a -> b where
  projection :: PiLabel l -> Pi a b

#if __GLASGOW_HASKELL__ >= 802
-- | Derive 'IsLabel' instance from 'HasProjection'.
instance HasProjection l a b => IsLabel l (Pi a b) where
  fromLabel = projection (GetPi :: PiLabel l)

-- | Derive 'PI' label.
instance (PersistableWidth a, HasProjection l a b)
          => IsLabel l (PI c a b) where
  fromLabel = (! projection (GetPi :: PiLabel l))
#else
-- | Derive 'IsLabel' instance from 'HasProjection'.
instance HasProjection l a b => IsLabel l (Pi a b) where
  fromLabel _ = projection (GetPi :: PiLabel l)

-- | Derive 'PI' label.
instance (PersistableWidth a, HasProjection l a b)
          => IsLabel l (PI c a b) where
  fromLabel _ = (! projection (GetPi :: PiLabel l))
#endif
#else
module Database.Relational.OverloadedProjection () where
#endif
