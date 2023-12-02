{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 800
-- |
-- Module      : Database.Relational.OverloadedProjection
-- Copyright   : 2017-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides interfaces of overloaded projections.
module Database.Relational.OverloadedProjection (
  PiLabel(..),
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
#define FROM_LABEL fromLabel
#else
#define FROM_LABEL fromLabel _
#endif

instance l ~ l' => IsLabel l (PiLabel l') where -- a type equality constraint makes better type inference
  FROM_LABEL = GetPi

-- | Derive 'IsLabel' instance from 'HasProjection'.
instance HasProjection l a b => IsLabel l (Pi a b) where
  FROM_LABEL = projection (GetPi :: PiLabel l)

#if defined(ISLABEL)
-- | Derive 'PI' label.
instance (PersistableWidth a, HasProjection l a b)
          => IsLabel l (PI c a b) where
  FROM_LABEL = (! projection (GetPi :: PiLabel l))
#endif
#else
module Database.Relational.OverloadedProjection () where
#endif
