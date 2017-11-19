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
module Database.Relational.OverloadedProjection (
  HasProjection (..),
  ) where

import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol)

import Database.Record (PersistableWidth)
import Database.Relational.SqlSyntax (PI)
import Database.Relational.Pi (Pi)
import Database.Relational.Projectable ((!))


data PiLabel (l :: Symbol) = GetPi

-- | Projection interface to implement Pi with row polymorphism.
class HasProjection l a b | l a -> b where
  projection :: PiLabel l -> Pi a b

instance HasProjection l a b => IsLabel l (Pi a b) where
  fromLabel _ = projection (GetPi :: PiLabel l)

instance (PersistableWidth a, IsLabel l (Pi a b))
          => IsLabel l (PI c a b) where
  fromLabel s = (! fromLabel s)
#else
module Database.Relational.OverloadedProjection () where
#endif
