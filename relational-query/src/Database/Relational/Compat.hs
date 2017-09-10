{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Compat
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains backward compatibility definitions of
-- relational-query <= 0.9.*
module Database.Relational.Compat
  {-# DEPRECATED "backward compatibility definitions relational-query <= 0.9.*" #-}
  (
  Projection,

  pfmap, pap, pzero,
  ) where

import Data.Functor.ProductIsomorphic (ProductConstructor, (|$|), (|*|), pureE)

import Database.Relational

{-# DEPRECATED Projection "Replaced by Record type" #-}
-- | old 'Projection' type replaced by 'Record' type.
type Projection = Record


-- Pi

{-# DEPRECATED pfmap "use (|$|) in Data.Functor.ProductIsomorphic instead of this." #-}
-- | old map definition for Pi type.
pfmap :: ProductConstructor (a -> b) => (a -> b) -> Pi r a -> Pi r b
pfmap = (|$|)

{-# DEPRECATED pap "use (|*|) in Data.Functor.ProductIsomorphic instead of this." #-}
-- | old ap definition for Pi type.
pap :: Pi r (a -> b) -> Pi r a -> Pi r b
pap = (|*|)

{-# DEPRECATED pzero "use pureE in Data.Functor.ProductIsomorphic instead of this." #-}
-- | old empty definition for Pi type.
pzero :: Pi a ()
pzero = pureE
