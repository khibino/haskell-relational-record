-- |
-- Module      : Database.Relational.Pi
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines typed projection path objects.
-- Contains normal interfaces.
module Database.Relational.Pi (
  -- * Projection path
  Pi, (<.>), (<?.>), (<?.?>),

  id',
  ) where

import qualified Control.Category as Category

import Database.Relational.Pi.Unsafe (Pi, (<.>), (<?.>), (<?.?>))


-- | Identity projection path.
id' :: Pi a a
id' = Category.id
