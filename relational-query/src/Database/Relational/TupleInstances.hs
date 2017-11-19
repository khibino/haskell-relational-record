{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.Relational.TupleInstances
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines ProductConstructor instances and projection path objects of tuple types.
module Database.Relational.TupleInstances where

import Control.Applicative ((<$>))

import Database.Record (PersistableWidth)

import Database.Relational.Pi (Pi)
import Database.Relational.InternalTH.Base
  (defineTuplePi, defineTupleShowConstantInstance,)


$(concat <$> mapM defineTuplePi [2..7])
$(concat <$> mapM defineTupleShowConstantInstance [2..7])
-- Generic instances of tuple types are generated from 2 to 7 in GHC.Generics.

-- | Projection path for fst of tuple.
fst' :: (PersistableWidth a, PersistableWidth b) => Pi (a, b) a
fst' = tuplePi2_0'

-- | Projection path for snd of tuple.
snd' :: (PersistableWidth a, PersistableWidth b) => Pi (a, b) b
snd' = tuplePi2_1'
