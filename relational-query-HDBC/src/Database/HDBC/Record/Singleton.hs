{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Record.Singleton
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides HDBC instance definitions of DB-record for 'Singleton' record.
module Database.HDBC.Record.Singleton () where

import Database.HDBC.Record.Persistable ()

import Database.HDBC (SqlValue)

import Database.Record (PersistableWidth, Persistable(persistable), PersistableValue)
import Database.Record.Singleton (Singleton, derivedPersistableSingleton)

-- | Infered 'PersistableRecord'.
instance (PersistableValue SqlValue a, PersistableWidth (Singleton a))
         => Persistable SqlValue (Singleton a)  where
  persistable = derivedPersistableSingleton
