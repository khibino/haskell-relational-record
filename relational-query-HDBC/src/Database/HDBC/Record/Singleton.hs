{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.HDBC.Record.Singleton () where

import Database.HDBC.Record.Persistable ()

import Database.HDBC (SqlValue)

import Database.Record (PersistableWidth, Persistable(persistable), PersistableValue)
import Database.Record.Singleton (Singleton, derivedPersistableSingleton)

instance (PersistableValue SqlValue a, PersistableWidth (Singleton a))
         => Persistable SqlValue (Singleton a)  where
  persistable = derivedPersistableSingleton
