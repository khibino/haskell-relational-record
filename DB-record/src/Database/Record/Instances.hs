{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Record.Instances
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Basic 'PersistableWidth' instances to load schema of system catalogs
module Database.Record.Instances () where

import Data.Int (Int16, Int32, Int64)
import Database.Record.Persistable
  (PersistableWidth(persistableWidth), valueWidth)


instance PersistableWidth String  where
  persistableWidth = valueWidth

instance PersistableWidth Int  where
  persistableWidth = valueWidth

instance PersistableWidth Int16  where
  persistableWidth = valueWidth

instance PersistableWidth Int32  where
  persistableWidth = valueWidth

instance PersistableWidth Int64  where
  persistableWidth = valueWidth
