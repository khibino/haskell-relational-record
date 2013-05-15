{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

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
import Database.Record.TH (deriveNotNullValue)

$(deriveNotNullValue [t| String |])
$(deriveNotNullValue [t| Int |])
$(deriveNotNullValue [t| Int16 |])
$(deriveNotNullValue [t| Int32 |])
$(deriveNotNullValue [t| Int64 |])
