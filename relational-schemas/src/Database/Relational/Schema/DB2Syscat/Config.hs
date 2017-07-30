-- |
-- Module      : Database.Relational.Schema.DB2Syscat.Config
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.DB2Syscat.Config (config) where

import Database.Relational (Config (..), ProductUnitSupport (..), defaultConfig)


-- | Configuration parameter against IBM DB2.
config :: Config
config =  defaultConfig { productUnitSupport = PUNotSupported }
