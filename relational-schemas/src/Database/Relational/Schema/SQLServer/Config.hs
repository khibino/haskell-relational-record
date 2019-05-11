-- |
-- Module      : Database.Relational.Schema.PgCatalog.Config
-- Copyright   : 2014-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.SQLServer.Config (config) where

import Database.Relational (Config, defaultConfig)


-- | Configuration parameter against SQLServer.
config :: Config
config = defaultConfig
