-- |
-- Module      : Database.Relational.Schema.PostgreSQL.Config
-- Copyright   : 2014-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PostgreSQL.Config (config) where

import Database.Relational (Config, defaultConfig)


-- | Configuration parameter against PostgreSQL.
config :: Config
config = defaultConfig
