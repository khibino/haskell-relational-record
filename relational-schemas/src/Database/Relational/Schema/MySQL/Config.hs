-- |
-- Module      : Database.Relational.Schema.MySQL.Config
-- Copyright   : 2014-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.MySQL.Config (config) where

import Database.Relational (Config (..), defaultConfig)


-- | Configuration parameter against MySQL.
config :: Config
config = defaultConfig { normalizedTableName = False }
