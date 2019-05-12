-- |
-- Module      : Database.Relational.Schema.Oracle.Config
-- Copyright   : 2014-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.Oracle.Config (config) where

import Database.Relational (Config, defaultConfig)


-- | Configuration parameter against Oracle.
config :: Config
config = defaultConfig
