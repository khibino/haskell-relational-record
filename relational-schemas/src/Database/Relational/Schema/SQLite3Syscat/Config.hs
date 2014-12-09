-- |
-- Module      : Database.Relational.Schema.SQLite3Syscat.Config
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.SQLite3Syscat.Config (config) where

import Database.Relational.Query (Config, defaultConfig)


-- | Configuration parameter against SQLite3.
config :: Config
config =  defaultConfig
