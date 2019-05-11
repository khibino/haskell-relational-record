-- |
-- Module      : Database.Relational.Schema.SQLite3.Config
-- Copyright   : 2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.SQLite3.Config (config) where

import Database.Relational (Config (addModifyTableAliasAS), defaultConfig)


-- | Configuration parameter against SQLite3.
config :: Config
config = defaultConfig { addModifyTableAliasAS = True }
