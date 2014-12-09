-- |
-- Module      : Database.Relational.Schema.MySQLInfo.Config
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.MySQLInfo.Config (config) where

import Database.Relational.Query (Config (..), defaultConfig)


-- | Configuration parameter against MySQL.
config :: Config
config =  defaultConfig { normalizedTableName = False }
