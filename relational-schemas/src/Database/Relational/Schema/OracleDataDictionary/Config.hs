-- |
-- Module      : Database.Relational.Schema.OracleDataDictionary.Config
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.OracleDataDictionary.Config (config) where

import Database.Relational.Query (Config, defaultConfig)


-- | Configuration parameter against Oracle.
config :: Config
config =  defaultConfig
