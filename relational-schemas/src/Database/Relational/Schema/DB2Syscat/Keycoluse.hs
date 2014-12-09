{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Schema.DB2Syscat.Keycoluse
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Generate template of SYSCAT.keycoluse system catalog table.
-- Not all columns are mapped to Haskell record.
-- Minimum implementation required to generate table constraints.
module Database.Relational.Schema.DB2Syscat.Keycoluse where

import Data.Int (Int16)
import Database.Record.TH (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)

import Database.Relational.Schema.DB2Syscat.Config (config)


-- Not all column is mapped. Minimum implementation.
$(defineTableTypesAndRecordDefault config
  "SYSCAT" "keycoluse"
  [("constname", [t| String |]),
   ("colname"  , [t| String |]),
   ("colseq"   , [t| Int16 |])]
  [derivingShow])
