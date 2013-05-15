{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Database.Relational.Schema.DB2Syscat.Tabconst
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.DB2Syscat.Tabconst where

import Database.Record.TH (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)


-- Not all column is mapped. Minimum implementation.
$(defineTableTypesAndRecordDefault
  "SYSCAT" "tabconst"
  [("constname", [t| String |]),
   ("tabschema", [t| String |]),
   ("tabname"  , [t| String |]),
   --
   ("type"     , [t| String |]),
   ("enforced" , [t| String |])]
  [derivingShow])
