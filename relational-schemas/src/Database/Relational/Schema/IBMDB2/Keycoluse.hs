{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Database.Relational.Schema.DB2Syscat.Keycoluse
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Generate template of SYSCAT.keycoluse system catalog table.
-- Not all columns are mapped to Haskell record.
-- Minimum implementation required to generate table constraints.
module Database.Relational.Schema.IBMDB2.Keycoluse where

import GHC.Generics (Generic)
import Data.Int (Int16)
import Database.Relational.TH (defineTableTypesAndRecord)

import Database.Relational.Schema.IBMDB2.Config (config)


-- Not all column is mapped. Minimum implementation.
$(defineTableTypesAndRecord config
  "SYSCAT" "keycoluse"
  [("constname", [t| String |]),
   ("colname"  , [t| String |]),
   ("colseq"   , [t| Int16 |])]
  [''Show, ''Generic])
