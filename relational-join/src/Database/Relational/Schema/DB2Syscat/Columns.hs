{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Database.Relational.Schema.DB2Syscat.Columns
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.DB2Syscat.Columns where

import Data.Int (Int16, Int32, Int64)

import Database.Record.Instances ()
import Database.Record.TH (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)

$(defineTableTypesAndRecordDefault
  "SYSCAT" "columns"
  [
    -- column                         schema    type               length         NULL
    -- ------------------------------ --------- ------------------ -------- ----- ------
    -- TABSCHEMA                      SYSIBM    VARCHAR                 128     0 No
    ("tabschema", [t|String|]),
    -- TABNAME                        SYSIBM    VARCHAR                 128     0 No
    ("tabname", [t|String|]),
    -- COLNAME                        SYSIBM    VARCHAR                 128     0 No
    ("colname", [t|String|]),
    -- COLNO                          SYSIBM    SMALLINT                  2     0 No
    ("colno", [t|Int16|]),
    -- TYPESCHEMA                     SYSIBM    VARCHAR                 128     0 No
    ("typeschema", [t|String|]),
    -- TYPENAME                       SYSIBM    VARCHAR                  18     0 No
    ("typename", [t|String|]),
    -- LENGTH                         SYSIBM    INTEGER                   4     0 No
    ("length", [t|Int32|]),
    -- SCALE                          SYSIBM    SMALLINT                  2     0 No
    ("scale", [t|Int16|]),
    -- DEFAULT                        SYSIBM    VARCHAR                 254     0 Yes
    ("default", [t|Maybe String|]),
    -- NULLS                          SYSIBM    CHARACTER                 1     0 No
    ("nulls", [t|String|]),
    -- CODEPAGE                       SYSIBM    SMALLINT                  2     0 No
    ("codepage", [t|Int16|]),
    -- LOGGED                         SYSIBM    CHARACTER                 1     0 No
    ("logged", [t|String|]),
    -- COMPACT                        SYSIBM    CHARACTER                 1     0 No
    ("compact", [t|String|]),
    -- COLCARD                        SYSIBM    BIGINT                    8     0 No
    ("colcard", [t|Int64|]),
    -- HIGH2KEY                       SYSIBM    VARCHAR                 254     0 Yes
    ("high2key", [t|Maybe String|]),
    -- LOW2KEY                        SYSIBM    VARCHAR                 254     0 Yes
    ("low2key", [t|Maybe String|]),
    -- AVGCOLLEN                      SYSIBM    INTEGER                   4     0 No
    ("avgcollen", [t|Int32|]),
    -- KEYSEQ                         SYSIBM    SMALLINT                  2     0 Yes
    ("keyseq", [t|Maybe Int16|]),
    -- PARTKEYSEQ                     SYSIBM    SMALLINT                  2     0 Yes
    ("partkeyseq", [t|Maybe Int16|]),
    -- NQUANTILES                     SYSIBM    SMALLINT                  2     0 No
    ("nquantiles", [t|Int16|]),
    -- NMOSTFREQ                      SYSIBM    SMALLINT                  2     0 No
    ("nmostfreq", [t|Int16|]),
    -- NUMNULLS                       SYSIBM    BIGINT                    8     0 No
    ("numnulls", [t|Int64|]),
    -- TARGET_TYPESCHEMA              SYSIBM    VARCHAR                 128     0 Yes
    ("target_typeschema", [t|Maybe String|]),
    -- TARGET_TYPENAME                SYSIBM    VARCHAR                  18     0 Yes
    ("target_typename", [t|Maybe String|]),
    -- SCOPE_TABSCHEMA                SYSIBM    VARCHAR                 128     0 Yes
    ("scope_tabschema", [t|Maybe String|]),
    -- SCOPE_TABNAME                  SYSIBM    VARCHAR                 128     0 Yes
    ("scope_tabname", [t|Maybe String|]),
    -- SOURCE_TABSCHEMA               SYSIBM    VARCHAR                 128     0 Yes
    ("source_tabschema", [t|Maybe String|]),
    -- SOURCE_TABNAME                 SYSIBM    VARCHAR                 128     0 Yes
    ("source_tabname", [t|Maybe String|]),
    -- DL_FEATURES                    SYSIBM    CHARACTER                10     0 Yes
    ("dl_features", [t|Maybe String|]),
    -- SPECIAL_PROPS                  SYSIBM    CHARACTER                 8     0 Yes
    ("special_props", [t|Maybe String|]),
    -- HIDDEN                         SYSIBM    CHARACTER                 1     0 No
    ("hidden", [t|String|]),
    -- INLINE_LENGTH                  SYSIBM    INTEGER                   4     0 No
    ("inline_length", [t|Int32|]),
    -- IDENTITY                       SYSIBM    CHARACTER                 1     0 No
    ("identity", [t|String|]),
    -- GENERATED                      SYSIBM    CHARACTER                 1     0 No
    ("generated", [t|String|]),
    -- TEXT                           SYSIBM    CLOB                  65538     0 Yes
    ("text", [t|Maybe String|]),
    -- REMARKS                        SYSIBM    VARCHAR                 254     0 Yes
    ("remarks", [t|Maybe String|])

  ]
  [derivingShow])
