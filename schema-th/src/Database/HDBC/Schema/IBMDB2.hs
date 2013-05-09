{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Schema.IBMDB2
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.Schema.IBMDB2 (
  Columns, driverIBMDB2
  ) where

import Prelude hiding (length)

import qualified Data.List as List
import Data.Int (Int16, Int32, Int64)
import Data.Char (toUpper, toLower)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Time (LocalTime, Day)
import Language.Haskell.TH (Q, Type)
import qualified Language.Haskell.TH.Name.Extra as TH

import Database.HDBC (IConnection, SqlValue)

import Database.HDBC.SqlValueExtra ()
import Database.HDBC.Record.Query (runQuery', listToUnique)
import Database.HDBC.Record.Persistable ()

import Database.Record.TH (derivingShow)

import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Type (unsafeTypedQuery, fromRelation)
import Database.Relational.Query.TH (defineRecordAndTableDefault)
import Database.Relational.Query
  (Query, Relation, inner, relation,
   wheres, (.=.), (!), (!?), placeholder, asc)

import Language.SQL.Keyword (Keyword(..))
import qualified Language.SQL.Keyword as SQL

import Database.HDBC.Schema.Driver
  (TypeMap, Driver, getFieldsWithMap, getPrimaryKey, emptyDriver)


$(defineRecordAndTableDefault
  [t| SqlValue |]
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


mapFromSqlDefault :: Map String (Q Type)
mapFromSqlDefault =
  fromList [("VARCHAR",   [t|String|]),
            ("CHAR",      [t|String|]),
            ("CHARACTER", [t|String|]),
            ("TIMESTAMP", [t|LocalTime|]),
            ("DATE",      [t|Day|]),
            ("SMALLINT",  [t|Int16|]),
            ("INTEGER",   [t|Int32|]),
            ("BIGINT",    [t|Int64|]),
            ("BLOB",      [t|String|]),
            ("CLOB",      [t|String|])]

normalizeField :: String -> String
normalizeField =  map toLower

notNull :: Columns -> Bool
notNull =  (== "N") . nulls

getType :: Map String (Q Type) -> Columns -> (String, Q Type)
getType mapFromSql rec =
  (normalizeField $ colname rec,
   mayNull $ mapFromSql Map.! typename rec)
  where mayNull typ = if notNull rec
                      then typ
                      else [t| Maybe $(typ) |]

columnsRelationFromTable :: Relation Columns
columnsRelationFromTable =  relation $ do
  c <- inner columns
  wheres $ c ! tabschema' .=. placeholder
  wheres $ c ! tabname'   .=. placeholder
  asc $ c ! colno'
  return c

columnsQuerySQL :: Query (String, String) Columns
columnsQuerySQL =  fromRelation columnsRelationFromTable


primaryKeyQuerySQL :: Query (String, String) String
primaryKeyQuerySQL =
  unsafeTypedQuery .
  SQL.unwordsSQL
  $ [SELECT, "key.colname",
     FROM,
     "SYSCAT.tabconst", AS, "const", ",",
     "SYSCAT.keycoluse", AS, "key", ",",
     SQL.word (Table.name tableOfColumns), AS, "col",
     WHERE,
     "const.tabschema = col.tabschema", AND,
     "const.tabname = col.tabname", AND,
     "key.colname = col.colname", AND,
     "const.constname = key.constname", AND,

     "col.nulls = 'N'", AND,
     "const.type = 'P'", AND, "const.enforced = 'Y'", AND,

     "const.tabschema = ?", AND, "const.tabname = ?"]

logPrefix :: String -> String
logPrefix =  ("IBMDB2: " ++)

putLog :: String -> IO ()
putLog =  putStrLn . logPrefix

compileErrorIO :: String -> IO a
compileErrorIO =  TH.compileErrorIO . logPrefix

getPrimaryKey' :: IConnection conn
              => conn
              -> String
              -> String
              -> IO (Maybe String)
getPrimaryKey' conn scm' tbl' = do
  let tbl = map toUpper tbl'
      scm = map toUpper scm'
  mayPrim <- runQuery' conn (scm, tbl) primaryKeyQuerySQL
             >>= listToUnique
  let mayPrimaryKey = normalizeField `fmap` mayPrim
  putLog $ "getPrimaryKey: primary key = " ++ show mayPrimaryKey

  return mayPrimaryKey

getFields' :: IConnection conn
          => TypeMap
          -> conn
          -> String
          -> String
          -> IO ([(String, Q Type)], [Int])
getFields' tmap conn scm' tbl' = do
  let tbl = map toUpper tbl'
      scm = map toUpper scm'
      
  cols <- runQuery' conn (scm, tbl) columnsQuerySQL
  case cols of
    [] ->  compileErrorIO
           $ "getFields: No columns found: schema = " ++ scm ++ ", table = " ++ tbl
    _  ->  return ()

  let notNullIdxs = map fst . filter (notNull . snd) . zip [0..] $ cols
  putLog
    $  "getFields: num of columns = " ++ show (List.length cols)
    ++ ", not null columns = " ++ show notNullIdxs
  let mapFromSql = fromList tmap `Map.union` mapFromSqlDefault

  return $ (map (getType mapFromSql) cols, notNullIdxs)

driverIBMDB2 :: IConnection conn => Driver conn
driverIBMDB2 =
  emptyDriver { getFieldsWithMap = getFields' }
              { getPrimaryKey    = getPrimaryKey' }
