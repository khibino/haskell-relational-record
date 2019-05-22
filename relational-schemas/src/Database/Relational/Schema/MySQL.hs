{-# LANGUAGE TemplateHaskell #-}
module Database.Relational.Schema.MySQL
    ( module Database.Relational.Schema.MySQL.Config

    , normalizeColumn
    , notNull
    , getType
    , columnsQuerySQL
    , primaryKeyQuerySQL
    )
    where

import           Data.Int               (Int8, Int16, Int32, Int64)
import           Data.Char              (toLower, toUpper)
import           Data.Map               (Map, fromList)
import qualified Data.Map               as Map
import           Data.Time              (Day, LocalTime, TimeOfDay)
import           Data.Time.Clock.POSIX  (POSIXTime)
import           Data.ByteString        (ByteString)
import           Control.Applicative    ((<|>))
import           Language.Haskell.TH    (TypeQ)

import Database.Relational              ( Query
                                        , relationalQuery
                                        , query
                                        , relation'
                                        , wheres
                                        , (.=.)
                                        , (!)
                                        , asc
                                        , value
                                        , fst'
                                        , snd'
                                        , toFlat
                                        )

import           Database.Relational.Schema.MySQL.Config
import           Database.Relational.Schema.MySQL.Columns           (Columns, columns)
import qualified Database.Relational.Schema.MySQL.Columns           as Columns
import           Database.Relational.Schema.MySQL.TableConstraints  (tableConstraints)
import qualified Database.Relational.Schema.MySQL.TableConstraints  as Tabconst
import           Database.Relational.Schema.MySQL.KeyColumnUsage    (keyColumnUsage)
import qualified Database.Relational.Schema.MySQL.KeyColumnUsage    as Keycoluse

-- TODO: Need to check unsigned int types to avoid wrong mapping

mapFromSqlDefault :: Map String TypeQ
mapFromSqlDefault = fromList
    [ ("CHAR",       [t| String |])
    , ("VARCHAR",    [t| String |])
    , ("TINYTEXT",   [t| String |])
    , ("TEXT",       [t| String |])
    , ("MEDIUMTEXT", [t| String |])
    , ("LONGTEXT",   [t| String |])
    , ("TINYBLOB",   [t| ByteString |])
    , ("BLOB",       [t| ByteString |])
    , ("MEDIUMBLOB", [t| ByteString |])
    , ("LONGBLOB",   [t| ByteString |])
    , ("DATE",       [t| Day |])
    , ("DATETIME",   [t| LocalTime |])
    , ("TIME",       [t| TimeOfDay |])
    , ("TIMESTAMP",  [t| POSIXTime |])
    , ("TINYINT",    [t| Int8 |])
    , ("SMALLINT",   [t| Int16 |])
    , ("MEDIUMINT",  [t| Int32 |])
    , ("INT",        [t| Int32 |])
    , ("INTEGER",    [t| Int32 |])
    , ("BIGINT",     [t| Int64 |])
    ]

normalizeColumn :: String -> String
normalizeColumn = map toLower

notNull :: Columns -> Bool
notNull = (== "NO") . Columns.isNullable

getType :: Map String TypeQ
        -> Columns
        -> Maybe (String, TypeQ)
getType mapFromSql rec = do
    typ <- Map.lookup key mapFromSql
           <|>
           Map.lookup key mapFromSqlDefault
    return (normalizeColumn $ Columns.columnName rec, mayNull typ)
    where
        key = map toUpper $ Columns.dataType rec
        mayNull typ = if notNull rec
                      then typ
                      else [t|Maybe $(typ)|]

columnsQuerySQL :: Query (String, String) Columns
columnsQuerySQL = relationalQuery columnsRelationFromTable
    where
        columnsRelationFromTable = relation' $ \ph -> do
            c <- query columns
            wheres $ c ! Columns.tableSchema' .=. toFlat (ph ! fst')
            wheres $ c ! Columns.tableName'   .=. toFlat (ph ! snd')
            asc $ c ! Columns.ordinalPosition'
            return c

primaryKeyQuerySQL :: Query (String, String) String
primaryKeyQuerySQL = relationalQuery primaryKeyRelation
    where
        primaryKeyRelation = relation' $ \ph -> do
            cons <- query tableConstraints
            key  <- query keyColumnUsage

            wheres $ cons ! Tabconst.tableSchema'    .=. key ! Keycoluse.tableSchema'
            wheres $ cons ! Tabconst.tableName'      .=. key ! Keycoluse.tableName'
            wheres $ cons ! Tabconst.constraintName' .=. key ! Keycoluse.constraintName'

            wheres $ cons ! Tabconst.tableSchema' .=. toFlat (ph ! fst')
            wheres $ cons ! Tabconst.tableName'   .=. toFlat (ph ! snd')
            wheres $ cons ! Tabconst.constraintType' .=. toFlat (value "PRIMARY KEY")

            asc $ key ! Keycoluse.ordinalPosition'

            return (key ! Keycoluse.columnName')
