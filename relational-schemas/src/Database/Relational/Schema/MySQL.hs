{-# LANGUAGE TemplateHaskell #-}
module Database.Relational.Schema.MySQL
    ( normalizeColumn
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
                                        , (><)
                                        , placeholder
                                        , asc
                                        , value
                                        )

import           Database.Relational.Schema.MySQLInfo.Columns           (Columns, columns)
import qualified Database.Relational.Schema.MySQLInfo.Columns           as Columns
import           Database.Relational.Schema.MySQLInfo.TableConstraints  (tableConstraints)
import qualified Database.Relational.Schema.MySQLInfo.TableConstraints  as Tabconst
import           Database.Relational.Schema.MySQLInfo.KeyColumnUsage    (keyColumnUsage)
import qualified Database.Relational.Schema.MySQLInfo.KeyColumnUsage    as Keycoluse

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
        key = map toUpper $ Columns.columnType rec
        mayNull typ = if notNull rec
                      then typ
                      else [t|Maybe $(typ)|]

columnsQuerySQL :: Query (String, String) Columns
columnsQuerySQL = relationalQuery columnsRelationFromTable
    where
        columnsRelationFromTable = relation' $ do
            c <- query columns
            (schemaP, ()) <- placeholder (\ph -> wheres $ c ! Columns.tableSchema' .=. ph)
            (nameP  , ()) <- placeholder (\ph -> wheres $ c ! Columns.tableName'   .=. ph)
            asc $ c ! Columns.ordinalPosition'
            return (schemaP >< nameP, c)

primaryKeyQuerySQL :: Query (String, String) String
primaryKeyQuerySQL = relationalQuery primaryKeyRelation
    where
        primaryKeyRelation = relation' $ do
            cons <- query tableConstraints
            key  <- query keyColumnUsage

            wheres $ cons ! Tabconst.tableSchema'    .=. key ! Keycoluse.tableSchema'
            wheres $ cons ! Tabconst.tableName'      .=. key ! Keycoluse.tableName'
            wheres $ cons ! Tabconst.constraintName' .=. key ! Keycoluse.constraintName'

            (schemaP, ()) <- placeholder (\ph -> wheres $ cons ! Tabconst.tableSchema' .=. ph)
            (nameP  , ()) <- placeholder (\ph -> wheres $ cons ! Tabconst.tableName'   .=. ph)
            wheres $ cons ! Tabconst.constraintType' .=. value "PRIMARY KEY"

            asc $ key ! Keycoluse.ordinalPosition'

            return (schemaP >< nameP, key ! Keycoluse.columnName')
