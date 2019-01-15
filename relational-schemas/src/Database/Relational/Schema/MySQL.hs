{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

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

import Database.Relational              ( Relation
                                        , Query
                                        , relationalQuery
                                        , query
                                        , relation
                                        , wheres
                                        , (.=.)
                                        , (!)
                                        , asc
                                        , value
                                        , ph
                                        )

import Database.Relational.ReboundSyntax hiding ((>>=), (>>))
import qualified Database.Relational.ReboundSyntax as ReboundSyntax
import Database.Relational.ExtensibleRecord (ExRecord, type (>:))

import           Database.Relational.Schema.MySQLInfo.Columns           (Columns, columns)
import qualified Database.Relational.Schema.MySQLInfo.Columns           as Columns
import           Database.Relational.Schema.MySQLInfo.TableConstraints  (tableConstraints)
import qualified Database.Relational.Schema.MySQLInfo.TableConstraints  as Tabconst
import           Database.Relational.Schema.MySQLInfo.KeyColumnUsage    (keyColumnUsage)
import qualified Database.Relational.Schema.MySQLInfo.KeyColumnUsage    as Keycoluse

import Prelude hiding ((>>=), (>>))
import qualified Prelude

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
        (>>=) :: Monad m => m a -> (a -> m b) -> m b
        (>>=) = (Prelude.>>=)


type NameInSchema = ExRecord
 '[ "tableSchema" >: String
  , "tableName" >: String
  ]


columnsQuerySQL :: Query () Columns
columnsQuerySQL = relationalQuery columnsRelationFromTable
    where
        columnsRelationFromTable :: Relation (ExRecord '[]) NameInSchema () Columns
        columnsRelationFromTable = relation $ do
            c <- query columns
            wheres $ c ! Columns.tableSchema' .=. ph #tableSchema
            wheres $ c ! Columns.tableName'   .=. ph #tableName
            asc $ c ! Columns.ordinalPosition'
            ireturn c

        (>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
        (>>=) = (ReboundSyntax.>>=)
        (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
        (>>) = (ReboundSyntax.>>)

-- igrep TODO: Convert to IxMonad
primaryKeyQuerySQL :: Query () String
primaryKeyQuerySQL = relationalQuery primaryKeyRelation
    where
        primaryKeyRelation :: Relation (ExRecord '[]) NameInSchema () String
        primaryKeyRelation = relation $ do
            cons <- query tableConstraints
            key  <- query keyColumnUsage

            wheres $ cons ! Tabconst.tableSchema'    .=. key ! Keycoluse.tableSchema'
            wheres $ cons ! Tabconst.tableName'      .=. key ! Keycoluse.tableName'
            wheres $ cons ! Tabconst.constraintName' .=. key ! Keycoluse.constraintName'

            wheres $ cons ! Tabconst.tableSchema' .=. ph #tableSchema
            wheres $ cons ! Tabconst.tableName'   .=. ph #tableName
            wheres $ cons ! Tabconst.constraintType' .=. value "PRIMARY KEY"

            asc $ key ! Keycoluse.ordinalPosition'

            ireturn (key ! Keycoluse.columnName')

        (>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
        (>>=) = (ReboundSyntax.>>=)
        (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
        (>>) = (ReboundSyntax.>>)
