{-# LANGUAGE TemplateHaskell #-}

module Database.Relational.Schema.SQLServer (
  module Database.Relational.Schema.SQLServer.Config,

  getType, normalizeColumn, notNull,
  columnTypeQuerySQL, primaryKeyQuerySQL
  ) where

import qualified Data.Map as Map
import           Database.Relational.Schema.SQLServer.Columns (Columns, columns)
import qualified Database.Relational.Schema.SQLServer.Columns as Columns
import qualified Database.Relational.Schema.SQLServer.Indexes as Indexes
import qualified Database.Relational.Schema.SQLServer.IndexColumns as IndexColumns
import           Database.Relational.Schema.SQLServer.Types (Types, types)
import qualified Database.Relational.Schema.SQLServer.Types as Types

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map (Map)
import Data.Time (LocalTime, Day, TimeOfDay)
import Database.Relational (Query, Relation, Record, Flat, PureOperand,
                            fst', snd', (!), (.=.), (><), asc, relationalQuery, just,
                            query, relation', unsafeShowSql,
                            unsafeProjectSql, wheres, toFlat,)

import Database.Relational.Schema.SQLServer.Config
import Language.Haskell.TH (TypeQ)

--{-# ANN module "HLint: ignore Redundant $" #-}

mapFromSqlDefault :: Map String TypeQ
mapFromSqlDefault =
    Map.fromList [ ("text",          [t|ByteString|])
                 , ("date",          [t|Day|])
                 , ("time",          [t|TimeOfDay|])
                 , ("tinyint",       [t|Int8|])
                 , ("smallint",      [t|Int16|])
                 , ("int",           [t|Int32|])
                 , ("real",          [t|Double|])
                 , ("datetime",      [t|LocalTime|])
                 , ("float",         [t|Double|])
                 , ("ntext",         [t|String|])
                 , ("bit",           [t|Char|])
                 , ("bigint",        [t|Int64|])
                 , ("varchar",       [t|String|])
                 , ("binary",        [t|ByteString|])
                 , ("char",          [t|String|])
                 , ("timestamp",     [t|LocalTime|])
                 , ("nvarchar",      [t|String|])
                 , ("nchar",         [t|String|])
                 ]

normalizeColumn :: String -> String
normalizeColumn = map toLower

notNull :: ((Columns, Types.Types), String) -> Bool
notNull ((cols,_),_) = isTrue . Columns.isNullable $ cols
  where
    isTrue (Just b) = not b
    isTrue _        = True

getType :: Map String TypeQ -> ((Columns, Types.Types), String) -> Maybe (String, TypeQ)
getType mapFromSql rec@((cols,typs),typScms) = do
    colName <- Columns.name cols
    typ <- Map.lookup key mapFromSql
           <|>
           Map.lookup key mapFromSqlDefault
    return (normalizeColumn colName, mayNull typ)
  where
    key = if typScms == "sys"
            then Types.name typs
            else typScms ++ "." ++ Types.name typs
    mayNull typ = if notNull rec
                    then typ
                    else [t|Maybe $(typ)|]

sqlsrvTrue :: Record Flat Bool
sqlsrvTrue =  unsafeProjectSql "1"

sqlsrvObjectId :: Record PureOperand String -> Record PureOperand String -> Record PureOperand Int32
sqlsrvObjectId s t = unsafeProjectSql $
    "OBJECT_ID(" ++ unsafeShowSql s ++ " + '.' + " ++ unsafeShowSql t ++ ")"

columnTypeRelation :: Relation (String,String) ((Columns,Types),String)
columnTypeRelation = relation' $ \ph -> do
    cols <- query columns
    typs <- query types

    wheres $ cols ! Columns.userTypeId' .=. typs ! Types.userTypeId'
    wheres $ cols ! Columns.objectId'   .=. toFlat (sqlsrvObjectId (ph ! fst') (ph ! snd'))
    asc $ cols ! Columns.columnId'
    return   (cols >< typs >< sqlsrvSchemaName (typs ! Types.schemaId' :: Record Flat Int32))
  where
    sqlsrvSchemaName i = unsafeProjectSql $
        "SCHEMA_NAME(" ++ unsafeShowSql i ++ ")"

columnTypeQuerySQL :: Query (String, String) ((Columns, Types), String)
columnTypeQuerySQL =  relationalQuery columnTypeRelation

primaryKeyRelation :: Relation (String,String) (Maybe String)
primaryKeyRelation = relation' $ \ph -> do
    idxes  <- query Indexes.indexes
    idxcol <- query IndexColumns.indexColumns
    cols   <- query columns
    wheres $ idxes  ! Indexes.objectId'      .=. idxcol ! IndexColumns.objectId'
    wheres $ idxes  ! Indexes.indexId'       .=. idxcol ! IndexColumns.indexId'
    wheres $ idxcol ! IndexColumns.objectId' .=. cols   ! Columns.objectId'
    wheres $ idxcol ! IndexColumns.columnId' .=. cols   ! Columns.columnId'
    wheres $ idxes  ! Indexes.isPrimaryKey'  .=. just sqlsrvTrue
    wheres $ idxes  ! Indexes.objectId'      .=. toFlat (sqlsrvObjectId (ph ! fst') (ph ! snd'))
    asc    $ idxcol ! IndexColumns.keyOrdinal'
    return   (cols  ! Columns.name')

primaryKeyQuerySQL :: Query (String,String) (Maybe String)
primaryKeyQuerySQL =  relationalQuery primaryKeyRelation
