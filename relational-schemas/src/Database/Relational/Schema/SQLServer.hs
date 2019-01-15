{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Database.Relational.Schema.SQLServer (
  getType, normalizeColumn, notNull,
  columnTypeQuerySQL, primaryKeyQuerySQL
  ) where

import qualified Data.Map as Map
import qualified Database.Relational.Schema.SQLServerSyscat.Columns as Columns
import qualified Database.Relational.Schema.SQLServerSyscat.Indexes as Indexes
import qualified Database.Relational.Schema.SQLServerSyscat.IndexColumns as IndexColumns
import qualified Database.Relational.Schema.SQLServerSyscat.Types as Types

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map (Map)
import Data.Time (LocalTime, Day, TimeOfDay)
import Database.Relational (Query, Relation, PlaceHolders, Record, Flat,
                            (!), (.=.), (><), asc, relationalQuery, just, placeholder',
                            query, relation, unsafeShowSql,
                            unsafeProjectSql, wheres, ph)

import Database.Relational.ReboundSyntax hiding ((>>=), (>>))
import qualified Database.Relational.ReboundSyntax as ReboundSyntax
import Database.Relational.ExtensibleRecord (ExRecord, ExRecordNil, type (>:), type (++))

import Database.Relational.Schema.SQLServerSyscat.Columns
import Database.Relational.Schema.SQLServerSyscat.Indexes
import Database.Relational.Schema.SQLServerSyscat.IndexColumns
import Database.Relational.Schema.SQLServerSyscat.Types
import Language.Haskell.TH (TypeQ)

import Prelude hiding ((>>=), (>>))
import qualified Prelude

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

notNull :: ((Columns,Types),String) -> Bool
notNull ((cols,_),_) = isTrue . Columns.isNullable $ cols
  where
    isTrue (Just b) = not b
    isTrue _        = True

getType :: Map String TypeQ -> ((Columns,Types),String) -> Maybe (String, TypeQ)
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
    (>>=) :: Monad m => m a -> (a -> m b) -> m b
    (>>=) = (Prelude.>>=)


type ObjectIdArgs = ExRecord
 '[ "objectName" >: String
  , "objectType" >: String
  ]


sqlsrvTrue :: Record ExRecordNil ExRecordNil Flat Bool
sqlsrvTrue =  unsafeProjectSql "1"

sqlsrvObjectId :: Record ExRecordNil (ExRecord xs) Flat String -> Record ExRecordNil (ExRecord ys) Flat String -> Record ExRecordNil (ExRecord (xs ++ ys)) Flat Int32
sqlsrvObjectId s t = unsafeProjectSql $
    "OBJECT_ID(" ++ unsafeShowSql s ++ " + '.' + " ++ unsafeShowSql t ++ ")"

sqlsrvOidPlaceHolder :: Record ExRecordNil ObjectIdArgs Flat Int32
sqlsrvOidPlaceHolder =  sqlsrvObjectId (ph #objectName) (ph #objectType)

columnTypeRelation :: Relation ExRecordNil ObjectIdArgs () ((Columns, Types), String)
columnTypeRelation = relation $ do
    cols <- query columns
    typs <- query types

    wheres $ cols ! Columns.userTypeId' .=. typs ! Types.userTypeId'
    wheres $ cols ! Columns.objectId'   .=. oid
    asc $ cols ! Columns.columnId'
    ireturn   (cols >< typs >< sqlsrvSchemaName (typs ! Types.schemaId' :: Record ExRecordNil ExRecordNil Flat Int32))
  where
    oid = sqlsrvOidPlaceHolder
    sqlsrvSchemaName i = unsafeProjectSql $
        "SCHEMA_NAME(" ++ unsafeShowSql i ++ ")"
    (>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
    (>>=) = (ReboundSyntax.>>=)
    (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
    (>>) = (ReboundSyntax.>>)

columnTypeQuerySQL :: Query () ((Columns, Types), String)
columnTypeQuerySQL =  relationalQuery columnTypeRelation

primaryKeyRelation :: Relation ExRecordNil ObjectIdArgs () (Maybe String)
primaryKeyRelation = relation $ do
    idxes  <- query indexes
    idxcol <- query indexColumns
    cols   <- query columns
    wheres $ idxes  ! Indexes.objectId'      .=. idxcol ! IndexColumns.objectId'
    wheres $ idxes  ! Indexes.indexId'       .=. idxcol ! IndexColumns.indexId'
    wheres $ idxcol ! IndexColumns.objectId' .=. cols   ! Columns.objectId'
    wheres $ idxcol ! IndexColumns.columnId' .=. cols   ! Columns.columnId'
    wheres $ idxes  ! Indexes.isPrimaryKey'  .=. just sqlsrvTrue
    let oid = sqlsrvOidPlaceHolder
    wheres $ idxes  ! Indexes.objectId'      .=. oid
    asc    $ idxcol ! IndexColumns.keyOrdinal'
    ireturn (cols   ! Columns.name')
  where
    (>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
    (>>=) = (ReboundSyntax.>>=)
    (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
    (>>) = (ReboundSyntax.>>)

primaryKeyQuerySQL :: Query () (Maybe String)
primaryKeyQuerySQL =  relationalQuery primaryKeyRelation
