{-# LANGUAGE TemplateHaskell #-}

module Database.Relational.Schema.Oracle
    ( module Database.Relational.Schema.Oracle.Config

    , normalizeColumn, notNull, getType
    , columnsQuerySQL, primaryKeyQuerySQL
    ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (LocalTime)

import Language.Haskell.TH (TypeQ)

import Database.Relational

import Database.Relational.Schema.Oracle.Config
import Database.Relational.Schema.Oracle.ConsColumns (dbaConsColumns)
import qualified Database.Relational.Schema.Oracle.ConsColumns as ConsCols
import Database.Relational.Schema.Oracle.Constraints (dbaConstraints)
import qualified Database.Relational.Schema.Oracle.Constraints as Cons
import Database.Relational.Schema.Oracle.TabColumns (DbaTabColumns, dbaTabColumns)
import qualified Database.Relational.Schema.Oracle.TabColumns as Cols

-- NOT COMPLETED
-- (ref: http://docs.oracle.com/cd/B28359_01/server.111/b28318/datatype.htm)
-- | Mapping between type in Oracle DB and Haskell type.
mapFromSqlDefault :: Map String TypeQ
mapFromSqlDefault = Map.fromList
    [ ("CHAR", [t|String|])
    , ("VARCHAR", [t|String|]) -- deprecated
    , ("VARCHAR2", [t|String|])
    , ("NCHAR", [t|String|])
    , ("NVARCHAR2", [t|String|])
    -- , ("NUMBER", [t|Integer or Double|]) see 'getType'
    , ("BINARY_FLOAT", [t|Double|]) -- Float don't work
    , ("BINARY_DOUBLE", [t|Double|])
    , ("DATE", [t|LocalTime|])
    , ("BLOB", [t|ByteString|])
    , ("CLOB", [t|String|])
    , ("NCLOB", [t|String|])
    , ("LONG RAW", [t|ByteString|]) -- deprecated
    , ("RAW", [t|ByteString|])
    , ("ROWID", [t|String|])
    , ("UROWID", [t|String|])
    ]

-- | Normalize column name string to query Oracle DB data dictionary.
normalizeColumn :: String -> String
normalizeColumn = map toLower

-- | Not-null attribute information of column.
notNull :: DbaTabColumns -> Bool
notNull = (== Just "N") . Cols.nullable

-- | Get column normalized name and column Haskell type.
getType :: Map String TypeQ -- ^ Type mapping specified by user
        -> DbaTabColumns -- ^ Column info in data dictionary
        -> Maybe (String, TypeQ) -- ^ Result normalized name and mapped Haskell type
getType mapFromSql cols = do
    ky  <- Cols.dataType cols
    typ <- if ky == "NUMBER"
        then return $ numberType $ Cols.dataScale cols
        else Map.lookup ky mapFromSql <|> Map.lookup ky mapFromSqlDefault
    return (normalizeColumn $ Cols.columnName cols, mayNull typ)
  where
    mayNull typ
        | notNull cols = typ
        | otherwise = [t|Maybe $(typ)|]
    numberType Nothing = [t|Integer|]
    numberType (Just n)
        | n <= 0 = [t|Integer|]
        | otherwise = [t|Double|]

-- | 'Relation' to query 'DbaTabColumns' from owner name and table name.
columnsRelationFromTable :: Relation (String, String) DbaTabColumns
columnsRelationFromTable = relation' $ \ph -> do
    cols <- query dbaTabColumns
    wheres $ cols ! Cols.owner' .=. toFlat (ph ! fst')
    wheres $ cols ! Cols.tableName' .=. toFlat (ph ! snd')
    asc $ cols ! Cols.columnId'
    return cols

-- | Phantom typed 'Query' to get 'DbaTabColumns' from owner name and table name.
columnsQuerySQL :: Query (String, String) DbaTabColumns
columnsQuerySQL = relationalQuery columnsRelationFromTable

-- | 'Relation' to query primary key name from owner name and table name.
primaryKeyRelation :: Relation (String, String) (Maybe String)
primaryKeyRelation = relation' $ \ph -> do
    cons <- query dbaConstraints
    cols <- query dbaTabColumns
    consCols <- query dbaConsColumns

    wheres $ cons ! Cons.owner' .=. just (cols ! Cols.owner')
    wheres $ cons ! Cons.tableName' .=. cols ! Cols.tableName'
    wheres $ consCols ! ConsCols.columnName' .=. just (cols ! Cols.columnName')
    wheres $ cons ! Cons.constraintName' .=. consCols ! ConsCols.constraintName'

    wheres $ cols ! Cols.nullable' .=. just (toFlat $ value "N")
    wheres $ cons ! Cons.constraintType' .=. just (toFlat $ value "P")

    wheres $ cons ! Cons.owner' .=. just (toFlat (ph ! fst'))
    wheres $ cons ! Cons.tableName' .=. toFlat (ph ! snd')

    asc $ consCols ! ConsCols.position'

    return (consCols ! ConsCols.columnName')

-- | Phantom typed 'Query' to get primary key name from owner name and table name.
primaryKeyQuerySQL :: Query (String, String) (Maybe String)
primaryKeyQuerySQL = relationalQuery primaryKeyRelation
