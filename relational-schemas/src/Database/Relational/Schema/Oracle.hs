{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Database.Relational.Schema.Oracle
    ( normalizeColumn, notNull, getType
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

import Database.Relational.ReboundSyntax hiding ((>>=), (>>))
import qualified Database.Relational.ReboundSyntax as ReboundSyntax
import Database.Relational.ExtensibleRecord (ExRecord, type (>:))

import Database.Relational.Schema.OracleDataDictionary.ConsColumns (dbaConsColumns)
import qualified Database.Relational.Schema.OracleDataDictionary.ConsColumns as ConsCols
import Database.Relational.Schema.OracleDataDictionary.Constraints (dbaConstraints)
import qualified Database.Relational.Schema.OracleDataDictionary.Constraints as Cons
import Database.Relational.Schema.OracleDataDictionary.TabColumns (DbaTabColumns, dbaTabColumns)
import qualified Database.Relational.Schema.OracleDataDictionary.TabColumns as Cols

import Prelude hiding ((>>=), (>>))
import qualified Prelude

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

    (>>=) :: Monad m => m a -> (a -> m b) -> m b
    (>>=) = (Prelude.>>=)


type OwnerAndTable = ExRecord
 '[ "owner" >: String
  , "table" >: String
  ]


-- | 'Relation' to query 'DbaTabColumns' from owner name and table name.
columnsRelationFromTable :: Relation (ExRecord '[]) OwnerAndTable () DbaTabColumns
columnsRelationFromTable = relation $ do
    cols <- query dbaTabColumns
    wheres $ cols ! Cols.owner' .=. ph #owner
    wheres $ cols ! Cols.tableName' .=. ph #table
    asc $ cols ! Cols.columnId'
    ireturn cols
  where
    (>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
    (>>=) = (ReboundSyntax.>>=)
    (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
    (>>) = (ReboundSyntax.>>)

-- | Phantom typed 'Query' to get 'DbaTabColumns' from owner name and table name.
columnsQuerySQL :: Query () DbaTabColumns
columnsQuerySQL = relationalQuery columnsRelationFromTable

-- | 'Relation' to query primary key name from owner name and table name.
primaryKeyRelation :: Relation (ExRecord '[]) OwnerAndTable () (Maybe String)
primaryKeyRelation = relation $ do
    cons <- query dbaConstraints
    cols <- query dbaTabColumns
    consCols <- query dbaConsColumns

    wheres $ cons ! Cons.owner' .=. just (cols ! Cols.owner')
    wheres $ cons ! Cons.tableName' .=. cols ! Cols.tableName'
    wheres $ consCols ! ConsCols.columnName' .=. just (cols ! Cols.columnName')
    wheres $ cons ! Cons.constraintName' .=. consCols ! ConsCols.constraintName'

    wheres $ cols ! Cols.nullable' .=. just (value "N")
    wheres $ cons ! Cons.constraintType' .=. just (value "P")

    wheres $ cons ! Cons.owner' .=. just (ph #owner)
    wheres $ cons ! Cons.tableName' .=. ph #table

    asc $ consCols ! ConsCols.position'

    ireturn (consCols ! ConsCols.columnName')

  where
    (>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
    (>>=) = (ReboundSyntax.>>=)
    (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
    (>>) = (ReboundSyntax.>>)

-- | Phantom typed 'Query' to get primary key name from owner name and table name.
primaryKeyQuerySQL :: Query () (Maybe String)
primaryKeyQuerySQL = relationalQuery primaryKeyRelation
