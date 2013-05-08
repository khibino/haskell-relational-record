{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.TH
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains templates to generate Haskell record types
-- and instances correspond to RDB table schema.
module Database.HDBC.TH (
  fieldInfo,

  defineRecordConstructFunction,
  definePersistableInstance,
  defineRecordDecomposeFunction,

  defineRecord,
  defineRecordDefault,

  defineConstantSql,
  defineSqlPrimarySelect,
  defineSqlPrimaryUpdate,
  defineSqlInsert,

  defineSqls, defineSqlsDefault,

  defineWithTableDefault,
  defineWithPrimaryKeyDefault,
  defineWithNotNullKeyDefault,

  defineTableFromDB
  ) where

import Data.Char (toUpper, toLower)
import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex)

import Database.HDBC (IConnection, SqlValue)

import Language.Haskell.TH.Name.CamelCase
  (ConName, VarName (varName),
   varCamelcaseName,
   varNameWithPrefix)
import Language.Haskell.TH.Name.Extra
  (integralE, simpleValD, compileError)
import Language.Haskell.TH
  (Q, runIO,
   TypeQ, Dec,
   listE, stringE,
   varP,
   sigD, valD,
   normalB)

import Database.HDBC.Session (withConnectionIO)
import Database.Record.TH
  (recordTypeDefault,
   defineHasPrimaryKeyInstanceDefault, defineHasNotNullKeyInstanceDefault,
   defineRecordConstructFunction, defineRecordDecomposeFunction,
   definePersistableInstance)
import qualified Database.Record.TH as Record
import Database.Relational.Query.Type (unsafeTypedQuery)
import Database.Relational.Query (Query)
import Database.HDBC.Record.Persistable ()
import Language.SQL.Keyword (Keyword(..), (.=.))
import qualified Language.SQL.Keyword as SQL

import Database.HDBC.Schema.Driver (Driver, getFields, getPrimaryKey)


nameOfTableSQL :: String -> String -> String
nameOfTableSQL schema table = map toUpper schema ++ '.' : map toLower table


fieldInfo :: String
          -> TypeQ
          -> ((VarName, TypeQ), String) -- ^ (fieldVarName, (fieldInSQL, fieldTypeInTable))
fieldInfo n t = ((varCamelcaseName n, t), n)


mayDeclare :: (a -> Q [Dec]) -> Maybe a -> Q [Dec]
mayDeclare =  maybe (return [])

defineTableInfo :: VarName -> String
                -> VarName -> [String]
                -> VarName -> Int
                -> Q [Dec]
defineTableInfo tableVar' table fieldsVar' fields widthVar' width = do
  let tableVar = varName tableVar'
      fieldsVar = varName fieldsVar'
      widthVar = varName widthVar'
  tableQ  <- simpleValD tableVar  [t| String |]   [| $(stringE table) |]
  fieldsQ <- simpleValD fieldsVar [t| [String] |] [| $(listE $ map stringE fields) |]
  widthQ  <- simpleValD widthVar  [t| Int |]      [| $(integralE $ width) |]
  return $ concat [tableQ, fieldsQ, widthQ]

defineRecord :: (VarName, VarName)
            -> (String, ConName)
            -> (VarName, VarName, VarName)
            -> [((VarName, TypeQ), String)]
            -> [ConName]
            -> Q [Dec]
defineRecord
  (cF, dF) (tableSQL, tyC)
  (tableN, fldsN, widthN)
  schemas drvs = do

  prec <- Record.defineRecord [t| SqlValue |] (cF, dF) tyC (map fst schemas) drvs
  tableI   <- defineTableInfo
              tableN tableSQL
              fldsN (map snd schemas) widthN (length schemas)
  return $ prec ++ tableI

defineRecordDefault :: String
                    -> String
                    -> [(String, TypeQ)]
                    -> [ConName]
                    -> Q [Dec]
defineRecordDefault schema table fields derives = do
  prec <- Record.defineRecordDefault [t| SqlValue |] table fields derives
  let tableSQL = nameOfTableSQL schema table
  tableI   <- defineTableInfo
              (table `varNameWithPrefix` "tableOf") tableSQL
              (table `varNameWithPrefix` "fieldsOf")
              (map fst fields)
              (table `varNameWithPrefix` "widthOf")
              (length fields)
  return $ prec ++ tableI

defineConstantSql :: VarName -> String -> Q [Dec]
defineConstantSql name' sqlStr = do
  let name = varName name'
  sig <- sigD name [t| String |]
  var <- valD (varP name)
         (normalB . stringE $ sqlStr)
         []
  return [sig, var]

defineConstantSqlQuery :: TypeQ -> TypeQ -> VarName -> String -> Q [Dec]
defineConstantSqlQuery pkeyType recordType name' sqlStr = do
  let name = varName name'
  sig <- sigD name [t| Query $pkeyType $recordType |]
  var <- valD (varP name)
         (normalB [| unsafeTypedQuery $(stringE $ sqlStr) |])
         []
  return [sig, var]

defineSqlPrimarySelect :: VarName -> (String, TypeQ)-> [(String, TypeQ)] -> String -> Q [Dec]
defineSqlPrimarySelect name' (table, recordType) fields pkey =
  defineConstantSqlQuery pkeyType recordType name'
  . SQL.unwordsSQL
  $ [SELECT, fields' `SQL.sepBy` ", ",
     FROM, SQL.word table, WHERE, SQL.word pkey .=. "?"]
  where fields' = map (SQL.word . fst) fields
        pkeyType = fromJust $ lookup pkey fields

defineSqlPrimaryUpdate :: VarName -> String -> [String] -> String -> Q [Dec]
defineSqlPrimaryUpdate name' table fields pkey =
  defineConstantSql name'
  . SQL.unwordsSQL
  $ [UPDATE, SQL.word table, SET, assignments `SQL.sepBy` ", ",
     WHERE, SQL.word pkey, "= ?"]
  where assignments = map (\f -> SQL.word f .=. "?") . filter (/= pkey) $ fields

defineSqlInsert :: VarName -> String -> [String] -> Q [Dec]
defineSqlInsert name' table fields = do
  defineConstantSql name'
  . SQL.unwordsSQL
  $ [INSERT, INTO, SQL.word table, fields' `SQL.parenSepBy` ", ",
     VALUES, pfs `SQL.parenSepBy` ", "]
    where fields' = map SQL.word fields
          pfs     = replicate (length fields) "?"

defineSqls :: VarName -- ^ SQL insert statement var name
               -> (String, TypeQ)
               -> [(String, TypeQ)]
               -> Q [Dec] -- ^ SQL statement String declarations
defineSqls ins (table, _recordType) fields =
  defineSqlInsert ins table (map fst fields)

defineSqlsWithPrimaryKey :: Int               -- ^ Primary key field index
                         -> VarName           -- ^ SQL select statement var name
                         -> VarName           -- ^ SQL update statement var name
                         -> (String, TypeQ)   -- ^ Table name String in SQL and record type
                         -> [(String, TypeQ)] -- ^ Field name strings
                         -> Q [Dec]           -- ^ SQL statement String declarations
defineSqlsWithPrimaryKey i sel upd (table, recordType) fields = do
  let width = length fields
      fields' = map fst fields
      getPrimaryKeyName
        | i < 0 || width <= i = compileError
                                $  "defineSqls: Index out of bounds!: "
                                ++ "fields count is " ++ show width ++ ", but index is " ++ show i
        | otherwise           = return . fst $ fields !! i
  keyName <- getPrimaryKeyName
  selD <- defineSqlPrimarySelect sel (table, recordType) fields keyName
  updD <- defineSqlPrimaryUpdate upd table fields' keyName
  return $ selD ++ updD

defineSqlsDefault :: String -> String -> [(String, TypeQ)] -> Q [Dec]
defineSqlsDefault schema table fields =
  defineSqls ins (tableSQL, recordType) fields
  where
    tableSQL = nameOfTableSQL schema table
    recordType = recordTypeDefault table
    ins = table `varNameWithPrefix` "insert"

defineSqlsWithPrimaryKeyDefault :: String -> String -> [(String, TypeQ)] -> Int -> Q [Dec]
defineSqlsWithPrimaryKeyDefault schema table fields idx =
  defineSqlsWithPrimaryKey idx sel upd (tableSQL, recordType) fields
  where
    tableSQL = nameOfTableSQL schema table
    recordType = recordTypeDefault table
    sel = table `varNameWithPrefix` "select"
    upd = table `varNameWithPrefix` "update"

defineWithTableDefault' :: String
                        -> String
                        -> [(String, TypeQ)]
                        -> [ConName]
                        -> Q [Dec]
defineWithTableDefault' schema table fields derives = do
  recD <- defineRecordDefault schema table fields derives
  sqlD <- defineSqlsDefault schema table fields
  return $ recD ++ sqlD

defineWithPrimaryKeyDefault :: String -> String -> [(String, TypeQ)] -> Int -> Q [Dec]
defineWithPrimaryKeyDefault schema table fields idx = do
  instD <- defineHasPrimaryKeyInstanceDefault table idx
  sqlsD <- defineSqlsWithPrimaryKeyDefault schema table fields idx
  return $ instD ++ sqlsD

defineWithNotNullKeyDefault :: String -> Int -> Q [Dec]
defineWithNotNullKeyDefault =  defineHasNotNullKeyInstanceDefault

defineWithTableDefault :: String
                       -> String
                       -> [(String, TypeQ)]
                       -> [ConName]
                       -> Maybe Int
                       -> Maybe Int
                       -> Q [Dec]
defineWithTableDefault schema table fields derives mayPrimaryIdx mayNotNullIdx  = do
  tblD  <- defineWithTableDefault' schema table fields derives
  primD <- mayDeclare (defineWithPrimaryKeyDefault schema table fields) mayPrimaryIdx
  nnD   <- mayDeclare (defineWithNotNullKeyDefault table) mayNotNullIdx
  return $ tblD ++ primD ++ nnD

putLog :: String -> IO ()
putLog =  putStrLn

defineTableFromDB :: IConnection conn
                   => IO conn
                   -> Driver conn
                   -> String
                   -> String 
                   -> [ConName]
                   -> Q [Dec]
defineTableFromDB connect drv scm tbl derives = do
  let getDBinfo =
        withConnectionIO connect
        (\conn ->  do
            (cols, notNullIdxs) <- getFields drv conn scm tbl
            mayPrimaryKey       <- getPrimaryKey drv conn scm tbl

            mayPrimaryIdx <- case mayPrimaryKey of
              Just key -> case elemIndex key $ map fst cols of
                Nothing -> do putLog $ "defineTableFromDB: fail to find index of pkey - " ++ key ++ ". Something wrong!!"
                              return   Nothing
                Just ix ->    return $ Just ix
              Nothing  ->     return   Nothing
            return (cols, notNullIdxs, mayPrimaryIdx) )

  (cols, notNullIdxs, mayPrimaryIdx) <- runIO getDBinfo
  defineWithTableDefault scm tbl cols derives mayPrimaryIdx (listToMaybe notNullIdxs)
