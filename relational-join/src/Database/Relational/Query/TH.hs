{-# LANGUAGE TemplateHaskell #-}

module Database.Relational.Query.TH (
  defineTable, defineTableDefault,
  defineRecordAndTableDefault,

  inlineQuery
  ) where

import Data.Char (toUpper, toLower)

import Language.Haskell.TH
  (Q, reify, Info (VarI), TypeQ, Type (AppT, ConT),
   Dec, sigD, valD, varP, normalB, stringE, listE, varE)
import Language.Haskell.TH.Name.CamelCase
  (VarName, varName, ConName, varNameWithPrefix, varCamelcaseName)
import Language.Haskell.TH.Name.Extra (compileError, simpleValD)

import Database.Record.TH (recordTypeDefault, defineRecordDefault)

import Database.Relational.Query.Table (Table)
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Relation (Relation, toSQL, fromTable)
import Database.Relational.Query.Type (Query, unsafeTypedQuery)


tableSQL :: String -> String -> String
tableSQL schema table = map toUpper schema ++ '.' : map toLower table

defineTable :: VarName  -- ^ Table declaration variable name
            -> VarName  -- ^ Relation declaration variable name
            -> TypeQ    -- ^ Record type
            -> String   -- ^ Table name in SQL ex. FOO_SCHEMA.table0
            -> [String] -- ^ Column names
            -> Q [Dec]  -- ^ Table and Relation declaration
defineTable tableVar' relVar' recordType table columns = do
  let tableVar = varName tableVar'
  tableV <- simpleValD tableVar [t| Table $(recordType) |]
            [| Table.table $(stringE table) $(listE $ map stringE columns) |]
  let relVar   = varName relVar'
  relV   <- simpleValD relVar   [t| Relation $(recordType) |]
            [| fromTable $(varE tableVar) |]
  return $ tableV ++ relV

defineTableDefault :: String   -- ^ Schema name
                   -> String   -- ^ Table name
                   -> [String] -- ^ Column names
                   -> Q [Dec]  -- ^ Result declarations
defineTableDefault schema table columns =
  defineTable
  (table `varNameWithPrefix` "tableOf")
  (varCamelcaseName table)
  (recordTypeDefault table)
  (tableSQL schema table)
  columns

defineRecordAndTableDefault :: TypeQ             -- ^ SQL value type
                            -> String            -- ^ Schema name
                            -> String            -- ^ Table name
                            -> [(String, TypeQ)] -- ^ Column names and types
                            -> [ConName]         -- ^ Record derivings
                            -> Q [Dec]           -- ^ Result declarations
defineRecordAndTableDefault sqlValueType schema table columns drives = do
  recDs   <- defineRecordDefault sqlValueType table columns drives
  tableDs <- defineTableDefault schema table (map fst columns)
  return $ recDs ++ tableDs

inlineQuery :: VarName -> Relation r -> VarName -> TypeQ -> Q [Dec]
inlineQuery relVar' rel qVar' paramType =  do
  let relVar = varName relVar'
      qVar   = varName qVar'
  relInfo <- reify relVar
  case relInfo of
    VarI _ (AppT (ConT rn) r) _ _ | rn == ''Relation ->  do
      sig <- sigD qVar [t| Query $(paramType) $(return r) |]
      var <- valD (varP qVar)
             (normalB [| unsafeTypedQuery $(stringE . toSQL $ rel) |])
             []
      return [sig, var]
    _                             ->
      compileError $ "expandRelation: Variable must have Relation type: " ++ show relVar
