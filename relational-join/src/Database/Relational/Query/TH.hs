{-# LANGUAGE TemplateHaskell #-}

module Database.Relational.Query.TH (
  defineColumn, defineColumnDefault,

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
import Language.Haskell.TH.Name.Extra (compileError, simpleValD, integralE)

import Database.Record.TH (recordTypeDefault, defineRecordDefault)

import Database.Relational.Query.Table (Table)
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Relation (Relation, toSQL, fromTable)
import Database.Relational.Query.Type (Query, unsafeTypedQuery)
import qualified Database.Relational.Query.Pi.Unsafe as UnsafePi
import Database.Relational.Query.Pi (Pi)


tableSQL :: String -> String -> String
tableSQL schema table = map toUpper schema ++ '.' : map toLower table

defineColumn :: TypeQ   -- ^ Record type
             -> VarName -- ^ Column declaration variable name
             -> Int     -- ^ Column index in record (begin with 0)
             -> TypeQ   -- ^ Column type
             -> Q [Dec] -- ^ Column declaration
defineColumn recType var' i colType = do
  let var = varName var'
  simpleValD var [t| Pi $recType $colType |]
    [| UnsafePi.defineColumn $(integralE i) |]

defineColumnDefault :: TypeQ   -- ^ Record type
                    -> String  -- ^ Column name
                    -> Int     -- ^ Column index in record (begin with 0)
                    -> TypeQ   -- ^ Column type
                    -> Q [Dec] -- ^ Column declaration
defineColumnDefault recType name i colType =
  defineColumn recType (varCamelcaseName (name ++ "'")) i colType

defineTable :: VarName           -- ^ Table declaration variable name
            -> VarName           -- ^ Relation declaration variable name
            -> TypeQ             -- ^ Record type
            -> String            -- ^ Table name in SQL ex. FOO_SCHEMA.table0
            -> [(String, TypeQ)] -- ^ Column names and types
            -> Q [Dec]           -- ^ Table and Relation declaration
defineTable tableVar' relVar' recordType table columns = do
  let tableVar = varName tableVar'
  tableDs <- simpleValD tableVar [t| Table $(recordType) |]
            [| Table.table $(stringE table) $(listE $ map stringE (map fst columns)) |]
  let defCol i (name, typ) = defineColumnDefault recordType name i typ
  colsDs  <- fmap concat . sequence . zipWith defCol [0..] $ columns
  let relVar   = varName relVar'
  relDs   <- simpleValD relVar   [t| Relation $(recordType) |]
             [| fromTable $(varE tableVar) |]
  return $ tableDs ++ colsDs ++ relDs

defineTableDefault :: String            -- ^ Schema name
                   -> String            -- ^ Table name
                   -> [(String, TypeQ)] -- ^ Column names and types
                   -> Q [Dec]           -- ^ Result declarations
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
  tableDs <- defineTableDefault schema table columns
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
