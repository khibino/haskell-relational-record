{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import Language.Haskell.TH.Name.Extra
  (compileError, simpleValD, integralE, maybeD)

import Database.Record.TH
  (recordTypeDefault, defineRecordDefault, defineHasKeyConstraintInstance)

import Database.Relational.Query
  (Table, Pi, Relation, PrimeRelation, fromTable, toSQL, Query,
   HasConstraintKey(constraintKey), Primary, NotNull)

import Database.Relational.Query.Constraint (defineConstraintKey, appendConstraint)
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Type (unsafeTypedQuery)
import qualified Database.Relational.Query.Pi.Unsafe as UnsafePi


defineHasConstraintKeyInstance :: TypeQ -> TypeQ -> TypeQ -> Int -> Q [Dec]
defineHasConstraintKeyInstance constraint recType colType index = do
  kc <- defineHasKeyConstraintInstance constraint recType index
  ck <- [d| instance HasConstraintKey $constraint $recType $colType  where
              constraintKey = defineConstraintKey $(integralE index)
          |]
  return $ kc ++ ck

defineHasPrimaryKeyInstance :: TypeQ -> TypeQ -> Int -> Q [Dec]
defineHasPrimaryKeyInstance =
  defineHasConstraintKeyInstance [t| Primary |]

defineHasNotNullKeyInstance :: TypeQ -> TypeQ -> Int -> Q [Dec]
defineHasNotNullKeyInstance =
  defineHasConstraintKeyInstance [t| NotNull |]


defineColumn' :: TypeQ   -- ^ Record type
              -> VarName -- ^ Column declaration variable name
              -> Int     -- ^ Column index in record (begin with 0)
              -> TypeQ   -- ^ Column type
              -> Q [Dec] -- ^ Column declaration
defineColumn' recType var' i colType = do
  let var = varName var'
  simpleValD var [t| Pi $recType $colType |]
    [| UnsafePi.defineColumn $(integralE i) |]

defineColumn :: Maybe TypeQ -- ^ May Constraint type
             -> TypeQ       -- ^ Record type
             -> VarName     -- ^ Column declaration variable name
             -> Int         -- ^ Column index in record (begin with 0)
             -> TypeQ       -- ^ Column type
             -> Q [Dec]     -- ^ Column declaration
defineColumn mayConstraint recType var' i colType = do
  col <- defineColumn' recType var' i colType
  cs  <- maybeD
         (\constraint -> do
             kc <- defineHasKeyConstraintInstance constraint recType i
             ck <- [d| instance HasConstraintKey $constraint $recType $colType  where
                         constraintKey = appendConstraint $(varE $ varName var')
                     |]
             return $ kc ++ ck)
         mayConstraint
  return $ col ++ cs

defineColumnDefault :: Maybe TypeQ -- ^ May Constraint type
                    -> TypeQ       -- ^ Record type
                    -> String      -- ^ Column name
                    -> Int         -- ^ Column index in record (begin with 0)
                    -> TypeQ       -- ^ Column type
                    -> Q [Dec]     -- ^ Column declaration
defineColumnDefault mayConstraint recType name =
  defineColumn mayConstraint recType (varCamelcaseName (name ++ "'"))

defineTable :: VarName                          -- ^ Table declaration variable name
            -> VarName                          -- ^ Relation declaration variable name
            -> TypeQ                            -- ^ Record type
            -> String                           -- ^ Table name in SQL ex. FOO_SCHEMA.table0
            -> [((String, TypeQ), Maybe TypeQ)] -- ^ Column names and types and constraint type
            -> Q [Dec]                          -- ^ Table and Relation declaration
defineTable tableVar' relVar' recordType table columns = do
  let tableVar = varName tableVar'
  tableDs <- simpleValD tableVar [t| Table $(recordType) |]
            [| Table.table $(stringE table) $(listE $ map stringE (map (fst . fst) columns)) |]
  let relVar   = varName relVar'
  relDs   <- simpleValD relVar   [t| Relation $(recordType) |]
             [| fromTable $(varE tableVar) |]
  return $ tableDs ++ relDs

tableSQL :: String -> String -> String
tableSQL schema table = map toUpper schema ++ '.' : map toLower table

defineTableDefault :: String                           -- ^ Schema name
                   -> String                           -- ^ Table name
                   -> [((String, TypeQ), Maybe TypeQ)] -- ^ Column names and types and constraint type
                   -> Q [Dec]                          -- ^ Result declarations
defineTableDefault schema table columns = do
  let recordType = recordTypeDefault table
  tableDs <- defineTable
             (table `varNameWithPrefix` "tableOf")
             (varCamelcaseName table)
             recordType
             (tableSQL schema table)
             columns
  let defCol i ((name, typ), constraint) = defineColumnDefault constraint recordType name i typ
  colsDs  <- fmap concat . sequence . zipWith defCol [0..] $ columns
  return $ tableDs ++ colsDs

defineRecordAndTableDefault :: TypeQ             -- ^ SQL value type
                            -> String            -- ^ Schema name
                            -> String            -- ^ Table name
                            -> [(String, TypeQ)] -- ^ Column names and types
                            -> [ConName]         -- ^ Record derivings
                            -> Q [Dec]           -- ^ Result declarations
defineRecordAndTableDefault sqlValueType schema table columns drives = do
  recDs   <- defineRecordDefault sqlValueType table columns drives
  tableDs <- defineTableDefault schema table [(c, Nothing) | c <- columns ]
  return $ recDs ++ tableDs


inlineQuery :: VarName -> PrimeRelation p r -> VarName -> Q [Dec]
inlineQuery relVar' rel qVar' =  do
  let relVar = varName relVar'
      qVar   = varName qVar'
  relInfo <- reify relVar
  case relInfo of
    VarI _ (AppT (AppT (ConT prn) p) r) _ _
      | prn == ''PrimeRelation    -> do
      sig <- sigD qVar [t| Query $(return p) $(return r) |]
      var <- valD (varP qVar)
             (normalB [| unsafeTypedQuery $(stringE . toSQL $ rel) |])
             []
      return [sig, var]
    _                             ->
      compileError $ "expandRelation: Variable must have PrimeRelation type: " ++ show relVar
