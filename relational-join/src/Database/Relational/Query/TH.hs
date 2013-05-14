{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Relational.Query.TH (
  defineHasPrimaryKeyInstance,
  defineHasPrimaryKeyInstanceDefault,
  defineHasNotNullKeyInstance,
  defineHasNotNullKeyInstanceDefault,

  defineColumn, defineColumnDefault,

  defineTable, defineTableDefault,

  defineRecordAndTableDefault,

  definePrimaryQuery,
  definePrimaryUpdate,
  defineInsert,

  tableVarExpDefault,
  relationVarExpDefault,

  defineSqlsWithPrimaryKey,
  defineSqls,
  defineSqlsWithPrimaryKeyDefault,
  defineSqlsDefault,

  inlineQuery
  ) where

import Data.Char (toUpper, toLower)

import Language.Haskell.TH
  (Q, reify, Info (VarI), TypeQ, Type (AppT, ConT), ExpQ,
   Dec, stringE, listE)
import Language.Haskell.TH.Name.CamelCase
  (VarName, varName, ConName, varNameWithPrefix, varCamelcaseName, toVarExp)
import Language.Haskell.TH.Name.Extra
  (compileError, simpleValD, integralE)

import Database.Record.TH
  (recordTypeDefault, defineRecordDefault, defineHasKeyConstraintInstance)

import Database.Relational.Query
  (Table, Pi, Relation, PrimeRelation, fromTable,
   toSQL, Query, fromRelation, Update, Insert, typedInsert,
   HasConstraintKey(constraintKey), projectionKey, Primary, NotNull)

import Database.Relational.Query.Constraint (Key, defineConstraintKey)
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Type (unsafeTypedQuery)
import qualified Database.Relational.Query.Pi.Unsafe as UnsafePi
import Database.Relational.Query.Derives (primary, primaryUpdate)


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

defineHasPrimaryKeyInstanceDefault :: String  -- ^ Table name
                                   -> TypeQ   -- ^ Column type
                                   -> Int     -- ^ Primary key index
                                   -> Q [Dec] -- ^ Declaration of primary constraint key
defineHasPrimaryKeyInstanceDefault =
  defineHasPrimaryKeyInstance . recordTypeDefault

defineHasNotNullKeyInstance :: TypeQ -> TypeQ -> Int -> Q [Dec]
defineHasNotNullKeyInstance =
  defineHasConstraintKeyInstance [t| NotNull |]

defineHasNotNullKeyInstanceDefault :: String  -- ^ Table name
                                   -> TypeQ   -- ^ Column type
                                   -> Int     -- ^ NotNull key index
                                   -> Q [Dec] -- ^ Declaration of not-null constraint key
defineHasNotNullKeyInstanceDefault =
  defineHasNotNullKeyInstance . recordTypeDefault


defineColumn' :: TypeQ   -- ^ Record type
              -> VarName -- ^ Column declaration variable name
              -> Int     -- ^ Column index in record (begin with 0)
              -> TypeQ   -- ^ Column type
              -> Q [Dec] -- ^ Column declaration
defineColumn' recType var' i colType = do
  let var = varName var'
  simpleValD var [t| Pi $recType $colType |]
    [| UnsafePi.defineColumn $(integralE i) |]

defineColumn :: Maybe (TypeQ, VarName) -- ^ May Constraint type and constraint object name
             -> TypeQ                  -- ^ Record type
             -> VarName                -- ^ Column declaration variable name
             -> Int                    -- ^ Column index in record (begin with 0)
             -> TypeQ                  -- ^ Column type
             -> Q [Dec]                -- ^ Column declaration
defineColumn mayConstraint recType var' i colType = do
  maybe
    (defineColumn' recType var' i colType)
    ( \(constraint, cname') -> do
         let cname = varName cname'
         ck  <- simpleValD cname [t| Key $constraint $recType $colType |]
                [| defineConstraintKey $(integralE i) |]

         col <- simpleValD (varName var') [t| Pi $recType $colType |]
                [| projectionKey $(toVarExp cname') |]
         return $ ck ++ col)
    mayConstraint

defineColumnDefault :: Maybe TypeQ -- ^ May Constraint type
                    -> TypeQ       -- ^ Record type
                    -> String      -- ^ Column name
                    -> Int         -- ^ Column index in record (begin with 0)
                    -> TypeQ       -- ^ Column type
                    -> Q [Dec]     -- ^ Column declaration
defineColumnDefault mayConstraint recType name =
  defineColumn (fmap withCName mayConstraint) recType varN
  where varN        = varCamelcaseName (name ++ "'")
        withCName t = (t, varCamelcaseName (name ++ "_constraint"))

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
             [| fromTable $(toVarExp tableVar') |]
  return $ tableDs ++ relDs

tableSQL :: String -> String -> String
tableSQL schema table = map toUpper schema ++ '.' : map toLower table

tableVarNameDefault :: String -> VarName
tableVarNameDefault =  (`varNameWithPrefix` "tableOf")

tableVarExpDefault :: String -> ExpQ
tableVarExpDefault =  toVarExp . tableVarNameDefault

relationVarNameDefault :: String -> VarName
relationVarNameDefault =  varCamelcaseName

relationVarExpDefault :: String -> ExpQ
relationVarExpDefault =  toVarExp . relationVarNameDefault

defineTableDefault :: String                           -- ^ Schema name
                   -> String                           -- ^ Table name
                   -> [((String, TypeQ), Maybe TypeQ)] -- ^ Column names and types and constraint type
                   -> Q [Dec]                          -- ^ Result declarations
defineTableDefault schema table columns = do
  let recordType = recordTypeDefault table
  tableDs <- defineTable
             (tableVarNameDefault table)
             (relationVarNameDefault table)
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


definePrimaryQuery :: VarName -> TypeQ -> TypeQ -> ExpQ -> Q [Dec]
definePrimaryQuery toDef' paramType recType relE = do
  let toDef = varName toDef'
  simpleValD toDef
    [t| Query $paramType $recType |]
    [|  fromRelation (primary $relE) |]

definePrimaryUpdate :: VarName -> TypeQ -> TypeQ -> ExpQ -> Q [Dec]
definePrimaryUpdate toDef' paramType recType tableE = do
  let toDef = varName toDef'
  simpleValD toDef
    [t| Update $paramType $recType |]
    [|  primaryUpdate $tableE |]


defineInsert :: VarName -> TypeQ -> ExpQ -> Q [Dec]
defineInsert toDef' recType tableE = do
  let toDef = varName toDef'
  simpleValD toDef
    [t| Insert $recType |]
    [|  typedInsert $tableE |]

defineSqlsWithPrimaryKey :: VarName
                         -> VarName
                         -> TypeQ
                         -> TypeQ
                         -> ExpQ
                         -> ExpQ
                         -> Q [Dec]
defineSqlsWithPrimaryKey sel upd paramType recType relE tableE = do
  selD <- definePrimaryQuery  sel paramType recType relE
  updD <- definePrimaryUpdate upd paramType recType tableE
  return $ selD ++ updD

defineSqls :: VarName -> TypeQ -> ExpQ -> Q [Dec]
defineSqls =  defineInsert

defineSqlsWithPrimaryKeyDefault :: String
                                -> TypeQ
                                -> TypeQ
                                -> ExpQ
                                -> ExpQ
                                -> Q [Dec]
defineSqlsWithPrimaryKeyDefault table  =
  defineSqlsWithPrimaryKey sel upd
  where
    sel = table `varNameWithPrefix` "select"
    upd = table `varNameWithPrefix` "update"

defineSqlsDefault :: String -> TypeQ -> ExpQ -> Q [Dec]
defineSqlsDefault table =
  defineSqls
    (table `varNameWithPrefix` "insert")


inlineQuery :: VarName -> PrimeRelation p r -> VarName -> Q [Dec]
inlineQuery relVar' rel qVar' =  do
  let relVar = varName relVar'
      qVar   = varName qVar'
  relInfo <- reify relVar
  case relInfo of
    VarI _ (AppT (AppT (ConT prn) p) r) _ _
      | prn == ''PrimeRelation    -> do
        simpleValD qVar
          [t| Query $(return p) $(return r) |]
          [| unsafeTypedQuery $(stringE . toSQL $ rel) |]
    _                             ->
      compileError $ "expandRelation: Variable must have PrimeRelation type: " ++ show relVar
