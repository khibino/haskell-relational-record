{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Relational.Query.TH (
  defineHasPrimaryKeyInstance,
  defineHasPrimaryKeyInstanceDefault,
  defineHasNotNullKeyInstance,
  defineHasNotNullKeyInstanceDefault,

  defineColumn, defineColumnDefault,

  defineTableTypes, defineTableTypesDefault,

  defineTableTypesAndRecordDefault,

  definePrimaryQuery,
  definePrimaryUpdate,
  defineInsert,

  tableVarExpDefault,
  relationVarExpDefault,

  defineSqlsWithPrimaryKey,
  defineSqls,
  defineSqlsWithPrimaryKeyDefault,
  defineSqlsDefault,

  defineTableDefault',
  defineTableDefault,

  inlineQuery
  ) where

import Data.Char (toUpper, toLower)

import Language.Haskell.TH
  (Q, reify, Info (VarI), TypeQ, Type (AppT, ConT), ExpQ,
   Dec, stringE, listE)
import Language.Haskell.TH.Name.CamelCase
  (VarName, varName, ConName, varNameWithPrefix, varCamelcaseName, toVarExp)
import Language.Haskell.TH.Lib.Extra
  (compileError, simpleValD, maybeD, integralE)

import Database.Record.TH
  (recordTypeDefault,
   defineRecordTypeDefault,
   defineHasKeyConstraintInstance)

import Database.Relational.Query
  (Table, Pi, Relation, PrimeRelation,
   toSQL, Query, fromRelation, Update, Insert, typedInsert,
   HasConstraintKey(constraintKey), projectionKey, Primary, NotNull)
import qualified Database.Relational.Query as Query

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

defineTableTypes :: VarName                          -- ^ Table declaration variable name
                 -> VarName                          -- ^ Relation declaration variable name
                 -> TypeQ                            -- ^ Record type
                 -> String                           -- ^ Table name in SQL ex. FOO_SCHEMA.table0
                 -> [((String, TypeQ), Maybe TypeQ)] -- ^ Column names and types and constraint type
                 -> Q [Dec]                          -- ^ Table and Relation declaration
defineTableTypes tableVar' relVar' recordType table columns = do
  let tableVar = varName tableVar'
  tableDs <- simpleValD tableVar [t| Table $(recordType) |]
            [| Table.table $(stringE table) $(listE $ map stringE (map (fst . fst) columns)) |]
  let relVar   = varName relVar'
  relDs   <- simpleValD relVar   [t| Relation $(recordType) |]
             [| Query.table $(toVarExp tableVar') |]
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

defineTableTypesDefault :: String                           -- ^ Schema name
                        -> String                           -- ^ Table name
                        -> [((String, TypeQ), Maybe TypeQ)] -- ^ Column names and types and constraint type
                        -> Q [Dec]                          -- ^ Result declarations
defineTableTypesDefault schema table columns = do
  let recordType = recordTypeDefault table
  tableDs <- defineTableTypes
             (tableVarNameDefault table)
             (relationVarNameDefault table)
             recordType
             (tableSQL schema table)
             columns
  let defCol i ((name, typ), constraint) = defineColumnDefault constraint recordType name i typ
  colsDs  <- fmap concat . sequence . zipWith defCol [0..] $ columns
  return $ tableDs ++ colsDs

defineTableTypesAndRecordDefault :: String            -- ^ Schema name
                                 -> String            -- ^ Table name
                                 -> [(String, TypeQ)] -- ^ Column names and types
                                 -> [ConName]         -- ^ Record derivings
                                 -> Q [Dec]           -- ^ Result declarations
defineTableTypesAndRecordDefault schema table columns drives = do
  recD    <- defineRecordTypeDefault table columns drives
  tableDs <- defineTableTypesDefault schema table [(c, Nothing) | c <- columns ]
  return $ recD : tableDs

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

defineSqlsWithPrimaryKey :: VarName -- ^ Variable name of select query definition from primary key
                         -> VarName -- ^ Variable name of update statement definition from primary key
                         -> TypeQ   -- ^ Primary key type
                         -> TypeQ   -- ^ Record type
                         -> ExpQ    -- ^ Relation expression
                         -> ExpQ    -- ^ Table expression
                         -> Q [Dec] -- ^ Result declarations
defineSqlsWithPrimaryKey sel upd paramType recType relE tableE = do
  selD <- definePrimaryQuery  sel paramType recType relE
  updD <- definePrimaryUpdate upd paramType recType tableE
  return $ selD ++ updD

defineSqls :: VarName -> TypeQ -> ExpQ -> Q [Dec]
defineSqls =  defineInsert

defineSqlsWithPrimaryKeyDefault :: String  -- ^ Table name of Database
                                -> TypeQ   -- ^ Primary key type
                                -> TypeQ   -- ^ Record type
                                -> ExpQ    -- ^ Relation expression
                                -> ExpQ    -- ^ Table expression
                                -> Q [Dec] -- ^ Result declarations
defineSqlsWithPrimaryKeyDefault table  =
  defineSqlsWithPrimaryKey sel upd
  where
    sel = table `varNameWithPrefix` "select"
    upd = table `varNameWithPrefix` "update"

defineSqlsDefault :: String -> TypeQ -> ExpQ -> Q [Dec]
defineSqlsDefault table =
  defineSqls
    (table `varNameWithPrefix` "insert")

defineTableDefault' :: String            -- ^ Schema name of Database
                    -> String            -- ^ Table name of Database
                    -> [(String, TypeQ)] -- ^ Column names and types
                    -> [ConName]         -- ^ derivings for Record type
                    -> Q [Dec]           -- ^ Result declarations
defineTableDefault' schema table fields derives = do
  recD <- defineTableTypesAndRecordDefault schema table fields derives
  let recType = recordTypeDefault table
      tableE  = tableVarExpDefault table
  sqlD <- defineSqlsDefault table recType tableE
  return $ recD ++ sqlD

defineWithPrimaryKeyDefault :: String -> TypeQ -> Int -> Q [Dec]
defineWithPrimaryKeyDefault table keyType idx = do
  instD <- defineHasPrimaryKeyInstanceDefault table keyType idx
  let recType  = recordTypeDefault table
      tableE   = tableVarExpDefault table
      relE     = relationVarExpDefault table
  sqlsD <- defineSqlsWithPrimaryKeyDefault table keyType recType relE tableE
  return $ instD ++ sqlsD

defineWithNotNullKeyDefault :: String -> TypeQ -> Int -> Q [Dec]
defineWithNotNullKeyDefault =  defineHasNotNullKeyInstanceDefault

defineTableDefault :: String            -- ^ Schema name of Database
                   -> String            -- ^ Table name of Database
                   -> [(String, TypeQ)] -- ^ Column names and types
                   -> [ConName]         -- ^ derivings for Record type
                   -> Maybe Int         -- ^ Primary key index
                   -> Maybe Int         -- ^ Not null key index
                   -> Q [Dec]           -- ^ Result declarations
defineTableDefault schema table fields derives mayPrimaryIdx mayNotNullIdx = do
  let keyType = snd . (fields !!)
  tblD  <- defineTableDefault' schema table fields derives
  primD <- maybeD (\i -> defineWithPrimaryKeyDefault table (keyType i) i) mayPrimaryIdx
  nnD   <- maybeD (\i -> defineWithNotNullKeyDefault table (keyType i) i) mayNotNullIdx
  return $ tblD ++ primD ++ nnD


inlineQuery :: VarName           -- ^ Top-level variable name which has 'PrimeRelation' type
            -> PrimeRelation p r -- ^ Object which has 'PrimeRelation' type
            -> VarName           -- ^ Variable name for inlined query
            -> Q [Dec]           -- ^ Result declarations
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
