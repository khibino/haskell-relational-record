{-# LANGUAGE TemplateHaskell #-}
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
module Database.HDBC.TH (
  ConName(conName), toConName,
  VarName(varName), toVarName,

  conCamelcaseName,
  varCamelcaseName,

  pprQ,

  fieldInfo,

  derivingEq, derivingShow, derivingRead, derivingData, derivingTypable,
  
  compileErrorIO, compileError,

  defineRecordType,
  defineRecordConstructFunction,
  definePersistableInstance,
  defineRecordDecomposeFunction,

  defineTable,
  defineTableDefault',

  defineConstantSql,
  defineSqlPrimarySelect,
  defineSqlPrimaryUpdate,
  defineSqlInsert,

  defineSqls, defineSqlsDefault,

  defineTableDefault,

  defineTableFromDB
  ) where

import Data.Char (toUpper, toLower)
import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex)

import Database.HDBC (IConnection, SqlValue, fromSql, toSql)

import Language.Haskell.TH
  (Q, Name, mkName, runQ, runIO, Ppr, ppr,
   TypeQ, ExpQ, DecQ, Dec,
   appsE, conE, varE, listE, litE, stringE, integerL,
   listP, varP, wildP,
   conT,
   dataD, sigD, funD, valD,
   clause, normalB,
   recC, cxt, varStrictType, strictType, isStrict)
import qualified Language.Haskell.TH.PprLib as TH
import qualified Language.Haskell.TH.Syntax as TH

import Database.HDBC.Session (withConnectionIO)
import Database.HDBC.Record.Persistable
  (persistableRecord, Persistable, persistable, Singleton)
import Database.HDBC.Record.KeyConstraint
  (HasKeyConstraint(constraintKey), specifyKeyConstraint, Primary, NotNull)
import Database.HDBC.Record.FromSql (FromSql(recordFromSql), recordFromSql')
import Database.HDBC.Record.ToSql (ToSql(recordToSql), recordToSql')
import Database.HDBC.Record.Query (Query, typedQuery)
import Language.SQL.SqlWord (SqlWord(..), (<=>))
import qualified Language.SQL.SqlWord as SQL

import Database.HDBC.Schema.Driver (Driver, getFields, getPrimaryKey)


capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize ""     = ""

unCapitalize :: String -> String
unCapitalize (c:cs) = toLower c : cs
unCapitalize ""     = ""

newtype ConName = ConName { conName :: Name }
newtype VarName = VarName { varName :: Name }

toConName :: String -> ConName
toConName =  ConName . mkName . capitalize

toVarName :: String -> VarName
toVarName =  VarName . mkName . unCapitalize

nameChars :: String
nameChars =  '\'' : ['0' .. '9'] ++ ['A' .. 'Z'] ++  ['a' .. 'z']

splitForName :: String -> [String]
splitForName str
  | rest /= [] = tk : splitForName (tail rest)
  | otherwise  = [tk]
  where
    (tk, rest) = span (`elem` nameChars) str

camelcaseUpper :: String -> String
camelcaseUpper =  concat . map capitalize . splitForName

-- camelcaseLower :: String -> String
-- camelcaseLower =  unCapitalize . camelcaseUpper

conCamelcaseName :: String -> ConName
conCamelcaseName =  toConName . camelcaseUpper

varCamelcaseName :: String -> VarName
varCamelcaseName =  toVarName . camelcaseUpper

varNameWithPrefix :: String -> String -> VarName
varNameWithPrefix n p =  toVarName $ p ++ camelcaseUpper n

recordTypeDefault :: String -> TypeQ
recordTypeDefault =  conT . conName . conCamelcaseName


pprQ :: (Functor m, TH.Quasi m, Ppr a) => Q a -> m TH.Doc
pprQ =  fmap ppr . runQ

fieldInfo :: String
          -> TypeQ
          -> ((VarName, TypeQ), String) -- ^ (fieldVarName, (fieldInSQL, fieldTypeInTable))
fieldInfo n t = ((varCamelcaseName n, t), n)


derivingEq   = conCamelcaseName "Eq"
derivingShow = conCamelcaseName "Show"
derivingRead = conCamelcaseName "Read"
derivingData = conCamelcaseName "Data"
derivingTypable = conCamelcaseName "Typable"
derivingEq, derivingShow, derivingRead, derivingData, derivingTypable :: ConName

compileErrorIO :: String -> IO a
compileErrorIO =  ioError . userError

compileError :: String -> Q a
compileError =  runIO . compileErrorIO

mayDeclare :: (a -> Q [Dec]) -> Maybe a -> Q [Dec]
mayDeclare =  maybe (return [])

integralE :: Integral a => a -> ExpQ
integralE =  litE . integerL . toInteger

defineRecordType :: ConName            -- ^ Name of the data type of table record type.
                 -> [(VarName, TypeQ)] -- ^ List of fields in the table. Must be legal, properly cased record fields.
                 -> [ConName]          -- ^ Deriving type class names.
                 -> DecQ               -- ^ The data type record declaration.
defineRecordType typeName' fields derives = do
  let typeName = conName typeName'
  dataD (cxt []) typeName [] [recC typeName (map fld fields)] (map conName derives)
  where
    fld (n, tq) = varStrictType (varName n) (strictType isStrict tq)

defineRecordConstructFunction :: VarName   -- ^ Name of record construct function.
                              -> ConName   -- ^ Name of record type.
                              -> Int       -- ^ Count of record fields.
                              -> Q [Dec]   -- ^ Declaration of record construct function from SqlValues.
defineRecordConstructFunction funName' typeName' width = do
  let funName = varName funName'
      typeName = conName typeName'
      names = map (mkName . ('f':) . show) [1 .. width]
      fromSqlE n = [| fromSql $(varE n) |]
  sig <- sigD funName [t| [SqlValue] -> $(conT typeName) |]
  var <- funD funName
         [ clause
           [listP (map varP names)]
            (normalB . appsE $ conE typeName : map fromSqlE names)
            [],
            clause [wildP]
            (normalB
             [| error
                $(stringE
                  $ "Generated code of 'defineRecordConstructFunction': Fail to pattern match in: "
                  ++ show funName
                  ++ ", count of fields is " ++ show width) |])
            [] ]
  return [sig, var]

simpleValD :: Name -> TypeQ -> ExpQ -> Q [Dec]
simpleValD var typ expr =  do
  sig <- sigD var typ
  val <- valD (varP var) (normalB expr) []
  return [sig, val]

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

definePersistableInstance :: VarName -> ConName -> VarName -> VarName -> Int -> Q [Dec]
definePersistableInstance widthVar' typeName' consFunName' decompFunName' width = do
  let typeCon = conT $ conName typeName'
  [d| instance Persistable $typeCon where
        persistable = persistableRecord
                      $(varE $ varName consFunName')
                      $(varE $ varName decompFunName')
                      $(varE $ varName widthVar')

      instance FromSql $typeCon where
        recordFromSql = recordFromSql'

      instance ToSql $typeCon where
        recordToSql = recordToSql'
    |]

defineHasKeyConstraintInstance :: TypeQ -> ConName -> Int -> Q [Dec]
defineHasKeyConstraintInstance constraint typeName' index =
  [d| instance HasKeyConstraint $constraint $(conT $ conName typeName') where
        constraintKey = specifyKeyConstraint $(integralE index) |]

defineHasNotNullKeyInstance :: ConName -> Int -> Q [Dec]
defineHasNotNullKeyInstance =
  defineHasKeyConstraintInstance [t| NotNull |]

defineHasPrimaryKeyInstance :: ConName -> Int -> Q [Dec]
defineHasPrimaryKeyInstance =
  defineHasKeyConstraintInstance [t| Primary |]

defineRecordDecomposeFunction :: VarName   -- ^ Name of record decompose function.
                              -> ConName   -- ^ Name of record type.
                              -> [VarName] -- ^ List of field names of record.
                              -> Q [Dec]   -- ^ Declaration of record construct function from SqlValues.
defineRecordDecomposeFunction funName' typeName' fields = do
  let funName = varName funName'
      typeName = conName typeName'
      accessors = map (varE . varName) fields
      recVar = mkName "rec"
  sig <- sigD funName [t| $(conT typeName) -> [SqlValue] |]
  var <- funD funName [ clause [varP recVar]
                        (normalB . listE $ map (\a -> [| toSql ($a $(varE recVar)) |]) accessors)
                        [] ]
  return [sig, var]

defineTable :: (VarName, VarName)
            -> (String, ConName)
            -> (VarName, VarName, VarName)
            -> [((VarName, TypeQ), String)]
            -> Maybe Int
            -> Maybe Int
            -> [ConName]
            -> Q [Dec]
defineTable
  (cF, dF) (tableSQL, tyC)
  (tableN, fldsN, widthN)
  schemas' primaryKey notNullKey drvs = do

  let schemas = map fst schemas'
  typ  <- defineRecordType tyC schemas drvs
  let width = length schemas'
  fromSQL  <- defineRecordConstructFunction cF tyC width
  toSQL    <- defineRecordDecomposeFunction dF tyC (map fst schemas)
  tableI   <- defineTableInfo
              tableN tableSQL
              fldsN (map snd schemas') widthN width
  instSQL  <- definePersistableInstance widthN tyC cF dF width
  mayHasNN <- mayDeclare (defineHasNotNullKeyInstance tyC) notNullKey
  mayHasPk <- mayDeclare (defineHasPrimaryKeyInstance tyC) primaryKey
  return $ typ : fromSQL ++ toSQL ++ tableI ++ instSQL ++ mayHasNN ++ mayHasPk

nameOfTableSQL :: String -> String -> String
nameOfTableSQL schema table = map toUpper schema ++ '.' : map toLower table

defineTableDefault' :: String
                    -> String
                    -> [(String, TypeQ)]
                    -> Maybe Int
                    -> Maybe Int
                    -> [ConName]
                    -> Q [Dec]
defineTableDefault' schema table fields {- primaryKey notNullKey drives -} =
  defineTable
  (table `varNameWithPrefix` "fromSqlOf",
   table `varNameWithPrefix` "toSqlOf")
  (tableSQL, conCamelcaseName table)
  (table `varNameWithPrefix` "tableOf",
   table `varNameWithPrefix` "fieldsOf",
   table `varNameWithPrefix` "widthOf")
  fields'
  -- primaryKey
  -- notNullKey
  -- drives
  where
    tableSQL = nameOfTableSQL schema table
    fields' = map (uncurry fieldInfo) fields

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
  sig <- sigD name [t| Query (Singleton $pkeyType) $recordType |]
  var <- valD (varP name)
         (normalB [| typedQuery $(stringE $ sqlStr) |])
         []
  return [sig, var]

defineSqlPrimarySelect :: VarName -> (String, TypeQ)-> [(String, TypeQ)] -> String -> Q [Dec]
defineSqlPrimarySelect name' (table, recordType) fields pkey =
  defineConstantSqlQuery pkeyType recordType name'
  . SQL.unwordsSQL
  $ [SELECT, fields' `SQL.sepBy` ", ",
     FROM, SQL.word table, WHERE, SQL.word pkey <=> SQL.word "?"]
  where fields' = map (SQL.word . fst) fields
        pkeyType = fromJust $ lookup pkey fields

defineSqlPrimaryUpdate :: VarName -> String -> [String] -> String -> Q [Dec]
defineSqlPrimaryUpdate name' table fields pkey =
  defineConstantSql name'
  . SQL.unwordsSQL
  $ [UPDATE, SQL.word table, SET, assignments `SQL.sepBy` ", ",
     WHERE, SQL.word pkey, SQL.word "= ?"]
  where assignments = map (\f -> SQL.word f <=> SQL.word "?") . filter (/= pkey) $ fields

defineSqlInsert :: VarName -> String -> [String] -> Q [Dec]
defineSqlInsert name' table fields = do
  defineConstantSql name'
  . SQL.unwordsSQL
  $ [INSERT, INTO, SQL.word table, fields' `SQL.parenSepBy` ", ",
     VALUES, pfs `SQL.parenSepBy` ", "]
    where fields' = map SQL.word fields
          pfs     = replicate (length fields) (SQL.word "?")

defineSqls :: VarName      -- ^ SQL insert statement var name
           -> (String, TypeQ)       -- ^ Table name string
           -> [(String, TypeQ)]     -- ^ Field name strings
           -> Maybe (Int, (VarName, VarName)) -- ^ Primary key field name, SQL select statement var name, SQL update statement var name
           -> Q [Dec]      -- ^ SQL statement String declarations
defineSqls ins (table, recordType) fields primaryKey = do
  let width = length fields
      getPKeyName = case primaryKey of
        Just (i, (_, _))
          | i < 0 || width <= i -> compileError
                                   $  "defineSqls: Index out of bounds!: "
                                   ++ "fields count is " ++ show width ++ ", but index is " ++ show i
          | otherwise           -> return (Just . fst $ fields !! i)
        Nothing                 -> return Nothing
  primaryKey' <- getPKeyName
  let (_, (sel, upd)) = fromJust primaryKey
      fields' = map fst fields
  selD <- mayDeclare (defineSqlPrimarySelect sel (table, recordType) fields) primaryKey'
  updD <- mayDeclare (defineSqlPrimaryUpdate upd table fields') primaryKey'
  insD <- defineSqlInsert ins table fields'
  return $ concat [selD, updD, insD]

defineSqlsDefault :: String -> (String, TypeQ) -> [(String, TypeQ)] -> Maybe Int -> Q [Dec]
defineSqlsDefault schema (table, recordType) fields primaryKey' =
  defineSqls ins (tableSQL, recordType) fields primaryKey
  where
    tableSQL = nameOfTableSQL schema table
    sel = table `varNameWithPrefix` "select"
    upd = table `varNameWithPrefix` "update"
    ins = table `varNameWithPrefix` "insert"
    primaryKey = fmap (\pk -> (pk, (sel, upd))) primaryKey'

defineTableDefault :: String
                   -> String
                   -> [(String, TypeQ)]
                   -> Maybe Int
                   -> Maybe Int
                   -> [ConName]
                   -> Q [Dec]
defineTableDefault schema table fields primaryKey notNullKey derives = do
  recD <- defineTableDefault' schema table fields primaryKey notNullKey derives
  sqlD <- defineSqlsDefault schema (table, recordTypeDefault table) fields primaryKey
  return $ recD ++ sqlD

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
  defineTableDefault scm tbl cols mayPrimaryIdx (listToMaybe notNullIdxs) derives
