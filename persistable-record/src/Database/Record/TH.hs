{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Record.TH
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines templates for Haskell record type and
-- type class instances to map between list of untyped SQL type and Haskell record type.
module Database.Record.TH (
  -- * Generate all templates about record
  defineRecord,
  defineRecordWithConfig,

  -- * Deriving class symbols
  derivingEq, derivingShow, derivingRead, derivingData, derivingTypeable,

  -- * Table constraint specified by key
  defineHasColumnConstraintInstance,
  defineHasPrimaryConstraintInstanceDerived,
  defineHasPrimaryKeyInstance,
  defineHasNotNullKeyInstance,

  -- * Record type
  defineRecordType,
  defineRecordTypeWithConfig,

  -- * Function declarations depending on SQL type
  makeRecordPersistableWithSqlType,
  makeRecordPersistableWithSqlTypeWithConfig,
  makeRecordPersistableWithSqlTypeDefault,

  -- * Function declarations against defined record types
  makeRecordPersistableWithSqlTypeFromDefined,
  makeRecordPersistableWithSqlTypeDefaultFromDefined,
  defineColumnOffsets,

  recordWidthTemplate,

  defineRecordParser,
  defineRecordPrinter,

  definePersistableInstance,

  -- * Reify
  reifyRecordType,

  -- * Templates about record type name
  NameConfig,  defaultNameConfig,

  recordTypeName, recordType,

  columnOffsetsVarNameDefault,

  persistableFunctionNamesDefault,

  -- * Not nullable single column type
  deriveNotNullType
  ) where


import Control.Applicative (pure, (<*>))
import Data.List (foldl')
import Data.Array (Array, listArray, (!))
import Data.Data (Data, Typeable)
import Language.Haskell.TH.Name.CamelCase
  (ConName(conName), VarName(varName),
   conCamelcaseName, varCamelcaseName, varNameWithPrefix,
   toTypeCon, toDataCon, toVarExp)
import Language.Haskell.TH.Lib.Extra (integralE, simpleValD)
import Language.Haskell.TH.Compat.Data (dataD', unDataD)
import Language.Haskell.TH
  (Q, newName, nameBase, reify, Info(TyConI), Name,
   TypeQ, conT, Con (NormalC, RecC),
   Dec, sigD, valD,
   ExpQ, Exp(ConE), conE, varE, lamE, listE, sigE,
   varP, conP, normalB, recC,
   cxt, varStrictType, strictType, isStrict)

import Database.Record
  (HasColumnConstraint(columnConstraint), Primary, NotNull,
   HasKeyConstraint(keyConstraint), derivedCompositePrimary,
   PersistableRecordWidth, PersistableWidth(persistableWidth),
   FromSql(recordFromSql), RecordFromSql,
   ToSql(recordToSql), RecordToSql, wrapToSql, putRecord, putEmpty)

import Database.Record.KeyConstraint
  (unsafeSpecifyColumnConstraint, unsafeSpecifyNotNullValue, unsafeSpecifyKeyConstraint)
import Database.Record.Persistable (unsafePersistableRecordWidth, runPersistableRecordWidth)
import qualified Database.Record.Persistable as Persistable


-- | 'NameConfig' type to customize names of expanded record templates.
data NameConfig =
  NameConfig
  { recordTypeName  ::  String -> String -> ConName
    -- ^ Make record type symbol name from schema name and table name in SQL
  , columnName      ::  String -> String -> VarName
    -- ^ Make column variable symbol name from table name and column name in SQL
  }

-- | Dummy show instance. Handy to define show instance recursively.
instance Show NameConfig where
  show = const "<nameConfig>"

-- | Default implementation of 'NameConfig' type
defaultNameConfig :: NameConfig
defaultNameConfig =
  NameConfig
  { recordTypeName  =  const conCamelcaseName
  , columnName      =  const varCamelcaseName
  }

-- | Record type constructor template from SQL table name 'String'.
recordType :: NameConfig -- ^ name rule config
           -> String     -- ^ Schema name string in SQL
           -> String     -- ^ Table name string in SQL
           -> TypeQ      -- ^ Record type constructor
recordType config scm = toTypeCon . recordTypeName config scm

-- | Variable expression of record column offset array.
columnOffsetsVarNameDefault :: Name    -- ^ Table type name
                            -> VarName -- ^ Result expression variable name
columnOffsetsVarNameDefault =  (`varNameWithPrefix` "columnOffsets") . nameBase

-- | Template of 'HasColumnConstraint' instance.
defineHasColumnConstraintInstance :: TypeQ   -- ^ Type which represent constraint type
                                  -> TypeQ   -- ^ Type constructor of record
                                  -> Int     -- ^ Key index which specifies this constraint
                                  -> Q [Dec] -- ^ Result declaration template
defineHasColumnConstraintInstance constraint typeCon index =
  [d| instance HasColumnConstraint $constraint $typeCon where
        columnConstraint = unsafeSpecifyColumnConstraint $(integralE index) |]

-- | Template of 'HasKeyConstraint' instance.
defineHasPrimaryConstraintInstanceDerived ::TypeQ    -- ^ Type constructor of record
                                          -> Q [Dec] -- ^ Result declaration template
defineHasPrimaryConstraintInstanceDerived typeCon =
  [d| instance HasKeyConstraint Primary $typeCon where
        keyConstraint = derivedCompositePrimary |]

-- | Template of 'HasColumnConstraint' 'Primary' instance.
defineHasPrimaryKeyInstance :: TypeQ   -- ^ Type constructor of record
                            -> [Int]   -- ^ Key index which specifies this constraint
                            -> Q [Dec] -- ^ Declaration of primary key constraint instance
defineHasPrimaryKeyInstance typeCon = d  where
  d []   = return []
  d [ix] = do
    col  <- defineHasColumnConstraintInstance [t| Primary |] typeCon ix
    comp <- defineHasPrimaryConstraintInstanceDerived typeCon
    return $ col ++ comp
  d ixs  =
    [d| instance HasKeyConstraint Primary $typeCon where
          keyConstraint = unsafeSpecifyKeyConstraint
                          $(listE [integralE ix | ix <- ixs ])
      |]

-- | Template of 'HasColumnConstraint' 'NotNull' instance.
defineHasNotNullKeyInstance :: TypeQ   -- ^ Type constructor of record
                            -> Int     -- ^ Key index which specifies this constraint
                            -> Q [Dec] -- ^ Declaration of not null key constraint instance
defineHasNotNullKeyInstance =
  defineHasColumnConstraintInstance [t| NotNull |]

{-# DEPRECATED derivingEq "Use TH quasi-quotation like ''Eq instead of this." #-}
-- | Name to specify deriving 'Eq'
derivingEq   :: Name
derivingEq   = ''Eq

{-# DEPRECATED derivingShow "Use TH quasi-quotation like ''Show instead of this." #-}
-- | Name to specify deriving 'Show'
derivingShow :: Name
derivingShow = ''Show

{-# DEPRECATED derivingRead "Use TH quasi-quotation like ''Read instead of this." #-}
-- | Name to specify deriving 'Read'
derivingRead :: Name
derivingRead = ''Read

{-# DEPRECATED derivingData "Use TH quasi-quotation like ''Data instead of this." #-}
-- | Name to specify deriving 'Data'
derivingData :: Name
derivingData = ''Data

{-# DEPRECATED derivingTypeable "Use TH quasi-quotation like ''Typeable instead of this." #-}
-- | Name to specify deriving 'Typeable'
derivingTypeable :: Name
derivingTypeable = ''Typeable

-- | Record type width expression template.
recordWidthTemplate :: TypeQ -- ^ Record type constructor.
                    -> ExpQ  -- ^ Expression to get record width.
recordWidthTemplate ty =
  [| runPersistableRecordWidth
     $(sigE [| persistableWidth |] [t| PersistableRecordWidth $(ty) |])
   |]

-- | Column offset array and 'PersistableWidth' instance declaration.
defineColumnOffsets :: ConName -- ^ Record type constructor.
                    -> [TypeQ] -- ^ Types of record columns.
                    -> Q [Dec] -- ^ Declaration of 'PersistableWidth' instance.
defineColumnOffsets typeName' tys = do
  let ofsVar = columnOffsetsVarNameDefault $ conName typeName'
      widthIxE = integralE $ length tys
  ar <- simpleValD (varName ofsVar) [t| Array Int Int |]
        [| listArray (0 :: Int, $widthIxE) $
           scanl (+) (0 :: Int) $(listE $ map recordWidthTemplate tys) |]
  pw <- [d| instance PersistableWidth $(toTypeCon typeName') where
              persistableWidth = unsafePersistableRecordWidth $ $(toVarExp ofsVar) ! $widthIxE
          |]
  return $ ar ++ pw

-- | Record type declaration template.
defineRecordType :: ConName            -- ^ Name of the data type of table record type.
                 -> [(VarName, TypeQ)] -- ^ List of columns in the table. Must be legal, properly cased record columns.
                 -> [Name]             -- ^ Deriving type class names.
                 -> Q [Dec]            -- ^ The data type record declaration.
defineRecordType typeName' columns derives = do
  let typeName = conName typeName'
      fld (n, tq) = varStrictType (varName n) (strictType isStrict tq)
  rec  <- dataD' (cxt []) typeName [] [recC typeName (map fld columns)] derives
  offs <- defineColumnOffsets typeName' [ty | (_, ty) <- columns]
  return $ rec : offs

-- | Generate column name from 'String'.
columnDefault :: String -> TypeQ -> (VarName, TypeQ)
columnDefault n t = (varCamelcaseName n, t)

-- | Record type declaration template with configured names.
defineRecordTypeWithConfig :: NameConfig -> String -> String -> [(String, TypeQ)] -> [Name] -> Q [Dec]
defineRecordTypeWithConfig config schema table columns =
  defineRecordType
  (recordTypeName config schema table)
  [ (columnName config schema n, t) | (n, t) <- columns ]


-- | Record parser template.
defineRecordParser :: TypeQ         -- ^ SQL value type.
                   -> VarName       -- ^ Name of record parser.
                   -> (TypeQ, ExpQ) -- ^ Record type constructor and data constructor.
                   -> Int           -- ^ Count of record columns.
                   -> Q [Dec]       -- ^ Declaration of record construct function from SQL values.
defineRecordParser sqlValType name' (tyCon, dataCon) width = do
  let name = varName name'
  sig <- sigD name [t| RecordFromSql $sqlValType $tyCon |]
  var <- valD (varP name)
         (normalB
          (foldl' (\a x -> [| $a <*> $x |]) [| pure $dataCon |]
           $ replicate width [| recordFromSql |])
         )
         []
  return [sig, var]

dataConInfo :: Exp -> Q Name
dataConInfo =  d  where
  d (ConE n) = return n
  d e        = fail $ "Not record data constructor: " ++ show e

-- | Record printer template.
defineRecordPrinter :: TypeQ         -- ^ SQL value type.
                    -> VarName       -- ^ Name of record printer.
                    -> (TypeQ, ExpQ) -- ^ Record type constructor and data constructor.
                    -> Int           -- ^ Count of record columns.
                    -> Q [Dec]       -- ^ Declaration of record construct function from SQL values.
defineRecordPrinter sqlValType name' (tyCon, dataCon) width = do
  let name = varName name'
  sig <- sigD name [t| RecordToSql $sqlValType $tyCon |]
  names <- mapM (newName . ('f':) . show) [1 .. width]
  dcn <- dataCon >>= dataConInfo
  var <- valD (varP name)
         (normalB [| wrapToSql
                     $(lamE
                       [ conP dcn [ varP n | n <- names ] ]
                       (foldr (\a x -> [| $a >> $x |]) [| putEmpty () |]
                        [ [| putRecord $(varE n) |] | n <- names ])) |])
         []
  return [sig, var]

-- | Record parser and printer instance templates for converting
--   between list of SQL type and Haskell record type.
definePersistableInstance :: TypeQ   -- ^ SQL value type.
                           -> TypeQ   -- ^ Record type constructor.
                           -> VarName -- ^ Record parser name.
                           -> VarName -- ^ Record printer name.
                           -> Int     -- ^ Count of record columns.
                           -> Q [Dec] -- ^ Instance declarations for 'Persistable'.
definePersistableInstance sqlType typeCon parserName printerName _width = do
  [d| instance FromSql $sqlType $typeCon where
        recordFromSql = $(toVarExp parserName)

      instance ToSql $sqlType $typeCon where
        recordToSql = $(toVarExp printerName)
    |]

-- | All templates depending on SQL value type.
makeRecordPersistableWithSqlType :: TypeQ              -- ^ SQL value type.
                                 -> (VarName, VarName) -- ^ Constructor function name and decompose function name.
                                 -> (TypeQ, ExpQ)      -- ^ Record type constructor and data constructor.
                                 -> Int                -- ^ Count of record columns.
                                 -> Q [Dec]            -- ^ Result declarations.
makeRecordPersistableWithSqlType
  sqlValueType
  (cF, dF) conPair@(tyCon, _)
  width = do
  fromSQL  <- defineRecordParser sqlValueType cF conPair width
  toSQL    <- defineRecordPrinter sqlValueType dF conPair width
  instSQL  <- definePersistableInstance sqlValueType tyCon cF dF width
  return $ fromSQL ++ toSQL ++ instSQL

-- | Default name of record construction function from SQL table name.
fromSqlNameDefault :: String -> VarName
fromSqlNameDefault =  (`varNameWithPrefix` "fromSqlOf")

-- | Default name of record decomposition function from SQL table name.
toSqlNameDefault :: String -> VarName
toSqlNameDefault =  (`varNameWithPrefix` "toSqlOf")

-- | All templates depending on SQL value type with configured names.
makeRecordPersistableWithSqlTypeWithConfig :: TypeQ      -- ^ SQL value type
                                         -> NameConfig -- ^ name rule config
                                         -> String     -- ^ Schema name of database
                                         -> String     -- ^ Table name of database
                                         -> Int        -- ^ Count of record columns
                                         -> Q [Dec]    -- ^ Result declarations
makeRecordPersistableWithSqlTypeWithConfig sqlValueType config schema table width =
  makeRecordPersistableWithSqlType
    sqlValueType
    (persistableFunctionNamesDefault . conName . conCamelcaseName $ table)
    (recordType config schema table, toDataCon . recordTypeName config schema $ table)
    width

-- | All templates depending on SQL value type with default names.
makeRecordPersistableWithSqlTypeDefault :: TypeQ   -- ^ SQL value type
                                        -> String  -- ^ Schema name
                                        -> String  -- ^ Table name
                                        -> Int     -- ^ Count of record columns
                                        -> Q [Dec] -- ^ Result declarations
makeRecordPersistableWithSqlTypeDefault sqlValueType =
  makeRecordPersistableWithSqlTypeWithConfig sqlValueType defaultNameConfig

recordInfo' :: Info -> Maybe ((TypeQ, ExpQ), (Maybe [Name], [TypeQ]))
recordInfo' =  d  where
  d (TyConI tcon) = do
    (_cxt, tcn, _bs, _mk, [r], _ds) <- unDataD tcon
    case r of
      NormalC dcn ts   -> Just ((conT tcn, conE dcn), (Nothing, [return t | (_, t) <- ts]))
      RecC    dcn vts  -> Just ((conT tcn, conE dcn), (Just ns, ts))
        where (ns, ts) = unzip [(n, return t) | (n, _, t) <- vts]
      _                -> Nothing
  d _                  =  Nothing

-- | Low-level reify interface for record type name.
reifyRecordType :: Name -> Q ((TypeQ, ExpQ), (Maybe [Name], [TypeQ]))
reifyRecordType recTypeName = do
  tyConInfo   <- reify recTypeName
  maybe
    (fail $ "Defined record type constructor not found: " ++ show recTypeName)
    return
    (recordInfo' tyConInfo)

-- | Generate persistable function symbol names using default rule.
persistableFunctionNamesDefault :: Name -> (VarName, VarName)
persistableFunctionNamesDefault recTypeName = (fromSqlNameDefault bn, toSqlNameDefault bn)  where
  bn = nameBase recTypeName

-- | All templates depending on SQL value type. Defined record type information is used.
makeRecordPersistableWithSqlTypeFromDefined :: TypeQ              -- ^ SQL value type
                                            -> (VarName, VarName) -- ^ Constructor function name and decompose function name
                                            -> Name               -- ^ Record type constructor name
                                            -> Q [Dec]            -- ^ Result declarations
makeRecordPersistableWithSqlTypeFromDefined sqlValueType fnames recTypeName = do
  (conPair, (_, cts)) <- reifyRecordType recTypeName
  makeRecordPersistableWithSqlType sqlValueType fnames conPair $ length cts

-- | All templates depending on SQL value type with default names. Defined record type information is used.
makeRecordPersistableWithSqlTypeDefaultFromDefined :: TypeQ   -- ^ SQL value type
                                                   -> Name    -- ^ Record type constructor name
                                                   -> Q [Dec] -- ^ Result declarations
makeRecordPersistableWithSqlTypeDefaultFromDefined sqlValueType recTypeName =
  makeRecordPersistableWithSqlTypeFromDefined sqlValueType (persistableFunctionNamesDefault recTypeName) recTypeName

-- | All templates for record type.
defineRecord :: TypeQ              -- ^ SQL value type
             -> (VarName, VarName) -- ^ Constructor function name and decompose function name
             -> ConName            -- ^ Record type name
             -> [(VarName, TypeQ)] -- ^ Column schema
             -> [Name]             -- ^ Record derivings
             -> Q [Dec]            -- ^ Result declarations
defineRecord
  sqlValueType
  fnames tyC
  columns drvs = do

  typ     <- defineRecordType tyC columns drvs
  withSql <- makeRecordPersistableWithSqlType sqlValueType fnames (toTypeCon tyC, toDataCon tyC) $ length columns
  return $ typ ++ withSql

-- | All templates for record type with configured names.
defineRecordWithConfig :: TypeQ             -- ^ SQL value type
                     -> NameConfig        -- ^ name rule config
                     -> String            -- ^ Schema name
                     -> String            -- ^ Table name
                     -> [(String, TypeQ)] -- ^ Column names and types
                     -> [Name]            -- ^ Record derivings
                     -> Q [Dec]           -- ^ Result declarations
defineRecordWithConfig sqlValueType config schema table columns derives = do
  typ     <- defineRecordTypeWithConfig config schema table columns derives
  withSql <- makeRecordPersistableWithSqlTypeWithConfig sqlValueType config schema table $ length columns
  return $ typ ++ withSql


-- | Templates for single column value type.
deriveNotNullType :: TypeQ -> Q [Dec]
deriveNotNullType typeCon =
  [d| instance PersistableWidth $typeCon where
        persistableWidth = Persistable.unsafeValueWidth

      instance HasColumnConstraint NotNull $typeCon where
        columnConstraint = unsafeSpecifyNotNullValue
    |]
