{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Record.TH
-- Copyright   : 2013-2017 Kei Hibino
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

  -- * Table constraint specified by key
  defineHasColumnConstraintInstance,
  defineHasPrimaryConstraintInstanceDerived,
  defineHasPrimaryKeyInstance,
  defineHasNotNullKeyInstance,

  -- * Record type
  defineRecordType,
  defineRecordTypeWithConfig,

  -- * Function declarations against defined record types
  defineColumnOffsets,

  recordWidthTemplate,

  -- * Reify
  reifyRecordType,

  -- * Templates about record name
  NameConfig,  defaultNameConfig,
  recordTypeName, columnName,

  recordTemplate,

  columnOffsetsVarNameDefault,

  -- * Not nullable single column type
  deriveNotNullType,

  -- * Template for tuple types
  defineTupleInstances,
  ) where

import GHC.Generics (Generic)
import Data.Array (Array)
import Language.Haskell.TH.Name.CamelCase
  (ConName(conName), VarName(varName),
   conCamelcaseName, varCamelcaseName, varNameWithPrefix,
   toTypeCon, toDataCon, )
import Language.Haskell.TH.Lib.Extra (integralE, simpleValD, reportWarning)
import Language.Haskell.TH.Compat.Data (dataD', unDataD)
import Language.Haskell.TH
  (Q, nameBase, reify, Info(TyConI), Name,
   TypeQ, conT, Con (NormalC, RecC),
   Dec,
   ExpQ, conE, listE, sigE,
   recC,
   cxt, varStrictType, strictType, isStrict)

import Control.Arrow ((&&&))

import Database.Record
  (HasColumnConstraint(columnConstraint), Primary, NotNull,
   HasKeyConstraint(keyConstraint), derivedCompositePrimary,
   PersistableRecordWidth, PersistableWidth(persistableWidth),
   FromSql, ToSql, )

import Database.Record.KeyConstraint
  (unsafeSpecifyColumnConstraint, unsafeSpecifyNotNullValue, unsafeSpecifyKeyConstraint)
import Database.Record.Persistable
  (runPersistableRecordWidth,
   ProductConst, getProductConst, genericFieldOffsets)
import qualified Database.Record.Persistable as Persistable
import Database.Record.InternalTH (defineTupleInstances)


-- | 'NameConfig' type to customize names of expanded record templates.
data NameConfig =
  NameConfig
  { recordTypeName  ::  String -> String -> ConName
    -- ^ Make record type name generated from the table's definition.
    --   The first argument is the schema name of the table, and the second argument is the table name.
  , columnName      ::  String -> String -> VarName
    -- ^ Make each field label of the record type generated from the table's definition.
    --   The first argument is the table name, and the second argument is the column name.
  }

-- | Dummy show instance. Handy to define show instance recursively.
instance Show NameConfig where
  show = const "<nameConfig>"

-- | Default implementation of 'NameConfig' type.
--   To change how the generated record type and its columns are named,
--   use record update syntax:
--
-- > defaultNameConfig
-- >   { recordTypeName = \schema table -> varCamelcaseName $ schema ++ "_" ++ table
-- >     ^ append the table name after the schema name. e.g. "schemaTable"
-- >   , columnName = \table column -> varCamelcaseName $ table ++ "_" ++ column
-- >     ^ append the column name after the table name. e.g. "tableColumn"
-- >   }
defaultNameConfig :: NameConfig
defaultNameConfig =
  NameConfig
  { recordTypeName  =  const conCamelcaseName
  , columnName      =  const varCamelcaseName
  }

-- | Record constructor templates from SQL table name 'String'.
recordTemplate :: NameConfig    -- ^ name rule config
               -> String        -- ^ Schema name string in SQL
               -> String        -- ^ Table name string in SQL
               -> (TypeQ, ExpQ) -- ^ Record type and data constructor
recordTemplate config scm = (toTypeCon &&& toDataCon) . recordTypeName config scm

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
        [| getProductConst (genericFieldOffsets :: ProductConst (Array Int Int) $(toTypeCon typeName')) |]
  pw <- [d| instance PersistableWidth $(toTypeCon typeName')
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
  derives1 <- if (''Generic `notElem` derives)
              then do reportWarning "HRR needs Generic instance, please add ''Generic manually."
                      return $ ''Generic : derives
                      {- DROP this hack in future version ups. -}
              else    return   derives
  rec' <- dataD' (cxt []) typeName [] [recC typeName (map fld columns)] derives1
  offs <- defineColumnOffsets typeName' [ty | (_, ty) <- columns]
  return $ rec' : offs

-- | Record type declaration template with configured names.
defineRecordTypeWithConfig :: NameConfig -> String -> String -> [(String, TypeQ)] -> [Name] -> Q [Dec]
defineRecordTypeWithConfig config schema table columns =
  defineRecordType
  (recordTypeName config schema table)
  [ (columnName config table n, t) | (n, t) <- columns ]


-- | Default name of record construction function from SQL table name.
fromSqlNameDefault :: String -> VarName
fromSqlNameDefault =  (`varNameWithPrefix` "fromSqlOf")

-- | Default name of record decomposition function from SQL table name.
toSqlNameDefault :: String -> VarName
toSqlNameDefault =  (`varNameWithPrefix` "toSqlOf")

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

-- | Record parser and printer instance templates for converting
--   between list of SQL type and Haskell record type.
definePersistableInstance :: TypeQ   -- ^ SQL value type.
                          -> TypeQ   -- ^ Record type constructor.
                          -> Q [Dec] -- ^ Instance declarations.
definePersistableInstance sqlType typeCon = do
  [d| instance FromSql $sqlType $typeCon
      instance ToSql $sqlType $typeCon
    |]

-- | All templates for record type.
defineRecord :: TypeQ              -- ^ SQL value type
             -> ConName            -- ^ Record type name
             -> [(VarName, TypeQ)] -- ^ Column schema
             -> [Name]             -- ^ Record derivings
             -> Q [Dec]            -- ^ Result declarations
defineRecord
  sqlValueType
  tyC
  columns drvs = do

  typ     <- defineRecordType tyC columns drvs
  withSql <- definePersistableInstance sqlValueType $ toTypeCon tyC
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
  withSql <- definePersistableInstance sqlValueType . fst $ recordTemplate config schema table

  return $ typ ++ withSql


-- | Templates for single column value type.
deriveNotNullType :: TypeQ -> Q [Dec]
deriveNotNullType typeCon =
  [d| instance PersistableWidth $typeCon where
        persistableWidth = Persistable.unsafeValueWidth

      instance HasColumnConstraint NotNull $typeCon where
        columnConstraint = unsafeSpecifyNotNullValue
    |]
