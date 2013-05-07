{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Record.TH (
  recordTypeNameDefault, recordTypeDefault,
  defineHasNotNullKeyInstance,
  defineHasPrimaryKeyInstance,
  defineHasPrimaryKeyInstanceDefault,
  defineHasNotNullKeyInstanceDefault,

  derivingEq, derivingShow, derivingRead, derivingData, derivingTypable,

  defineRecordType,
  defineRecordConstructFunction,
  defineRecordDecomposeFunction
  ) where

import Language.Haskell.TH.Name.CamelCase
  (ConName(conName), VarName(varName), conCamelcaseName, toTypeCon)
import Language.Haskell.TH.Name.Extra (integralE)
import Language.Haskell.TH
  (Q, mkName,
   TypeQ, conT,
   Dec, DecQ, dataD, sigD, funD,
   appsE, conE, varE, listE, stringE,
   listP, varP, wildP,
   clause, normalB, recC, cxt, varStrictType, strictType, isStrict)

import Database.Record.KeyConstraint
  (HasKeyConstraint(constraintKey), specifyKeyConstraint, Primary, NotNull)
import Database.Record.Persistable
  (fromSql, toSql)

defineHasKeyConstraintInstance :: TypeQ -> TypeQ -> Int -> Q [Dec]
defineHasKeyConstraintInstance constraint typeCon index =
  [d| instance HasKeyConstraint $constraint $typeCon where
        constraintKey = specifyKeyConstraint $(integralE index) |]

recordTypeNameDefault :: String -> ConName
recordTypeNameDefault =  conCamelcaseName

recordTypeDefault :: String -> TypeQ
recordTypeDefault =  toTypeCon . recordTypeNameDefault

defineHasNotNullKeyInstance :: TypeQ -> Int -> Q [Dec]
defineHasNotNullKeyInstance =
  defineHasKeyConstraintInstance [t| NotNull |]

defineHasPrimaryKeyInstance :: TypeQ -> Int -> Q [Dec]
defineHasPrimaryKeyInstance =
  defineHasKeyConstraintInstance [t| Primary |]

defineHasPrimaryKeyInstanceDefault :: String -> Int -> Q [Dec]
defineHasPrimaryKeyInstanceDefault =
  defineHasPrimaryKeyInstance . recordTypeDefault

defineHasNotNullKeyInstanceDefault :: String -> Int -> Q [Dec]
defineHasNotNullKeyInstanceDefault =
  defineHasNotNullKeyInstance . recordTypeDefault

derivingEq   = conCamelcaseName "Eq"
derivingShow = conCamelcaseName "Show"
derivingRead = conCamelcaseName "Read"
derivingData = conCamelcaseName "Data"
derivingTypable = conCamelcaseName "Typable"
derivingEq, derivingShow, derivingRead, derivingData, derivingTypable :: ConName

defineRecordType :: ConName            -- ^ Name of the data type of table record type.
                 -> [(VarName, TypeQ)] -- ^ List of fields in the table. Must be legal, properly cased record fields.
                 -> [ConName]          -- ^ Deriving type class names.
                 -> DecQ               -- ^ The data type record declaration.
defineRecordType typeName' fields derives = do
  let typeName = conName typeName'
  dataD (cxt []) typeName [] [recC typeName (map fld fields)] (map conName derives)
  where
    fld (n, tq) = varStrictType (varName n) (strictType isStrict tq)

defineRecordConstructFunction :: TypeQ     -- ^ SQL value type.
                              -> VarName   -- ^ Name of record construct function.
                              -> ConName   -- ^ Name of record type.
                              -> Int       -- ^ Count of record fields.
                              -> Q [Dec]   -- ^ Declaration of record construct function from SqlValues.
defineRecordConstructFunction sqlValType funName' typeName' width = do
  let funName = varName funName'
      typeName = conName typeName'
      names = map (mkName . ('f':) . show) [1 .. width]
      fromSqlE n = [| fromSql $(varE n) |]
  sig <- sigD funName [t| [$(sqlValType)] -> $(conT typeName) |]
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

defineRecordDecomposeFunction :: TypeQ     -- ^ SQL value type.
                              -> VarName   -- ^ Name of record decompose function.
                              -> TypeQ     -- ^ Name of record type.
                              -> [VarName] -- ^ List of field names of record.
                              -> Q [Dec]   -- ^ Declaration of record construct function from SqlValues.
defineRecordDecomposeFunction sqlValType funName' typeCon fields = do
  let funName = varName funName'
      accessors = map (varE . varName) fields
      recVar = mkName "rec"
  sig <- sigD funName [t| $typeCon -> [$(sqlValType)] |]
  var <- funD funName [ clause [varP recVar]
                        (normalB . listE $ map (\a -> [| toSql ($a $(varE recVar)) |]) accessors)
                        [] ]
  return [sig, var]
