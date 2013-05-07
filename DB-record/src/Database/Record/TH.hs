{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Record.TH (
  recordTypeNameDefault, recordTypeDefault,
  defineHasNotNullKeyInstance,
  defineHasPrimaryKeyInstance,
  defineHasPrimaryKeyInstanceDefault,
  defineHasNotNullKeyInstanceDefault
  ) where

import Language.Haskell.TH.Name.CamelCase
  (ConName, conCamelcaseName, toTypeCon)
import Language.Haskell.TH.Name.Extra (integralE)
import Language.Haskell.TH (Q, TypeQ, Dec)

import Database.Record.KeyConstraint
  (HasKeyConstraint(constraintKey), specifyKeyConstraint, Primary, NotNull)

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
