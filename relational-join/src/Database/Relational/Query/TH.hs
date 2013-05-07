{-# LANGUAGE TemplateHaskell #-}

module Database.Relational.Query.TH (
  inlineQuery
  ) where

import Language.Haskell.TH
  (Q, reify, Info (VarI), TypeQ, Type (AppT, ConT),
   Dec, sigD, valD, varP, normalB, stringE, listE)
import Language.Haskell.TH.Name.CamelCase (VarName, varName)
import Language.Haskell.TH.Name.Extra (compileError, simpleValD)

import Database.Relational.Query.Table (Table)
import qualified Database.Relational.Query.Table as Table
import Database.Relational.Query.Relation (Relation, toSQL)
import Database.Relational.Query.Type (Query, unsafeTypedQuery)

defineTableInfo :: VarName
                -> TypeQ
                -> String
                -> [String]
                -> Int
                -> Q [Dec]
defineTableInfo tableVar' recordType table fields width = do
  let tableVar = varName tableVar'
  simpleValD tableVar [t| Table $(recordType) |]
    [| Table.table $(stringE table) $(listE $ map stringE fields) |]

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
