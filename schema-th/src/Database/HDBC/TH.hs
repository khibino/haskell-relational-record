{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
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
--
-- This module contains templates to generate Haskell record types
-- and instances correspond to RDB table schema.
module Database.HDBC.TH (
  defineWithTableDefault,

  defineTableFromDB
  ) where

import Data.Maybe (listToMaybe)
import Data.List (elemIndex)

import Database.HDBC (IConnection, SqlValue)

import Language.Haskell.TH.Name.CamelCase (ConName)
import Language.Haskell.TH.Name.Extra (maybeD)
import Language.Haskell.TH (Q, runIO, TypeQ, Dec)

import Database.HDBC.Session (withConnectionIO)
import Database.Record.TH (recordTypeDefault)
import Database.Relational.Query.TH
  (defineRecordAndTableDefault, defineSqlsDefault,
   defineHasPrimaryKeyInstanceDefault, defineHasNotNullKeyInstanceDefault,
   tableVarExpDefault, relationVarExpDefault)
import qualified Database.Relational.Query.TH as Query
import Database.HDBC.Record.Persistable ()

import Database.HDBC.Schema.Driver (Driver, getFields, getPrimaryKey)


defineWithTableDefault' :: String
                        -> String
                        -> [(String, TypeQ)]
                        -> [ConName]
                        -> Q [Dec]
defineWithTableDefault' schema table fields derives = do
  recD <- defineRecordAndTableDefault [t| SqlValue |] schema table fields derives
  let recType = recordTypeDefault table
      tableE  = tableVarExpDefault table
  sqlD <- defineSqlsDefault table recType tableE
  return $ recD ++ sqlD

defineWithPrimaryKeyDefault :: String -> [(String, TypeQ)] -> Int -> Q [Dec]
defineWithPrimaryKeyDefault table fields idx = do
  let pkeyType = snd $ fields !! idx
  instD <- defineHasPrimaryKeyInstanceDefault table pkeyType idx
  let recType  = recordTypeDefault table
      tableE   = tableVarExpDefault table
      relE     = relationVarExpDefault table
  sqlsD <- Query.defineSqlsWithPrimaryKeyDefault table pkeyType recType relE tableE
  return $ instD ++ sqlsD

defineWithNotNullKeyDefault :: String -> TypeQ -> Int -> Q [Dec]
defineWithNotNullKeyDefault =  defineHasNotNullKeyInstanceDefault

defineWithTableDefault :: String
                       -> String
                       -> [(String, TypeQ)]
                       -> [ConName]
                       -> Maybe Int
                       -> Maybe Int
                       -> Q [Dec]
defineWithTableDefault schema table fields derives mayPrimaryIdx mayNotNullIdx  = do
  let keyType = snd . (fields !!)
  tblD  <- defineWithTableDefault' schema table fields derives
  primD <- maybeD (defineWithPrimaryKeyDefault table fields) mayPrimaryIdx
  nnD   <- maybeD (\i -> defineWithNotNullKeyDefault table (keyType i) i) mayNotNullIdx
  return $ tblD ++ primD ++ nnD

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
  defineWithTableDefault scm tbl cols derives mayPrimaryIdx (listToMaybe notNullIdxs)
