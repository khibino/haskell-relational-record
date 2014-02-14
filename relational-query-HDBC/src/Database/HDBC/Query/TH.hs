{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Query.TH
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains templates to generate Haskell record types
-- and HDBC instances correspond to RDB table schema.
module Database.HDBC.Query.TH (
  defineTableDefault',
  defineTableDefault,

  defineTableFromDB
  ) where

import Data.Maybe (listToMaybe, isJust, catMaybes)
import qualified Data.Map as Map

import Database.HDBC (IConnection, SqlValue)

import Language.Haskell.TH.Name.CamelCase (ConName)
import Language.Haskell.TH (Q, runIO, TypeQ, Dec)

import Database.Record.TH (defineRecordWithSqlTypeDefault)
import qualified Database.Relational.Query.TH as Relational

import Database.HDBC.Session (withConnectionIO)
import Database.HDBC.Record.Persistable ()

import Database.HDBC.Schema.Driver (Driver, getFields, getPrimaryKey)


-- | Generate all HDBC templates about table except for constraint keys using default naming rule.
defineTableDefault' :: String            -- ^ Schema name
                    -> String            -- ^ Table name
                    -> [(String, TypeQ)] -- ^ List of column name and type
                    -> [ConName]         -- ^ Derivings
                    -> Q [Dec]           -- ^ Result declaration
defineTableDefault' schema table columns derives = do
  modelD <- Relational.defineTableTypesAndRecordDefault schema table columns derives
  sqlvD  <- defineRecordWithSqlTypeDefault [t| SqlValue |] table $ length columns
  return $ modelD ++ sqlvD

-- | Generate all HDBC templates about table using default naming rule.
defineTableDefault :: String            -- ^ Schema name
                   -> String            -- ^ Table name
                   -> [(String, TypeQ)] -- ^ List of column name and type
                   -> [ConName]         -- ^ Derivings
                   -> [Int]             -- ^ Indexes to represent primary key
                   -> Maybe Int         -- ^ Index of not-null key
                   -> Q [Dec]           -- ^ Result declaration
defineTableDefault schema table columns derives primary notNull = do
  modelD <- Relational.defineTableDefault schema table columns derives primary notNull
  sqlvD  <- defineRecordWithSqlTypeDefault [t| SqlValue |] table $ length columns
  return $ modelD ++ sqlvD

putLog :: String -> IO ()
putLog =  putStrLn

-- | Generate all HDBC templates using system catalog informations.
defineTableFromDB :: IConnection conn
                  => IO conn     -- ^ Connect action to system catalog database
                  -> Driver conn -- ^ Driver definition
                  -> String      -- ^ Schema name
                  -> String      -- ^ Table name
                  -> [ConName]   -- ^ Derivings
                  -> Q [Dec]     -- ^ Result declaration
defineTableFromDB connect drv scm tbl derives = do
  let getDBinfo =
        withConnectionIO connect
        (\conn ->  do
            (cols, notNullIdxs) <- getFields drv conn scm tbl
            primCols            <- getPrimaryKey drv conn scm tbl

            let colIxMap = Map.fromList $ zip [c | (c, _) <- cols] [(0 :: Int) .. ]
                lookup' k = do
                  case Map.lookup k colIxMap of
                    Just i  -> return $ Just i
                    Nothing -> do
                      putLog $ "defineTableFromDB: fail to find index of pkey - " ++ k ++ ". Something wrong!!"
                      return Nothing

            primaryIxs <- case primCols of
              _:_ -> do
                founds <- mapM lookup' primCols
                if all isJust founds
                  then return $ catMaybes founds
                  else return []
              []  ->   return []
            return (cols, notNullIdxs, primaryIxs) )

  (cols, notNullIdxs, primaryIxs) <- runIO getDBinfo
  defineTableDefault scm tbl cols derives primaryIxs (listToMaybe notNullIdxs)
