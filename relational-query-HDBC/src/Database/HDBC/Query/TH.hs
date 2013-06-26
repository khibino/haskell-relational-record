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

import Data.Maybe (listToMaybe, maybeToList)
import Data.List (elemIndex)

import Database.HDBC (IConnection, SqlValue)

import Language.Haskell.TH.Name.CamelCase (ConName)
import Language.Haskell.TH (Q, runIO, TypeQ, Dec)

import Database.Record.TH (defineRecordWithSqlTypeDefault)
import qualified Database.Relational.Query.TH as Relational

import Database.HDBC.Session (withConnectionIO)
import Database.HDBC.Record.Persistable ()

import Database.HDBC.Schema.Driver (Driver, getFields, getPrimaryKey)


-- | Generate all DHBC templates about table except for constraint keys using default naming rule.
defineTableDefault' :: String            -- ^ Schema name
                    -> String            -- ^ Table name
                    -> [(String, TypeQ)] -- ^ List of column name and type
                    -> [ConName]         -- ^ Derivings
                    -> Q [Dec]           -- ^ Result declaration
defineTableDefault' schema table columns derives = do
  modelD <- Relational.defineTableDefault' schema table columns derives
  sqlvD  <- defineRecordWithSqlTypeDefault [t| SqlValue |] table columns
  return $ modelD ++ sqlvD

-- | Generate all DHBC templates about table using default naming rule.
defineTableDefault :: String            -- ^ Schema name
                   -> String            -- ^ Table name
                   -> [(String, TypeQ)] -- ^ List of column name and type
                   -> [ConName]         -- ^ Derivings
                   -> [Int]             -- ^ Indexes to represent primary key
                   -> Maybe Int         -- ^ Index of not-null key
                   -> Q [Dec]           -- ^ Result declaration
defineTableDefault schema table columns derives primary notNull = do
  modelD <- Relational.defineTableDefault schema table columns derives primary notNull
  sqlvD  <- defineRecordWithSqlTypeDefault [t| SqlValue |] table columns
  return $ modelD ++ sqlvD

putLog :: String -> IO ()
putLog =  putStrLn

-- | Generate all DHBC templates using system catalog informations.
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
            mayPrimaryKey       <- getPrimaryKey drv conn scm tbl

            mayPrimaryIdx <- case mayPrimaryKey of
              Just key -> case elemIndex key $ map fst cols of
                Nothing -> do putLog $ "defineTableFromDB: fail to find index of pkey - " ++ key ++ ". Something wrong!!"
                              return   Nothing
                Just ix ->    return $ Just ix
              Nothing  ->     return   Nothing
            return (cols, notNullIdxs, mayPrimaryIdx) )

  (cols, notNullIdxs, mayPrimaryIdx) <- runIO getDBinfo
  defineTableDefault scm tbl cols derives (maybeToList mayPrimaryIdx) (listToMaybe notNullIdxs)
