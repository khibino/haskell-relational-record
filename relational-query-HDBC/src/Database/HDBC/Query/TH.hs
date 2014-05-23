{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}

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
  makeRecordPersistableDefault,

  defineTableDefault',
  defineTableDefault,

  defineTableFromDB,

  inlineVerifiedQuery
  ) where

import Data.Maybe (listToMaybe, isJust, catMaybes)
import qualified Data.Map as Map

import Database.HDBC (IConnection, SqlValue, prepare)

import Language.Haskell.TH.Name.CamelCase (ConName)
import Language.Haskell.TH (Q, runIO, Name, nameBase, TypeQ, Dec)
import Language.Haskell.TH.Name.CamelCase (varCamelcaseName)

import Database.Record.TH (makeRecordPersistableWithSqlTypeDefault)
import qualified Database.Record.TH as Record
import Database.Relational.Query (Relation, Config, relationalQuerySQL)
import Database.Relational.Query.SQL (QuerySuffix)
import qualified Database.Relational.Query.TH as Relational

import Database.HDBC.Session (withConnectionIO)
import Database.HDBC.Record.Persistable ()

import Database.HDBC.Schema.Driver (Driver, getFields, getPrimaryKey)


-- | Generate all persistable templates against defined record like type constructor.
makeRecordPersistableDefault :: Name    -- ^ Type constructor name
                             -> Q [Dec] -- ^ Resutl declaration
makeRecordPersistableDefault recTypeName = do
  (pair@(tyCon, dataCon), (mayNs, cts)) <- Record.reifyRecordType recTypeName
  let width = length cts
  pw <- Record.definePersistableWidthInstance tyCon width
  ps <- Record.makeRecordPersistableWithSqlType [t| SqlValue |] (Record.persistableFunctionNamesDefault recTypeName) pair width
  cs <- maybe
        (return [])
        (\ns -> fmap concat . sequence $
                [ Relational.defineColumnDefault Nothing tyCon (nameBase n) i ct
                | n  <- ns  | i  <- [0 ..]  | ct <- cts ])
        mayNs
  pc <- Relational.defineProductConstructorInstance tyCon dataCon cts
  return $ concat [pw, ps, cs, pc]

-- | Generate all HDBC templates about table except for constraint keys using default naming rule.
defineTableDefault' :: String            -- ^ Schema name
                    -> String            -- ^ Table name
                    -> [(String, TypeQ)] -- ^ List of column name and type
                    -> [ConName]         -- ^ Derivings
                    -> Q [Dec]           -- ^ Result declaration
defineTableDefault' schema table columns derives = do
  modelD <- Relational.defineTableTypesAndRecordDefault schema table columns derives
  sqlvD  <- makeRecordPersistableWithSqlTypeDefault [t| SqlValue |] table $ length columns
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
  sqlvD  <- makeRecordPersistableWithSqlTypeDefault [t| SqlValue |] table $ length columns
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

-- | Verify composed 'Query' and inline it in compile type.
inlineVerifiedQuery :: IConnection conn
                    => IO conn      -- ^ Connect action to system catalog database
                    -> Name         -- ^ Top-level variable name which has 'Relation' type
                    -> Relation p r -- ^ Object which has 'Relation' type
                    -> Config       -- ^ Configuration to generate SQL
                    -> QuerySuffix  -- ^ suffix SQL words
                    -> String       -- ^ Variable name to define as inlined query
                    -> Q [Dec]      -- ^ Result declarations
inlineVerifiedQuery connect relVar rel config sufs qns = do
  (p, r) <- Relational.reifyRelation relVar
  let sql = relationalQuerySQL config rel sufs
  _ <- runIO $ withConnectionIO connect
       (\conn -> do
           putLog $ "Verify with prepare: " ++ sql
           prepare conn sql)
  Relational.unsafeInlineQuery (return p) (return r) sql (varCamelcaseName qns)
