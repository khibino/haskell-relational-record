{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Query.TH
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains templates to generate Haskell record types
-- and HDBC instances correspond to RDB table schema.
module Database.HDBC.Query.TH (
  makeRelationalRecord,
  makeRelationalRecord',

  defineTableDefault',
  defineTableDefault,

  defineTableFromDB',
  defineTableFromDB,

  inlineVerifiedQuery
  ) where

import Control.Applicative ((<$>), pure, (<*>))
import Control.Monad (when, void)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Map as Map
import Data.Functor.ProductIsomorphic.TH (reifyRecordType)

import Database.HDBC (IConnection, SqlValue, prepare)

import Language.Haskell.TH (Q, runIO, Name, TypeQ, Type (AppT, ConT), Dec)
import Language.Haskell.TH.Name.CamelCase (varCamelcaseName)
import Language.Haskell.TH.Lib.Extra (reportWarning, reportError)

import Database.Record (ToSql, FromSql)
import Database.Record.TH (recordTemplate, defineSqlPersistableInstances)
import Database.Relational
  (Config, nameConfig, recordConfig, enableWarning, verboseAsCompilerWarning,
   defaultConfig, Relation, untypeQuery, relationalQuery_, QuerySuffix)
import qualified Database.Relational.TH as Relational

import Database.HDBC.Session (withConnectionIO)
import Database.HDBC.Record.Persistable ()

import Database.HDBC.Schema.Driver
  (foldLog, emptyLogChan, takeLogs, Driver, driverConfig, getFields, getPrimaryKey)


defineInstancesForSqlValue :: TypeQ   -- ^ Record type constructor.
                          -> Q [Dec] -- ^ Instance declarations.
defineInstancesForSqlValue typeCon = do
  [d| instance FromSql SqlValue $typeCon
      instance ToSql SqlValue $typeCon
    |]

-- | Generate all persistable templates against defined record like type constructor.
makeRelationalRecord' :: Config
                      -> Name    -- ^ Type constructor name
                      -> Q [Dec] -- ^ Result declaration
makeRelationalRecord' config recTypeName = do
  rr <- Relational.makeRelationalRecordDefault' config recTypeName
  (((typeCon, avs), _), _) <- reifyRecordType recTypeName
  ps <- defineSqlPersistableInstances [t| SqlValue |] typeCon avs
  return $ rr ++ ps

-- | Generate all persistable templates against defined record like type constructor.
makeRelationalRecord :: Name    -- ^ Type constructor name
                     -> Q [Dec] -- ^ Result declaration
makeRelationalRecord = makeRelationalRecord' defaultConfig

-- | Generate all HDBC templates about table except for constraint keys.
defineTableDefault' :: Config            -- ^ Configuration to generate query with
                    -> String            -- ^ Schema name
                    -> String            -- ^ Table name
                    -> [(String, TypeQ)] -- ^ List of column name and type
                    -> [Name]            -- ^ Derivings
                    -> Q [Dec]           -- ^ Result declaration
defineTableDefault' config schema table columns derives = do
  modelD <- Relational.defineTableTypesAndRecord config schema table columns derives
  sqlvD <- defineSqlPersistableInstances [t| SqlValue |]
           (fst $ recordTemplate (recordConfig $ nameConfig config) schema table)
           []
  return $ modelD ++ sqlvD

-- | Generate all HDBC templates about table.
defineTableDefault :: Config            -- ^ Configuration to generate query with
                   -> String            -- ^ Schema name
                   -> String            -- ^ Table name
                   -> [(String, TypeQ)] -- ^ List of column name and type
                   -> [Name]            -- ^ Derivings
                   -> [Int]             -- ^ Indexes to represent primary key
                   -> Maybe Int         -- ^ Index of not-null key
                   -> Q [Dec]           -- ^ Result declaration
defineTableDefault config schema table columns derives primary notNull = do
  modelD <- Relational.defineTable config schema table columns derives primary notNull
  sqlvD <- defineInstancesForSqlValue . fst $ recordTemplate (recordConfig $ nameConfig config) schema table
  return $ modelD ++ sqlvD

tableAlongWithSchema :: IConnection conn
                     => IO conn           -- ^ Connect action to system catalog database
                     -> Driver conn       -- ^ Driver definition
                     -> String            -- ^ Schema name
                     -> String            -- ^ Table name
                     -> [(String, TypeQ)] -- ^ Additional column-name and column-type mapping to overwrite default
                     -> [Name]            -- ^ Derivings
                     -> Q [Dec]           -- ^ Result declaration
tableAlongWithSchema connect drv scm tbl cmap derives = do
  let config = driverConfig drv
      getDBinfo = do
        logChan  <-  emptyLogChan
        infoP    <-  withConnectionIO connect
                     (\conn ->
                       (,)
                       <$> getFields drv conn logChan scm tbl
                       <*> getPrimaryKey drv conn logChan scm tbl)
        (,) infoP <$> takeLogs logChan

  (((cols, notNullIdxs), primaryCols), logs) <- runIO getDBinfo
  let reportWarning'
        | enableWarning config             =  reportWarning
        | otherwise                        =  const $ pure ()
      reportVerbose
        | verboseAsCompilerWarning config  =  reportWarning
        | otherwise                        =  const $ pure ()
  mapM_ (foldLog reportVerbose reportWarning' reportError) logs
  when (null primaryCols) . reportWarning'
    $ "getPrimaryKey: Primary key not found for table: " ++ scm ++ "." ++ tbl

  let colIxMap = Map.fromList $ zip [c | (c, _) <- cols] [(0 :: Int) .. ]
      ixLookups = [ (k, Map.lookup k colIxMap) | k <- primaryCols ]
      warnLk k = maybe
                 (reportWarning $ "defineTableFromDB: fail to find index of pkey - " ++ k ++ ". Something wrong!!")
                 (const $ return ())
      primaryIxs = fromMaybe [] . sequence $ map snd ixLookups
  mapM_ (uncurry warnLk) ixLookups

  let liftMaybe tyQ sty = do
        ty <- tyQ
        case ty of
          (AppT (ConT n) _) | n == ''Maybe  -> [t| Maybe $(sty) |]
          _                                 -> sty
      cols1 = [ (,) cn . maybe ty (liftMaybe ty) . Map.lookup cn $ Map.fromList cmap | (cn, ty) <- cols ]
  defineTableDefault config scm tbl cols1 derives primaryIxs (listToMaybe notNullIdxs)

-- | Generate all HDBC templates using system catalog information with specified config.
defineTableFromDB' :: IConnection conn
                   => IO conn           -- ^ Connect action to system catalog database
                   -> Driver conn       -- ^ Driver definition
                   -> String            -- ^ Schema name
                   -> String            -- ^ Table name
                   -> [(String, TypeQ)] -- ^ Additional column-name and column-type mapping to overwrite default
                   -> [Name]            -- ^ Derivings
                   -> Q [Dec]           -- ^ Result declaration
defineTableFromDB' = tableAlongWithSchema

-- | Generate all HDBC templates using system catalog information.
defineTableFromDB :: IConnection conn
                  => IO conn     -- ^ Connect action to system catalog database
                  -> Driver conn -- ^ Driver definition
                  -> String      -- ^ Schema name
                  -> String      -- ^ Table name
                  -> [Name]      -- ^ Derivings
                  -> Q [Dec]     -- ^ Result declaration
defineTableFromDB connect driver tbl scm = tableAlongWithSchema connect driver tbl scm []

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
  let sql = untypeQuery $ relationalQuery_ config rel sufs
  when (verboseAsCompilerWarning config) . reportWarning $ "Verify with prepare: " ++ sql
  void . runIO $ withConnectionIO connect (\conn -> prepare conn sql)
  Relational.unsafeInlineQuery (return p) (return r) sql (varCamelcaseName qns)
