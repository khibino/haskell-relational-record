-- |
-- Module      : Database.Relational.Internal.Config
-- Copyright   : 2017-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines configuration datatype used in query products.
module Database.Relational.Internal.Config (
  Config,

  defaultConfig,

  {- field labels of 'Config' type.
     To avoid haddock bug ( https://github.com/haskell/haddock/issues/456 ),
     they are not listed in Config ( ... ). -}
  productUnitSupport,
  chunksInsertSize,
  schemaNameMode,
  normalizedTableName,
  enableWarning,
  verboseAsCompilerWarning,
  disableOverloadedProjection,
  disableSpecializedProjection,
  identifierQuotation,
  nameConfig,

  NameConfig,
  defaultNameConfig,

  {- field labels of 'NameConfig' type.
     To avoid haddock bug ( https://github.com/haskell/haddock/issues/456 ),
     they are not listed in NameConfig ( ... ). -}
  recordConfig,
  relationVarName,

  ProductUnitSupport (..), SchemaNameMode (..), IdentifierQuotation (..),
  ) where

import Language.Haskell.TH.Name.CamelCase (VarName, varCamelcaseName)
import qualified Database.Record.TH as RecordTH


-- | 'NameConfig' type to customize names of expanded templates.
data NameConfig =
  NameConfig
  { recordConfig       ::  RecordTH.NameConfig
  -- ^ Configurations related to the names of generated record types
  --   and their field labels.
  , relationVarName    ::  String -> String -> VarName
  -- ^ Function to build the name of 'Database.Relational.Monad.BaseType.Relation' representing the table.
  --   The first argument is the scheme name, and second argument is the table name.
  }

instance Show NameConfig where
  show = const "<NameConfig>"

-- | Default implementation of 'NameConfig' type.
defaultNameConfig :: NameConfig
defaultNameConfig =
  NameConfig
  { recordConfig    = RecordTH.defaultNameConfig
  , relationVarName = const varCamelcaseName
  }

-- | Unit of product is supported or not.
data ProductUnitSupport = PUSupported | PUNotSupported  deriving Show

-- | Schema name qualify mode in SQL string.
data SchemaNameMode
  = SchemaQualified     -- ^ Schema qualified table name in SQL string
  | SchemaNotQualified  -- ^ Not qualified table name in SQL string
  deriving (Eq, Show)

-- | Configuration for quotation of identifiers of SQL.
data IdentifierQuotation = NoQuotation | Quotation Char deriving Show

-- | Configuration type.
data Config =
  Config
  { productUnitSupport           ::  !ProductUnitSupport
  -- ^ No FROM clause (corresponding the unit of JOIN) is allowed or not.
  , chunksInsertSize             ::  !Int
  -- ^ Threshold count of placeholders in the insert statement with multi-values.
  , schemaNameMode               ::  !SchemaNameMode
  -- ^ 'SchemaNameMode' configuration
  , normalizedTableName          ::  !Bool
  -- ^ If True, schema names become uppercase, and table names become lowercase.
  , enableWarning                ::  !Bool
  -- ^ If True, print warning messages in macros of relational-record.
  , verboseAsCompilerWarning     ::  !Bool
  -- ^ If True, more detailed logs are printed when generating record types from schema.
  , disableOverloadedProjection  ::  !Bool
  -- ^ If True, instance of 'Database.Relational.OverloadedProjection.HasProjection' for each column is NOT generated.
  , disableSpecializedProjection ::  !Bool
  -- ^ If True, 'Database.Relational.Pi.Pi' for each column is NOT generated.
  , identifierQuotation          ::  !IdentifierQuotation
  -- ^ 'IdentifierQuotation' configuration
  , nameConfig                   ::  !NameConfig
  -- ^ 'NameConfig' configuration
  } deriving Show

-- | Default configuration of 'Config'.
--   To change some behaviour of relational-query,
--   use record update syntax:
--
-- @
--   defaultConfig
--     { productUnitSupport            =  'PUSupported'
--     , chunksInsertSize              =  256
--     , schemaNameMode                =  'SchemaQualified'
--     , normalizedTableName           =  True
--     , enableWarning                 =  True
--     , verboseAsCompilerWarning      =  False
--     , disableOverloadedProjection   =  False
--     , disableSpecializedProjection  =  False
--     , identifierQuotation           =  'NoQuotation'
--     , nameConfig                    =
--        defaultNameConfig
--        { recordConfig     =  'RecordTH.defaultNameConfig'
--        , relationVarName  =  \\schema table -> 'varCamelcaseName' $ table ++ "_" ++ scheme
--        -- ^ append the table name after the schema name. e.g. "schemaTable"
--        }
--     }
-- @
defaultConfig :: Config
defaultConfig =
  Config { productUnitSupport            =  PUSupported
         , chunksInsertSize              =  256
         , schemaNameMode                =  SchemaQualified
         , normalizedTableName           =  True
         , enableWarning                 =  True
         , verboseAsCompilerWarning      =  False
         , disableOverloadedProjection   =  False
         , disableSpecializedProjection  =  False
         , identifierQuotation           =  NoQuotation
         , nameConfig                    =  defaultNameConfig
         }
