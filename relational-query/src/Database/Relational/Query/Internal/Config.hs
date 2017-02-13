-- |
-- Module      : Database.Relational.Query.Internal.Config
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines configuration datatype used in query products.
module Database.Relational.Query.Internal.Config (
  NameConfig (..),
  ProductUnitSupport (..), SchemaNameMode (..), IdentifierQuotation (..),
  Config ( productUnitSupport
         , chunksInsertSize
         , schemaNameMode
         , normalizedTableName
         , verboseAsCompilerWarning
         , identifierQuotation
         , nameConfig),
  defaultConfig,
  ) where

import Language.Haskell.TH.Name.CamelCase (VarName, varCamelcaseName)
import qualified Database.Record.TH as RecordTH


-- | 'NameConfig' type to customize names of expanded templates.
data NameConfig =
  NameConfig
  { recordConfig       ::  RecordTH.NameConfig
  , relationVarName    ::  String -> String -> VarName
  }

instance Show NameConfig where
  show = const "<NameConfig>"

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
  { productUnitSupport        ::  !ProductUnitSupport
  , chunksInsertSize          ::  !Int
  , schemaNameMode            ::  !SchemaNameMode
  , normalizedTableName       ::  !Bool
  , verboseAsCompilerWarning  ::  !Bool
  , identifierQuotation       ::  !IdentifierQuotation
  , nameConfig                ::  !NameConfig
  } deriving Show

-- | Default configuration.
defaultConfig :: Config
defaultConfig =
  Config { productUnitSupport        =  PUSupported
         , chunksInsertSize          =  256
         , schemaNameMode            =  SchemaQualified
         , normalizedTableName       =  True
         , verboseAsCompilerWarning  =  False
         , identifierQuotation       =  NoQuotation
         , nameConfig                =  NameConfig { recordConfig     =  RecordTH.defaultNameConfig
                                                   , relationVarName  =  const varCamelcaseName
                                                   }
         }
