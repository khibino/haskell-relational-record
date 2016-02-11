{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module      : Database.HDBC.Record.TH
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides DB-record templates depends on HDBC.
module Database.HDBC.Record.TH (
  derivePersistableInstanceFromConvertible,
  ) where

import Data.Convertible (convert)
import Language.Haskell.TH (Q, Dec, Type)
import Database.HDBC (SqlValue)
import Database.HDBC.SqlValueExtra ()
import Database.Record (FromSql (..), ToSql(..))
import Database.Record.FromSql (valueRecordFromSql)
import Database.Record.ToSql (valueRecordToSql)


-- | Template to declare HDBC instances of DB-record against single value type.
derivePersistableInstanceFromConvertible :: Q Type  -- ^ Type to implement instances
                                         -> Q [Dec] -- ^ Result declarations
derivePersistableInstanceFromConvertible typ =
  [d| instance FromSql SqlValue $(typ)  where
        recordFromSql = valueRecordFromSql convert

      instance ToSql SqlValue $(typ)  where
        recordToSql = valueRecordToSql convert
    |]
