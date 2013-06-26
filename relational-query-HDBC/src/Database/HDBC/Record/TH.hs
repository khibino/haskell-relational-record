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
  derivePersistableInstanceFromValue,
  ) where

import Language.Haskell.TH (Q, Dec, Type)
import Database.HDBC (SqlValue)
import Database.HDBC.SqlValueExtra ()
import Database.Record
  (Persistable(persistable), derivedPersistableValueRecord,
   FromSql(recordFromSql), recordFromSql', ToSql(recordToSql), recordToSql')


-- | Template to declare HDBC instances of DB-record against single value type.
derivePersistableInstanceFromValue :: Q Type  -- ^ Type to implement instances
                                   -> Q [Dec] -- ^ Result declarations
derivePersistableInstanceFromValue typ =
  [d| instance Persistable SqlValue $(typ)  where
        persistable = derivedPersistableValueRecord

      instance FromSql SqlValue $(typ)  where
        recordFromSql = recordFromSql'

      instance ToSql SqlValue $(typ)  where
        recordToSql = recordToSql'
    |]
