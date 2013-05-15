{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Database.HDBC.Record.TH (
  derivePersistableInstanceFromValue,
  ) where

import Language.Haskell.TH (Q, Dec, Type)
import Database.HDBC (SqlValue)
import Database.HDBC.SqlValueExtra ()
import Database.Record
  (Persistable(persistable), derivedPersistableValueRecord,
   FromSql(recordFromSql), recordFromSql', ToSql(recordToSql), recordToSql')


derivePersistableInstanceFromValue :: Q Type -> Q [Dec]
derivePersistableInstanceFromValue typ =
  [d| instance Persistable SqlValue $(typ)  where
        persistable = derivedPersistableValueRecord

      instance FromSql SqlValue $(typ)  where
        recordFromSql = recordFromSql'

      instance ToSql SqlValue $(typ)  where
        recordToSql = recordToSql'
    |]
