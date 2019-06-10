{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Database.Relational.HDBC.Persistable
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides HDBC instance definitions of DB-record.
module Database.Relational.HDBC.Persistable () where

import Database.Record (PersistableType (..))
import Database.Record.Persistable (unsafePersistableSqlTypeFromNull)
import Database.Relational.HDBC.InternalTH (derivePersistableInstancesFromConvertibleSqlValues)

import Database.HDBC (SqlValue(SqlNull))

instance PersistableType SqlValue  where
  persistableType = unsafePersistableSqlTypeFromNull SqlNull

$(derivePersistableInstancesFromConvertibleSqlValues)
