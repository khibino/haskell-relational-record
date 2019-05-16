{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module      : Database.HDBC.Record.TH
-- Copyright   : 2013-2019 Kei Hibino
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

import Language.Haskell.TH (Q, Dec, Type, )

import qualified Database.HDBC.Record.InternalTH as Internal
  (derivePersistableInstanceFromConvertible)


derivePersistableInstanceFromConvertible :: Q Type  -- ^ Type to implement instances
                                         -> Q [Dec] -- ^ Result declarations
derivePersistableInstanceFromConvertible =
  Internal.derivePersistableInstanceFromConvertible
{-# DEPRECATED derivePersistableInstanceFromConvertible "internal API, will be dropped in the future releases." #-}
