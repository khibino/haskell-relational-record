{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Record.Instances
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Single column instances for example to load schema of system catalogs.
module Database.Record.Instances () where

import Data.Int (Int16, Int32, Int64)
import Database.Record.TH (deriveNotNullType)

$(fmap concat $ mapM deriveNotNullType
  [ [t| Bool |]
  , [t| Char |]
  , [t| String |]
  , [t| Int16 |]
  , [t| Int32 |]
  , [t| Int64 |]
  ])
