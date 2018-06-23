{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Database.Relational.OverloadedInstances
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides basic instances of overloaded projections like tuples..
module Database.Relational.OverloadedInstances () where

import Control.Applicative ((<$>))
#if __GLASGOW_HASKELL__ >= 800
import Database.Relational.OverloadedProjection (projection)
#endif
import Database.Relational.InternalTH.Overloaded (tupleProjection)


$(concat <$> mapM tupleProjection [2 .. 7])
-- Generic instances of tuple types are generated from 2 to 7 in GHC.Generics.
