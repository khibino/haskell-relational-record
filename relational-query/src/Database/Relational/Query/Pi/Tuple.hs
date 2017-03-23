{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.Relational.Query.Pi.Tuple
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines projection path objects for tuple types.
module Database.Relational.Query.Pi.Tuple where

import Control.Applicative ((<$>))

import Database.Relational.Query.Internal.TH (defineTuplePi)


$(concat <$> mapM defineTuplePi [2..7])
-- Generic instances of tuple types are generated from 2 to 7 in GHC.Generics.
