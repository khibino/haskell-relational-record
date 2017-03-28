{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.Relational.Query.TupleInstances
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines ProductConstructor instances and projection path objects of tuple types.
module Database.Relational.Query.TupleInstances where

import Control.Applicative ((<$>))

import Database.Relational.Query.BaseTH (defineTuplePi, defineTupleProductConstructor)


$(concat <$> mapM defineTupleProductConstructor [2..7])
-- Generic instances of tuple types are generated from 2 to 7 in GHC.Generics.

$(concat <$> mapM defineTuplePi [2..7])
-- Generic instances of tuple types are generated from 2 to 7 in GHC.Generics.
