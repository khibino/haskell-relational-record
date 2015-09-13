{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Query.Monad.Unique
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about unique query type
-- to support scalar queries.
module Database.Relational.Query.Monad.Unique (
  QueryUnique, toSubQuery
  ) where

import Control.Applicative (Applicative)

import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Class (MonadQualifyUnique(..), MonadQuery)
import Database.Relational.Query.Monad.Trans.Join (join')
import Database.Relational.Query.Monad.Trans.Restricting (restrictings)
import Database.Relational.Query.Monad.BaseType (ConfigureQuery, askConfig)
import Database.Relational.Query.Monad.Type (QueryCore, extractCore)
import Database.Relational.Query.Projectable (PlaceHolders)

import Database.Relational.Query.Component (Duplication, QueryRestriction)
import Database.Relational.Query.Sub (SubQuery, flatSubQuery, JoinProduct)

-- | Unique query monad type.
newtype QueryUnique a = QueryUnique (QueryCore a)
                      deriving (MonadQuery, Monad, Applicative, Functor)

-- | Lift from qualified table forms into 'QueryUnique'.
queryUnique :: ConfigureQuery a -> QueryUnique a
queryUnique =  QueryUnique . restrictings . join'

-- | Instance to lift from qualified table forms into 'QueryUnique'.
instance MonadQualifyUnique ConfigureQuery QueryUnique where
  liftQualifyUnique = queryUnique

extract :: QueryUnique a
        -> ConfigureQuery (((a, QueryRestriction Flat), JoinProduct), Duplication)
extract (QueryUnique c) = extractCore c

-- | Run 'SimpleQuery' to get 'SubQuery' with 'Qualify' computation.
toSubQuery :: QueryUnique (PlaceHolders p, Projection c r) -- ^ 'QueryUnique' to run
           -> ConfigureQuery SubQuery      -- ^ Result 'SubQuery' with 'Qualify' computation
toSubQuery q = do
  ((((_ph, pj), rs), pd), da) <- extract q
  c <- askConfig
  return $ flatSubQuery c (Projection.untype pj) da pd rs []
