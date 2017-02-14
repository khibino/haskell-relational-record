{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Query.Monad.Unique
-- Copyright   : 2014-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about unique query type
-- to support scalar queries.
module Database.Relational.Query.Monad.Unique
       ( QueryUnique, unsafeUniqueSubQuery,
         toSubQuery,
       ) where

import Control.Applicative (Applicative)

import Database.Relational.Query.Internal.BaseSQL (Duplication)

import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable (PlaceHolders)
import Database.Relational.Query.Monad.Class (MonadQualify, MonadQuery)
import Database.Relational.Query.Monad.Trans.Join (unsafeSubQueryWithAttr)
import Database.Relational.Query.Monad.Trans.Restricting (restrictings)
import Database.Relational.Query.Monad.BaseType (ConfigureQuery, askConfig)
import Database.Relational.Query.Monad.Type (QueryCore, extractCore)
import Database.Relational.Query.Sub
  (SubQuery, QueryRestriction, Qualified, JoinProduct, NodeAttr, flatSubQuery)


-- | Unique query monad type.
newtype QueryUnique a = QueryUnique (QueryCore a)
                      deriving (MonadQualify ConfigureQuery, MonadQuery, Monad, Applicative, Functor)

-- | Unsafely join sub-query with this unique query.
unsafeUniqueSubQuery :: NodeAttr                     -- ^ Attribute maybe or just
                     -> Qualified SubQuery           -- ^ 'SubQuery' to join
                     -> QueryUnique (Projection c r) -- ^ Result joined context and 'SubQuery' result projection.
unsafeUniqueSubQuery a  = QueryUnique . restrictings . unsafeSubQueryWithAttr a

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
