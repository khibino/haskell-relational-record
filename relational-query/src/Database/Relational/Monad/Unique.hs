{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Monad.Unique
-- Copyright   : 2014-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions about unique query type
-- to support scalar queries.
module Database.Relational.Monad.Unique
       ( QueryUnique, unsafeUniqueSubQuery, liftToQueryUnique,
         toSubQuery,
       ) where

import Control.Applicative (Applicative)
import Data.DList (DList)

import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.SqlSyntax
  (Duplication, Record, JoinProduct, NodeAttr,
   SubQuery, Predicate, Qualified, flatSubQuery)

import qualified Database.Relational.Record as Record
import Database.Relational.Projectable (PlaceHolders)
import Database.Relational.Monad.Class (MonadQualify, MonadQuery)
import Database.Relational.Monad.Trans.Join (unsafeSubQueryWithAttr)
import Database.Relational.Monad.Trans.ReferredPlaceholders
  (ReferredPlaceholders, extractReferredPlaceholders, referredPlaceholders)
import Database.Relational.Monad.Trans.Restricting (restrictings)
import Database.Relational.Monad.BaseType (ConfigureQuery, askConfig)
import Database.Relational.Monad.Type (QueryCore, extractCore)


-- | Unique query monad type.
newtype QueryUnique a = QueryUnique (ReferredPlaceholders QueryCore a)
                      deriving (MonadQualify ConfigureQuery, MonadQuery, Monad, Applicative, Functor)

-- | Unsafely join sub-query with this unique query.
unsafeUniqueSubQuery :: NodeAttr                 -- ^ Attribute maybe or just
                     -> Qualified SubQuery       -- ^ 'SubQuery' to join
                     -> QueryUnique (Record c r) -- ^ Result joined context and record of 'SubQuery' result.
unsafeUniqueSubQuery a  = QueryUnique . referredPlaceholders . restrictings . unsafeSubQueryWithAttr a

-- igrep TODO: prefix by "unsafe"?
liftToQueryUnique :: ReferredPlaceholders QueryCore a -> QueryUnique a
liftToQueryUnique = QueryUnique

extract :: QueryUnique a
        -> ConfigureQuery ((((a, DList Int), [Predicate Flat]), JoinProduct), Duplication)
extract (QueryUnique c) = extractCore $ extractReferredPlaceholders c

-- | Run 'SimpleQuery' to get 'SubQuery' with 'Qualify' computation.
toSubQuery :: QueryUnique (PlaceHolders p, Record c r) -- ^ 'QueryUnique' to run
           -> ConfigureQuery (DList Int, SubQuery)     -- ^ Result 'SubQuery' with 'Qualify' computation
toSubQuery q = do
  (((((_ph, pj), phs), rs), pd), da) <- extract q
  c <- askConfig
  return (phs, flatSubQuery c (Record.untype pj) da pd rs [])
