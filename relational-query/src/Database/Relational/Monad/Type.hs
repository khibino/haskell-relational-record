-- |
-- Module      : Database.Relational.Monad.Type
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines core query type.
module Database.Relational.Monad.Type
       ( -- * Core query monad
         QueryCore, extractCore,
         OrderedQuery,
       ) where

import Database.Relational.Internal.BaseSQL (Duplication)

import Database.Relational.Sub (JoinProduct, QueryRestriction)
import Database.Relational.Context (Flat)
import Database.Relational.Projection (Projection)
import Database.Relational.Projectable (PlaceHolders)
import Database.Relational.Monad.BaseType (ConfigureQuery)
import Database.Relational.Monad.Trans.Join (QueryJoin, extractProduct)
import Database.Relational.Monad.Trans.Restricting (Restrictings, extractRestrict)
import Database.Relational.Monad.Trans.Ordering (Orderings)


-- | Core query monad type used from flat(not-aggregated) query and aggregated query.
type QueryCore = Restrictings Flat (QueryJoin ConfigureQuery)

-- | Extract 'QueryCore' computation.
extractCore :: QueryCore a
            -> ConfigureQuery (((a, QueryRestriction Flat), JoinProduct), Duplication)
extractCore =  extractProduct . extractRestrict

-- | OrderedQuery monad type with placeholder type 'p'. Projection must be the same as 'Orderings' context type parameter 'c'.
type OrderedQuery c m p r = Orderings c m (PlaceHolders p, Projection c r)
