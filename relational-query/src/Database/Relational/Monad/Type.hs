{-# LANGUAGE DataKinds #-}

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

import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.SqlSyntax
  (Duplication, Record, JoinProduct, Tuple, )

import Database.Relational.ExtensibleRecord
import Database.Relational.Projectable (PlaceHolders)
import Database.Relational.Monad.BaseType (ConfigureQuery)
import Database.Relational.Monad.Trans.Join (QueryJoin, extractProduct)
import qualified Database.Relational.Monad.Trans.Placeholders as P
import Database.Relational.Monad.Trans.Restricting (Restrictings, extractRestrict)
import Database.Relational.Monad.Trans.Ordering (Orderings)


-- | Core query monad type used from flat(not-aggregated) query and aggregated query.
type QueryCore = Restrictings Flat (QueryJoin ConfigureQuery)

-- | Extract 'QueryCore' computation.
extractCore :: QueryCore a
            -> ConfigureQuery (((a, [Tuple{-Predicate i j Flat-}]), JoinProduct), Duplication)
extractCore =  extractProduct . extractRestrict

-- | OrderedQuery monad type with placeholder type 'p'. Record must be the same as 'Orderings' context type parameter 'c'.
--   igrep TODO: Make composable with PlaceHolders?
--   igrep TODO: Change `j` of `Record (ExRecord '[]`
type OrderedQuery i j c m p r = P.Placeholders (Orderings c m) i j (PlaceHolders p, Record (ExRecord '[]) (ExRecord '[])  c r)
