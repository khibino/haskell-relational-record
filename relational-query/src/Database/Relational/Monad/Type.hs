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
       ) where

import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.SqlSyntax
  (Duplication, JoinProduct, Predicate, )

import Database.Relational.Monad.BaseType (ConfigureQuery)
import Database.Relational.Monad.Trans.Join (QueryJoin, extractProduct)
import Database.Relational.Monad.Trans.Restricting (Restrictings, extractRestrict)


-- | Core query monad type used from flat(not-aggregated) query and aggregated query.
type QueryCore = Restrictings Flat (QueryJoin ConfigureQuery)

-- | Extract 'QueryCore' computation.
extractCore :: QueryCore a
            -> ConfigureQuery (((a, [Predicate Flat]), JoinProduct), Duplication)
extractCore =  extractProduct . extractRestrict
