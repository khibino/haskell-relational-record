{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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
       ( QueryUnique, unsafeUniqueSubQuery,
         toSubQuery, liftToQueryUnique
       ) where

import Control.Applicative (Applicative)
import Control.Monad.Indexed (IxFunctor, IxPointed, IxApplicative, IxMonad)

import Database.Relational.SqlSyntax
  (Duplication, Record, JoinProduct, NodeAttr,
   SubQuery, Tuple, Qualified, flatSubQuery)

import qualified Database.Relational.Record as Record
import Database.Relational.Projectable (PlaceHolders)
import Database.Relational.Monad.Class (MonadQualify, MonadQuery)
import qualified Database.Relational.Monad.Trans.Placeholders as P
import Database.Relational.Monad.Trans.Join (unsafeSubQueryWithAttr)
import Database.Relational.Monad.Trans.Restricting (restrictings)
import Database.Relational.Monad.BaseType (ConfigureQuery, askConfig)
import Database.Relational.Monad.Type (QueryCore, extractCore)


-- | Unique query monad type.
newtype QueryUnique i j a = QueryUnique (P.Placeholders QueryCore i j a)
                      deriving (Functor, IxFunctor, IxPointed, IxApplicative, IxMonad)

deriving instance Applicative (QueryUnique i i)
deriving instance Monad (QueryUnique i i)
deriving instance MonadQualify ConfigureQuery (QueryUnique i i)
deriving instance MonadQuery (QueryUnique i i)

-- | Unsafely join sub-query with this unique query.
unsafeUniqueSubQuery :: NodeAttr                 -- ^ Attribute maybe or just
                     -> Qualified SubQuery       -- ^ 'SubQuery' to join
                     -> QueryUnique i i (Record i j c r) -- ^ Result joined context and record of 'SubQuery' result.
unsafeUniqueSubQuery a  = QueryUnique . P.placeholders . restrictings . unsafeSubQueryWithAttr a

extract :: QueryUnique i j a
        -> ConfigureQuery (((a, [Tuple{-Predicate i j Flat-}]), JoinProduct), Duplication)
extract (QueryUnique c) = extractCore $ P.extractPlaceholders c

-- | Run 'SimpleQuery' to get 'SubQuery' with 'Qualify' computation.
toSubQuery :: QueryUnique i j (PlaceHolders p, Record i j c r) -- ^ 'QueryUnique' to run
           -> ConfigureQuery SubQuery                  -- ^ Result 'SubQuery' with 'Qualify' computation
toSubQuery q = do
  ((((_ph, pj), rs), pd), da) <- extract q
  c <- askConfig
  return $ flatSubQuery c (Record.untype pj) da pd rs []

liftToQueryUnique :: P.Placeholders QueryCore i j a -> QueryUnique i j a
liftToQueryUnique = QueryUnique
