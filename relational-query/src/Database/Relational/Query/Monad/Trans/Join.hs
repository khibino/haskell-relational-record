{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Database.Relational.Query.Monad.Trans.Join
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift to basic 'MonadQuery'.
module Database.Relational.Query.Monad.Trans.Join (
  -- * Transformer into join query
  QueryJoin, join',

  -- * Result
  extractProduct
  ) where

import Prelude hiding (product)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans.State (modify, StateT, runStateT)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (first, second)

import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Monad.Trans.JoinState
  (JoinContext, primeJoinContext, updateProduct, joinProduct)
import Database.Relational.Query.Internal.Product (NodeAttr, restrictProduct, growProduct)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Expr (Expr, fromJust)
import Database.Relational.Query.Component (Duplication (Distinct))
import Database.Relational.Query.Sub (SubQuery, Qualified, JoinProduct)

import Database.Relational.Query.Monad.Class (MonadQuery (..))


-- | 'StateT' type to accumulate join product context.
newtype QueryJoin m a =
  QueryJoin (StateT JoinContext (WriterT Duplication m) a)
  deriving (Monad, Functor, Applicative)

-- | Lift to 'QueryJoin'
join' :: Monad m => m a -> QueryJoin m a
join' =  QueryJoin . lift . lift

-- | Unsafely update join product context.
updateContext :: Monad m => (JoinContext -> JoinContext) -> QueryJoin m ()
updateContext =  QueryJoin . modify

-- | Add last join product restriction.
updateJoinRestriction :: Monad m => Expr Flat (Maybe Bool) -> QueryJoin m ()
updateJoinRestriction e = updateContext (updateProduct d)  where
  d  Nothing  = error "on: Product is empty! Restrict target product is not found!"
  d (Just pt) = restrictProduct pt (fromJust e)

-- | Joinable query instance.
instance (Monad q, Functor q) => MonadQuery (QueryJoin q) where
  distinct           = QueryJoin . lift $ tell Distinct
  restrictJoin       = updateJoinRestriction
  unsafeSubQuery     = unsafeSubQueryWithAttr

-- | Unsafely join subquery with this query.
unsafeSubQueryWithAttr :: Monad q
                       => NodeAttr                        -- ^ Attribute maybe or just
                       -> Qualified SubQuery              -- ^ 'SubQuery' to join
                       -> QueryJoin q (Projection Flat r) -- ^ Result joined context and 'SubQuery' result projection.
unsafeSubQueryWithAttr attr qsub = do
  updateContext (updateProduct (`growProduct` (attr, qsub)))
  return $ Projection.unsafeFromQualifiedSubQuery qsub

-- | Run 'QueryJoin' to get 'JoinProduct'
extractProduct :: Functor m => QueryJoin m a -> m ((a, JoinProduct), Duplication)
extractProduct (QueryJoin s) = first (second joinProduct) <$> runWriterT (runStateT s primeJoinContext)
