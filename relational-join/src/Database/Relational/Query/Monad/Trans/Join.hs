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

  -- * Result SQL
  expandSQL
  ) where

import Prelude hiding (product)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (modify, StateT, runStateT)
import Control.Applicative (Applicative)

import Database.Relational.Query.Internal.Context
  (Context, primeContext, updateProduct)
import qualified Database.Relational.Query.Internal.Context as Context
import Database.Relational.Query.Internal.Product (NodeAttr, restrictProduct, growProduct)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Expr (Expr, fromTriBool)
import Database.Relational.Query.Sub (SubQuery, Qualified)

import Database.Relational.Query.Monad.Class (MonadQuery (..))


-- | 'StateT' type to accumulate join product context.
newtype QueryJoin m a =
  QueryJoin { queryState :: StateT Context m a }
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Run 'QueryJoin' to expand context state.
runQueryJoin :: QueryJoin m a  -- ^ Context to expand
             -> Context        -- ^ Initial context
             -> m (a, Context) -- ^ Expanded result
runQueryJoin =  runStateT . queryState

-- | Run 'QueryJoin' with primary empty context to expand context state.
runQueryPrime :: QueryJoin m a  -- ^ Context to expand
              -> m (a, Context) -- ^ Expanded result
runQueryPrime q = runQueryJoin q primeContext

-- | Lift to 'QueryJoin'
join' :: Monad m => m a -> QueryJoin m a
join' =  lift

-- | Unsafely update join product context.
updateContext :: Monad m => (Context -> Context) -> QueryJoin m ()
updateContext =  QueryJoin . modify

-- | Add last join product restriction.
updateJoinRestriction :: Monad m => Expr Projection (Maybe Bool) -> QueryJoin m ()
updateJoinRestriction e = updateContext (updateProduct d)  where
  d  Nothing  = error "on: product is empty!"
  d (Just pt) = restrictProduct pt (fromTriBool e)

-- | Add whole query restriction.
updateRestriction :: Monad m => Expr Projection (Maybe Bool) -> QueryJoin m ()
updateRestriction e = updateContext (Context.addRestriction e)

{-
takeProduct :: QueryJoin (Maybe QueryProductNode)
takeProduct =  queryCore Context.takeProduct

restoreLeft :: QueryProductNode -> NodeAttr -> QueryJoin ()
restoreLeft pL naR = updateContext $ Context.restoreLeft pL naR
-}

-- | Basic query instance.
instance (Monad q, Functor q) => MonadQuery (QueryJoin q) where
  on     =  updateJoinRestriction
  wheres =  updateRestriction
  unsafeSubQuery          = unsafeSubQueryWithAttr
  -- unsafeMergeAnotherQuery = unsafeQueryMergeWithAttr

-- | Unsafely join subquery with this query.
unsafeSubQueryWithAttr :: Monad q
                       => NodeAttr                   -- ^ Attribute maybe or just
                       -> Qualified SubQuery         -- ^ 'SubQuery' to join
                       -> QueryJoin q (Projection r) -- ^ Result joined context and 'SubQuery' result projection.
unsafeSubQueryWithAttr attr qsub = do
  updateContext (updateProduct (`growProduct` (attr, qsub)))
  return $ Projection.fromQualifiedSubQuery qsub

{-
unsafeMergeAnother :: NodeAttr -> QueryJoin a -> QueryJoin a
unsafeMergeAnother naR qR = do
  mayPL <- takeProduct
  v     <- qR
  maybe (return ()) (\pL -> restoreLeft pL naR) mayPL
  return v

unsafeQueryMergeWithAttr :: NodeAttr -> QueryJoin (Projection r) -> QueryJoin (Projection r)
unsafeQueryMergeWithAttr =  unsafeMergeAnother
-}

-- | Run 'QueryJoin' to get SQL string.
expandSQL :: Monad m => QueryJoin m (Projection r, t) -> m ((String, Projection r), t)
expandSQL qp = do
  ((pj, st), c) <- runQueryPrime qp
  return ((Context.composeSQL pj c, pj), st)
