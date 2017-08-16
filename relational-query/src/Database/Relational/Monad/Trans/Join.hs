{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Trans.Join
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift to basic 'MonadQuery'.
module Database.Relational.Monad.Trans.Join
       ( -- * Transformer into join query
         QueryJoin, join',

         -- * Result
         extractProduct,

         -- * Unsafe API
         unsafeSubQueryWithAttr,
       ) where

import Prelude hiding (product)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans.State (modify, StateT, runStateT)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow (second, (***))
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (Last, getLast))

import Database.Relational.Internal.Product (restrictProduct, growProduct)
import Database.Relational.SqlSyntax
  (Duplication (All), NodeAttr (Just', Maybe), SubQuery,
   Qualified, JoinProduct, Record)

import Database.Relational.Context (Flat)
import Database.Relational.Monad.Trans.JoinState
  (JoinContext, primeJoinContext, updateProduct, joinProduct)
import qualified Database.Relational.Record as Record
import Database.Relational.Projectable (PlaceHolders, unsafeAddPlaceHolders)
import Database.Relational.Monad.BaseType (ConfigureQuery, qualifyQuery, Relation, untypeRelation)
import Database.Relational.Monad.Class (MonadQualify (..), MonadQuery (..))


-- | 'StateT' type to accumulate join product context.
newtype QueryJoin m a =
  QueryJoin (StateT JoinContext (WriterT (Last Duplication) m) a)
  deriving (Monad, Functor, Applicative)

instance MonadTrans QueryJoin where
  lift = QueryJoin . lift . lift

-- | Lift to 'QueryJoin'
join' :: Monad m => m a -> QueryJoin m a
join' = lift

-- | Unsafely update join product context.
updateContext :: Monad m => (JoinContext -> JoinContext) -> QueryJoin m ()
updateContext =  QueryJoin . modify

-- | Add last join product restriction.
updateJoinRestriction :: Monad m => Record Flat (Maybe Bool) -> QueryJoin m ()
updateJoinRestriction e = updateContext (updateProduct d)  where
  d  Nothing  = error "on: Product is empty! Restrict target product is not found!"
  d (Just pt) = restrictProduct pt e

instance MonadQualify q m => MonadQualify q (QueryJoin m) where
  liftQualify = join' . liftQualify

-- | Joinable query instance.
instance MonadQuery (QueryJoin ConfigureQuery) where
  setDuplication     = QueryJoin . lift . tell . Last . Just
  restrictJoin       = updateJoinRestriction
  query'             = queryWithAttr Just'
  queryMaybe' pr     = do
    (ph, pj) <- queryWithAttr Maybe pr
    return (ph, Record.just pj)

-- | Unsafely join sub-query with this query.
unsafeSubQueryWithAttr :: Monad q
                       => NodeAttr                 -- ^ Attribute maybe or just
                       -> Qualified SubQuery       -- ^ 'SubQuery' to join
                       -> QueryJoin q (Record c r) -- ^ Result joined context and record of 'SubQuery' result.
unsafeSubQueryWithAttr attr qsub = do
  updateContext (updateProduct (`growProduct` (attr, qsub)))
  return $ Record.unsafeFromQualifiedSubQuery qsub

-- | Basic monadic join operation using 'MonadQuery'.
queryWithAttr :: NodeAttr
              -> Relation p r
              -> QueryJoin ConfigureQuery (PlaceHolders p, Record c r)
queryWithAttr attr = unsafeAddPlaceHolders . run where
  run rel = do
    q <- liftQualify $ do
      sq <- untypeRelation rel
      qualifyQuery sq
    unsafeSubQueryWithAttr attr q

-- | Run 'QueryJoin' to get 'JoinProduct'
extractProduct :: Functor m => QueryJoin m a -> m ((a, JoinProduct), Duplication)
extractProduct (QueryJoin s) = (second joinProduct *** (fromMaybe All . getLast))
                               <$> runWriterT (runStateT s primeJoinContext)
