{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Trans.Join
-- Copyright   : 2013-2019 Kei Hibino
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
import Data.Monoid (Last (Last, getLast), (<>))

import Database.Relational.Internal.ContextType (Flat, PureOperand)
import Database.Relational.Internal.Config (addQueryTableAliasAS)
import Database.Relational.SqlSyntax
  (Duplication (All), NodeAttr (Just', Maybe), Predicate, Record,
   SubQuery, Qualified, JoinProduct, restrictProduct, growProduct,
   PlaceholderOffsets, WithPlaceholderOffsets,
   mapWithPlaceholderOffsets, attachEmptyPlaceholderOffsets,
   placeholderOffsetsOfRecord, emptyPlaceholderOffsetsOfRecord, untypeRecord)

import Database.Relational.Monad.Class (liftQualify)
import Database.Relational.Monad.Trans.JoinState
  (JoinContext, primeJoinContext, updateProduct, joinProduct)
import qualified Database.Relational.Record as Record
import Database.Relational.Monad.BaseType (ConfigureQuery, askConfig, qualifyQuery, Relation, untypeRelation)
import Database.Relational.Monad.Class (MonadQualify (..), MonadQuery (..))


-- | 'StateT' type to accumulate join product context.
newtype QueryJoin m a =
  QueryJoin (StateT (WithPlaceholderOffsets JoinContext) (WriterT (Last Duplication) m) a)
  deriving (Monad, Functor, Applicative)

instance MonadTrans QueryJoin where
  lift = QueryJoin . lift . lift

-- | Lift to 'QueryJoin'
join' :: Monad m => m a -> QueryJoin m a
join' = lift

-- | Unsafely update join product context.
updateContext :: Monad m => PlaceholderOffsets -> (JoinContext -> JoinContext) -> QueryJoin m ()
updateContext phs f =
  QueryJoin $ modify (mapWithPlaceholderOffsets (f *** (phs <>)))

-- | Add last join product restriction.
updateJoinRestriction :: Monad m => Predicate Flat -> QueryJoin m ()
updateJoinRestriction e = updateContext (placeholderOffsetsOfRecord e) (updateProduct d)  where
  d  Nothing  = error "on: Product is empty! Restrict target product is not found!"
  d (Just pt) = restrictProduct pt $ untypeRecord e

instance MonadQualify q m => MonadQualify q (QueryJoin m) where
  liftQualify = join' . liftQualify

-- | Joinable query instance.
instance MonadQuery (QueryJoin ConfigureQuery) where
  setDuplication     = QueryJoin . lift . tell . Last . Just
  restrictJoin       = updateJoinRestriction
  query'             = queryWithAttr Just'
  queryMaybe' phs    =
    fmap Record.just . queryWithAttr Maybe phs

-- | Unsafely join sub-query with this query.
unsafeSubQueryWithAttr :: MonadQualify ConfigureQuery q
                       => NodeAttr                 -- ^ Attribute maybe or just
                       -> Qualified SubQuery       -- ^ 'SubQuery' to join
                       -> QueryJoin q (Record c r) -- ^ Result joined context and record of 'SubQuery' result.
unsafeSubQueryWithAttr attr qsub = do
  let r = Record.unsafeFromQualifiedSubQuery qsub
  addAS <- addQueryTableAliasAS <$> liftQualify askConfig
  updateContext (placeholderOffsetsOfRecord r) (updateProduct (`growProduct` (attr, (addAS, qsub))))
  return $ emptyPlaceholderOffsetsOfRecord r

-- | Basic monadic join operation using 'MonadQuery'.
queryWithAttr :: NodeAttr
              -> Record PureOperand p
              -> Relation p r
              -> QueryJoin ConfigureQuery (Record c r)
queryWithAttr attr phs rel = do
  q <- liftQualify $ do
    sq <- untypeRelation rel phs
    qualifyQuery sq
  unsafeSubQueryWithAttr attr q

-- | Run 'QueryJoin' to get 'JoinProduct'
extractProduct :: Functor m => QueryJoin m a -> m ((a, WithPlaceholderOffsets JoinProduct), Duplication)
extractProduct (QueryJoin s) = (second (fmap joinProduct) *** (fromMaybe All . getLast))
                               <$> runWriterT (runStateT s $ attachEmptyPlaceholderOffsets primeJoinContext)
