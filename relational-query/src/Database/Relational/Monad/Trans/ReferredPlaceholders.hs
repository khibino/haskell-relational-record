{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Relational.Monad.Trans.ReferredPlaceholders where


import Data.DList (DList)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

import Database.Relational.Monad.Class (MonadQualify (..), MonadQuery (..))
import Database.Relational.SqlSyntax (Record, placeholderOffsets)


-- | Remember referred placeholders\' offsets.
newtype ReferredPlaceholders m a =
  ReferredPlaceholders ((WriterT (DList Int) m) a)
  deriving (Functor, Applicative, Monad, MonadTrans)

referredPlaceholders :: Monad m => m a -> ReferredPlaceholders m a
referredPlaceholders = lift

instance MonadQuery m => MonadQuery (ReferredPlaceholders m) where
  setDuplication   = referredPlaceholders . setDuplication
  restrictJoinNoPh = referredPlaceholders . restrictJoinNoPh
  queryNoPh'       = referredPlaceholders . queryNoPh'
  queryMaybeNoPh'  = referredPlaceholders . queryMaybeNoPh'

instance MonadQualify q m => MonadQualify q (ReferredPlaceholders m) where
  liftQualify = referredPlaceholders . liftQualify

appendPlaceholderOffsets :: Monad m => DList Int -> ReferredPlaceholders m ()
appendPlaceholderOffsets = ReferredPlaceholders . tell


appendPlaceholdersOfRecord :: Monad m => Record c a -> ReferredPlaceholders m ()
appendPlaceholdersOfRecord = appendPlaceholderOffsets . placeholderOffsets


extractReferredPlaceholders :: Functor m => ReferredPlaceholders m a -> m (a, DList Int)
extractReferredPlaceholders (ReferredPlaceholders act) = runWriterT act
