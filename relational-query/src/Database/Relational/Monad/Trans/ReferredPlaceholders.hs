{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Relational.Monad.Trans.ReferredPlaceholders where


import Database.Relational.Monad.Class
import Database.Relational.SqlSyntax
  (Record, placeholderOffsets, aggregateKeyRecord, emptyPlaceholderOffsets)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.DList (DList)


-- | Remember referred placeholders\' offsets.
newtype ReferredPlaceholders m a =
  ReferredPlaceholders ((WriterT (DList Int) m) a)
  deriving (Functor, Applicative, Monad, MonadTrans)

referredPlaceholders :: Monad m => m a -> ReferredPlaceholders m a
referredPlaceholders = lift

instance MonadRestrict c m => MonadRestrict c (ReferredPlaceholders m) where
  restrict pd = do
    ReferredPlaceholders . tell . placeholderOffsets $ pd
    referredPlaceholders $ restrict pd

-- igrep TODO: Add placeholders to Relation?
instance MonadQuery m => MonadQuery (ReferredPlaceholders m) where
  setDuplication = referredPlaceholders . setDuplication
  restrictJoin   = referredPlaceholders . restrictJoin
  query'         = referredPlaceholders . query'
  queryMaybe'    = referredPlaceholders . queryMaybe'

instance MonadQualify q m => MonadQualify q (ReferredPlaceholders m) where
  liftQualify = referredPlaceholders . liftQualify

instance MonadAggregate m => MonadAggregate (ReferredPlaceholders m) where
  groupBy r = do
    ReferredPlaceholders . tell $ placeholderOffsets r
    referredPlaceholders . groupBy $ emptyPlaceholderOffsets r
    -- NOTE: ^ The returned record should not have any placeholderOffsets.
    --         Because its placeholderOffsets have already been recorded the above line.
  groupBy' k = do
    ReferredPlaceholders . tell . placeholderOffsets $ aggregateKeyRecord k
    referredPlaceholders (emptyPlaceholderOffsets <$> groupBy' k)

instance MonadPartition c m => MonadPartition c (ReferredPlaceholders m) where
  partitionBy r = do
    ReferredPlaceholders . tell $ placeholderOffsets r
    referredPlaceholders $ partitionBy r

instance MonadReferPlaceholders m => MonadReferPlaceholders (ReferredPlaceholders m) where
  appendPlaceholderOffsets = ReferredPlaceholders . tell


appendPlaceholdersOfRecord :: (Monad m, MonadReferPlaceholders m) => Record c a -> m ()
appendPlaceholdersOfRecord = appendPlaceholderOffsets . placeholderOffsets


extractReferredPlaceholders :: Functor m => ReferredPlaceholders m a -> m (a, DList Int)
extractReferredPlaceholders (ReferredPlaceholders act) = runWriterT act
