{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.HDBC.Record.KeyConstraint (
  KeyConstraint (index), specifyKeyConstraint,

  Unique,
  NotNull,

  Primary,
  unique, notNull,

  leftKeyConstraint,
  HasKeyConstraint (constraintKey)
  ) where

newtype KeyConstraint c a = KeyConstraint { index :: Int }

data Unique
data NotNull
data Primary

type UniqueKey  = KeyConstraint Unique
type NotNullKey = KeyConstraint NotNull
type PrimaryKey = KeyConstraint Primary

specifyKeyConstraint :: Int -> KeyConstraint c a
specifyKeyConstraint =  KeyConstraint

unique :: PrimaryKey a -> UniqueKey a
unique =  specifyKeyConstraint . index

notNull :: PrimaryKey a -> NotNullKey a
notNull =  specifyKeyConstraint . index


leftKeyConstraint :: KeyConstraint k a -> KeyConstraint k (a, b)
leftKeyConstraint pa = KeyConstraint (index pa)

class HasKeyConstraint c a where
  constraintKey :: KeyConstraint c a

instance HasKeyConstraint c a => HasKeyConstraint c (a, b) where
  constraintKey = leftKeyConstraint constraintKey
