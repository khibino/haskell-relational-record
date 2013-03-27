{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Record.KeyConstraint
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.Record.KeyConstraint (
  KeyConstraint (index), specifyKeyConstraint,

  Unique, UniqueConstraint,
  NotNull, NotNullConstraint,

  Primary, PrimaryConstraint,
  unique, notNull,

  leftKeyConstraint,
  HasKeyConstraint (constraintKey)
  ) where

newtype KeyConstraint c a = KeyConstraint { index :: Int }

data Unique
data NotNull
data Primary

type UniqueConstraint  = KeyConstraint Unique
type NotNullConstraint = KeyConstraint NotNull
type PrimaryConstraint = KeyConstraint Primary

specifyKeyConstraint :: Int -> KeyConstraint c a
specifyKeyConstraint =  KeyConstraint

unique :: PrimaryConstraint a -> UniqueConstraint a
unique =  specifyKeyConstraint . index

notNull :: PrimaryConstraint a -> NotNullConstraint a
notNull =  specifyKeyConstraint . index


leftKeyConstraint :: KeyConstraint k a -> KeyConstraint k (a, b)
leftKeyConstraint pa = KeyConstraint (index pa)

class HasKeyConstraint c a where
  constraintKey :: KeyConstraint c a

instance HasKeyConstraint c a => HasKeyConstraint c (a, b) where
  constraintKey = leftKeyConstraint constraintKey
