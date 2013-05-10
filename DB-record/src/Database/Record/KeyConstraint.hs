{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.HDBC.Record.KeyConstraint
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Record.KeyConstraint (
  KeyConstraint (index), specifyKeyConstraint,

  Unique, UniqueConstraint,
  NotNull, NotNullConstraint,

  Primary, PrimaryConstraint,
  unique, notNull,

  leftKeyConstraint,
  HasKeyConstraint (keyConstraint),

  derivedUniqueConstraint,
  derivedNotNullConstraint
  ) where

newtype KeyConstraint c r = KeyConstraint { index :: Int }

data Unique
data NotNull
data Primary

type UniqueConstraint  = KeyConstraint Unique
type NotNullConstraint = KeyConstraint NotNull
type PrimaryConstraint = KeyConstraint Primary

specifyKeyConstraint :: Int -> KeyConstraint c r
specifyKeyConstraint =  KeyConstraint

unique :: PrimaryConstraint r -> UniqueConstraint r
unique =  specifyKeyConstraint . index

notNull :: PrimaryConstraint r -> NotNullConstraint r
notNull =  specifyKeyConstraint . index


leftKeyConstraint :: KeyConstraint k a -> KeyConstraint k (a, b)
leftKeyConstraint pa = KeyConstraint (index pa)

class HasKeyConstraint c a where
  keyConstraint :: KeyConstraint c a

instance HasKeyConstraint c a => HasKeyConstraint c (a, b) where
  keyConstraint = leftKeyConstraint keyConstraint

derivedUniqueConstraint :: HasKeyConstraint Primary r => UniqueConstraint r
derivedUniqueConstraint =  unique keyConstraint

derivedNotNullConstraint :: HasKeyConstraint Primary r => NotNullConstraint r
derivedNotNullConstraint =  notNull keyConstraint
