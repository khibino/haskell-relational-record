{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Query.Constraint (
  Key, defineConstraintKey, recordConstraint, projectionKey,

  unique, notNull,

  HasConstraintKey (..),

  Primary, Unique, NotNull
  ) where

import Database.Relational.Query.Pi.Unsafe (Pi, defineColumn)
import Database.Record.KeyConstraint
  (KeyConstraint, specifyKeyConstraint,
   Primary, Unique, NotNull)
import qualified Database.Record.KeyConstraint as C


newtype Key c r f = Key { index :: Int }

defineConstraintKey :: Int -> Key c r f
defineConstraintKey =  Key

recordConstraint :: Key c r ft -> KeyConstraint c r
recordConstraint =  specifyKeyConstraint . index

projectionKey :: Key c r ft -> Pi r ft
projectionKey =  defineColumn . index

returnKey :: KeyConstraint c r -> Key c r ft
returnKey =  defineConstraintKey . C.index


mapConstraint :: (KeyConstraint c0 r -> KeyConstraint c1 r)
              -> Key c0 r f
              -> Key c1 r f
mapConstraint f = returnKey . f . recordConstraint

unique :: Key Primary r f -> Key Unique r f
unique  = mapConstraint C.unique

notNull :: Key Primary r f -> Key NotNull r f
notNull = mapConstraint C.notNull


class HasConstraintKey c r f  where
  constraintKey :: Key c r f

instance HasConstraintKey Primary r f
         => HasConstraintKey Unique r f  where
  constraintKey = unique constraintKey

instance HasConstraintKey Primary r f
         => HasConstraintKey NotNull r f  where
  constraintKey = notNull constraintKey
