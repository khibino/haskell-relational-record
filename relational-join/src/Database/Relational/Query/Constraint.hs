{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Relational.Query.Constraint (
  Key, index, defineConstraintKey, recordConstraint, projectionKey,

  returnKey, appendConstraint,

  uniqueKey, notNullKey,

  HasConstraintKey (..),

  Primary, Unique, NotNull
  ) where

import Database.Relational.Query.Pi (Pi, leafIndex)
import Database.Relational.Query.Pi.Unsafe (defineColumn)
import Database.Record.KeyConstraint
  (KeyConstraint, specifyKeyConstraint,
   Primary, Unique, NotNull)
import qualified Database.Record.KeyConstraint as C


newtype Key c r ct = Key { index :: Int }

defineConstraintKey :: Int -> Key c r ct
defineConstraintKey =  Key

recordConstraint :: Key c r ct -> KeyConstraint c r
recordConstraint =  specifyKeyConstraint . index

projectionKey :: Key c r ct -> Pi r ct
projectionKey =  defineColumn . index

-- | Unsafe. Make constraint key to add column phantom type
returnKey :: KeyConstraint c r -> Key c r ct
returnKey =  defineConstraintKey . C.index

-- | Unsafe. Make constraint key to add constraint phantom type
appendConstraint :: Pi r ct -> Key c r ct
appendConstraint =  defineConstraintKey . leafIndex


mapConstraint :: (KeyConstraint c0 r -> KeyConstraint c1 r)
              -> Key c0 r ct
              -> Key c1 r ct
mapConstraint f = returnKey . f . recordConstraint

uniqueKey :: Key Primary r ct -> Key Unique r ct
uniqueKey  = mapConstraint C.unique

notNullKey :: Key Primary r ct -> Key NotNull r ct
notNullKey =  mapConstraint C.notNull


class HasConstraintKey c r ct  where
  constraintKey :: Key c r ct

instance HasConstraintKey Primary r ct
         => HasConstraintKey Unique r ct  where
  constraintKey = uniqueKey constraintKey

instance HasConstraintKey Primary r ct
         => HasConstraintKey NotNull r ct  where
  constraintKey = notNullKey constraintKey
