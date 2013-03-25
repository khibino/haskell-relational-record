{-# LANGUAGE FlexibleContexts #-}
{--# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Database.HDBC.Record.Persistable
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.HDBC.Record.Persistable (
  PersistableRecord, persistableRecord,
  toRecord, fromRecord, width,
  Singleton(runSingleton), singleton,

  persistableSingleton,

  Persistable (..),

  takeRecord
  ) where

import Data.Convertible (Convertible)
import Database.HDBC (SqlValue, fromSql, toSql)


data PersistableRecord a =
  PersistableRecord
  { toRecord   :: [SqlValue] -> a
  , fromRecord :: a -> [SqlValue]
  , width      :: !Int
  }

newtype Singleton a = Singleton { runSingleton :: a }

singleton :: a -> Singleton a
singleton = Singleton

persistableRecord :: ([SqlValue] -> a) -> (a -> [SqlValue]) -> Int -> PersistableRecord a
persistableRecord = PersistableRecord

persistableSingleton :: (Convertible SqlValue a, Convertible a SqlValue)
                     => PersistableRecord (Singleton a)
persistableSingleton =  persistableRecord (Singleton . fromSql . head) ((:[]) . toSql . runSingleton) 1

class Persistable a where
  persistable :: PersistableRecord a

instance (Convertible SqlValue a, Convertible a SqlValue)
         => Persistable (Singleton a)  where
  persistable = persistableSingleton

takeRecord :: PersistableRecord a -> [SqlValue] -> (a, [SqlValue])
takeRecord rec vals = (toRecord rec va, vr) where
  (va, vr) = splitAt (width rec) vals
