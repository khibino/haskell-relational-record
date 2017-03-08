{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Model where

import Control.Applicative ((<$>), (<*>))

import Database.Record
  (PersistableType (..),
   FromSql (..), valueRecordFromSql,
   ToSql (..), valueRecordToSql)
import Database.Record.Persistable (unsafePersistableSqlTypeFromNull)


instance PersistableType String where
  persistableType = unsafePersistableSqlTypeFromNull "<null>"


instance FromSql String String where
  recordFromSql = valueRecordFromSql id

instance FromSql String Int where
  recordFromSql = valueRecordFromSql read

instance ToSql String String where
  recordToSql = valueRecordToSql id

instance ToSql String Int where
  recordToSql = valueRecordToSql show


data User =
  User
  { uid    ::  Int
  , uname  ::  String
  , note   ::  String
  } deriving (Eq, Show)

data Group =
  Group
  { gid    ::  Int
  , gname  ::  String
  } deriving (Eq, Show)

data Membership =
  Membership
  { user   ::  User
  , group  ::  Group
  } deriving (Eq, Show)

instance FromSql String User where
  recordFromSql = User <$> recordFromSql <*> recordFromSql <*> recordFromSql

instance FromSql String Group where
  recordFromSql = Group <$> recordFromSql <*> recordFromSql

instance FromSql String Membership where
  recordFromSql = Membership <$> recordFromSql <*> recordFromSql
