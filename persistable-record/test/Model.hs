{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Model where

import Control.Applicative ((<$>), (<*>))

import Database.Record
  (PersistableType (..), PersistableWidth (..),
   FromSql (..), valueRecordFromSql,
   ToSql (..), valueRecordToSql, wrapToSql, putRecord)
import Database.Record.KeyConstraint (HasColumnConstraint (..), NotNull, unsafeSpecifyColumnConstraint)
import Database.Record.Persistable (unsafePersistableSqlTypeFromNull, unsafePersistableRecordWidth)


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
  , group  ::  Maybe Group
  } deriving (Eq, Show)

instance HasColumnConstraint NotNull User where
  columnConstraint = unsafeSpecifyColumnConstraint 0

instance HasColumnConstraint NotNull Group where
  columnConstraint = unsafeSpecifyColumnConstraint 0

instance PersistableWidth User where
  persistableWidth = unsafePersistableRecordWidth 3

instance PersistableWidth Group where
  persistableWidth = unsafePersistableRecordWidth 2

instance FromSql String User where
  recordFromSql = User <$> recordFromSql <*> recordFromSql <*> recordFromSql

instance FromSql String Group where
  recordFromSql = Group <$> recordFromSql <*> recordFromSql

instance FromSql String Membership where
  recordFromSql = Membership <$> recordFromSql <*> recordFromSql

instance ToSql String User where
  recordToSql = wrapToSql $ \u -> do
    putRecord $ uid u
    putRecord $ uname u
    putRecord $ note u

instance ToSql String Group where
  recordToSql = wrapToSql $ \g -> do
    putRecord $ gid g
    putRecord $ gname g

instance ToSql String Membership where
  recordToSql = wrapToSql $ \m -> do
    putRecord $ user m
    putRecord $ group m
