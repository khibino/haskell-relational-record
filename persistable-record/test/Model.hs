{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Model where

import GHC.Generics (Generic)

import Database.Record
  (PersistableType (..), PersistableWidth (..),
   FromSql (..), valueRecordFromSql,
   ToSql (..), valueRecordToSql, wrapToSql, putRecord)
import Database.Record.KeyConstraint (HasColumnConstraint (..), NotNull, unsafeSpecifyColumnConstraint)
import Database.Record.Persistable (unsafePersistableSqlTypeFromNull, unsafeValueWidth, )


instance PersistableType String where
  persistableType = unsafePersistableSqlTypeFromNull "<null>"


instance PersistableWidth String where
  persistableWidth = unsafeValueWidth

instance PersistableWidth Int where
  persistableWidth = unsafeValueWidth

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
  } deriving (Eq, Show, Generic)

data Group =
  Group
  { gid    ::  Int
  , gname  ::  String
  } deriving (Eq, Show, Generic)

data Membership =
  Membership
  { user   ::  User
  , group  ::  Maybe Group
  } deriving (Eq, Show, Generic)

instance HasColumnConstraint NotNull User where
  columnConstraint = unsafeSpecifyColumnConstraint 0

instance HasColumnConstraint NotNull Group where
  columnConstraint = unsafeSpecifyColumnConstraint 0

instance PersistableWidth User where
instance PersistableWidth Group where

instance FromSql String User where
instance FromSql String Group where
instance FromSql String Membership where

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
