{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Model where

import GHC.Generics (Generic)

import Database.Record
  (PersistableType (..), PersistableWidth (..),
   FromSql (..), valueRecordFromSql,
   ToSql (..), valueRecordToSql)
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

instance PersistableWidth User
instance PersistableWidth Group
instance PersistableWidth Membership

instance FromSql String User
instance FromSql String Group
instance FromSql String Membership

instance ToSql String User
instance ToSql String Group
instance ToSql String Membership
