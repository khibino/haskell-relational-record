{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck.Simple (defaultMain, eqTest)

import Database.Record
  (PersistableType (..),
   FromSql (..), valueRecordFromSql, toRecord,
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

main :: IO ()
main =
  defaultMain
  [ eqTest
    "nestedEq"
    (Membership { user  = User { uid = 1, uname = "Kei Hibino", note = "HRR developer" }
                , group = Group { gid = 1, gname = "Haskellers" }
                } )
    (toRecord ["1", "Kei Hibino", "HRR developer", "1", "Haskellers"]) ]
