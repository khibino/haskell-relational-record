{-# LANGUAGE MonadComprehensions #-}

import Database.Relational.Query

import qualified User
import User (User, user)
import qualified Group
import Group (Group, group)
import Membership (Membership, groupId', userId', membership)

import PgTestDataSource (connect)
import Database.HDBC.Record.Query (runQuery)
import Database.HDBC.Session (withConnectionIO)

um :: Relation (User, Maybe Membership)
um =  relation $
      [ u >*< m
      | u <- query      user
      , m <- queryMaybe membership
      , () <- on $ just (u ! User.id') .=. m !? userId'
      ]

r0 :: Relation (Maybe User, Maybe Group)
r0 =  relation $
      [ (um' !? fst') >*< g
      | um' <- queryMaybe um
      , g   <- queryMaybe group
      , ()  <- on $ flatten (um' !? snd') !? groupId' .=. g !? Group.id'
      ]

run :: IO ()
run =  withConnectionIO connect
       (\conn -> do
           records  <- runQuery conn () (fromRelation r0)
           mapM_ print records)

main :: IO ()
main =  run
