{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}

import Database.Record

import Database.Relational.Query
import Database.HDBC (IConnection, SqlValue)
import Data.Int (Int32)

import qualified User
import User (User, user)
import qualified Group
import Group (Group, group)
import Membership (Membership, groupId', userId', membership)

import PgTestDataSource (connect)
import Database.HDBC.Record.Query (runQuery)
import Database.HDBC.Session (withConnectionIO)


groupMemberShip :: Relation (Maybe Membership, Group)
groupMemberShip =
  relation $
  [ m >< g
  | m  <- queryMaybe membership
  , g  <- query      group
  , () <- on $ m !? groupId' .=. just (g ! Group.id')
  ]

userGroup0 :: Relation (Maybe User, Maybe Group)
userGroup0 =
  relation $
  [ u   >< mg !? snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe groupMemberShip

  , ()  <- on $ u !? User.id' .=. mg !? fst' !?? userId'

  , ()  <- asc $ u !? User.id'
  ]

userGroup1 :: Relation (Maybe User, Maybe Group)
userGroup1 =
  relation $
  [ u >< g
  | umg <- query $
           user `left` membership `on'` [\ u m -> just (u ! User.id') .=. m !? userId' ]
           `full` group `on'` [ \ um g -> um !? snd' !?? groupId' .=. g !? Group.id' ]
  , let um = umg ! fst'
        u  = um !? fst'
        g  = umg ! snd'

  , ()  <- asc $ u !? User.id'
  ]

userGroup0Aggregate :: PrimeRelation p ((Maybe String, Int32), Maybe Bool)
userGroup0Aggregate =
  aggregateRelation $
  [ flattenMaybe g >< c >< every (uid .<. just (value 3))
  | ug  <- query userGroup0
  , g   <- groupBy (ug ! snd' !?? Group.name')
  , let uid = ug ! fst' !? User.id'
  , let c = count uid
  , ()  <- having $ c .<. value 3
  , ()  <- asc $ c
  ]

runAndPrint :: (Show a, IConnection conn, FromSql SqlValue a) => conn -> Relation a -> IO ()
runAndPrint conn rel = do
  putStrLn $ "SQL: " ++ sqlFromRelation rel
  records  <- runQuery conn () (fromRelation rel)
  mapM_ print records
  putStrLn ""

run :: IO ()
run =  withConnectionIO connect
       (\conn -> do
           runAndPrint conn userGroup0
           runAndPrint conn userGroup1
           runAndPrint conn userGroup0Aggregate
       )

main :: IO ()
main =  run
