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
import Database.HDBC.Session (withConnectionIO, handleSqlError')


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

userGroup2 :: Relation (Maybe User, Maybe Group)
userGroup2 =
  relation $
  [ u   >< mg !? snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe . relation $
           [ m >< g
           | m  <- queryMaybe membership
           , g  <- query      group
           , () <- on $ m !? groupId' .=. just (g ! Group.id')
           ]

  , ()  <- on $ u !? User.id' .=. mg !? fst' !?? userId'

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

userGroup2Fail :: Relation (Maybe User, Maybe Group)
userGroup2Fail =
  relation $
  [ u   >< mg !? snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe . relation $
           [ m >< g
           | m  <- queryMaybe membership
           , g  <- query      group
           , () <- on $ m !? groupId' .=. just (g ! Group.id')
           , () <- wheres $ u !? User.id' .>. just (value 0)  -- bad line
           ]

  , ()  <- on $ u !? User.id' .=. mg !? fst' !?? userId'

  , ()  <- asc $ u !? User.id'
  ]

runAndPrint :: (Show a, IConnection conn, FromSql SqlValue a) => conn -> Relation a -> IO ()
runAndPrint conn rel = do
  putStrLn $ "SQL: " ++ sqlFromRelation rel
  records  <- runQuery conn () (fromRelation rel)
  mapM_ print records
  putStrLn ""

run :: IO ()
run =  handleSqlError' $ withConnectionIO connect
       (\conn -> do
           let run' :: (Show a, FromSql SqlValue a) => Relation a -> IO ()
               run' = runAndPrint conn
           run' userGroup0
           run' userGroup1
           run' userGroup2
           run' userGroup0Aggregate
           run' userGroup2Fail
       )

main :: IO ()
main =  run
