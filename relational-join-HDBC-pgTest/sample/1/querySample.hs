{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}

import Database.Record

import Database.Relational.Query
import Database.HDBC (IConnection, SqlValue)

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
  , mg  <- queryMaybe $ nested groupMemberShip
          -- Call one subquery via relation layer
          -- Simple implementation.
          -- Nested SQL. Nested table form joins.

  , ()  <- on $ u !? User.id' .=. mg !? fst' !?? userId'

  , ()  <- asc $ u !? User.id'
  ]

userGroup1 :: Relation (Maybe User, Maybe Group)
userGroup1 =
  relation $
  [ u >< mg !? snd'
  | u  <- queryMaybe user
  , mg <- queryMaybe groupMemberShip
          -- Directly merge another QueryJoin monad.
          -- Complex implementation.
          -- Flat SQL. Flat table form joins.

  , () <- on $ u !? User.id' .=. mg !? fst' !?? userId'

  , () <- asc $ u !? User.id'
  ]

-- userGroup2 :: Relation (Maybe User, Maybe Group)
-- userGroup2 =
--   relation $
--   [ u >< g
--   | umg <- query $ (user `left` membership) `full` group
--   , let um = umg ! fst'
--         u  = um !? fst'
--         m  = flattenMaybe $ um !? snd'
--         g  = umg ! snd'
--   , ()  <- wheres $ u !? User.id' .=. m !? userId'
--   , ()  <- wheres $ m !? groupId' .=. g !? Group.id'

--   , ()  <- asc $ u !? User.id'
--   ]

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
           -- runAndPrint conn userGroup2
       )

main :: IO ()
main =  run
