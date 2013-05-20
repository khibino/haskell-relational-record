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


groupMemberShip :: QueryJoin (Projection (Maybe Membership, Group))
groupMemberShip =
  [ m >*< g
  | m  <- queryMaybe membership
  , g  <- query      group
  , () <- on $ m !? groupId' .=. just (g ! Group.id')
  ]

userGroup0 :: Relation (Maybe User, Maybe Group)
userGroup0 =
  relation $
  [ u   >*< mg !? snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe $ relation groupMemberShip
          -- Call one subquery via relation layer
          -- Simple implementation.
          -- Complex SQL. Nested table form joins.

  , ()  <- on $ u !? User.id' .=. flatten (mg !? fst') !? userId'

  , ()  <- asc $ u !? User.id'
  ]

-- userGroup1 :: Relation (Maybe User, Maybe Group)
-- userGroup1 =
--   relation $
--   [ u >*< mg !? snd'
--   | u  <- queryMaybe user
--   , mg <- queryMergeMaybe groupMemberShip
--           -- Directly merge another QueryJoin monad.
--           -- Complex implementation.
--           -- Simple SQL. Flat table form joins.

--   , () <- on $ u !? User.id' .=. flatten (mg !? fst') !? userId'

--   , ()  <- asc $ u !? User.id'
--   ]

runAndPrint :: (Show a, IConnection conn, FromSql SqlValue a) => conn -> Relation a -> IO ()
runAndPrint conn rel = do
  putStrLn $ "SQL: " ++ toSQL rel
  records  <- runQuery conn () (fromRelation rel)
  mapM_ print records
  putStrLn ""

run :: IO ()
run =  withConnectionIO connect
       (\conn -> do
           runAndPrint conn userGroup0
--           runAndPrint conn userGroup1
       )

main :: IO ()
main =  run
