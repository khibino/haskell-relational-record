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
import Database.HDBC.Record.Statement
  (ExecutedStatement, bindTo, execute)
import Database.HDBC.Record.Query
  (runQuery, prepare, fetchUnique, fetchUnique')
import Database.HDBC.Session (withConnectionIO, handleSqlError')


groupMemberShip :: Relation () (Maybe Membership, Group)
groupMemberShip =
  relation $
  [ m >< g
  | m  <- queryMaybe membership
  , g  <- query      group
  , () <- on $ m ?! groupId' .=. just (g ! Group.id')
  ]

groupMemberShipE :: Relation () (Maybe Membership, Group)
groupMemberShipE =
  relation $
  [ m >< g
  | m  <- queryMaybe membership
  , g  <- query      group
  , () <- onE $ m .! groupId' .=. g ! Group.id'
  ]

-- Monadic join style
userGroup0 :: Relation () (Maybe User, Maybe Group)
userGroup0 =
  relation $
  [ u   >< mg ?! snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe groupMemberShip

  , ()  <- on  $ u ?! User.id' .=. mg ?!? fst' ?! userId'

  , ()  <- asc $ u ?! User.id'
  ]

userGroup0E :: Relation () (Maybe User, Maybe Group)
userGroup0E =
  relation $
  [ u   >< mg ?! snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe groupMemberShipE

  , ()  <- onE $ u .! User.id' .=. mg ?!? fst' .! userId'

  , ()  <- asc $ u ?! User.id'
  ]

haskellUser :: Relation () (String, Maybe String)
haskellUser =
  relation
  [ value "Functional programmer!" >< hu ?!? User.name'
  | let hus = relation
              [ ug ! fst'
              | ug <- query userGroup0
              , () <- wheres $ ug ! snd' ?!? Group.name' .=. just (value "Haskell")
              ]
  , hu <- query hus
  , () <- wheres . exists . queryList $ hus
  ]

-- Direct join style
userGroup1 :: Relation () (Maybe User, Maybe Group)
userGroup1 =
  relation $
  [ u >< g
  | umg <- query $
           user `left` membership `on'` [\ u m -> just (u ! User.id') .=. m ?! userId' ]
           `full` group `on'` [ \ um g -> um ?!? snd' ?! groupId' .=. g ?! Group.id' ]
  , let um = umg ! fst'
        u  = um ?! fst'
        g  = umg ! snd'

  , ()  <- asc $ u ?! User.id'
  ]

{-
userGroup1E :: Relation () (Maybe User, Maybe Group)
userGroup1E =
  relation $
  [ u >< g
  | umg <- query $
           user `left` membership `on'` [\ u m -> u ! User.id' .=. m .! userId' ]
           `full` group `on'` [ \ um g -> um ?!? snd' .! groupId' .=. g .! Group.id' ]
  , let um = umg ! fst'
        u  = um ?! fst'
        g  = umg ! snd'

  , ()  <- asc $ u ?! User.id'
  ]
-}

-- Nested monad
userGroup2 :: Relation () (Maybe User, Maybe Group)
userGroup2 =
  relation $
  [ u   >< mg ?! snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe . relation $
           [ m >< g
           | m  <- queryMaybe membership
           , g  <- query      group
           , () <- on $ m ?! groupId' .=. just (g ! Group.id')
           ]

  , ()  <- on $ u ?! User.id' .=. mg ?!? fst' ?! userId'

  , ()  <- asc $ u ?! User.id'
  ]

userGroup2E :: Relation () (Maybe User, Maybe Group)
userGroup2E =
  relation $
  [ u   >< mg ?! snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe . relation $
           [ m >< g
           | m  <- queryMaybe membership
           , g  <- query      group
           , () <- onE $ m .! groupId' .=. g ! Group.id'
           ]

  , ()  <- onE $ u .! User.id' .=. mg ?!? fst' .! userId'

  , ()  <- asc $ u ?! User.id'
  ]

-- Aggregation
userGroupAggregate0 :: Relation () ((Maybe String, Int32), Maybe Bool)
userGroupAggregate0 =
  aggregateRelation $
  [ g >< c >< every (uid .<. just (value 3))
  | ug  <- query userGroup0
  , g   <- groupBy (ug ! snd' ?!? Group.name')
  , let uid = ug ! fst' ?! User.id'
  , let c  = count uid
  , ()  <- having $ c `in'` values [1, 2]
  , ()  <- asc $ c
  ]

user3 :: Relation () (Maybe Int32)
user3 =
  relation $
  [ just uid
  | u  <- query user
  , let uid = u ! User.id'
  , () <- wheres $ uid .<. value 3
  ]

userGroupAggregate1 :: Relation () ((Maybe String, Int32), Maybe Bool)
userGroupAggregate1 =
  aggregateRelation $
  [ g >< c >< every (uid `in'` queryList user3)
  | ug  <- query userGroup0
  , g   <- groupBy (ug ! snd' ?!? Group.name')
  , let uid = ug ! fst' ?! User.id'
  , let c  = count uid
  , ()  <- having $ c `in'` values [1, 2]
  , ()  <- asc $ c
  ]

-- Concatinate operator
userGroupStr :: Relation () (Maybe String)
userGroupStr =
  relation $
  [ u ?!? User.name' ?||? just (value " - ") ?||? g ?!? Group.name'
  | ug <- query userGroup2
  , let u = ug ! fst'
        g = ug ! snd'
  ]

-- Type check is imcomplete when nested case
userGroup2Fail :: Relation () (Maybe User, Maybe Group)
userGroup2Fail =
  relation $
  [ u   >< mg ?! snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe . relation $
           [ m >< g
           | m  <- queryMaybe membership
           , g  <- query      group
           , () <- on $ m ?! groupId' .=. just (g ! Group.id')
           , () <- wheres $ u ?! User.id' .>. just (value 0)  -- bad line
           ]

  , ()  <- on $ u ?! User.id' .=. mg ?!? fst' ?! userId'

  , ()  <- asc $ u ?! User.id'
  ]

-- Relation making placeholder
specifiedGroup :: Relation String Group
specifiedGroup =  relation' $ do
  g <- query group
  (ph', ()) <- placeholder (\ph -> wheres $ g ! Group.name' .=. just ph)
  return (ph', g)

-- Placeholder propagation
userGroup3 :: Relation String (User, Group)
userGroup3 =
  relation' $
  [ (ph, u >< g)
  | (ph, umg) <- query' . rightPh
                 $ user `inner` membership `on'` [\ u m -> u ! User.id' .=. m ! userId' ]
                 `inner'` specifiedGroup `on'` [ \ um g -> um ! snd' ! groupId' .=. g ! Group.id' ]
  , let um = umg ! fst'
        u  = um  ! fst'
        g  = umg ! snd'

  , ()  <- asc $ u ! User.id'
  ]

specifiedUser :: Relation String User
specifiedUser =  relation' $ do
  u <- query user
  (ph', ()) <- placeholder (\ph -> wheres $ u ! User.name' .=. just ph)
  return (ph', u)

userGroupU :: Relation (String, String) (User, Group)
userGroupU =
  relation' $
  [ (ph, u >< g)
  | (ph, umg) <- query'
                 $ leftPh (specifiedUser
                           `inner'` membership `on'` [\ u m -> u ! User.id' .=. m ! userId' ])
                 `inner'` specifiedGroup `on'` [ \ um g -> um ! snd' ! groupId' .=. g ! Group.id' ]
  , let um = umg ! fst'
        u  = um  ! fst'
        g  = umg ! snd'
  ]

runAndPrint :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
            => conn -> Relation p a -> p -> IO ()
runAndPrint conn rel param = do
  putStrLn $ "SQL: " ++ sqlFromRelation rel
  records  <- runQuery conn param (relationalQuery rel)
  mapM_ print records
  putStrLn ""

run :: IO ()
run =  handleSqlError' $ withConnectionIO connect
       (\conn -> do
           let run' :: (Show a, FromSql SqlValue a, ToSql SqlValue p)
                    => Relation p a -> p -> IO ()
               run' = runAndPrint conn
           run' userGroup0 ()
           run' userGroup0E ()
           run' haskellUser ()
           run' userGroup1 ()
           -- run' userGroup1E ()
           run' userGroup2 ()
           run' userGroup2E ()
           run' userGroupAggregate0 ()
           run' userGroupAggregate1 ()
           run' userGroup3 "Haskell"
           run' userGroupU ("Kei Hibino", "Haskell")
           run' userGroupStr ()
           run' userGroup2Fail ()
       )

runU :: Show a => (ExecutedStatement (User, Group) -> IO a) -> IO ()
runU f = handleSqlError' $ withConnectionIO connect
        (\conn -> do
            putStrLn $ "SQL: " ++ sqlFromRelation userGroupU
            pq <- prepare conn (relationalQuery userGroupU)
            let bs = ("Kei Hibino", "Haskell") `bindTo` pq
            es <- execute bs
            r  <- f es
            print r
        )

runAll :: IO ()
runAll = do
  runU fetchUnique
  runU fetchUnique'
  run

main :: IO ()
main =  runAll
