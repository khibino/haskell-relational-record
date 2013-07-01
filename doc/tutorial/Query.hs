{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Int (Int32)
import Database.Relational.Query

import qualified User
import User (User, user)
import qualified Group
import Group (Group, group)
import Membership (Membership, groupId', userId', membership)

groupMemberShip :: Relation () (Maybe Membership, Group)
groupMemberShip =
  relation $
  [ m >< g
  | m  <- queryMaybe membership
  , g  <- query      group
  , () <- on $ m ?! groupId' .=. just (g ! Group.id')
  ]

-- Monadic join style
userGroup0 :: Relation () (Maybe User, Maybe Group)
userGroup0 =
  relation $
  [ u   >< mg ?! snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe groupMemberShip

  , ()  <- on $ u ?! User.id' .=. mg ?!? fst' ?! userId'

  , ()  <- asc $ u ?! User.id'
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

-- Aggregation
userGroup0Aggregate :: Relation () ((Maybe String, Int32), Maybe Bool)
userGroup0Aggregate =
  aggregateRelation $
  [ g >< c >< every (uid .<. just (value 3))
  | ug  <- query userGroup0
  , g   <- groupBy (ug ! snd' ?!? Group.name')
  , let uid = ug ! fst' ?! User.id'
  , let c = count uid
  , ()  <- having $ c .<. value 3
  , ()  <- asc $ c
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

main :: IO ()
main = do
    print userGroup0
    print userGroup1
    print userGroup2
    print userGroup0Aggregate
    print userGroup3
    print userGroupU
    print userGroup2Fail
