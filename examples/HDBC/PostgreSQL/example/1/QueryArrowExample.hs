{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module QueryArrowExample where

import GHC.Generics (Generic)
import Database.Record

import Database.Relational.Query.Arrow
import Database.HDBC.Query.TH (makeRecordPersistableDefault)
import Data.Int (Int32, Int64)
import Control.Arrow (returnA)

import qualified User
import User (User, user)
import qualified Group
import Group (Group, group)
import Membership (Membership, groupId', userId', membership)


groupMemberShip :: Relation () (Maybe Membership, Group)
groupMemberShip =
  relation $ proc () -> do
    m  <- queryMaybe membership -< ()
    g  <- query      group -< ()
    on -< m ?! groupId' .=. just (g ! Group.id')
    returnA -< m >< g

userGroup0 :: Relation () (Maybe User, Maybe Group)
userGroup0 =
  relation $ proc () -> do
    u   <- queryMaybe user -< ()
    mg  <- queryMaybe groupMemberShip -< ()

    on  -< u ?! User.id' .=. mg ?!? fst' ?! userId'

    asc -< u ?! User.id'
    returnA -< u >< mg ?! snd'

haskUserGroup :: Relation () (Maybe User, Maybe Group)
haskUserGroup =
  relation $ proc () -> do
    u   <- queryMaybe user -< ()
    mg  <- queryMaybe groupMemberShip -< ()

    on -< u ?! User.id' .=. mg ?!? fst' ?! userId'

    let g = mg ?! snd'

    wheres -< g ?!? Group.name' `likeMaybe` "Hask%"

    asc -< u ?! User.id'

    returnA -< u   >< mg ?! snd'

data UserOrGroup = UserOrGroup { mayUser :: Maybe User, mayGroup :: Maybe Group }
                   deriving (Show, Generic)

$(makeRecordPersistableDefault ''UserOrGroup)

userGroup0' :: Relation () UserOrGroup
userGroup0' =
  relation $ proc () -> do
    u   <- queryMaybe user -< ()
    mg  <- queryMaybe groupMemberShip -< ()

    on  -< u ?! User.id' .=. mg ?!? fst' ?! userId'

    asc -< u ?! User.id'

    returnA -< UserOrGroup |$| u |*| mg ?! snd'


haskellUser :: Relation () (String, Maybe String)
haskellUser =
  relation $ proc () -> do
    hu  <- query hus -< ()
    hul <- queryListU hus -< ()
    wheres -< exists hul

    returnA -< value "Functional programmer!" >< hu ?!? User.name'
  where
    hus = relation $ proc () -> do
      ug <- query userGroup0 -< ()
      wheres -< ug ! snd' ?!? Group.name' .=. just (value "Haskell")
      returnA -< ug ! fst'

-- Direct join style
userGroup1 :: Relation () (Maybe User, Maybe Group)
userGroup1 =
  relation $ proc () -> do
    umg <- query $
           user `left` membership `on'` [\ u m -> just (u ! User.id') .=. m ?! userId' ]
           `full` group `on'` [ \ um g -> um ?!? snd' ?! groupId' .=. g ?! Group.id' ]
           -< ()
    let um = umg ! fst'
        u  = um ?! fst'
        g  = umg ! snd'

    asc -< u ?! User.id'

    returnA -< u >< g

-- Nested arrow
userGroup2 :: Relation () (Maybe User, Maybe Group)
userGroup2 =
  relation $ proc () -> do
    u   <- queryMaybe user -< ()
    mg  <- queryMaybe $ relation
           (proc () -> do
               m  <- queryMaybe membership -< ()
               g  <- query      group      -< ()
               on -< m ?! groupId' .=. just (g ! Group.id')
               returnA -< m >< g)
        -< ()

    on -< u ?! User.id' .=. mg ?!? fst' ?! userId'

    asc -< u ?! User.id'

    returnA -< u   >< mg ?! snd'

-- Aggregation
userGroupAggregate0 :: Relation () ((Maybe String, Int64), Maybe Bool)
userGroupAggregate0 =
  aggregateRelation $ proc () -> do
    ug  <- query userGroup0 -< ()
    g   <- groupBy -< ug ! snd' ?!? Group.name'
    let uid = ug ! fst' ?! User.id'
        c  = count uid
    having -< c `in'` values [1, 2]
    asc -< c
    returnA -< g >< c >< every (uid .<. just (value 3))

user3 :: Relation () (Maybe Int32)
user3 =
  relation $ proc () -> do
    u  <- query user -< ()
    let uid = u ! User.id'
    wheres -< uid .<. value 3
    returnA -< just uid

userGroupAggregate1 :: Relation () ((Maybe String, Int64), Maybe Bool)
userGroupAggregate1 =
  aggregateRelation $ proc () -> do
    ug  <- query userGroup0 -< ()
    g   <- groupBy -< ug ! snd' ?!? Group.name'
    let uid = ug ! fst' ?! User.id'
        c  = count uid
    having -< c `in'` values [1, 2]
    asc -< c
    us  <- queryListU user3 -< ()
    returnA -< g >< c >< every (uid `in'` us)

userGroupAggregate2 :: Relation () ((Maybe String, Int64), Maybe Bool)
userGroupAggregate2 =
  aggregateRelation $ proc () -> do
    ug  <- query userGroup0 -< ()
    g   <- groupBy -< ug ! snd' ?!? Group.name'
    let uid = ug ! fst' ?! User.id'
    wheres -< uid .<. just (value 2)
    let c  = count uid
    having -< c `in'` values [1, 2]
    asc -< c
    returnA -< g >< c >< every (uid .<. just (value 3))

-- Concatinate operator
userGroupStr :: Relation () (Maybe String)
userGroupStr =
  relation $ proc () -> do
    distinct -< ()
    ug <- query userGroup2 -< ()
    let u = ug ! fst'
        g = ug ! snd'
    returnA -< u ?!? User.name' ?||? just (value " - ") ?||? g ?!? Group.name'

-- Good scope check of arrow when nested case
{-
userGroup2Fail :: Relation () (Maybe User, Maybe Group)
userGroup2Fail =
  relation $ proc () -> do
    u   <- queryMaybe user -< ()
    mg  <- queryMaybe $ relation
           (proc () -> do
               m  <- queryMaybe membership -< ()
               g  <- query      group      -< ()
               on -< m ?! groupId' .=. just (g ! Group.id')
               wheres -< u ?! User.id' .>. just (value 0)  -- bad line -- not in scope error of `u'
               returnA -< m >< g)
           -< ()
    on  -< u ?! User.id' .=. mg ?!? fst' ?! userId'

    asc -< u ?! User.id'
    returnA -< u   >< mg ?! snd'
 -}

-- Relation making placeholder
specifiedGroup :: Relation String Group
specifiedGroup =  relation' $ proc () -> do
  g <- query group -< ()
  (ph', ()) <- placeholder -< proc ph -> do wheres -< g ! Group.name' .=. just ph
  returnA -< (ph', g)

-- Placeholder propagation
userGroup3 :: Relation String (User, Group)
userGroup3 =
  relation' $ proc () -> do
    (ph, umg) <- (query' . rightPh
                  $ user `inner` membership `on'` [\ u m -> u ! User.id' .=. m ! userId' ]
                  `inner'` specifiedGroup `on'` [ \ um g -> um ! snd' ! groupId' .=. g ! Group.id' ] )
                 -< ()
    let um = umg ! fst'
        u  = um  ! fst'
        g  = umg ! snd'

    asc -< u ! User.id'
    returnA -< (ph, u >< g)

specifiedUser :: Relation String User
specifiedUser =  relation' $ proc () -> do
  u <- query user -< ()
  (ph', ()) <- placeholder -< proc ph -> do wheres -< u ! User.name' .=. just ph
  returnA -< (ph', u)

userGroupU :: Relation (String, String) (User, Group)
userGroupU =
  relation' $ proc () -> do
    (ph, umg) <- query'
                 ( leftPh (specifiedUser
                           `inner'` membership `on'` [\ u m -> u ! User.id' .=. m ! userId' ])
                   `inner'` specifiedGroup `on'` [ \ um g -> um ! snd' ! groupId' .=. g ! Group.id' ] )
                 -< ()
    let um = umg ! fst'
        u  = um  ! fst'
        g  = umg ! snd'
    returnA -< (ph, u >< g)

-- Window funcions
windowRankByGroup :: Relation () ((Int64, Maybe Int32), (Maybe String, Maybe String))
windowRankByGroup =  relation $ proc () -> do
  u <- query user       -< ()
  m <- query membership -< ()
  on -< u ! User.id' .=. m ! userId'
  g <- query group      -< ()
  on -< g ! Group.id' .=. m ! groupId'

  let gwindow = proc () -> do partitionBy -< g ! Group.id'
                              asc -< u ! User.name'

  returnA -<
    rank `over` gwindow
    ><
    sum' (u ! User.id') `over` gwindow
    ><
    (u ! User.name'
     ><
     g ! Group.name')

-- Composed Key
userAndGroup :: Pi (Maybe User, Maybe Group) (Maybe String, Maybe String)
userAndGroup = fst' <?.?> User.name'
               ><
               snd' <?.?> Group.name'

-- Composed value
specifiedUserAndGroup :: Relation () (Maybe User, Maybe Group)
specifiedUserAndGroup =  relation $ proc () -> do
  ug <- query userGroup0 -< ()
  wheres -< ug ! userAndGroup .=. value (Just "Kei Hibino", Just "Haskell")
  returnA -< ug

userPrimaryUnique :: Key Unique User Int32
userPrimaryUnique =  derivedUniqueKey

groupPrimaryUnique :: Key Unique Group Int32
groupPrimaryUnique =  derivedUniqueKey

-- Scalar queries
userGroupScalar :: Relation () (Maybe String, Maybe String)
userGroupScalar =
  relation $ proc() -> do
    m  <- query membership -< ()
    un <- queryScalar uniqUser   -< m
    gn <- queryScalar uniqGroup  -< m
    returnA -< flatten un >< flatten gn
  where
    uniqUser m = uniqueRelation' $ proc () -> do
      (uph, u) <- uniqueQuery' (derivedUniqueRelation userPrimaryUnique $ m ! userId') -< ()
      returnA -< (uph, u ! User.name')
    uniqGroup m = uniqueRelation' $ proc () -> do
      (uph, g) <- uniqueQuery' (derivedUniqueRelation groupPrimaryUnique $ m ! groupId') -< ()
      returnA -< (uph, g ! Group.name')

groups :: Relation () (Group, Maybe Int64)
groups =  relation $ proc () -> do
  g  <- query group -< ()
  gc <- queryScalarU $ aggregatedUnique group Group.id' count -< ()
  returnA -< g >< gc
