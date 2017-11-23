{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, DataKinds, DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module QueryExampleO where

import GHC.Generics (Generic)
import Data.Functor.ProductIsomorphic
import Database.Record

import Database.Relational
import Database.Relational.OverloadedInstances ()
import Database.HDBC.Query.TH (makeRelationalRecord)
import Data.Int (Int32, Int64)

import qualified User
import User (User, user)
import qualified Group
import Group (Group, group)
import Membership (Membership, groupId', userId', membership)


groupMemberShip :: Relation () (Maybe Membership, Group)
groupMemberShip =
  relation
  [ m >< g
  | m  <- queryMaybe membership
  , g  <- query      group
  , () <- on $ (? #groupId) m  .=. just (#id g)
  ]

-- Monadic join style
userGroup0 :: Relation () (Maybe User, Maybe Group)
userGroup0 =
  relation
  [ u   >< mg ?! snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe groupMemberShip

  , ()  <- on  $ (? #id) u .=. mg ?? #fst ? #userId

  , ()  <- asc $ (? #id) u
  ]

haskUserGroup :: Relation () (Maybe User, Maybe Group)
haskUserGroup =
  relation
  [ u   >< mg ?! snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe groupMemberShip

  , ()  <- on  $ (? #id) u .=. mg ?? #fst ? #userId

  , let g = (? #snd) mg

  , ()  <- wheres $ (?? #name) g `likeMaybe` "Hask%"

  , ()  <- orderBy' mg Asc NullsLast
  ]

data UserOrGroup = UserOrGroup { mayUser :: Maybe User, mayGroup :: Maybe Group }
                   deriving (Show, Generic)

$(makeRelationalRecord ''UserOrGroup)

userGroup0' :: Relation () UserOrGroup
userGroup0' =
  relation
  [ UserOrGroup |$| u |*| (? #snd) mg
  | u   <- queryMaybe user
  , mg  <- queryMaybe groupMemberShip

  , ()  <- on  $ (? #id) u .=. mg ?? #fst ? #userId

  , ()  <- asc $ (? #id) u
  ]


haskellUser :: Relation () (String, Maybe String)
haskellUser =
  relation
  [ value "Functional programmer!" >< hu ?!? User.name'
  | let hus = relation
              [ #fst ug
              | ug <- query userGroup0
              , () <- wheres $ #snd ug ?? #name .=. just (value "Haskell")
              ]
  , hu  <- query hus
  , hul <-  queryList hus
  , () <- wheres $ exists hul
  ]

-- Direct join style
userGroup1 :: Relation () (Maybe User, Maybe Group)
userGroup1 =
  relation
  [ u >< g
  | umg <- query $
           user `left` membership `on'` [\ u m -> just (#id u) .=. (? #userId) m ]
           `full` group `on'` [ \ um g -> um ?? #snd ? #groupId .=. (? #id) g ]
  , let um = #fst umg
        u  = (? #fst) um
        g  = #snd umg

  , ()  <- asc $ (? #id) u
  ]

-- Nested monad
userGroup2 :: Relation () (Maybe User, Maybe Group)
userGroup2 =
  relation
  [ u   >< mg ?! snd'
  | u   <- queryMaybe user
  , mg  <- queryMaybe . relation $
           [ m >< g
           | m  <- queryMaybe membership
           , g  <- query      group
           , () <- on $ (? #groupId) m .=. just (#id g)
           ]

  , ()  <- on $ (? #id) u .=.mg ?? #fst ? #userId

  , ()  <- asc $ (? #id) u
  ]

-- Aggregation
userGroupAggregate0 :: Relation () ((Maybe String, Int64), Maybe Bool)
userGroupAggregate0 =
  aggregateRelation
  [ g >< c >< every (uid .<. just (value 3))
  | ug  <- query userGroup0
  , g   <- groupBy (ug ! #snd ?? #name)
  , let uid = ug ! #fst ? #id
  , let c  = count uid
  , ()  <- having $ c `in'` values [1, 2]
  , ()  <- asc c
  ]

user3 :: Relation () (Maybe Int32)
user3 =
  relation
  [ just uid
  | u  <- query user
  , let uid = #id u
  , () <- wheres $ uid .<. value 3
  ]

userGroupAggregate1 :: Relation () ((Maybe String, Int64), Maybe Bool)
userGroupAggregate1 =
  aggregateRelation
  [ g >< c >< every (uid `in'` us)
  | ug  <- query userGroup0
  , g   <- groupBy (ug ! #snd ?? #name)
  , let uid = ug ! #fst ? #id
  , let c  = count uid
  , ()  <- having $ c `in'` values [1, 2]
  , ()  <- asc c
  , us  <- queryList user3
  ]

userGroupAggregate2 :: Relation () ((Maybe String, Int64), Maybe Bool)
userGroupAggregate2 =
  aggregateRelation
  [ g >< c >< every (uid .<. just (value 3))
  | ug  <- query userGroup0
  , g   <- groupBy (ug ! #snd ?? #name)
  , let uid = ug ! #fst ? #id
  , ()  <- wheres $ uid .<. just (value 2)
  , let c  = count uid
  , ()  <- having $ c `in'` values [1, 2]
  , ()  <- asc c
  ]

-- Concatinate operator
userGroupStr :: Relation () (Maybe String)
userGroupStr =
  relation
  [ (?? #name) u ?||? just (value " - ") ?||? (?? #name) g
  | () <- distinct
  , ug <- query userGroup2
  , let u = #fst ug
        g = #snd ug
  ]

-- Type check is imcomplete when nested case
userGroup2Fail :: Relation () (Maybe User, Maybe Group)
userGroup2Fail =
  relation
  [ u   >< (? #snd) mg
  | u   <- queryMaybe user
  , mg  <- queryMaybe . relation $
           [ m >< g
           | m  <- queryMaybe membership
           , g  <- query      group
           , () <- on $ (? #groupId) m .=. just (#id g)
           , () <- wheres $ (? #id) u .>. just (value 0)  -- bad line
           ]

  , ()  <- on $ (? #id) u .=. mg ?? #fst ? #userId

  , ()  <- asc $ (? #id) u
  ]

-- Relation making placeholder
specifiedGroup :: Relation String Group
specifiedGroup =  relation' $ do
  g <- query group
  (ph', ()) <- placeholder (\ph -> wheres $ #name g .=. just ph)
  return (ph', g)

-- Placeholder propagation
userGroup3 :: Relation String (User, Group)
userGroup3 =
  relation'
  [ (ph, u >< g)
  | (ph, umg) <- query' . rightPh
                 $ user `inner` membership `on'` [\ u m -> #id u .=. #userId m ]
                 `inner'` specifiedGroup `on'` [ \ um g -> um ! #snd ! #groupId .=. #id g ]
  , let um = #fst umg
        u  = (! #fst) um
        g  = #snd umg

  , ()  <- asc $ u ! User.id'
  ]

specifiedUser :: Relation String User
specifiedUser =  relation' $ do
  u <- query user
  (ph', ()) <- placeholder (\ph -> wheres $ u ! User.name' .=. just ph)
  return (ph', u)

userGroupU :: Relation (String, String) (User, Group)
userGroupU =
  relation'
  [ (ph, u >< g)
  | (ph, umg) <- query'
                 $ leftPh (specifiedUser
                           `inner'` membership `on'` [\ u m -> u ! User.id' .=. m ! userId' ])
                 `inner'` specifiedGroup `on'` [ \ um g -> um ! snd' ! groupId' .=. g ! Group.id' ]
  , let um = #fst umg
        u  = (! #fst) um
        g  = #snd umg
  ]

-- Window funcions
windowRankByGroup :: Relation () ((Int64, Maybe Int32), (Maybe String, Maybe String))
windowRankByGroup =  relation $ do
  u <- query user
  m <- query membership
  on $ #id u .=. #userId m
  g <- query group
  on $ #id g .=. #groupId m

  let gwindow = do partitionBy $ (! #id) g -- g ! Group.id'
                   asc $ #name u

  return (rank `over` gwindow
          ><
          sum' (#id u) `over` gwindow
          ><
          (#name u
           ><
           #name g))

-- Composed Key
userAndGroup :: Pi (Maybe User, Maybe Group) (Maybe String, Maybe String)
userAndGroup = fst' <?.?> #name
               ><
               snd' <?.?> #name

-- Composed value
specifiedUserAndGroup :: Relation () (Maybe User, Maybe Group)
specifiedUserAndGroup =  relation $ do
  ug <- query userGroup0
  wheres $ ug ! userAndGroup .=. value (Just "Kei Hibino", Just "Haskell")
  return ug

userPrimaryUnique :: Key Unique User Int32
userPrimaryUnique =  derivedUniqueKey

groupPrimaryUnique :: Key Unique Group Int32
groupPrimaryUnique =  derivedUniqueKey

-- Scalar queries
userGroupScalar :: Relation () (Maybe String, Maybe String)
userGroupScalar =  relation $ do
  m  <- query membership
  un <- queryScalar . uniqueRelation'
       $ do (uph, u) <- uniqueQuery' $ derivedUniqueRelation userPrimaryUnique (m ! userId')
            return (uph, #name u)
  gn <- queryScalar . uniqueRelation'
       $ do (uph, g) <- uniqueQuery' $ derivedUniqueRelation groupPrimaryUnique (m ! groupId')
            return (uph, #name g)
  return $ flatten un >< flatten gn

groups :: Relation () (Group, Maybe Int64)
groups =  relation $ do
  g  <- query group
  gc <- queryScalar $ aggregatedUnique group #id count
  return $ g >< gc

doubleValue1 :: Relation () Double
doubleValue1 =  relation .
  return $ value 0.1 .+. value 0.1 .+. value 0.1

doubleValue2 :: Relation () Double
doubleValue2 =  relation .
  return . value $ 0.1 + 0.1 + 0.1
