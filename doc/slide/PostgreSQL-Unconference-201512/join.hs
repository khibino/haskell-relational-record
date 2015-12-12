{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Int
import Data.Time
import Database.Relational.Query
import Database.Relational.Query.TH
import Database.Relational.Query.Monad.Trans.Ordering (Orderings)

import Person (Person (Person), person)
import Birthday (Birthday, birthday)
import qualified Person
import qualified Birthday


personAndBirthday :: Relation () (Person, Birthday)
personAndBirthday =  relation $ do
  p <- query person    -- Join product accumulated
  b <- query birthday
  on $ p ! Person.name' .=. b ! Birthday.name'
  return $ p >< b

personAndBirthdayL :: Relation () (Person, Maybe Birthday)
personAndBirthdayL =  relation $ do
  p <- query person
  b <- queryMaybe birthday  -- Maybe not match
  on $ just (p ! Person.name') .=. b ?! Birthday.name'
  return $ p >< b

sameBirthdayHeisei' :: Relation () (Day, Int64)
sameBirthdayHeisei' =  aggregateRelation $ do
  p <- query person
  b <- query birthday
  on $ p ! Person.name' .=. b ! Birthday.name'
  wheres $ b ! Birthday.day' .>=. value (fromGregorian 1989 1 8)
  gbd <- groupBy $ b ! Birthday.day'
  having $ count (p ! Person.name') .>. value (1 :: Int64)
  return $ gbd >< count (p ! Person.name')

sameBirthdayHeisei :: Relation () (Day, Int64)
sameBirthdayHeisei =  aggregateRelation $ do
  p <- query person
  b <- query birthday
  on $ p ! Person.name' .=. b ! Birthday.name'
  let birthDay = b ! Birthday.day'
  wheres $ birthDay .>=. value (fromGregorian 1989 1 8)
  gbd <- groupBy birthDay
  let personCount = count $ p ! Person.name'
  having $ personCount .>. value 1
  return $ gbd >< personCount

birthdayHeiseiDesc :: Relation () (Day, Int64)
birthdayHeiseiDesc =  aggregateRelation $ do
  p <- query person
  b <- query birthday
  on $ p ! Person.name' .=. b ! Birthday.name'
  let birthDay = b ! Birthday.day'
  wheres $ birthDay .>=. value (fromGregorian 1989 1 8)
  gbd <- groupBy birthDay
  let personCount = count $ p ! Person.name'
  orderBy personCount Desc
  return $ gbd >< personCount

personAndBirthdayO :: Relation () (Person, Birthday)
personAndBirthdayO =  relation $ do
  p <- query person
  b <- query birthday
  on $ p ! Person.name' .=. b ! Birthday.name'
  orderBy (b ! Birthday.day') Asc  -- Specify ordering key
  orderBy (p ! Person.name') Asc
  return $ p >< b

specifyPerson :: Relation String (Person, Birthday)
specifyPerson =  relation' $ do
  pb <- query personAndBirthday
  (ph, ()) <- placeholder (\ph' -> wheres $ pb ! fst' ! Person.name' .=. ph')
  return (ph, pb)


data PersonAndBirthday =
  PersonAndBirthday
  { pbPerson :: Person
  , pbBirthday :: Birthday
  }

$(makeRelationalRecordDefault ''PersonAndBirthday)

personAndBirthdayT :: Relation () PersonAndBirthday
personAndBirthdayT =  relation $ do
  p <- query person
  b <- query birthday
  on $ p ! Person.name' .=. b ! Birthday.name'
  return $ PersonAndBirthday |$| p |*| b  -- Build record phantom type

-- Birthday.day' :: Pi Birthday Day

uncurryPB :: Pi (Person, Birthday) PersonAndBirthday
uncurryPB =  PersonAndBirthday |$| fst' |*| snd'

personAndBirthdayP :: Relation Person PersonAndBirthday
personAndBirthdayP =  relation' $ do
  p <- query person
  b <- query birthday
  (ph, ()) <- placeholder (\ph' -> on $ p .=. ph')
  return $ (ph, PersonAndBirthday |$| p |*| b)

personAndBirthdayP2 :: Relation Person PersonAndBirthday
personAndBirthdayP2 =  relation' $ do
  p <- query person
  b <- query birthday
  (ph0, ()) <- placeholder (\ph0' -> on $ p ! Person.name'     .=. ph0')
  (ph1, ()) <- placeholder (\ph1' -> on $ p ! Person.age'      .=. ph1')
  (ph2, ()) <- placeholder (\ph2' -> on $ p ! Person.family'   .=. ph2')

  return (Person |$| ph0 |*| ph1 |*| ph2, PersonAndBirthday |$| p |*| b)

agesOfFamilies :: Relation () (String, Maybe Int32)
agesOfFamilies =  aggregateRelation $ do
  p <- query person
  gFam <- groupBy $ p ! Person.family'    -- Specify grouping key
  return $ gFam >< sum' (p ! Person.age') -- Aggregated results

agesOfFamiliesO :: Relation () (String, Maybe Int32)
agesOfFamiliesO =  aggregateRelation $ do
  p <- query person
  gFam <- groupBy $ p ! Person.family'
  let s = sum' (p ! Person.age')
  orderBy s Desc    -- Only aggregated value is allowed to pass
  orderBy gFam Asc
  return $ gFam >< s

ageRankOfFamilies :: Relation () ((Int64, String), Int32)
ageRankOfFamilies =  relation $ do
  p <- query person
  return $
    rank `over` do
      partitionBy $ p ! Person.family'  -- Monad to build window
      orderBy (p ! Person.age') Desc
    ><
    p ! Person.family'
    ><
    p ! Person.age'



nonsense :: Relation () (Person, Birthday)
nonsense = personAndBirthday `union` personAndBirthdayO

heiseiBirthday :: MonadRestrict Flat m
               => Projection Flat Birthday -> m ()
heiseiBirthday b = wheres $ b ! Birthday.day' .>=. value (fromGregorian 1989 1 8)

orderByName :: Monad m
            => Projection c Person
            -> Orderings c m ()
orderByName p = orderBy (p ! Person.name') Asc
