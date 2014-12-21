{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Int
import Database.Record
import Database.Relational.Query
import Database.Relational.Query.TH

import Person (Person (Person), person)
import Birthday (Birthday, birthday)
import qualified Person
import qualified Birthday


personAndBirthday :: Relation () (Person, Birthday)
personAndBirthday =  relation $ do
  p <- query person    -- Join product accumulated
  b <- query birthday
  wheres $ p ! Person.name' .=. b ! Birthday.name'
  return $ p >< b

personAndBirthdayL :: Relation () (Person, Maybe Birthday)
personAndBirthdayL =  relation $ do
  p <- query person
  b <- queryMaybe birthday  -- Maybe not match
  wheres $ just (p ! Person.name') .=. b ?! Birthday.name'
  return $ p >< b

personAndBirthdayO :: Relation () (Person, Birthday)
personAndBirthdayO =  relation $ do
  p <- query person
  b <- query birthday
  wheres $ p ! Person.name' .=. b ! Birthday.name'
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
  wheres $ p ! Person.name' .=. b ! Birthday.name'
  return $ PersonAndBirthday |$| p |*| b  -- Build record phantom type

-- Birthday.day' :: Pi Birthday Day

uncurryPB :: Pi (Person, Birthday) PersonAndBirthday
uncurryPB =  PersonAndBirthday |$| fst' |*| snd'

personAndBirthdayP :: Relation Person PersonAndBirthday
personAndBirthdayP =  relation' $ do
  p <- query person
  b <- query birthday
  (ph, ()) <- placeholder (\ph' -> wheres $ p .=. ph')
  return $ (ph, PersonAndBirthday |$| p |*| b)

placeholder3 :: (Monad m, PersistableWidth t, PersistableWidth t1, PersistableWidth t2, SqlProjectable p, SqlProjectable p1, SqlProjectable p2) => (p t -> p1 t1 -> p2 t2 -> m a) -> m (PlaceHolders t, (PlaceHolders t1, (PlaceHolders t2, a)))
placeholder3 f =
  placeholder (\p0 -> placeholder (\p1 -> placeholder (\p2 -> f p0 p1 p2)))

personAndBirthdayP2 :: Relation ((String, Int32), String) PersonAndBirthday
personAndBirthdayP2 =  relation' $ do
  p <- query person
  b <- query birthday
  (ph0, (ph1, (ph2, ()))) <-
    placeholder3 (\ph0' ph1' ph2' ->
                   wheres $
                   (Person |$| p ! Person.name' |*| p ! Person.age' |*| p ! Person.address')
                   .=.
                   (Person |$| ph0' |*| ph1' |*| ph2') )
  return $ (ph0 >< ph1 >< ph2, PersonAndBirthday |$| p |*| b)
