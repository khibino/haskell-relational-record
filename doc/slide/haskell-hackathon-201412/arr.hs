{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

import Control.Arrow
import ArrowQuery

import Person (Person, person)
import Birthday (Birthday, birthday)
import qualified Person
import qualified Birthday


personAndJoinA :: QuerySimple () (Projection Flat (Person, Birthday))
personAndJoinA =  proc () -> do
  p <- query -< person
  b <- query -< birthday
  wheres -< p ! Person.name' .=. b ! Birthday.name'
  returnA -< p >< b
