{-# LANGUAGE Arrows #-}

import Control.Arrow
import Opaleye

import Person
import Birthday
import qualified Person
import qualified Birthday

personAndBirthday :: Query (PersonColumn, BirthdayColumn)
personAndBirthday = proc () -> do
  p   <- personQuery   -< ()
  b   <- birthdayQuery -< ()
  restrict -< Person.name p .== Birthday.name b
  returnA -< (p, b)
