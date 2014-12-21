{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Birthday where

import Data.Time

import Opaleye
import Data.Profunctor.Product.TH

data Birthday' a b = Birthday { name :: a, day :: b }
type Birthday = Birthday' String Day
type BirthdayColumn = Birthday' (Column PGText) (Column PGDate)

$(makeAdaptorAndInstance "pBirthday" ''Birthday')

birthdayTable :: Table BirthdayColumn BirthdayColumn
birthdayTable = Table "birthdayTable"
                (pBirthday Birthday { name = required "name"
                                    , day = required "day" })

birthdayQuery :: Query BirthdayColumn
birthdayQuery =  queryTable birthdayTable
