{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Person where

import Data.Time
import Data.Int

import Opaleye
import Data.Profunctor.Product.TH

data Person' a b = Person { name :: a, age :: b }
type Person = Person' String Int32
type PersonColumn = Person' (Column PGText) (Column PGInt4)

$(makeAdaptorAndInstance "pPerson" ''Person')

personTable :: Table PersonColumn PersonColumn
personTable = Table "birthdayTable"
              (pPerson Person { name = required "name"
                              , age  = required "age" })

personQuery :: Query PersonColumn
personQuery =  queryTable personTable
