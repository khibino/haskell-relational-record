{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Relational.Query.Pi.Tuple where

import Control.Applicative ((<$>))

import Database.Relational.Query.Internal.TH (defineTuplePi)


$(concat <$> mapM defineTuplePi [2..7])
-- Generic instances of tuple types are generated from 2 to 7 in GHC.Generics.
