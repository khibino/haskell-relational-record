{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Database.Record.TupleInstances () where

import Control.Applicative ((<$>))

import Database.Record.InternalTH (defineTupleInstances)


$(concat <$> mapM defineTupleInstances [2..7])
-- Generic instances of tuple types are generated from 2 to 7 in GHC.Generics.
