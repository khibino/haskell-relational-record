{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module SinglePKey where

import GHC.Generics (Generic)
import DS
import Database.Relational.HDBC.PostgreSQL ()

$(definePgTable
  "EXAMPLE" "inet_example"
  [''Eq, ''Show, ''Generic])
