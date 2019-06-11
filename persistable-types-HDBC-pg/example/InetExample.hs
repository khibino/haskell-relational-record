{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module SinglePKey where

import GHC.Generics (Generic)
import DS
import Database.HDBC.PostgreSQL.Persistable ()

$(definePgTable
  "EXAMPLE" "inet_example"
  [''Eq, ''Show, ''Generic])
