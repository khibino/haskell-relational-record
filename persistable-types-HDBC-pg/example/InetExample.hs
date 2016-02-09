{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module SinglePKey where

import DS
import Database.HDBC.PostgreSQL.Persistable ()

$(definePgTable
  "EXAMPLE" "inet_example"
  [''Eq, ''Show])
