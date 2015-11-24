{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module SinglePKey where

import Language.Haskell.TH.Name.CamelCase (ConName (..))
import DS
import Database.HDBC.PostgreSQL.Persistable ()

$(definePgTable
  "EXAMPLE" "inet_example"
  [ConName n | n <- [''Eq, ''Show]])
