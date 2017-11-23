{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.User where

import GHC.Generics (Generic)
import Prelude hiding (id)
import Example.DataSource (defineTable)

$(defineTable
    []
    "TEST" "user" [''Generic])
