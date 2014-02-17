{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Example.User where

import Prelude hiding (id)
import Example.DataSource (defineTable)

$(defineTable
    []
    "TEST" "user" [])
