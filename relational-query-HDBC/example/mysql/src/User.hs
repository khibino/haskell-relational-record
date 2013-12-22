{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module User where

import Prelude hiding (id)
import MySQLTestDataSource (defineTable)

$(defineTable
    []
    "TEST" "user" [])
