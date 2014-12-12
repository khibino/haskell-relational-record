{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Employee where

import DataSource (defineTable)
import Prelude hiding (id)

$(defineTable "employee")
