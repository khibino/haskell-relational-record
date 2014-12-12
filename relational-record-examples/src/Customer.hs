{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Customer where

import DataSource (defineTable)
import Prelude hiding (id)

$(defineTable "customer")
