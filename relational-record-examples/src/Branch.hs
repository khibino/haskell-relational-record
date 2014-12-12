{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Branch where

import DataSource (defineTable)
import Prelude hiding (id, zip)

$(defineTable "main" "branch")
