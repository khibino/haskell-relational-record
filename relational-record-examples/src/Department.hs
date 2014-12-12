{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Department where

import DataSource (defineTable)
import Prelude hiding (id)

$(defineTable "department")
