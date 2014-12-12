{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Business where

import DataSource (defineTable)
import Prelude hiding (id)

$(defineTable "main" "business")
