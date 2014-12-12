{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Officer where

import DataSource (defineTable)
import Prelude hiding (id)

$(defineTable "officer")
