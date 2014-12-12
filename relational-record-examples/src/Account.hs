{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Account where

import DataSource (defineTable)
import Prelude hiding (id)

$(defineTable "account")
