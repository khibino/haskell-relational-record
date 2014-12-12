{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Product where

import DataSource (defineTable)
import Prelude hiding (product)

$(defineTable "product")

