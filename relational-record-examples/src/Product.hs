{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Product where

import DataSource (defineTable)
import Prelude hiding (id, product)

$(defineTable "product")

