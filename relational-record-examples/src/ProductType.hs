{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module ProductType where

import DataSource (defineTable)
import Prelude hiding (id)

$(defineTable "main" "product_type)

