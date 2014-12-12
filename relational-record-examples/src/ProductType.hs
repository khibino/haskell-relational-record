{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module ProductType where

import DataSource (defineTable)

$(defineTable "product_type")
