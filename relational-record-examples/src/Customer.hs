{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Customer where

import DataSource (defineTable)

$(defineTable "customer")
