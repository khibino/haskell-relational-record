{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Person where

import DataSource (defineTable)

$(defineTable "person")
