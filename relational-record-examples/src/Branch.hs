{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Branch where

import DataSource (defineTable)

$(defineTable "branch")
