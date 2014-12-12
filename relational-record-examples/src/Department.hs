{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Department where

import DataSource (defineTable)

$(defineTable "department")
