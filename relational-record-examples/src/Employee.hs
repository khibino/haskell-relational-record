{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Employee where

import DataSource (defineTable)

$(defineTable "employee")
