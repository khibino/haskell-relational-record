{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Business where

import DataSource (defineTable)

$(defineTable "business")
