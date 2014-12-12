{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Officer where

import DataSource (defineTable)

$(defineTable "officer")
