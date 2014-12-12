{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Account where

import DataSource (defineTable)

$(defineTable "account")
