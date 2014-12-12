{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Individual where

import DataSource (defineTable)

$(defineTable "individual")
