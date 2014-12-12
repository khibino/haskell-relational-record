{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Individual where

import DataSource (defineTable)
import Prelude hiding (id)

$(defineTable "main" "individual")
