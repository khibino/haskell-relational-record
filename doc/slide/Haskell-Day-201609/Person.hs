{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Person where

import DataSource (definePgConTable)

$(definePgConTable "EXAMPLE" "person"
  [''Eq, ''Show])
