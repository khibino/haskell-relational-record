{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Person where

import DS (definePgConTable)

$(definePgConTable "EXAMPLE" "person"
  [''Eq, ''Show])
