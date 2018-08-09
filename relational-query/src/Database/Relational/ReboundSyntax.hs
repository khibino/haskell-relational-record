module Database.Relational.ReboundSyntax
  ( (>>=)
  , (>>)
  , IxMonad
  , ifThenElse
  , ireturn
  , fromLabel
  , fromString
  , module Prelude
  ) where

import Control.Monad.Indexed
import Data.Bool
import Data.String
import GHC.OverloadedLabels (fromLabel)

import Prelude hiding ((>>=), (>>))


(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)


(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
ma >> mb = ma >>>= const mb


ifThenElse :: Bool -> a -> a -> a
ifThenElse b t f = bool t f b
