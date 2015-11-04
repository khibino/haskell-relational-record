{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Either.Instances () where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Data.Monoid (Monoid (..), (<>))

instance Monoid e => Alternative (Either e) where
  empty         =  Left mempty
  x@(Right _) <|> _            =  x
  Left  _     <|> y@(Right _)  =  y
  Left  e1    <|> Left e2      =  Left $ e1 <> e2

instance Monoid e => MonadPlus (Either e) where
  mzero = empty
  mplus = (<|>)
