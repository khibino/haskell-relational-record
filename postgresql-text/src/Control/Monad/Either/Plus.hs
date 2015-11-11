{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Either.Plus
       ( EitherP (..)
       , eitherP, emap
       , leftP, rightP
       ) where

import Control.Applicative ((<$>), Applicative (..), Alternative (..))
import Control.Monad (MonadPlus (..))
import Data.Monoid (Monoid (..), (<>))


newtype EitherP e a = EitherP { unEitherP :: Either e a }

eitherP :: (a -> c) -> (b -> c) -> EitherP a b -> c
eitherP f g = either f g . unEitherP

emap :: (e0 -> e1) -> EitherP e0 a -> EitherP e1 a
emap f = EitherP . eitherP (Left . f) Right

leftP :: e -> EitherP e a
leftP = EitherP . Left

rightP :: a -> EitherP e a
rightP = EitherP . Right

instance Functor (EitherP e) where
  fmap f (EitherP e)  =  EitherP $ f <$> e

instance Applicative (EitherP e) where
  pure   =  EitherP . pure
  EitherP a <*> EitherP b  =  EitherP $ a <*> b

instance Monad (EitherP e) where
  (EitherP e) >>= f  =  EitherP (e >>= unEitherP . f)
  return             =  EitherP . return

instance Monoid e => Alternative (EitherP e) where
  empty  =  EitherP $ Left mempty
  EitherP a <|> EitherP b  =  EitherP $ a `plus` b  where
    x@(Right _) `plus` _            =  x
    Left  _     `plus` y@(Right _)  =  y
    Left  e1    `plus` Left e2      =  Left $ e1 <> e2

instance Monoid e => MonadPlus (EitherP e) where
  mzero = empty
  mplus = (<|>)
