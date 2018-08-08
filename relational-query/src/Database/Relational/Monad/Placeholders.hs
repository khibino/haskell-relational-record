{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Relational.Monad.Placeholders where


import           Control.Monad.Indexed
import           Control.Monad.Indexed.State
import           Control.Monad.Indexed.Trans
import           Database.Relational.SqlSyntax.Types
import           Database.Relational.Monad.Class



newtype Placeholders m i j a =
  Placeholders { unPlaceholders :: IxStateT m i j a }
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, IxMonadTrans, Functor)


placeholders :: Monad m => m a -> Placeholders m i i a
placeholders = ilift


instance MonadPlaceholder Placeholders where
  dropPlaceholder = Placeholders . iput . nextIndexOfRecord
