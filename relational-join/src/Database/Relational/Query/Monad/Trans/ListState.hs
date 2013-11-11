-- |
-- Module      : Database.Relational.Query.Monad.Trans.ListState
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides polymorphic list state definitions.
module Database.Relational.Query.Monad.Trans.ListState (
  TermsContext, primeTermsContext, appendTerm, termsList
  ) where

import Data.DList (DList, toList)
import Data.Monoid (mempty, (<>))
import Control.Applicative (pure)

type TermsContext = DList

primeTermsContext :: TermsContext a
primeTermsContext =  mempty

appendTerm :: a -> TermsContext a -> TermsContext a
appendTerm at ctx = ctx <> pure at

termsList :: TermsContext a -> [a]
termsList =  toList
