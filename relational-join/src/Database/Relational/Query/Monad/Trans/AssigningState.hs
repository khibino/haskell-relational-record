{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.Relational.Query.Monad.Trans.AssigningState
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides context definition for
-- "Database.Relational.Query.Monad.Trans.Assigning".
module Database.Relational.Query.Monad.Trans.AssigningState (
  -- * Assigning context
  AssigningTerms,
  AssigningContext,

  AssignColumn, AssignTerm,

  primeAssigningContext,

  updateAssignments,

  composeAssignments
  ) where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid ((<>))
import Control.Applicative (pure)

import Database.Relational.Query.Table (sqlWordFromColumn)
import Database.Relational.Query.Sub (AssignColumn, AssignTerm, Assignment)

import Language.SQL.Keyword (Keyword(..), unwordsSQL)
import qualified Language.SQL.Keyword as SQL


-- | Assigning terms.
type AssigningTerms = DList Assignment

-- | Context type for Assignings.
newtype AssigningContext = AssigningContext { assignments :: AssigningTerms }

-- | Initial 'AssigningContext'
primeAssigningContext :: AssigningContext
primeAssigningContext =  AssigningContext DList.empty

-- | Add order-by term.
updateAssignments :: AssignColumn -> AssignTerm -> AssigningContext -> AssigningContext
updateAssignments col term ctx =
  ctx { assignments = assignments ctx <> pure (col, term)  }

-- | Concatinate order-by terms into SQL string.
composeAssignments :: AssigningContext -> String
composeAssignments ac = unwordsSQL $ [SET, assignList `SQL.sepBy` ", "]  where
  assignList = DList.foldr (\ (col, term) r -> [sqlWordFromColumn col, sqlWordFromColumn term] `SQL.sepBy` " = "  : r) []
               $ assignments ac
