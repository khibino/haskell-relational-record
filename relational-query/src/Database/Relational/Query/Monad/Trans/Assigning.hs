{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Query.Monad.Trans.Assigning
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift
-- from context into context with assigning.
module Database.Relational.Query.Monad.Trans.Assigning (
  -- * Transformer into context with assignments
  Assignings, assignings,

  -- * API of context with assignments
  assignTo, (!#), (<-#), AssignTarget,

  -- * Result SQL set clause
  extractAssignments
  ) where

import Database.Relational.Query.Context (Flat)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT, runStateT, modify)
import Control.Applicative (Applicative, (<$>))
import Control.Arrow ((>>>), second)

import Database.Relational.Query.Component (Assignments)
import Database.Relational.Query.Monad.Trans.AssigningState
  (AssigningContext, primeAssigningContext, updateAssignments, assignments)
import Database.Relational.Query.Pi (Pi)
import Database.Relational.Query.Table (Table)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Monad.Class (MonadRestrict(..))

-- | Type to accumulate assigning context.
--   Type 'r' is table record type.
newtype Assignings r m a =
  Assignings { assigningState :: StateT AssigningContext m a }
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Run 'Assignings' to expand context state.
runAssignings :: Assignings r m a        -- ^ Context to expand
              -> AssigningContext        -- ^ Initial context
              -> m (a, AssigningContext) -- ^ Expanded result
runAssignings =  runStateT . assigningState

-- | Run 'Assignings' with primary empty context to expand context state.
runAssigningsPrime :: Assignings r m a        -- ^ Context to expand
                   -> m (a, AssigningContext) -- ^ Expanded result
runAssigningsPrime q = runAssignings q $ primeAssigningContext

-- | Lift to 'Assignings'
assignings :: Monad m => m a -> Assignings r m a
assignings =  lift

-- | Unsafely update assigning context.
updateAssigningContext :: Monad m => (AssigningContext -> AssigningContext) -> Assignings r m ()
updateAssigningContext =  Assignings . modify

-- | 'MonadRestrict' with ordering.
instance MonadRestrict c m => MonadRestrict c (Assignings r m) where
  restrictContext = assignings . restrictContext

-- | Target of assignment.
newtype AssignTarget r v = AssignTarget (Table r, Pi r v)

targetProjection :: AssignTarget r v -> Projection Flat v
targetProjection (AssignTarget (tbl, pi')) =
  Projection.pi (Projection.unsafeFromTable tbl) pi'

-- | Add an assignment.
assignTo :: Monad m => Projection Flat v ->  AssignTarget r v -> Assignings r m ()
assignTo vp target = updateAssigningContext . foldr (>>>) id
                     $ zipWith updateAssignments lefts rights  where
  lefts  = Projection.columns $ targetProjection target
  rights = Projection.columns vp

-- | Specify target of assignment.
(!#) :: Table r -> Pi r v -> AssignTarget r v
(!#) =  curry AssignTarget

-- | Add and assginment.
(<-#) :: Monad m => AssignTarget r v -> Projection Flat v -> Assignings r m ()
(<-#) =  flip assignTo

infix 8 !#
infix 4 <-#

-- | Run 'Assignings' to get 'Assignments'
extractAssignments :: (Monad m, Functor m)
                   => Assignings r m a
                   -> m (a, Assignments)
extractAssignments q = second assignments <$> runAssigningsPrime q
