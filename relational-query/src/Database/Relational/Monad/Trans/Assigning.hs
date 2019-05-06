{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Database.Relational.Monad.Trans.Assigning
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines monad transformer which lift
-- from context into context with assigning.
module Database.Relational.Monad.Trans.Assigning (
  -- * Transformer into context with assignments
  Assignings, assignings,

  -- * API of context with assignments
  assignTo, (<-#), AssignTarget,

  -- * Result SQL set clause
  extractAssignments
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Applicative (Applicative, pure, (<$>))
import Control.Arrow (second)
import Data.Monoid (mconcat)
import Data.DList (DList, toList)

import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.SqlSyntax (Record, Assignment)

import Database.Relational.Pi (Pi)
import Database.Relational.Table (Table, recordWidth)
import qualified Database.Relational.Record as Record
import Database.Relational.Monad.Class (MonadQualify (..), MonadRestrict(..))


-- | Type to accumulate assigning context.
--   Type 'r' is table record type.
newtype Assignings r m a =
  Assignings (WriterT (Table r -> DList Assignment) m a)
  deriving (MonadTrans, Monad, Functor, Applicative)

-- | Lift to 'Assignings'
assignings :: Monad m => m a -> Assignings r m a
assignings =  lift

-- | 'MonadRestrict' with assigning.
instance MonadRestrict c m => MonadRestrict c (Assignings r m) where
  restrict = assignings . restrict

-- | 'MonadQualify' with assigning.
instance MonadQualify q m => MonadQualify q (Assignings r m) where
  liftQualify = assignings . liftQualify

-- | Target of assignment.
type AssignTarget r v = Pi r v

targetRecord :: AssignTarget r v ->  Table r -> Record Flat v
targetRecord pi' tbl = Record.wpi (recordWidth tbl) (Record.unsafeFromTable tbl) pi'

-- | Add an assignment.
assignTo :: Monad m => Record Flat v ->  AssignTarget r v -> Assignings r m ()
assignTo vp target = Assignings . tell
                     $ \t -> mconcat $ zipWith (curry pure) (leftsR t) rights  where
  leftsR = Record.columns . targetRecord target
  rights = Record.columns vp

-- | Add and assginment.
(<-#) :: Monad m => AssignTarget r v -> Record Flat v -> Assignings r m ()
(<-#) =  flip assignTo

infix 4 <-#

-- | Run 'Assignings' to get ['Assignment']
extractAssignments :: (Monad m, Functor m)
                   => Assignings r m a
                   -> m (a, Table r -> [Assignment])
extractAssignments (Assignings ac) = second (toList .) <$> runWriterT ac
