{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Database.Relational.Query.Monad.Core (
  QueryCore,

  expr,

  unsafeSubQueryWithAttr,
  -- unsafeQueryMergeWithAttr,

  expandSQL
  ) where

import Prelude hiding (product)

import Database.Relational.Query.Internal.Context (updateProduct)
import Database.Relational.Query.Internal.Product (NodeAttr, growProduct)

import Database.Relational.Query.Expr (Expr)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable (Projectable(project))

import Database.Relational.Query.Sub (SubQuery)

import Database.Relational.Query.Monad.Qualify (Qualify, evalQualifyPrime, qualifyQuery)
import Database.Relational.Query.Monad.Class (MonadQuery(on, wheres, unsafeSubQuery))
import Database.Relational.Query.Monad.Trans.Join
  (QueryJoin, join', expandSQL, updateContext, updateJoinRestriction, updateRestriction)


expr :: Projection ft -> Expr ft
expr =  project

instance MonadQuery (QueryJoin Qualify) where
  on     =  updateJoinRestriction
  wheres =  updateRestriction
  unsafeSubQuery          = unsafeSubQueryWithAttr
  -- unsafeMergeAnotherQuery = unsafeQueryMergeWithAttr

type QueryCore = QueryJoin Qualify

unsafeSubQueryWithAttr ::  NodeAttr -> Qualify SubQuery -> QueryJoin Qualify (Projection t)
unsafeSubQueryWithAttr attr qualSub = do
  qsub <- join' (qualSub >>= qualifyQuery)
  updateContext (updateProduct (`growProduct` (attr, qsub)))
  return $ Projection.fromQualifiedSubQuery qsub

-- unsafeMergeAnother :: NodeAttr -> QueryJoin a -> QueryJoin a
-- unsafeMergeAnother naR qR = do
--   mayPL <- takeProduct
--   v     <- qR
--   maybe (return ()) (\pL -> restoreLeft pL naR) mayPL
--   return v

-- unsafeQueryMergeWithAttr :: NodeAttr -> QueryJoin (Projection r) -> QueryJoin (Projection r)
-- unsafeQueryMergeWithAttr =  unsafeMergeAnother

instance Show (QueryJoin Qualify (Projection r)) where
  show = fst . fst . doExpand
    where doExpand = evalQualifyPrime . expandSQL . fmap (\x -> (,) x ())
