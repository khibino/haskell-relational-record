{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Relational.Query.Monad.Core (
  QueryCore,

  expr,
  ) where

import Prelude hiding (product)

import Database.Relational.Query.Expr (Expr)

import Database.Relational.Query.Projection (Projection)
import Database.Relational.Query.Projectable (Projectable(project))

import Database.Relational.Query.Monad.Qualify (Qualify, evalQualifyPrime)
import Database.Relational.Query.Monad.Trans.Join (QueryJoin, expandSQL)


expr :: Projection ft -> Expr ft
expr =  project

type QueryCore = QueryJoin Qualify

instance Show (QueryJoin Qualify (Projection r)) where
  show = fst . fst . doExpand
    where doExpand = evalQualifyPrime . expandSQL . fmap (\x -> (,) x ())
