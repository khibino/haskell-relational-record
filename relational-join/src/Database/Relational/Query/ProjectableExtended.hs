{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Query.ProjectableExtended
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines operators on various polymorphic projections
-- which needs extended GHC features.
module Database.Relational.Query.ProjectableExtended (
  -- * Projection for nested 'Maybe's
  ProjectableFlattenMaybe (flatten),

  flattenPiMaybe,

  -- * Get narrower projections
  (!), (?!), (?!?), (!??),

  (.!), (.?),

  -- -- * Get weaken projection type
  -- Projectable (project),

  -- * Aggregate functions
  unsafeAggregateOp,
  count, sum', avg, max', min', every, any', some',

  -- * Zipping projection type trick
  ProjectableIdZip (leftId, rightId),
  ProjectableRunIdsZip (runIds), flattenPh
  -- generalizedZip', (>?<)
  )  where

import Prelude hiding (pi)
import Data.Int (Int32)

import qualified Language.SQL.Keyword as SQL

import Database.Relational.Query.Internal.String (paren)
import Database.Relational.Query.Context (Flat, Aggregated)
import Database.Relational.Query.Expr (Expr, fromJust)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable
  (expr, PlaceHolders,
   ProjectableMaybe (flattenMaybe), ProjectableIdZip (leftId, rightId),
   SqlProjectable, unsafeProjectSql, ProjectableShowSql (unsafeShowSql))
import Database.Relational.Query.Pi (Pi)


-- | Projection interface.
class Projectable p0 p1 where
  -- ｜ Project from projection type 'p0' into weaken projection types 'p1'.
  project :: p0 c a -> p1 c a

-- | Uni-operator type for SQL String
type SqlUniOp = String -> String

-- | Uni-operator from SQL keyword.
sqlUniOp :: SQL.Keyword -> SqlUniOp
sqlUniOp kw = (SQL.wordShow kw ++) . (' ' :) . paren

-- | Unsafely make aggregation uni-operator from SQL keyword.
unsafeAggregateOp :: SqlProjectable (p Aggregated)
                  => SQL.Keyword -> Projection Flat a -> p Aggregated b
unsafeAggregateOp op = unsafeProjectSql . sqlUniOp op . unsafeShowSql

-- | Aggregation function COUNT.
count :: SqlProjectable (p Aggregated) => Projection Flat a -> p Aggregated Int32
count =  unsafeAggregateOp SQL.COUNT

-- | Aggregation function SUM.
sum'  :: (Num a, SqlProjectable (p Aggregated)) => Projection Flat a -> p Aggregated (Maybe a)
sum'  =  unsafeAggregateOp SQL.SUM

-- | Aggregation function AVG.
avg   :: (Num a, Fractional b, SqlProjectable (p Aggregated))=> Projection Flat a -> p Aggregated (Maybe b)
avg   =  unsafeAggregateOp SQL.AVG

-- | Aggregation function MAX.
max'  :: (Ord a, SqlProjectable (p Aggregated)) => Projection Flat a -> p Aggregated (Maybe a)
max'  =  unsafeAggregateOp SQL.MAX

-- | Aggregation function MIN.
min'  :: (Ord a, SqlProjectable (p Aggregated)) => Projection Flat a -> p Aggregated (Maybe a)
min'  =  unsafeAggregateOp SQL.MIN

-- | Aggregation function EVERY.
every :: (SqlProjectable (p Aggregated)) => Projection Flat (Maybe Bool) -> p Aggregated (Maybe Bool)
every =  unsafeAggregateOp SQL.EVERY

-- | Aggregation function ANY.
any'  :: (SqlProjectable (p Aggregated)) => Projection Flat (Maybe Bool) -> p Aggregated (Maybe Bool)
any'  =  unsafeAggregateOp SQL.ANY

-- | Aggregation function SOME.
some' :: (SqlProjectable (p Aggregated)) => Projection Flat (Maybe Bool) -> p Aggregated (Maybe Bool)
some' =  unsafeAggregateOp SQL.SOME

-- | Project from 'Projection' into 'Projection'.
instance Projectable Projection Projection where
  project = id

-- | Project from 'Projection' into 'Expr' 'Projection'.
instance Projectable Projection Expr where
  project = expr

projectPi :: Projectable Projection p1 => Projection c a -> Pi a b -> p1 c b
projectPi p = project . Projection.pi p

projectPiMaybe :: Projectable Projection p1 => Projection c (Maybe a) -> Pi a b -> p1 c (Maybe b)
projectPiMaybe p = project . Projection.piMaybe p

projectPiMaybe' :: Projectable Projection p1 => Projection c (Maybe a) -> Pi a (Maybe b) -> p1 c (Maybe b)
projectPiMaybe' p = project . Projection.piMaybe' p

-- | Get narrower projection along with projection path
--   and project into result projection type.
(!) :: Projectable Projection p
    => Projection c a   -- ^ Source projection
    -> Pi a b -- ^ Projection path
    -> p c b   -- ^ Narrower projected object
(!) =  projectPi

-- | Get narrower projection along with projection path
--   and project into result projection type.
--   'Maybe' phantom type is propagated.
(?!) :: Projectable Projection p
     => Projection c (Maybe a) -- ^ Source 'Projection'. 'Maybe' type
     -> Pi a b       -- ^ Projection path
     -> p c (Maybe b) -- ^ Narrower projected object. 'Maybe' type result
(?!) =  projectPiMaybe

-- | Get narrower projection along with projection path
--   and project into result projection type.
--   'Maybe' phantom type is propagated. Projection path leaf is 'Maybe' case.
(?!?) :: Projectable Projection p
      => Projection c (Maybe a)   -- ^ Source 'Projection'. 'Maybe' phantom type
      -> Pi a (Maybe b) -- ^ Projection path. 'Maybe' type leaf
      -> p c (Maybe b)   -- ^ Narrower projected object. 'Maybe' phantom type result
(?!?) =  projectPiMaybe'

-- | Get narrower projected expression along with projectino path
--   and strip 'Maybe' phantom type off.
(.!) :: Projection c (Maybe a) -- ^ Source projection type 'p'. 'Maybe' phantom type
     -> Pi a b      -- ^ Projection path
     -> Expr c b    -- ^ Narrower projected expression. 'Maybe' phantom type is stripped off
(.!) p = fromJust . projectPiMaybe p

-- | Get narrower projected expression along with projectino path
--   and strip 'Maybe' phantom type off.
--   Projection path leaf is 'Maybe' case.
(.?) :: Projection c (Maybe a)    -- ^ Source projection type 'p'. 'Maybe' phantom type
     -> Pi a (Maybe b) -- ^ Projection path. 'Maybe' type leaf
     -> Expr c b       -- ^ Narrower projected expression. 'Maybe' phantom type is stripped off
(.?) p = fromJust . projectPiMaybe' p


-- | Interface to compose phantom 'Maybe' nested type.
class ProjectableFlattenMaybe a b where
  flatten :: ProjectableMaybe p => p a -> p b

-- | Compose 'Maybe' type in projection phantom type.
instance ProjectableFlattenMaybe (Maybe a) b
         => ProjectableFlattenMaybe (Maybe (Maybe a)) b where
  flatten = flatten . flattenMaybe

-- | Not 'Maybe' type is not processed.
instance ProjectableFlattenMaybe (Maybe a) (Maybe a) where
  flatten = id

-- | Get narrower projection with flatten leaf phantom Maybe types along with projection path.
flattenPiMaybe :: (ProjectableMaybe (Projection cont), ProjectableFlattenMaybe (Maybe b) c)
               => Projection cont (Maybe a) -- ^ Source 'Projection'. 'Maybe' phantom type
               -> Pi a b               -- ^ Projection path
               -> Projection cont c         -- ^ Narrower 'Projection'. Flatten 'Maybe' phantom type
flattenPiMaybe p = flatten . Projection.piMaybe p

projectFlattenPiMaybe :: (ProjectableMaybe (Projection cont),
                          Projectable Projection p1, ProjectableFlattenMaybe (Maybe b) c)
                      => Projection cont (Maybe a) -- ^ Source 'Projection'. 'Maybe' phantom type
                      -> Pi a b                    -- ^ Projection path
                      -> p1 cont c                 -- ^ Narrower 'Projection'. Flatten 'Maybe' phantom type
projectFlattenPiMaybe p = project . flattenPiMaybe p

-- | Get narrower projection with flatten leaf phantom Maybe types along with projection path
--   and project into result projection type.
(!??) :: (ProjectableFlattenMaybe (Maybe b) c,
          Projectable Projection p, ProjectableMaybe (p cont))
      => Projection cont (Maybe a) -- ^ Source 'Projection'. 'Maybe' phantom type
      -> Pi a b                    -- ^ Projection path
      -> p cont c                  -- ^ Narrower flatten and projected object.
(!??) =  projectFlattenPiMaybe


-- | Interface to run recursively identity element laws.
class ProjectableRunIdsZip a b where
  runIds :: ProjectableIdZip p => p a -> p b

-- | Run left identity element law.
instance ProjectableRunIdsZip a b => ProjectableRunIdsZip ((), a) b where
  runIds = runIds . leftId

-- | Run right identity element law.
instance ProjectableRunIdsZip a b => ProjectableRunIdsZip (a, ()) b where
  runIds = runIds . rightId

-- | Base case definition to run recursively identity element laws.
instance ProjectableRunIdsZip a a where
  runIds = id

-- | Specialize 'runIds' for 'PlaceHolders' type.
flattenPh :: ProjectableRunIdsZip a b => PlaceHolders a -> PlaceHolders b
flattenPh =  runIds

-- -- | Binary operator the same as 'generalizedZip'.
-- (>?<) :: (ProjectableIdZip p, ProjectableRunIdsZip (a, b) c)
--       => p a -> p b -> p c
-- (>?<) =  generalizedZip'

infixl 8 !, ?!, ?!?, !??, .!, .?
-- infixl 1 >?<
