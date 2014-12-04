-- |
-- Module      : Database.Relational.Query.Relation
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines simple restriction
-- for update and delete statement.
module Database.Relational.Query.Restriction (
  -- * Object to express simple restriction.
  Restriction, RestrictionContext, restriction, restriction',

  -- * Object to express update target columns and restriction.
  UpdateTarget, UpdateTargetContext, updateTarget, updateTarget',
  liftTargetAllColumn, liftTargetAllColumn',
  updateTargetAllColumn, updateTargetAllColumn',

  -- * Generate SQL from restriction.
  sqlWhereFromRestriction,
  sqlFromUpdateTarget
  ) where

import Data.Monoid ((<>))

import Database.Record (PersistableWidth)

import Database.Relational.Query.Internal.SQL (StringSQL, showStringSQL)
import Database.Relational.Query.Context (Flat)
import Database.Relational.Query.Pi (id')
import Database.Relational.Query.Table (Table, TableDerivable, derivedTable)
import Database.Relational.Query.Component (composeWhere, composeSets)
import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable
  (PlaceHolders, placeholder, addPlaceHolders, (><), rightId)

import Database.Relational.Query.Monad.Trans.Assigning (assignings, (<-#))
import Database.Relational.Query.Monad.Restrict (Restrict, RestrictedStatement)
import qualified Database.Relational.Query.Monad.Restrict as Restrict
import Database.Relational.Query.Monad.Target (AssignStatement)
import qualified Database.Relational.Query.Monad.Target as Target


-- | Restriction type with place-holder parameter 'p' and projection record type 'r'.
newtype Restriction p r = Restriction (Projection Flat r -> Restrict ())

-- | Not finalized 'Restrict' monad type.
type RestrictionContext p r = RestrictedStatement r (PlaceHolders p)

-- | Finalize 'Restrict' monad and generate 'Restriction'.
restriction :: RestrictedStatement r () -> Restriction () r
restriction =  Restriction

-- | Finalize 'Restrict' monad and generate 'Restriction' with place-holder parameter 'p'
restriction' :: RestrictedStatement r (PlaceHolders p) -> Restriction p r
restriction' =  Restriction . (fmap (const ()) .)

runRestriction :: Restriction p r
               -> RestrictedStatement r (PlaceHolders p)
runRestriction (Restriction qf) =
  fmap fst . addPlaceHolders . qf

-- | SQL WHERE clause 'StringSQL' string from 'Restriction'.
sqlWhereFromRestriction :: Table r -> Restriction p r -> StringSQL
sqlWhereFromRestriction tbl (Restriction q) = composeWhere rs
  where (_ph, rs) = Restrict.extract (q $ Projection.unsafeFromTable tbl)

-- | Show where clause.
instance TableDerivable r => Show (Restriction p r) where
  show = showStringSQL . sqlWhereFromRestriction derivedTable

-- | UpdateTarget type with place-holder parameter 'p' and projection record type 'r'.
newtype UpdateTarget p r = UpdateTarget (AssignStatement r ())

-- | Not finalized 'Target' monad type.
type UpdateTargetContext p r = AssignStatement r (PlaceHolders p)

-- | Finalize 'Target' monad and generate 'UpdateTarget'.
updateTarget :: AssignStatement r ()
             -> UpdateTarget () r
updateTarget =  UpdateTarget

-- | Finalize 'Target' monad and generate 'UpdateTarget' with place-holder parameter 'p'.
updateTarget' :: AssignStatement r (PlaceHolders p)
              -> UpdateTarget p r
updateTarget' qf = UpdateTarget $ fmap (const ()) . qf

_runUpdateTarget :: UpdateTarget p r
                 -> AssignStatement r (PlaceHolders p)
_runUpdateTarget (UpdateTarget qf) =
  fmap fst . addPlaceHolders . qf

updateAllColumn :: PersistableWidth r
                => Restriction p r
                -> AssignStatement r (PlaceHolders (r, p))
updateAllColumn rs proj = do
  (ph0, ()) <- placeholder (\ph -> id' <-# ph)
  ph1       <- assignings $ runRestriction rs proj
  return $ ph0 >< ph1

-- | Lift 'Restriction' to 'UpdateTarget'. Update target columns are all.
liftTargetAllColumn :: PersistableWidth r
                     => Restriction () r
                     -> UpdateTarget r r
liftTargetAllColumn rs = updateTarget' $ \proj -> fmap rightId $ updateAllColumn rs proj

-- | Lift 'Restriction' to 'UpdateTarget'. Update target columns are all. With placefolder type 'p'.
liftTargetAllColumn' :: PersistableWidth r
                     => Restriction p r
                     -> UpdateTarget (r, p) r
liftTargetAllColumn' rs = updateTarget' $ updateAllColumn rs

-- | Finalize 'Restrict' monad and generate 'UpdateTarget'. Update target columns are all.
updateTargetAllColumn :: PersistableWidth r
                      => RestrictedStatement r ()
                      -> UpdateTarget r r
updateTargetAllColumn = liftTargetAllColumn . restriction

-- | Finalize 'Restrict' monad and generate 'UpdateTarget'. Update target columns are all. With placefolder type 'p'.
updateTargetAllColumn' :: PersistableWidth r
                       => RestrictedStatement r (PlaceHolders p)
                       -> UpdateTarget (r, p) r
updateTargetAllColumn' = liftTargetAllColumn' . restriction'


-- | SQL SET clause and WHERE clause 'StringSQL' string from 'UpdateTarget'
sqlFromUpdateTarget :: Table r -> UpdateTarget p r -> StringSQL
sqlFromUpdateTarget tbl (UpdateTarget q) = composeSets (asR tbl) <> composeWhere rs
  where ((_ph, asR), rs) = Target.extract (q (Projection.unsafeFromTable tbl))
