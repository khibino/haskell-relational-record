-- |
-- Module      : Database.Relational.Query.Effect
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines effect statements
-- like update and delete.
module Database.Relational.Query.Effect (
  -- * Object to express simple restriction.
  Restriction, restriction, restriction',

  -- * Object to express update target columns and restriction.
  UpdateTarget, updateTarget, updateTarget',
  liftTargetAllColumn, liftTargetAllColumn',
  updateTargetAllColumn, updateTargetAllColumn',

  -- * Generate SQL from restriction.
  sqlWhereFromRestriction,
  sqlFromUpdateTarget
  ) where

import Data.Monoid ((<>))

import Database.Record (PersistableWidth)

import Database.Relational.Query.Internal.SQL (StringSQL, showStringSQL)
import Database.Relational.Query.Pi (id')
import Database.Relational.Query.Table (Table, TableDerivable, derivedTable)
import Database.Relational.Query.Component (Config, defaultConfig, composeSets)
import Database.Relational.Query.Sub (composeWhere)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable
  (PlaceHolders, placeholder, unitPlaceHolder, unsafeAddPlaceHolders, (><), rightId)

import Database.Relational.Query.Monad.Trans.Assigning (assignings, (<-#))
import Database.Relational.Query.Monad.Restrict (RestrictedStatement)
import qualified Database.Relational.Query.Monad.Restrict as Restrict
import Database.Relational.Query.Monad.Assign (AssignStatement)
import qualified Database.Relational.Query.Monad.Assign as Assign


-- | Restriction type with place-holder parameter 'p' and projection record type 'r'.
newtype Restriction p r = Restriction (RestrictedStatement r (PlaceHolders p))

-- | Finalize 'Restrict' monad and generate 'Restriction'.
restriction :: RestrictedStatement r () -> Restriction () r
restriction = Restriction . ((>> return unitPlaceHolder) .)

-- | Finalize 'Restrict' monad and generate 'Restriction' with place-holder parameter 'p'
restriction' :: RestrictedStatement r (PlaceHolders p) -> Restriction p r
restriction' = Restriction

runRestriction :: Restriction p r
               -> RestrictedStatement r (PlaceHolders p)
runRestriction (Restriction qf) =
  fmap fst . unsafeAddPlaceHolders . qf

-- | SQL WHERE clause 'StringSQL' string from 'Restriction'.
sqlWhereFromRestriction :: Config -> Table r -> Restriction p r -> StringSQL
sqlWhereFromRestriction config tbl (Restriction q) = composeWhere rs
  where (_ph, rs) = Restrict.extract (q $ Projection.unsafeFromTable tbl) config

-- | Show where clause.
instance TableDerivable r => Show (Restriction p r) where
  show = showStringSQL . sqlWhereFromRestriction defaultConfig derivedTable

-- | UpdateTarget type with place-holder parameter 'p' and projection record type 'r'.
newtype UpdateTarget p r = UpdateTarget (AssignStatement r (PlaceHolders p))

-- | Finalize 'Target' monad and generate 'UpdateTarget'.
updateTarget :: AssignStatement r ()
             -> UpdateTarget () r
updateTarget =  UpdateTarget . ((>> return unitPlaceHolder) .)

-- | Finalize 'Target' monad and generate 'UpdateTarget' with place-holder parameter 'p'.
updateTarget' :: AssignStatement r (PlaceHolders p)
              -> UpdateTarget p r
updateTarget' = UpdateTarget

_runUpdateTarget :: UpdateTarget p r
                 -> AssignStatement r (PlaceHolders p)
_runUpdateTarget (UpdateTarget qf) =
  fmap fst . unsafeAddPlaceHolders . qf

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
sqlFromUpdateTarget :: Config -> Table r -> UpdateTarget p r -> StringSQL
sqlFromUpdateTarget config tbl (UpdateTarget q) = composeSets (asR tbl) <> composeWhere rs
  where ((_ph, asR), rs) = Assign.extract (q (Projection.unsafeFromTable tbl)) config

instance TableDerivable r => Show (UpdateTarget p r) where
  show = showStringSQL . sqlFromUpdateTarget defaultConfig derivedTable
