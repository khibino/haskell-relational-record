{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Effect
-- Copyright   : 2013-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines effect statements
-- like update and delete.
module Database.Relational.Effect (
  -- * Object to express simple restriction.
  Restriction, restriction, restriction',

  -- * Object to express update target columns and restriction.
  UpdateTarget, updateTarget',
  liftTargetAllColumn, liftTargetAllColumn',

  -- * Object to express insert terget.
  InsertTarget, insertTarget', piRegister,

  -- * Generate SQL from restriction.
  deleteFromRestriction,
  updateFromUpdateTarget,
  sqlChunkFromInsertTarget,
  sqlFromInsertTarget,
  sqlChunksFromRecordList,

  -- * Deprecated
  updateTarget, updateTargetAllColumn, updateTargetAllColumn',
  insertTarget,
  sqlWhereFromRestriction,
  sqlFromUpdateTarget,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.Monoid ((<>))
import Data.List (unfoldr)
import Data.Functor.ProductIsomorphic (peRight)

import Language.SQL.Keyword (Keyword(..))
import Database.Record.Persistable (PersistableWidth)

import Database.Relational.Internal.Config
  (Config (chunksInsertSize, addModifyTableAliasAS), defaultConfig)
import Database.Relational.Internal.String (StringSQL, stringSQL, showStringSQL)
import Database.Relational.SqlSyntax
  (Record, composeWhere, composeSets,
   composeChunkValuesWithColumns, composeValuesListWithColumns,
   Qualified, SubQuery, corrSubQueryTerm)

import Database.Relational.Pi (Pi, id')
import qualified Database.Relational.Pi.Unsafe as Pi
import Database.Relational.Table (Table, TableDerivable, derivedTable)
import qualified Database.Relational.Table as Table
import qualified Database.Relational.Record as Record
import Database.Relational.ProjectableClass (LiteralSQL)
import Database.Relational.Projectable
  (PlaceHolders, unitPH, pwPlaceholder, placeholder, (><), value, )
import Database.Relational.Monad.BaseType (ConfigureQuery, qualifyQuery, askConfig)
import Database.Relational.Monad.Class (MonadQualify (..))
import Database.Relational.Monad.Trans.Assigning (assignings, (<-#))
import Database.Relational.Monad.Restrict (RestrictedStatement)
import qualified Database.Relational.Monad.Restrict as Restrict
import Database.Relational.Monad.Assign (AssignStatement)
import qualified Database.Relational.Monad.Assign as Assign
import Database.Relational.Monad.Register (Register)
import qualified Database.Relational.Monad.Register as Register


-- helper function for UPDATE and DELETE
withQualified :: MonadQualify ConfigureQuery m => Table r -> (Record c r -> m a) -> m StringSQL
withQualified tbl q = do
  let qualTandR :: MonadQualify ConfigureQuery m => Table r -> m (Qualified SubQuery, Record c r)
      qualTandR tbl_ = liftQualify $ do
        qq <- qualifyQuery $ Table.toSubQuery tbl_
        return (qq, Record.unsafeFromQualifiedSubQuery qq {- qualified record expression -})
  (qq, r) <- qualTandR tbl
  void $ q r -- placeholder info is not used
  addAS <- addModifyTableAliasAS <$> liftQualify askConfig
  return $ corrSubQueryTerm addAS qq {- qualified table -}

-- | Restriction type with place-holder parameter 'p' and projected record type 'r'.
newtype Restriction p r = Restriction (RestrictedStatement r (PlaceHolders p))

-- | Finalize 'Restrict' monad and generate 'Restriction'.
restriction :: RestrictedStatement r () -> Restriction () r
restriction = Restriction . ((>> return unitPH) .)

-- | Finalize 'Restrict' monad and generate 'Restriction' with place-holder parameter 'p'
restriction' :: RestrictedStatement r (PlaceHolders p) -> Restriction p r
restriction' = Restriction

runRestriction :: Restriction p r
               -> RestrictedStatement r (PlaceHolders p)
runRestriction (Restriction qf) = qf

fromRestriction :: Config -> Table r -> Restriction p r -> (StringSQL, StringSQL)
fromRestriction config tbl (Restriction q) = (qt, composeWhere rs)
  where (qt, rs) = Restrict.extract (withQualified tbl q) config

-- | SQL WHERE clause 'StringSQL' string from 'Restriction'.
sqlWhereFromRestriction :: Config -> Table r -> Restriction p r -> StringSQL
sqlWhereFromRestriction config tbl = snd . fromRestriction config tbl
{-# DEPRECATED sqlWhereFromRestriction "low-level API, this API will be expired." #-}

-- | DELETE statement with WHERE clause 'StringSQL' string from 'Restriction'.
deleteFromRestriction :: Config -> Table r -> Restriction p r -> StringSQL
deleteFromRestriction config tbl r =
  DELETE <> FROM <> uncurry (<>) (fromRestriction config tbl r)

-- | Show WHERE clause.
instance TableDerivable r => Show (Restriction p r) where
  show = showStringSQL . snd . fromRestriction defaultConfig derivedTable


-- | UpdateTarget type with place-holder parameter 'p' and projected record type 'r'.
newtype UpdateTarget p r = UpdateTarget (AssignStatement r (PlaceHolders p))

-- | Finalize 'Target' monad and generate 'UpdateTarget'.
updateTarget :: AssignStatement r ()
             -> UpdateTarget () r
updateTarget =  UpdateTarget . ((>> return unitPH) .)
{-# DEPRECATED updateTarget "old-style API. Use new-style Database.Relational.updateNoPH." #-}

-- | Finalize 'Target' monad and generate 'UpdateTarget' with place-holder parameter 'p'.
updateTarget' :: AssignStatement r (PlaceHolders p)
              -> UpdateTarget p r
updateTarget' = UpdateTarget

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
liftTargetAllColumn rs = updateTarget' $ \proj -> fmap peRight $ updateAllColumn rs proj

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
{-# DEPRECATED updateTargetAllColumn "Use Database.Relational.updateAllColumnNoPH instead of this." #-}

-- | Finalize 'Restrict' monad and generate 'UpdateTarget'. Update target columns are all. With placefolder type 'p'.
updateTargetAllColumn' :: PersistableWidth r
                       => RestrictedStatement r (PlaceHolders p)
                       -> UpdateTarget (r, p) r
updateTargetAllColumn' = liftTargetAllColumn' . restriction'
{-# DEPRECATED updateTargetAllColumn' "Use Database.Relational.updateAllColumn instead of this." #-}


fromUpdateTarget :: Config -> Table r -> UpdateTarget p r -> (StringSQL, StringSQL)
fromUpdateTarget config tbl (UpdateTarget q) = (qt, composeSets (asR tbl) <> composeWhere rs)
  where ((qt, asR), rs) = Assign.extract (withQualified tbl q) config

-- | SQL SET clause and WHERE clause 'StringSQL' string from 'UpdateTarget'
sqlFromUpdateTarget :: Config -> Table r -> UpdateTarget p r -> StringSQL
sqlFromUpdateTarget config tbl = snd . fromUpdateTarget config tbl
{-# DEPRECATED sqlFromUpdateTarget "low-level API, this API will be expired." #-}

-- | UPDATE statement with SET clause and WHERE clause 'StringSQL' string from 'UpdateTarget'
updateFromUpdateTarget :: Config -> Table r -> UpdateTarget p r -> StringSQL
updateFromUpdateTarget config tbl ut =
  UPDATE <> uncurry (<>) (fromUpdateTarget config tbl ut)

-- | Show Set clause and WHERE clause.
instance TableDerivable r => Show (UpdateTarget p r) where
  show = showStringSQL . snd . fromUpdateTarget defaultConfig derivedTable


-- | InsertTarget type with place-holder parameter 'p' and projected record type 'r'.
newtype InsertTarget p r = InsertTarget (Register r (PlaceHolders p))

-- | Finalize 'Register' monad and generate 'InsertTarget'.
insertTarget :: Register r ()
             -> InsertTarget () r
insertTarget =  InsertTarget . (>> return unitPH)
{-# DEPRECATED insertTarget "old-style API. Use new-style Database.Relational.insertValueNoPH ." #-}

-- | Finalize 'Target' monad and generate 'UpdateTarget' with place-holder parameter 'p'.
insertTarget' :: Register r (PlaceHolders p)
              -> InsertTarget p r
insertTarget' = InsertTarget

-- | parametalized 'Register' monad from 'Pi'
piRegister :: PersistableWidth r
           => Pi r r'
           -> Register r (PlaceHolders r')
piRegister pi' = do
  let (ph', ma) = pwPlaceholder (Pi.width' pi') (\ph -> pi' <-# ph)
  () <- ma
  return ph'

sqlChunkFromInsertTarget' :: Config
                          -> Int
                          -> Table r
                          -> InsertTarget p r
                          -> StringSQL
sqlChunkFromInsertTarget' config sz tbl (InsertTarget q) =
    INSERT <> INTO <> stringSQL (Table.name tbl) <> composeChunkValuesWithColumns sz (asR tbl)
  where
    (_ph, asR) = Register.extract q config

countChunks :: Config
            -> Table r
            -> Int
countChunks config tbl =
    (th + w - 1) `quot` w
  where
    th = chunksInsertSize config
    w  = Table.width tbl

-- | Make 'StringSQL' string of SQL INSERT record chunk statement from 'InsertTarget'
sqlChunkFromInsertTarget :: Config
                         -> Table r
                         -> InsertTarget p r
                         -> (StringSQL, Int)
sqlChunkFromInsertTarget config tbl it =
    (sqlChunkFromInsertTarget' config n tbl it, n)
  where
    n = countChunks config tbl

-- | Make 'StringSQL' string of SQL INSERT statement from 'InsertTarget'
sqlFromInsertTarget :: Config -> Table r -> InsertTarget p r -> StringSQL
sqlFromInsertTarget config = sqlChunkFromInsertTarget' config 1

-- | Make 'StringSQL' strings of SQL INSERT strings from records list
sqlChunksFromRecordList :: LiteralSQL r'
                        => Config
                        -> Table r
                        -> Pi r r'
                        -> [r']
                        -> [StringSQL]
sqlChunksFromRecordList config tbl pi' xs =
    [ INSERT <> INTO <> stringSQL (Table.name tbl) <>
      composeValuesListWithColumns
      [ tf tbl
      | r <- rs
      , let ((), tf) = Register.extract (pi' <-# value r) config
      ]
    | rs <- unfoldr step xs
    ]
  where
    n = countChunks config tbl
    step ys
      | null ys    =  Nothing
      | otherwise  =  Just $ splitAt n ys
