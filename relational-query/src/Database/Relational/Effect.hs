{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

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
  Restriction,

  -- * Object to express update target columns and restriction.
  UpdateTarget,
  liftTargetAllColumn',

  -- * Object to express insert terget.
  InsertTarget, piRegister,

  -- * Generate SQL from restriction.
  deleteFromRestrict,
  updateFromAssign,
  chunkInsertFromRegister,
  insertFromRegister,
  chunkInsertFromRecords,

  -- * Deprecated
  restriction, restriction',
  updateTarget, updateTarget',
  liftTargetAllColumn,
  updateTargetAllColumn, updateTargetAllColumn',
  insertTarget', insertTarget,
  sqlWhereFromRestriction, deleteFromRestriction,
  sqlFromUpdateTarget, updateFromUpdateTarget,
  sqlChunkFromInsertTarget, sqlFromInsertTarget, sqlChunksFromRecordList,
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
import Database.Relational.Internal.ContextType (Flat)
import Database.Relational.Internal.String (StringSQL, stringSQL, showStringSQL)
import Database.Relational.SqlSyntax
  (Qualified, SubQuery, corrSubQueryTerm, composeWhere, composeSets,
   composeChunkValuesWithColumns, composeValuesListWithColumns)
import Database.Relational.Typed.Table (Table, TableDerivable, derivedTable, tableName)
import qualified Database.Relational.Typed.Table as Table
import Database.Relational.Typed.Record
  (Record, untypeRecord, unsafeRecordFromQualifiedQuery)

import Database.Relational.Pi (Pi, id')
import qualified Database.Relational.Pi.Unsafe as Pi
import Database.Relational.ProjectableClass (LiteralSQL)
import Database.Relational.Projectable
  (PlaceHolders, unitPH, pwPlaceholder, placeholder, (><), value, )
import Database.Relational.Monad.BaseType (ConfigureQuery, qualifyQuery, askConfig)
import Database.Relational.Monad.Class (MonadQualify (..))
import Database.Relational.Monad.Trans.Assigning (assignings, (<-#))
import Database.Relational.Monad.Restrict (Restrict)
import qualified Database.Relational.Monad.Restrict as Restrict
import Database.Relational.Monad.Assign (Assign)
import qualified Database.Relational.Monad.Assign as Assign
import Database.Relational.Monad.Register (Register)
import qualified Database.Relational.Monad.Register as Register


-- helper function for UPDATE and DELETE
withQualified :: MonadQualify ConfigureQuery m => Table r -> (Record c r -> m a) -> m StringSQL
withQualified tbl q = do
  let qualTandR :: MonadQualify ConfigureQuery m => Table r -> m (Qualified SubQuery, Record c r)
      qualTandR tbl_ = liftQualify $ do
        qq <- qualifyQuery $ Table.toSubQuery tbl_
        return (qq, unsafeRecordFromQualifiedQuery qq {- qualified record expression -})
  (qq, r) <- qualTandR tbl
  void $ q r -- placeholder info is not used
  addAS <- addModifyTableAliasAS <$> liftQualify askConfig
  return $ corrSubQueryTerm addAS qq {- qualified table -}

-- | Restriction type with place-holder parameter 'p' and projected record type 'r'.
type Restriction p r = Record Flat r -> Restrict (PlaceHolders p)

-- | Deprecated.
restriction :: (Record Flat r -> Restrict ()) -> Restriction () r
restriction = ((>> return unitPH) .)
{-# DEPRECATED restriction "same as ((>> return unitPH) .)" #-}

-- | Deprecated.
restriction' :: (Record Flat r -> Restrict (PlaceHolders p)) -> Restriction p r
restriction' = id
{-# DEPRECATED restriction' "same as id" #-}

fromRestrict :: Config -> Table r -> (Record Flat r -> Restrict (PlaceHolders p)) -> (StringSQL, StringSQL)
fromRestrict config tbl q = (qt, composeWhere $ map untypeRecord rs)
  where (qt, rs) = Restrict.extract (withQualified tbl q) config

-- | Deprecated.
sqlWhereFromRestriction :: Config -> Table r -> (Record Flat r -> Restrict (PlaceHolders p)) -> StringSQL
sqlWhereFromRestriction config tbl q =
  composeWhere . map untypeRecord . snd $ Restrict.extract (withQualified tbl q) config
{-# DEPRECATED sqlWhereFromRestriction "low-level API, this API will be expired." #-}

-- | DELETE statement with WHERE clause 'StringSQL' string from 'Restrict' computation.
deleteFromRestrict :: Config -> Table r -> (Record Flat r -> Restrict (PlaceHolders p)) -> StringSQL
deleteFromRestrict config tbl r =
  DELETE <> FROM <> uncurry (<>) (fromRestrict config tbl r)

-- | Deprecated.
deleteFromRestriction :: Config -> Table r -> (Record Flat r -> Restrict (PlaceHolders p)) -> StringSQL
deleteFromRestriction = deleteFromRestrict
{-# DEPRECATED deleteFromRestriction "low-level API, this API will be expired." #-}

-- | Show WHERE clause.
instance TableDerivable r => Show (Record Flat r -> Restrict (PlaceHolders p)) where
  show = showStringSQL . snd . fromRestrict defaultConfig derivedTable


-- | UpdateTarget type with place-holder parameter 'p' and projected record type 'r'.
type UpdateTarget p r = Record Flat r -> Assign r (PlaceHolders p)

-- | Deprecated.
updateTarget :: (Record Flat r -> Assign r ())
             -> UpdateTarget () r
updateTarget =  ((>> return unitPH) .)
{-# DEPRECATED updateTarget "old-style API. Use new-style Database.Relational.updateNoPH." #-}

-- | Deprecated.
updateTarget' :: (Record Flat r -> Assign r (PlaceHolders p))
              -> UpdateTarget p r
updateTarget' = id
{-# DEPRECATED updateTarget' "same as id" #-}

updateAllColumn :: PersistableWidth r
                => (Record Flat r -> Restrict (PlaceHolders p))
                -> (Record Flat r -> Assign r (PlaceHolders (r, p)))
updateAllColumn rs proj = do
  (ph0, ()) <- placeholder (\ph -> id' <-# ph)
  ph1       <- assignings $ rs proj
  return $ ph0 >< ph1

-- | Lift 'Restrict' computation to 'Assign' computation. Assign target columns are all.
liftTargetAllColumn :: PersistableWidth r
                     => (Record Flat r -> Restrict (PlaceHolders ()))
                     -> (Record Flat r -> Assign r (PlaceHolders r))
liftTargetAllColumn rs = \proj -> fmap peRight $ updateAllColumn rs proj
{-# DEPRECATED liftTargetAllColumn "old-style API. use Database.Relational.updateAllColumnNoPH instead of this." #-}

-- | Lift 'Restrict' computation to 'Assign' computation. Assign target columns are all. With placefolder type 'p'.
liftTargetAllColumn' :: PersistableWidth r
                     => (Record Flat r -> Restrict (PlaceHolders p))
                     -> (Record Flat r -> Assign r (PlaceHolders (r, p)))
liftTargetAllColumn' rs = updateAllColumn rs

-- | Deprecated.
updateTargetAllColumn :: PersistableWidth r
                      => (Record Flat r -> Restrict ())
                      -> (Record Flat r -> Assign r (PlaceHolders r))
updateTargetAllColumn = liftTargetAllColumn . restriction
{-# DEPRECATED updateTargetAllColumn "Use Database.Relational.updateAllColumnNoPH instead of this." #-}

-- | Deprecated.
updateTargetAllColumn' :: PersistableWidth r
                       => (Record Flat r -> Restrict (PlaceHolders p))
                       -> (Record Flat r -> Assign r (PlaceHolders (r, p)))
updateTargetAllColumn' = liftTargetAllColumn'
{-# DEPRECATED updateTargetAllColumn' "Use Database.Relational.updateAllColumn instead of this." #-}


fromAssign :: Config -> Table r -> (Record Flat r -> Assign r (PlaceHolders p)) -> (StringSQL, StringSQL)
fromAssign config tbl q = (qt, composeSets (asR tbl) <> (composeWhere $ map untypeRecord rs))
  where ((qt, asR), rs) = Assign.extract (withQualified tbl q) config

-- | Deprecated.
sqlFromUpdateTarget :: Config -> Table r -> (Record Flat r -> Assign r (PlaceHolders p)) -> StringSQL
sqlFromUpdateTarget config tbl q = composeSets (asR tbl) <> (composeWhere $ map untypeRecord rs)
  where ((_, asR), rs) = Assign.extract (withQualified tbl q) config
{-# DEPRECATED sqlFromUpdateTarget "low-level API, this API will be expired." #-}

-- | UPDATE statement with SET clause and WHERE clause 'StringSQL' string from 'Assign' computation.
updateFromAssign :: Config -> Table r -> (Record Flat r -> Assign r (PlaceHolders p)) -> StringSQL
updateFromAssign config tbl ut =
  UPDATE <> uncurry (<>) (fromAssign config tbl ut)

-- | Deprecated.
updateFromUpdateTarget :: Config -> Table r -> (Record Flat r -> Assign r (PlaceHolders p)) -> StringSQL
updateFromUpdateTarget = updateFromAssign
{-# DEPRECATED updateFromUpdateTarget "low-level API, this API will be expired." #-}

-- | Show Set clause and WHERE clause.
instance TableDerivable r => Show (Record Flat r -> Assign r (PlaceHolders p)) where
  show = showStringSQL . snd . fromAssign defaultConfig derivedTable


-- | InsertTarget type with place-holder parameter 'p' and projected record type 'r'.
type InsertTarget p r = Register r (PlaceHolders p)

-- | Finalize 'Register' monad and generate 'InsertTarget'.
insertTarget :: Register r ()
             -> InsertTarget () r
insertTarget =  (>> return unitPH)
{-# DEPRECATED insertTarget "old-style API. Use new-style Database.Relational.insertValueNoPH ." #-}

-- | Finalize 'Register' monad and generate 'InsertTarget' with place-holder parameter 'p'.
insertTarget' :: Register r (PlaceHolders p)
              -> InsertTarget p r
insertTarget' = id
{-# DEPRECATED insertTarget' "same as id" #-}

-- | parametalized 'Register' monad from 'Pi'
piRegister :: PersistableWidth r
           => Pi r r'
           -> Register r (PlaceHolders r')
piRegister pi' = do
  let (ph', ma) = pwPlaceholder (Pi.width' pi') (\ph -> pi' <-# ph)
  () <- ma
  return ph'

fromRegister :: Config
             -> Int
             -> Table r
             -> InsertTarget p r
             -> StringSQL
fromRegister config sz tbl q =
    INSERT <> INTO <> stringSQL (tableName tbl) <> composeChunkValuesWithColumns sz (asR tbl)
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
chunkInsertFromRegister :: Config
                        -> Table r
                        -> InsertTarget p r
                        -> (StringSQL, Int)
chunkInsertFromRegister config tbl it =
    (fromRegister config n tbl it, n)
  where
    n = countChunks config tbl

-- | Deprecated.
sqlChunkFromInsertTarget :: Config
                         -> Table r
                         -> InsertTarget p r
                         -> (StringSQL, Int)
sqlChunkFromInsertTarget = chunkInsertFromRegister
{-# DEPRECATED sqlChunkFromInsertTarget "low-level API, this API will be expired." #-}

-- | Make 'StringSQL' string of SQL INSERT statement from 'InsertTarget'
insertFromRegister :: Config -> Table r -> InsertTarget p r -> StringSQL
insertFromRegister config = fromRegister config 1

-- | Deprecated.
sqlFromInsertTarget :: Config -> Table r -> InsertTarget p r -> StringSQL
sqlFromInsertTarget = insertFromRegister
{-# DEPRECATED sqlFromInsertTarget "low-level API, this API will be expired." #-}

-- | Make 'StringSQL' strings of SQL INSERT strings from records list
chunkInsertFromRecords :: LiteralSQL r'
                       => Config
                       -> Table r
                       -> Pi r r'
                       -> [r']
                       -> [StringSQL]
chunkInsertFromRecords config tbl pi' xs =
    [ INSERT <> INTO <> stringSQL (tableName tbl) <>
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

-- | Deprecated.
sqlChunksFromRecordList :: LiteralSQL r'
                        => Config
                        -> Table r
                        -> Pi r r'
                        -> [r']
                        -> [StringSQL]
sqlChunksFromRecordList = chunkInsertFromRecords
{-# DEPRECATED sqlChunksFromRecordList "low-level API, this API will be expired." #-}
