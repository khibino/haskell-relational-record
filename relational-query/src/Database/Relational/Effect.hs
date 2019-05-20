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
  InsertTarget, insertTarget', piRegister,

  -- * Generate SQL from restriction.
  deleteFromRestriction,
  updateFromUpdateTarget,
  sqlChunkFromInsertTarget,
  sqlFromInsertTarget,
  sqlChunksFromRecordList,

  -- * Deprecated
  restriction, restriction',
  updateTarget, updateTarget',
  liftTargetAllColumn,
  updateTargetAllColumn, updateTargetAllColumn',
  insertTarget,
  sqlWhereFromRestriction,
  sqlFromUpdateTarget,
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void)
import Data.Monoid ((<>))
import Data.List (unfoldr)
import Data.Traversable (sequenceA)

import Language.SQL.Keyword (Keyword(..))
import Database.Record.Persistable (PersistableWidth)

import Database.Relational.Internal.Config
  (Config (chunksInsertSize, addModifyTableAliasAS), defaultConfig)
import Database.Relational.Internal.ContextType (Flat, PureOperand)
import Database.Relational.Internal.String (stringSQL, showStringSQL)
import Database.Relational.SqlSyntax
  (SQLWithPlaceholderOffsets', Record, composeWhere, composeSets,
   composeChunkValuesWithColumns, composeValuesListWithColumns,
   Qualified, SubQuery, corrSubQueryTerm, unQualify,
   withPlaceholderOffsets, detachPlaceholderOffsets, collectPlaceholderOffsets,)

import Database.Relational.Pi (Pi, id')
import qualified Database.Relational.Pi.Unsafe as Pi
import Database.Relational.Table (Table, TableDerivable, derivedTable)
import qualified Database.Relational.Table as Table
import qualified Database.Relational.Record as Record
import Database.Relational.TupleInstances (fst', snd')
import Database.Relational.ProjectableClass (LiteralSQL)
import Database.Relational.Projectable ((!), value, )
import Database.Relational.Monad.BaseType
  (ConfigureQuery, qualifyQuery, askConfig, pwPlaceholders, defaultPlaceholders)
import Database.Relational.Monad.Class (MonadQualify (..))
import Database.Relational.Monad.Trans.Assigning (assignings, (<-#))
import Database.Relational.Monad.Restrict (Restrict)
import qualified Database.Relational.Monad.Restrict as Restrict
import Database.Relational.Monad.Assign (Assign)
import qualified Database.Relational.Monad.Assign as Assign
import Database.Relational.Monad.Register (Register)
import qualified Database.Relational.Monad.Register as Register
import Database.Relational.Monad.Trans.ReadPlaceholders
  (ReadPlaceholders, runReadPlaceholders, readPlaceholders, askPlaceholders)


-- helper function for UPDATE and DELETE
withQualified :: MonadQualify ConfigureQuery m => Table r -> (Record c r -> m a) -> m SQLWithPlaceholderOffsets'
withQualified tbl q = do
  let qualTandR :: MonadQualify ConfigureQuery m => Table r -> m (Qualified SubQuery, Record c r)
      qualTandR tbl_ = liftQualify $ do
        qq <- qualifyQuery $ Table.toSubQuery tbl_
        return (qq, Record.unsafeFromQualifiedSubQuery qq {- qualified record expression -})
  (qq, r) <- qualTandR tbl
  void $ q r
  addAS <- addModifyTableAliasAS <$> liftQualify askConfig
  return . withPlaceholderOffsets (collectPlaceholderOffsets $ unQualify qq) $ corrSubQueryTerm addAS qq {- qualified table -}

-- | Restriction type with place-holder parameter 'p' and projected record type 'r'.
type Restriction p r = Record Flat r -> ReadPlaceholders p Restrict ()

-- | Deprecated.
restriction :: (Record Flat r -> ReadPlaceholders () Restrict ()) -> Restriction () r
restriction = id
{-# DEPRECATED restriction "same as ((>> return unitPH) .)" #-}

-- | Deprecated.
restriction' :: (Record Flat r -> ReadPlaceholders p Restrict ()) -> Restriction p r
restriction' = id
{-# DEPRECATED restriction' "same as id" #-}

fromRestriction :: PersistableWidth p => Config -> Table r -> (Record Flat r -> ReadPlaceholders p Restrict ()) -> (SQLWithPlaceholderOffsets', SQLWithPlaceholderOffsets')
fromRestriction config tbl q = (qt, composeWhere <$> sequenceA rs)
  where (qt, rs) = Restrict.extract (runReadPlaceholders (withQualified tbl q) defaultPlaceholders) config

-- | SQL WHERE clause 'StringSQL' string from 'Restrict' computation.
sqlWhereFromRestriction :: PersistableWidth p => Config -> Table r -> (Record Flat r -> ReadPlaceholders p Restrict ()) -> SQLWithPlaceholderOffsets'
sqlWhereFromRestriction config tbl = snd . fromRestriction config tbl
{-# DEPRECATED sqlWhereFromRestriction "low-level API, this API will be expired." #-}

-- | DELETE statement with WHERE clause 'StringSQL' string from 'Restrict' computation.
deleteFromRestriction :: PersistableWidth p => Config -> Table r -> (Record Flat r -> ReadPlaceholders p Restrict ()) -> SQLWithPlaceholderOffsets'
deleteFromRestriction config tbl rs = (\t r ->  DELETE <> t <> r) <$> twp <*> rwp
  where (twp, rwp) = fromRestriction config tbl rs

-- | Show WHERE clause.
instance (PersistableWidth p, TableDerivable r) => Show (Record Flat r -> ReadPlaceholders p Restrict ()) where
  show = showStringSQL . detachPlaceholderOffsets . snd . fromRestriction defaultConfig derivedTable


-- | UpdateTarget type with place-holder parameter 'p' and projected record type 'r'.
type UpdateTarget p r = Record Flat r -> ReadPlaceholders p (Assign r) ()

-- | Deprecated.
updateTarget :: (Record Flat r -> ReadPlaceholders () (Assign r) ())
             -> UpdateTarget () r
updateTarget =  id
{-# DEPRECATED updateTarget "old-style API. Use new-style Database.Relational.updateNoPH." #-}

-- | Deprecated.
updateTarget' :: (Record Flat r -> ReadPlaceholders p (Assign r) ())
              -> UpdateTarget p r
updateTarget' = id
{-# DEPRECATED updateTarget' "same as id" #-}

updateAllColumn :: (PersistableWidth r, PersistableWidth p)
                => (Record Flat r -> ReadPlaceholders p Restrict ())
                -> (Record Flat r -> ReadPlaceholders (r, p) (Assign r) ())
updateAllColumn rs proj = do
  ph <- askPlaceholders
  readPlaceholders $ do
    id' <-# ph ! fst'
    assignings $ runReadPlaceholders (rs proj) (ph ! snd')
  return ()

-- | Lift 'Restrict' computation to 'Assign' computation. Assign target columns are all.
liftTargetAllColumn :: PersistableWidth r
                     => (Record Flat r -> ReadPlaceholders () Restrict ())
                     -> (Record Flat r -> ReadPlaceholders r (Assign r) ())
liftTargetAllColumn rs proj = do
  ph <- askPlaceholders
  readPlaceholders $ do
    id' <-# ph
    assignings $ runReadPlaceholders (rs proj) Record.pempty
  return ()
{-# DEPRECATED liftTargetAllColumn "old-style API. use Database.Relational.updateAllColumnNoPH instead of this." #-}

-- | Lift 'Restrict' computation to 'Assign' computation. Assign target columns are all. With placefolder type 'p'.
liftTargetAllColumn' :: (PersistableWidth r, PersistableWidth p)
                     => (Record Flat r -> ReadPlaceholders p Restrict ())
                     -> (Record Flat r -> ReadPlaceholders (r, p) (Assign r) ())
liftTargetAllColumn' rs = updateAllColumn rs

-- | Deprecated.
updateTargetAllColumn :: PersistableWidth r
                      => (Record Flat r -> ReadPlaceholders () Restrict ())
                      -> (Record Flat r -> ReadPlaceholders r (Assign r) ())
updateTargetAllColumn = liftTargetAllColumn . restriction
{-# DEPRECATED updateTargetAllColumn "Use Database.Relational.updateAllColumnNoPH instead of this." #-}

-- | Deprecated.
updateTargetAllColumn' :: (PersistableWidth r, PersistableWidth p)
                       => (Record Flat r -> ReadPlaceholders p Restrict ())
                       -> (Record Flat r -> ReadPlaceholders (r, p) (Assign r) ())
updateTargetAllColumn' = liftTargetAllColumn'
{-# DEPRECATED updateTargetAllColumn' "Use Database.Relational.updateAllColumn instead of this." #-}


fromUpdateTarget :: PersistableWidth p
                 => Config
                 -> Table r
                 -> (Record Flat r -> ReadPlaceholders p (Assign r) ())
                 -> (SQLWithPlaceholderOffsets', SQLWithPlaceholderOffsets')
fromUpdateTarget config tbl q = (qt, (<>) <$> composeSets (asR tbl) <*> (composeWhere <$> sequenceA rs))
  where ((qt, asR), rs) = Assign.extract (runReadPlaceholders (withQualified tbl q) defaultPlaceholders) config

-- | SQL SET clause and WHERE clause 'StringSQL' string from 'Assign' computation.
sqlFromUpdateTarget :: PersistableWidth p => Config -> Table r -> (Record Flat r -> ReadPlaceholders p (Assign r) ()) -> SQLWithPlaceholderOffsets'
sqlFromUpdateTarget config tbl = snd . fromUpdateTarget config tbl
{-# DEPRECATED sqlFromUpdateTarget "low-level API, this API will be expired." #-}

-- | UPDATE statement with SET clause and WHERE clause 'StringSQL' string from 'Assign' computation.
updateFromUpdateTarget :: PersistableWidth p => Config -> Table r -> (Record Flat r -> ReadPlaceholders p (Assign r) ()) -> SQLWithPlaceholderOffsets'
updateFromUpdateTarget config tbl ut = (\t r ->  UPDATE <> t <> r) <$> twp <*> rwp
  where (twp, rwp) = fromUpdateTarget config tbl ut

-- | Show Set clause and WHERE clause.
instance (PersistableWidth p, TableDerivable r) => Show (Record Flat r -> ReadPlaceholders p (Assign r) ()) where
  show = showStringSQL . detachPlaceholderOffsets . sqlFromUpdateTarget defaultConfig derivedTable


-- | InsertTarget type with place-holder parameter 'p' and projected record type 'r'.
newtype InsertTarget p r = InsertTarget (ReadPlaceholders p (Register r) ())

-- | Finalize 'Register' monad and generate 'InsertTarget'.
insertTarget :: ReadPlaceholders () (Register r) ()
             -> InsertTarget () r
insertTarget = InsertTarget
{-# DEPRECATED insertTarget "old-style API. Use new-style Database.Relational.insertValueNoPH ." #-}

-- | Finalize 'Register' monad and generate 'InsertTarget' with place-holder parameter 'p'.
insertTarget' :: ReadPlaceholders p (Register r) ()
              -> InsertTarget p r
insertTarget' = InsertTarget

-- | parametalized 'Register' monad from 'Pi'
piRegister :: PersistableWidth r
           => Pi r r'
           -> ReadPlaceholders r' (Register r) ()
piRegister pi' = readPlaceholders (pi' <-# pwPlaceholders (Pi.width' pi'))

sqlChunkFromInsertTarget' :: Config
                          -> Int
                          -> Record PureOperand p
                          -> Table r
                          -> InsertTarget p r
                          -> SQLWithPlaceholderOffsets'
sqlChunkFromInsertTarget' config sz phs tbl (InsertTarget q) =
    (\cs -> INSERT <> INTO <> stringSQL (Table.name tbl) <> cs) <$> composeChunkValuesWithColumns sz (asR tbl)
  where
    (_, asR) = Register.extract (runReadPlaceholders q phs) config

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
                         -> Record PureOperand p
                         -> Table r
                         -> InsertTarget p r
                         -> (SQLWithPlaceholderOffsets', Int)
sqlChunkFromInsertTarget config phs tbl it =
    (sqlChunkFromInsertTarget' config n phs tbl it, n)
  where
    n = countChunks config tbl

-- | Make 'StringSQL' string of SQL INSERT statement from 'InsertTarget'
sqlFromInsertTarget :: Config -> Record PureOperand p -> Table r -> InsertTarget p r -> SQLWithPlaceholderOffsets'
sqlFromInsertTarget config = sqlChunkFromInsertTarget' config 1

-- | Make 'StringSQL' strings of SQL INSERT strings from records list
sqlChunksFromRecordList :: LiteralSQL r'
                        => Config
                        -> Table r
                        -> Pi r r'
                        -> [r']
                        -> [SQLWithPlaceholderOffsets']
sqlChunksFromRecordList config tbl pi' xs =
    [ (\cs -> INSERT <> INTO <> stringSQL (Table.name tbl) <> cs)
      <$>
        composeValuesListWithColumns
        [ tf tbl
        | r <- rs
        , let ((), tf) = Register.extract (pi' <-# Record.toFlat (value r)) config
        ]
    | rs <- unfoldr step xs
    ]
  where
    n = countChunks config tbl
    step ys
      | null ys    =  Nothing
      | otherwise = Just $ splitAt n ys
