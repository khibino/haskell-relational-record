{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Module      : Database.Relational.Effect
-- Copyright   : 2013-2018 Kei Hibino
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
  UpdateTarget, updateTarget, updateTarget',
  liftTargetAllColumn, liftTargetAllColumn',
  updateTargetAllColumn, updateTargetAllColumn',

  -- * Object to express insert terget.
  InsertTarget, insertTarget, insertTarget', piRegister,

  -- * Generate SQL from restriction.
  sqlWhereFromRestriction,
  sqlFromUpdateTarget,
  sqlChunkFromInsertTarget,
  sqlFromInsertTarget,
  sqlChunksFromRecordList,
  ) where


import Data.Monoid ((<>))
import Data.List (unfoldr)
import qualified Data.Extensible as E
import Data.Functor.ProductIsomorphic (peRight)

import Language.SQL.Keyword (Keyword(..))
import Database.Record.Persistable (PersistableWidth)

import Database.Relational.ExtensibleRecord (ExRecord, type (++))
import Database.Relational.Internal.Config (Config (chunksInsertSize), defaultConfig)
import Database.Relational.Internal.String (StringSQL, stringSQL, showStringSQL)
import Database.Relational.ReboundSyntax (ireturn, IxMonad)
import Database.Relational.Internal.ContextType (Flat)
import qualified Database.Relational.ReboundSyntax as ReboundSyntax
import Database.Relational.SqlSyntax
  (composeWhere, composeSets, composeChunkValuesWithColumns, composeValuesListWithColumns)

import Database.Relational.Pi (Pi, id')
import qualified Database.Relational.Pi.Unsafe as Pi
import Database.Relational.Table (Table, TableDerivable, derivedTable)
import qualified Database.Relational.Table as Table
import qualified Database.Relational.Record as Record
import Database.Relational.ProjectableClass (ShowConstantTermsSQL)
import Database.Relational.Projectable
  (PlaceHolders, unitPH, pwPlaceholder, placeholder, (><), value, )
import Database.Relational.Monad.Trans.Assigning (assignings, (<-#))
import Database.Relational.Monad.Restrict (RestrictedStatement)
import qualified Database.Relational.Monad.Restrict as Restrict
import Database.Relational.Monad.Assign (AssignStatement)
import qualified Database.Relational.Monad.Assign as Assign
import Database.Relational.Monad.Register (Register)
import qualified Database.Relational.Monad.Register as Register
import           Database.Relational.Monad.Trans.Placeholders (placeholders, addPlaceholders)

import Prelude hiding ((>>=), (>>))
import qualified Prelude


-- | Restriction type with place-holder parameter 'p' and projected record type 'r'.
newtype Restriction p r j = Restriction (RestrictedStatement r j (PlaceHolders p))

-- | Finalize 'Restrict' monad and generate 'Restriction'.
restriction :: RestrictedStatement r j () -> Restriction () r j
restriction = Restriction . ((Prelude.>> return unitPH) .)

-- | Finalize 'Restrict' monad and generate 'Restriction' with place-holder parameter 'p'
restriction' :: RestrictedStatement r j (PlaceHolders p) -> Restriction p r j
restriction' = Restriction

runRestriction :: Restriction p r j
               -> RestrictedStatement r j (PlaceHolders p)
runRestriction (Restriction qf) = qf

-- | SQL WHERE clause 'StringSQL' string from 'Restriction'.
sqlWhereFromRestriction :: Config -> Table r -> Restriction p r j -> StringSQL
sqlWhereFromRestriction config tbl (Restriction q) = composeWhere rs
  where (_ph, rs) = Restrict.extract (q $ Record.unsafeFromTable tbl) config

-- | Show where clause.
instance TableDerivable r => Show (Restriction p r j) where
  show = showStringSQL . sqlWhereFromRestriction defaultConfig derivedTable


-- | UpdateTarget type with place-holder parameter 'p' and projected record type 'r'.
-- igrep NOTE: Uncecessary? Looks like that it just adds extra internal newtype...
--             Actually, the external API update function uses AssignStatement i j r (PlaceHolders p) directly
newtype UpdateTarget p r i j = UpdateTarget (AssignStatement r i j (PlaceHolders p))

-- | Finalize 'Target' monad and generate 'UpdateTarget'.
updateTarget :: AssignStatement r i j ()
             -> UpdateTarget () r i j
updateTarget =  UpdateTarget . ((ReboundSyntax.>>= const (ireturn unitPH)) .)

-- | Finalize 'Target' monad and generate 'UpdateTarget' with place-holder parameter 'p'.
updateTarget' :: AssignStatement r i j (PlaceHolders p)
              -> UpdateTarget p r i j
updateTarget' = UpdateTarget

updateAllColumn :: PersistableWidth r
                => Restriction p r (ExRecord ys)
                -- igrep NOTE: Ideally, the result ExRecord should be ExRecord (xs ++ ysAppendedByPlaceholder ++ ysAppendedByRunRestriction)
                -- -> AssignStatement r (ExRecord xs) (ExRecord (xs ++ ys)) (PlaceHolders (r, p))
                -> AssignStatement r (ExRecord xs) (ExRecord (xs ++ ys)) (PlaceHolders (r, p))
updateAllColumn rs proj = do
  undefined
  {-
    (ph0, ()) <- placeholder (\ph -> id' <-# ph)
    -- igrep TODO: append (ExRecord ys). Don't use `placeholders`, which never append indices
    ph1       <- placeholders $ assignings $ runRestriction rs proj
    ireturn $ ph0 >< ph1
  where
    (>>=) :: forall m i j k a b. IxMonad m => m i j a -> (a -> m j k b) -> m i k b
    (>>=) = (ReboundSyntax.>>=)

    (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
    (>>) = (ReboundSyntax.>>)
  -}

-- | Lift 'Restriction' to 'UpdateTarget'. Update target columns are all.
liftTargetAllColumn :: PersistableWidth r
                     => Restriction () r (ExRecord ys)
                     -> UpdateTarget r r (ExRecord xs) (ExRecord (xs ++ ys))
liftTargetAllColumn rs = updateTarget' $ \proj -> fmap peRight $ updateAllColumn rs proj

-- | Lift 'Restriction' to 'UpdateTarget'. Update target columns are all. With placefolder type 'p'.
liftTargetAllColumn' :: PersistableWidth r
                     => Restriction p r (ExRecord ys)
                     -> UpdateTarget (r, p) r (ExRecord xs) (ExRecord (xs ++ ys))
liftTargetAllColumn' rs = updateTarget' $ updateAllColumn rs

-- | Finalize 'Restrict' monad and generate 'UpdateTarget'. Update target columns are all.
--  igrep TODO: unused?
updateTargetAllColumn :: PersistableWidth r
                      => RestrictedStatement r (ExRecord ys) ()
                      -> UpdateTarget r r (ExRecord xs) (ExRecord (xs ++ ys))
updateTargetAllColumn = liftTargetAllColumn . restriction

-- | Finalize 'Restrict' monad and generate 'UpdateTarget'. Update target columns are all. With placefolder type 'p'.
updateTargetAllColumn' :: PersistableWidth r
                       => RestrictedStatement r (ExRecord ys) (PlaceHolders p)
                       -> UpdateTarget (r, p) r (ExRecord xs) (ExRecord (xs ++ ys))
updateTargetAllColumn' = liftTargetAllColumn' . restriction'


-- | SQL SET clause and WHERE clause 'StringSQL' string from 'UpdateTarget'
sqlFromUpdateTarget :: Config -> Table r -> UpdateTarget p r i j -> StringSQL
sqlFromUpdateTarget config tbl (UpdateTarget q) = composeSets (asR tbl) <> composeWhere rs
  where ((_ph, asR), rs) = Assign.extract (q (Record.unsafeFromTable tbl)) config

instance TableDerivable r => Show (UpdateTarget p r i j) where
  show = showStringSQL . sqlFromUpdateTarget defaultConfig derivedTable


-- | InsertTarget type with place-holder parameter 'p' and projected record type 'r'.
newtype InsertTarget p i j r = InsertTarget (Register i j r (PlaceHolders p))

-- | Finalize 'Register' monad and generate 'InsertTarget'.
insertTarget :: Register i j r ()
             -> InsertTarget () i j r
insertTarget =  InsertTarget . (ReboundSyntax.>>= (const $ ireturn unitPH))

-- | Finalize 'Target' monad and generate 'UpdateTarget' with place-holder parameter 'p'.
insertTarget' :: Register i j r (PlaceHolders p)
              -> InsertTarget p i j r
insertTarget' = InsertTarget

-- | parametalized 'Register' monad from 'Pi'
piRegister :: forall r r' xs
            . PersistableWidth r
           => Pi r r' -- igrep TODO: Convert r' to ExRecord ys to append to the result
           -- -> Register (ExRecord xs) (ExRecord (xs ++ ys)) r (PlaceHolders r')
           -> Register (ExRecord xs) (ExRecord xs) r (PlaceHolders r')
piRegister pi' = do
  undefined
  {-
    let act :: Record.Record (ExRecord '[]) (ExRecord '[]) Flat r' -> Register (ExRecord xs) (ExRecord xs) r ()
        act ph = pi' <-# ph
        ph' :: PlaceHolders r'
        ma :: Register (ExRecord xs) (ExRecord xs) r ()
        (ph', ma) = pwPlaceholder (Pi.width' pi') act
    () <- ma
    ireturn ph'
  where
    (>>=) :: forall m i j k a b. IxMonad m => m i j a -> (a -> m j k b) -> m i k b
    (>>=) = (ReboundSyntax.>>=)

    (>>) :: IxMonad m => m i j a -> m j k b -> m i k b
    (>>) = (ReboundSyntax.>>)
  -}

sqlChunkFromInsertTarget' :: Config
                          -> Int
                          -> Table r
                          -> InsertTarget p i j r
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
                         -> InsertTarget p i j r
                         -> (StringSQL, Int)
sqlChunkFromInsertTarget config tbl it =
    (sqlChunkFromInsertTarget' config n tbl it, n)
  where
    n = countChunks config tbl

-- | Make 'StringSQL' string of SQL INSERT statement from 'InsertTarget'
sqlFromInsertTarget :: Config -> Table r -> InsertTarget p i j r -> StringSQL
sqlFromInsertTarget config = sqlChunkFromInsertTarget' config 1

-- | Make 'StringSQL' strings of SQL INSERT strings from records list
sqlChunksFromRecordList :: ShowConstantTermsSQL r'
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
