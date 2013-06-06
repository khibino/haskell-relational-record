-- |
-- Module      : Database.Relational.Query.Projectable
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines operators on various polymorphic projections.
module Database.Relational.Query.Projectable (
  -- * Projectable from Projections
  Projectable (project),

  projectAggregation,
  expr,

  -- * Projectable from SQL strings
  SqlProjectable (unsafeProjectSql),

  value,
  valueTrue, valueFalse,
  values,
  valueNull,

  -- * Placeholders
  PlaceHolders, addPlaceHolders,
  placeholder', placeholder,

  -- * Aggregate functions
  unsafeAggregateOp,
  count, sum', avg, max', min', every, any', some',

  -- * Projectable into SQL strings
  ProjectableShowSql (unsafeShowSql),

  -- * Binary Operators
  SqlBinOp,
  unsafeBinOp,

  (.=.), (.<>.), (.>.), (.<.), in', isNull, and, or,

  (.+.), (.-.), (./.), (.*.),

  -- * Manipulate phantom types
  ProjectableZip (projectZip), (><),
  ProjectableMaybe (just, flattenMaybe)
  ) where

import Prelude hiding (and, or)

import Data.Int (Int32)
import Data.List (intercalate)

import qualified Language.SQL.Keyword as SQL
import qualified Language.SQL.Keyword.ConcatString as SQLs

import Database.Relational.Query.Expr (Expr, ShowConstantSQL (showConstantSQL))
import qualified Database.Relational.Query.Expr as Expr
import qualified Database.Relational.Query.Expr.Unsafe as UnsafeExpr

import Database.Relational.Query.Projection (Projection, columns, unsafeFromColumns)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Aggregation (Aggregation)
import qualified Database.Relational.Query.Aggregation as Aggregation


-- | SQL expression strings which represent projection.
sqlString :: Projection r -> String
sqlString =  d . columns  where
  d ([])  = error $ "Projection: no columns."
  d ([c]) = c
  d (cs) = '(' : intercalate ", " cs ++ [')']

-- | 'Expr' from 'Projection'
toExpr :: Projection r -> Expr r
toExpr =  UnsafeExpr.Expr . sqlString  where

-- | Projection interface.
class Projectable p where
  project :: Projection a -> p a

-- | Project into 'Projection' type.
instance Projectable Projection where
  project = id

-- | Project into 'Expr' type.
instance Projectable Expr where
  project = toExpr

-- | Project from 'Aggregation' like 'Projection'.
projectAggregation :: Projectable p => Aggregation a -> p a
projectAggregation =  project . Aggregation.projection

-- | 'Expr' from 'Projection'
expr :: Projection ft -> Expr ft
expr =  toExpr


-- | Unsafely make 'Projection' from single SQL term.
unsafeSqlProjection :: String -> Projection t
unsafeSqlProjection =  unsafeFromColumns . (:[])

-- | Interface to project single SQL term unsafely.
class SqlProjectable p where
  unsafeProjectSql :: String -> p t

-- | Unsafely make 'Projection' from single SQL term.
instance SqlProjectable Projection where
  unsafeProjectSql = unsafeSqlProjection

-- | Unsafely make 'Expr' from single SQL term.
instance SqlProjectable Expr where
  unsafeProjectSql = UnsafeExpr.Expr

-- | Unsafely make 'Aggregation' from single SQL term.
instance SqlProjectable Aggregation where
  unsafeProjectSql = Aggregation.unsafeFromProjection . unsafeProjectSql

-- | Polymorphic projection of SQL null value.
valueNull :: SqlProjectable p => p (Maybe a)
valueNull =  unsafeProjectSql "NULL"

-- | Generate polymorphic projection of SQL constant values from Haskell value.
value :: (ShowConstantSQL t, SqlProjectable p) => t -> p t
value =  unsafeProjectSql . showConstantSQL

-- | Polymorphic proejction of SQL true value.
valueTrue  :: (SqlProjectable p, ProjectableMaybe p) => p (Maybe Bool)
valueTrue  =  just $ value True

-- | Polymorphic proejction of SQL false value.
valueFalse :: (SqlProjectable p, ProjectableMaybe p) => p (Maybe Bool)
valueFalse =  just $ value False

-- | Polymorphic proejction of SQL set value from Haskell list.
values :: (Projectable p, ShowConstantSQL t) => [t] -> p [t]
values =  project . unsafeFromColumns . map showConstantSQL


-- | Interface to get SQL term from projections.
class ProjectableShowSql p where
  unsafeShowSql :: p a -> String

-- | Unsafely get SQL term from 'Proejction'.
instance ProjectableShowSql Projection where
  unsafeShowSql = sqlString

-- | Unsafely get SQL term from 'Expr'.
instance ProjectableShowSql Expr where
  unsafeShowSql = UnsafeExpr.showExpr

-- | Unsafely get SQL term from 'Aggregation.unsafeFromProjection'.
instance ProjectableShowSql Aggregation where
  unsafeShowSql = unsafeShowSql . Aggregation.projection


-- | Parened String.
paren :: String -> String
paren =  ('(' :) . (++[')'])

type SqlUniOp = String -> String

sqlUniOp :: SQL.Keyword -> SqlUniOp
sqlUniOp kw = (SQL.wordShow kw ++) . (' ' :) . paren

unsafeAggregateOp :: SQL.Keyword
                  -> Projection a -> Aggregation b
unsafeAggregateOp op = unsafeProjectSql . sqlUniOp op . unsafeShowSql

count :: Projection a -> Aggregation Int32
count =  unsafeAggregateOp SQL.COUNT

sum'  :: Num a => Projection a -> Aggregation a
sum'  =  unsafeAggregateOp SQL.SUM

avg   :: (Num a, Fractional b)=> Projection a -> Aggregation b
avg   =  unsafeAggregateOp SQL.AVG

max'  :: Ord a => Projection a -> Aggregation a
max'  =  unsafeAggregateOp SQL.MAX

min'  :: Ord a => Projection a -> Aggregation a
min'  =  unsafeAggregateOp SQL.MIN

every =  unsafeAggregateOp SQL.EVERY
any'  =  unsafeAggregateOp SQL.ANY
some' =  unsafeAggregateOp SQL.SOME

every, any', some' :: Projection (Maybe Bool) -> Aggregation (Maybe Bool)


type SqlBinOp = String -> String -> String

sqlBinOp :: String -> SqlBinOp
sqlBinOp =  SQLs.defineBinOp . SQL.word

unsafeBinOp :: (SqlProjectable p, ProjectableShowSql p)
            => SqlBinOp
            -> p a -> p b -> p c
unsafeBinOp op a b = unsafeProjectSql . paren
                     $ op (unsafeShowSql a) (unsafeShowSql b)

compareBinOp :: (SqlProjectable p, ProjectableShowSql p)
             => SqlBinOp
             -> p a -> p a -> p (Maybe Bool)
compareBinOp =  unsafeBinOp

numBinOp :: (SqlProjectable p, ProjectableShowSql p, Num a)
         => SqlBinOp
         -> p a -> p a -> p a
numBinOp =  unsafeBinOp


(.=.)  =  compareBinOp (SQLs..=.)
(.<>.) =  compareBinOp (SQLs..<>.)
(.>.)  =  compareBinOp (SQLs..>.)
(.<.)  =  compareBinOp (SQLs..<.)

(.=.), (.<>.), (.>.), (.<.)
  :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)

and = compareBinOp SQLs.and
or  = compareBinOp SQLs.or

and, or
  :: (SqlProjectable p, ProjectableShowSql p)
  => p (Maybe Bool) ->  p (Maybe Bool) ->  p (Maybe Bool)

numBinOp' :: (SqlProjectable p, ProjectableShowSql p, Num a)
          => String -> p a -> p a -> p a
numBinOp' = numBinOp . sqlBinOp

(.+.) =  numBinOp' "+"
(.-.) =  numBinOp' "-"
(./.) =  numBinOp' "/"
(.*.) =  numBinOp' "*"

(.+.), (.-.), (./.), (.*.)
  :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a

in' :: (SqlProjectable p, ProjectableShowSql p)
    => p t -> p [t] -> p (Maybe Bool)
in' =  unsafeBinOp (SQLs.in')

isNull :: (SqlProjectable p, ProjectableShowSql p)
       => p (Maybe t) -> p (Maybe Bool)
isNull x = compareBinOp (SQLs.defineBinOp SQL.IS) x valueNull


data PlaceHolders p = PlaceHolders

addPlaceHolders :: Functor f => f a -> f (PlaceHolders p, a)
addPlaceHolders =  fmap ((,) PlaceHolders)

unsafeCastPlaceHolders :: PlaceHolders a -> PlaceHolders b
unsafeCastPlaceHolders PlaceHolders = PlaceHolders

placeholder' :: SqlProjectable p => (p t -> a) ->  (PlaceHolders t, a)
placeholder' f = (PlaceHolders, f $ unsafeProjectSql "?")

placeholder :: (SqlProjectable p, Monad m) => (p t -> m a) -> m (PlaceHolders t, a)
placeholder f = do
  let (ph, ma) = placeholder' f
  a <- ma
  return (ph, a)


class ProjectableZip p where
  projectZip :: p a -> p b -> p (a, b)

instance ProjectableZip PlaceHolders where
  projectZip PlaceHolders PlaceHolders = PlaceHolders

instance ProjectableZip Projection where
  projectZip = Projection.compose

instance ProjectableZip Aggregation where
  projectZip = Aggregation.compose

(><) ::ProjectableZip p => p a -> p b -> p (a, b)
(><) = projectZip

class ProjectableMaybe p where
  just :: p a -> p (Maybe a)
  flattenMaybe :: p (Maybe (Maybe a)) -> p (Maybe a)

instance ProjectableMaybe PlaceHolders where
  just         = unsafeCastPlaceHolders
  flattenMaybe = unsafeCastPlaceHolders

instance ProjectableMaybe Projection where
  just         = Projection.just
  flattenMaybe = Projection.flattenMaybe

instance ProjectableMaybe Expr where
  just         = Expr.just
  flattenMaybe = Expr.flattenMaybe

instance ProjectableMaybe Aggregation where
  just         = Aggregation.just
  flattenMaybe = Aggregation.flattenMaybe


infixl 7 .*., ./.
infixl 6 .+., .-.
infix  4 .=., .<>., .>., .<., `in'`
infixr 3 `and`
infixr 2 `or`
infixl 1  ><
