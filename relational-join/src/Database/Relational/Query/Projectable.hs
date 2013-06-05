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


sqlString :: Projection r -> String
sqlString =  d . columns  where
  d ([])  = error $ "Projection: no columns."
  d ([c]) = c
  d (cs) = '(' : intercalate ", " cs ++ [')']

toExpr :: Projection r -> Expr r
toExpr =  UnsafeExpr.Expr . sqlString  where

class Projectable p where
  project :: Projection a -> p a

instance Projectable Projection where
  project = id

instance Projectable Expr where
  project = toExpr

projectAggregation :: Projectable p => Aggregation a -> p a
projectAggregation =  project . Aggregation.projection


unsafeSqlProjection :: String -> Projection t
unsafeSqlProjection =  unsafeFromColumns . (:[])

class SqlProjectable p where
  unsafeProjectSql :: String -> p t

instance SqlProjectable Projection where
  unsafeProjectSql = unsafeSqlProjection

instance SqlProjectable Expr where
  unsafeProjectSql = UnsafeExpr.Expr

instance SqlProjectable Aggregation where
  unsafeProjectSql = Aggregation.unsafeFromProjection . unsafeProjectSql

valueNull :: SqlProjectable p => p (Maybe a)
valueNull =  unsafeProjectSql "NULL"

value :: (ShowConstantSQL t, SqlProjectable p) => t -> p t
value =  unsafeProjectSql . showConstantSQL

valueTrue  :: (SqlProjectable p, ProjectableMaybe p) => p (Maybe Bool)
valueTrue  =  just $ value True

valueFalse :: (SqlProjectable p, ProjectableMaybe p) => p (Maybe Bool)
valueFalse =  just $ value False

values :: (Projectable p, ShowConstantSQL t) => [t] -> p [t]
values =  project . unsafeFromColumns . map showConstantSQL


class ProjectableShowSql p where
  unsafeShowSql :: p a -> String

instance ProjectableShowSql Projection where
  unsafeShowSql = sqlString

instance ProjectableShowSql Expr where
  unsafeShowSql = UnsafeExpr.showExpr

instance ProjectableShowSql Aggregation where
  unsafeShowSql = unsafeShowSql . Aggregation.projection


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
