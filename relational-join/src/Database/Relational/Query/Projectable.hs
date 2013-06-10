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

  AggregateProjectable (projectAggregation),
  expr,

  -- * Projectable from SQL strings
  SqlProjectable (unsafeProjectSqlTerms), unsafeProjectSql,

  -- * Projections of values
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

  (.=.), (.<.), (.<=.), (.>.), (.>=.), (.<>.),

  in', isNull, and, or,

  (.+.), (.-.), (./.), (.*.),

  -- * Zipping projections
  ProjectableZip (projectZip), (><),

  -- * 'Maybe' type projecitoins
  ProjectableMaybe (just, flattenMaybe)
  ) where

import Prelude hiding (and, or)

import Data.Int (Int32)
import Data.List (intercalate)
import Control.Applicative ((<$>))

import qualified Language.SQL.Keyword as SQL
import qualified Language.SQL.Keyword.ConcatString as SQLs

import Database.Record (PersistableWidth, PersistableRecordWidth, derivedWidth)

import Database.Relational.Query.Expr (Expr, ShowConstantSQL (showConstantSQL))
import qualified Database.Relational.Query.Expr as Expr
import qualified Database.Relational.Query.Expr.Unsafe as UnsafeExpr

import Database.Relational.Query.Projection (Projection, columns, unsafeFromColumns)
import qualified Database.Relational.Query.Projection as Projection

import Database.Relational.Query.Aggregation (Aggregation)
import qualified Database.Relational.Query.Aggregation as Aggregation


-- | Parened String.
paren :: String -> String
paren =  ('(' :) . (++[')'])

-- | String of SQL terms.
sqlTermsString :: [String] -> String
sqlTermsString = d  where
  d ([])  = error $ "Projection: no columns."
  d ([c]) = c
  d (cs) =  paren $ intercalate ", " cs

-- | SQL expression strings which represent projection.
sqlString :: Projection r -> String
sqlString =  sqlTermsString . columns  where

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

-- | Projection interface for 'Aggregation'.
class AggregateProjectable p where
  projectAggregation :: Aggregation a -> p a

-- | Project from 'Aggregation' into 'Aggregation'.
instance AggregateProjectable Aggregation where
  projectAggregation = id

-- | Project from 'Aggregation' into 'Projection'.
instance AggregateProjectable Projection where
  projectAggregation = project . Aggregation.projection

-- | Project from 'Aggregation' into 'Expr'.
instance AggregateProjectable Expr where
  projectAggregation = project . Aggregation.projection

-- | 'Expr' from 'Projection'
expr :: Projection ft -> Expr ft
expr =  toExpr


unsafeSqlTermsProjection :: [String] -> Projection t
unsafeSqlTermsProjection =  unsafeFromColumns

-- | Interface to project SQL terms unsafely.
class SqlProjectable p where
  unsafeProjectSqlTerms :: [String] -> p t

-- | Unsafely make 'Projection' from SQL terms.
instance SqlProjectable Projection where
  unsafeProjectSqlTerms = unsafeSqlTermsProjection

-- | Unsafely make 'Expr' from SQL terms.
instance SqlProjectable Expr where
  unsafeProjectSqlTerms = UnsafeExpr.Expr . sqlTermsString

-- | Unsafely make 'Aggregation' from SQL terms.
instance SqlProjectable Aggregation where
  unsafeProjectSqlTerms = Aggregation.unsafeFromProjection . unsafeProjectSqlTerms

-- | Unsafely Project single SQL term.
unsafeProjectSql :: SqlProjectable p => String -> p t
unsafeProjectSql =  unsafeProjectSqlTerms . (:[])

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


-- | Uni-operator type for SQL String
type SqlUniOp = String -> String

-- | Uni-operator from SQL keyword.
sqlUniOp :: SQL.Keyword -> SqlUniOp
sqlUniOp kw = (SQL.wordShow kw ++) . (' ' :) . paren

-- | Unsafely make aggregation uni-operator from SQL keyword.
unsafeAggregateOp :: SQL.Keyword
                  -> Projection a -> Aggregation b
unsafeAggregateOp op = unsafeProjectSql . sqlUniOp op . unsafeShowSql

-- | Aggregation function COUNT.
count :: Projection a -> Aggregation Int32
count =  unsafeAggregateOp SQL.COUNT

-- | Aggregation function SUM.
sum'  :: Num a => Projection a -> Aggregation a
sum'  =  unsafeAggregateOp SQL.SUM

-- | Aggregation function AVG.
avg   :: (Num a, Fractional b)=> Projection a -> Aggregation b
avg   =  unsafeAggregateOp SQL.AVG

-- | Aggregation function MAX.
max'  :: Ord a => Projection a -> Aggregation a
max'  =  unsafeAggregateOp SQL.MAX

-- | Aggregation function MIN.
min'  :: Ord a => Projection a -> Aggregation a
min'  =  unsafeAggregateOp SQL.MIN

-- | Aggregation function EVERY.
every :: Projection (Maybe Bool) -> Aggregation (Maybe Bool)
every =  unsafeAggregateOp SQL.EVERY

-- | Aggregation function ANY.
any'  :: Projection (Maybe Bool) -> Aggregation (Maybe Bool)
any'  =  unsafeAggregateOp SQL.ANY

-- | Aggregation function SOME.
some' :: Projection (Maybe Bool) -> Aggregation (Maybe Bool)
some' =  unsafeAggregateOp SQL.SOME


-- | Binary operator type for SQL String.
type SqlBinOp = String -> String -> String

-- | Binary operator from SQL operator string.
sqlBinOp :: String -> SqlBinOp
sqlBinOp =  SQLs.defineBinOp . SQL.word

-- | Unsafely make projection binary operator from SQL operator string.
unsafeBinOp :: (SqlProjectable p, ProjectableShowSql p)
            => SqlBinOp
            -> p a -> p b -> p c
unsafeBinOp op a b = unsafeProjectSql . paren
                     $ op (unsafeShowSql a) (unsafeShowSql b)

-- | Unsafely make compare projection binary operator from string binary operator.
compareBinOp :: (SqlProjectable p, ProjectableShowSql p)
             => SqlBinOp
             -> p a -> p a -> p (Maybe Bool)
compareBinOp =  unsafeBinOp

-- | Unsafely make number projection binary operator from string binary operator.
numBinOp :: (SqlProjectable p, ProjectableShowSql p, Num a)
         => SqlBinOp
         -> p a -> p a -> p a
numBinOp =  unsafeBinOp


-- | Compare operator corresponding SQL /=/ .
(.=.)  :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.=.)  =  compareBinOp (SQLs..=.)

-- | Compare operator corresponding SQL /</ .
(.<.)  :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.<.)  =  compareBinOp (SQLs..<.)

-- | Compare operator corresponding SQL /<=/ .
(.<=.)  :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.<=.)  =  compareBinOp (SQLs..<=.)

-- | Compare operator corresponding SQL />/ .
(.>.)  :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.>.)  =  compareBinOp (SQLs..>.)

-- | Compare operator corresponding SQL />=/ .
(.>=.)  :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.>=.)  =  compareBinOp (SQLs..>=.)

-- | Compare operator corresponding SQL /<>/ .
(.<>.) :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.<>.) =  compareBinOp (SQLs..<>.)

-- | Logical operator corresponding SQL /AND/ .
and :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
and =  compareBinOp SQLs.and

-- | Logical operator corresponding SQL /OR/ .
or  :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
or  =  compareBinOp SQLs.or

-- | Unsafely make number projection binary operator from SQL operator string.
numBinOp' :: (SqlProjectable p, ProjectableShowSql p, Num a)
          => String -> p a -> p a -> p a
numBinOp' = numBinOp . sqlBinOp

-- | Number operator corresponding SQL /+/ .
(.+.) :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a
(.+.) =  numBinOp' "+"

-- | Number operator corresponding SQL /-/ .
(.-.) :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a
(.-.) =  numBinOp' "-"

-- | Number operator corresponding SQL /// .
(./.) :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a
(./.) =  numBinOp' "/"

-- | Number operator corresponding SQL /*/ .
(.*.) :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a
(.*.) =  numBinOp' "*"

-- | Binary operator corresponding SQL /IN/ .
in' :: (SqlProjectable p, ProjectableShowSql p)
    => p t -> p [t] -> p (Maybe Bool)
in' =  unsafeBinOp (SQLs.in')

-- | Operator corresponding SQL /IS NULL/ .
isNull :: (SqlProjectable p, ProjectableShowSql p)
       => p (Maybe t) -> p (Maybe Bool)
isNull x = compareBinOp (SQLs.defineBinOp SQL.IS) x valueNull


-- | Placeholder parameter type which has real parameter type arguemnt 'p'.
data PlaceHolders p = PlaceHolders

-- | Unsafely add placeholder parameter to queries.
addPlaceHolders :: Functor f => f a -> f (PlaceHolders p, a)
addPlaceHolders =  fmap ((,) PlaceHolders)

-- | Unsafely cast placeholder parameter type.
unsafeCastPlaceHolders :: PlaceHolders a -> PlaceHolders b
unsafeCastPlaceHolders PlaceHolders = PlaceHolders

unsafeProjectPlaceHolder' :: (PersistableWidth r, SqlProjectable p)
                               => (PersistableRecordWidth r, p r)
unsafeProjectPlaceHolder' =  unsafeProjectSqlTerms . (`replicate` "?") <$> derivedWidth

unsafeProjectPlaceHolder :: (PersistableWidth r, SqlProjectable p)
                               => p r
unsafeProjectPlaceHolder =  snd unsafeProjectPlaceHolder'

-- | Provide scoped placeholder and return its parameter object.
placeholder' :: (PersistableWidth t, SqlProjectable p) => (p t -> a) ->  (PlaceHolders t, a)
placeholder' f = (PlaceHolders, f $ unsafeProjectPlaceHolder)

-- | Provide scoped placeholder and return its parameter object. Monadic version.
placeholder :: (PersistableWidth t, SqlProjectable p, Monad m) => (p t -> m a) -> m (PlaceHolders t, a)
placeholder f = do
  let (ph, ma) = placeholder' f
  a <- ma
  return (ph, a)


-- | Interface to zip projections.
class ProjectableZip p where
  projectZip :: p a -> p b -> p (a, b)

-- | Zip placeholder parameters.
instance ProjectableZip PlaceHolders where
  projectZip PlaceHolders PlaceHolders = PlaceHolders

-- | Zip 'Projection'.
instance ProjectableZip Projection where
  projectZip = Projection.compose

-- | Zip 'Aggregation'
instance ProjectableZip Aggregation where
  projectZip = Aggregation.compose

-- | Binary operator the same as 'projectZip'.
(><) ::ProjectableZip p => p a -> p b -> p (a, b)
(><) = projectZip

-- | Interface to control 'Maybe' of phantom type in projections.
class ProjectableMaybe p where
  just :: p a -> p (Maybe a)
  flattenMaybe :: p (Maybe (Maybe a)) -> p (Maybe a)

-- | Control phantom 'Maybe' type in placeholder parameters.
instance ProjectableMaybe PlaceHolders where
  just         = unsafeCastPlaceHolders
  flattenMaybe = unsafeCastPlaceHolders

-- | Control phantom 'Maybe' type in projection type 'Projection'.
instance ProjectableMaybe Projection where
  just         = Projection.just
  flattenMaybe = Projection.flattenMaybe

-- | Control phantom 'Maybe' type in SQL expression type 'Expr'.
instance ProjectableMaybe Expr where
  just         = Expr.just
  flattenMaybe = Expr.flattenMaybe

-- | Control phantom 'Maybe' type in aggregate projection type 'Projection'.
instance ProjectableMaybe Aggregation where
  just         = Aggregation.just
  flattenMaybe = Aggregation.flattenMaybe


infixl 7 .*., ./.
infixl 6 .+., .-.
infix  4 .=., .<>., .>., .<., `in'`
infixr 3 `and`
infixr 2 `or`
infixl 1  ><
