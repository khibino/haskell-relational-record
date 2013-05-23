module Database.Relational.Query.Projectable (
  Projectable (project),

  value,

  valueTrue, valueFalse,

  values,

  SqlProjectable (unsafeProjectSql),
  valueNull, placeholder,

  ProjectableShowSql (unsafeShowSql),
  unsafeBinOp,

  (.=.), (.<>.), (.>.), (.<.), in', isNull, and, or,

  (.+.), (.-.), (./.), (.*.),

  PlaceHolders, addPlaceHolders,

  ProjectableZip (projectZip), (><),
  ProjectableMaybe (just, flattenMaybe)
  ) where

import Prelude hiding (and, or)

import Data.List (intercalate)

import qualified Language.SQL.Keyword as SQL
import qualified Language.SQL.Keyword.ConcatString as SQLs

import Database.Relational.Query.Expr (Expr, ShowConstantSQL (showConstantSQL))
import qualified Database.Relational.Query.Expr as Expr
import qualified Database.Relational.Query.Expr.Unsafe as UnsafeExpr

import Database.Relational.Query.Projection (Projection, columns, unsafeFromColumns)
import qualified Database.Relational.Query.Projection as Projection


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

unsafeSqlProjection :: String -> Projection t
unsafeSqlProjection =  unsafeFromColumns . (:[])


class SqlProjectable p where
  unsafeProjectSql :: String -> p t

instance SqlProjectable Projection where
  unsafeProjectSql = unsafeSqlProjection

instance SqlProjectable Expr where
  unsafeProjectSql = UnsafeExpr.Expr

valueNull :: SqlProjectable p => p (Maybe a)
valueNull =  unsafeProjectSql "NULL"

placeholder :: SqlProjectable p => p t
placeholder =  unsafeProjectSql "?"

value :: (ShowConstantSQL t, SqlProjectable p) => t -> p t
value =  unsafeProjectSql . showConstantSQL

valueTrue :: SqlProjectable p => p Bool
valueTrue =  value True

valueFalse :: SqlProjectable p => p Bool
valueFalse =  value False

values :: (Projectable p, ShowConstantSQL t) => [t] -> p [t]
values =  project . unsafeFromColumns . map showConstantSQL


class ProjectableShowSql p where
  unsafeShowSql :: p a -> String

instance ProjectableShowSql Projection where
  unsafeShowSql = sqlString

instance ProjectableShowSql Expr where
  unsafeShowSql = UnsafeExpr.showExpr


type SqlBinOp = String -> String -> String

sqlBinOp :: String -> SqlBinOp
sqlBinOp =  SQLs.defineBinOp . SQL.word

unsafeBinOp :: (SqlProjectable p, ProjectableShowSql p)
            => SqlBinOp
            -> p a -> p b -> p c
unsafeBinOp op a b = unsafeProjectSql . paren
                     $ op (unsafeShowSql a) (unsafeShowSql b)
  where paren = ('(' :) . (++[')'])

compareBinOp :: (SqlProjectable p, ProjectableShowSql p)
             => SqlBinOp
             -> p a -> p a -> p Bool
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
  => p ft -> p ft -> p Bool

and = compareBinOp SQLs.and
or  = compareBinOp SQLs.or

and, or
  :: (SqlProjectable p, ProjectableShowSql p)
  => p Bool ->  p Bool ->  p Bool

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
    => p t -> p [t] -> p Bool
in' =  unsafeBinOp (SQLs.in')

isNull :: (SqlProjectable p, ProjectableShowSql p)
       => p (Maybe t) -> p Bool
isNull x = compareBinOp (SQLs.defineBinOp SQL.IS) x valueNull


data PlaceHolders p = PlaceHolders

addPlaceHolders :: Functor f => f a -> f (PlaceHolders p, a)
addPlaceHolders =  fmap ((,) PlaceHolders)

unsafeCastPlaceHolders :: PlaceHolders a -> PlaceHolders b
unsafeCastPlaceHolders PlaceHolders = PlaceHolders


class ProjectableZip p where
  projectZip :: p a -> p b -> p (a, b)

instance ProjectableZip PlaceHolders where
  projectZip PlaceHolders PlaceHolders = PlaceHolders

instance ProjectableZip Projection where
  projectZip = Projection.compose

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


infixl 7 .*., ./.
infixl 6 .+., .-.
infix  4 .=., .<>., .>., .<., `in'`
infixr 3 `and`
infixr 2 `or`
infixl 1  ><
