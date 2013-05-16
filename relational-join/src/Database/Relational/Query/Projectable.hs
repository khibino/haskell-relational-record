module Database.Relational.Query.Projectable (
  Projectable (project),

  value,

  valueTrue, valueFalse,

  values,

  SqlProjectable (unsafeSqlValue),
  valueNull, placeholder,

  ProjectableSqlTerm (unsafeSqlTerm),
  unsafeBinOp,

  (.=.), (.<>.), (.>.), (.<.), in', isNull, and, or,

  (.+.), (.-.), (./.), (.*.)
  ) where

import Prelude hiding (and, or)

import Data.List (intercalate)

import qualified Language.SQL.Keyword as SQL

import Database.Relational.Query.Expr (Expr, ShowConstantSQL (showConstantSQL))
import qualified Database.Relational.Query.Expr.Unsafe as UnsafeExpr

import Database.Relational.Query.Projection (Projection, columns, unsafeFromColumns)


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
  unsafeSqlValue :: String -> p t

instance SqlProjectable Projection where
  unsafeSqlValue = unsafeSqlProjection

instance SqlProjectable Expr where
  unsafeSqlValue = UnsafeExpr.Expr

valueNull :: SqlProjectable p => p (Maybe a)
valueNull =  unsafeSqlValue "NULL"

placeholder :: SqlProjectable p => p t
placeholder =  unsafeSqlValue "?"

value :: (ShowConstantSQL t, SqlProjectable p) => t -> p t
value =  unsafeSqlValue . showConstantSQL

valueTrue :: SqlProjectable p => p Bool
valueTrue =  value True

valueFalse :: SqlProjectable p => p Bool
valueFalse =  value False

values :: (Projectable p, ShowConstantSQL t) => [t] -> p [t]
values =  project . unsafeFromColumns . map showConstantSQL


class ProjectableSqlTerm p where
  unsafeSqlTerm :: p a -> String

instance ProjectableSqlTerm Projection where
  unsafeSqlTerm = sqlString

instance ProjectableSqlTerm Expr where
  unsafeSqlTerm = UnsafeExpr.showExpr


sqlBinOp :: String -> SQL.Keyword -> SQL.Keyword -> SQL.Keyword
sqlBinOp =  SQL.defineBinOp . SQL.word

unsafeBinOp :: (SqlProjectable p, ProjectableSqlTerm p)
            => (SQL.Keyword -> SQL.Keyword -> SQL.Keyword)
            -> p a -> p b -> p c
unsafeBinOp op a b = unsafeSqlValue . paren . SQL.wordShow
                     $ op (wordTerm a) (wordTerm b)
  where wordTerm = SQL.word . unsafeSqlTerm
        paren = ('(' :) . (++[')'])

compareBinOp :: (SqlProjectable p, ProjectableSqlTerm p)
             => (SQL.Keyword -> SQL.Keyword -> SQL.Keyword)
             -> p a -> p a -> p Bool
compareBinOp =  unsafeBinOp

numBinOp :: (SqlProjectable p, ProjectableSqlTerm p, Num a)
         => (SQL.Keyword -> SQL.Keyword -> SQL.Keyword)
         -> p a -> p a -> p a
numBinOp =  unsafeBinOp


(.=.) :: (SqlProjectable p, ProjectableSqlTerm p)
      => p ft -> p ft -> p Bool
(.=.) =  compareBinOp (SQL..=.)

(.<>.) :: (SqlProjectable p, ProjectableSqlTerm p)
       => p ft -> p ft -> p Bool
(.<>.) =  compareBinOp (SQL..<>.)

(.>.) :: (SqlProjectable p, ProjectableSqlTerm p)
      => p ft -> p ft -> p Bool
(.>.) =  compareBinOp (SQL..>.)

(.<.) :: (SqlProjectable p, ProjectableSqlTerm p)
      => p ft -> p ft -> p Bool
(.<.) =  compareBinOp (SQL..<.)

and :: (SqlProjectable p, ProjectableSqlTerm p)
    => p Bool ->  p Bool ->  p Bool
and =  compareBinOp SQL.and

or :: (SqlProjectable p, ProjectableSqlTerm p)
   => p Bool ->  p Bool ->  p Bool
or =  compareBinOp SQL.or

numBinOp' :: (SqlProjectable p, ProjectableSqlTerm p, Num a)
          => String -> p a -> p a -> p a
numBinOp' = numBinOp . sqlBinOp

(.+.) =  numBinOp' "+"
(.-.) =  numBinOp' "-"
(./.) =  numBinOp' "/"
(.*.) =  numBinOp' "*"

(.+.), (.-.), (./.), (.*.) :: (SqlProjectable p, ProjectableSqlTerm p, Num a)
                           => p a -> p a -> p a

in' :: (SqlProjectable p, ProjectableSqlTerm p)
    => p t -> p [t] -> p Bool
in' =  unsafeBinOp (SQL.in')

isNull :: (SqlProjectable p, ProjectableSqlTerm p)
       => p (Maybe t) -> p Bool
isNull x = compareBinOp (SQL.defineBinOp SQL.IS) x valueNull

infixl 7 .*., ./.
infixl 6 .+., .-.
infix  4 .=., .<>., .>., .<., `in'`
infixr 3 `and`
infixr 2 `or`
