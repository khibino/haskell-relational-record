{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
  -- * Conversion between individual Projections
  expr,

  -- * Projectable from SQL strings
  SqlProjectable (unsafeProjectSqlTerms), unsafeProjectSql,

  -- * Projections of values
  value,
  valueTrue, valueFalse,
  values,
  unsafeValueNull,

  -- * Placeholders
  PlaceHolders, addPlaceHolders, unsafePlaceHolders,
  placeholder', placeholder,

  -- * Projectable into SQL strings
  unsafeShowSqlExpr,
  unsafeShowSqlProjection,
  ProjectableShowSql (unsafeShowSql),

  -- * Operators
  SqlBinOp,
  unsafeBinOp,

  unsafeUniOp,

  (.=.), (.<.), (.<=.), (.>.), (.>=.), (.<>.),

  casesOrElse, casesOrElse',
  caseSearch, caseSearchMaybe, case', caseMaybe,
  in', and', or',

  isNothing, isJust, isNull, isNotNull,
  fromMaybe', not', exists,

  (.||.), (?||?),
  (.+.), (.-.), (./.), (.*.), negate', fromIntegral', showNum,
  (?+?), (?-?), (?/?), (?*?), negateMaybe, fromIntegralMaybe, showNumMaybe,

  -- * Terms for Window function types
  rank, dense_rank, row_number, percent_rank, cume_dist,

  -- * Zipping projections
  projectZip, (><),
  ProjectableIdZip (..),

  -- * 'Maybe' type projecitoins
  ProjectableMaybe (just, flattenMaybe),

  -- * ProjectableFunctor and ProjectableApplicative
  ProjectableFunctor (..), ProjectableApplicative (..), ipfmap
  ) where

import Prelude hiding (pi)

import Data.Int (Int64)
import Data.String (IsString)
import Data.Monoid ((<>), mconcat)
import Control.Applicative ((<$>))

import Language.SQL.Keyword (Keyword)
import qualified Language.SQL.Keyword as SQL

import Database.Record
  (PersistableWidth, PersistableRecordWidth, derivedWidth,
   HasColumnConstraint, NotNull)

import Database.Relational.Query.Internal.SQL (stringSQL, showStringSQL, rowStringSQL)
import Database.Relational.Query.Context (Flat, Aggregated, Exists, OverWindow)
import Database.Relational.Query.Component (columnSQL, showsColumnSQL)
import Database.Relational.Query.Expr (Expr)
import qualified Database.Relational.Query.Expr as Expr
import qualified Database.Relational.Query.Expr.Unsafe as UnsafeExpr

import Database.Relational.Query.Pure
  (ShowConstantTermsSQL (showConstantTermsSQL))
import Database.Relational.Query.Pi (Pi)
import qualified Database.Relational.Query.Pi as Pi

import Database.Relational.Query.Pure (ProductConstructor (..))
import Database.Relational.Query.Projection
  (Projection, unsafeFromColumns, columns,
   ListProjection, unsafeShowSqlListProjection)
import qualified Database.Relational.Query.Projection as Projection


-- | Unsafely get SQL term from 'Proejction'.
unsafeShowSqlProjection :: Projection c r -> String
unsafeShowSqlProjection =  showStringSQL . rowStringSQL . map showsColumnSQL . columns

-- | 'Expr' from 'Projection'
exprOfProjection :: Projection c r -> Expr c r
exprOfProjection =  UnsafeExpr.Expr . stringSQL . unsafeShowSqlProjection

-- | Project from Projection type into expression type.
expr :: Projection p a -> Expr p a
expr =  exprOfProjection


-- | Unsafely generate 'Projection' from SQL expression strings.
unsafeSqlTermsProjection :: [String] -> Projection c t
unsafeSqlTermsProjection =  unsafeFromColumns . map columnSQL

-- | Interface to project SQL terms unsafely.
class SqlProjectable p where
  -- | Unsafely project from SQL expression strings.
  unsafeProjectSqlTerms :: [String] -- ^ SQL expression strings
                        -> p t      -- ^ Result projection object

-- | Unsafely make 'Projection' from SQL terms.
instance SqlProjectable (Projection Flat) where
  unsafeProjectSqlTerms = unsafeSqlTermsProjection

-- | Unsafely make 'Projection' from SQL terms.
instance SqlProjectable (Projection Aggregated) where
  unsafeProjectSqlTerms = unsafeSqlTermsProjection

-- | Unsafely make 'Projection' from SQL terms.
instance SqlProjectable (Projection OverWindow) where
  unsafeProjectSqlTerms = unsafeSqlTermsProjection

-- | Unsafely make 'Expr' from SQL terms.
instance SqlProjectable (Expr p) where
  unsafeProjectSqlTerms = UnsafeExpr.Expr . rowStringSQL . map stringSQL

-- | Unsafely Project single SQL term.
unsafeProjectSql :: SqlProjectable p => String -> p t
unsafeProjectSql =  unsafeProjectSqlTerms . (:[])

-- | Polymorphic projection of SQL null value.
unsafeValueNull :: SqlProjectable p => p (Maybe a)
unsafeValueNull =  unsafeProjectSql "NULL"

-- | Generate polymorphic projection of SQL constant values from Haskell value.
value :: (ShowConstantTermsSQL t, SqlProjectable p) => t -> p t
value =  unsafeProjectSqlTerms . showConstantTermsSQL

-- | Polymorphic proejction of SQL true value.
valueTrue  :: (SqlProjectable p, ProjectableMaybe p) => p (Maybe Bool)
valueTrue  =  just $ value True

-- | Polymorphic proejction of SQL false value.
valueFalse :: (SqlProjectable p, ProjectableMaybe p) => p (Maybe Bool)
valueFalse =  just $ value False

-- | Polymorphic proejction of SQL set value from Haskell list.
values :: (ShowConstantTermsSQL t, SqlProjectable p) => [t] -> ListProjection p t
values =  Projection.list . map value


-- | Interface to get SQL term from projections.
class ProjectableShowSql p where
  -- | Unsafely generate SQL expression string from projection object.
  unsafeShowSql :: p a    -- ^ Source projection object
                -> String -- ^ Result SQL expression string.

-- | Unsafely get SQL term from 'Expr'.
unsafeShowSqlExpr :: Expr p t -> String
unsafeShowSqlExpr =  UnsafeExpr.showExpr

-- | Unsafely get SQL term from 'Expr'.
instance ProjectableShowSql (Expr p) where
  unsafeShowSql = unsafeShowSqlExpr

-- | Unsafely get SQL term from 'Proejction'.
instance ProjectableShowSql (Projection c) where
  unsafeShowSql = unsafeShowSqlProjection


-- | Binary operator type for SQL String.
type SqlBinOp = Keyword -> Keyword -> Keyword

-- | Unsafely make projection unary operator from SQL keyword.
unsafeUniOp :: (ProjectableShowSql p0, SqlProjectable p1)
             => (Keyword -> Keyword) -> p0 a -> p1 b
unsafeUniOp u = unsafeProjectSql . SQL.strUniOp u . unsafeShowSql

unsafeFlatUniOp :: (SqlProjectable p, ProjectableShowSql p)
               => Keyword -> p a -> p b
unsafeFlatUniOp kw = unsafeUniOp (SQL.paren . SQL.defineUniOp kw)

parenBinStr :: SqlBinOp -> String -> String -> String
parenBinStr op = SQL.strBinOp $ \x y -> SQL.paren $ op x y

-- | Unsafely make projection binary operator from string binary operator.
unsafeBinOp :: (SqlProjectable p, ProjectableShowSql p)
            => SqlBinOp
            -> p a -> p b -> p c
unsafeBinOp op a b = unsafeProjectSql
                     $ parenBinStr op (unsafeShowSql a) (unsafeShowSql b)

-- | Unsafely make compare projection binary operator from string binary operator.
compareBinOp :: (SqlProjectable p, ProjectableShowSql p)
             => SqlBinOp
             -> p a -> p a -> p (Maybe Bool)
compareBinOp =  unsafeBinOp

-- | Unsafely make number projection binary operator from string binary operator.
monoBinOp :: (SqlProjectable p, ProjectableShowSql p)
         => SqlBinOp
         -> p a -> p a -> p a
monoBinOp =  unsafeBinOp


-- | Compare operator corresponding SQL /=/ .
(.=.)  :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.=.)  =  compareBinOp (SQL..=.)

-- | Compare operator corresponding SQL /</ .
(.<.)  :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.<.)  =  compareBinOp (SQL..<.)

-- | Compare operator corresponding SQL /<=/ .
(.<=.)  :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.<=.)  =  compareBinOp (SQL..<=.)

-- | Compare operator corresponding SQL />/ .
(.>.)  :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.>.)  =  compareBinOp (SQL..>.)

-- | Compare operator corresponding SQL />=/ .
(.>=.)  :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.>=.)  =  compareBinOp (SQL..>=.)

-- | Compare operator corresponding SQL /<>/ .
(.<>.) :: (SqlProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.<>.) =  compareBinOp (SQL..<>.)

-- | Logical operator corresponding SQL /AND/ .
and' :: (SqlProjectable p, ProjectableShowSql p)
     => p ft -> p ft -> p (Maybe Bool)
and' =  compareBinOp SQL.and

-- | Logical operator corresponding SQL /OR/ .
or' :: (SqlProjectable p, ProjectableShowSql p)
    => p ft -> p ft -> p (Maybe Bool)
or'  =  compareBinOp SQL.or

-- | Logical operator corresponding SQL /NOT/ .
not' :: (SqlProjectable p, ProjectableShowSql p)
    => p (Maybe Bool) -> p (Maybe Bool)
not' =  unsafeFlatUniOp SQL.NOT

-- | Logical operator corresponding SQL /EXISTS/ .
exists :: (SqlProjectable p, ProjectableShowSql p)
       => ListProjection (Projection Exists) r -> p (Maybe Bool)
exists =  unsafeProjectSql . SQL.strUniOp (SQL.paren . SQL.defineUniOp SQL.EXISTS)
          . unsafeShowSqlListProjection unsafeShowSql

-- | Concatinate operator corresponding SQL /||/ .
(.||.) :: (SqlProjectable p, ProjectableShowSql p, IsString a)
       => p a -> p a -> p a
(.||.) =  unsafeBinOp (SQL..||.)

-- | Concatinate operator corresponding SQL /||/ . Maybe type version.
(?||?) :: (SqlProjectable p, ProjectableShowSql p, IsString a)
       => p (Maybe a) -> p (Maybe a) -> p (Maybe a)
(?||?) =  unsafeBinOp (SQL..||.)

-- | Unsafely make number projection binary operator from SQL operator string.
monoBinOp' :: (SqlProjectable p, ProjectableShowSql p)
          => Keyword -> p a -> p a -> p a
monoBinOp' = monoBinOp . SQL.defineBinOp

-- | Number operator corresponding SQL /+/ .
(.+.) :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a
(.+.) =  monoBinOp' "+"

-- | Number operator corresponding SQL /-/ .
(.-.) :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a
(.-.) =  monoBinOp' "-"

-- | Number operator corresponding SQL /// .
(./.) :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a
(./.) =  monoBinOp' "/"

-- | Number operator corresponding SQL /*/ .
(.*.) :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a
(.*.) =  monoBinOp' "*"

-- | Number negate uni-operator corresponding SQL /-/.
negate' :: (SqlProjectable p, ProjectableShowSql p, Num a)
        => p a -> p a
negate' =  unsafeFlatUniOp $ SQL.word "-"

unsafeCastProjectable :: (SqlProjectable p, ProjectableShowSql p)
                           => p a -> p b
unsafeCastProjectable = unsafeProjectSql . unsafeShowSql

-- | Number fromIntegral uni-operator.
fromIntegral' :: (SqlProjectable p, ProjectableShowSql p, Integral a, Num b)
              => p a -> p b
fromIntegral' =  unsafeCastProjectable

-- | Unsafely show number into string-like type in projections.
showNum :: (SqlProjectable p, ProjectableShowSql p, Num a, IsString b)
              => p a -> p b
showNum =  unsafeCastProjectable

-- | Number operator corresponding SQL /+/ .
(?+?) :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p (Maybe a) -> p (Maybe a) -> p (Maybe a)
(?+?) =  monoBinOp' "+"

-- | Number operator corresponding SQL /-/ .
(?-?) :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p (Maybe a) -> p (Maybe a) -> p (Maybe a)
(?-?) =  monoBinOp' "-"

-- | Number operator corresponding SQL /// .
(?/?) :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p (Maybe a) -> p (Maybe a) -> p (Maybe a)
(?/?) =  monoBinOp' "/"

-- | Number operator corresponding SQL /*/ .
(?*?) :: (SqlProjectable p, ProjectableShowSql p, Num a)
  => p (Maybe a) -> p (Maybe a) -> p (Maybe a)
(?*?) =  monoBinOp' "*"

-- | Number negate uni-operator corresponding SQL /-/.
negateMaybe :: (SqlProjectable p, ProjectableShowSql p, Num a)
            => p (Maybe a) -> p (Maybe a)
negateMaybe =  unsafeFlatUniOp $ SQL.word "-"

-- | Number fromIntegral uni-operator.
fromIntegralMaybe :: (SqlProjectable p, ProjectableShowSql p, Integral a, Num b)
                  => p (Maybe a) -> p (Maybe b)
fromIntegralMaybe =  unsafeCastProjectable

-- | Unsafely show number into string-like type in projections.
showNumMaybe :: (SqlProjectable p, ProjectableShowSql p, Num a, IsString b)
                   => p (Maybe a) -> p (Maybe b)
showNumMaybe = unsafeCastProjectable

unsafeSqlWord :: ProjectableShowSql p => p a -> Keyword
unsafeSqlWord =  SQL.word . unsafeShowSql

whensClause :: (SqlProjectable p, ProjectableShowSql p)
            => String       -- ^ Error tag
            -> [(p a, p b)] -- ^ Each when clauses
            -> p b          -- ^ Else result projection
            -> Keyword      -- ^ Result projection
whensClause eTag cs0 e = d cs0  where
  d []       = error $ eTag ++ ": Empty when clauses!"
  d cs@(_:_) = mconcat [when' p r | (p, r) <- cs] <> else' <> SQL.END
  when' p r = SQL.WHEN <> unsafeSqlWord p <> SQL.THEN <> unsafeSqlWord r
  else'     = SQL.ELSE <> unsafeSqlWord e

-- | Search case operator correnponding SQL search /CASE/.
--   Like, /CASE WHEN p0 THEN a WHEN p1 THEN b ... ELSE c END/
caseSearch :: (SqlProjectable p, ProjectableShowSql p)
           => [(p (Maybe Bool), p a)] -- ^ Each when clauses
           -> p a                     -- ^ Else result projection
           -> p a                     -- ^ Result projection
caseSearch cs e = unsafeProjectSql . SQL.wordShow $ SQL.CASE <> whensClause "caseSearch" cs e

-- | Same as 'caseSearch', but you can write like <when list> `casesOrElse` <else clause>.
casesOrElse :: (SqlProjectable p, ProjectableShowSql p)
            => [(p (Maybe Bool), p a)] -- ^ Each when clauses
            -> p a                     -- ^ Else result projection
            -> p a                     -- ^ Result projection
casesOrElse = caseSearch

-- | Null default version of 'caseSearch'.
caseSearchMaybe :: (ProjectableShowSql p, SqlProjectable p)
                => [(p (Maybe Bool), p (Maybe a))] -- ^ Each when clauses
                -> p (Maybe a)                     -- ^ Result projection
caseSearchMaybe cs = caseSearch cs unsafeValueNull

-- | Simple case operator correnponding SQL simple /CASE/.
--   Like, /CASE x WHEN v THEN a WHEN w THEN b ... ELSE c END/
case' :: (SqlProjectable p, ProjectableShowSql p)
      => p a          -- ^ Projection value to match
      -> [(p a, p b)] -- ^ Each when clauses
      -> p b          -- ^ Else result projection
      -> p b          -- ^ Result projection
case' v cs e = unsafeProjectSql . SQL.wordShow $ SQL.CASE <> unsafeSqlWord v <> whensClause "case'" cs e

-- | Uncurry version of 'case'', and you can write like ... `casesOrElse'` <else clause>.
casesOrElse' :: (SqlProjectable p, ProjectableShowSql p)
             => (p a, [(p a, p b)]) -- ^ Projection value to match and each when clauses list
             -> p b                 -- ^ Else result projection
             -> p b                 -- ^ Result projection
casesOrElse' =  uncurry case'

-- | Null default version of 'case''.
caseMaybe :: (SqlProjectable p, ProjectableShowSql p, ProjectableMaybe p)
          => p a                  -- ^ Projection value to match
          -> [(p a, p (Maybe b))] -- ^ Each when clauses
          -> p (Maybe b)          -- ^ Result projection
caseMaybe v cs = case' v cs unsafeValueNull

-- | Binary operator corresponding SQL /IN/ .
in' :: (SqlProjectable p, ProjectableShowSql p)
    => p t -> ListProjection p t -> p (Maybe Bool)
in' a lp = unsafeProjectSql
           $ parenBinStr SQL.in' (unsafeShowSql a) (unsafeShowSqlListProjection unsafeShowSql lp)

-- | Operator corresponding SQL /IS NULL/ , and extended against record types.
isNothing :: (SqlProjectable (Projection c), ProjectableShowSql (Projection c), HasColumnConstraint NotNull r)
          => Projection c (Maybe r) -> Projection c (Maybe Bool)
isNothing mr = unsafeProjectSql $
               parenBinStr (SQL.defineBinOp SQL.IS)
               (Projection.unsafeShowSqlNotNullMaybeProjection mr) (SQL.wordShow SQL.NULL)

{-# DEPRECATED isNull "Use isNothing instead of this." #-}
-- | Same as 'isNothing'.
isNull :: (SqlProjectable (Projection c), ProjectableShowSql (Projection c), HasColumnConstraint NotNull r)
       => Projection c (Maybe r) -> Projection c (Maybe Bool)
isNull =  isNothing

-- | Operator corresponding SQL /NOT (... IS NULL)/ , and extended against record type.
isJust :: (SqlProjectable (Projection c), ProjectableShowSql (Projection c), HasColumnConstraint NotNull r)
          => Projection c (Maybe r) -> Projection c (Maybe Bool)
isJust =  not' . isNothing

{-# DEPRECATED isNotNull "Use isJust instead of this." #-}
-- | Same as 'isJust'.
isNotNull :: (SqlProjectable (Projection c), ProjectableShowSql (Projection c), HasColumnConstraint NotNull r)
          => Projection c (Maybe r) -> Projection c (Maybe Bool)
isNotNull =  isJust

-- | Operator from maybe type using record extended 'isNull'.
fromMaybe' :: (SqlProjectable (Projection c), ProjectableShowSql (Projection c), HasColumnConstraint NotNull r)
           => Projection c r -> Projection c (Maybe r) -> Projection c r
fromMaybe' d p = [ (isJust p, unsafeCastProjectable p) ] `casesOrElse` d

unsafeUniTermFunction :: SqlProjectable p => Keyword -> p t
unsafeUniTermFunction =  unsafeProjectSql . (++ "()") . SQL.wordShow

-- | /RANK()/ term.
rank :: Projection OverWindow Int64
rank =  unsafeUniTermFunction SQL.RANK

-- | /DENSE_RANK()/ term.
dense_rank :: Projection OverWindow Int64
dense_rank =  unsafeUniTermFunction SQL.DENSE_RANK

-- | /ROW_NUMBER()/ term.
row_number :: Projection OverWindow Int64
row_number =  unsafeUniTermFunction SQL.ROW_NUMBER

-- | /PERCENT_RANK()/ term.
percent_rank :: Projection OverWindow Double
percent_rank =  unsafeUniTermFunction SQL.PERCENT_RANK

-- | /CUME_DIST()/ term.
cume_dist :: Projection OverWindow Double
cume_dist =  unsafeUniTermFunction SQL.CUME_DIST

-- | Placeholder parameter type which has real parameter type arguemnt 'p'.
data PlaceHolders p = PlaceHolders

-- | Unsafely add placeholder parameter to queries.
addPlaceHolders :: Functor f => f a -> f (PlaceHolders p, a)
addPlaceHolders =  fmap ((,) PlaceHolders)

-- | Unsafely get placeholder parameter
unsafePlaceHolders :: PlaceHolders p
unsafePlaceHolders =  PlaceHolders

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


-- | Zipping projections.
projectZip :: ProjectableApplicative p => p a -> p b -> p (a, b)
projectZip pa pb = (,) |$| pa |*| pb

-- | Binary operator the same as 'projectZip'.
(><) :: ProjectableApplicative p => p a -> p b -> p (a, b)
(><) =  projectZip

-- | Interface to control 'Maybe' of phantom type in projections.
class ProjectableMaybe p where
  -- | Cast projection phantom type into 'Maybe'.
  just :: p a -> p (Maybe a)
  -- | Compose nested 'Maybe' phantom type on projection.
  flattenMaybe :: p (Maybe (Maybe a)) -> p (Maybe a)

-- | Control phantom 'Maybe' type in placeholder parameters.
instance ProjectableMaybe PlaceHolders where
  just         = unsafeCastPlaceHolders
  flattenMaybe = unsafeCastPlaceHolders

-- | Control phantom 'Maybe' type in projection type 'Projection'.
instance ProjectableMaybe (Projection c) where
  just         = Projection.just
  flattenMaybe = Projection.flattenMaybe

-- | Control phantom 'Maybe' type in SQL expression type 'Expr'.
instance ProjectableMaybe (Expr p) where
  just         = Expr.just
  flattenMaybe = Expr.fromJust

-- | Zipping except for identity element laws.
class ProjectableApplicative p => ProjectableIdZip p where
  leftId  :: p ((), a) -> p a
  rightId :: p (a, ()) -> p a

-- | Zipping except for identity element laws against placeholder parameter type.
instance ProjectableIdZip PlaceHolders where
  leftId  = unsafeCastPlaceHolders
  rightId = unsafeCastPlaceHolders

-- | Weaken functor on projections.
class ProjectableFunctor p where
  -- | Method like 'fmap'.
  (|$|) :: ProductConstructor (a -> b) => (a -> b) -> p a -> p b

-- | Same as '|$|' other than using infered record constructor.
ipfmap :: (ProjectableFunctor p, ProductConstructor (a -> b))
       => p a -> p b
ipfmap =  (|$|) productConstructor

-- | Weaken applicative functor on projections.
class ProjectableFunctor p => ProjectableApplicative p where
  -- | Method like '<*>'.
  (|*|) :: p (a -> b) -> p a -> p b

-- | Compose seed of record type 'PlaceHolders'.
instance ProjectableFunctor PlaceHolders where
  _ |$| PlaceHolders = PlaceHolders

-- | Compose record type 'PlaceHolders' using applicative style.
instance ProjectableApplicative PlaceHolders where
  pf |*| pa = unsafeCastPlaceHolders (pf >< pa)

-- | Compose seed of record type 'Projection'.
instance ProjectableFunctor (Projection c) where
  (|$|) = Projection.pfmap

-- | Compose record type 'Projection' using applicative style.
instance ProjectableApplicative (Projection c) where
  (|*|) = Projection.pap

-- | Compose seed of projection path 'Pi' which has record result type.
instance ProjectableFunctor (Pi a) where
  (|$|) = Pi.pfmap

-- | Compose projection path 'Pi' which has record result type using applicative style.
instance ProjectableApplicative (Pi a) where
  (|*|) = Pi.pap

infixl 7 .*., ./., ?*?, ?/?
infixl 6 .+., .-., ?+?, ?-?
infixl 5 .||., ?||?
infixl 4 |$|, |*|
infix  4 .=., .<>., .>., .>=., .<., .<=., `in'`
infixr 3 `and'`
infixr 2 `or'`
infixl 1  ><
