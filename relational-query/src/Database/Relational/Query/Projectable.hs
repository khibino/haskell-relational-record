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
  SqlProjectable (unsafeProjectSqlTerms'), unsafeProjectSql',
  unsafeProjectSqlTerms, unsafeProjectSql,

  -- * Projections of values
  value,
  valueTrue, valueFalse,
  values,
  unsafeValueNull,

  -- * Placeholders
  PlaceHolders, unsafeAddPlaceHolders, unsafePlaceHolders,
  placeholder', placeholder, unitPlaceHolder,

  -- * Projectable into SQL strings
  ProjectableShowSql (unsafeShowSql'), unsafeShowSql,

  -- * Operators
  (.=.), (.<.), (.<=.), (.>.), (.>=.), (.<>.),

  in', and', or',

  isNothing, isJust, fromMaybe,
  not', exists,

  (.||.), (?||?), like, likeMaybe, like', likeMaybe',
  (.+.), (.-.), (./.), (.*.), negate', fromIntegral', showNum,
  (?+?), (?-?), (?/?), (?*?), negateMaybe, fromIntegralMaybe, showNumMaybe,

  casesOrElse, casesOrElse',
  caseSearch, caseSearchMaybe, case', caseMaybe,

  SqlBinOp, unsafeBinOp, unsafeUniOp,

  -- * Terms for Window function types
  rank, denseRank, rowNumber, percentRank, cumeDist,

  -- * Zipping projections
  projectZip, (><),
  ProjectableIdZip (..),

  -- * 'Maybe' type projecitoins
  ProjectableMaybe (just, flattenMaybe),

  -- * ProjectableFunctor and ProjectableApplicative
  ProjectableFunctor (..), ProjectableApplicative (..), ipfmap
  ) where

import Prelude hiding (pi)

import Data.String (IsString)
import Data.Monoid ((<>), mconcat)
import Control.Applicative ((<$>))

import Language.SQL.Keyword (Keyword)
import qualified Language.SQL.Keyword as SQL

import Database.Record
  (PersistableWidth, PersistableRecordWidth, derivedWidth,
   HasColumnConstraint, NotNull)

import Database.Relational.Query.Internal.SQL (StringSQL, stringSQL, showStringSQL, rowStringSQL)
import Database.Relational.Query.Context (Flat, Aggregated, Exists, OverWindow)
import Database.Relational.Query.Expr (Expr)
import qualified Database.Relational.Query.Expr as Expr
import qualified Database.Relational.Query.Expr.Unsafe as UnsafeExpr

import Database.Relational.Query.Pure
  (ShowConstantTermsSQL, showConstantTermsSQL', ProductConstructor (..))
import Database.Relational.Query.Pi (Pi)
import qualified Database.Relational.Query.Pi as Pi

import Database.Relational.Query.Projection
  (Projection, ListProjection)
import qualified Database.Relational.Query.Projection as Projection


-- | Project from Projection type into expression type.
expr :: Projection p a -> Expr p a
expr =  UnsafeExpr.Expr . Projection.unsafeStringSql


-- | Interface to project SQL terms unsafely.
class SqlProjectable p where
  -- | Unsafely project from SQL expression terms.
  unsafeProjectSqlTerms' :: [StringSQL] -- ^ SQL expression strings
                         -> p t         -- ^ Result projection object

-- | Unsafely project from SQL strings. String interface of 'unsafeProjectSqlTerms''.
unsafeProjectSqlTerms :: SqlProjectable p
                      => [String] -- ^ SQL expression strings
                      -> p t      -- ^ Result projection object
unsafeProjectSqlTerms =  unsafeProjectSqlTerms' . map stringSQL

-- | Unsafely make 'Projection' from SQL terms.
instance SqlProjectable (Projection Flat) where
  unsafeProjectSqlTerms' = Projection.unsafeFromSqlTerms

-- | Unsafely make 'Projection' from SQL terms.
instance SqlProjectable (Projection Aggregated) where
  unsafeProjectSqlTerms' = Projection.unsafeFromSqlTerms

-- | Unsafely make 'Projection' from SQL terms.
instance SqlProjectable (Projection OverWindow) where
  unsafeProjectSqlTerms' = Projection.unsafeFromSqlTerms

-- | Unsafely make 'Expr' from SQL terms.
instance SqlProjectable (Expr p) where
  unsafeProjectSqlTerms' = UnsafeExpr.Expr . rowStringSQL

class SqlProjectable p => OperatorProjectable p
instance OperatorProjectable (Projection Flat)
instance OperatorProjectable (Projection Aggregated)
instance OperatorProjectable (Expr Flat)
instance OperatorProjectable (Expr Aggregated)

-- | Unsafely Project single SQL term.
unsafeProjectSql' :: SqlProjectable p => StringSQL -> p t
unsafeProjectSql' =  unsafeProjectSqlTerms' . (:[])

-- | Unsafely Project single SQL string. String interface of 'unsafeProjectSql''.
unsafeProjectSql :: SqlProjectable p => String -> p t
unsafeProjectSql =  unsafeProjectSql' . stringSQL

-- | Polymorphic projection of SQL null value.
unsafeValueNull :: OperatorProjectable p => p (Maybe a)
unsafeValueNull =  unsafeProjectSql "NULL"

-- | Generate polymorphic projection of SQL constant values from Haskell value.
value :: (ShowConstantTermsSQL t, OperatorProjectable p) => t -> p t
value =  unsafeProjectSqlTerms' . showConstantTermsSQL'

-- | Polymorphic proejction of SQL true value.
valueTrue  :: (OperatorProjectable p, ProjectableMaybe p) => p (Maybe Bool)
valueTrue  =  just $ value True

-- | Polymorphic proejction of SQL false value.
valueFalse :: (OperatorProjectable p, ProjectableMaybe p) => p (Maybe Bool)
valueFalse =  just $ value False

-- | Polymorphic proejction of SQL set value from Haskell list.
values :: (ShowConstantTermsSQL t, OperatorProjectable p) => [t] -> ListProjection p t
values =  Projection.list . map value


-- | Interface to get SQL term from projections.
class ProjectableShowSql p where
  -- | Unsafely generate SQL expression term from projection object.
  unsafeShowSql' :: p a       -- ^ Source projection object
                 -> StringSQL -- ^ Result SQL expression string.

-- | Unsafely generate SQL expression string from projection object.
--   String interface of 'unsafeShowSql''.
unsafeShowSql :: ProjectableShowSql p
              => p a    -- ^ Source projection object
              -> String -- ^ Result SQL expression string.
unsafeShowSql =  showStringSQL . unsafeShowSql'

-- | Unsafely get SQL term from 'Expr'.
instance ProjectableShowSql (Expr p) where
  unsafeShowSql' = UnsafeExpr.unsafeStringSql

-- | Unsafely get SQL term from 'Proejction'.
instance ProjectableShowSql (Projection c) where
  unsafeShowSql' = Projection.unsafeStringSql


-- | Binary operator type for SQL String.
type SqlBinOp = Keyword -> Keyword -> Keyword

-- | Unsafely make projection unary operator from SQL keyword.
unsafeUniOp :: (ProjectableShowSql p0, SqlProjectable p1)
             => (Keyword -> Keyword) -> p0 a -> p1 b
unsafeUniOp u = unsafeProjectSql' . u . unsafeShowSql'

unsafeFlatUniOp :: (SqlProjectable p, ProjectableShowSql p)
               => Keyword -> p a -> p b
unsafeFlatUniOp kw = unsafeUniOp (SQL.paren . SQL.defineUniOp kw)

-- | Unsafely make projection binary operator from string binary operator.
unsafeBinOp :: (SqlProjectable p, ProjectableShowSql p)
            => SqlBinOp
            -> p a -> p b -> p c
unsafeBinOp op a b = unsafeProjectSql' . SQL.paren $
                     op (unsafeShowSql' a) (unsafeShowSql' b)

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
(.=.)  :: (OperatorProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.=.)  =  compareBinOp (SQL..=.)

-- | Compare operator corresponding SQL /</ .
(.<.)  :: (OperatorProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.<.)  =  compareBinOp (SQL..<.)

-- | Compare operator corresponding SQL /<=/ .
(.<=.)  :: (OperatorProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.<=.)  =  compareBinOp (SQL..<=.)

-- | Compare operator corresponding SQL />/ .
(.>.)  :: (OperatorProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.>.)  =  compareBinOp (SQL..>.)

-- | Compare operator corresponding SQL />=/ .
(.>=.)  :: (OperatorProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.>=.)  =  compareBinOp (SQL..>=.)

-- | Compare operator corresponding SQL /<>/ .
(.<>.) :: (OperatorProjectable p, ProjectableShowSql p)
  => p ft -> p ft -> p (Maybe Bool)
(.<>.) =  compareBinOp (SQL..<>.)

-- | Logical operator corresponding SQL /AND/ .
and' :: (OperatorProjectable p, ProjectableShowSql p)
     => p ft -> p ft -> p (Maybe Bool)
and' =  compareBinOp SQL.and

-- | Logical operator corresponding SQL /OR/ .
or' :: (OperatorProjectable p, ProjectableShowSql p)
    => p ft -> p ft -> p (Maybe Bool)
or'  =  compareBinOp SQL.or

-- | Logical operator corresponding SQL /NOT/ .
not' :: (OperatorProjectable p, ProjectableShowSql p)
    => p (Maybe Bool) -> p (Maybe Bool)
not' =  unsafeFlatUniOp SQL.NOT

-- | Logical operator corresponding SQL /EXISTS/ .
exists :: (OperatorProjectable p, ProjectableShowSql p)
       => ListProjection (Projection Exists) r -> p (Maybe Bool)
exists =  unsafeProjectSql' . SQL.paren . SQL.defineUniOp SQL.EXISTS
          . Projection.unsafeStringSqlList unsafeShowSql'

-- | Concatinate operator corresponding SQL /||/ .
(.||.) :: (OperatorProjectable p, ProjectableShowSql p, IsString a)
       => p a -> p a -> p a
(.||.) =  unsafeBinOp (SQL..||.)

-- | Concatinate operator corresponding SQL /||/ . Maybe type version.
(?||?) :: (OperatorProjectable p, ProjectableShowSql p, IsString a)
       => p (Maybe a) -> p (Maybe a) -> p (Maybe a)
(?||?) =  unsafeBinOp (SQL..||.)

unsafeLike :: (OperatorProjectable p, ProjectableShowSql p)
           => p a -> p b -> p (Maybe Bool)
unsafeLike = unsafeBinOp (SQL.defineBinOp SQL.LIKE)

-- | String-compare operator corresponding SQL /LIKE/ .
like' :: (OperatorProjectable p, ProjectableShowSql p, IsString a)
      => p a -> p a -> p (Maybe Bool)
x `like'` y = x `unsafeLike` y

-- | String-compare operator corresponding SQL /LIKE/ .
likeMaybe' :: (OperatorProjectable p, ProjectableShowSql p, IsString a)
           => p (Maybe a) -> p (Maybe a) -> p (Maybe Bool)
x `likeMaybe'` y = x `unsafeLike` y

-- | String-compare operator corresponding SQL /LIKE/ .
like :: (OperatorProjectable p, ProjectableShowSql p, IsString a, ShowConstantTermsSQL a)
       => p a -> a -> p (Maybe Bool)
x `like` a = x `like'` value a

-- | String-compare operator corresponding SQL /LIKE/ . Maybe type version.
likeMaybe :: (OperatorProjectable p, ProjectableShowSql p, IsString a, ShowConstantTermsSQL a)
       => p (Maybe a) -> a -> p (Maybe Bool)
x `likeMaybe` a = x `unsafeLike` value a

-- | Unsafely make number projection binary operator from SQL operator string.
monoBinOp' :: (SqlProjectable p, ProjectableShowSql p)
          => Keyword -> p a -> p a -> p a
monoBinOp' = monoBinOp . SQL.defineBinOp

-- | Number operator corresponding SQL /+/ .
(.+.) :: (OperatorProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a
(.+.) =  monoBinOp' "+"

-- | Number operator corresponding SQL /-/ .
(.-.) :: (OperatorProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a
(.-.) =  monoBinOp' "-"

-- | Number operator corresponding SQL /// .
(./.) :: (OperatorProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a
(./.) =  monoBinOp' "/"

-- | Number operator corresponding SQL /*/ .
(.*.) :: (OperatorProjectable p, ProjectableShowSql p, Num a)
  => p a -> p a -> p a
(.*.) =  monoBinOp' "*"

-- | Number negate uni-operator corresponding SQL /-/.
negate' :: (OperatorProjectable p, ProjectableShowSql p, Num a)
        => p a -> p a
negate' =  unsafeFlatUniOp $ SQL.word "-"

unsafeCastProjectable :: (SqlProjectable p, ProjectableShowSql p)
                           => p a -> p b
unsafeCastProjectable = unsafeProjectSql' . unsafeShowSql'

-- | Number fromIntegral uni-operator.
fromIntegral' :: (SqlProjectable p, ProjectableShowSql p, Integral a, Num b)
              => p a -> p b
fromIntegral' =  unsafeCastProjectable

-- | Unsafely show number into string-like type in projections.
showNum :: (SqlProjectable p, ProjectableShowSql p, Num a, IsString b)
              => p a -> p b
showNum =  unsafeCastProjectable

-- | Number operator corresponding SQL /+/ .
(?+?) :: (OperatorProjectable p, ProjectableShowSql p, Num a)
  => p (Maybe a) -> p (Maybe a) -> p (Maybe a)
(?+?) =  monoBinOp' "+"

-- | Number operator corresponding SQL /-/ .
(?-?) :: (OperatorProjectable p, ProjectableShowSql p, Num a)
  => p (Maybe a) -> p (Maybe a) -> p (Maybe a)
(?-?) =  monoBinOp' "-"

-- | Number operator corresponding SQL /// .
(?/?) :: (OperatorProjectable p, ProjectableShowSql p, Num a)
  => p (Maybe a) -> p (Maybe a) -> p (Maybe a)
(?/?) =  monoBinOp' "/"

-- | Number operator corresponding SQL /*/ .
(?*?) :: (OperatorProjectable p, ProjectableShowSql p, Num a)
  => p (Maybe a) -> p (Maybe a) -> p (Maybe a)
(?*?) =  monoBinOp' "*"

-- | Number negate uni-operator corresponding SQL /-/.
negateMaybe :: (OperatorProjectable p, ProjectableShowSql p, Num a)
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

whensClause :: (OperatorProjectable p, ProjectableShowSql p)
            => String       -- ^ Error tag
            -> [(p a, p b)] -- ^ Each when clauses
            -> p b          -- ^ Else result projection
            -> Keyword      -- ^ Result projection
whensClause eTag cs0 e = d cs0  where
  d []       = error $ eTag ++ ": Empty when clauses!"
  d cs@(_:_) = mconcat [when' p r | (p, r) <- cs] <> else' <> SQL.END
  when' p r = SQL.WHEN <> unsafeShowSql' p <> SQL.THEN <> unsafeShowSql' r
  else'     = SQL.ELSE <> unsafeShowSql' e

-- | Search case operator correnponding SQL search /CASE/.
--   Like, /CASE WHEN p0 THEN a WHEN p1 THEN b ... ELSE c END/
caseSearch :: (OperatorProjectable p, ProjectableShowSql p)
           => [(p (Maybe Bool), p a)] -- ^ Each when clauses
           -> p a                     -- ^ Else result projection
           -> p a                     -- ^ Result projection
caseSearch cs e = unsafeProjectSql' $ SQL.CASE <> whensClause "caseSearch" cs e

-- | Same as 'caseSearch', but you can write like <when list> `casesOrElse` <else clause>.
casesOrElse :: (OperatorProjectable p, ProjectableShowSql p)
            => [(p (Maybe Bool), p a)] -- ^ Each when clauses
            -> p a                     -- ^ Else result projection
            -> p a                     -- ^ Result projection
casesOrElse = caseSearch

-- | Null default version of 'caseSearch'.
caseSearchMaybe :: (OperatorProjectable p, ProjectableShowSql p)
                => [(p (Maybe Bool), p (Maybe a))] -- ^ Each when clauses
                -> p (Maybe a)                     -- ^ Result projection
caseSearchMaybe cs = caseSearch cs unsafeValueNull

-- | Simple case operator correnponding SQL simple /CASE/.
--   Like, /CASE x WHEN v THEN a WHEN w THEN b ... ELSE c END/
case' :: (OperatorProjectable p, ProjectableShowSql p)
      => p a          -- ^ Projection value to match
      -> [(p a, p b)] -- ^ Each when clauses
      -> p b          -- ^ Else result projection
      -> p b          -- ^ Result projection
case' v cs e = unsafeProjectSql' $ SQL.CASE <> unsafeShowSql' v <> whensClause "case'" cs e

-- | Uncurry version of 'case'', and you can write like ... `casesOrElse'` <else clause>.
casesOrElse' :: (OperatorProjectable p, ProjectableShowSql p)
             => (p a, [(p a, p b)]) -- ^ Projection value to match and each when clauses list
             -> p b                 -- ^ Else result projection
             -> p b                 -- ^ Result projection
casesOrElse' =  uncurry case'

-- | Null default version of 'case''.
caseMaybe :: (OperatorProjectable p, ProjectableShowSql p, ProjectableMaybe p)
          => p a                  -- ^ Projection value to match
          -> [(p a, p (Maybe b))] -- ^ Each when clauses
          -> p (Maybe b)          -- ^ Result projection
caseMaybe v cs = case' v cs unsafeValueNull

-- | Binary operator corresponding SQL /IN/ .
in' :: (OperatorProjectable p, ProjectableShowSql p)
    => p t -> ListProjection p t -> p (Maybe Bool)
in' a lp = unsafeProjectSql' . SQL.paren
           $ SQL.in' (unsafeShowSql' a) (Projection.unsafeStringSqlList unsafeShowSql' lp)

-- | Operator corresponding SQL /IS NULL/ , and extended against record types.
isNothing :: (OperatorProjectable (Projection c), ProjectableShowSql (Projection c), HasColumnConstraint NotNull r)
          => Projection c (Maybe r) -> Projection c (Maybe Bool)
isNothing mr = unsafeProjectSql' $
               SQL.paren $ (SQL.defineBinOp SQL.IS)
               (Projection.unsafeStringSqlNotNullMaybe mr) SQL.NULL

-- | Operator corresponding SQL /NOT (... IS NULL)/ , and extended against record type.
isJust :: (OperatorProjectable (Projection c), ProjectableShowSql (Projection c), HasColumnConstraint NotNull r)
          => Projection c (Maybe r) -> Projection c (Maybe Bool)
isJust =  not' . isNothing

-- | Operator from maybe type using record extended 'isNull'.
fromMaybe :: (OperatorProjectable (Projection c), ProjectableShowSql (Projection c), HasColumnConstraint NotNull r)
          => Projection c r -> Projection c (Maybe r) -> Projection c r
fromMaybe d p = [ (isNothing p, d) ] `casesOrElse` unsafeCastProjectable p

unsafeUniTermFunction :: SqlProjectable p => Keyword -> p t
unsafeUniTermFunction =  unsafeProjectSql' . (SQL.<++> stringSQL "()")

-- | /RANK()/ term.
rank :: Integral a => Projection OverWindow a
rank =  unsafeUniTermFunction SQL.RANK

-- | /DENSE_RANK()/ term.
denseRank :: Integral a => Projection OverWindow a
denseRank =  unsafeUniTermFunction SQL.DENSE_RANK

-- | /ROW_NUMBER()/ term.
rowNumber :: Integral a => Projection OverWindow a
rowNumber =  unsafeUniTermFunction SQL.ROW_NUMBER

-- | /PERCENT_RANK()/ term.
percentRank :: Projection OverWindow Double
percentRank =  unsafeUniTermFunction SQL.PERCENT_RANK

-- | /CUME_DIST()/ term.
cumeDist :: Projection OverWindow Double
cumeDist =  unsafeUniTermFunction SQL.CUME_DIST

-- | Placeholder parameter type which has real parameter type arguemnt 'p'.
data PlaceHolders p = PlaceHolders

-- | Unsafely add placeholder parameter to queries.
unsafeAddPlaceHolders :: Functor f => f a -> f (PlaceHolders p, a)
unsafeAddPlaceHolders =  fmap ((,) PlaceHolders)

-- | Unsafely get placeholder parameter
unsafePlaceHolders :: PlaceHolders p
unsafePlaceHolders =  PlaceHolders

-- | No placeholder semantics
unitPlaceHolder :: PlaceHolders ()
unitPlaceHolder =  unsafePlaceHolders

-- | Unsafely cast placeholder parameter type.
unsafeCastPlaceHolders :: PlaceHolders a -> PlaceHolders b
unsafeCastPlaceHolders PlaceHolders = PlaceHolders

unsafeProjectPlaceHolder' :: (PersistableWidth r, SqlProjectable p)
                               => (PersistableRecordWidth r, p r)
unsafeProjectPlaceHolder' =  unsafeProjectSqlTerms' . (`replicate` "?") <$> derivedWidth

unsafeProjectPlaceHolder :: (PersistableWidth r, SqlProjectable p)
                               => p r
unsafeProjectPlaceHolder =  snd unsafeProjectPlaceHolder'

-- | Provide scoped placeholder and return its parameter object.
placeholder' :: (PersistableWidth t, SqlProjectable p) => (p t -> a) ->  (PlaceHolders t, a)
placeholder' f = (PlaceHolders, f unsafeProjectPlaceHolder)

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

-- | Same as '|$|' other than using inferred record constructor.
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
infix  4 .=., .<>., .>., .>=., .<., .<=., `in'`, `like`, `likeMaybe`, `like'`, `likeMaybe'`
infixr 3 `and'`
infixr 2 `or'`
infixl 1  ><
