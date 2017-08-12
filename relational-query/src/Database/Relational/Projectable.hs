{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.Relational.Projectable
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines operators on various polymorphic projections.
module Database.Relational.Projectable (
  -- * Projectable from SQL strings
  SqlProjectable (unsafeProjectSqlTerms), unsafeProjectSql',
  unsafeProjectSql,

  -- * Projections of values
  value,
  valueTrue, valueFalse,
  values,
  nothing,

  -- * Placeholders
  PlaceHolders, unsafeAddPlaceHolders, unsafePlaceHolders,
  pwPlaceholder, placeholder', placeholder, unitPlaceHolder, unitPH,

  -- * Projectable into SQL strings
  ProjectableShowSql (unsafeShowSql'), unsafeShowSql,

  -- * Operators
  (.=.), (.<.), (.<=.), (.>.), (.>=.), (.<>.),

  and', or', in',

  (.||.), (?||?), like, likeMaybe, like', likeMaybe',
  (.+.), (.-.), (.*.), (./.),
  (?+?), (?-?), (?*?), (?/?),

  isNothing, isJust, fromMaybe,
  not', exists,

  negate', fromIntegral', showNum,
  negateMaybe, fromIntegralMaybe, showNumMaybe,

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
  ) where

import Prelude hiding (pi)

import Data.String (IsString)

import Language.SQL.Keyword (Keyword)
import qualified Language.SQL.Keyword as SQL

import Database.Record
  (PersistableWidth, persistableWidth, PersistableRecordWidth,
   HasColumnConstraint, NotNull)
import Database.Record.Persistable (runPersistableRecordWidth)

import Database.Relational.Internal.SQL (StringSQL, stringSQL, showStringSQL)
import Database.Relational.Internal.Sub (Record)
import qualified Database.Relational.Internal.Sub as Internal

import Database.Relational.ProjectableClass
  (ProjectableFunctor (..), ProjectableApplicative (..), )
import Database.Relational.Context (Flat, Aggregated, Exists, OverWindow)
import Database.Relational.TupleInstances ()
import Database.Relational.ProjectableClass
  (ShowConstantTermsSQL, showConstantTermsSQL, )
import Database.Relational.Projection (RecordList)
import qualified Database.Relational.Projection as Projection


-- | Interface to project SQL terms unsafely.
class SqlProjectable p where
  -- | Unsafely project from SQL expression terms.
  unsafeProjectSqlTerms :: [StringSQL] -- ^ SQL expression strings
                        -> p t         -- ^ Result projection object

-- | Unsafely make 'Projection' from SQL terms.
instance SqlProjectable (Record Flat) where
  unsafeProjectSqlTerms = Projection.unsafeFromSqlTerms

-- | Unsafely make 'Projection' from SQL terms.
instance SqlProjectable (Record Aggregated) where
  unsafeProjectSqlTerms = Projection.unsafeFromSqlTerms

-- | Unsafely make 'Projection' from SQL terms.
instance SqlProjectable (Record OverWindow) where
  unsafeProjectSqlTerms = Projection.unsafeFromSqlTerms

class SqlProjectable p => OperatorProjectable p
instance OperatorProjectable (Record Flat)
instance OperatorProjectable (Record Aggregated)

-- | Unsafely Project single SQL term.
unsafeProjectSql' :: SqlProjectable p => StringSQL -> p t
unsafeProjectSql' =  unsafeProjectSqlTerms . (:[])

-- | Unsafely Project single SQL string. String interface of 'unsafeProjectSql''.
unsafeProjectSql :: SqlProjectable p => String -> p t
unsafeProjectSql =  unsafeProjectSql' . stringSQL

-- | Polymorphic projection of SQL null value. Semantics of comparing is unsafe.
nothing :: (OperatorProjectable (Record c), SqlProjectable (Record c), PersistableWidth a)
        => Record c (Maybe a)
nothing = proxyWidth persistableWidth
  where
    proxyWidth :: SqlProjectable (Record c) => PersistableRecordWidth a -> Record c (Maybe a)
    proxyWidth w = unsafeProjectSqlTerms $ replicate (runPersistableRecordWidth w) SQL.NULL

-- | Generate polymorphic projection of SQL constant values from Haskell value.
value :: (ShowConstantTermsSQL t, OperatorProjectable p) => t -> p t
value = unsafeProjectSqlTerms . showConstantTermsSQL

-- | Polymorphic proejction of SQL true value.
valueTrue  :: (OperatorProjectable p, ProjectableMaybe p) => p (Maybe Bool)
valueTrue  =  just $ value True

-- | Polymorphic proejction of SQL false value.
valueFalse :: (OperatorProjectable p, ProjectableMaybe p) => p (Maybe Bool)
valueFalse =  just $ value False

-- | Polymorphic proejction of SQL set value from Haskell list.
values :: (ShowConstantTermsSQL t, OperatorProjectable p) => [t] -> RecordList p t
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

-- | Unsafely get SQL term from 'Proejction'.
instance ProjectableShowSql (Record c) where
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

-- | Unsafely make numrical projection binary operator from string binary operator.
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
     => p (Maybe Bool) -> p (Maybe Bool) -> p (Maybe Bool)
and' = monoBinOp SQL.and

-- | Logical operator corresponding SQL /OR/ .
or' :: (OperatorProjectable p, ProjectableShowSql p)
    => p (Maybe Bool) -> p (Maybe Bool) -> p (Maybe Bool)
or'  = monoBinOp SQL.or

-- | Logical operator corresponding SQL /NOT/ .
not' :: (OperatorProjectable p, ProjectableShowSql p)
    => p (Maybe Bool) -> p (Maybe Bool)
not' =  unsafeFlatUniOp SQL.NOT

-- | Logical operator corresponding SQL /EXISTS/ .
exists :: (OperatorProjectable p, ProjectableShowSql p)
       => RecordList (Record Exists) r -> p (Maybe Bool)
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

-- | Search case operator correnponding SQL search /CASE/.
--   Like, /CASE WHEN p0 THEN a WHEN p1 THEN b ... ELSE c END/
caseSearch :: OperatorProjectable (Record c)
           => [(Record c (Maybe Bool), Record c a)] -- ^ Each when clauses
           -> Record c a                            -- ^ Else result projection
           -> Record c a                            -- ^ Result projection
caseSearch = Internal.caseSearch

-- | Same as 'caseSearch', but you can write like <when list> `casesOrElse` <else clause>.
casesOrElse :: OperatorProjectable (Record c)
            => [(Record c (Maybe Bool), Record c a)] -- ^ Each when clauses
            -> Record c a                            -- ^ Else result projection
            -> Record c a                            -- ^ Result projection
casesOrElse = caseSearch

-- | Null default version of 'caseSearch'.
caseSearchMaybe :: (OperatorProjectable (Record c) {- (Record c) is always ProjectableMaybe -}, PersistableWidth a)
                => [(Record c (Maybe Bool), Record c (Maybe a))] -- ^ Each when clauses
                -> Record c (Maybe a)                            -- ^ Result projection
caseSearchMaybe cs = caseSearch cs nothing

-- | Simple case operator correnponding SQL simple /CASE/.
--   Like, /CASE x WHEN v THEN a WHEN w THEN b ... ELSE c END/
case' :: OperatorProjectable (Record c)
      => Record c a                 -- ^ Record value to match
      -> [(Record c a, Record c b)] -- ^ Each when clauses
      -> Record c b                 -- ^ Else result projection
      -> Record c b                 -- ^ Result projection
case' = Internal.case'

-- | Uncurry version of 'case'', and you can write like ... `casesOrElse'` <else clause>.
casesOrElse' :: OperatorProjectable (Record c)
             => (Record c a, [(Record c a, Record c b)]) -- ^ Record value to match and each when clauses list
             -> Record c b                               -- ^ Else result projection
             -> Record c b                               -- ^ Result projection
casesOrElse' =  uncurry case'

-- | Null default version of 'case''.
caseMaybe :: (OperatorProjectable (Record c) {- (Record c) is always ProjectableMaybe -}, PersistableWidth b)
          => Record c a                         -- ^ Record value to match
          -> [(Record c a, Record c (Maybe b))] -- ^ Each when clauses
          -> Record c (Maybe b)                 -- ^ Result projection
caseMaybe v cs = case' v cs nothing

-- | Binary operator corresponding SQL /IN/ .
in' :: (OperatorProjectable p, ProjectableShowSql p)
    => p t -> RecordList p t -> p (Maybe Bool)
in' a lp = unsafeProjectSql' . SQL.paren
           $ SQL.in' (unsafeShowSql' a) (Projection.unsafeStringSqlList unsafeShowSql' lp)

-- | Operator corresponding SQL /IS NULL/ , and extended against record types.
isNothing :: (OperatorProjectable (Record c), ProjectableShowSql (Record c), HasColumnConstraint NotNull r)
          => Record c (Maybe r) -> Record c (Maybe Bool)
isNothing mr = unsafeProjectSql' $
               SQL.paren $ (SQL.defineBinOp SQL.IS)
               (Projection.unsafeStringSqlNotNullMaybe mr) SQL.NULL

-- | Operator corresponding SQL /NOT (... IS NULL)/ , and extended against record type.
isJust :: (OperatorProjectable (Record c), ProjectableShowSql (Record c), HasColumnConstraint NotNull r)
          => Record c (Maybe r) -> Record c (Maybe Bool)
isJust =  not' . isNothing

-- | Operator from maybe type using record extended 'isNull'.
fromMaybe :: (OperatorProjectable (Record c), ProjectableShowSql (Record c), HasColumnConstraint NotNull r)
          => Record c r -> Record c (Maybe r) -> Record c r
fromMaybe d p = [ (isNothing p, d) ] `casesOrElse` unsafeCastProjectable p

unsafeUniTermFunction :: SqlProjectable p => Keyword -> p t
unsafeUniTermFunction =  unsafeProjectSql' . (SQL.<++> stringSQL "()")

-- | /RANK()/ term.
rank :: Integral a => Record OverWindow a
rank =  unsafeUniTermFunction SQL.RANK

-- | /DENSE_RANK()/ term.
denseRank :: Integral a => Record OverWindow a
denseRank =  unsafeUniTermFunction SQL.DENSE_RANK

-- | /ROW_NUMBER()/ term.
rowNumber :: Integral a => Record OverWindow a
rowNumber =  unsafeUniTermFunction SQL.ROW_NUMBER

-- | /PERCENT_RANK()/ term.
percentRank :: Record OverWindow Double
percentRank =  unsafeUniTermFunction SQL.PERCENT_RANK

-- | /CUME_DIST()/ term.
cumeDist :: Record OverWindow Double
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
unitPlaceHolder = unsafePlaceHolders

-- | No placeholder semantics. Same as `unitPlaceHolder`
unitPH :: PlaceHolders ()
unitPH = unitPlaceHolder

-- | Unsafely cast placeholder parameter type.
unsafeCastPlaceHolders :: PlaceHolders a -> PlaceHolders b
unsafeCastPlaceHolders PlaceHolders = PlaceHolders

-- | Provide scoped placeholder from width and return its parameter object.
pwPlaceholder :: SqlProjectable p
              => PersistableRecordWidth a
              -> (p a -> b)
              -> (PlaceHolders a, b)
pwPlaceholder pw f = (PlaceHolders, f $ projectPlaceHolder pw)
  where
    projectPlaceHolder :: SqlProjectable p
                       => PersistableRecordWidth a
                       -> p a
    projectPlaceHolder = unsafeProjectSqlTerms . (`replicate` "?") . runPersistableRecordWidth

-- | Provide scoped placeholder and return its parameter object.
placeholder' :: (PersistableWidth t, SqlProjectable p) => (p t -> a) ->  (PlaceHolders t, a)
placeholder' = pwPlaceholder persistableWidth

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
instance ProjectableMaybe (Record c) where
  just         = Projection.just
  flattenMaybe = Projection.flattenMaybe

-- | Zipping except for identity element laws.
class ProjectableApplicative p => ProjectableIdZip p where
  leftId  :: p ((), a) -> p a
  rightId :: p (a, ()) -> p a

-- | Zipping except for identity element laws against placeholder parameter type.
instance ProjectableIdZip PlaceHolders where
  leftId  = unsafeCastPlaceHolders
  rightId = unsafeCastPlaceHolders

-- | Compose seed of record type 'PlaceHolders'.
instance ProjectableFunctor PlaceHolders where
  _ |$| PlaceHolders = PlaceHolders

-- | Compose record type 'PlaceHolders' using applicative style.
instance ProjectableApplicative PlaceHolders where
  pf |*| pa = unsafeCastPlaceHolders (pf >< pa)

infixl 7 .*., ./., ?*?, ?/?
infixl 6 .+., .-., ?+?, ?-?
infixl 5 .||., ?||?
infix  4 .=., .<>., .>., .>=., .<., .<=., `in'`, `like`, `likeMaybe`, `like'`, `likeMaybe'`
infixr 3 `and'`
infixr 2 `or'`
infixl 1  ><
