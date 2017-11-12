{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- This module defines operators on various projected records.
module Database.Relational.Projectable (
  -- * Projectable from SQL strings
  SqlContext (unsafeProjectSqlTerms), unsafeProjectSql',
  unsafeProjectSql,

  -- * Records of values
  value,
  valueTrue, valueFalse,
  values,
  nothing,

  -- * Placeholders
  PlaceHolders, unsafeAddPlaceHolders, unsafePlaceHolders,
  pwPlaceholder, placeholder', placeholder, unitPlaceHolder, unitPH,

  -- * Projectable into SQL strings
  unsafeShowSql', unsafeShowSql,

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

  -- * 'Maybe' type projecitoins
  ProjectableMaybe (just, flattenMaybe),

  -- * Projection for nested 'Maybe's
  ProjectableFlattenMaybe (flatten),

  flattenPiMaybe,

  -- * Get narrower records
  (!), (?!), (?!?), (!??),

  -- * Aggregate functions
  unsafeAggregateOp,
  count,
  sum', sumMaybe, avg, avgMaybe,
  max', maxMaybe, min', minMaybe,
  every, any', some',
  ) where

import Prelude hiding (pi)

import Data.String (IsString)
import Data.Functor.ProductIsomorphic
  ((|$|), ProductIsoApplicative, pureP, (|*|), )

import Language.SQL.Keyword (Keyword)
import qualified Language.SQL.Keyword as SQL

import Database.Record
  (PersistableWidth, persistableWidth, PersistableRecordWidth,
   HasColumnConstraint, NotNull)
import Database.Record.Persistable (runPersistableRecordWidth)

import Database.Relational.Internal.ContextType (Flat, Exists, OverWindow)
import Database.Relational.Internal.String (StringSQL, stringSQL, showStringSQL)
import Database.Relational.SqlSyntax (Record, Predicate)
import qualified Database.Relational.SqlSyntax as Syntax

import Database.Relational.Pure ()
import Database.Relational.TupleInstances ()
import Database.Relational.Pi (Pi)
import Database.Relational.ProjectableClass
  (ShowConstantTermsSQL, showConstantTermsSQL, )
import Database.Relational.Record (RecordList)
import qualified Database.Relational.Record as Record
import Database.Relational.Projectable.Unsafe
  (SqlContext (..), OperatorContext, AggregatedContext, PlaceHolders (..))
import Database.Relational.Projectable.Instances ()


-- | Unsafely Project single SQL term.
unsafeProjectSql' :: SqlContext c => StringSQL -> Record c t
unsafeProjectSql' = unsafeProjectSqlTerms . (:[])

-- | Unsafely Project single SQL string. String interface of 'unsafeProjectSql'''.
unsafeProjectSql :: SqlContext c => String -> Record c t
unsafeProjectSql = unsafeProjectSql' . stringSQL

-- | Record with polymorphic phantom type of SQL null value. Semantics of comparing is unsafe.
nothing :: (OperatorContext c, SqlContext c, PersistableWidth a)
        => Record c (Maybe a)
nothing = proxyWidth persistableWidth
  where
    proxyWidth :: SqlContext c => PersistableRecordWidth a -> Record c (Maybe a)
    proxyWidth w = unsafeProjectSqlTerms $ replicate (runPersistableRecordWidth w) SQL.NULL

-- | Generate record with polymorphic type of SQL constant values from Haskell value.
value :: (ShowConstantTermsSQL t, OperatorContext c) => t -> Record c t
value = unsafeProjectSqlTerms . showConstantTermsSQL

-- | Record with polymorphic type of SQL true value.
valueTrue  :: (OperatorContext c, ProjectableMaybe (Record c)) => Record c (Maybe Bool)
valueTrue  =  just $ value True

-- | Record with polymorphic type of SQL false value.
valueFalse :: (OperatorContext c, ProjectableMaybe (Record c)) => Record c (Maybe Bool)
valueFalse =  just $ value False

-- | RecordList with polymorphic type of SQL set value from Haskell list.
values :: (ShowConstantTermsSQL t, OperatorContext c) => [t] -> RecordList (Record c) t
values =  Record.list . map value


-- | Unsafely generate SQL expression term from record object.
unsafeShowSql' :: Record c a -> StringSQL
unsafeShowSql' = Record.unsafeStringSql

-- | Unsafely generate SQL expression string from record object.
--   String interface of 'unsafeShowSql''.
unsafeShowSql :: Record c a    -- ^ Source record object
              -> String -- ^ Result SQL expression string.
unsafeShowSql =  showStringSQL . unsafeShowSql'


-- | Binary operator type for SQL String.
type SqlBinOp = Keyword -> Keyword -> Keyword

-- | Unsafely make unary operator for records from SQL keyword.
unsafeUniOp :: SqlContext c2
            => (Keyword -> Keyword) -> Record c1 a -> Record c2 b
unsafeUniOp u = unsafeProjectSql' . u . unsafeShowSql'

unsafeFlatUniOp :: SqlContext c
                => Keyword -> Record c a -> Record c b
unsafeFlatUniOp kw = unsafeUniOp (SQL.paren . SQL.defineUniOp kw)

-- | Unsafely make binary operator for records from string binary operator.
unsafeBinOp :: SqlContext k
            => SqlBinOp
            -> Record k a -> Record k b -> Record k c
unsafeBinOp op a b = unsafeProjectSql' . SQL.paren $
                     op (unsafeShowSql' a) (unsafeShowSql' b)

-- | Unsafely make binary operator to compare records from string binary operator.
compareBinOp :: SqlContext c
             => SqlBinOp
             -> Record c a -> Record c a -> Record c (Maybe Bool)
compareBinOp =  unsafeBinOp

-- | Unsafely make numrical binary operator for records from string binary operator.
monoBinOp :: SqlContext c
          => SqlBinOp
          -> Record c a -> Record c a -> Record c a
monoBinOp =  unsafeBinOp


-- | Compare operator corresponding SQL /=/ .
(.=.)  :: OperatorContext c
       => Record c ft -> Record c ft -> Record c (Maybe Bool)
(.=.)  =  compareBinOp (SQL..=.)

-- | Compare operator corresponding SQL /</ .
(.<.)  :: OperatorContext c
       => Record c ft -> Record c ft -> Record c (Maybe Bool)
(.<.)  =  compareBinOp (SQL..<.)

-- | Compare operator corresponding SQL /<=/ .
(.<=.)  :: OperatorContext c
        => Record c ft -> Record c ft -> Record c (Maybe Bool)
(.<=.)  =  compareBinOp (SQL..<=.)

-- | Compare operator corresponding SQL />/ .
(.>.)  :: OperatorContext c
       => Record c ft -> Record c ft -> Record c (Maybe Bool)
(.>.)  =  compareBinOp (SQL..>.)

-- | Compare operator corresponding SQL />=/ .
(.>=.)  :: OperatorContext c
        => Record c ft -> Record c ft -> Record c (Maybe Bool)
(.>=.)  =  compareBinOp (SQL..>=.)

-- | Compare operator corresponding SQL /<>/ .
(.<>.) :: OperatorContext c
       => Record c ft -> Record c ft -> Record c (Maybe Bool)
(.<>.) =  compareBinOp (SQL..<>.)

-- | Logical operator corresponding SQL /AND/ .
and' :: OperatorContext c
     => Record c (Maybe Bool) -> Record c (Maybe Bool) -> Record c (Maybe Bool)
and' = monoBinOp SQL.and

-- | Logical operator corresponding SQL /OR/ .
or' :: OperatorContext c
    => Record c (Maybe Bool) -> Record c (Maybe Bool) -> Record c (Maybe Bool)
or'  = monoBinOp SQL.or

-- | Logical operator corresponding SQL /NOT/ .
not' :: OperatorContext c
     => Record c (Maybe Bool) -> Record c (Maybe Bool)
not' =  unsafeFlatUniOp SQL.NOT

-- | Logical operator corresponding SQL /EXISTS/ .
exists :: OperatorContext c
       => RecordList (Record Exists) r -> Record c (Maybe Bool)
exists =  unsafeProjectSql' . SQL.paren . SQL.defineUniOp SQL.EXISTS
          . Record.unsafeStringSqlList unsafeShowSql'

-- | Concatinate operator corresponding SQL /||/ .
(.||.) :: OperatorContext c
       => Record c a -> Record c a -> Record c a
(.||.) =  unsafeBinOp (SQL..||.)

-- | Concatinate operator corresponding SQL /||/ . Maybe type version.
(?||?) :: (OperatorContext c, IsString a)
       => Record c (Maybe a) -> Record c (Maybe a) -> Record c (Maybe a)
(?||?) =  unsafeBinOp (SQL..||.)

unsafeLike :: OperatorContext c
           => Record c a -> Record c b -> Record c (Maybe Bool)
unsafeLike = unsafeBinOp (SQL.defineBinOp SQL.LIKE)

-- | String-compare operator corresponding SQL /LIKE/ .
like' :: (OperatorContext c, IsString a)
      => Record c a -> Record c a -> Record c (Maybe Bool)
x `like'` y = x `unsafeLike` y

-- | String-compare operator corresponding SQL /LIKE/ .
likeMaybe' :: (OperatorContext c, IsString a)
           => Record c (Maybe a) -> Record c (Maybe a) -> Record c (Maybe Bool)
x `likeMaybe'` y = x `unsafeLike` y

-- | String-compare operator corresponding SQL /LIKE/ .
like :: (OperatorContext c, IsString a, ShowConstantTermsSQL a)
       => Record c a -> a -> Record c (Maybe Bool)
x `like` a = x `like'` value a

-- | String-compare operator corresponding SQL /LIKE/ . Maybe type version.
likeMaybe :: (OperatorContext c, IsString a, ShowConstantTermsSQL a)
          => Record c (Maybe a) -> a -> Record c (Maybe Bool)
x `likeMaybe` a = x `unsafeLike` value a

-- | Unsafely make number binary operator for records from SQL operator string.
monoBinOp' :: SqlContext c
           => Keyword -> Record c a -> Record c a -> Record c a
monoBinOp' = monoBinOp . SQL.defineBinOp

-- | Number operator corresponding SQL /+/ .
(.+.) :: (OperatorContext c, Num a)
      => Record c a -> Record c a -> Record c a
(.+.) =  monoBinOp' "+"

-- | Number operator corresponding SQL /-/ .
(.-.) :: (OperatorContext c, Num a)
      => Record c a -> Record c a -> Record c a
(.-.) =  monoBinOp' "-"

-- | Number operator corresponding SQL /// .
(./.) :: (OperatorContext c, Num a)
      => Record c a -> Record c a -> Record c a
(./.) =  monoBinOp' "/"

-- | Number operator corresponding SQL /*/ .
(.*.) :: (OperatorContext c, Num a)
      => Record c a -> Record c a -> Record c a
(.*.) =  monoBinOp' "*"

-- | Number negate uni-operator corresponding SQL /-/.
negate' :: (OperatorContext c, Num a)
        => Record c a -> Record c a
negate' =  unsafeFlatUniOp $ SQL.word "-"

unsafeCastProjectable :: SqlContext c
                      => Record c a -> Record c b
unsafeCastProjectable = unsafeProjectSql' . unsafeShowSql'

-- | Number fromIntegral uni-operator.
fromIntegral' :: (SqlContext c, Integral a, Num b)
              => Record c a -> Record c b
fromIntegral' =  unsafeCastProjectable

-- | Unsafely show number into string-like type in records.
showNum :: (SqlContext c, Num a, IsString b)
        => Record c a -> Record c b
showNum =  unsafeCastProjectable

-- | Number operator corresponding SQL /+/ .
(?+?) :: (OperatorContext c, Num a)
      => Record c (Maybe a) -> Record c (Maybe a) -> Record c (Maybe a)
(?+?) =  monoBinOp' "+"

-- | Number operator corresponding SQL /-/ .
(?-?) :: (OperatorContext c, Num a)
      => Record c (Maybe a) -> Record c (Maybe a) -> Record c (Maybe a)
(?-?) =  monoBinOp' "-"

-- | Number operator corresponding SQL /// .
(?/?) :: (OperatorContext c, Num a)
      => Record c (Maybe a) -> Record c (Maybe a) -> Record c (Maybe a)
(?/?) =  monoBinOp' "/"

-- | Number operator corresponding SQL /*/ .
(?*?) :: (OperatorContext c, Num a)
      => Record c (Maybe a) -> Record c (Maybe a) -> Record c (Maybe a)
(?*?) =  monoBinOp' "*"

-- | Number negate uni-operator corresponding SQL /-/.
negateMaybe :: (OperatorContext c, Num a)
            => Record c (Maybe a) -> Record c (Maybe a)
negateMaybe =  unsafeFlatUniOp $ SQL.word "-"

-- | Number fromIntegral uni-operator.
fromIntegralMaybe :: (SqlContext c, Integral a, Num b)
                  => Record c (Maybe a) -> Record c (Maybe b)
fromIntegralMaybe =  unsafeCastProjectable

-- | Unsafely show number into string-like type in records.
showNumMaybe :: (SqlContext c, Num a, IsString b)
             => Record c (Maybe a) -> Record c (Maybe b)
showNumMaybe = unsafeCastProjectable

-- | Search case operator correnponding SQL search /CASE/.
--   Like, /CASE WHEN p0 THEN a WHEN p1 THEN b ... ELSE c END/
caseSearch :: OperatorContext c
           => [(Predicate c, Record c a)] -- ^ Each when clauses
           -> Record c a                            -- ^ Else result record
           -> Record c a                            -- ^ Result record
caseSearch = Syntax.caseSearch

-- | Same as 'caseSearch', but you can write like <when list> `casesOrElse` <else clause>.
casesOrElse :: OperatorContext c
            => [(Predicate c, Record c a)] -- ^ Each when clauses
            -> Record c a                            -- ^ Else result record
            -> Record c a                            -- ^ Result record
casesOrElse = caseSearch

-- | Null default version of 'caseSearch'.
caseSearchMaybe :: (OperatorContext c {- (Record c) is always ProjectableMaybe -}, PersistableWidth a)
                => [(Predicate c, Record c (Maybe a))] -- ^ Each when clauses
                -> Record c (Maybe a)                            -- ^ Result record
caseSearchMaybe cs = caseSearch cs nothing

-- | Simple case operator correnponding SQL simple /CASE/.
--   Like, /CASE x WHEN v THEN a WHEN w THEN b ... ELSE c END/
case' :: OperatorContext c
      => Record c a                 -- ^ Record value to match
      -> [(Record c a, Record c b)] -- ^ Each when clauses
      -> Record c b                 -- ^ Else result record
      -> Record c b                 -- ^ Result record
case' = Syntax.case'

-- | Uncurry version of 'case'', and you can write like ... `casesOrElse'` <else clause>.
casesOrElse' :: OperatorContext c
             => (Record c a, [(Record c a, Record c b)]) -- ^ Record value to match and each when clauses list
             -> Record c b                               -- ^ Else result record
             -> Record c b                               -- ^ Result record
casesOrElse' =  uncurry case'

-- | Null default version of 'case''.
caseMaybe :: (OperatorContext c {- (Record c) is always ProjectableMaybe -}, PersistableWidth b)
          => Record c a                         -- ^ Record value to match
          -> [(Record c a, Record c (Maybe b))] -- ^ Each when clauses
          -> Record c (Maybe b)                 -- ^ Result record
caseMaybe v cs = case' v cs nothing

-- | Binary operator corresponding SQL /IN/ .
in' :: OperatorContext c
    => Record c t -> RecordList (Record c) t -> Record c (Maybe Bool)
in' a lp = unsafeProjectSql' . SQL.paren
           $ SQL.in' (unsafeShowSql' a) (Record.unsafeStringSqlList unsafeShowSql' lp)

-- | Operator corresponding SQL /IS NULL/ , and extended against record types.
isNothing :: (OperatorContext c, HasColumnConstraint NotNull r)
          => Record c (Maybe r) -> Predicate c
isNothing mr = unsafeProjectSql' $
               SQL.paren $ (SQL.defineBinOp SQL.IS)
               (Record.unsafeStringSqlNotNullMaybe mr) SQL.NULL

-- | Operator corresponding SQL /NOT (... IS NULL)/ , and extended against record type.
isJust :: (OperatorContext c, HasColumnConstraint NotNull r)
          => Record c (Maybe r) -> Predicate c
isJust =  not' . isNothing

-- | Operator from maybe type using record extended 'isNull'.
fromMaybe :: (OperatorContext c, HasColumnConstraint NotNull r)
          => Record c r -> Record c (Maybe r) -> Record c r
fromMaybe d p = [ (isNothing p, d) ] `casesOrElse` unsafeCastProjectable p

unsafeUniTermFunction :: SqlContext c => Keyword -> Record c t
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

-- | Unsafely add placeholder parameter to queries.
unsafeAddPlaceHolders :: Functor f => f a -> f (PlaceHolders p, a)
unsafeAddPlaceHolders =  fmap ((,) PlaceHolders)

-- | Unsafely get placeholder parameter
unsafePlaceHolders :: PlaceHolders p
unsafePlaceHolders =  PlaceHolders

-- | No placeholder semantics
unitPlaceHolder :: PlaceHolders ()
unitPlaceHolder = pureP ()

-- | No placeholder semantics. Same as `unitPlaceHolder`
unitPH :: PlaceHolders ()
unitPH = pureP ()

-- | Unsafely cast placeholder parameter type.
unsafeCastPlaceHolders :: PlaceHolders a -> PlaceHolders b
unsafeCastPlaceHolders PlaceHolders = PlaceHolders

-- | Provide scoped placeholder from width and return its parameter object.
pwPlaceholder :: SqlContext c
              => PersistableRecordWidth a
              -> (Record c a -> b)
              -> (PlaceHolders a, b)
pwPlaceholder pw f = (PlaceHolders, f $ projectPlaceHolder pw)
  where
    projectPlaceHolder :: SqlContext c
                       => PersistableRecordWidth a
                       -> Record c a
    projectPlaceHolder = unsafeProjectSqlTerms . (`replicate` "?") . runPersistableRecordWidth

-- | Provide scoped placeholder and return its parameter object.
placeholder' :: (PersistableWidth t, SqlContext c) => (Record c t -> a) ->  (PlaceHolders t, a)
placeholder' = pwPlaceholder persistableWidth

-- | Provide scoped placeholder and return its parameter object. Monadic version.
placeholder :: (PersistableWidth t, SqlContext c, Monad m) => (Record c t -> m a) -> m (PlaceHolders t, a)
placeholder f = do
  let (ph, ma) = placeholder' f
  a <- ma
  return (ph, a)


-- | Zipping projections.
projectZip :: ProductIsoApplicative p => p a -> p b -> p (a, b)
projectZip pa pb = (,) |$| pa |*| pb

-- | Binary operator the same as 'projectZip'.
(><) :: ProductIsoApplicative p => p a -> p b -> p (a, b)
(><) = projectZip

-- | Interface to control 'Maybe' of phantom type in records.
class ProjectableMaybe p where
  -- | Cast record phantom type into 'Maybe'.
  just :: p a -> p (Maybe a)
  -- | Compose nested 'Maybe' phantom type on record.
  flattenMaybe :: p (Maybe (Maybe a)) -> p (Maybe a)

-- | Control phantom 'Maybe' type in placeholder parameters.
instance ProjectableMaybe PlaceHolders where
  just         = unsafeCastPlaceHolders
  flattenMaybe = unsafeCastPlaceHolders

-- | Control phantom 'Maybe' type in record type 'Record'.
instance ProjectableMaybe (Record c) where
  just         = Record.just
  flattenMaybe = Record.flattenMaybe


-- | Unsafely make aggregation uni-operator from SQL keyword.
unsafeAggregateOp :: (AggregatedContext ac, SqlContext ac)
                  => SQL.Keyword -> Record Flat a -> Record ac b
unsafeAggregateOp op = unsafeUniOp ((op SQL.<++>) . SQL.paren)

-- | Aggregation function COUNT.
count :: (Integral b, AggregatedContext ac, SqlContext ac)
      => Record Flat a -> Record ac b
count =  unsafeAggregateOp SQL.COUNT

-- | Aggregation function SUM.
sumMaybe :: (Num a, AggregatedContext ac, SqlContext ac)
         => Record Flat (Maybe a) -> Record ac (Maybe a)
sumMaybe  =  unsafeAggregateOp SQL.SUM

-- | Aggregation function SUM.
sum' :: (Num a, AggregatedContext ac, SqlContext ac)
     => Record Flat a -> Record ac (Maybe a)
sum'  =  sumMaybe . Record.just

-- | Aggregation function AVG.
avgMaybe :: (Num a, Fractional b, AggregatedContext ac, SqlContext ac)
         => Record Flat (Maybe a) -> Record ac (Maybe b)
avgMaybe   =  unsafeAggregateOp SQL.AVG

-- | Aggregation function AVG.
avg :: (Num a, Fractional b, AggregatedContext ac, SqlContext ac)
    => Record Flat a -> Record ac (Maybe b)
avg =  avgMaybe . Record.just

-- | Aggregation function MAX.
maxMaybe :: (Ord a, AggregatedContext ac, SqlContext ac)
         => Record Flat (Maybe a) -> Record ac (Maybe a)
maxMaybe  =  unsafeAggregateOp SQL.MAX

-- | Aggregation function MAX.
max' :: (Ord a, AggregatedContext ac, SqlContext ac)
     => Record Flat a -> Record ac (Maybe a)
max' =  maxMaybe . Record.just

-- | Aggregation function MIN.
minMaybe :: (Ord a, AggregatedContext ac, SqlContext ac)
         => Record Flat (Maybe a) -> Record ac (Maybe a)
minMaybe  =  unsafeAggregateOp SQL.MIN

-- | Aggregation function MIN.
min' :: (Ord a, AggregatedContext ac, SqlContext ac)
     => Record Flat a -> Record ac (Maybe a)
min' =  minMaybe . Record.just

-- | Aggregation function EVERY.
every :: (AggregatedContext ac, SqlContext ac)
      => Predicate Flat -> Record ac (Maybe Bool)
every =  unsafeAggregateOp SQL.EVERY

-- | Aggregation function ANY.
any' :: (AggregatedContext ac, SqlContext ac)
     => Predicate Flat -> Record ac (Maybe Bool)
any'  =  unsafeAggregateOp SQL.ANY

-- | Aggregation function SOME.
some' :: (AggregatedContext ac, SqlContext ac)
      => Predicate Flat -> Record ac (Maybe Bool)
some' =  unsafeAggregateOp SQL.SOME

-- | Get narrower record along with projection path.
(!) :: PersistableWidth a
    => Record c a -- ^ Source 'Record'
    -> Pi a b     -- ^ Record path
    -> Record c b -- ^ Narrower projected object
(!) = Record.pi

-- | Get narrower record along with projection path
--   'Maybe' phantom functor is 'map'-ed.
(?!) :: PersistableWidth a
     => Record c (Maybe a) -- ^ Source 'Record'. 'Maybe' type
     -> Pi a b             -- ^ Record path
     -> Record c (Maybe b) -- ^ Narrower projected object. 'Maybe' type result
(?!) = Record.piMaybe

-- | Get narrower record along with projection path
--   and project into result record type.
--   Source record 'Maybe' phantom functor and projection path leaf 'Maybe' functor are 'join'-ed.
(?!?) :: PersistableWidth a
      => Record c (Maybe a) -- ^ Source 'Record'. 'Maybe' phantom type
      -> Pi a (Maybe b)     -- ^ Record path. 'Maybe' type leaf
      -> Record c (Maybe b) -- ^ Narrower projected object. 'Maybe' phantom type result
(?!?) = Record.piMaybe'


-- | Interface to compose phantom 'Maybe' nested type.
class ProjectableFlattenMaybe a b where
  flatten :: ProjectableMaybe p => p a -> p b

-- | Compose 'Maybe' type in record phantom type.
instance ProjectableFlattenMaybe (Maybe a) b
         => ProjectableFlattenMaybe (Maybe (Maybe a)) b where
  flatten = flatten . flattenMaybe

-- | Not 'Maybe' type is not processed.
instance ProjectableFlattenMaybe (Maybe a) (Maybe a) where
  flatten = id

-- | Get narrower record with flatten leaf phantom Maybe types along with projection path.
flattenPiMaybe :: (PersistableWidth a, ProjectableMaybe (Record cont), ProjectableFlattenMaybe (Maybe b) c)
               => Record cont (Maybe a) -- ^ Source 'Record'. 'Maybe' phantom type
               -> Pi a b                -- ^ Projection path
               -> Record cont c         -- ^ Narrower 'Record'. Flatten 'Maybe' phantom type
flattenPiMaybe p = flatten . Record.piMaybe p

-- | Get narrower record with flatten leaf phantom Maybe types along with projection path.
(!??) :: (PersistableWidth a, ProjectableMaybe (Record cont), ProjectableFlattenMaybe (Maybe b) c)
      => Record cont (Maybe a) -- ^ Source 'Record'. 'Maybe' phantom type
      -> Pi a b                -- ^ Projection path
      -> Record cont c         -- ^ Narrower flatten and projected object.
(!??) = flattenPiMaybe


infixl 8 !, ?!, ?!?, !??
infixl 7 .*., ./., ?*?, ?/?
infixl 6 .+., .-., ?+?, ?-?
infixl 5 .||., ?||?
infix  4 .=., .<>., .>., .>=., .<., .<=., `in'`, `like`, `likeMaybe`, `like'`, `likeMaybe'`
infixr 3 `and'`
infixr 2 `or'`
infixl 1  ><
