{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Module      : Database.Relational.Compat
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains backward compatibility definitions of
-- relational-query <= 0.9.*
module Database.Relational.Compat
  {-# DEPRECATED "backward compatibility definitions relational-query <= 0.9.*" #-} (
  -- * deprecated types
  Projection, ListProjection,

  -- * deprecated classes
  ProductConstructor (..),
  ProjectableFunctor, (|$|), ProjectableApplicative, (|*|),

  ProjectableIdZip, rightId, leftId,

  SqlProjectable (..), unsafeProjectSql,
  ProjectableShowSql (..), unsafeShowSql,

  -- * deprecated defintions about Pi
  pfmap, pap, pzero,
  ) where

import Data.Functor.ProductIsomorphic
  (ProductIsoFunctor, (|$|), ProductIsoApplicative, (|*|),
   ProductIsoEmpty, pureE, peRight, peLeft, )
import Data.Functor.ProductIsomorphic.Unsafe (ProductConstructor (..))

import Database.Relational hiding (unsafeShowSql', unsafeShowSql, unsafeProjectSqlTerms, unsafeProjectSql, )
import Database.Relational.Internal.String (showStringSQL, stringSQL)
import qualified Database.Relational.Record as Record
import qualified Database.Relational.Projectable as Projectable

{-# DEPRECATED Projection "Replaced by Record type" #-}
-- | deprecated 'Projection' type replaced by 'Record' type.
type Projection = Record

{-# DEPRECATED ListProjection "Replaced by RecordList type" #-}
-- | deprecated 'Projection' type replaced by 'Record' type.
type ListProjection = RecordList

{-# DEPRECATED ProjectableFunctor "use ProductIsoFunctor in Data.Functor.ProductIsomorphic instead of this. " #-}
-- | deprecated 'ProjectableFunctor' class replaced by 'ProductIsoFunctor' class.
type ProjectableFunctor = ProductIsoFunctor

{-# DEPRECATED ProjectableApplicative "use ProductIsoApplicative in Data.Functor.ProductIsomorphic instead of this. " #-}
-- | deprecated 'ProjectableApplicative' class replaced by 'ProductIsoApplicative' class.
type ProjectableApplicative = ProductIsoApplicative

{-# DEPRECATED ProjectableIdZip "use ProductIsoEmpty in Data.Functor.ProductIsomorphic instead of this. " #-}
-- | deprecated 'ProjectableIdZip' class replaced by 'ProductIsoEmpty' class.
type ProjectableIdZip p = ProductIsoEmpty p ()

{-# DEPRECATED rightId "use peRight in Data.Functor.ProductIsomorphic instead of this. " #-}
-- | deprecated right-id-law definition replaced by 'peRight'
rightId :: ProjectableIdZip p => p (a, ()) -> p a
rightId = peRight

{-# DEPRECATED leftId "use peLeft in Data.Functor.ProductIsomorphic instead of this. " #-}
-- | deprecated left-id-law definition replaced by 'peLeft'
leftId :: ProjectableIdZip p => p ((), a) -> p a
leftId = peLeft

-- Pi

{-# DEPRECATED pfmap "use (|$|) in Data.Functor.ProductIsomorphic instead of this." #-}
-- | deprecated map definition for Pi type. use ('|$|') instead of this.
pfmap :: ProductConstructor (a -> b) => (a -> b) -> Pi r a -> Pi r b
pfmap = (|$|)

{-# DEPRECATED pap "use (|*|) in Data.Functor.ProductIsomorphic instead of this." #-}
-- | deprecated ap definition for Pi type. use ('|*|') instead of this.
pap :: Pi r (a -> b) -> Pi r a -> Pi r b
pap = (|*|)

{-# DEPRECATED pzero "use pureE in Data.Functor.ProductIsomorphic instead of this." #-}
-- | deprecated empty definition for Pi type. use 'pureE' instead of this.
pzero :: Pi a ()
pzero = pureE

-- type classes

{-# DEPRECATED SqlProjectable "use 'SqlContext c => Record c a' instead of 'SqlProjectable p => p a'." #-}
-- | Interface to project SQL terms unsafely.
class SqlProjectable p where
  -- | Unsafely project from SQL expression terms.
  unsafeProjectSqlTerms :: [StringSQL] -- ^ SQL expression strings
                        -> p t         -- ^ Result record
{-# DEPRECATED unsafeProjectSqlTerms "Use Database.Relational.unsafeProjectSqlTerms instead of this." #-}

-- | Unsafely make 'Record' from SQL terms.
instance SqlContext c => SqlProjectable (Record c) where
  unsafeProjectSqlTerms = Projectable.unsafeProjectSqlTerms

{-# DEPRECATED unsafeProjectSql "Use Database.Relational.unsafeProjectSql instead of this." #-}
-- | Deprecated. Use Database.Relational.unsafeProjectSql instead of this.
unsafeProjectSql :: SqlProjectable p => String -> p a
unsafeProjectSql = unsafeProjectSqlTerms . (:[]) . stringSQL

{-# DEPRECATED ProjectableShowSql "specialized type 'Record c' should be used instead of this constraint." #-}
-- | Interface to get SQL expression from a record.
class ProjectableShowSql p where
  unsafeShowSql' :: p a -> StringSQL
{-# DEPRECATED unsafeShowSql' "Use Database.Relational.unsafeShowSql' instead of this." #-}

instance ProjectableShowSql (Record c) where
  unsafeShowSql' = Record.unsafeStringSql

{-# DEPRECATED unsafeShowSql "Use Database.Relational.unsafeShowSql instead of this." #-}
-- | Deprecated. Use Database.Relational.unsafeShowSql instead of this.
unsafeShowSql :: ProjectableShowSql p => p a -> String
unsafeShowSql = showStringSQL . unsafeShowSql'
