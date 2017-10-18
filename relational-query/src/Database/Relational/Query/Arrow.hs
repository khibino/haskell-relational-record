-- |
-- Module      : Database.Relational.Query.Arrow
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains backword compatibility interface of
-- relational-query <= 0.9.*
module Database.Relational.Query.Arrow
       {-# DEPRECATED "Should switch namespace to Database.Relational.Arrow" #-}
       ( module Database.Relational.Arrow,
         module Database.Relational.Compat,
       )  where

import Database.Relational.Arrow hiding (unsafeShowSql', unsafeShowSql, unsafeProjectSqlTerms, unsafeProjectSql)
import Database.Relational.Compat
