-- |
-- Module      : Database.HDBC.Schema.Driver
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides driver interface
-- to load database system catalog via HDBC.
module Database.HDBC.Schema.Driver (
  TypeMap,
  Driver(Driver, typeMap, getFieldsWithMap, getPrimaryKey),
  emptyDriver,
  getFields
  ) where

import Database.HDBC (IConnection)
import Data.IORef (IORef, modifyIORef)
import Data.Monoid ((<>))
import Data.DList (DList)
import Control.Applicative (pure)
import Language.Haskell.TH (TypeQ)


-- | Mapping between type name string of DBMS and type in Haskell.
--   Type name string depends on specification of DBMS system catalogs.
type TypeMap = [(String, TypeQ)]

data Log
  = Warning String
  | Error String

runLog :: (String -> t) -> (String -> t) -> Log -> t
runLog wf ef = d  where
  d (Warning m) = wf m
  d (Error m)   = ef m

-- | Channel to store compile-time warning messages.
type LogChan = IORef (DList Log)

putLog :: Maybe LogChan -> Log -> IO ()
putLog mc m = maybe (return ()) (`modifyIORef` (<> pure m)) mc

putWarning :: Maybe LogChan -> String -> IO ()
putWarning mc = putLog mc . Warning

putError :: Maybe LogChan -> String -> IO ()
putError mc = putLog mc . Warning

-- | Interface type to load database system catalog via HDBC.
data Driver conn =
  Driver
  { -- | Custom type mapping of this driver
    typeMap   :: TypeMap

    -- | Get column name and Haskell type pairs and not-null columns index.
  , getFieldsWithMap :: TypeMap                       --  Custom type mapping
                     -> conn                          --  Connection to query system catalog
                     -> String                        --  Schema name string
                     -> String                        --  Table name string
                     -> IO ([(String, TypeQ)], [Int]) --  Action to get column name and Haskell type pairs and not-null columns index.

    -- | Get primary key column name.
  , getPrimaryKey :: conn          --  Connection to query system catalog
                  -> String        --  Schema name string
                  -> String        --  Table name string
                  -> IO [String]   --  Action to get column names of primary key
  }

-- | Empty definition of 'Driver'
emptyDriver :: IConnection conn => Driver conn
emptyDriver =  Driver [] (\_ _ _ _ -> return ([],[])) (\_ _ _ -> return [])

-- | Helper function to call 'getFieldsWithMap' using 'typeMap' of 'Driver'.
getFields :: IConnection conn => Driver conn -> conn -> String -> String -> IO ([(String, TypeQ)], [Int])
getFields drv = getFieldsWithMap drv (typeMap drv)
