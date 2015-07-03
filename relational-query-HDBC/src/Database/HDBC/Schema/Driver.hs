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

  Log, runLog,
  LogChan, newLogChan, takeLogs, putWarning, putError, putVerbose,

  Driver(Driver, typeMap, getFieldsWithMap, getPrimaryKey),
  emptyDriver,
  getFields
  ) where

import Database.HDBC (IConnection)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Monoid (mempty, (<>))
import Data.DList (DList, toList)
import Control.Applicative ((<$>), pure, (<*>))
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
data LogChan =
  LogChan
  { chan :: IORef (DList Log)
  , verboseAsWarning :: Bool
  }

newLogChan :: Bool -> IO LogChan
newLogChan v =
  LogChan <$> newIORef mempty <*> pure v

takeLogs :: LogChan -> IO [Log]
takeLogs lchan = do
  xs <- readIORef $ chan lchan
  writeIORef (chan lchan) mempty
  return $ toList xs

putLog :: LogChan -> Log -> IO ()
putLog lchan m = chan lchan `modifyIORef` (<> pure m)

putWarning :: LogChan -> String -> IO ()
putWarning lchan = putLog lchan . Warning

putError :: LogChan -> String -> IO ()
putError lchan = putLog lchan . Warning

-- | Put verbose compile-time message as warning when 'verboseAsWarning'.
putVerbose :: LogChan -> String -> IO ()
putVerbose lchan
  | verboseAsWarning lchan  =  putWarning lchan . ("info: " ++)
  | otherwise               =  const $ pure ()

-- | Interface type to load database system catalog via HDBC.
data Driver conn =
  Driver
  { -- | Custom type mapping of this driver
    typeMap   :: TypeMap

    -- | Get column name and Haskell type pairs and not-null columns index.
  , getFieldsWithMap :: TypeMap                       --  Custom type mapping
                     -> conn                          --  Connection to query system catalog
                     -> LogChan
                     -> String                        --  Schema name string
                     -> String                        --  Table name string
                     -> IO ([(String, TypeQ)], [Int]) {-  Action to get column name and Haskell type pairs
                                                           and not-null columns index. -}

    -- | Get primary key column name.
  , getPrimaryKey :: conn          --  Connection to query system catalog
                  -> LogChan
                  -> String        --  Schema name string
                  -> String        --  Table name string
                  -> IO [String]   --  Action to get column names of primary key
  }

-- | Empty definition of 'Driver'
emptyDriver :: IConnection conn => Driver conn
emptyDriver =  Driver [] (\_ _ _ _ _ -> return ([],[])) (\_ _ _ _ -> return [])

-- | Helper function to call 'getFieldsWithMap' using 'typeMap' of 'Driver'.
getFields :: IConnection conn => Driver conn -> conn -> LogChan -> String -> String -> IO ([(String, TypeQ)], [Int])
getFields drv = getFieldsWithMap drv (typeMap drv)
