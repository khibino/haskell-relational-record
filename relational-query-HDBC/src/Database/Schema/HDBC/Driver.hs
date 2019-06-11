-- |
-- Module      : Database.Schema.HDBC.Driver
-- Copyright   : 2013-2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides driver interface
-- to load database system catalog via HDBC.
module Database.Schema.HDBC.Driver (
  TypeMap,

  Log, foldLog,
  LogChan, emptyLogChan, takeLogs, putWarning, putError, putVerbose,
  failWith, hoistMaybe, maybeIO,

  Driver(Driver, typeMap, driverConfig, getFieldsWithMap, getPrimaryKey),
  emptyDriver,
  getFields,
  ) where

import Language.Haskell.TH (TypeQ)
import Control.Applicative ((<$>), pure)
import Control.Monad (MonadPlus, mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Monoid (mempty, (<>))
import Data.DList (DList, toList)

import Database.HDBC (IConnection)
import Database.Relational (Config, defaultConfig)


-- | Mapping between type name string of DBMS and type in Haskell.
--   Type name string depends on specification of DBMS system catalogs.
type TypeMap = [(String, TypeQ)]

-- | Log string type for compile time.
data Log
  = Verbose String
  | Warning String
  | Error String

-- | Folding operation of 'Log' type.
foldLog :: (String -> t) -> (String -> t) -> (String -> t) -> Log -> t
foldLog vf wf ef = d  where
  d (Verbose m) = vf m
  d (Warning m) = wf m
  d (Error m)   = ef m

-- | Channel to store compile-time warning messages.
newtype LogChan = LogChan { chan :: IORef (DList Log) }

-- | Build and return a new instance of 'LogChan'.
emptyLogChan :: IO LogChan
emptyLogChan = LogChan <$> newIORef mempty

-- | Take all logs list from channel.
takeLogs :: LogChan -> IO [Log]
takeLogs lchan = do
  xs <- readIORef $ chan lchan
  writeIORef (chan lchan) mempty
  return $ toList xs

putLog :: LogChan -> Log -> IO ()
putLog lchan m = chan lchan `modifyIORef` (<> pure m)

-- | Push a warning string into 'LogChan'.
putWarning :: LogChan -> String -> IO ()
putWarning lchan = putLog lchan . Warning

-- | Push an error string into 'LogChan'.
putError :: LogChan -> String -> IO ()
putError lchan = putLog lchan . Error

-- | Put verbose compile-time message as warning when 'verboseAsWarning'.
putVerbose :: LogChan -> String -> IO ()
putVerbose lchan = putLog lchan . Verbose

-- | Push an error string into 'LogChan' and return failed context.
failWith :: LogChan -> String -> MaybeT IO a
failWith lchan m = do
  lift $ putError lchan m
  mzero

hoistM :: MonadPlus m => Maybe a -> m a
hoistM = maybe mzero return

-- | Hoist from 'Maybe' context into 'MaybeT'.
hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = hoistM

maybeT :: Functor f => b -> (a -> b) -> MaybeT f a -> f b
maybeT zero f = (maybe zero f <$>) . runMaybeT

-- | Run 'MaybeT' with default value.
maybeIO :: b -> (a -> b) -> MaybeT IO a -> IO b
maybeIO = maybeT

-- | Interface type to load database system catalog via HDBC.
data Driver conn =
  Driver
  { -- | Custom type mapping of this driver
    typeMap   :: TypeMap

    -- | Custom configuration for this driver
  , driverConfig :: Config

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
emptyDriver :: IConnection conn
            => Driver conn
emptyDriver = Driver [] defaultConfig (\_ _ _ _ _ -> return ([],[])) (\_ _ _ _ -> return [])

-- | Helper function to call 'getFieldsWithMap' using 'typeMap' of 'Driver'.
getFields :: IConnection conn
          => Driver conn   -- ^ driver record
          -> conn          -- ^ connection
          -> LogChan       -- ^ log channel
          -> String        -- ^ schema name string
          -> String        -- ^ table name string
          -> IO ([(String, TypeQ)], [Int])
getFields drv = getFieldsWithMap drv (typeMap drv)
