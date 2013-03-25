{-# LANGUAGE Rank2Types #-}

module Database.HDBC.Session (
  -- * Bracketed session
  -- $bracketedSession
  withConnection, withConnectionIO
  ) where

import Database.HDBC (IConnection, handleSqlError)
import qualified Database.HDBC as HDBC
import Control.Exception (bracket)


{- $bracketedSession
This module provides a base function to call close correctly against opend DB connection.

Bracket function implementation is provided by several packages,
so this package provides base implementation which requires
bracket function and corresponding lift function.
-}

-- | Run a transaction on a HDBC IConnection and close the connection.
withConnection :: (Monad m, IConnection conn)
            => (m conn -> (conn -> m ()) -> (conn -> m a) -> m a) -- ^ bracket
            -> (forall b. IO b -> m b)                            -- ^ lift
            -> IO conn                                            -- ^ Connect action
            -> (conn -> m a)                                      -- ^ Transaction body
            -> m a
withConnection bracket' lift connect tbody =
  bracket'
    (lift $ handleSqlError connect)
    (lift
     . handleSqlError
     . HDBC.disconnect)
    (\conn -> do
        x <- tbody conn
        -- Do rollback independent from driver default behavior when disconnect.
        lift $ HDBC.rollback conn
        return x)

-- | Run a transaction on a HDBC 'IConnection' and close the connection.
--   Simple 'IO' version.
withConnectionIO :: IConnection conn
          => IO conn        -- ^ Connect action
          -> (conn -> IO a) -- ^ Transaction body
          -> IO a
withConnectionIO =  withConnection bracket id
