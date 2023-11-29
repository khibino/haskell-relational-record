{-# LANGUAGE Rank2Types #-}

-- |
-- Module      : Database.HDBC.Session
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides a base bracketed function
-- to call close correctly against opend DB connection.
module Database.HDBC.Session (
  -- * Bracketed session
  -- $bracketedSession
  transaction,

  withConnectionIO, withConnectionIO_,

  bracketConnection,

  -- * Show errors
  -- $showErrors
  showSqlError, handleSqlError',

  -- * Deprecated
  withConnection,
  withConnectionIO',
  withConnectionCommit,
  ) where

import Database.HDBC (IConnection, handleSql,
                      SqlError(seState, seNativeError, seErrorMsg))
import qualified Database.HDBC as HDBC
import Control.Exception (bracket)


{- $bracketedSession
Bracket function implementation is provided by several packages,
so this package provides base implementation which requires
bracket function and corresponding lift function.
-}

{- $showErrors
Functions to show 'SqlError' type not to show 'String' fields.
-}

-- | show 'SqlError' not to show 'String' fields.
showSqlError :: SqlError -> String
showSqlError se = unlines
  ["seState: '" ++ seState se ++ "'",
   "seNativeError: " ++ show (seNativeError se),
   "seErrorMsg: '" ++ seErrorMsg se ++ "'"]

-- | Like 'handleSqlError', but not to show 'String' fields of SqlError.
handleSqlError' :: IO a -> IO a
handleSqlError' =  handleSql (fail . reformat . showSqlError)  where
  reformat = ("SQL error: \n" ++) . unlines . map ("  " ++) . lines

-- | Generalized session with bracketed HDBC connection.
--   Run a transaction on a HDBC IConnection and close the connection.
bracketConnection :: (Monad m, IConnection conn)
                  => (forall c. m c -> (c -> m ()) -> (c -> m a) -> m a) -- ^ bracket
                  -> (forall b. IO b -> m b)                             -- ^ lift
                  -> IO conn                                             -- ^ Connect action
                  -> (conn -> m a)                                       -- ^ Transaction body
                  -> m a
bracketConnection bracket_ lift connect tbody =
    bracket_ (lift open) (lift . close) bodyWithRollback
  where
    open  = handleSqlError' connect
    close :: IConnection conn => conn -> IO ()
    close =  handleSqlError' . HDBC.disconnect
    bodyWithRollback conn =
      bracket_
      (return ())
      -- Do rollback independent from driver default behavior when disconnect.
      (const . lift . handleSqlError' $ HDBC.rollback conn)
      (const $ tbody conn)

{-# DEPRECATED withConnection "use 'bracketConnection' instead of this." #-}
-- | Deprecated. use 'bracketConnection' instead of this.
withConnection :: (Monad m, IConnection conn)
               => (forall c. m c -> (c -> m ()) -> (c -> m a) -> m a)
               -> (forall b. IO b -> m b)
               -> IO conn
               -> (conn -> m a)
               -> m a
withConnection = bracketConnection

-- | Same as 'withConnectionIO' other than not wrapping transaction body in 'handleSqlError''.
withConnectionIO_ :: IConnection conn
                  => IO conn        -- ^ Connect action
                  -> (conn -> IO a) -- ^ Transaction body
                  -> IO a           -- ^ Result transaction action
withConnectionIO_ = bracketConnection bracket id

-- | Run a transaction on a HDBC 'IConnection' and close the connection.
--   Not issuing commit at last, so if you need, issue commit manually in transaction body.
withConnectionIO :: IConnection conn
                 => IO conn        -- ^ Connect action
                 -> (conn -> IO a) -- ^ Transaction body
                 -> IO a           -- ^ Result transaction action
withConnectionIO connect body = withConnectionIO_ connect $ handleSqlError' . body

{-# DEPRECATED withConnectionIO' "use 'withConnectionIO' instead of this." #-}
-- | Deprecated. use 'withConnectionIO' instead of this.
withConnectionIO' :: IConnection conn
                  => IO conn        -- ^ Connect action
                  -> (conn -> IO a) -- ^ Transaction body
                  -> IO a           -- ^ Result transaction action
withConnectionIO' = withConnectionIO

-- | Run a transaction on a HDBC 'IConnection' and commit at last, and then close the connection.
--   In other words, the transaction with no exception is committed.
--   Handy definition for simple transactions.
transaction :: IConnection conn
            => IO conn        -- ^ Connect action
            -> (conn -> IO a) -- ^ Transaction body
            -> IO a           -- ^ Result transaction action
transaction conn body =
  withConnectionIO conn $ \c -> do
    x <- body c
    HDBC.commit c
    return x

{-# DEPRECATED withConnectionCommit "use 'transaction' instead of this." #-}
-- | Deprecated. use 'transaction' instead of this.
withConnectionCommit :: IConnection conn
                     => IO conn        -- ^ Connect action
                     -> (conn -> IO a) -- ^ Transaction body
                     -> IO a           -- ^ Result transaction action
withConnectionCommit conn body =
  withConnectionIO_ conn $ \c -> do
    x <- body c
    HDBC.commit c
    return x
