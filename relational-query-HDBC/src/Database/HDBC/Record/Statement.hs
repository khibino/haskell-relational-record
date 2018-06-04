{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.HDBC.Record.Statement
-- Copyright   : 2013-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed statement running sequence
-- which intermediate structures are typed.
module Database.HDBC.Record.Statement (
  PreparedStatement, untypePrepared, unsafePrepare, finish,

  withUnsafePrepare, withPrepareNoFetch,

  BoundStatement (..), bind, bindTo,

  ExecutedStatement, executed, result,

  executeBound, execute, executePrepared,

  prepareNoFetch,
  executeBoundNoFetch, executeNoFetch, runPreparedNoFetch,
  runNoFetch, mapNoFetch,
  ) where

import Control.Exception (bracket)
import Database.Relational (UntypeableNoFetch (untypeNoFetch))
import Database.HDBC (IConnection, Statement, SqlValue)
import qualified Database.HDBC as HDBC

import Database.Record (ToSql, fromRecord)

-- | Typed prepared statement type.
newtype PreparedStatement p a =
  PreparedStatement {
    -- | Untyped prepared statement before executed.
    prepared :: Statement
    }

-- | Typed prepared statement which has bound placeholder parameters.
data BoundStatement a =
  BoundStatement
  {
    -- | Untyped prepared statement before executed.
    bound  :: !Statement
    -- | Bound parameters.
  , params :: [SqlValue]
  }

-- | Typed executed statement.
data ExecutedStatement a =
  ExecutedStatement
  { -- | Untyped executed statement.
    executed :: !Statement
    -- | Result of HDBC execute.
  , result   :: !Integer
  }

-- | Unsafely untype prepared statement.
untypePrepared :: PreparedStatement p a -> Statement
untypePrepared =  prepared

-- | Run prepare and unsafely make Typed prepared statement.
unsafePrepare :: IConnection conn
              => conn                       -- ^ Database connection
              -> String                     -- ^ Raw SQL String
              -> IO (PreparedStatement p a) -- ^ Result typed prepared query with parameter type 'p' and result type 'a'
unsafePrepare conn = fmap PreparedStatement . HDBC.prepare conn

-- | Generalized prepare inferred from 'UntypeableNoFetch' instance.
prepareNoFetch :: (UntypeableNoFetch s, IConnection conn)
               => conn
               -> s p
               -> IO (PreparedStatement p ())
prepareNoFetch conn = unsafePrepare conn . untypeNoFetch

-- | Close PreparedStatement. Useful for connection pooling cases.
finish :: PreparedStatement p a -> IO ()
finish = HDBC.finish . prepared

-- | Bracketed prepare operation.
--   Unsafely make Typed prepared statement.
withUnsafePrepare :: IConnection conn
                  => conn   -- ^ Database connection
                  -> String -- ^ Raw SQL String
                  -> (PreparedStatement p a -> IO b)
                  -> IO b
withUnsafePrepare conn qs =
  bracket (unsafePrepare conn qs) finish

-- | Bracketed prepare operation.
--   Generalized prepare inferred from 'UntypeableNoFetch' instance.
withPrepareNoFetch :: (UntypeableNoFetch s, IConnection conn)
                   => conn
                   -> s p
                   -> (PreparedStatement p () -> IO a)
                   -> IO a
withPrepareNoFetch conn s =
  bracket (prepareNoFetch conn s) finish

-- | Typed operation to bind parameters. Inferred 'ToSql' is used.
bind :: ToSql SqlValue p
     => PreparedStatement p a -- ^ Prepared query to bind to
     -> p                     -- ^ Parameter to bind
     -> BoundStatement a      -- ^ Result parameter bound statement
bind q p = BoundStatement { bound = prepared q, params = fromRecord p }

-- | Same as 'bind' except for argument is flipped.
bindTo :: ToSql SqlValue p => p -> PreparedStatement p a -> BoundStatement a
bindTo =  flip bind

-- | Typed execute operation.
executeBound :: BoundStatement a -> IO (ExecutedStatement a)
executeBound bs = do
  let stmt = bound bs
  n <- HDBC.execute stmt (params bs)
  n `seq` return (ExecutedStatement stmt n)

{-# WARNING execute "Use 'executeBound' instead of this. This name will be used for executePrepared function in future release." #-}
-- | Use 'executeBound' instead of this.
--   WARNING! This name will be used for executePrepared function in future release.
execute :: BoundStatement a -> IO (ExecutedStatement a)
execute = executeBound

-- | Bind parameters, execute prepared statement and get executed statement.
executePrepared ::  ToSql SqlValue p => PreparedStatement p a -> p -> IO (ExecutedStatement a)
executePrepared st = executeBound . bind st

-- | Typed execute operation. Only get result.
executeBoundNoFetch :: BoundStatement () -> IO Integer
executeBoundNoFetch = fmap result . executeBound

{- WARNING executeNoFetch "Use 'executeBoundNoFetch' instead of this. This name will be used for runPreparedNoFetch function in future release." -}
-- | Use 'executeBoundNoFetch' instead of this.
--   WARNING! This name will be used for runPreparedNoFetch function in future release.
executeNoFetch :: BoundStatement () -> IO Integer
executeNoFetch = executeBoundNoFetch

-- | Bind parameters, execute prepared statement and get execution result.
runPreparedNoFetch :: ToSql SqlValue a
                   => PreparedStatement a ()
                   -> a
                   -> IO Integer
runPreparedNoFetch p = executeBoundNoFetch . (p `bind`)

-- | Prepare and run sequence for polymorphic no-fetch statement.
runNoFetch :: (UntypeableNoFetch s, IConnection conn, ToSql SqlValue a)
           => conn
           -> s a
           -> a
           -> IO Integer
runNoFetch conn s p = withPrepareNoFetch conn s (`runPreparedNoFetch` p)

-- | Prepare and run it against each parameter list.
mapNoFetch :: (UntypeableNoFetch s, IConnection conn, ToSql SqlValue a)
           => conn
           -> s a
           -> [a]
           -> IO [Integer]
mapNoFetch conn s rs =
  withPrepareNoFetch conn s (\ps -> mapM (runPreparedNoFetch ps) rs)
