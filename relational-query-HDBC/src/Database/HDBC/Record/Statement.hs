{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.HDBC.Record.Statement
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides typed statement running sequence
-- which intermediate structres are typed.
module Database.HDBC.Record.Statement (
  PreparedStatement, unsafePrepare,

  BoundStatement (..), bind', bind, bindTo,

  ExecutedStatement, executed, result, execute,

  executeNoFetch, runPreparedNoFetch
  ) where

import Database.HDBC (IConnection, Statement, SqlValue)
import qualified Database.HDBC as HDBC

import Database.Record
  (RecordToSql, ToSql(recordToSql), runFromRecord)

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
    bound  :: Statement
    -- | Bound parameters.
  , params :: [SqlValue]
  }

-- | Typed executed statement.
data ExecutedStatement a =
  ExecutedStatement
  { -- | Untyped executed statement.
    executed :: Statement
    -- | Result of HDBC execute.
  , result   :: Integer
  }

-- | Run prepare and unsafely make Typed prepared statement.
unsafePrepare :: IConnection conn
        => conn                   -- ^ Database connection
        -> String              -- ^ Raw SQL String
        -> IO (PreparedStatement p a) -- ^ Result typed prepared query with parameter type 'p' and result type 'a'
unsafePrepare conn = fmap PreparedStatement . HDBC.prepare conn

-- | Typed operation to bind parameters.
bind' :: RecordToSql SqlValue p -- ^ Proof object to convert from parameter type 'p' into 'SqlValue' list.
      -> PreparedStatement p a  -- ^ Prepared query to bind to
      -> p                      -- ^ Parameter to bind
      -> BoundStatement a       -- ^ Result parameter bound statement
bind' toSql q p = BoundStatement { bound = prepared q, params = runFromRecord toSql p }

-- | Typed operation to bind parameters. Infered 'RecordToSql' is used.
bind :: ToSql SqlValue p => PreparedStatement p a -> p -> BoundStatement a
bind =  bind' recordToSql

-- | Same as 'bind' except for argument is flipped.
bindTo :: ToSql SqlValue p => p -> PreparedStatement p a -> BoundStatement a
bindTo =  flip bind

-- | Typed execute operation.
execute :: BoundStatement a -> IO (ExecutedStatement a)
execute bs = do
  let stmt = bound bs
  n <- HDBC.execute stmt (params bs)
  return $ ExecutedStatement stmt n

-- | Typed execute operation. Only get result.
executeNoFetch :: BoundStatement () -> IO Integer
executeNoFetch =  fmap result . execute

-- | Bind parameters, execute statement and get execution result.
runPreparedNoFetch :: ToSql SqlValue a
                  => PreparedStatement a ()
                  -> a
                  -> IO Integer
runPreparedNoFetch p = executeNoFetch . (p `bind`)
