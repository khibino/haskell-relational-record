module Text.Parser.List
       ( Parser, runParser, evalParser
       , Error, errorE, mapE, errorP, noteP

       , token, eof, sink, satisfy', satisfy, list
       ) where

import Control.Applicative (pure)
import Control.Monad (guard)
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT, get, put)
import Data.Monoid (Last (..))

import Control.Monad.Either.Instances ()


type Error = Last String
type Parser t = StateT [t] (Either Error)

runParser :: Parser t a -> [t] -> Either Error (a, [t])
runParser = runStateT

evalParser :: Parser t a -> [t] -> Either Error a
evalParser = evalStateT

errorE :: String -> Either Error a
errorE = Left . Last . Just

mapE :: (String -> String) -> Either Error a -> Either Error a
mapE f ee = case ee of
  Left (Last (Just m))  ->  errorE $ f m
  Left (Last Nothing)   ->  ee
  Right _               ->  ee

errorP :: String -> Parser t a
errorP = StateT . const . errorE

noteP :: String -> Maybe a -> Parser t a
noteP s = maybe (errorP s) pure

token :: Parser t t
token = do
  cs0 <- get
  case cs0 of
    c:cs  ->  do
      put cs
      pure c
    []    ->
      errorP "token: end of input"

eof :: Parser t ()
eof = do
  cs <- get
  case cs of
    []   ->  pure ()
    _:_  ->  errorP "eof: not empty input"

sink :: Parser t [t]
sink = do
  cs <- get
  put []
  pure cs

satisfy' :: String         -- ^ Parser name to print when error
         -> (t -> String)  -- ^ Function to build error string
         -> (t -> Bool)    -- ^ Predicate to satisfy
         -> Parser t t     -- ^ Result parser
satisfy' n ef p = do
  c <- token
  noteP (n ++ ": " ++ ef c) . guard $ p c
  return c

-- | make satisfy parser with monoid-empty error.
satisfy :: (t -> Bool) -> Parser t t
satisfy p = do
  c <- token
  guard $ p c -- expect empty error
  return c

list :: Eq t => [t] -> Parser t [t]
list = mapM (satisfy . (==))
