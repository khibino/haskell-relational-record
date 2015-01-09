module Lex (eqProp) where

import Control.Applicative
  ((<$>), (<*>), pure, (*>), (<*), (<|>), empty, many, some)
import Control.Monad (void)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import qualified Text.ParserCombinators.ReadP as ReadP

import Distribution.TestSuite (Test)
import Distribution.TestSuite.Compat (prop')


type Var = Int

data Token
  = Qualifier Var
  | Table Var
  | Symbol String
  | Op String
  | String String
  | LParen
  | RParen
  | Comma
  | PlaceHolder
  deriving (Eq, Show)

type VarName = String

data QState =
  QState
  { nextVar  ::  Var
  , varMap   ::  Map VarName Var
  } deriving Eq

type Parser = StateT QState ReadP

char :: Char -> Parser Char
char =  lift . ReadP.char

satisfy :: (Char -> Bool) -> Parser Char
satisfy =  lift . ReadP.satisfy

quote :: Parser Char
quote =  char '\''

symbolCharset :: [Char]
symbolCharset =  '_' : ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

symbol' :: Parser String
symbol' =  some $ satisfy (`elem` symbolCharset)

symbol :: Parser Token
symbol =  Symbol <$> symbol'

opCharset :: [Char]
opCharset =  "=<>+-*/|"

op :: Parser Token
op =  Op <$> some (satisfy (`elem` opCharset))

stringChar :: Parser Char
stringChar =  quote *> quote <|> satisfy (`notElem` ("'().\""))

string :: Parser Token
string =  String <$> (quote *> many stringChar <* quote)

queryVar :: VarName -> Parser Var
queryVar n = do
  s  <-  get
  let m  =  varMap s
      v' =  nextVar s
  maybe
    (do put $ QState { nextVar = v' + 1, varMap = Map.insert n v' m }
        return v')
    return
    $ n `Map.lookup` m

qualified :: Parser Token
qualified =  do
  s <- symbol'
  void $ char '.'
  Qualifier <$> queryVar s

table :: Parser Token
table =  do
  t <- (:) <$> char 'T' <*> some (satisfy (`elem` ['0'..'9']))
  Table <$> queryVar t

space :: Parser Char
space =  satisfy (`elem` " \t")

someSpaces :: Parser ()
someSpaces =  some space *> pure ()

spaces :: Parser ()
spaces =  many space *> pure ()

peekChar :: Parser (Maybe Char)
peekChar =  listToMaybe <$> lift ReadP.look

peekSatisfy :: (Char -> Bool) -> Parser Char
peekSatisfy pre = do
  mc <- peekChar
  case mc of
    Just c | pre c      -> pure c
           | otherwise  -> empty
    Nothing             -> empty

symbolSep :: Parser ()
symbolSep =  peekSatisfy (`elem` ("()," ++ opCharset)) *> return () <|> someSpaces <|> eof

opSep :: Parser ()
opSep =  peekSatisfy (`elem` symbolCharset) *> return () <|> someSpaces <|> eof

lParen :: Parser Token
lParen =  char '(' *> pure LParen

rParen :: Parser Token
rParen =  char ')' *> pure RParen

comma :: Parser Token
comma =  char ',' *> pure Comma

placeholder :: Parser Token
placeholder =  char '?' *> pure PlaceHolder

eof :: Parser ()
eof =  lift ReadP.eof

token :: Parser Token
token =
  qualified             <|>
  table  <* symbolSep   <|>
  symbol <* symbolSep   <|>
  op     <* opSep       <|>
  string                <|>
  lParen                <|>
  rParen                <|>
  comma                 <|>
  placeholder

tokens :: Parser [Token]
tokens =  (many $ token <* spaces) <* eof

run' :: Parser a -> String -> Maybe (a, String)
run' p =
  (result <$>) . listToMaybe .
  readP_to_S (evalStateT p (QState { nextVar = 0, varMap = Map.empty }))  where
    result (a, in') = (a, in')

run :: String -> Maybe [Token]
run =  (fst <$>) . run' tokens


eq :: String -> String -> Bool
eq a b = fromMaybe False $ do
  x <- run a
  y <- run b
  return $ x == y

eqProp' :: String -> (a -> String) -> a -> String -> Test
eqProp' name t x est = prop' name (Just em) (t x `eq` est)
  where em = unlines [show $ run $ t x, " -- compares --", show $ run est]

eqProp :: Show a => String -> a -> String -> Test
eqProp name = eqProp' name show
