---
layout: default
title: Quick start
---

### Preparing HRR

To start using Haskell Relational Record (HRR), you need to install:

- Glasgow Haskell Compiler (GHC) + the "cabal" command
-- We recommend to use Haskell Platform
- The Haskell "relational-record" library
- Relational database system
-- In this quickstart, we assume that "sqlite3" has been installed

To install the Haskell "relational-record" library, type as follows:

    % cabal update
    % cabal install relational-record

### The first relation

Let's defined our first relation. Copy the following to "hello.hs":

```haskell
import Data.Int (Int32)
import Database.Record.Instances ()
import Database.Relational.Query

hello :: Relation () (Int32, String)
hello = relation $ return (value 0 >< value "Hello")

main :: IO ()
main = putStrLn $ show hello ++ ";"
```

'hello' defines the fist relation. This "SELECT"s a constant tuple value (0,"Hello") from the (virtual) empty table. In other words, this relation just returns (0,"Hello").

Let's run this Haskell code to show what kind of SQL statement is generated:

```shell
% runghc hello.hs
SELECT ALL 0 AS f0, 'Hello' AS f1;
```

OK. Next, let's execute this SQL in SQLite3:

```shell
% runghc hello.hs | sqlite3 dummy.db
0|Hello
```

We got "0|Hello"! Note that "dummy.db" is really a dummy file.

### Composing relations

```haskell
import Data.Int (Int32)
import Database.Record.Instances ()
import Database.Relational.Query

hello :: Relation () (Int32, String)
hello = relation $ return (value 0 >< value "Hello")

world :: Relation () (Int32, String)
world = relation $ return (value 0 >< value "World!")

helloWorld :: Relation () ((Int32, String), (Int32, String))
helloWorld = relation $ do
    h <- query hello
    w <- query world
    on $ h ! fst' .=. w ! fst'
    return $ h >< w

main :: IO ()
main = putStrLn $ show helloWorld ++ ";"
```



