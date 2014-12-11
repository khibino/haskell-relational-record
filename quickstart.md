---
layout: default
title: Quick start
---

### Preparing HRR

To start using [Haskell Relational Record](http://khibino.github.io/haskell-relational-record/) (HRR), you need to install:

1. [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) (GHC) + the "cabal" command
    - We recommend to use [Haskell Platform](https://www.haskell.org/platform/)
2. The Haskell ["relational-record"](http://hackage.haskell.org/package/relational-record) library
3. Relational database system
    - In this quickstart, we assume that [SQLite](http://www.sqlite.org/) has been installed

To install the Haskell "relational-record" library, type as follows:

    % cabal update
    % cabal install relational-record

### The first relation

Let's defined our first relation. Copy the following to "hello.hs":

    import Data.Int (Int32)
    import Database.Record.Instances ()
    import Database.Relational.Query
    
    hello :: Relation () (Int32, String)
    hello = relation $ return (value 0 >< value "Hello")
    
    main :: IO ()
    main = putStrLn $ show hello ++ ";"

'hello' defines the fist relation. This "SELECT"s a constant tuple value (0,"Hello") from the (virtual) empty table. In other words, this relation just returns (0,"Hello").

Let's run this Haskell code to show what kind of SQL statement is generated:

    % runghc hello.hs
    SELECT ALL 0 AS f0, 'Hello' AS f1;

OK. Next, let's execute this SQL in SQLite:

    % runghc hello.hs | sqlite3 dummy.db
    0|Hello

We got "0\|Hello"! Note that "dummy.db" is really a dummy file.

### Composing relations

Next, let's compose relations. Copy the following to "helloworld.hs":

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

This code defines queries called 'hello' and 'world'. And 'helloworld' composes them by joining them on the first element of the tuples.

This code generates the following SQL statement:

    % runghc helloworld.hs
    SELECT ALL T0.f0 AS f0, T0.f1 AS f1, T1.f0 AS f2, T1.f1 AS f3 FROM (SELECT ALL 0 AS f0, 'Hello' AS f1) T0 INNER JOIN (SELECT ALL 0 AS f0, 'World!' AS f1) T1 ON (T0.f0 = T1.f0);

Finally, let's execute it in SQLite:

    % runghc helloworld.hs | sqlite3 dummy.db
    0|Hello|0|World!
