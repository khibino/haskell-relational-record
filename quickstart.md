---
layout: default
title: Quick start
---

### Preparing HRR

Following installation methods are selectable

#### <a name="install-on-debian"></a> Debian sid

    % sudo apt-get install haskell-relational-record

#### <a name="install-on-opensuse"></a> OpenSUSE

    OpenSUSE Tumbleweed

    # zypper addrepo http://download.opensuse.org/repositories/devel:languages:haskell:lts:6/openSUSE_Tumbleweed/devel:languages:haskell:lts:6.repo
    # zypper refresh
    # zypper install ghc-relational-record

#### <a name="build-with-stack"></a> Build with stack

    % stack build relational-record

#### <a name="build-with-cabal"></a> Build with cabal

To start using [Haskell Relational Record](http://khibino.github.io/haskell-relational-record/) (HRR), you need to install:

1. [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) (GHC) + the "cabal" command
    - We recommend using the [Haskell Platform](https://www.haskell.org/platform/)
2. The Haskell ["relational-record"](http://hackage.haskell.org/package/relational-record) library

To install the Haskell "relational-record" library, run the following commands:

    % cabal update
    % cabal install relational-record

### Prepare Relational database system

Relational database system
    - In this quickstart, we assume that [SQLite](http://www.sqlite.org/) version 3 has been installed

### The first relation

In HRR, select statements are called relations.
Let's define our first relation. Copy the following to "hello.hs":

{% highlight haskell %}
import Data.Int (Int32)
import Database.Relational.Query

hello :: Relation () (Int32, String)
hello = relation $ return (value 0 >< value "Hello")

main :: IO ()
main = putStrLn $ show hello ++ ";"
{% endhighlight %}

`hello` defines our first relation. This "SELECT"s a constant tuple value (0,"Hello") from the (virtual) empty table. In other words, this relation just returns (0,"Hello").

Let's run the above Haskell code to show what kind of SQL statement is generated:

{% highlight sql %}
% runghc hello.hs
SELECT ALL 0 AS f0, 'Hello' AS f1;
{% endhighlight %}

OK. Next, let's execute the SQL produced above in SQLite:

    % runghc hello.hs | sqlite3 dummy.db
    0|Hello

We got "0\|Hello"! Note that "dummy.db" is just a dummy file.

### Composing relations

Next, let's compose relations. Copy the following to "helloworld.hs":

{% highlight haskell %}
import Data.Int (Int32)
import Database.Relational.Query

hello :: Relation () (Int32, String)
hello = relation $ return (value 0 >< value "Hello")

world :: Relation () (Int32, String)
world = relation $ return (value 0 >< value "World!")

helloWorld :: Relation () (Int32, (String, String))
helloWorld = relation $ do
    h <- query hello
    w <- query world
    on $ h ! fst' .=. w ! fst'
    return $ h ! fst' >< (h ! snd' >< w ! snd')

main :: IO ()
main = putStrLn $ show helloWorld ++ ";"
{% endhighlight %}

This code defines queries called `hello` and `world`. And `helloworld` composes them by joining them on the first element of the tuples.

This code generates the following SQL statement:

{% highlight sql %}
% runghc helloworld.hs
SELECT ALL T0.f0 AS f0, T0.f1 AS f1, T1.f1 AS f2 FROM (SELECT ALL 0 AS f0, 'Hello' AS f1) T0 INNER JOIN (SELECT ALL 0 AS f0, 'World!' AS f1) T1 ON (T0.f0 = T1.f0);
{% endhighlight %}

Finally, let's execute it in SQLite:

    % runghc helloworld.hs | sqlite3 dummy.db
    0|Hello|World!

Now we understand that relations are composable. Raw SQL does NOT have this feature. Moreover, relations are type safe. If our HRR code can be compiled by GHC, it always generates valid SQL statements.

The next step is to read the [HRR tutorial](tutorial.html).
