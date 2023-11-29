% Haskell Relational Record
  Composable, Typesafe SQL building
% Kei Hibino
% 2016-09-17

Concepts
-----

* SQL building DSL using Haskell
    * http://khibino.github.io/haskell-relational-record/
* Composability and Type-Safety
    * Want to find errors of SQL at compile time
* Compile time schema loading and generating type information

Intro
=====

Intro / Join - Set
-----

When building joined query:

~~~~~ {.sql}
SELECT ALL T0.name AS f0, T0.age AS f1, T0.family AS f2,
           T1.name AS f3, T1.day AS f4
      FROM EXAMPLE.person T0 INNER JOIN EXAMPLE.birthday T1
	ON (T0.name = T1.name)
~~~~~

Like the following set operation:

$$\{ (p, b) | p \in P, b \in B, \pi_{P.name}(p) = \pi_{B.name}(b) \}$$


Intro / Set operation and Haskell
-----

$$\{ (p, b) | p \in P, b \in B, \pi_{P.name}(p) = \pi_{B.name}(b) \}$$

* ` <- ` is analogy of $\in$

~~~~~ {.haskell}
   [ (p, b)
   | p <- person, b <- birthday , P.name p == B.name b ]
   -- List Comprehension

do { p <- person; b <- birthday; guard (P.name p == B.name b)
   ; return (p, b) }  -- List Monad
~~~~~

The same meanings

Intro / DSL using Haskell!
-----

~~~~~ {.haskell}
  do { p <- person; b <- birthday; guard (P.name p == B.name b)
     ; return (p, b) } -- List Monad
~~~~~

Building a joined query like list monad:

~~~~~ {.haskell}
personAndBirthday :: Relation () (Person, Birthday)
personAndBirthday =  relation $ do
  p <- query person
  b <- query birthday  -- Join product accumulated
  on $ p ! Person.name' .=. b ! Birthday.name'
  return $ p >< b
~~~~~

Intro / Built joined query
-----

~~~~~ {.haskell}
personAndBirthday :: Relation () (Person, Birthday)
personAndBirthday =  relation $ do
  p <- query person
  b <- query birthday  -- Join product accumulated
  on $ p ! Person.name' .=. b ! Birthday.name'
  return $ p >< b
~~~~~

~~~~~ {.sql}
SELECT ALL T0.name AS f0, T0.age AS f1, T0.family AS f2,
           T1.name AS f3, T1.day AS f4
      FROM EXAMPLE.person T0 INNER JOIN EXAMPLE.birthday T1
        ON (T0.name = T1.name)
~~~~~

By examples
=====

By examples / Left outer join
-----

~~~~~ {.haskell}
personAndBirthdayL :: Relation () (Person, Maybe Birthday)
personAndBirthdayL =  relation $ do
  p <- query person
  b <- queryMaybe birthday
  on $ just (p ! Person.name') .=. b ?! Birthday.name'
  return $ p >< b
~~~~~

generates left-joined SQL:

~~~~~ {.sql}
SELECT ALL T0.name AS f0, T0.age AS f1, T0.family AS f2,
           T1.name AS f3, T1.day AS f4
      FROM EXAMPLE.person T0 LEFT JOIN EXAMPLE.birthday T1
        ON (T0.name = T1.name)
~~~~~

By examples / Aggregation
-----

~~~~~ {.haskell}
agesOfFamilies :: Relation () (String, Maybe Int32)
agesOfFamilies =  aggregateRelation $ do
  p <- query person
  gFam <- groupBy $ p ! Person.family'    -- Specify grouping key
  return $ gFam >< sum' (p ! Person.age') -- Aggregated results
~~~~~

sums ages per family.

Generated SQL:

~~~~~ {.sql}
SELECT ALL T0.family AS f0, SUM(T0.age) AS f1
      FROM EXAMPLE.person T0
  GROUP BY T0.family
~~~~~

By examples / Restriction
-----

~~~~~ {.haskell}
sameBirthdayHeisei' :: Relation () (Day, Int64)
sameBirthdayHeisei' =  aggregateRelation $ do
  p <- query person
  b <- query birthday
  on $ p ! Person.name' .=. b ! Birthday.name'
  wheres $
    b ! Birthday.day' .>=. value (fromGregorian 1989 1 8)
  gbd <- groupBy $ b ! Birthday.day'
  having $ count (p ! Person.name') .>. value (1 :: Int64)
  return $ gbd >< count (p ! Person.name')
~~~~~

counts people with the same birthday, who were born in the Heisei period.

Generated SQL:

~~~~~ {.sql}
SELECT ALL T1.day AS f0, COUNT(T0.name) AS f1
      FROM EXAMPLE.person T0 INNER JOIN EXAMPLE.birthday T1
        ON (T0.name = T1.name)
     WHERE (T1.day >= DATE '1989-01-08')
  GROUP BY T1.day
    HAVING (COUNT(T0.name) > 1)
~~~~~

By examples / Restriction - let
-----

~~~~~ {.haskell}
sameBirthdayHeisei :: Relation () (Day, Int64)
sameBirthdayHeisei =  aggregateRelation $ do
  p <- query person
  b <- query birthday
  on $ p ! Person.name' .=. b ! Birthday.name'
  let birthDay = b ! Birthday.day'
  wheres $ birthDay .>=. value (fromGregorian 1989 1 8)
  gbd <- groupBy birthDay
  let personCount = count $ p ! Person.name'
  having $ personCount .>. value 1
  return $ gbd >< personCount
~~~~~

By examples / Ordering
-----

~~~~~ {.haskell}
personAndBirthdayO :: Relation () (Person, Birthday)
personAndBirthdayO =  relation $ do
  p <- query person
  b <- query birthday
  on $ p ! Person.name' .=. b ! Birthday.name'
  orderBy (b ! Birthday.day') Asc  -- Specify ordering key
  orderBy (p ! Person.name') Asc
  return $ p >< b
~~~~~

orders by birthday and then name:

~~~~~ {.sql}
SELECT ALL T0.name AS f0, T0.age AS f1, T0.family AS f2,
           T1.name AS f3, T1.day AS f4
      FROM EXAMPLE.person T0 INNER JOIN EXAMPLE.birthday T1
        ON (T0.name = T1.name)
  ORDER BY T1.day ASC, T0.name ASC
~~~~~

By examples / Placeholders
-----

~~~~~ {.haskell}
specifyPerson :: Relation String (Person, Birthday)
specifyPerson =  relation' $ do
  pb <- query personAndBirthday -- Re-use predefined
  (ph, ()) <- placeholder
              (\ph' -> wheres $ pb ! fst' ! Person.name' .=. ph')
  return (ph, pb)
~~~~~

specifies a person name using a placeholder:

~~~~~ {.sql}
SELECT ALL T2.f0 AS f0, T2.f1 AS f1, T2.f2 AS f2,
           T2.f3 AS f3, T2.f4 AS f4
  FROM (SELECT ALL T0.name AS f0, T0.age AS f1, T0.family AS f2,
	           T1.name AS f3, T1.day AS f4
              FROM EXAMPLE.person T0 INNER JOIN
                   EXAMPLE.birthday T1
                ON (T0.name = T1.name)) T2
 WHERE (T2.f0 = ?)
~~~~~

By examples / Window function
-----

Building windows:

~~~~~ {.haskell}
ageRankOfFamilies :: Relation () ((Int64, String), Int32)
ageRankOfFamilies =  relation $ do
  my <- query myTable
  return $
    rank `over` do
      partitionBy $ my ! family'  -- Monad to build window
      orderBy (my ! age') Desc
    ><
    my ! family' >< my ! age'
~~~~~

age ranking per family:

~~~~~ {.sql}
SELECT ALL
       RANK() OVER (PARTITION BY T0.family
                    ORDER BY T0.age DESC) AS f0,
       T0.family AS f1, T0.age AS f2
  FROM PUBLIC.my_table T0
~~~~~

Demo
-----

* expansion of Template Haskell

* aggregation type check


Conclusion
-----

* Composable
    * Haskell expressions in HRR DSL bound to variables are reusable.
* Type Safety
    * Statically type checking makes composition of small expressions safer.

Question
-----


Structure
=====

Structure / Concepts
-----

A simple and useful method:

* Untype and accumulate from typeful DSL terms into a state monad context
* Typeful result (Phantom context and Phantom result type)

Structure / Monad Stack
-----

Relational Record's query-building DSL accumulates various context in a state or writer monad context stack.

~~~~~ {.sql}
  SELECT ...
    FROM ... -- State , join product tree
   WHERE ... -- Writer, restrictions terms monoid
GROUP BY ... -- Writer, aggregate terms monoid
  HAVING ... -- Writer, restrictions terms monoid
ORDER BY ... -- Writer, ordering key and spec monoid
~~~~~

Structure / Join Product
-----

~~~~~ {.haskell}
query :: (MonadQualify ConfigureQuery m, MonadQuery m)
      => Relation () r
      -> m (Projection Flat r)

-- Used for outer join
queryMaybe :: (MonadQualify ConfigureQuery m, MonadQuery m)
           => Relation () r
           -> m (Projection Flat (Maybe r))

on :: MonadQuery m => Projection Flat (Maybe Bool) -> m ()
~~~~~

'query' and 'queryMaybe' return a record Projection result corresponding table forms.

~~~~~ {.sql}
SELECT .. FROM ...
            -- Accumulating uniquely qualified
            -- ( like 'as T0', 'as T1' ... )
            -- table forms of SQL FROM clause
~~~~~

Structure / Aggregation
-----

~~~~~ {.haskell}
groupBy :: MonadAggregate m
        => Projection Flat r
        -- ^ Projection to add into group by
        -> m (Projection Aggregated r)
        -- ^ Result context and aggregated projection

count :: Projection Flat a -> Projection Aggregated Int64
max'  :: Ord a
      => Projection Flat a -> Projection Aggregated (Maybe a)
~~~~~

'groupBy' can be used under only 'MonadAggregate' monad constraint,
stronger than 'MonadQuery'.

'groupBy' returns a Projection value with an Aggregated context type:

~~~~~ {.sql}
SELECT .. GROUP BY ...
                -- Accumulating keys
                -- of SQL GROUP BY clause
~~~~~

Structure / Restriction / WHERE
-----

~~~~~ {.haskell}
restrict :: MonadRestrict c m
         => Projection c (Maybe Bool)
         -> m ()

wheres :: MonadRestrict Flat m
       => Projection Flat (Maybe Bool)
       -> m ()
~~~~~

adds a WHERE clause restriction:

~~~~~ {.sql}
SELECT .. WHERE x AND y AND ...
             -- Accumulating AND predicates
             -- of SQL WHERE clause
~~~~~

Structure / Restriction / HAVING
-----

~~~~~ {.haskell}
restrict :: MonadRestrict c m
         => Projection c (Maybe Bool)
         -> m ()

having :: MonadRestrict Aggregated m
       => Projection Aggregated (Maybe Bool)
       -> m ()
~~~~~
