% Composable, Typesafe Query building
% 2015-12-12
% Kei Hibino

今日の発表/Agenda
-----

* 静的型付けの関数型言語である Haskell を使って SQL を組み立てる DSL を作りました
    * http://khibino.github.io/haskell-relational-record/
* DSL のアピールポイントは 合成可能性 と 型安全性 です
* コンパイル時に PostgreSQL のシステムカタログからテーブルスキーマを読み取って、型を自動生成します。

※ 発表には Haskell のコードが出てきますが、見た目の気分を感じとるぐらいで十分です。
対応するSQLも見せるので意味がわかると思います。

<!--
* SQL building DSL using Haskell
    * http://khibino.github.io/haskell-relational-record/
* The appeal point of this DSL is composability and type-safety
* Compile time schema loading and generating type informations
 -->

結合式の例/Joined Query building
-----

結合式を組み立てるとき:

When building joined query:

~~~~~ {.sql}
SELECT ALL T0.name AS f0, T0.age AS f1, T0.family AS f2,
           T1.name AS f3, T1.day AS f4
      FROM EXAMPLE.person T0 INNER JOIN EXAMPLE.birthday T1
	ON (T0.name = T1.name)
~~~~~

<!-- 以下のような集合の演算を考えているはず ... -->

$$\{ (p, b) | p \in P, b \in B, \pi_{P.name}(p) = \pi_{B.name}(b) \}$$


集合演算とHaskell/Set operation and Haskell
-----

$$\{ (p, b) | p \in P, b \in B, \pi_{P.name}(p) = \pi_{B.name}(b) \}$$

※ <- と $\in$

~~~~~ {.haskell}
   [ (p, b)
   | p <- person, b <- birthday , P.name p == B.name b ]
   -- 内包表記/Comprehension

do { p <- person; b <- birthday; guard (P.name p == B.name b)
   ; return (p, b) }  -- List Monad
~~~~~

どちらの記法でも同じの意味/The same meanings

HaskellでDSL/DSL using Haskell!
-----

~~~~~ {.haskell}
  do { p <- person; b <- birthday; guard (P.name p == B.name b)
     ; return (p, b) } -- List Monad
~~~~~

結合式をリスト内包表記あるいはList Monad のように組み立てる:

Building a joined query like list comprehension or list monad:

~~~~~ {.haskell}
personAndBirthday :: Relation () (Person, Birthday)
personAndBirthday =  relation $ do
  p <- query person
  b <- query birthday  -- 結合積の蓄積
                       -- Join product accumulated
  on $ p ! Person.name' .=. b ! Birthday.name'
  return $ p >< b
~~~~~

組み上がった結合式/Built joined query
-----

~~~~~ {.haskell}
personAndBirthday :: Relation () (Person, Birthday)
personAndBirthday =  relation $ do
  p <- query person
  b <- query birthday  -- 結合積の集積
                       -- Join product accumulated
  on $ p ! Person.name' .=. b ! Birthday.name'
  return $ p >< b
~~~~~

~~~~~ {.sql}
SELECT ALL T0.name AS f0, T0.age AS f1, T0.family AS f2,
           T1.name AS f3, T1.day AS f4
      FROM EXAMPLE.person T0 INNER JOIN EXAMPLE.birthday T1
        ON (T0.name = T1.name)
~~~~~

例 - 外部左結合/Left outer join example
-----

~~~~~ {.haskell}
personAndBirthdayL :: Relation () (Person, Maybe Birthday)
personAndBirthdayL =  relation $ do
  p <- query person
  b <- queryMaybe birthday
  on $ just (p ! Person.name') .=. b ?! Birthday.name'
  return $ p >< b
~~~~~

~~~~~ {.sql}
SELECT ALL T0.name AS f0, T0.age AS f1, T0.family AS f2,
           T1.name AS f3, T1.day AS f4
      FROM EXAMPLE.person T0 LEFT JOIN EXAMPLE.birthday T1
        ON (T0.name = T1.name)
~~~~~

例 - 集約/Aggregation example
-----

~~~~~ {.haskell}
agesOfFamilies :: Relation () (String, Maybe Int32)
agesOfFamilies =  aggregateRelation $ do
  p <- query person
  gFam <- groupBy $ p ! Person.family'    -- Specify grouping key
  return $ gFam >< sum' (p ! Person.age') -- Aggregated results
~~~~~

~~~~~ {.sql}
SELECT ALL T0.family AS f0, SUM(T0.age) AS f1
      FROM EXAMPLE.person T0
  GROUP BY T0.family
~~~~~

例 - 絞り込み/Restriction example
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

平成生まれで誕生日が同じ人を数える

counts people with the same birthday, who were born in the Heisei period.

~~~~~ {.sql}
SELECT ALL T1.day AS f0, COUNT(T0.name) AS f1
      FROM EXAMPLE.person T0 INNER JOIN EXAMPLE.birthday T1
        ON (T0.name = T1.name)
     WHERE (T1.day >= DATE '1989-01-08')
  GROUP BY T1.day
    HAVING (COUNT(T0.name) > 1)
~~~~~

例 - 絞り込み/Restriction example
-----

~~~~~ {.sql}
SELECT ALL T1.day AS f0, COUNT(T0.name) AS f1
      FROM EXAMPLE.person T0 INNER JOIN EXAMPLE.birthday T1
        ON (T0.name = T1.name)
     WHERE (T1.day >= DATE '1989-01-08')
  GROUP BY T1.day
    HAVING (COUNT(T0.name) > 1)
~~~~~

例 - 絞り込み/Restriction example
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

例 - 順序付け/Ordering example
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

例 - 順序付け/Ordering example
-----

~~~~~ {.sql}
SELECT ALL T0.name AS f0, T0.age AS f1, T0.family AS f2,
           T1.name AS f3, T1.day AS f4
      FROM EXAMPLE.person T0 INNER JOIN EXAMPLE.birthday T1
        ON (T0.name = T1.name)
  ORDER BY T1.day ASC, T0.name ASC
~~~~~

例 - プレースホルダー/Placeholders example
-----

~~~~~ {.haskell}
specifyPerson :: Relation String (Person, Birthday)
specifyPerson =  relation' $ do
  pb <- query personAndBirthday -- Re-use predefined
  (ph, ()) <- placeholder
              (\ph' -> wheres $ pb ! fst' ! Person.name' .=. ph')
  return (ph, pb)
~~~~~

名前をプレースホルダーで指定する:

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

例 - プレースホルダー/Placeholders example
-----

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


例 - ウィンドウ関数/Window function example
-----

ウィンドウを組み立てる:

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

~~~~~ {.sql}
SELECT ALL
       RANK() OVER (PARTITION BY T0.family
                    ORDER BY T0.age DESC) AS f0,
       T0.family AS f1, T0.age AS f2
  FROM PUBLIC.my_table T0
~~~~~

例 - ウィンドウ関数/Window function example
-----

~~~~~ {.sql}
SELECT ALL
       RANK() OVER (PARTITION BY T0.family
                    ORDER BY T0.age DESC) AS f0,
       T0.family AS f1, T0.age AS f2
  FROM PUBLIC.my_table T0
~~~~~

合成可能/Composable
-----

* すべてが Haskell の式なので、変数に束縛して再利用できる
* ORDER BY 節や WHERE 節も関数の式で部品化可能
* 静的型検査を行うので、細かい部品を積極的に合成しても安全


デモ
-----


型安全/Type safety
-----

静的型付けされた式を組み立てる演算子

Operators which builds statically typed expressions

~~~~~ {.haskell}
query :: (MonadQualify ConfigureQuery m, MonadQuery m)
      => Relation () r
      -> m (Projection Flat r)
queryMaybe :: (MonadQualify ConfigureQuery m, MonadQuery m)
           => Relation () r
           -> m (Projection Flat (Maybe r))
on :: MonadQuery m => Projection Flat (Maybe Bool) -> m ()
~~~~~

型安全/Type safety
-----

静的型付けされた式を組み立てる演算子

Operators which builds statically typed expressions

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

型安全/Type safety
-----

静的型付けされた式を組み立てる演算子

Operators which builds statically typed expressions

~~~~~ {.haskell}
restrict :: MonadRestrict c m
         => Projection c (Maybe Bool)
         -> m ()
wheres :: MonadRestrict Flat m
       => Projection Flat (Maybe Bool)
       -> m ()
having :: MonadRestrict Aggregated m
       => Projection Aggregated (Maybe Bool)
       -> m ()
~~~~~

型安全/Type safety
-----

静的型付けされた式を組み立てる演算子

Operators which builds statically typed expressions

~~~~~ {.haskell}
orderBy :: Monad m
        => Projection c t
        -- ^ Ordering terms to add
        -> Order
        -- ^ Order direction -- Asc | Desc
        -> Orderings c m ()
        -- ^ Result context with ordering
~~~~~

質疑応答/Discussion
-----
