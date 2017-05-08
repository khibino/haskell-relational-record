<!-- -*- Markdown -*- -->

## 0.9.1.0

- Fix of unsafeValueNull. ( https://github.com/khibino/haskell-relational-record/issues/55 )

## 0.9.0.2

- Bugfix of case projected record. ( https://github.com/khibino/haskell-relational-record/issues/54 )

## 0.9.0.1

- Use Haskell implementation test instead of flag test in .cabal

## 0.9.0.0

- Add HRR instances of tuple types derived by generic programming.
- Add generic instances of ShowConstantTermsSQL.

## 0.8.3.6

- Bugfix of lazy instances of ShowConstantTermsSQL.

## 0.8.3.5

- Deprecate some exported interfaces which are internal definitions.

## 0.8.3.4

- Update this changelog

## 0.8.3.3

- simpl-tick-factor work-around to avoid bug of GHC

## 0.8.3.2

- Export Register interface type from Query module.

## 0.8.3.1

- Refactor around sub-query and its builder.
- Compatibility with dlist-0.5.

## 0.8.3.0

- Add configuration to quote SQL string of table names.

## 0.8.2.3

- Add tested-with meta-data.

## 0.8.2.2

- Update for GHC 8.

## 0.8.2.1

- Fix constraint of build-depends.

## 0.8.2.0

- Add TIMESTAMPTZ literal of PostgreSQL.

## 0.8.1.0

- Add schemaNameMode configuration.

## 0.8.0.5

- Update tests along with deprecations.

## 0.8.0.4

- Drop unused pragma.

## 0.8.0.3

- Drop unreferenced overloading.

## 0.8.0.2

- Update documentation.

## 0.8.0.1

- Fix build-depends.

## 0.8.0.0

- Drop unsafe Expr type.
- Drop redundant type synonyms around DELETE and UPDATE.
- Add Register monad to add build-able INSERT statement
- Configurable relation template names.

## 0.7.1.0

- Deprecate redundant type synonyms.

## 0.7.0.2

- Prepare to drop Expr type and deprecate around it.
- Fix boolean projection operator types.

## 0.7.0.1

- Update this changelog.

## 0.7.0.0

- Use TH quotations for deriving class symbols.

## 0.6.4.0

- Fix around correlated sub-queries.
- Update unit-test cases.

## 0.6.3.0

- Add Int8 type as SQL constant int value. (e.g. MySQL)

## 0.6.2.0

- Make InsertQuery type as PreparedNoFetch instance.

## 0.6.1.0

- Add a configuration flag to pring verbose compile-time messages.

## 0.6.0.0

- Increase type safety of interfaces.
- Simplify interfaces arond unique query.

## 0.5.2.0

- Add MonadTrans instance of QueryJoin.
- Update links about Opaleye.

## 0.5.1.1

- Update documentation.

## 0.5.1.0

- Add the arrow combinator module and its unit-test cases.

## 0.5.0.3

- Update unit-test cases.
- Add the fixity of `over` operator.
- Avoid an `a future Prelude name' warning.

## 0.5.0.2

- Switch libraries to use from test-suites not to depend on Cabal library.

## 0.5.0.1

- Add this ChangeLog file.

## 0.5.0.0

- Prevent window function context expression from using normal SQL expressions.
- Generalize the result types of aggregate and window functions.
- Allow to embed a integer literal in SQL from Haskell Int type.
- Add SQL LIKE operators.
- Drop old deprecated functions. (fromMaybe', dense_rank, ...)
- Fix typo.
  https://github.com/khibino/haskell-relational-record/pull/15
- Fix for "invalid single-column insert syntax".
  https://github.com/khibino/haskell-relational-record/issues/16

## 0.4.0.0

- Extend derivedInsert.

## 0.3.0.0

- Add generalized restrict.
- Pass configuration to DELETE and UPDATE.

## 0.2.0.0

- Update structure of query with placeholders.
