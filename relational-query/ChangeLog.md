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
