
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
