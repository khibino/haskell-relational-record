---
layout: default
title: Project top page
---

### What is HRR?

Haskell Relational Record (HRR) is a query generator based on typed relational algebra and a mapper between SQL value lists and Haskell record types.

- Abstracted - relations are expressed as high level expressions and they are translated into SQL statements. Drivers are provided for DB2, ProsgreSQL, SQLite, MySQL, MicroSoft SQL Server and OracleSQL.
- Type safe - SQL statements produced by HRR are guaranteed to be valid if the Haskell code compiles. Even placeholders are typed.
- Composable - relations can be composed to build bigger relations.
- Automatic - SQL schema is obtained from a target DB and Haskell types are automatically generated at compile time.

### Documentations

- [Quick start](quickstart.html)
- [Tutorial](tutorial.html)
- [Examples](examples.html)
- [Manual](http://hackage.haskell.org/package/relational-record/docs/Database-Relational-Query-Documentation.html)
- [Technical information](techinfo.html)

### Questions

If you have any questions and/or suggestions, please submit [github issues](https://github.com/khibino/haskell-relational-record/issues).

### Authors and contributors

- [Kei Hibino](https://github.com/khibino), [Shohei Murayama](https://github.com/yuga), [Shohei Yasutake](https://github.com/amutake), [Sho Kuroda](https://github.com/krdlab) and [Kazu Yamamoto](https://github.com/kazu-yamamoto)
