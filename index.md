---
layout: default
title: Project top page
---

### What is HRR?

Haskell Relational Record (HRR) is a joined query generator based on typefull relational algebra and a mapper between SQL value list and Haskell record type.

- Abstracted - relations are expressed in high level expressions and they are translated into SQL statements. Drivers are provided for DB2, ProsgreSQL, SQLite, MySQL, MicroSoft SQL Server and OracleSQL.
- Type safe - if HRR code in Haskell is compiled, it is ensured that valid SQL statements are generated. Even placeholders are typed.
- Composable - relations can be composed to build a bigger relation.
- Automatic - SQL schema is obtained from a target DB and Haskell recode types are automatically generated at compile time.

### Documentations

- [Quick start](quickstart.html)
- [Tutorial](tutorial.html)
- [Examples](examples.html)
- [Manual](http://hackage.haskell.org/package/relational-record/docs/Database-Relational-Query-Documentation.html)
- [Technical information](techinfo.html)

### Authors and contributors

- [Kei Hibino](https://github.com/khibino), [Shohei Murayama](https://github.com/yuga), [Shohei Yasutake](https://github.com/amutake), [Sho Kuroda](https://github.com/krdlab) and [Kazu Yamamoto](https://github.com/kazu-yamamoto)
