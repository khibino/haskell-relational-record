---
layout: default
title: Project site
---

### <a name="what-is-hrr"></a> What is HRR?

Haskell Relational Record (HRR) is a query generator based on typed relational algebra and correspondence between SQL value lists and Haskell record types, which provide programming interfaces to Relational DataBase Managemsnt Systems (RDBMS).

- Abstracted - relations are expressed as high level expressions and they are translated into SQL statements. Drivers are provided for DB2, PostgreSQL, SQLite, MySQL, Microsoft SQL Server and OracleSQL.
- Type safe - SQL statements produced by HRR are guaranteed to be valid if the Haskell code compiles. Even the types of placeholders are propagated.
- Composable - relations can be composed to build bigger relations.
- Automatic - SQL schema is obtained from a target DB and Haskell types are automatically generated at compile time.

### Documentation

- [Quick start](quickstart.html)
- [Tutorial](tutorial.html)
- [Examples](examples.html)
- [Manual](http://hackage.haskell.org/package/relational-record/docs/Database-Relational-Documentation.html)
- [Frequently Asked Questions](faq.html)
- [Technical information](techinfo.html)

### Questions

If you have any questions and/or suggestions, please submit [github issues](https://github.com/khibino/haskell-relational-record/issues).

### Authors and contributors

- [Kei Hibino](https://github.com/khibino), [Shohei Murayama](https://github.com/yuga), [Shohei Yasutake](https://github.com/amutake), [Sho Kuroda](https://github.com/krdlab) and [Kazu Yamamoto](https://github.com/kazu-yamamoto)
