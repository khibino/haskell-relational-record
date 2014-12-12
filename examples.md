---
layout: default
title: Examples
---

### Preparing

We assume that you have read both [quick start](quickstart.html) and [tutorial](tutorial.htm).

### Schema of examples

We use the bank example in [Learning SQL](http://shop.oreilly.com/product/9780596007270.do). Its support page provides a script to create the tables of the bank examples for MySQL. We modified [it for SQLite](https://github.com/khibino/haskell-relational-record/blob/master/relational-record-examples/sql/add.sql) and created a DB file called "examples.db" in the top directory of "relational-record-examples". We deeply thank Alan Beaulieu, the author of "Learning SQL".

Here is a list of tables copied from page 34 of "Learning SQL":

- Account -- a particular product opened for a particular customer
- Business -- a corporate customer (subtype of the Customer table)
- Customer -- a person or corporation known to the bank
- Department -- a group of bank employees implementing a particular banking function
- Employee -- a person working for the bank
- Individual -- a noncorporate customer (subtype of the Customer table)
- Officer -- a person allowed to transact business for a corporate customer
- Product -- a banking function offered to customers
- Product_type -- a group of products having similar function
- Transaction -- a change made to an account balance

### Queries

TBD
