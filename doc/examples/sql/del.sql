/*
   % psql -f del.sql testdb
   % dropdb testdb
 */

drop table LEARNINGSQL.transaction;
drop table LEARNINGSQL.account;
drop table LEARNINGSQL.officer;
drop table LEARNINGSQL.business;
drop table LEARNINGSQL.individual;
drop table LEARNINGSQL.customer;
drop table LEARNINGSQL.product;
drop table LEARNINGSQL.product_type;
drop table LEARNINGSQL.employee;
drop table LEARNINGSQL.branch;
drop table LEARNINGSQL.department;

drop type cust_type_cd_t;
drop type status_t;
drop type txn_type_cd_t;

drop schema LEARNINGSQL;


