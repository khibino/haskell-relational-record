/*
   % psql -f del.sql
 */

drop table transaction;
drop table account;
drop table officer;
drop table business;
drop table individual;
drop table customer;
drop table product;
drop table product_type;
drop table employee;
drop table branch;
drop table department;

drop type cust_type_cd_t;
drop type status_t;
drop type txn_type_cd_t;

