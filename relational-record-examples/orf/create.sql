/*
   % sqlite3 examples.db < create.sql
*/

/* begin table creation */

create table person
 (id integer primary key autoincrement not null,
  name varchar(20) not null
 );

create table product
 (id integer primary key autoincrement not null,
  name varchar(20) not null
 );
