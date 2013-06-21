#! /bin/sh

create_user_table='
CREATE TABLE SAMPLE1.user (
 id integer NOT NULL,
 name VARCHAR(128),

 PRIMARY KEY(id)
)
'

create_group_table='
CREATE TABLE SAMPLE1.group (
 id integer NOT NULL,
 name VARCHAR(128),

 PRIMARY KEY(id)
)
'

create_membership_table='
CREATE TABLE SAMPLE1.membership (
 user_id integer NOT NULL,
 group_id integer NOT NULL
)
'

set -x

psql -c "CREATE SCHEMA SAMPLE1" testdb
psql -c "$create_user_table" testdb
psql -c "$create_group_table" testdb
psql -c "$create_membership_table" testdb

insertUser () {
	psql -c "INSERT INTO SAMPLE1.user (id, name) VALUES ($1, '$2')" testdb
}

insertGroup () {
	psql -c "INSERT INTO SAMPLE1.group (id, name) VALUES ($1, '$2')" testdb
}

insertMembership() {
	psql -c "INSERT INTO SAMPLE1.membership (user_id, group_id) VALUES ($1, $2)" testdb
}

insertUser 1 'Kei Hibino'
insertUser 2 'Kazu Yamamoto'
insertUser 3 'New-comer'

insertGroup 1 'Haskell'
insertGroup 2 'C++'
insertGroup 3 'Java'

insertMembership 1 1
insertMembership 2 1
