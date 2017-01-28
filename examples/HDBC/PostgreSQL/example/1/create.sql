CREATE SCHEMA EXAMPLE1;

CREATE TABLE EXAMPLE1.user (
 id integer NOT NULL,
 name VARCHAR(128),

 PRIMARY KEY(id)
);

CREATE TABLE EXAMPLE1.group (
 id integer NOT NULL,
 name VARCHAR(128),

 PRIMARY KEY(id)
);

CREATE TABLE EXAMPLE1.membership (
 user_id integer NOT NULL,
 group_id integer NOT NULL
);

INSERT INTO EXAMPLE1.user (id, name) VALUES (1, 'Kei Hibino');
INSERT INTO EXAMPLE1.user (id, name) VALUES (2, 'Kazu Yamamoto');
INSERT INTO EXAMPLE1.user (id, name) VALUES (3, 'Shouhei Murayama');
INSERT INTO EXAMPLE1.user (id, name) VALUES (255, '<New-comer>');

INSERT INTO EXAMPLE1.group (id, name) VALUES (1, 'Haskell');
INSERT INTO EXAMPLE1.group (id, name) VALUES (2, 'C++');
INSERT INTO EXAMPLE1.group (id, name) VALUES (3, 'Java');

INSERT INTO EXAMPLE1.membership (user_id, group_id) VALUES (1, 1);
INSERT INTO EXAMPLE1.membership (user_id, group_id) VALUES (2, 1);
INSERT INTO EXAMPLE1.membership (user_id, group_id) VALUES (3, 1);

INSERT INTO EXAMPLE1.membership (user_id, group_id) VALUES (1, 3);
INSERT INTO EXAMPLE1.membership (user_id, group_id) VALUES (3, 3);
