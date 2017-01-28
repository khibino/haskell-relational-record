CREATE SCHEMA EXAMPLE3;

CREATE TABLE EXAMPLE3.set_a (
 seq  INTEGER NOT NULL,
 name VARCHAR(30) NOT NULL,

 PRIMARY KEY(seq)
);

CREATE TABLE EXAMPLE3.set_b (
 seq  INTEGER NOT NULL,
 name VARCHAR(30) NOT NULL,

 PRIMARY KEY(seq)
);

CREATE TABLE EXAMPLE3.history (
  seq INTEGER NOT NULL,
  register_time TIMESTAMP NOT NULL,
  log VARCHAR(30) NOT NULL,

  PRIMARY KEY(seq)
);

INSERT INTO EXAMPLE3.set_a (seq, name) VALUES (1, 'Apple');
INSERT INTO EXAMPLE3.set_a (seq, name) VALUES (2, 'Orange');
INSERT INTO EXAMPLE3.set_a (seq, name) VALUES (5, 'Banana');
INSERT INTO EXAMPLE3.set_a (seq, name) VALUES (6, 'Cherry');

INSERT INTO EXAMPLE3.set_b (seq, name) VALUES (2, 'Orange');
INSERT INTO EXAMPLE3.set_b (seq, name) VALUES (6, 'Cherry');
INSERT INTO EXAMPLE3.set_b (seq, name) VALUES (7, 'Melon');

INSERT INTO EXAMPLE3.history (seq, register_time, log) VALUES (1, '2013-03-05 17:44:02', 'start');
