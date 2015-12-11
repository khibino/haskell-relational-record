
CREATE SCHEMA IF NOT EXISTS EXAMPLE;

CREATE TABLE EXAMPLE.person
  ( name    VARCHAR(64)   NOT NULL
  , age     INTEGER       NOT NULL
  , family  VARCHAR(64)   NOT NULL

  , PRIMARY KEY(name)
  );

CREATE TABLE EXAMPLE.birthday
  ( name     VARCHAR(64)  NOT NULL
  , day      DATE         NOT NULL

  , PRIMARY KEY(name)
  );
