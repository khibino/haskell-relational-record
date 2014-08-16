CREATE DATABASE IF NOT EXISTS TEST DEFAULT CHARACTER SET UTF8;

DROP TABLE IF EXISTS TEST.test_pk1;
CREATE TABLE TEST.test_pk1 (a INT, b VARCHAR(32) NOT NULL, PRIMARY KEY (a));
DROP TABLE IF EXISTS TEST.test_pk2;
CREATE TABLE TEST.test_pk2 (a INT, b INT, c VARCHAR(32) NOT NULL, PRIMARY KEY (a, b));

DROP TABLE IF EXISTS TEST.user;
CREATE TABLE TEST.user (
      id            BIGINT PRIMARY KEY
    , name          VARCHAR(32) NOT NULL
    , email         VARCHAR(255) NOT NULL UNIQUE
    , passwd_hash   VARCHAR(512) NOT NULL
    , completed     TINYINT(1) NOT NULL DEFAULT 0
    , deleted       TINYINT(1) NOT NULL DEFAULT 0
    , frozen        TINYINT(1) NOT NULL DEFAULT 0
    , memo          TEXT NOT NULL
    , created_at    DATE NOT NULL
    , updated_at    DATE NOT NULL
);

INSERT INTO TEST.user
    (id, name, email, passwd_hash, completed, deleted, frozen, memo, created_at, updated_at)
    VALUES
    (1, 'krdlab', 'krdlab@gmail.com', 'dummy hashed password 1', 1, 0, 0, '', '2014-02-01', '2014-02-01'),
    (2, 'foo',    'foo@example.com',  'dummy hashed password 2', 0, 0, 0, '', '2014-02-10', '2014-02-10'),
    (3, 'bar',    'bar@example.com',  'dummy hashed password 3', 1, 0, 1, 'limit exceeded', '2014-02-11', '2014-02-20')
    ;

GRANT ALL PRIVILEGES ON TEST.* TO 'hrr-tester'@'127.0.0.1';
