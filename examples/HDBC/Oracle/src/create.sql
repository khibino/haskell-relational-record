/* http://otndnld.oracle.co.jp/document/products/oracle11g/111/doc_dvd/server.111/E05765-03/datatype.htm */
create table hrr_datatype_test (
  char_10 char(10) not null,             -- 10文字までの固定長文字列(あまりは空白が入る)
  char_1 char(1) not null,               -- 1文字までの固定長文字列
  varchar2_10 varchar2(10) not null,     -- 10文字までの可変長文字列(あまってもなにも入らない)
  varchar_10 varchar(10) not null,       -- 同上(非推奨)
  nchar_10 nchar(10) not null,           -- 10文字までのUnicode固定長文字列
  nvarchar2_10 nvarchar2(10) not null,   -- 10文字までのUnicode可変長文字列
  -- nvarchar_10 nvarchar(10) not null,     -- 同上(非推奨)(動かない)
  -- long_raw long not null,                -- 2GBまでの可変長文字列(非推奨)
  number_raw number not null,            -- (ほぼ)任意の数値
  number_ast_1 number(*, 1) not null,    -- 任意桁、小数点以下1桁
  number_9 number(9) not null,           -- 9桁の整数
  number_9_2 number(9, 2) not null,      -- 9桁、小数点以下2桁
  number_9_1 number(9, -2) not null,     -- 9桁、下二桁を丸めた整数
  binary_float binary_float not null,    -- 32bit 浮動小数点数
  binary_double binary_double not null,  -- 64bit 倍精度浮動小数点数
  number_2_2 number(2, 2) not null,      -- issue#6
  date_raw date not null,                -- 年（世紀を含む）、月、日、時、分および秒（真夜中から数える）
  blob_raw blob not null,                -- 128TBまでのバイナリデータ
  clob_raw clob not null,                -- 128TBまでの可変長文字列
  nclob_raw nclob not null,              -- 128TBまでのUnicode可変長文字列
  long_raw_raw long raw not null,        -- バイナリデータ(非推奨)(なぜかlong型のものと同時に定義しようとするとエラー)
  raw_raw raw(255) not null,             -- バイナリデータ
  rowid_raw rowid not null,              -- 行番号(string？)
  urowid_raw urowid not null             -- グローバルな行番号？
  -- xml xmltype not null,                  -- XML
  -- uri uritype not null                   -- URI
);

insert into hrr_datatype_test values (
  'abcd', 'a', 'abcd', 'abcd', 'abあい', 'abあい',
  1234.1234, 1234.12, 1234, 1234.1234, 1234.1234, 1234.1234, 1234.1234, 0.2,
  sysdate, '123456789ABCDEF', 'abcdあいうえ', 'abcdあいうえ',
  '1234abcd', '1234abcd', 'AAAK6mAAAAAABF8AAA', 'AAAK6mAAAAAABF8AAA'
  -- xmltype.createxml('<test></test>'), httpuritype.createuri('http://www.example.com')
);
