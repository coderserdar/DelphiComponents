select * from customer;

CREATE TABLE dbx_test_char (
  f_int int NULL,
  f_char_1 char(1) NULL,
  f_char_3 char(3) NULL,
  f_char_254 char(254) NULL,
  f_char_255 char(255) NULL
)
;

select * from dbx_test_char;

CREATE TABLE dbx_test_varchar (
  f_int int NULL,
  f_varchar_10 varchar(10) NULL,
  f_varchar_254 varchar(254) NULL,
  f_varchar_255 varchar(255) NULL,
  f_text text NULL
)
;

select * from dbx_test_varchar;

CREATE TABLE dbx_test_short (
  f_int int NULL,
  f_datetime datetime NULL
)
;

select * from dbx_test_short;

CREATE TABLE dbx_test_long (
  f_int int NULL,
  f_memo memo NULL,
  f_binary binary(300) NULL,
  f_image image NULL,
  f_varbinary varbinary(300) NULL
)
;

select * from dbx_test_long;
