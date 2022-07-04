SELECT
  id,
  f_binary,
  f_bit,
  f_char_1,
  f_char_3,
  f_datetime,
  f_decimal_19_5,
  f_float,
  f_image,
  f_int,
  f_money,
  f_nchar_1,
  f_nchar_3,
  f_ntext,
  f_numeric_19_5,
  f_nvarchar,
--  f_real, -- precission: is not recommended use in condition "where"
  f_smalldatetime, -- precission: is not recommended use in condition "where"
  f_smallint,
  f_smallmoney,
  f_sql_variant,
  f_sysname,
  f_text,
--  f_timestamp, -- mssql driver returned uncorrect field type information
  f_tinyint,
  f_uniqueidentifier,
  f_varbinary,
  f_varchar
--  f_xml, -- MSSQL 2005
--  f_sysname_sys  -- MSSQL 2005
FROM
  dbo.table_field_test1
