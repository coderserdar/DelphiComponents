/*
CREATE TABLE dbx_test_? (
  -- INT
  [f_int] int NULL,
  [f_bigint] bigint NOT NULL,
  --
  --
  PRIMARY KEY ([f_bigint])
);
*/

CREATE TABLE dbx_test_short (
  -- INT
  [f_int] int NULL,
  [f_bigint] bigint NOT NULL,
  [f_smallint] smallint NULL,
  [f_tinyint] tinyint NULL,
  -- BOOL
  [f_bit] bit NULL,
  -- DECIMAL/FLOAT
  [f_decimal_19_5] decimal(19, 5) NULL,
  [f_numeric_19_5] numeric(19, 5) NULL,
  [f_decimal_19_0] decimal(19, 0) NULL,
  [f_numeric_19_0] numeric(19, 0) NULL,
  [f_decimal_5_5] decimal(5, 5) NULL,
  [f_numeric_5_5] numeric(5, 5) NULL,
  [f_real] real NULL,
  [f_float] float NULL,
  [f_smallmoney] smallmoney NULL,
  [f_money] money NULL,
  -- DATETIME
  [f_datetime] datetime NULL,
  [f_smalldatetime] smalldatetime NULL,
  --[f_timestamp] timestamp NULL, -- not need for user access
  -- PK
  PRIMARY KEY ([f_bigint])
);


CREATE TABLE dbx_test_char (
  -- INT
  [f_int] int NULL,
  [f_bigint] bigint NOT NULL,
  -- CHAR
  [f_char_1] char(1) NULL,
  [f_char_3] char(3) NULL,
  [f_char_254] char(254) NULL,
  [f_char_255] char(255) NULL,
  [f_char_256] char(256) NULL,
  [f_char_355] char(355) NULL,
  [f_char_2048] char(2048) NULL,
  [f_char_3k] char(3000) NULL, -- max 8000
  --
  PRIMARY KEY ([f_bigint])
);

CREATE TABLE dbx_test_varchar (
  -- INT
  [f_int] int NULL,
  [f_bigint] bigint NOT NULL,
  -- VARCHAR
  [f_varchar_10] varchar(10) NULL,
  [f_varchar_254] varchar(254) NULL,
  [f_varchar_255] varchar(255) NULL,
  [f_varchar_256] varchar(256) NULL,
  [f_varchar_2048] varchar(2048) NULL,
  [f_varchar_3k] varchar(3000) NULL, -- max 8000
  -- VARCHAR BLOB
  [f_text] text NULL,
  -- CHAR/NCHAR ANALOG
  [f_sql_variant] sql_variant NULL,
  [f_uniqueidentifier] uniqueidentifier NULL,
  [f_xml] xml NULL,
  [f_sysname] sysname NULL
  --
  PRIMARY KEY ([f_bigint])
);

CREATE TABLE dbx_test_nchar (
  -- INT
  [f_int] int NULL,
  [f_bigint] bigint NOT NULL,
  -- NCHAR
  [f_nchar_1] nchar(1) NULL,
  [f_nchar_3] nchar(3) NULL,
  [f_nchar_128] nchar(128) NULL,
  [f_nchar_129] nchar(129) NULL,
  [f_nchar_254] nchar(254) NULL,
  [f_nchar_255] nchar(255) NULL,
  [f_nchar_256] nchar(256) NULL,
  [f_nchar_355] nchar(355) NULL,
  [f_nchar_2048] nchar(2048) NULL,
  --[f_nchar_4k] nchar(4000) NULL, -- max 4000
  --
  PRIMARY KEY ([f_bigint])
);

CREATE TABLE dbx_test_nvarchar (
  -- INT
  [f_int] int NULL,
  [f_bigint] bigint NOT NULL,
  -- NVARCHAR
  [f_nvarchar_10] nvarchar(10) NULL,
  [f_nvarchar_128] nvarchar(128) NULL,
  [f_nvarchar_129] nvarchar(129) NULL,
  [f_nvarchar_254] nvarchar(254) NULL,
  [f_nvarchar_255] nvarchar(255) NULL,
  [f_nvarchar_256] nvarchar(256) NULL,
  [f_nvarchar_2048] nvarchar(2048) NULL,
  [f_nvarchar_4k] nvarchar(4000) NULL, -- max
  --
  PRIMARY KEY ([f_bigint])
);

CREATE TABLE dbx_test_nchar2 (
  -- INT
  [f_int] int NULL,
  [f_bigint] bigint NOT NULL,
  -- CHAR
  [f_nchar_4k] nchar(4000) NULL, -- max 4000
  -- NCHAR
  [f_ntext] ntext NULL,
  --
  PRIMARY KEY ([f_bigint])
);

CREATE TABLE dbx_test_long (
  -- INT
  [f_int] int NULL,
  [f_bigint] bigint NOT NULL,
  -- CHAR
  --[f_nchar_4k] nchar(4000) NULL, -- max 4000
  -- NCHAR
  [f_ntext] ntext NULL,
  -- BLOB/BINARY
  [f_binary] binary(300) NULL,
  [f_image] image NULL,
  [f_varbinary] varbinary(300) NULL,
  --
  PRIMARY KEY ([f_bigint])
);
