CREATE TABLE [table_fields_test] (
  [id] bigint NOT NULL,
  [f_varchar] varchar(10) NULL,
  [f_nvarchar] nvarchar(10) NULL,
  [f_varbinary] varbinary(300) NULL,
  [f_ntext] ntext NULL,
  [f_smalldatetime] smalldatetime NULL,
  PRIMARY KEY CLUSTERED ([id])
)
GO


select * from table_fields_test


///////////////////////////////
CREATE TABLE [table_fields_test2] (
  [id] int NOT NULL,
  [f_varchar] varchar(10) NULL,
  PRIMARY KEY CLUSTERED ([id])
);

select * from table_fields_test2;

CREATE TABLE [table_fields_test3] (
  [id] bigint NOT NULL,
  [f_varchar] varchar(10) NULL,
  PRIMARY KEY CLUSTERED ([id])
);

select * from table_fields_test3;


CREATE TABLE [table_fields_test4] (
  [id] bigint NOT NULL,
  [f_varchar] varchar(10) NULL,
  [f_nvarchar] nvarchar(10) NULL,
  PRIMARY KEY CLUSTERED ([id])
);

select * from table_fields_test4;


CREATE TABLE [table_fields_test5] (
  [id] bigint NOT NULL,
  [f_varchar] varchar(10) NULL,
  [f_ntext] ntext NULL,
  PRIMARY KEY CLUSTERED ([id])
);

select * from table_fields_test5;


CREATE TABLE [table_fields_test6] (
  [id] bigint NOT NULL,
  [f_varchar] varchar(10) NULL,
  [f_varbinary] varbinary(300) NULL,
  PRIMARY KEY CLUSTERED ([id])
);

select * from table_fields_test6;


CREATE TABLE [table_fields_test7] (
  [id] bigint NOT NULL,
  [f_varchar] varchar(10) NULL,
  [f_smalldatetime] smalldatetime NULL,
  PRIMARY KEY CLUSTERED ([id])
);

select * from table_fields_test7;
