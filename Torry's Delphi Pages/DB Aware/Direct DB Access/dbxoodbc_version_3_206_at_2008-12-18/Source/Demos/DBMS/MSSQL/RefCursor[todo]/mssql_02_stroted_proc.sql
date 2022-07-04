USE [dbxoodbc]
GO

IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[dual]') AND type in (N'U'))
DROP TABLE [dbo].[dual]
GO

IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[test_refcursor2]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[test_refcursor2]
GO

SET ANSI_NULLS ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[dual](
  [x] [char](1) NULL
) ON [PRIMARY]

GO

INSERT INTO [dbxoodbc].[dbo].[dual]
           ([x])
     VALUES
           ('X')
GO

SET QUOTED_IDENTIFIER ON
GO

create proc [dbo].[test_refcursor2]
  @a int = null
as
  select * from dual
  select 'A' F_A, 0 F_B, * from dual

GO
