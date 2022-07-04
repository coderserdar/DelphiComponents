USE [dbxoodbc]
GO

IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[test_refcursor2]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[test_refcursor2]
GO
