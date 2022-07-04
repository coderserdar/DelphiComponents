{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
{
  Kylix / Delphi DbExpress driver for ODBC Version 3 drivers
  (Also works with ODBC Version 2).

  Normally SqlExpress loads the driver DLL dynamically (ie dbxoodbc.dll),
  according to TSQLConnection.LibraryName.

  Alternatively if you add this unit to a USES anywhere in your project,
  the driver will be statically linked into your program and SqlExpress will
  ignore TSQLConnection.LibraryName. (This magic is achieved by the call to
  SqlExpr.RegisterDbXpressLib in the initialization section of this unit, but
  it only works in Windows - in Kylix SqlExpr.RegisterDbXpressLib is not defined)

  The project source for DLL version is 'dbxoodbc.dpr', which just
  USES this module and EXPORTS the getSQLDriverODBC function.

  ------------------------------------------------------------------------------

  Steps to manually install new DBExpress driver
  (This is not really necessary - it just makes it easier to change drivers, by
  selecting TSQLConnection.DriverName and TSQLConnection.ConnectionName drop-down)

  1. Retrieve DbExpress INI file names from registry
     '\Software\Borland\DBExpress' Value 'Driver Registry File'
     (The default ini file names will be:
      C:\Program Files\Common Files\Borland Shared\DBExpress\dbxdrivers.ini)
      C:\Program Files\Common Files\Borland Shared\DBExpress\dbxconnections.ini)

  2. In dbxdrivers.ini file

  a. Add line to '[INSTALLEDDRIVERS]' section
       drivername=1 (eg 'Dbx Open Odbc Driver=1') (The driver name can be any name you like)

  b. Add new [drivername] section ie [Dbx Open Odbc Driver], with following 3 lines
       LibraryName=dbxoodbc.dll        (Not case-sensitive in Windows)
       GetDriverFunc=getSQLDriverODBC  (NB This is case-sensitive, even in Windows)
       VendorLib=ODBC32.DLL            (Value not actually used by this driver - ODBC32.DLL is hard-coded)

  3. In dbxconnections.ini file

  a. Add '[ConnectionName]' section with following 4 lines
       DriverName=Dbx Open Odbc Driver (or whatever you called it in dbxdrivers.ini)
       Database=ODBC DSN name, or ODBC connection string, or ? for Driver Prompt
       User_Name=user name (this can be ommitted)
       Password=password (this can be ommitted)

  b. You can have as many different [ConnectionName] sections as you like, so
     you can pre-configure several ODBC connection settings.

  If you skip the install, you must set TSQLConnection properties
  before you connect:
    .LibaryName = dbxoodbc.dll (must be fully qualified if not on the search path)
    .GetDriverFunc = getSQLDriverODBC (NB this is case-sensitive)
    .VendorLib = ODBC32.DLL
    .Params - As described dbxconnections.ini above - Database is required, others optional
}

{
  Note on Error checking:

  In this driver almost every function that implements the various
  DBXpress interfaces is guarded by "try / except".

  If an error occurs, an EDbxError exception of one of the following sub-types is rasied:

    EDbxOdbcError: An error was returned from an ODBC function call
    EDbxInvalidCall: The function or its parameters are not valid
    EDbxNotSupported: The function or its parameters are valid but not yet supported by this driver
    EDbxInternalError: Some other error occurred with this module

  Control then goes to the Except routine in the relevant interface functuon.

  If EDbxNotSupported can happen, this is always checked first, and the
  function returns DBXERR_NOTSUPPORTED, so the caller can take action as necessary.

  Otherwise the EDbxError handler gets invoked (because EDbxError is the parent
  class of the other 3 exception types). The handler retrieves the Exception message,
  saves it in an instance variable (occasionally some other info is also saved),
  and then the function returns MaxReservedStaticErrors + 1.
  The caller can get the error text (via GetErrorMessage/Len) and raise its own
  error or ignore, as appropriate.

  The reason for raising and trapping EDbxError, rather than simply returning
  non SQL_SUCCESS code, is that the calling programs may not always check
  the function return code, or it may check for error code but retrieve the error
  message from the wrong interface. So by raising exception at the point of
  error, the IDE halts on the exception during source debugging, and this makes
  it much easier to trace errors.
}

{
  Note to contributors

  DO NOT REFORMAT EXISTING CODE

  Not only is it impolite, but (more importantly) it obscures the 'real' changes.
  Also, it is a waste of time, because I will change any reformatting back again.

  Here are the formatting rules --

  As closely as possible, follow Borland formatting conventions.
  For this, run code though DelForExp (Delphi Formatting Expert),
  set with "Borland Style", but wrap lines at position 100.

  Variable naming and capitalization:
  Use lowercase for reserved words (as per Borland standard) [N.B. "string" is a reserved word!]
  Use "infix caps" (also known as "Pascal capitialzation") for identifiers.
  In this convention, each word within variable starts with uppercase, example, MyVariableName.
  In the standard used in this project, acronyms are treated as single words,
  example - MySqlOdbcDbxVariableName (NOT MySQLODBCDBXVariableName)
  Common type names are treated as single words, not two words,
  example, Smallint, Timestamp (not SmallInt, TimeStamp)
  American English spellings are used, not British English,
  examples, Color, License, Synchronize (not Colour, Licence (as a noun), Synchronise).

  Exceptions to "infix" caps:
  Our class member variables begin lowercase "f"
  (Because then the difference between a method and a field is more distinct)
  Our procedure local variables may begin lowercase or uppercase (eg i, aTempVar, or TempVar)

  Where names come from a pre-defined header, (eg, Interface method names in DBXpress.pas,
  or names in OdbcApi.pas), we always follow capitalization of the header file, even if it
  is inconsistent (eg ISQLCommand.close, ISQLCommand.getParameter, ISQLCommand.GetOption).

{
History:
********

Beta, 2001-10-26 [ Edward Benson ]
----------------------------------
+ First public release

Version 1.01, 2001-11-28  [ Edward Benson ]
-------------------------------------------
+ Fix bug in TSqlCursorMetaDataIndexes
+ Support Interbase 6 Easysoft ODBC Driver
+ Support MySql ODBC Driver (ODBC level 2)

Version 1.02, 2001-12-05 [ Edward Benson ]
------------------------------------------
+ Fix bug in TSqlCursorOdbc.getBcd to cater for comma decimal separator

Version 1.03, 2001-12-06 [ Edward Benson ]
------------------------------------------
+ Change to support Kylix
  (fixes posted by Ivan Francolin Martinez)

Version 1.04, 2002-01-22 [ Edward Benson ] (Not released to public)
------------------------------------------
+ Internally clone connection for databases that only support 1 statement
  handle per connection, such as MsSqlServer
  (maintain internal connection cache for such databases, until disconnected)
+ Work around MySql bug - odbc driver incorrectly reports that
  it supports transactions when it doesn't
+ More changes to support Kylix (in OdbcApi.pas)
  (fixes posted and tested by Ivan Francolin Martinez)
+ Allow for blank column names (returned by Informix stored procedures)
  (fix posted and tested by Bulent Erdemir)

Version 1.05, 2002-06-09 [ Edward Benson ] (Not released to public)
------------------------------------------
+ Change to support TIMESTAMP parameters
  (fix posted and tested by Michael Schwarzl)
+ Work around to support multiple GetBlob calls for MS SqlServer
  (fix posted and tested by Michael Schwarzl)
+ Work around for Delphi 6.02 -
  SqlExpress now calls ISqlCommand.SetOption(RowSetSize) for all drivers
+ Fix TSqlCursorOdbc functions: isReadOnly, isAutoIncrement, isSearchable
  Were incorrectly using ColNo-1 (ie 0-based) - ODBC column indexes are 1-based
  (Confusing, because the bind array (fOdbcBindList) is 0-based)
+ eOdbcDriverTypeAccess renamed eOdbcDriverTypeMsJet
  (MsJet driver works for other databases, not just Access)

Version 1.06, 2002-11-01 [ Edward Benson ] (Prepare for Vadim's changes)
------------------------------------------
+ Reformatted comments and code, so diff shows up changes for 2.00
}

{+2.01 WhatNews}
(*
Version 2.01, 2002-11-01 (Vadim Lopushansky)
------------------------

Edward> + below means I have included Vadim's change,
Edward> - means I have not

  + Change to support Delphi7. See block: {$IFDEF _D7UP_}.
  + Change to support INFORMIX (tested on version IDS 7.31 TD3).
  + Change to support ThinkSQL (tested on version 0.4.07 beta. http://thinksql.com/).
  + Change to detect database types for Multiplatform DataDirect ODBC Drivers
    (http://www.datadirect-technologies.com)
  - Change to detect database type method TSqlConnectionOdbc.RetrieveDriverName.
    For detecting usage specific RDMS query.
    Edward> I have not included this:
    Edward> I think it is better to use SQLGetInfoString(SQL_DBMS_NAME) instead
  + Change to remapping Int64 to BCD
    (optional. Connection parameter: "Database"="...;coMapInt64ToBCD=1"
    or "Custom String"="...;coMapInt64ToBCD=1")
  + Change to remapping small BCD to native
    (optional. Connection parameter: "Database"="...;coMapSmallBcdToNative=1"
    or "Custom String"="...;coMapSmallBcdToNative=1"))
    Is problem in editing controls when native type length is more then BCD data type length.
    For editing you mast usage controls with format string...
  + Change for addition of possibility of disconnecting of support of the metadata.
    Is used in case of availability of errors in the ODBC driver.
    For disconnecting the metadata it is necessary to add to connection line Metadata=0
    (Connection parameter: "Database"="...;coMetadata=0"  or "Custom String"="...;coMetadata=1").
  + Change to updating BCD values when DecimalSeparator <> '.'
  + Change to reading of PK_INDEX from metadata (Calculating fPkNameLenMax).
    For an example look: ($DELPHI$)\Demos\Db\DbxExplorer\dbxexplorer.dpr
    (Read PKEY_NAME error).
    All metadata fields returned length more 0.
  + Change in %Metadata%.getColumnLength:
    Adapting to calculate visible of columns in SqlExpr.pas type.
    For an example look: ($DELPHI$)\Demos\Db\DbxExplorer\dbxexplorer.dpr
    (Read procedure parameters position error).
  + Change to remove warnings and hints.
  + Change to Access Violation code
    (When returned column precision from LongWord to Smallint type when
    precission is more High(smallint), ... )
  + Change to setting metadata position for bad odbc driver
    (Read of columns information with the "Easysoft Interbase ODBC Driver"
     version 1.00.01.67 on example "dbxexplorer.dpr").
  + Changes when QuoteChar=' '.
    In this situation QuoteChar must be empty (''). (MSSQL, Informix,...)
    ( Edward> ???Ed>Vad/All: But I think MSSQL uses doublequote char ("), not blank. )
  + Change to support Trim of Fixed Char when connection parameter "Trim Char" is True
    or when connection parameter: Database=...;coTrimChar=1.
    The mode 1 - allows to work in the mode compatible
    with the "BDE" mode for "FixedChar" of strings.
    Mode 0 - is default - the strings of fixed width are not truncating.
  + New SchemaFilter parameter in login parameter "Custom String".
    This parameter allows to filter the metadata of the only current scheme.
    By default filtering is on for: Oracle.
    If it does not settle - disable filtering through the parameter of connection:
    Custom String=...;coSchemFlt=0
  + Change to autodetect ODBC driver level mode 2.
  + Change to autodetect SupportsCatalog Options.
    Warning: Some of the driver is illconditioned work with this option.
    For example do not return an error at installation of a unknown of the catalog.
    From behind it the procedure of installation of the catalog was received
    cumbersome and depending from database.
    But you have possibility of load shedding of support of the catalog.
    Read further about parameter of Catalog...
  + The possibility is supplemented to disable support of the
    'Catalog option. Database=...;coCatalog=0
  + Change to increase of speed of blob fetching.
    Database or "Custom String" parameter "coBlobChunkSize".
    In Bytes. Define size blob buffer for loop-fetching.
    The size of a cache can be synchronized with a size of a cache assigned in the ODBC driver.
  + Vadim> ???Vad>All: Change to support Odbc driver attribute SQL_ATTR_PACKET_SIZE.
    Edward> Very good! Although I think it is not advisable to ever change this,
    Edward> Borland has seen fit to add it as an option, so we should implement it.
    For support this attribute you must define value it parameter in
    "Custom String" (Delphi 7) or "Database" (Delphi 6, 7)
    "Custom String"="...;coNetPacketSize=8192";
    Database="..;coNetPacketSize=8192"
    ConPacketSize should not be less than 4096. The upper range is defined by the driver.
  - Database or "Custom String" parameter "DriverLevel"
    user defined ODBC driver level mode.
    Edward> Now removed - we now auto-detect driver level
  - Change in ParseTableName for parsing in informix ...
    Warning: Probably and for other database servers it is necessary to change
    in view of their format of the job(definition) of a full name of the table.
  + Ignoring of exceptions for want of indexes.
  + It is possible at call to the tables from other spaces (catalog, servers, references).
    (Edward> what do you mean?)
  + The possibility of the external definition of parameters of the driver
    is supplemented (Catalog,TrimChar,BlobChankSize).
      Examples:
        Delphi 7:
          Custom String=;coCatalog=0;coTrimChar=1;coMapInt64ToBCD=1;coSchemFlt=1;
           coMetadata=0;coMapSmallBcdToNative=0;coBlobChunkSize=32768
        Delphi 6 (also will work for Delphi 7):
          Database=DSN=DBDEMOS;UID=anonymous;PWD=unknown;
           coTrimChar=1;coBlobChunkSize=32768;coNetPacketSize=3072
  - Change in "SqlExpr.pas" (Delphi 6,7):
     Change for reading of metadata when ODBC driver supported
     only one sql statement (MSSQL...).
     Change for support of the "connection string with prompt" '?'
     when need clone connection.
     For more detail look then file WhatNews.Txt.
    (Edward> I have done this by internally cloning the connection to handle this case)
  + All changes are included in the block:
      //                                   {+ver Optional description}
      //                                   ... new or changed code
      //                                   {/+ver /Optional description}
*)
{/+2.01 /WhatNews}

{+2.02 WhatNews}
(*
Version 2.02, 2002-11-04 [ Vadim V.Lopushansky pult@ukr.net ]
------------------------

      All changes are concluded in the block:
      {+2.02 Optional Description}
       ... new or changed code
      {/+2.02 /Optional Description}

+ added suported INTERVAL types as Fixed Char
  (look SQL_INTERVAL_YEAR or SQL_INTERVAL_MINUTE_TO_SECOND )
+ added optiong for ignoring of uknknown field types
  (look coNoIgnoreUnknownFieldType and IgnoreUnknownType )
  Connectin parameter: Database=...;coIgnoreUnkFldType=1
  or parameter "Custom String"="...;coIgnoreUnkFldType=1"
  Default is False(0) except informix. For informix=True(1)
+ Set default isolation to DirtyRead (look SQL_TXN_READ_UNCOMMITTED) (???)
- ??? Set default CURSOR BEHAVIOR to PRESERVE MODE. !!! Has failed !!!
(look SQL_CURSOR_COMMIT_BEHAVIOR) (???)
+ detect RDBMS types ( you can analyze RDBMS name, major and minor version, and client version )
*)
{/+2.02 /WhatNews}

{
Version 2.03, 2002-11-20 [ Edward Benson ]
------------------------
{
+ Split ISqlConnectionOdbc out to new module, DbxOpenOdbcInterface.
  This allows you to call the new methods of ISqlConnection,
  but without having to statically link in this module.
  (See QueryInterface comments in DbxOpenOdbcInterface on how to do this).

{+2.04 WhatNews}
(*
Version 2.04, 2002-12-19 [ Vadim V.Lopushansky pult@ukr.net ]
------------------------
+ Regular Expression Parser for Decode/Encode different DBMS object name format.
  Usage of this capability is adjusted(regulated) in parameter: {$define _RegExprParser_}
  It option can be turned off.

  If for your DBMS the off-gauge format of the definition of a full name of the object of DBMS,
  to you is necessary to describe the template of this format in the file "DbxObjectParser.pas".
  For debugging your template you can take advantage of an example from "RegExprParser.zip".

+ The capability of mapping of text fields into memo field is added as it is made in BDE:
  the fields with lengthy more than 256 characters are imaged on BlobMemo
  (optional. Connection parameter: "Database"="...;coMapCharAsBDE=1"
    or "Custom String"="...;coMapCharAsBDE=1")
*)
{/+2.04 /WhatNews}
{
Version 2.041, 2003-01-15 [ Dmitry Arefiev <darefiev@gs-soft.ru> ]
------------------------
+ Dmitry Arefiev <darefiev@gs-soft.ru>
  Some AV fixed in:
   TSqlCursorOdbc.BindResultSet
   TSqlCursorMetaDataIndexes.getString

Version 2.042, 2003-02-05 [ Vadim V.Lopushansky pult@ukr.net ]
------------------------
+ Alteration of implementation for ConnectionOptions
+ Cannot disconnect when inTransaction and fAutoCommitMode = SQL_AUTOCOMMIT_OFF.
   INFORMIX, MSACCESS:
   ERROR: SQLDisconnect(fhCon) returned error
   example:
    SQLCon.Open;
    SQLCon.StartTransaction(...;
    SQLQuery.Open;
    SQLCon.Close; // <- ERROR: SQLDisconnect(fhCon) returned error
+ The procedures are changed:
    - procedure FreeHCon
    - procedure FreeHStmt
      The clearing of the parameter SQLHDBC/SQLHSTMT is added.
+ Variables of types (SQLHDBC,SQLHSTMT) i compare with "SQL_NULL_HANDLE", instead of with "nil".
+ Optimization of speed by replacement:
    "OdbcCheck(...)" to "if OdbcRetCode < > OdbcApi.SQL_SUCCESS then OdbcCheck(...)"
+ Possibility of disconnecting of processing of a situation with hidden cloning of connections
  at limitation on quantity of simultaneous cursors. Search for an option
  $define _DisableOverloadCloningConnection_
+ The procedure "setParameter(.." are changed.
+ New Connection Option: "EmptyStrParam"
  Some ODBC of the driver do not handle a situation, when the string has zero length. For them it
  was necessary to write in the code following:
  procedure TForm1.DataSetBeforePost((DataSet: TDataSet);
  begin
    if Length(DataSet.FieldByName('customer').AsString) = 0 then
      DataSet.FieldByName('customer').Clear;
  end;
  To avoid it, now it is enough in connection string to indicate EmptyStrParam=0

Version 2.043, 2003-02-11 [ Vadim V.Lopushansky pult@ukr.net ]
------------------------
+ MSSQL SERVER: The situation with a mistake of cloning connection is corrected at connection
  through PIPE.
  ODBC Driver: SQL Server, version: 2000.81.9030.04.
  The problem consists that at connection through PIPE ODBC Driver returns a non-working line
  of connection (prevents option Network).

Version 2.05, 2003-04-04 [ Edward Benson ]
------------------------
+ Reformatting to agreed standard only.
  No functional changes.
  Release to CVS, to so we can see real functional DIFFs between next version and 2.043

Version 2.06, 2003-04-22 [ Edward Benson ]
------------------------
+ Use CompilerVersion instead of VERxxx to determine Compiler Version - this is more future-proof
+ Show Sql command text (if available) in GetErrorMessage
+ Show parameter values (if available) in GetErrorMessage
+ Show Sql Connect string (but with hidden password) in GetErrorMessage
+ Allow for Input, Output, Inout and Return parameters
+ Allow for transaction isolation level to be specified in ISQLConnection.beginTransaction
+ Reformat some exception messages
+ After SqlCloseCursor, add ODBC calls to UNBIND cursor bind vars and RESET parameters
+ Rollback outstanding transaction on disconnect, in case where concurrent statements
  per connection not unlimited (Vadim Lopushansky)
+ Conditional compilation for Kylix 3 [Kurt Fitzner / Stig Johansen]
+ TSqlCursorMetaData.isSearchable no longer raises error (Dmitry Arefiev)
+ Rename conditional (not) _DisableOverloadCloningConnection_ to _InternalCloneConnection_
+ Procedure FreeHCon changed from (out HCon: SqlHDbc) to (var HCon: SqlHDbc) [Stig Johansen]
+ Fix problem with TSqlCommandOdbc.SetParameter for fldFLOAT [Stig Johansen]
+ Do not call SQLGetStmtOption(SQL_ROWSET_SIZE) -- it does nothing [Stig Johansen]
+ In TSqlCommandOdbc.Destroy add check for statement already freed [Stig Johansen]

Version 2.07, 2003-05-07  [ Vadim V.Lopushansky pult@ukr.net ]
------------------------
+ Some critical errors have been corrected. (Uncorrect call FillChar, ...).
+ Added the possibility of watching of a sequence of calls of methods. See directive "_TRACE_CALLS_".
+ Tracing can be applied for watching problems with new drivers or DBMS.

Version 2.08, 2003-10-30  [ Vadim V.Lopushansky pult@ukr.net ]
------------------------
+ Fixed FetchLateBoundData and FetchLongData. There can be errors, when connection
  option coIgnoreUnknownFieldType is turned ON and query contained unsupported
  field type and fetching usage  LateBoundFound method for field located after
  unsupported field.
+ Fixed set SQL_VARCHAR parameter with length>255.
  AV: the reason - record outside the allocated memory.
+ Added access to INFORMIX LOB fields (not fully tested).
+ Fixed BcdToStr from FMTBcd.pas. See QC: 6169.
+ Release fhCon at disconnecting reconciled in impossibility of a reconnecting.
+ Fixed SQLite field mapping
+ Added checking of reasonableness of field size (returned from odbc driver) for next SQLTypes:
  SQL_CHAR, SQL_VARCHAR, ... SQL_BINARY, SQL_VARBINARY, SQL_LONGVARCHAR, SQL_WLONGVARCHAR
+ Added limitation to BindBufferSize for simple types:
  SQL_BINARY, SQL_VARBINARY,
  SQL_CHAR, SQL_VARCHAR ...
  It is handled only when fCommandBlobSizeLimitK<=0 (When it is possible to apply OdbcLateBound).
+ Added support in MSSQL sql_variant field type: SELECT value FROM "dbo"."sysproperties"
  But you can usage the next code for accessing:
  http://www.novicksoftware.com/UDFofWeek/Vol1/T-SQL-UDF-Volume-1-Number-12-udf_SQL_VariantToDatatypeName.htm
+ Set ConPacketSize for Cloned Cnnection
+ Fixed when transaction not supported
+ Fixed Connect: Removal of user options is made of a line of connection before its analysis.
+ Fixed Fetch Bytes Fields. Has been skipped FetchLateBoundData.
+ Added support of fldVARBYTES.
+ Fixed SortOrder. Must be in Upper Case.
+ Fixed indexing for TClientDataSet. Index cannot be unnamed (Very impotent for PK and Unique indexes).
+ Fixed Create RegExp Parser when QuoteChar = #0
+ Added support for:
    Pervasive.SQL (odbc ver: '8.10.117.17'),
    PostgreSQL (odbc ver: '7.03.01.00'),
    Cache`.
+ Added the opportunity to collect the driver in younger version Delphi for the senior version.
  Access to new opportunities now are accessible and in younger versions Delphi.
+ Added demo application: ODBC Explorer.

Version 2.09, 2003-10-31  [ Vadim V.Lopushansky pult@ukr.net ]
------------------------
+ Added usage of one buffer of memory for all types with a known size.
+ Added support BLOCK FETCHING options:
   eCommBlockRead (default=True),
   eCommRowsetSize (default in SqlExp=20).
  *****************************************************************
  *** Added support ODBC option: SQL_ATTR_ROW_ARRAY_SIZE !!!    ***
  *** It mapped to standard connection option: eCommRowsetSize. ***
  *****************************************************************
  Main changes: BindResultSet, Next, SetOption(eCommRowsetSize,...), connect

Version 2.010, 2003-11-06  [ Vadim V.Lopushansky pult@ukr.net ]
------------------------
+ Fixed procedure .next() for 'ARRAY FETCH' when the driver returns lines less than should.
  For example SAP DB ' ODBC Driver, ver: '7.04.03.00' always returns only one line at any
  set of rows.
+ Fixed TextFile( *.txt, *.csv ) Table Name parsing. TableName can contain the '.'.
+ Fixed SQLite update/fetch BLOB (Memo) data types.
New/Changed options:
+ Added option "ENABLEBCD". In this option any BCD is mapped to Float.
+ Added mapping unsupported BCD Size to Float. See MaxFMTBcdDigits in FMTBcd.pas
+ Added MAXBCD option. For this option any BCD is mapped to BCD(32,??).
  It is possible for using in a case when the driver returns the incorrect column info.
+ Added check BCD Oveflow for procedure .getBCD(). Uncorrect values are trimmed.
+ Added default driver option. You can set default driver value at any time for any properties.
  For  make it you must set option value to 'X':
  example:
    SQLConnection1.SQLConnection.SetOption(
      TSQLConnectionOption(xeConnCustomInfo), Integer(PAnsiChar('MAXBCD=X')) );
New supported odbc drivers:
+ Added support for 'SAP DB ODBC Driver', ver: '7.04.03.00'
+ Added support for Firebird 'IBPhoenix ODBC Driver': http://www.ibphoenix.com/, ver: '01.00.0000'
  There were problems for the some Float fields.
+ Added support for 'XTG System Open Source Firebird, Interbase6 ODBC Driver':
  http://www.xtgsystems.com/, ver: '1.00.00.16'
  At use of this driver for function RowsAffected you will always receive 0 as the driver
  does not realize function SQLRowCount. From for it by transfer of changes to a DB by means
  of DSP you will receive mistakes: 'Record not found or changed by another user'. In part
  this problem can be solved as it is made in example OdbcExplor.dpr
  Also this driver returns bad columns Size(fColSize) for BCD field.
+ Added support for 'MERANT DBASE ODBC Driver', ver: '3.60.00.00'.
  Fixed .FetchIndexes() for it driver. The driver returns fields for an index as their concatenation
  (with a separator '+'). Example: 'fieldA+fieldB'.

Version 2.011, 2003-12-09   [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ Parse ConnectionString connection option (Delphi 7) and also the normal connection string,
  and if they BOTH contain "DATABASE=" we use replace the database value with
  the value from the ConnectionString option.
  eg, if normal connection string is "DSN=MyDsn;DATABASE=MyDb"
  and ConnectionString is "DATABASE=OtherDb",
  then the Odbc connect string will become "DSN=MyDsn;DATABASE=OtherDb"
  I think that's what it does.
  But I don't know why Vadim has added this.
+ Extra tracing
+ changed: coIgnoreUnknownFieldType default value is ON

Version 2.012, 2003-12-10  [ Edward Benson ]
-------------------------
+ Simplify explanations of _MIXED_FETCH_ and _MULTIROWS_FETCH_
+ coIgnoreUnknownFieldType default value is OFF
  except Informix, which has default value ON.
+ _DENT_ conditional define [Dmitry Arefiev]

Version 2.013, 2004-02-25  [ Dmitry Arefiev <darefiev@gs-soft.ru> ]
-------------------------
+ Changed TSqlCommandOdbc - added stored procedure support.
+ Changed TMetaIndexColumn, TSqlCursorMetaDataIndexes -
  ISQLMetaData.getIndices now returns correct info
  (schema and catalog columns are filled too).
+ Changed TSqlCursorMetaDataProcedures - added getShort method.
+ Renamed DENT registration routines.
+ Fixed OdbcDataTypeToDbxType. For SQL_VARCHAR and
+ SQL_WVARCHAR it was returning fldstFIXED, although should not.
+ Changed FormatParameters - added support for SQL_BINARY and SQL_VARBINARY.
+ Fixed TSqlCursorOdbc.getShort - returns correct values for BIT typed values.
+ Fixed TSqlCursorMetaDataTable.getString - was returning as
  TABLE_NAME qualified table name, although should return "raw"
  table name without catalog and schema. They are in dedicated columns.
+ fixed memory leak in TSqlConnectionOdbc.disconnect
+ fixed memory leak in TSqlCursorMetaDataProcedureParams.Destroy
+ Implemented UNICODE support (fldZSTRING, fldstWIDEMEMO/fldstUNICODE).
  Although dbExpress components does not support that, DENT
  supports and driver works very well. I have added connection
  parameter 'EnableUnicode', it is by default Off.
+ Added new ISqlCommandOdbc interface, with new cancel method.

Version 2.014, 2004-04-12  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ Encoding a full name of stored procedures is updated (added support regexp and informix).
+ Processing of errors is moved from "TSqlCursorOdbc.fCursorErrorLines" to
  "TSqlCursorOdbc.fOwnerCommand.fCommandErrorLines". DbExpress cannot handle errors
  for ISQLCursor. From for it there was a set of messages inappropriate to a context,
  such as "Error mapping field".
+ fixed AV for call StrLen(), StrCopy().
+ fixed access to metadata columns of procedures.
+ changed: At use of mode MixedFetch, it should be specified obviously. If the driver
  will not support this mode option MixedFetch will be ignored.
+ Warnings for use of an option "_MULTIROWS_FETCH_":
  ...
  For more details see ChangesLog.Txt.
}(*
Version 2.015, 2004-05-23  [ Dmitry Arefiev <darefiev@gs-soft.ru> ], [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
  [ Dmitry Arefiev <darefiev@gs-soft.ru> ]
+ changed: ISqlCommandOdbc now supported by TSqlCommandOdbc
+ added: TSQLMetaDataOdbc.SetOption supports schema and catalog names.
    Also other metadata related code uses these option values.
+ changed: extended Unicode support
+ added: optimized routines string <-> bcd conversions
+ changed: fixed AV's with empty catalog and schema names. This overwrites Vadims fixes,
    because it is not required to always check PAnsiChar's for nil.
+ changed: fixed FormatParameter and SQL_TIME
+ changed: beginTransaction set SQL_ATTR_TXN_ISOLATION only if it  was changed. It allows to
    overcome issue with DB2& "CLI0126E".
+ changed: TSqlCommandOdbc.Get/Set Option was returning error for xeCommTransactionID option.
    I have removed that, to make driver more compatible with Borland ones.
+ changed: fixed TSqlCommandOdbc.setParameter. It was transmiting wrong length values to
    SQLBindParameter when length was around 256. And made uniform handling for variable length
    data types.
+ changed: fixed TSqlCursorMetaDataIndexes.getString. It was returning wrong data in PKEY_NAME column.
-? changed: i have overwrite Vadims fix for "access to metadata columns of procedures".
    I have implemented order of columns not as in dbExpress help, but in
    consistent way with Borland's drivers.
+ changed: optimized BLOB fetching. Now cursor keeps BLOB buffers allocated until cursor will be closed.
+ changed: replaced AllocMem by GetMem to exclude not required memory filling by zeros
  [ Vadim V.Lopushansky pult@ukr.net ]
+ changed function GetOptionValue (Partial fixed). Completely correct work is
    impossible because of restriction of syntax ODBC Connection String.
+ Added unit SqlExprFix.pas:
    Some runtime memory fixes of Borland Delphi (6 Upd2, 7, 7 Upd1) system modules.
    For correction of errors it is enough to include this module in your project or in any package
    used by your project. After loading such package in IDE the bugs in standard packages will be
    corrected.
    Fixed units: db.pas, dbCommon.pas, Provider.pas, SqlExpr.pas.
       ...
       Added to Delphi6 connection handling options from Delphi7:
         CUSTOM_INFO        = 'Custom String'
         SERVERPORT         = 'Server Port'
         MULTITRANSENABLED  = 'Multiple Transaction'
         TRIMCHAR           = 'Trim Char'
         CONN_TIMEOUT       = 'Connection Timeout'
         OSAUTHENTICATION   = 'Os Authentication'
       For more details see ChangesLog.Txt.
+ Map compiler directive _InternalCloneConnection_ to connection option 'coICloneCon'.
+ Added field fCursorPreserved. When fCursorPreserved = False then problems are possible at work
    with transactions for open cursors (MSAccess, ...).
    In such situations it is recommended to set TClientDataSet.PackedRecords equal "-1".
    To find out value fCursorPreserved for the current connection it is possible at use ISqlConnectionOdbc.
    The general recommendation:
      Always establish in line ODBC of connection or in properties of ODBC DSN then
        "Cursor Behavior" = "Preserved". Examplpe connection string of INFORMIX ODBC Driver:
       "DRIVER={INFORMIX 3.82 32 BIT};DATABASE=test165;HOST=pult;SRVR=ol_pult;SERV=turbo;PRO=olsoctcp;"+
       "CLOC=ua_UA.1251;DLOC=ua_UA.1251;VMB=0;CURB=1;OPT=;SCUR=0;ICUR=0;OAC=1;OPTOFC=1;RKC=0;ODTYP=0;"+
       "DDFP=0;DNL=0;RCWC=0".
      But not all ODBC the driver support fCursorPreserved = True. Example:
      "DRIVER={Microsoft Access Driver (*.mdb)};DBQ=\dbdemos.mdb".

Version 3.000, 2004-05-31  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ added odbc direct mode:
  + added Dynamic Odbc Api Loading (implement simple internal "odbc api manager").
  + added support property "TSQLConnection.VendorLib"
      It allows to use odbc libraries directly, passing odbc manager.
      This mode is set by option DynamicOdbcImport in module "OdbcApi.pas".
      Work through system ODBC Manager is more reliable.
+ added processing "statement per connection" when it is more then one.
+ added clear odbc error or warning when ignore handling OdbcRetCode
  (see last parameter in "TSqlDriverOdbc.RetrieveOdbcErrorInfo").
+ For more details see ChangesLog.Txt.

Version 3.016, 2004-06-20  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ fixed refresh(reoped) statemenet when transaction changed and fCursorPreserved = False.
    See method ".execute()".
+ added: packing of connections when called "commit" or "rollback".
+ changed: exception handling for methods: ".GetOption()", ".GetMetaDataOption()"
    ( minimization of efforts at formation of exceptions in these methods ).
+ added connection option coReadOnly. It option is mapped to odbc SQL_MODE_READ_ONLY.
    Default value is "0" (ReadOnly = False).
+ added: Restrictions of updating for connection options depending on the current status of connection.
+ added option "_debug_emulate_stmt_per_con_".
    It is only for developer testing _InternalCloneConnection_ (emulated fStatementPerConnection).
- fixed default value for coEmptyStrParam ( == osOn).
+ added connection Option: "coNullStrParam":
    Some ODBC of the driver do not handle a situation, when the string is Null (SQLite).
    For them it  was necessary to write in the code following:
    procedure TForm1.DataSetBeforePost((DataSet: TDataSet);
    begin
      if DataSet.FieldByName('customer').IsNull then
      begin
        ...
        DataSet.FieldByName('customer').AsString := '';
    To avoid it, now it is enough in connection string to indicate coNullStrParam=0.
    Default value is osOn;
+ changed: Binding is changed for types (date, time, date time) depending on the odbc version.
+ updated binding of parameters of AnsiString/WideString types.
+ OdbcApi.pas: fixed call" wide odbc api functions", add compatible with odbc ver 2...
+ added connection option 'coLockMode':
    -1: Suspends the process until the lock is released.
     0: Ends the operation immediately and returns an error code.
    >0: Suspends the process until the lock is released, or until the end of the specified number of seconds.
        Default: cLockModeDefault.
+ fixed: TrimChar for Unicode String.
+ added: connection option 'coCatPrefix'.
    It option define format for 'odbc catalog prefix' in odbc connection string.
+ changed: Recognition of a name of the catalogue is changed.
    Now the catalogue can be taken from connection string.
+ fixed: GetOptionValue when Option is empty and it position is last.
+ added: support direct odbc 2 (tested on "IB6 XTG ODBC").
+ added: theoretical support nected transactions (fSupportsNestedTransactions).
    ODBC does not support nested transactions. But probably corresponding expansions odbc will appear.
+ fixed: read metadata when fSupportsCatalog = False (.FetchColumns, .FetchIndexes, ...).
+ added: Minimization of use of cursors when fStatementPerConnection is very small (SQL Server).
    SQLHStmt it is released compulsorily on achievement of the end of the cursor. By a call "Refresh()"
    the cursor will be automatically created with old characteristics ( TSqlCursorOdbc.next() ).
+ fixed: AV: when fStatementPerConnection > 0:
    Assignment of the incorrect Pointer for CurrentConnectionInfo
    ( TSqlCommandOdbc.execute(); TSqlCommandOdbc.ExecuteImmediate() ).
+ fixed: The library odbc the driver was not unloaded if there was a mistake of creation TSqlDriverOdbc.

Version 3.018, 2004-08-05  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ added: connection option 'coAutoInc'.
    Disabling this option allow update AutoInc fields.
    It is needed for migrating table data to other database/table.
+ fixed: Limitation of errors quantity for buffered fetch.
+ added: Partial support of killed connection (fConnectionClosed).
    It is critical for Application Server, etc
+ added: fetching only infomix opaque types.
+ recommendation: informix odbc ansi drivers (iclit09b.dll, iclit09a.dll)
    contain many bugs (SQLFetch, not supported BOOLEAN type).
    I recommend to use unicode versions of drivers (iclit09bw.dll, iclit09aw.dll).
    At use ODBC Manager it is necessary to correct links to the driver in the
    registry "HKEY_LOCAL_MACHINE\SOFTWARE\ODBC\ODBCINST.INI" or in 'ODBC.INI' file.
+ fixed: skip unnamed indexes: FetchIndexes().
+ added: support INTERSOLV ODBC Drivers:
    Paradox ( supports Paradox 3.0, 3.5, 4.0, 4.5, 5.0, 7.0, and 8.0 tables),
    Btrieve, DB2, dBase, Clipper, FoxPro, INFORMIX, OpenIngres, Oracle, Progress,
    SQLBase, SQLServer, Sybase, TXT.
+ added: connection option coFldReadOnly.
    Disabling this option("coFldReadOnly=1") allow update database tables when
    odbc driver returned uncorrect ReadOnly field attribute (Merant, Intersolv,
    DataDirect dBase, Paradox drivers). For the listed drivers this attribute is
    defined automatically if not has been specified at connection. Default is True.
+ added: connection option coParamDateByOdbcLevel2.
    Enabling this option("coParDateByLev3=1") allow set command datetime parameters
    follow Odbc Level 2.
    Default is False.
    For drivers (Merant, Intersolv, DataDirect dBase, Paradox) this attribute is
    defined automatically to True if it has not been specified at connection.
+ fixed: BCD2Str(): 1) Strip leading '0' chars; 2) Right trim '0' chars.
+ fixed: detecting oracle8 dbms.
+ fixed: read of synonyms for oracle ( TSqlCursorMetaDataTables.FetchTables(...) ).
    Oracle does not support concept of the scheme for a synonym. Example:
    'SELECT * FROM PUBLIC.ALL_CLUSTERS'
+ changed: The default of coSupportsSchemaFilter('coSchemFlt') parameter of the
    driver for Oracle more is not installed.
+ added: 'OdbcApi.pas'.LoadOdbcDriverManager():
    For WinNT only: In a case when VendorLib includes path to the library the
    driver automatically expands environment variables PATH that other libraries
    used VendorLib could load.
    Also added automated addition of path to BDE at usage of the odbc driver
    'PB Interlolv OEM Paradox' (PBIDP13.DLL).
+ fixed: TSqlCursorOdbc.getString
    Remove +1 / +SizeOf(WideChar) from Move

Version 3.019, 2004-08-13  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ fixed: TSqlCursorOdbc.getString(..).
    Uncorrect fetching of last symbol of fixed string. Thanks Luis Paulo.
- fixed: added CheckLastOSError when loading odbc driver.
    It is necessary in those cases when the driver is installed incorrectly
    (It does not work truly).
+ fixed: call SQLDriverConnectW when password is null. Thanks <daniel@mimer.se>.
+ added: define odbc level in driver name. example: "2:odbc32.dll".
    If it defined then it use for call
    SQLSetEnvAttr(SQL_ATTR_ODBC_VERSION...).
    And for this driver there is a new separate driver proxy.
+ added: support OTERRO RBase ODBC Drivers.
    For details see files dbxdrivers.ini, dbxconnections.ini, ChangesLog.Txt.
+ fixed: reimplemented handling datetime types for odbc level 2.
    (connection option coParamDateByOdbcLevel2 changed to coParamDateByOdbcLevel3.)
+ added: advanced detecting drivers with not supported multirows fetching mode
+ added: autohandled situation when driver not implemented SQLGetStmtAttr

Version 3.020, 2004-09-22  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ fixed: Daniel Gustafsson: Transaction management problem
    https://sourceforge.net/tracker/index.php?func=detail&aid=1021006&group_id=38250&atid=422094

Version 3.021, 2004-09-22  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ fixed: SQLFreeHandle(SQL_HANDLE_ENV) in module OdbcApi.pas for
    direct odbc mode when OdbcLevel = 2.
+ fixed: Loss HStmt in " function TSqlConnectionOdbc.connect(..)".
+ changed: BindResultSet(), RetrieveOdbcErrorInfo(). By call SQLDescribeCol(...) attempt of
    detection of a situation is done when is specified insufficient longer.

Version 3.022, 2004-10-22  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ changed: OdbcApi.pas: TRTLCriticalSection it is changed on TCriticalSection (at the request lionux developers).
+ fixed: Reading SQL_DBMS_VER version (detected on postgre Sql dbms). Changed:
    'TSqlConnectionOdbc.RetrieveDriverName', 'TSQLConnectionOdbc.GetMetaDataOption',
    'TSqlConnectionOdbc.GetOption' (GetConnServerVersion).

Version 3.023, 2004-11-08  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ fixed: compiling odbcapi.pas when undefined DynamicOdbcImport
+ added: "IOdbcApi" Interface for external access to actual OdbcApi. It will allow to have full
    access to all expansions ODBC.
+ fixed: Restrictions on length of line fields in SQLExpr are taken into account (BindresultSet).
+ added: The bug in function SQLBindParameter for odbc the driver postgreSQL is processed.
    The driver can accept only utf8 chars values and does not process unicode a chars.
+ added support unicode chars. This opportunity will be involved by the instructionin a line of
    connection of an option: "coEnableUnicode=1". Thus it is necessary to include correction
    "SqlExprFix.pas" in your project. Simple unicode lines will be accessible as "TWideStringField",
    and long as BLOB as in delphi are not present TWideMemoField. Thus not all odbc the driver
    will allow to update line fields as BLOB.
+ fixed: Overflow of the buffer at work with chars types is longer than dsMaxStringSize (8192).

Version 3.024, 2005-11-05  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ added support Delphi 2005 Win32.
+ added Connection Timeout option. Connection string example: "DSN=MyDsn;coConTimeout=60".
  added support connection parameter CONN_TIMEOUT
+ added: connection option coBcd2Exp:
    // https://sourceforge.net/forum/forum.php?thread_id=1251922&forum_id=119358
    Changed BCD2Str.
    In this option BCD will converted into exp format.
+ changed: Str2BCD. Removed dependency from DecimalSeparator.
+ added: connection option 'coNumSepr' - coNumericSeparator.
     It option allow define 'Decimal Separator' transmitted into odbc driver.
     Example (uses ... DbxOpenOdbcInterface;):
       SQLConnection1.SQLConnection.SetOption(
         TSQLConnectionOption(xeConnDecimalSeparator), Integer(Ord(','))
       );
     For " Oracle ODBC the Driver " DesimalSeparator is detected out automatically (look:
       function TSqlConnectionOdbc.RetrieveDbmsOptions).
+ added: connection option coSafeMode (default = true).
    (Maximum handling any errors. It is critical for app servers...).
+ changed: identify readonly fields.
+ fixed: coMixedFetch.

Version 3.025, 2006-01-12  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ Added support Delphi 2006 Win32 DBX Driver version 2.5 (Ansi Driver).
+ ReAllocation of memory at reading BLOBS is excluded (BLOB fragmentation mode).
  Connection option "coBlobFragmntns" (default is on).
  For detail see method "TSqlCursorOdbc.FetchLongData" .
+ Fixed: Fetcn BLOB binary and WideChar data (method "TSqlCursorOdbc.FetchLongData").
+ Processing of an option eConnNativeHandle is changed. For access to interface ISqlConnectionOdbc now it is
  necessary to use this option. For more details see ChangesLog.Txt.
+ NEW UNIVERSAL dbxoodbc connection method. Solved problem when DATABASENAME_KEY is more 255 chars.
  (Use CONNECTION_STRING is accessible for Unicode DBX drivers). Example:
  ...
  uses ..., SqlConst, DbxOpenOdbcInterface;
  ...
  MyConnectionString := 'Database=DRIVER={Microsoft Paradox Driver (*.db )};DefaultDir=E:\BORLAND\BORLAND SHARED\DATA\PDX5;DriverId=538;FIL=Paradox 5.X;MaxBufferSize=2048;PageTimeout=17;coLockMode=-1';
  SQLConnection1.Params.Values[CUSTOM_INFO] := cConnectionOptionsNames[coConnectionString] + '=' + MyConnectionString;
  SQLConnection1.Params.Values[DATABASENAME_KEY] := '?'; // '?' == ignored value when CONNECTION_STRING is defined.
  ...
  SQLConnection1.Connect;

Version 3.026, 2006-03-07  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ Added connection option 'coBlobNotTermChar' ( TSqlCursorOdbc.FetchLongData ).
  This option allows to process bug in ODBC drivers at reading of text blob fields (SYBASE).
+ Added check of null termination when fetching of text blob ( TSqlCursorOdbc.FetchLongData ).
+ Added detected: "Sybase System 11", "Adaptive Server Anywhere", "IBM DB2/400 SQL".
    Thanks: Ed Blanchard, Jose Belanger.

+ Fixed: coConTimeout   - Login Timeout -> SQL_ATTR_LOGIN_TIMEOUT
  Note:  coLockMode     - Lock  Timeout -> SQL_ATTR_QUERY_TIMEOUT
+ Added: coNetTimeout   - Query Timeout -> SQL_ATTR_CONNECTION_TIMEOUT; default == -1 (not used); 0 - there is no timeout.
+ Changed: Transaction:  xilDIRTYREAD - > SQL_TXN_READ_UNCOMMITTED

Version 3.027, 2006-06-15  [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
+ fixed default value for coEmptyStrParam ( == osOff).
+ fixed default value for coNullStrParam  ( == osOff).
+ fixed buffer oveflow in SqlExpr after call TSqlCursorOdbc.getColumnName
+ updated FastMM4.pas to 4.66 version

Version 3.028, 2008-02-05 [ Vadim V.Lopushansky pult@ukr.net ]
-------------------------
[https://sourceforge.net/forum/message.php?msg_id=4745244]:
+ fixed multiple cursors result for Microsoft SQL Server:
  TSqlCursorOdbc.ClearCursor, TSqlCursorOdbc.Next, TSqlCommandOdbc.ExecuteImmediate, TSqlCommandOdbc.Execute;
+ fixed check rows affected for ExecSql.
+ fixed errors check when not cursor returned: TSqlCommandOdbc.ExecuteImmediate, TSqlCommandOdbc.Execute.
+ added support: ASA MESSAGES CALLBACK: see DbxOpenOdbcASA.pas and directive _ASA_MESSAGE_CALLBACK_.
    https://sourceforge.net/tracker/index.php?func=detail&aid=1508015&group_id=38250&atid=422097
+ added demo: Demos\DBMS\MSSQL\MultipleCursors
+ updated FastMM4 to 4.78

Version 3.100, 2008-03-24: Beta
-------------------------
  + First version with DBX3 supports.
      //  Limitation:
      //    Information about metadata (names of tables, views, fields) given is processed by means of odbc ansi api.
      //    So if name table or fields are given in national symbol - a possible problems...
      //
  + fixed TSqlCursorOdbc.getColumnLength for TDateTime field (fldDATETIME).
  + fixed TSqlCommandOdbc.setParameter for null parameters and for CHAR, NCHAR, VARCHAR, NVARCHAR, BLOB, VARBINARY types.
  + fixed caching fRowsAffected after any exec query. This takes away error in CDS: record was changed by other user.
  + fixed bind long string parameters (>255) (TSqlCommandOdbc.setParameter).
  + fixed widestring symbol length information
  + fixed TSqlCursorMetaData.OdbcCheck
  + fixed multiple cursors: skip result sets without columns (MSSQL)
  + fixed fetch nvarchar memo (TSqlCursorOdbc.FetchLongData)
  + fixed TSqlCursorOdbc.isAutoIncrement
  + fixed setting parameters fldBYTES, fldVARBYTES, fldBLOB (function TSqlCommandOdbc.setParameter).
  + fixed update DATETIME type. Not correctly calculated driver option coParamDateByOdbcLevel3.
  + fixed set null bcd parameter for MSSQL
  + fixed update MSSQL sql_varian field type (set null into parameter: "TSqlCommandOdbc.setParameter").
      //  It type (sql_varian) is mapped into delphi field type ftString/ftWideString.
      //
  + fixed TSqlCursorOdbc.isReadOnly
  + fixed calculate field size for SQL_GUID column type.
  + fixed DbxOpenOdbcTrace.pas for WideString
  + fixed TSqlDriverOdbcmethods: setOption, getOption
  + fixed fetch LateBounds data when previous data is not fetched. Thanks to Kvitnitskiy V. Vladimir.
  + fixed bug in odbc drivers "SQL SERVER 2000/2005" ('sqlsrv32.dll'/'sqlncli.dll').
      Driver report length of string field is equal 0 for next query:
      ---
      declare @content varchar(max)
      select @content = 'hello world'
      select content1 = @content
      ---
  + unprepare stmt when call prepare/execute returned error
      //
  + reimplemented static linking of dbxdriver into application (DbxOpenOdbcStatic.pas, DbxOpenOdbcApiHook.pas)
     // it work also on Delphi2007 Win32.
     // For static linking of the driver in your exhibit, it is enough, to indicate "DbxOpenOdbcStatic" in section "unit".
  + added support connection option 'Prepare SQL' ( eConnPrepareSQL ) (Delphi 2005 UP).
  + added full support of the callback. Warning: Delphi 2006 contain bug for callback for dbx ansi driver and
      //  for SqlExpr.pas:TSQLMonitor.UpdateTraceCallBack.
      //  For Delphi 2006 ansi driver callback support is disabled. And calback is disabled when TraceCat==trSTMT.
      //  For details see method DbxOpenOdbc.pas:TSqlConnectionOdbc.DbxCallBackSendMsg.
      //  demo: Demos\DBMS\MSACCESS\SQL.
  + added support multiple quotation schemes of database object. The "DbxObjectParser.pas" is changed.
      //  for sqlserver added schema: < [catalog].[schema].[object] > .
  - added support mssql datatype "xml" but not tested it.
      //
  - dbx3: untested function TSqlCommandOdbc.getParameter for types
      //  fldWIDESTRING, fldINT32, fldUINT32, fldINT16, fldUINT16, fldINT64, fldUINT64, fldFLOAT, fldBOOL,
      //  fldBLOB, fldBYTES, fldVARBYTES
  + dbx3: Int64 Field type support, but:
      //  Delphi not correctly supported Int64 (TLargeintField). See db.pas: "function TParam.GetDataSize: Integer;".
      //  The Field it is impossible will change since it is impossible calculate parameter data size.
      //  Int64 is mapped to Float.
      //  When is connection options "coMapInt64ToBCD=1" then Int64 is mapped to BCD ( numeric(18,0) ).
      //  Full-fledged support can be included only after corrections mistake in Delphi. If mistake will is corrected,
      //  will possible include full-fledged support by means of directives _INT64_BUGS_FIXED_.
  + dbx3: fixed fetch fixed nchar field types (fixed width of ftWideString)
  + added additional driver dbx using ansi or unicode odbc api for any one of dbx driver dbx2, dbx3:
    dbx2:
      //  priority ansi odbc api
      //  getSQLDriverODBC - priority ansi odbc api
      //  getSQLDriverODBCAW - priority unicode odbc api
    dbx3:
      //  getSQLDriverODBCW - priority unicode odbc api
      //  getSQLDriverODBCWA - priority ansi odbc api
  + ddditional record will added in "dbxdrivers.ini, dbxconnections.ini" for access to MSAccess.
      //  -  dbxdrivers.ini
      //      [DbxMSJetAccess]
      //          getSQLDriverODBC   - priority ansi odbc api
      //      [DbxMSJetAccessAW]
      //          getSQLDriverODBCAW - priority unicode odbc api
      //      [DbxMSJetAccessW]
      //          getSQLDriverODBCW  - priority unicode odbc api
      //      [DbxMSJetAccessWA]
      //          getSQLDriverODBCWA - priority ansi odbc api
      //  - dbxconnections.ini
      //      [Dbx:MSJet:Access]
      //      [Dbx:MSJet:Access:AW]
      //      [Dbx:MSJet:Access:W]
      //      [Dbx:MSJet:Access:W:AnsiField]
      //          Custom String=...;coEnableUnicode=0
      //      [Dbx:MSJet:Access:WA]
      //      [Dbx:MSJet:Access:WA:AnsiField]
      //          Custom String=...;coEnableUnicode=0
  + added demos:
      //  Demos\DBMS\MSSQL\TableEdit
      //  Demos\DBMS\MSACCESS\Connect - connect to MSAccess
      //  Demos\DBMS\MSACCESS\SQL - Dbx3 access demo to MSAccess
  + adapted demos
      //  Demos\DbExplor\OdbcExplor.dpr

Version 3.200, 2008-09-18: Beta
-------------------------
  + Added support Delphi 2009
     Delphi 2009 require dbxdrivers.ini at design/runtime and requires presence of section for your driver name.
     look "dbxdrivers.ini" at path
     search for "dbxdrivers.ini" at path from registry:
       ;[HKEY_CURRENT_USER\Software\CodeGear\BDS\6.0\DBExpress]
       ;"Driver Registry File"="C:\\Programm Files\\CodeGear Rad Studio 2009\\dbxdrivers.ini"
       ;"Connection Registry File"="C:\\Programm Files\\CodeGear Rad Studio 2009\\dbxconnections.ini"
       ;[HKEY_LOCAL_MACHINE\SOFTWARE\CodeGear\BDS\6.0\DBExpress]
       ;"Driver Registry File"="C:\\Programm Files\\CodeGear Rad Studio 2009\\dbxdrivers.ini"
       ;"Connection Registry File"="C:\\Programm Files\\CodeGear Rad Studio 2009\\dbxconnections.ini"
     "dbxdrivers.ini" changes like:
       [Installed Drivers]
       ...
       DbxWSQLServer=1
       DbxWOracle=1
       DbxWMSOracle=1
       ...
       ...
       ...
       [DbxWOracle]
       DriverUnit=DBXDynalink
       DriverPackageLoader=TDBXDynalinkDriverLoader
       DriverPackage=DbxCommonDriver120.bpl
       ;--
       LibraryName=dbxoodbc.dll
       GetDriverFunc=getSQLDriverODBCW
       VendorLib=SQORA32.DLL
       DbxWOracle TransIsolation=ReadCommited
       Database=DBQ=SMROL;DBA=W;APA=T;FEN=T;QTO=T;FRC=10;FDL=10;LOB=T;RST=T;FRL=F;MTS=F;CSR=F;PFC=10;TLO=0;UID=Anonymous;PWD=Unknown
       User_Name=user
       Password=password
       RowsetSize=20
       BlobSize=-1
       ; Delphi10Up
       Trim Char=True
       Custom String=coNetPacketSize=8192;coLockMode=-1;coBlobChunkSize=40960;coSchemFlt=0;coCatPrefix=UID

       [DbxWOracle TransIsolation]
       DirtyRead=0
       ReadCommited=1
       RepeatableRead=2

       [DbxWMSOracle]
       DriverUnit=DBXDynalink
       DriverPackageLoader=TDBXDynalinkDriverLoader
       DriverPackage=DbxCommonDriver120.bpl
       ;--
       LibraryName=dbxoodbc.dll
       GetDriverFunc=getSQLDriverODBCW
       VendorLib=MSORCL32.DLL
       DbxWMSOracle TransIsolation=ReadCommited
       Database=DATABASE=dbdemos;FRL=T
       User_Name=user
       Password=password
       RowsetSize=20
       BlobSize=-1
       Trim Char=True
       Custom String=coNetPacketSize=8192;coLockMode=-1;coBlobChunkSize=40960;coCatPrefix=DATABASE
       [DbxWMSOracle TransIsolation]
       DirtyRead=0
       ReadCommited=1
       RepeatableRead=2

       [DbxWSQLServer]
       DriverUnit=DBXDynalink
       DriverPackageLoader=TDBXDynalinkDriverLoader
       DriverPackage=DbxCommonDriver120.bpl
       ;--
       LibraryName=dbxoodbc.dll
       GetDriverFunc=getSQLDriverODBCW
       VendorLib=sqlsrv32.dll
       DbxWSQLServer TransIsolation=ReadCommited
       ; 'OS Authentication' option in odbc connection string: Trusted_Connection=Yes
       ; Other options in connection string: LANGUAGE=Ukrainian;QueryLog_On=Yes
       Database=DATABASE=dbdemos;Trusted_Connection=Yes
       User_Name=user
       Password=password
       RowsetSize=20
       BlobSize=-1
       ; Delphi10Up
       Trim Char=True
       Custom String=coICloneCon=1;coNetPacketSize=8192;coLockMode=17;coCatPrefix=DATABASE
       [DbxWSQLServer TransIsolation]
       DirtyRead=0
       ReadCommited=1
       RepeatableRead=2
  + DbxObjectParser.pas: optimization and fixed regexp expression for mssql
  + fixed read stored proc parameters for Db2AS400 (eOdbcDriverTypeIbmDb2AS400)
  + update FastMM4.pas to 4.90
  + chahged RegExprParser.zip - regexp tools (for DbxObjectParser.pas).
  + fixed stored proc for unicode

Version 3.201, 2008-09-26: Beta
-------------------------
  + Fixed Delphi 2009 compiler bug (DbxObjectParser.pas).
  + Adapted oracle demo "RefCursor" to Delphi 2009 and other dbx oracle drivers.

TODO:
****

- MS SQL Server ODBC Error:
    If in options ODBC alias to clean the checkbox " Use ANSI quoted identifiers ", then metadata
    in OdbcTest3.exe will be all the same taken in inverted commas "". But performance of inquiries
    of type:
      select * from "dbo." "syscolumns"
    will be impossible. Probably a mistake in ODBC driver - it should not return a quoting
    character !!!

    Line:
      OdbcRetCode := SQLGetInfoString(fhCon, SQL_IDENTIFIER_QUOTE_CHAR, @GetInfoStringBuffer, ...
    returned:
      GetInfoStringBuffer[0]=='"'

- INFORMIX:
  1) "odbc manager" or "odbc direct mode":
    SELECT aggid, handlesnulls FROM sysaggregates
    Native Error Code: -9628;
    Informix Type (%s) not found.

    9628:
    Type (type_name) not found.
    The specified type_name could not be found. Before you can use an opaque type,
    you must create it with the CREATE OPAQUE TYPE statement.

    @@: It is bug for ansi driver, but work fine for unicode drivers (iclit09bw.dll;iclit09aw.dll).

  2)
  LVARCHAR field "m2" cannot be changed:

  function TSqlCommandOdbc.setParameter(
    ...
    fldBLOB:
    ...
      fOdbcParamSqlType := SQL_LONGVARCHAR;
    Does not work CDS.ApplyUpdates for a field of type SQL_VARCHAR (in Database LVARCHAR):
      Error: ... no cast from text to lvarchar.

  If to specify fOdbcParamSqlType := SQL_VARCHAR that updating for this type of a field will pass,
  but for SQL_LONGVARCHAR will cease to work.

  Example table:
  CREATE TABLE test
  (
   id SERIAL,
   m1 TEXT,
   m2 LVARCHAR,
   m3 BLOB
  );
  alter table test add constraint primary key (id) constraint ct_pr_test0;

  BLOB field "m3" cannot be changed:
  ERROR:
  Error returned from ODBC function SQLExecute
  ODBC Return Code: -1: SQL_ERROR
  ODBC SqlState:        HY000
  Native Error Code:    -609
  [Informix][Informix ODBC Driver][Informix]Illegal attempt to use Text/Byte host variable.

  3)
  Hung Informix odbc "version: 3.81" for query ( hung SQLFetch(...) ):
    "select first 1 procid, paramtypes, percallcost, commutator from sysprocedures".
  @@: It is bug for ansi driver, but work fine for unicode drivers (iclit09bw.dll;iclit09aw.dll).

// *)
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
