Installation Instructions

Create a directory  ..\Delphi7\Source\dbExpressPlus.
Copy all source files to the new directory.
Open dbExprPlus_r70.dpk and compile.
Open dbExprPlus_d70.dpk, compile, and install.
Go To “Tools|Environment Options” menu item and open the Environment page.
Go To the Library tab, Library Path setting and add
$(DELPHI)\Source\dbExpressPlus

Overview dbExpress Plus

dbExpress Plus is an extension of the standard dbExpress components shipped with Delphi 6 + and Kylix.  When migrating from BDE database access components, you will find several important pieces missing to make a smooth migration.  The two components obviously missing are TSession and TBatchMove. The session component has multiple methods for retrieving metadata information.  This capability is available in dbExpress, but buried in complex methods and algorithms.  TSQLMetaData encapsulates this capability into easy to use methods.  TBatchMove’s real capabilities are housed inside the BDE.  TSQLDataPump replaces the TBatchMove and adds ASCII import/export capability.  

In addition to replacing missing functionality, the component suite adds new capabilities.  Also included is a new component essential to SQL programming, TSQLScript.  This component executes one or more SQL statements and a procedure.  This component is ideal for non-SQL select statements and metadata statements.  SQLFilter allows you to add an end user SQL “Where” clause wizard to your program.  SQLMultiDbQuery is a component aimed at developers of systems that are multi database compliant.  DBNavPlus is an enhanced version of Borland’s standard DBNavigator.  You may also find other components that are R&D studies or works in progress.   Please feel free to use them and give us your feedback.  

The dbExpress Plus suite is open freeware project released under IBM’sCommon Public License Version 0.5.  This license was chosen because it allows for more liberal use of code in commercial software then other more popular open source licenses.  You can find the project at sourceforge.net/projects/dbexpressplus. 

For more information on participating in the development of the dbExpress Plus Suite please contact Thomas Miller tmiller@bss-software.com.  

Note:  Runtime only properties are signified by an asterisk.

SQLScript

Overview


SQLScript runs scripts made up of multiple SQL statements.  It can also be used to run a single procedure or function.  This mode is especially useful for metadata create statements for stored procedures and triggers.
 
Properties
About: String– Version of the software.  This property will be updated to display a dialog box with contributor and update information.
CommitEach: Boolean– Uses the transaction object assigned to the property TransDesc to start a transaction and then executes a commit for each SQL statement in the script.
CurrentSQL*: String– The current SQL statement being executed.
Debug: Boolean– When True, each SQL statement is copied to the clipboard and displayed before it is executed.    
Name: String– The name of the component.
RecordsAffected *: LongWord– The total number of records effected.
SQLProc: Boolean - Default is False.  When true, the SQL statement is processed as one SQL statement.  In this mode, colons and semi-colons are ignored and assumed to be part of the whole.
SQLConnection: TSQLConnection - Use SQLConnection to specify an SQL connection object that connects the dataset to a database server.
SQL: TStrings - Use the properties and methods of SQL to specify the SQL Script or other valid SQL statement.
Tag: LongInt - Tag has no predefined meaning. The Tag property is provided for the convenience of developers. It can be used for storing an additional integer value or it can be typecast to any 32-bit value such as a component reference or a pointer.

Methods
ExecuteDirect: LongWord– Call ExecuteDirect to execute the script in the SQL property.  Returns the number of records affected.

SQLMetaData

Overview
SQLMetaData gives you quick and easy access to metadata information.  Inherited from TSQLConnection. For information on properties, methods, and events introduced in TSQLConnection or one of its ancestors, please refer to the appropriate Borland help file.

Properties
About: String– Version of the software.  This property will be updated to display a dialog box with contributor and update information.
MetaDataFilter: String– Is an SQL pattern mask that filters the resulting metadata object names.

Methods
GetTableNames(AList: TStrings)– Populates a TStrings compatible object with a list of Table names visible to the user logged in.
GetSysTableNames(AList: TStrings)– Populates a TStrings compatible object with a list of System Table names visible to the user logged in.
GetViewNames(AList: TStrings)– Populates a TStrings compatible object with a list of View names visible to the user logged in.
GetSynonymNames(AList: TStrings)– Populates a TStrings compatible object with a list of Synonym names visible to the user logged in.
GetFieldNames(const ATableName: string; AList: TStrings; ASortOrder: TMetaDataSortOrder = soPosition)– Populates a TStrings compatible object with a list of field names visible to the user logged in. The list is sorted either by name or by column position within the database object (soName, soPosition).  The default is by column position. 
GetFieldMetaData(const ATableName, AColumnName: string): TFieldMetaData– Populates the following record structure with the appropriate information for the Table and Column given.

TFieldMetaData = record
    ColumnName: string;
    ColumnPosition: LongInt;
    ColumnDataType: LongInt;
    ColumnTypeName: string;
    ColumnSubtype: LongInt;
    ColumnLength: LongInt;
    ColumnPrecision: LongInt;
	   ColumnScale: LongInt;
 	   ColumnNullable: LongInt;  // 1=Not Nullable, 0=Nullable
  	end;

GetFieldMetaData(const ATableName: string; AFieldIndex: Integer): TFieldMetaData - Populates the following record structure with the appropriate information for the Table and Column Index given (previously named GetTableMetaData).

TFieldMetaData = record
    ColumnName: string;
    ColumnPosition: LongInt;
    ColumnDataType: LongInt;
    ColumnTypeName: string;
    ColumnSubtype: LongInt;
    ColumnLength: LongInt;
    ColumnPrecision: LongInt;
	   ColumnScale: LongInt;
 	   ColumnNullable: LongInt;  // 1=Not Nullable, 0=Nullable
  	end;

GetPrimaryKeyFieldNames(const ATableName: string; AList: TStrings)– Populates a TStrings compatible object with a list of Column names comprising the key of the Table given. 
GetPrimaryKeyFields(const ATableName: string): string– Returns a string with a list of Column names comprising the key of the Table given.  A semicolon separates the column names.
GetIndexNames(const ATableName: string; AList: TStrings)– Populates a TStrings compatible object with a list of Index names related to the Table given.
GetIndexFieldNames(const ATableName, AIndexName: string; AList: TStrings) – Populates a TStrings compatible object with a list of Column names comprising the key of the Table and Index name given. 

GetIndexFields(const ATableName, AIndexName: string): string – Returns a string with a list of Column names comprising the Index given.  A semicolon separates the column names. 
GetInsertStatement(const ATableName: string; ASQL: TStrings): Integer– Populates a TStrings compatible object with an SQL insert statement for the Table given.  It also returns the index of the first line that will need to be set with an actual value.  For instance, if the table has 3 columns, then the return value would be 5.

INSERT INTO
mytable
(column_1,
column_2,
column_3) VALUES
value_for_column_1,     // Index 5
value_for_column_2,
value_for_column_3)

GetUpdateStatement(const ATableName: string; ASQL: TStrings): Integer– Populates a TStrings compatible object with an SQL update statement for the Table given.  It also returns the index of the first line that will need to be set with an actual value.  For instance, if the table has 3 columns, then the return value would be 3.

UPDATE
mytable
SET
column_1 = new_value,     // Index 3
column_2 = new_value,
column_3 = new_value

GetSelectStatement(const ATableName: string; ASQL: TStrings): Integer – Populates a TStrings compatible object with an SQL select statement for the Table given.  It also returns the index of the line that contains the table name.  For instance, if the table has 3 columns, then the return value would be 5.

SELECT
column_1,
column_2,
column_3
FROM
Mytable     //  Index 5

SQLDataPump

Overview
Move data from one table to another table, from an ASCII file to a table, and from a table to an ASCII file. 

Properties

AbortOnException: Boolean– When true, on an exception, the system halts.  When false, the system ignores the error and continues on with the data pumping.  If ExceptionFileAction is set to efaCreate or efaAppend, an entry will be made into the log file specified in the ExceptionFile property.
About: String– Version of the software.  This property will be updated to display a dialog box with contributor and update information.
AsciiDataDef: TCollection - The AsciiDataDef property is where you decribe the format of the data stored or to be stored in the Ascii file.   and tell the system how to pump data into each individual field.  There are three options:  From another datasource (a source column), a literal value, or by assigning the value dynamically through the OnBeforeFieldPump event.

AsciiDataType: TAsciiDataType – The Ascii data type (dtDate, dtDateTime, dtNumber, dtOther, dtString, dtTime).
AsciiFieldName: String– This is for reference only.
AsciiFieldSize: Integer– The maximum size the value requires for storage in a fixed length file format.
AsciiFormatOption: TAsciiFormatOption -  Options are:  foNone, foBlank, foIgnore, foZeroFillNum, foCurrencyNum, foNumberDate, foShortDate, foLongDate, foEuroDate.  This feature is not yet implemented.
ExportSourceDataColumn: String– This is the source and destination data column name. The value is taken from here when the SourceValueType is set to fvtColumn.
ExportSourceDataLiteral: String– A literal value.  The value is taken from here when the SourceValueType is set to fvtLiteral.
SourceValueType: TFieldValueType– Options are fvtColumn and fvtLiteral.  
AsciiDateSeparator: Char– Character used to separate date segment for short date formats.
AsciiDelimiter: Char– Character used for delimiting a String and / or Date.   This is also used for numbers in certain export formats.
AsciiExportRefTable: String– When assigned, it allows the AsciiFieldDef property to use this table as a reference when setting the individual columns of the ASCII file.
AsciiFileName: String– The name of the file that the system is exporting too or retrieving data from.
AsciiRecordDelimiter: TAsciiRecordDelimiter– Options are rdDos or rdUnix.  rdDos ends each line with carriage return and line feed.  rdUnix only uses line feed.
AsciiRecordFormat: TAsciiRecordFormat– Options are:  ffFixedLength, ffCommaDoubleQuote, ffFieldPerLine, ffTabDoubleQuote, ffCustom.  
AsciiSeparator: Char– Character used for distinguishing when on field ends and another begins.
ClearDestination: Boolean - When true, the destination table will be cleared of all records.  If ConfirmClear is set to true, the user will be prompted to clear the table (Yes, No, Abort).
CommitCount: LongInt - Default is one.  This will commit each record as it is pumped.  When less then one, all records are commited in one transaction.  When set to other positive numbers, the systems commits in batches of that count. If set to 25, the system will commit records in batches of 25.  If ExceptionFileAction is in an active mode (other then “efaNone”), then the CommitCount must be equal to one.  If not, the system will not be able to tell which record caused the exception.
ConfirmClear: Boolean - When ClearDestination is set to true and ComfirmClear is set to true, the user will be prompted to clear the table (Yes, No, Abort).
DestinationDateTimeFormat: String - A custom DateTime format for moving date data type values.  The system will attempt to set the appropriate format based on the database of the SQLMetaDataDestination object.
DestinationFields: TCollection - The DestinationFields property is where you declare the destination fields and tell the system how to pump data into each individual field.  There are three options:  From another datasource (a source column), a literal value, or by assigning the value dynamically through the OnBeforeFieldPump event.

FieldName: String– The destination field name.
SourceColumn: String– The source column name.  The value is taken from here when the SourceValueType is set to fvtColumn.
SourceLiteral: String– A literal value.  The value is taken from here when the SourceValueType is set to fvtLiteral.
SourceValueType: TFieldValueType– Options are fvtColumn and fvtLiteral.  Values assigned in OnBeforeFieldPump take precidents over both design time value types (fvtColumn and fvtLiteral).
OnBeforeFieldPump – Set the destination value before an insert or update statement is executed.

DestinationTable: String - The table where the data is being pumped to.
DataMoveMode: TDataMoveMode - There are three option: sdpTableToTable, sdpTableToAscii, and sdpAsciiToTable.
ExceptionFileAction: TExceptionFileAction - There are three options:  efaCreate, efaAppend, and efaNone.  Create mode creates a new log file.  Append mode appends new log information to an existing file, or creates a new file if the file doesn’t already exist.  In None mode, exception information is not sent to a file.
ExceptionFileName: String - The name of the file exceptions will be written to.
InsertCount: LongInt* – The number of records inserted into the destination table or an ASCII file.  
ReadCount: LongInt*– The number of records read from the source table or source ASCII file.
ShowRunningStatistics: TDataMoveStatistic - A “Running Statistics” panel can be shown during the pumping process to provide visual feedback to the user.  Options are: dmsReadWrite, dmsReadUpdateInsert, and dmsNone.  When in Read/Write mode, the system shows the progress of the number of records read from the source and the number or records either inserted or updated for the destination.  Read/Update/Insert mode shows the records Read, Updated, and Inserted separately.  In None mode, a Running Statistics panel is not displayed.
ShowSummaryStatistic: TDataMoveStatistic - A summary statistics dialog box can be displayed at the completion of pumping. When in Read/Write mode, the system shows the total of the number of records read from the source and the number or records either inserted or updated for the destination.  Read/Update/Insert mode shows the records Read, Updated, and Inserted separately.  In None mode, a summary statistics dialog box is not displayed.  
SQLDataPumpMode: TSQLDataPumpMode - The mode in which the component moves data from the source to the destination.  Options are: dmAlwaysInsert, dmAppend, dmUpdate, dmAppendUpdate, and dmDelete.  Always Insert mode always inserts each record.  This eliminates the time it takes to check if the record already exists so the system knows whether to perform an update or an insert.  This is most often used on empty tables or exporting to ASCII files.  Append mode adds the record to the table or ASCII file.  Update mode, only updates existing records.  New records are discarded.  This mode is not available when exporting to an ASCII file (DataMoveMode = TableToAscii).  Append and Update mode updates existing records and inserts new records.  This mode is also unavailable when exporting to an ASCII file (DataMoveMode = TableToAscii).  Delete mode removes records from the destination table that exist in the source table.  Again, this mode is not available when exporting to an ASCII file (DataMoveMode = TableToAscii).
SQLMetaDataDestination: TSQLMetaData - An SQLMetaData component.
SQLMetaDataSource: TSQLMetaData - An SQLMetaData component.
SQLSource: TStrings - A valid SQL statement.  When DataMoveMode is either sdpTableToTable or sdpTableToAscii, this statement is the source of data for the destination object (either Table or ASCII).
StatisticsCaption: String - The caption of the Summary Statistics Dialog Box.
StatisticsInterval: LongInt - The frequency in which the Running Statistics panel should be updated.  This is based on the records read.
UpdateCount: LongInt* - The number of records updated in the destination table.  Records can’t be updated in an ASCII file.
UseTransaction: Boolean - The default is True.  
WriteCount: LongInt* - The number of records inserted and updated in the destination table or inserted into an ASCII file.

Methods

SetDataPumpTable(TableName: String)– This prepares the Destination table, Destination Fields, and SQL Source for the pumping process.  This should only be used on tables that match exactly.
GetCurrentSQLStatement: String– Only available when pumping process is terminated by an exception.  Returns the last failed SQL statement.  This accommodates external reporting of a failed SQL statement.  
GetSourceQuery: TSQLQuery– Returns the DataSet object used by the source SQL statement.
Execute: LongInt– Starts the pumping process and returns the total number of records moved.

SQLMultiDBQuery

Overview
SQLMultiDBQuery is specifically designed to be used for systems that run against multiple databases.  The component allows you to store vendor database specific queries and then runs the proper query at run time.  Inherited from TSQLQuery. For information on properties, methods, and events introduced in TSQLQuery or one of its ancestors, please refer to the appropriate Borland help file.

Properties
About: String– Version of the software.  This property will be updated to display a dialog box with contributor and update information.
SQLFirebird: TStrings - An SQL query using the syntax appropriate for the open source Firebird database.
SQLIBMAS400: TStrings– An SQL query using the syntax appropriate for an IBM AS/400 database.
SQLIBMDB2: TStrings - An SQL query using the syntax appropriate for an IBM DB2 database.
SQLInformix: TStrings - An SQL query using the syntax appropriate for an IBM Informix database.
SQLInterbase: TStrings - An SQL query using the syntax appropriate for an Borland Interbase database.
SQLMSSQLServer: TStrings - An SQL query using the syntax appropriate for an Microsoft SQL Server database.
SQLMySQL: TStrings - An SQL query using the syntax appropriate for the open source MySQL database.
SQLOracle: TStrings - An SQL query using the syntax appropriate for an Oracle database.
SQLOther: TStrings - An SQL query using the syntax appropriate for a database vendor not currently supported by the TSQLMultiDbQuery component.
SQLPostgres: TStrings - An SQL query using the syntax appropriate for the open source Postgres database.
SQLSQLAnywhere: TStrings - An SQL query using the syntax appropriate for a Sybase SQL Anywhere database.
SQLSybase: TStrings - An SQL query using the syntax appropriate for a Sybase database.

Methods
ResetSQL– Sets the SQL property based on the value in SQLConnection.Params.Values['DatabaseVendor'].  The DatabaseVendor is a string value matching the appropriate specific SQL database property.  For example, the Property SQLFirebird would be shortened to just ‘Firebird’ for the DatabaseVendor value.


