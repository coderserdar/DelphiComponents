{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MsSql Database metadata information           }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcDbLibMetadata;

interface

{$I ZDbc.inc}

uses
  Types, Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZDbcMetadata,
  ZCompatibility, ZSelectSchema;

type

   IZDbLibDatabaseInfo = Interface(IZDataBaseInfo)
     ['{A99F9433-6A2F-40C8-9D15-96FE5632654E}']
     procedure InitIdentifierCase(const Collation: String);
   End;
  // technobot 2008-06-25 - methods moved as is from TZDbLibBaseDatabaseMetadata:
  {** Implements MsSql Database Information. }
  TZDbLibDatabaseInfo = class(TZAbstractDatabaseInfo, IZDbLibDatabaseInfo)
  private
    fCaseIdentifiers: TZIdentifierCase;
  public
    procedure InitIdentifierCase(const Collation: String);
  public
    // database/driver/server info:
    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    function GetDriverName: string; override;
//    function GetDriverVersion: string; override; -> Same as parent
    function GetDriverMajorVersion: Integer; override;
    function GetDriverMinorVersion: Integer; override;
//    function GetServerVersion: string; -> Not implemented

    // capabilities (what it can/cannot do):
//    function AllProceduresAreCallable: Boolean; override; -> Not implemented
//    function AllTablesAreSelectable: Boolean; override; -> Not implemented
    function SupportsMixedCaseIdentifiers: Boolean; override;
    function SupportsMixedCaseQuotedIdentifiers: Boolean; override;
//    function SupportsAlterTableWithAddColumn: Boolean; override; -> Not implemented
//    function SupportsAlterTableWithDropColumn: Boolean; override; -> Not implemented
//    function SupportsColumnAliasing: Boolean; override; -> Not implemented
//    function SupportsConvert: Boolean; override; -> Not implemented
//    function SupportsConvertForTypes(FromType: TZSQLType; ToType: TZSQLType):
//      Boolean; override; -> Not implemented
//    function SupportsTableCorrelationNames: Boolean; override; -> Not implemented
//    function SupportsDifferentTableCorrelationNames: Boolean; override; -> Not implemented
    function SupportsExpressionsInOrderBy: Boolean; override;
    function SupportsOrderByUnrelated: Boolean; override;
    function SupportsGroupBy: Boolean; override;
    function SupportsGroupByUnrelated: Boolean; override;
    function SupportsGroupByBeyondSelect: Boolean; override;
//    function SupportsLikeEscapeClause: Boolean; override; -> Not implemented
//    function SupportsMultipleResultSets: Boolean; override; -> Not implemented
//    function SupportsMultipleTransactions: Boolean; override; -> Not implemented
//    function SupportsNonNullableColumns: Boolean; override; -> Not implemented
//    function SupportsMinimumSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsCoreSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsExtendedSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsANSI92EntryLevelSQL: Boolean; override; -> Not implemented
//    function SupportsANSI92IntermediateSQL: Boolean; override; -> Not implemented
//    function SupportsANSI92FullSQL: Boolean; override; -> Not implemented
    function SupportsIntegrityEnhancementFacility: Boolean; override;
//    function SupportsOuterJoins: Boolean; override; -> Not implemented
//    function SupportsFullOuterJoins: Boolean; override; -> Not implemented
//    function SupportsLimitedOuterJoins: Boolean; override; -> Not implemented
    function SupportsSchemasInDataManipulation: Boolean; override;
    function SupportsSchemasInProcedureCalls: Boolean; override;
    function SupportsSchemasInTableDefinitions: Boolean; override;
    function SupportsSchemasInIndexDefinitions: Boolean; override;
    function SupportsSchemasInPrivilegeDefinitions: Boolean; override;
    function SupportsCatalogsInDataManipulation: Boolean; override;
    function SupportsCatalogsInProcedureCalls: Boolean; override;
    function SupportsCatalogsInTableDefinitions: Boolean; override;
    function SupportsCatalogsInIndexDefinitions: Boolean; override;
    function SupportsCatalogsInPrivilegeDefinitions: Boolean; override;
    function SupportsPositionedDelete: Boolean; override;
    function SupportsPositionedUpdate: Boolean; override;
    function SupportsSelectForUpdate: Boolean; override;
    function SupportsStoredProcedures: Boolean; override;
    function SupportsSubqueriesInComparisons: Boolean; override;
    function SupportsSubqueriesInExists: Boolean; override;
    function SupportsSubqueriesInIns: Boolean; override;
    function SupportsSubqueriesInQuantifieds: Boolean; override;
    function SupportsCorrelatedSubqueries: Boolean; override;
    function SupportsUnion: Boolean; override;
    function SupportsUnionAll: Boolean; override;
    function SupportsOpenCursorsAcrossCommit: Boolean; override;
    function SupportsOpenCursorsAcrossRollback: Boolean; override;
    function SupportsOpenStatementsAcrossCommit: Boolean; override;
    function SupportsOpenStatementsAcrossRollback: Boolean; override;
    function SupportsTransactions: Boolean; override;
    function SupportsTransactionIsolationLevel(const {%H-}Level: TZTransactIsolationLevel):
      Boolean; override;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; override;
    function SupportsDataManipulationTransactionsOnly: Boolean; override;
    function SupportsResultSetType(const {%H-}_Type: TZResultSetType): Boolean; override;
    function SupportsResultSetConcurrency(const {%H-}_Type: TZResultSetType;
      const {%H-}Concurrency: TZResultSetConcurrency): Boolean; override;
//    function SupportsBatchUpdates: Boolean; override; -> Not implemented

    // maxima:
    function GetMaxBinaryLiteralLength: Integer; override;
    function GetMaxCharLiteralLength: Integer; override;
    function GetMaxColumnNameLength: Integer; override;
    function GetMaxColumnsInGroupBy: Integer; override;
    function GetMaxColumnsInIndex: Integer; override;
    function GetMaxColumnsInOrderBy: Integer; override;
    function GetMaxColumnsInSelect: Integer; override;
    function GetMaxColumnsInTable: Integer; override;
    function GetMaxConnections: Integer; override;
    function GetMaxCursorNameLength: Integer; override;
    function GetMaxIndexLength: Integer; override;
    function GetMaxSchemaNameLength: Integer; override;
    function GetMaxProcedureNameLength: Integer; override;
    function GetMaxCatalogNameLength: Integer; override;
    function GetMaxRowSize: Integer; override;
    function GetMaxStatementLength: Integer; override;
    function GetMaxStatements: Integer; override;
    function GetMaxTableNameLength: Integer; override;
    function GetMaxTablesInSelect: Integer; override;
    function GetMaxUserNameLength: Integer; override;

    // policies (how are various data and operations handled):
//    function IsReadOnly: Boolean; override; -> Not implemented
//    function IsCatalogAtStart: Boolean; override; -> Not implemented
    function DoesMaxRowSizeIncludeBlobs: Boolean; override;
//    function NullsAreSortedHigh: Boolean; override; -> Not implemented
//    function NullsAreSortedLow: Boolean; override; -> Not implemented
//    function NullsAreSortedAtStart: Boolean; override; -> Not implemented
//    function NullsAreSortedAtEnd: Boolean; override; -> Not implemented
//    function NullPlusNonNullIsNull: Boolean; override; -> Not implemented
//    function UsesLocalFiles: Boolean; override; -> Not implemented
    function UsesLocalFilePerTable: Boolean; override;
    function StoresUpperCaseIdentifiers: Boolean; override;
    function StoresLowerCaseIdentifiers: Boolean; override;
    function StoresMixedCaseIdentifiers: Boolean; override;
    function StoresUpperCaseQuotedIdentifiers: Boolean; override;
    function StoresLowerCaseQuotedIdentifiers: Boolean; override;
    function StoresMixedCaseQuotedIdentifiers: Boolean; override;
    function GetDefaultTransactionIsolation: TZTransactIsolationLevel; override;
    function DataDefinitionCausesTransactionCommit: Boolean; override;
    function DataDefinitionIgnoredInTransactions: Boolean; override;

    // interface details (terms, keywords, etc):
    function GetSchemaTerm: string; override;
    function GetProcedureTerm: string; override;
    function GetCatalogTerm: string; override;
    function GetCatalogSeparator: string; override;
    function GetSQLKeywords: string; override;
    function GetNumericFunctions: string; override;
    function GetStringFunctions: string; override;
    function GetSystemFunctions: string; override;
    function GetTimeDateFunctions: string; override;
    function GetSearchStringEscape: string; override;
    function GetExtraNameCharacters: string; override;
  end;

  TZMsSqlDatabaseInfo = class(TZDbLibDatabaseInfo)
    // database/driver/server info:
    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    function GetDriverName: string; override;
  end;

  TZSybaseDatabaseInfo = class(TZDbLibDatabaseInfo)
    // database/driver/server info:
    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    function GetDriverName: string; override;
  end;

  {** Implements DbLib Database Metadata. }
  TZDbLibBaseDatabaseMetadata = class(TZAbstractDatabaseMetadata)
  protected
    function ConvertEscapes(const Pattern: String): String;
    function GetSP_Prefix(const Catalog, Schema: String): String;
    function ComposeObjectString(const S: String; Const NullText: String = 'null';
      QuoteChar: Char = #39): String;
    function DecomposeObjectString(const S: String): String; override;
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-25

    function UncachedGetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
  end;

  {** Implements MsSql Database Metadata. }
  TZMsSqlDatabaseMetadata = class(TZDbLibBaseDatabaseMetadata)
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-25

    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
    function UncachedGetSchemas: IZResultSet; override;
    function UncachedGetCatalogs: IZResultSet; override;
    function UncachedGetTableTypes: IZResultSet; override;
    function UncachedGetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet; override;
    function UncachedGetColumnPrivileges(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet; override;
    function UncachedGetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet; override;
    function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet; override;
    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string):
      IZResultSet; override;
    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetTypeInfo: IZResultSet; override;
  end;

  {** Implements Sybase Database Metadata. }
  TZSybaseDatabaseMetadata = class(TZDbLibBaseDatabaseMetadata)
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-25

    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
    function UncachedGetSchemas: IZResultSet; override;
    function UncachedGetCatalogs: IZResultSet; override;
    function UncachedGetTableTypes: IZResultSet; override;
    function UncachedGetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet; override;
    function UncachedGetColumnPrivileges(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet; override;
    function UncachedGetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet; override;
    function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet; override;
    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string):
      IZResultSet; override;
    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetTypeInfo: IZResultSet; override;
    function UncachedGetUDTs(const Catalog: string; const SchemaPattern: string;
      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet; override;
    function RemoveQuotesFromIdentifier(const Identifier: String): String;
  end;

implementation

uses ZFastCode, ZDbcDbLibUtils, ZDbcDbLib;

{ TZDbLibDatabaseInfo }

{**
//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What's the name of this database product?
  @return database product name
}
function TZDbLibDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := '';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZDbLibDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZDbLibDatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Abstract Database Connectivity Driver for DbLib Server';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZDbLibDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZDbLibDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZDbLibDatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will
  always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := fCaseIdentifiers in [icMixed,icSpecial];
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := False
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := fCaseIdentifiers <> icSpecial;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := fCaseIdentifiers = icSpecial;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZDbLibDatabaseInfo.GetSQLKeywords: string;
begin
  { TODO -ofjanos -cAPI : SQL Keywords that are not SQL92 compliant }
  Result := inherited GetSQLKeywords;
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZDbLibDatabaseInfo.GetNumericFunctions: string;
begin
  Result := 'ABS,ACOS,ASIN,ATAN,ATN2,CEILING,COS,COT,DEGREES,EXP,FLOOR,LOG,LOG10,'+
            'PI,POWER,RADIANS,RAND,ROUND,SIGN,SIN,SQUARE,SQRT,TAN';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZDbLibDatabaseInfo.GetStringFunctions: string;
begin
  Result := 'ASCII,CHAR,CHARINDEX,DIFFERENCE,LEFT,LEN,LOWER,LTRIM,NCHAR,PATINDEX,'+
            'REPLACE,QUOTENAME,REPLICATE,REVERSE,RIGHT,RTRIM,SOUNDEX,SPACE,STR,'+
            'STUFF,SUBSTRING,UNICODE,UPPER';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZDbLibDatabaseInfo.GetSystemFunctions: string;
begin
  Result := 'APP_NAME,CASE,CAST,CONVERT,COALESCE,CURRENT_TIMESTAMP,CURRENT_USER,'+
            'DATALENGTH,@@ERROR,FORMATMESSAGE,GETANSINULL,HOST_ID,HOST_NAME,'+
            'IDENT_INCR,IDENT_SEED,@@IDENTITY,IDENTITY,ISDATE,ISNULL,ISNUMERIC,'+
            'NEWID,NULLIF,PARSENAME,PERMISSIONS,@@ROWCOUNT,SESSION_USER,STATS_DATE,'+
            'SYSTEM_USER,@@TRANCOUNT,USER_NAME';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZDbLibDatabaseInfo.GetTimeDateFunctions: string;
begin
  Result := 'DATEADD,DATEDIFF,DATENAME,DATEPART,DAY,GETDATE,MONTH,YEAR';
end;

procedure TZDbLibDatabaseInfo.InitIdentifierCase(const Collation: String);
begin
  if ZFastCode.Pos('_CI_', Collation) > 0
  then fCaseIdentifiers := icLower
  else if (ZFastCode.Pos('_BIN', Collation) > 0){SQLServer} or (ZFastCode.Pos('bin_', Collation) > 0) {Sybase}
  then fCaseIdentifiers := icSpecial
  else fCaseIdentifiers := icMixed;
end;

{**
  Gets the string that can be used to escape wildcard characters.
  This is the string that can be used to escape '_' or '%' in
  the string pattern style catalog search parameters.

  <P>The '_' character represents any single character.
  <P>The '%' character represents any sequence of zero or
  more characters.

  @return the string used to escape wildcard characters
}
function TZDbLibDatabaseInfo.GetSearchStringEscape: string;
begin
{ TODO -ofjanos -cgeneral :
In sql server this must be specified as the parameter of like.
example: WHERE ColumnA LIKE '%5/%%' ESCAPE '/' }
  Result := '/';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZDbLibDatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '@$#';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := True;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsOrderByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsGroupBy: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := True;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := False;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZDbLibDatabaseInfo.GetSchemaTerm: string;
begin
  Result := 'owner';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZDbLibDatabaseInfo.GetProcedureTerm: string;
begin
  Result := 'procedure';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZDbLibDatabaseInfo.GetCatalogTerm: string;
begin
  Result := 'database';
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZDbLibDatabaseInfo.GetCatalogSeparator: string;
begin
  Result := '.';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsPositionedDelete: Boolean;
begin
//CURRENT OF
//Specifies that the DELETE is done at the current position of the specified cursor.
  Result := True;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsPositionedUpdate: Boolean;
begin
  Result := True;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsSelectForUpdate: Boolean;
begin
  Result := True;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsStoredProcedures: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsSubqueriesInExists: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsSubqueriesInIns: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := True;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsUnion: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsUnionAll: Boolean;
begin
  Result := True;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZDbLibDatabaseInfo.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := True;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZDbLibDatabaseInfo.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := True;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZDbLibDatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZDbLibDatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
begin
  Result := False;
end;

//----------------------------------------------------------------------
// The following group of methods exposes various limitations
// based on the target database with the current driver.
// Unless otherwise specified, a result of zero means there is no
// limit, or the limit is not known.

{**
  How many hex characters can you have in an inline binary literal?
  @return max binary literal length in hex characters;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 16000;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 8000;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 4096;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 1024;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxCursorNameLength: Integer;
begin
  Result := 128;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 900;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxSchemaNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxProcedureNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxRowSize: Integer;
begin
  Result := 8060;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := False;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxStatementLength: Integer;
begin
  Result := 0;
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 256;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDbLibDatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := 128;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZDbLibDatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiReadCommitted;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsTransactions: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZDbLibDatabaseInfo.SupportsTransactionIsolationLevel(
  const Level: TZTransactIsolationLevel): Boolean;
begin
  Result := True;
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := False;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := False;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := False;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsResultSetType(
  const _Type: TZResultSetType): Boolean;
begin
  Result := True;
end;

{**
  Does the database support the concurrency type in combination
  with the given result set type?

  @param type defined in <code>java.sql.ResultSet</code>
  @param concurrency type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDbLibDatabaseInfo.SupportsResultSetConcurrency(
  const _Type: TZResultSetType; const Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := True;
end;


{ TZDbLibBaseDatabaseMetadata }

function TZDbLibBaseDatabaseMetadata.GetSP_Prefix(const Catalog, Schema: String): String;
begin
  if (UpperCase(Catalog) = 'INFORMATION_SCHEMA') or
     (UpperCase(Schema)  = 'INFORMATION_SCHEMA') then
    Result := ''
  else
    Result := Catalog+'.'+ConvertEscapes(Schema)+'.';
end;

{**
  Composes a object name, AnsiQuotedStr or NullText
  @param S the object string
  @param NullText the "NULL"-Text default: 'null'
  @param QuoteChar the QuoteChar default: '
  @return 'null' if S is '' or S if s is already Quoted or AnsiQuotedStr(S, #39)
}
function TZDbLibBaseDatabaseMetadata.ComposeObjectString(const S: String;
  Const NullText: String = 'null'; QuoteChar: Char = #39): String;
begin
  if S = '' then
    Result := NullText
  else begin
    Result := ConvertEscapes(S);
    if not IC.IsQuoted(Result) then
      Result := AnsiQuotedStr(Result, QuoteChar);
  end;
end;

{**
  Decomposes a object name, AnsiQuotedStr or NullText
  @param S the object string
  @return 'null' if S is '' or S if s is already Quoted or AnsiQuotedStr(S, #39)
}
function TZDbLibBaseDatabaseMetadata.DecomposeObjectString(const S: String): String;
begin
  if S = '' then
    Result := 'null'
  else
    Result := AnsiQuotedStr(Inherited DecomposeObjectString(S), #39);
end;

function TZDbLibBaseDatabaseMetadata.ConvertEscapes(const Pattern: String): String;
var
  EscapeChar: Char;
  P: PChar;
begin
  Result := '';
  if Length(Pattern) = 0 then Exit;
  EscapeChar := GetDatabaseInfo.GetSearchStringEscape[1];
  P := Pointer(Pattern);
  ClearBuf;
  while P^ <> #0 do begin
    if (P^ = EscapeChar) and (((P+1)^ = WildcardsArray[0]) or ((P+1)^=WildcardsArray[1])) then begin
      ToBuf('[', Result);
      Inc(P);
      ToBuf(P^, Result);
      ToBuf(']', Result);
    end else
      ToBuf(P^, Result);
    Inc(P);
  end;
  FlushBuf(Result);
end;

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZDbLibBaseDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZDbLibDatabaseInfo.Create(Self);
end;

{**
  Gets a description of the primary key columns that are
  referenced by a table's foreign key columns (the primary keys
  imported by a table).  They are ordered by PKTABLE_CAT,
  PKTABLE_SCHEM, PKTABLE_NAME, and KEY_SEQ.

  <P>Each primary key column description has the following columns:
   <OL>
        <LI><B>PKTABLE_CAT</B> String => primary key table catalog
       being imported (may be null)
        <LI><B>PKTABLE_SCHEM</B> String => primary key table schema
       being imported (may be null)
        <LI><B>PKTABLE_NAME</B> String => primary key table name
       being imported
        <LI><B>PKCOLUMN_NAME</B> String => primary key column name
       being imported
        <LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
        <LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
        <LI><B>FKTABLE_NAME</B> String => foreign key table name
        <LI><B>FKCOLUMN_NAME</B> String => foreign key column name
        <LI><B>KEY_SEQ</B> short => sequence number within foreign key
        <LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
        <LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
        <LI><B>FK_NAME</B> String => foreign key name (may be null)
        <LI><B>PK_NAME</B> String => primary key name (may be null)
        <LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a primary key column description
  @see #getExportedKeys
}
function TZDbLibBaseDatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result := UncachedGetCrossReference('', '', '', Catalog, Schema, Table);
end;

{**
  What's the name of this database product?
  @return database product name
}
function TZMsSqlDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := 'MS SQL';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZMsSqlDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '7+';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZMsSqlDatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for Microsoft SQL Server';
end;

{**
  What's the name of this database product?
  @return database product name
}
function TZSybaseDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := 'Sybase';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZSybaseDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '12+';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZSybaseDatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for Sybase ASE Server';
end;

{ TZMsSqlDatabaseMetadata }

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZMsSqlDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZMsSqlDatabaseInfo.Create(Self);
end;

{**
  Gets a description of the stored procedures available in a
  catalog.

  <P>Only procedure descriptions matching the schema and
  procedure name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM, and PROCEDURE_NAME.

  <P>Each procedure description has the the following columns:
   <OL>
        <LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
        <LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
        <LI><B>PROCEDURE_NAME</B> String => procedure name
   <LI> reserved for future use
   <LI> reserved for future use
   <LI> reserved for future use
        <LI><B>REMARKS</B> String => explanatory comment on the procedure
        <LI><B>PROCEDURE_TYPE</B> short => kind of procedure:
       <UL>
       <LI> procedureResultUnknown - May return a result
       <LI> procedureNoResult - Does not return a result
       <LI> procedureReturnsResult - Returns a result
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @return <code>ResultSet</code> - each row is a procedure description
  @see #getSearchStringEscape
}
function TZMsSqlDatabaseMetadata.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
begin
    Result:=inherited UncachedGetProcedures(Catalog, SchemaPattern, ProcedureNamePattern);

    with GetStatement.ExecuteQuery('exec '+Catalog+'.'+SchemaPattern+'.'+'sp_stored_procedures '+
      ComposeObjectString(ProcedureNamePattern)+', '+ComposeObjectString(SchemaPattern)+', '+ComposeObjectString(Catalog)) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(CatalogNameIndex, GetStringByName('PROCEDURE_QUALIFIER'));
        Result.UpdateString(SchemaNameIndex, GetStringByName('PROCEDURE_OWNER'));
        Result.UpdateString(ProcedureNameIndex, GetStringByName('PROCEDURE_NAME'));
        Result.UpdateString(ProcedureRemarksIndex, GetStringByName('REMARKS'));
        Result.UpdateSmall(ProcedureTypeIndex, 0);
        Result.InsertRow;
      end;
      Close;
    end;
    Result.BeforeFirst;
end;

{**
  Gets a description of a catalog's stored procedure parameters
  and result columns.

  <P>Only descriptions matching the schema, procedure and
  parameter name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM and PROCEDURE_NAME. Within this, the return value,
  if any, is first. Next are the parameter descriptions in call
  order. The column descriptions follow in column number order.

  <P>Each row in the <code>ResultSet</code> is a parameter description or
  column description with the following fields:
   <OL>
        <LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
        <LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
        <LI><B>PROCEDURE_NAME</B> String => procedure name
        <LI><B>COLUMN_NAME</B> String => column/parameter name
        <LI><B>COLUMN_TYPE</B> Short => kind of column/parameter:
       <UL>
       <LI> procedureColumnUnknown - nobody knows
       <LI> procedureColumnIn - IN parameter
       <LI> procedureColumnInOut - INOUT parameter
       <LI> procedureColumnOut - OUT parameter
       <LI> procedureColumnReturn - procedure return value
       <LI> procedureColumnResult - result column in <code>ResultSet</code>
       </UL>
   <LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
        <LI><B>TYPE_NAME</B> String => SQL type name, for a UDT type the
   type name is fully qualified
        <LI><B>PRECISION</B> int => precision
        <LI><B>LENGTH</B> int => length in bytes of data
        <LI><B>SCALE</B> short => scale
        <LI><B>RADIX</B> short => radix
        <LI><B>NULLABLE</B> short => can it contain NULL?
       <UL>
       <LI> procedureNoNulls - does not allow NULL values
       <LI> procedureNullable - allows NULL values
       <LI> procedureNullableUnknown - nullability unknown
       </UL>
        <LI><B>REMARKS</B> String => comment describing parameter/column
   </OL>

  <P><B>Note:</B> Some databases may not return the column
  descriptions for a procedure. Additional columns beyond
  REMARKS can be defined by the database.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row describes a stored procedure parameter or
       column
  @see #getSearchStringEscape
}
function TZMsSqlDatabaseMetadata.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
begin
    Result:=inherited UncachedGetProcedureColumns(Catalog, SchemaPattern,
      ProcedureNamePattern, ColumnNamePattern);

    with GetStatement.ExecuteQuery('exec '+Catalog+'.'+SchemaPattern+'.'+
      'sp_sproc_columns '+ComposeObjectString(ProcedureNamePattern)+', '+
      ComposeObjectString(SchemaPattern)+', '+ComposeObjectString(Catalog)+', '+
      ComposeObjectString(ColumnNamePattern)) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(CatalogNameIndex, GetStringByName('PROCEDURE_QUALIFIER'));
        Result.UpdateString(SchemaNameIndex, GetStringByName('PROCEDURE_OWNER'));
        Result.UpdateString(ProcColProcedureNameIndex, GetStringByName('PROCEDURE_NAME'));
        Result.UpdateString(ProcColColumnNameIndex, GetStringByName('COLUMN_NAME'));
        case GetSmallByName('COLUMN_TYPE') of
          1: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctIn));
          2: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctInOut));
          3: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctUnknown));
          4: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctInOut));
          5: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctReturn));
        else
          Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctUnknown));
        end;
        Result.UpdateSmall(ProcColDataTypeIndex,
          Ord(ConvertODBCToSqlType(GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
        Result.UpdateString(ProcColTypeNameIndex, GetStringByName('TYPE_NAME'));
        Result.UpdateInt(ProcColPrecisionIndex, GetIntByName('PRECISION'));
        Result.UpdateInt(ProcColLengthIndex, GetIntByName('LENGTH'));
        Result.UpdateSmall(ProcColScaleIndex, GetSmallByName('SCALE'));
        Result.UpdateSmall(ProcColRadixIndex, GetSmallByName('RADIX'));
        Result.UpdateSmall(ProcColNullableIndex, 2);
        if GetStringByName('IS_NULLABLE') = 'NO' then
          Result.UpdateSmall(ProcColNullableIndex, 0);
        if GetStringByName('IS_NULLABLE') = 'YES' then
          Result.UpdateSmall(ProcColNullableIndex, 1);
        Result.UpdateString(ProcColRemarksIndex, GetStringByName('REMARKS'));
        Result.InsertRow;
      end;
      Close;
    end;
    Result.BeforeFirst;
end;

{**
  Gets a description of tables available in a catalog.

  <P>Only table descriptions matching the catalog, schema, table
  name and type criteria are returned.  They are ordered by
  TABLE_TYPE, TABLE_SCHEM and TABLE_NAME.

  <P>Each table description has the following columns:
   <OL>
        <LI><B>TABLE_CAT</B> String => table catalog (may be null)
        <LI><B>TABLE_SCHEM</B> String => table schema (may be null)
        <LI><B>TABLE_NAME</B> String => table name
        <LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
                        "VIEW", "SYSTEM TABLE", "GLOBAL TEMPORARY",
                        "LOCAL TEMPORARY", "ALIAS", "SYNONYM".
        <LI><B>REMARKS</B> String => explanatory comment on the table
   </OL>

  <P><B>Note:</B> Some databases may not return information for
  all tables.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param types a list of table types to include; null returns all types
  @return <code>ResultSet</code> - each row is a table description
  @see #getSearchStringEscape
}
function TZMsSqlDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  I: Integer;
  TableTypes: string;
begin
    Result:=inherited UncachedGetTables(Catalog, SchemaPattern, TableNamePattern, Types);

    TableTypes := '';
    for I := 0 to Length(Types) - 1 do
    begin
      if Length(TableTypes) > 0 then
        TableTypes := TableTypes + ',';
      TableTypes := TableTypes + AnsiQuotedStr(Types[I], '''');
    end;
    if TableTypes = '' then
      TableTypes := 'null'
    else TableTypes := AnsiQuotedStr(TableTypes, '"');

    with GetStatement.ExecuteQuery(
      Format('exec sp_tables %s, %s, %s, %s',
      [ComposeObjectString(TableNamePattern), ComposeObjectString(SchemaPattern), ComposeObjectString(Catalog), TableTypes])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_QUALIFIER'));
        Result.UpdateString(SchemaNameIndex, GetStringByName('TABLE_OWNER'));
        Result.UpdateString(TableNameIndex, GetStringByName('TABLE_NAME'));
        Result.UpdateString(TableColumnsSQLType, GetStringByName('TABLE_TYPE'));
        Result.UpdateString(TableColumnsRemarks, GetStringByName('REMARKS'));
        Result.InsertRow;
      end;
      Close;
    end;
    Result.BeforeFirst;
end;

{**
  Gets the schema names available in this database.  The results
  are ordered by schema name.

  <P>The schema column is:
   <OL>
        <LI><B>TABLE_SCHEM</B> String => schema name
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  schema name
}
function TZMsSqlDatabaseMetadata.UncachedGetSchemas: IZResultSet;
begin
    Result:=inherited UncachedGetSchemas;

    with GetStatement.ExecuteQuery(
      'select name as TABLE_OWNER from sysusers where islogin = 1') do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(SchemaColumnsTableSchemaIndex, GetString(FirstDbcIndex));
        Result.InsertRow;
      end;
      Close;
    end;
    Result.BeforeFirst;
end;

{**
  Gets the catalog names available in this database.  The results
  are ordered by catalog name.

  <P>The catalog column is:
   <OL>
        <LI><B>TABLE_CAT</B> String => catalog name
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  catalog name
}
function TZMsSqlDatabaseMetadata.UncachedGetCatalogs: IZResultSet;
begin
    Result:=inherited UncachedGetCatalogs;

    with GetStatement.ExecuteQuery('exec sp_databases') do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(CatalogNameIndex, GetStringByName('DATABASE_NAME'));
        Result.InsertRow;
      end;
      Close;
    end;
    Result.BeforeFirst;
end;

{**
  Gets the table types available in this database.  The results
  are ordered by table type.

  <P>The table type is:
   <OL>
        <LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
                        "VIEW", "SYSTEM TABLE", "GLOBAL TEMPORARY",
                        "LOCAL TEMPORARY", "ALIAS", "SYNONYM".
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  table type
}
function TZMsSqlDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  TableTypes: array[0..2] of string = ('SYSTEM TABLE', 'TABLE', 'VIEW');
var
  I: Integer;
begin
    Result:=inherited UncachedGetTableTypes;

    for I := 0 to 2 do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(TableTypeColumnTableTypeIndex, TableTypes[I]);
      Result.InsertRow;
    end;
    Result.BeforeFirst;
end;

{**
  Gets a description of table columns available in
  the specified catalog.

  <P>Only column descriptions matching the catalog, schema, table
  and column name criteria are returned.  They are ordered by
  TABLE_SCHEM, TABLE_NAME and ORDINAL_POSITION.

  <P>Each column description has the following columns:
   <OL>
        <LI><B>TABLE_CAT</B> String => table catalog (may be null)
        <LI><B>TABLE_SCHEM</B> String => table schema (may be null)
        <LI><B>TABLE_NAME</B> String => table name
        <LI><B>COLUMN_NAME</B> String => column name
        <LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
        <LI><B>TYPE_NAME</B> String => Data source dependent type name,
   for a UDT the type name is fully qualified
        <LI><B>COLUMN_SIZE</B> int => column size.  For char or date
            types this is the maximum number of characters, for numeric or
            decimal types this is precision.
        <LI><B>BUFFER_LENGTH</B> is not used.
        <LI><B>DECIMAL_DIGITS</B> int => the number of fractional digits
        <LI><B>NUM_PREC_RADIX</B> int => Radix (typically either 10 or 2)
        <LI><B>NULLABLE</B> int => is NULL allowed?
       <UL>
       <LI> columnNoNulls - might not allow NULL values
       <LI> columnNullable - definitely allows NULL values
       <LI> columnNullableUnknown - nullability unknown
       </UL>
        <LI><B>REMARKS</B> String => comment describing column (may be null)
        <LI><B>COLUMN_DEF</B> String => default value (may be null)
        <LI><B>SQL_DATA_TYPE</B> int => unused
        <LI><B>SQL_DATETIME_SUB</B> int => unused
        <LI><B>CHAR_OCTET_LENGTH</B> int => for char types the
        maximum number of bytes in the column
        <LI><B>ORDINAL_POSITION</B> int => index of column in table
       (starting at 1)
        <LI><B>IS_NULLABLE</B> String => "NO" means column definitely
       does not allow NULL values; "YES" means the column might
       allow NULL values.  An empty string means nobody knows.
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row is a column description
  @see #getSearchStringEscape
}
function TZMsSqlDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  SQLType: TZSQLType;
  tmp: String;
  ResultHasRows: Boolean;
begin
  ResultHasRows := False;
  Result:=inherited UncachedGetColumns(Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);

  with GetStatement.ExecuteQuery('exec '+GetSP_Prefix(Catalog, SchemaPattern)+'sp_columns '+
      ComposeObjectString(TableNamePattern)+', '+ComposeObjectString(SchemaPattern)+', '+
      ComposeObjectString(Catalog)+', '+ComposeObjectString(ColumnNamePattern)) do
  begin
    while Next do
    begin
      ResultHasRows := True;
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_QUALIFIER'));
      Result.UpdateString(SchemaNameIndex, GetStringByName('TABLE_OWNER'));
      Result.UpdateString(TableNameIndex, GetStringByName('TABLE_NAME'));
      tmp := GetStringByName('COLUMN_NAME');
      Result.UpdateString(ColumnNameIndex, tmp);
      Result.UpdateBoolean(TableColColumnCaseSensitiveIndex, IC.IsCaseSensitive(tmp));
      //The value in the resultset will be used
      SQLType := ConvertODBCToSqlType(GetSmallByName('DATA_TYPE'), ConSettings.CPType);
      tmp := UpperCase(GetStringByName('TYPE_NAME'));
      if SQLType = stUnknown then
        Result.UpdateNull(TableColColumnTypeIndex)
      else if ( SQLType = stBytes) and (tmp = 'UNIQUEIDENTIFIER') then
        Result.UpdateSmall(TableColColumnTypeIndex, Ord(stGUID))
      else if ( SQLType = stDouble) and StartsWith(tmp, 'MONEY') then
        Result.UpdateSmall(TableColColumnTypeIndex, Ord(stCurrency))
      else if (SQLType = stString) and (tmp = 'DATE') then
        Result.UpdateSmall(TableColColumnTypeIndex, Ord(stDate))
      else
        Result.UpdateSmall(TableColColumnTypeIndex, Ord(SQLType));
      Result.UpdateString(TableColColumnTypeNameIndex, tmp);
      Result.UpdateInt(TableColColumnSizeIndex, GetIntByName('LENGTH'));
      Result.UpdateInt(TableColColumnBufLengthIndex, GetIntByName('LENGTH'));
      Result.UpdateInt(TableColColumnDecimalDigitsIndex, GetIntByName('SCALE'));
      Result.UpdateSmall(TableColColumnNumPrecRadixIndex, GetSmallByName('RADIX'));
      tmp := GetStringByName('IS_NULLABLE');
      if tmp = 'NO' then
        Result.UpdateInt(TableColColumnNullableIndex, Ord(ntNoNulls))
      else if tmp = 'YES' then
        Result.UpdateSmall(TableColColumnNullableIndex, Ord(ntNullable))
      else
        Result.UpdateSmall(TableColColumnNullableIndex, Ord(ntNullableUnknown));
      Result.UpdateString(TableColColumnRemarksIndex, GetStringByName('REMARKS'));
      //if (GetConnection as IZDBLibConnection).GetProvider = dpSybase then
        Result.UpdateString(TableColColumnColDefIndex, GetStringByName('COLUMN_DEF'))
      {else //MSSQL bizarity: defaults are double braked '((value))' or braked and quoted '(''value'')'
      begin
        default_val := GetStringByName('COLUMN_DEF');
        if default_val <> '' then
          Result.UpdateString(TableColColumnColDefIndex, Copy(default_val, 3, Length(default_val)-4));
      end};
      Result.UpdateSmall(TableColColumnSQLDataTypeIndex, GetSmallByName('SQL_DATA_TYPE'));
      Result.UpdateSmall(TableColColumnSQLDateTimeSubIndex, GetSmallByName('SQL_DATETIME_SUB'));
      Result.UpdateInt(TableColColumnCharOctetLengthIndex, GetIntByName('CHAR_OCTET_LENGTH'));
      Result.UpdateInt(TableColColumnOrdPosIndex, GetIntByName('ORDINAL_POSITION'));
      Result.UpdateString(TableColColumnIsNullableIndex, tmp);
      Result.UpdateSmall(TableColColumnCharOctetLengthIndex, GetSmallByName('CHAR_OCTET_LENGTH'));
      if (GetConnection as IZDBLibConnection).GetProvider = dpMsSQL then
        Result.UpdateBoolean(TableColColumnSearchableIndex,
          not (GetSmallByName('SS_DATA_TYPE') in [34, 35]));
      Result.InsertRow;
    end;
    Close;
  end;

  if ResultHasRows then begin

    // hint by Jan: I am not sure wether this statement still works with SQL Server 2000 or before.
    Result.BeforeFirst;
    with GetStatement.ExecuteQuery(
      Format('select c.colid, c.name, c.type, c.prec, c.scale, c.colstat, c.status, c.iscomputed '
      + ' from syscolumns c '
      + '   inner join sys.sysobjects o on (o.id = c.id) '
      + '   inner join sys.schemas s on (o.uid = s.schema_id) '
      + ' where c.number=0 '
      + '   and (o.name like %0:s escape ''%2:s'' or (%0:s is null)) '
      + '   and (s.name like %1:s escape ''%2:s'' or (%1:s is null)) '
      + ' order by colid ',
      [DeComposeObjectString(TableNamePattern), DeComposeObjectString(SchemaPattern), GetDataBaseInfo.GetSearchStringEscape])) do
      // hint http://blog.sqlauthority.com/2007/04/30/case-sensitive-sql-query-search/ for the collation setting to get a case sensitive behavior
    begin
      while Next do
      begin
        Result.Next;
        Result.UpdateBoolean(TableColColumnAutoIncIndex, (GetSmallByName('status') and $80) <> 0);
        Result.UpdateBoolean(TableColColumnSearchableIndex,
          Result.GetBoolean(TableColColumnSearchableIndex) and (GetIntByName('iscomputed') = 0));
        Result.UpdateBoolean(TableColColumnWritableIndex,
          ((GetSmallByName('status') and $80) = 0)
          (*and (GetSmallByName('type') <> 37)*)   // <<<< *DEBUG WARUM?
          and (GetIntByName('iscomputed') = 0));
        Result.UpdateBoolean(TableColColumnDefinitelyWritableIndex,
          Result.GetBoolean(TableColColumnWritableIndex));
        Result.UpdateBoolean(TableColColumnReadonlyIndex,
          not Result.GetBoolean(TableColColumnWritableIndex));
        if Result.GetBoolean(TableColColumnAutoIncIndex) then
        begin
          Result.UpdateSmall(TableColColumnNullableIndex, 1);
          Result.UpdateString(TableColColumnIsNullableIndex, 'YES');
        end;
        Result.UpdateRow;
      end;
      Close;
    end;
    Result.BeforeFirst;
  end;
end;

{**
  Gets a description of the access rights for a table's columns.

  <P>Only privileges matching the column name criteria are
  returned.  They are ordered by COLUMN_NAME and PRIVILEGE.

  <P>Each privilige description has the following columns:
   <OL>
        <LI><B>TABLE_CAT</B> String => table catalog (may be null)
        <LI><B>TABLE_SCHEM</B> String => table schema (may be null)
        <LI><B>TABLE_NAME</B> String => table name
        <LI><B>COLUMN_NAME</B> String => column name
        <LI><B>GRANTOR</B> => grantor of access (may be null)
        <LI><B>GRANTEE</B> String => grantee of access
        <LI><B>PRIVILEGE</B> String => name of access (SELECT,
       INSERT, UPDATE, REFRENCES, ...)
        <LI><B>IS_GRANTABLE</B> String => "YES" if grantee is permitted
       to grant to others; "NO" if not; null if unknown
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row is a column privilege description
  @see #getSearchStringEscape
}
function TZMsSqlDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
begin
    Result:=inherited UncachedGetColumnPrivileges(Catalog, Schema, Table, ColumnNamePattern);

    with GetStatement.ExecuteQuery('exec '+GetSP_Prefix(Catalog, Schema)+'sp_column_privileges '+
      ComposeObjectString(Table)+', '+ComposeObjectString(Schema)+', '+
      ComposeObjectString(Catalog)+', '+ComposeObjectString(ColumnNamePattern)) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_QUALIFIER'));
        Result.UpdateString(SchemaNameIndex, GetStringByName('TABLE_OWNER'));
        Result.UpdateString(TableNameIndex, GetStringByName('TABLE_NAME'));
        Result.UpdateString(ColumnNameIndex, GetStringByName('COLUMN_NAME'));
        Result.UpdateString(TableColPrivGrantorIndex, GetStringByName('GRANTOR'));
        Result.UpdateString(TableColPrivGranteeIndex, GetStringByName('GRANTEE'));
        Result.UpdateString(TableColPrivPrivilegeIndex, GetStringByName('PRIVILEGE'));
        Result.UpdateString(TableColPrivIsGrantableIndex, GetStringByName('IS_GRANTABLE'));
        Result.InsertRow;
      end;
      Close;
    end;
    Result.BeforeFirst;
end;

{**
  Gets a description of the access rights for each table available
  in a catalog. Note that a table privilege applies to one or
  more columns in the table. It would be wrong to assume that
  this priviledge applies to all columns (this may be true for
  some systems but is not true for all.)

  <P>Only privileges matching the schema and table name
  criteria are returned.  They are ordered by TABLE_SCHEM,
  TABLE_NAME, and PRIVILEGE.

  <P>Each privilige description has the following columns:
   <OL>
        <LI><B>TABLE_CAT</B> String => table catalog (may be null)
        <LI><B>TABLE_SCHEM</B> String => table schema (may be null)
        <LI><B>TABLE_NAME</B> String => table name
        <LI><B>GRANTOR</B> => grantor of access (may be null)
        <LI><B>GRANTEE</B> String => grantee of access
        <LI><B>PRIVILEGE</B> String => name of access (SELECT,
       INSERT, UPDATE, REFRENCES, ...)
        <LI><B>IS_GRANTABLE</B> String => "YES" if grantee is permitted
       to grant to others; "NO" if not; null if unknown
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @return <code>ResultSet</code> - each row is a table privilege description
  @see #getSearchStringEscape
}
function TZMsSqlDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
begin
    Result:=inherited UncachedGetTablePrivileges(Catalog, SchemaPattern, TableNamePattern);

    with GetStatement.ExecuteQuery(
      Format('exec sp_table_privileges %s, %s, %s',
      [ComposeObjectString(TableNamePattern), ComposeObjectString(SchemaPattern), ComposeObjectString(Catalog)])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_QUALIFIER'));
        Result.UpdateString(SchemaNameIndex,GetStringByName('TABLE_OWNER'));
        Result.UpdateString(TableNameIndex, GetStringByName('TABLE_NAME'));
        Result.UpdateString(TablePrivGrantorIndex, GetStringByName('GRANTOR'));
        Result.UpdateString(TablePrivGranteeIndex, GetStringByName('GRANTEE'));
        Result.UpdateString(TablePrivPrivilegeIndex, GetStringByName('PRIVILEGE'));
        Result.UpdateString(TablePrivIsGrantableIndex, GetStringByName('IS_GRANTABLE'));
        Result.InsertRow;
      end;
      Close;
    end;
    Result.BeforeFirst;
end;

{**
  Gets a description of a table's columns that are automatically
  updated when any value in a row is updated.  They are
  unordered.

  <P>Each column description has the following columns:
   <OL>
        <LI><B>SCOPE</B> short => is not used
        <LI><B>COLUMN_NAME</B> String => column name
        <LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
        <LI><B>TYPE_NAME</B> String => Data source dependent type name
        <LI><B>COLUMN_SIZE</B> int => precision
        <LI><B>BUFFER_LENGTH</B> int => length of column value in bytes
        <LI><B>DECIMAL_DIGITS</B> short  => scale
        <LI><B>PSEUDO_COLUMN</B> short => is this a pseudo column
       like an Oracle ROWID
       <UL>
       <LI> versionColumnUnknown - may or may not be pseudo column
       <LI> versionColumnNotPseudo - is NOT a pseudo column
       <LI> versionColumnPseudo - is a pseudo column
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a column description
  @exception SQLException if a database access error occurs
}
function TZMsSqlDatabaseMetadata.UncachedGetVersionColumns(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result:=inherited UncachedGetVersionColumns(Catalog, Schema, Table);

  with GetStatement.ExecuteQuery(
    Format('exec sp_special_columns %s, %s, %s, %s',
    [ComposeObjectString(Table), ComposeObjectString(Schema), ComposeObjectString(Catalog), '''V'''])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateSmall(TableColVerScopeIndex, GetSmallByName('SCOPE'));
      Result.UpdateString(TableColVerColNameIndex, GetStringByName('COLUMN_NAME'));
      Result.UpdateSmall(TableColVerDataTypeIndex, Ord(ConvertODBCToSqlType(
        GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
      Result.UpdateString(TableColVerTypeNameIndex, GetStringByName('TYPE_NAME'));
      Result.UpdateInt(TableColVerColSizeIndex, GetIntByName('LENGTH'));
      Result.UpdateInt(TableColVerBufLengthIndex, GetIntByName('LENGTH'));
      Result.UpdateInt(TableColVerDecimalDigitsIndex, GetIntByName('SCALE'));
      Result.UpdateSmall(TableColVerPseudoColumnIndex, GetSmallByName('PSEUDO_COLUMN'));
      Result.InsertRow;
    end;
    Close;
  end;
  Result.BeforeFirst;
end;

{**
  Gets a description of a table's primary key columns.  They
  are ordered by COLUMN_NAME.

  <P>Each primary key column description has the following columns:
   <OL>
        <LI><B>TABLE_CAT</B> String => table catalog (may be null)
        <LI><B>TABLE_SCHEM</B> String => table schema (may be null)
        <LI><B>TABLE_NAME</B> String => table name
        <LI><B>COLUMN_NAME</B> String => column name
        <LI><B>KEY_SEQ</B> short => sequence number within primary key
        <LI><B>PK_NAME</B> String => primary key name (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a primary key column description
  @exception SQLException if a database access error occurs
}
function TZMsSqlDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result:=inherited UncachedGetPrimaryKeys(Catalog, Schema, Table);

  with GetStatement.ExecuteQuery(
    Format('exec sp_pkeys %s, %s, %s',
    [ComposeObjectString(Table), ComposeObjectString(Schema), ComposeObjectString(Catalog)])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_QUALIFIER'));
      Result.UpdateString(SchemaNameIndex, GetStringByName('TABLE_OWNER'));
      Result.UpdateString(TableNameIndex, GetStringByName('TABLE_NAME'));
      Result.UpdateString(PrimaryKeyColumnNameIndex, GetStringByName('COLUMN_NAME'));
      Result.UpdateSmall(PrimaryKeyKeySeqIndex, GetSmallByName('KEY_SEQ'));
      Result.UpdateString(PrimaryKeyPKNameIndex, GetStringByName('PK_NAME'));
      Result.InsertRow;
    end;
    Close;
  end;
  Result.BeforeFirst;
end;

{**
  Gets a description of the foreign key columns in the foreign key
  table that reference the primary key columns of the primary key
  table (describe how one table imports another's key.) This
  should normally return a single foreign key/primary key pair
  (most tables only import a foreign key from a table once.)  They
  are ordered by FKTABLE_CAT, FKTABLE_SCHEM, FKTABLE_NAME, and
  KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
        <LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
        <LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
        <LI><B>PKTABLE_NAME</B> String => primary key table name
        <LI><B>PKCOLUMN_NAME</B> String => primary key column name
        <LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
        <LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
        <LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
        <LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
        <LI><B>KEY_SEQ</B> short => sequence number within foreign key
        <LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
        <LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
        <LI><B>FK_NAME</B> String => foreign key name (may be null)
        <LI><B>PK_NAME</B> String => primary key name (may be null)
        <LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param primaryCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param primarySchema a schema name; "" retrieves those
  without a schema
  @param primaryTable the table name that exports the key
  @param foreignCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param foreignSchema a schema name; "" retrieves those
  without a schema
  @param foreignTable the table name that imports the key
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZMsSqlDatabaseMetadata.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
begin
  Result:=inherited UncachedGetCrossReference(PrimaryCatalog, PrimarySchema, PrimaryTable,
                                              ForeignCatalog, ForeignSchema, ForeignTable);
  with GetStatement.ExecuteQuery(
    Format('exec sp_fkeys %s, %s, %s, %s, %s, %s',
    [ComposeObjectString(PrimaryTable), ComposeObjectString(PrimarySchema), ComposeObjectString(PrimaryCatalog),
     ComposeObjectString(ForeignTable), ComposeObjectString(ForeignSchema), ComposeObjectString(ForeignCatalog)])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CrossRefKeyColPKTableCatalogIndex, GetStringByName('PKTABLE_QUALIFIER'));
      Result.UpdateString(CrossRefKeyColPKTableSchemaIndex, GetStringByName('PKTABLE_OWNER'));
      Result.UpdateString(CrossRefKeyColPKTableNameIndex, GetStringByName('PKTABLE_NAME'));
      Result.UpdateString(CrossRefKeyColPKColumnNameIndex, GetStringByName('PKCOLUMN_NAME'));
      Result.UpdateString(CrossRefKeyColFKTableCatalogIndex, GetStringByName('FKTABLE_QUALIFIER'));
      Result.UpdateString(CrossRefKeyColFKTableSchemaIndex, GetStringByName('FKTABLE_OWNER'));
      Result.UpdateString(CrossRefKeyColFKTableNameIndex, GetStringByName('FKTABLE_NAME'));
      Result.UpdateString(CrossRefKeyColFKColumnNameIndex, GetStringByName('FKCOLUMN_NAME'));
      Result.UpdateSmall(CrossRefKeyColKeySeqIndex, GetSmallByName('KEY_SEQ'));
      Result.UpdateSmall(CrossRefKeyColUpdateRuleIndex, GetSmallByName('UPDATE_RULE'));
      Result.UpdateSmall(CrossRefKeyColDeleteRuleIndex, GetSmallByName('DELETE_RULE'));
      Result.UpdateString(CrossRefKeyColFKNameIndex, GetStringByName('FK_NAME'));
      Result.UpdateString(CrossRefKeyColPKNameIndex, GetStringByName('PK_NAME'));
      Result.UpdateInt(CrossRefKeyColDeferrabilityIndex, 0);
      Result.InsertRow;
    end;
    Close;
  end;
  Result.BeforeFirst;
end;

{**
  Gets a description of the foreign key columns that reference a
  table's primary key columns (the foreign keys exported by a
  table).  They are ordered by FKTABLE_CAT, FKTABLE_SCHEM,
  FKTABLE_NAME, and KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
        <LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
        <LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
        <LI><B>PKTABLE_NAME</B> String => primary key table name
        <LI><B>PKCOLUMN_NAME</B> String => primary key column name
        <LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
        <LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
        <LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
        <LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
        <LI><B>KEY_SEQ</B> short => sequence number within foreign key
        <LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
        <LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
        <LI><B>FK_NAME</B> String => foreign key name (may be null)
        <LI><B>PK_NAME</B> String => primary key name (may be null)
        <LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZMsSqlDatabaseMetadata.UncachedGetExportedKeys(const Catalog, Schema,
  Table: string): IZResultSet;
begin
  Result := UncachedGetCrossReference(Catalog, Schema, Table, '', '', '');
end;

{**
  Gets a description of all the standard SQL types supported by
  this database. They are ordered by DATA_TYPE and then by how
  closely the data type maps to the corresponding JDBC SQL type.

  <P>Each type description has the following columns:
   <OL>
        <LI><B>TYPE_NAME</B> String => Type name
        <LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
        <LI><B>PRECISION</B> int => maximum precision
        <LI><B>LITERAL_PREFIX</B> String => prefix used to quote a literal
       (may be null)
        <LI><B>LITERAL_SUFFIX</B> String => suffix used to quote a literal
        (may be null)
        <LI><B>CREATE_PARAMS</B> String => parameters used in creating
       the type (may be null)
        <LI><B>NULLABLE</B> short => can you use NULL for this type?
       <UL>
       <LI> typeNoNulls - does not allow NULL values
       <LI> typeNullable - allows NULL values
       <LI> typeNullableUnknown - nullability unknown
       </UL>
        <LI><B>CASE_SENSITIVE</B> boolean=> is it case sensitive?
        <LI><B>SEARCHABLE</B> short => can you use "WHERE" based on this type:
       <UL>
       <LI> typePredNone - No support
       <LI> typePredChar - Only supported with WHERE .. LIKE
       <LI> typePredBasic - Supported except for WHERE .. LIKE
       <LI> typeSearchable - Supported for all WHERE ..
       </UL>
        <LI><B>UNSIGNED_ATTRIBUTE</B> boolean => is it unsigned?
        <LI><B>FIXED_PREC_SCALE</B> boolean => can it be a money value?
        <LI><B>AUTO_INCREMENT</B> boolean => can it be used for an
       auto-increment value?
        <LI><B>LOCAL_TYPE_NAME</B> String => localized version of type name
       (may be null)
        <LI><B>MINIMUM_SCALE</B> short => minimum scale supported
        <LI><B>MAXIMUM_SCALE</B> short => maximum scale supported
        <LI><B>SQL_DATA_TYPE</B> int => unused
        <LI><B>SQL_DATETIME_SUB</B> int => unused
        <LI><B>NUM_PREC_RADIX</B> int => usually 2 or 10
   </OL>

  @return <code>ResultSet</code> - each row is an SQL type description
}
function TZMsSqlDatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
begin
  Result:=inherited UncachedGetTypeInfo;

  with GetStatement.ExecuteQuery('exec sp_datatype_info') do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(TypeInfoTypeNameIndex, GetStringByName('TYPE_NAME'));
      Result.UpdateSmall(TypeInfoDataTypeIndex, Ord(ConvertODBCToSqlType(
        GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
      Result.UpdateInt(TypeInfoPecisionIndex, GetIntByName('PRECISION'));
      Result.UpdateString(TypeInfoLiteralPrefixIndex, GetStringByName('LITERAL_PREFIX'));
      Result.UpdateString(TypeInfoLiteralSuffixIndex, GetStringByName('LITERAL_SUFFIX'));
      Result.UpdateString(TypeInfoCreateParamsIndex, GetStringByName('CREATE_PARAMS'));
      Result.UpdateSmall(TypeInfoNullAbleIndex, GetSmallByName('NULLABLE'));
      Result.UpdateBoolean(TypeInfoCaseSensitiveIndex, GetSmallByName('CASE_SENSITIVE') = 1);
      Result.UpdateSmall(TypeInfoSearchableIndex, GetSmallByName('SEARCHABLE'));
      Result.UpdateBoolean(TypeInfoUnsignedAttributeIndex, GetSmallByName('UNSIGNED_ATTRIBUTE') = 1);
      Result.UpdateBoolean(TypeInfoFixedPrecScaleIndex, GetSmallByName('MONEY') = 1);
      Result.UpdateBoolean(TypeInfoAutoIncrementIndex, GetSmallByName('AUTO_INCREMENT') = 1);
      Result.UpdateString(TypeInfoLocaleTypeNameIndex, GetStringByName('LOCAL_TYPE_NAME'));
      Result.UpdateSmall(TypeInfoMinimumScaleIndex, GetSmallByName('MINIMUM_SCALE'));
      Result.UpdateSmall(TypeInfoMaximumScaleIndex, GetSmallByName('MAXIMUM_SCALE'));
      Result.UpdateSmall(TypeInfoSQLDataTypeIndex, GetSmallByName('SQL_DATA_TYPE'));
      Result.UpdateSmall(TypeInfoSQLDateTimeSubIndex, GetSmallByName('SQL_DATETIME_SUB'));
      Result.UpdateSmall(TypeInfoNumPrecRadix, GetSmallByName('NUM_PREC_RADIX'));
      Result.InsertRow;
    end;
    Close;
  end;
  Result.BeforeFirst;
end;

{**
  Gets a description of a table's indices and statistics. They are
  ordered by NON_UNIQUE, TYPE, INDEX_NAME, and ORDINAL_POSITION.

  <P>Each index column description has the following columns:
   <OL>
        <LI><B>TABLE_CAT</B> String => table catalog (may be null)
        <LI><B>TABLE_SCHEM</B> String => table schema (may be null)
        <LI><B>TABLE_NAME</B> String => table name
        <LI><B>NON_UNIQUE</B> boolean => Can index values be non-unique?
       false when TYPE is tableIndexStatistic
        <LI><B>INDEX_QUALIFIER</B> String => index catalog (may be null);
       null when TYPE is tableIndexStatistic
        <LI><B>INDEX_NAME</B> String => index name; null when TYPE is
       tableIndexStatistic
        <LI><B>TYPE</B> short => index type:
       <UL>
       <LI> tableIndexStatistic - this identifies table statistics that are
            returned in conjuction with a table's index descriptions
       <LI> tableIndexClustered - this is a clustered index
       <LI> tableIndexHashed - this is a hashed index
       <LI> tableIndexOther - this is some other style of index
       </UL>
        <LI><B>ORDINAL_POSITION</B> short => column sequence number
       within index; zero when TYPE is tableIndexStatistic
        <LI><B>COLUMN_NAME</B> String => column name; null when TYPE is
       tableIndexStatistic
        <LI><B>ASC_OR_DESC</B> String => column sort sequence, "A" => ascending,
       "D" => descending, may be null if sort sequence is not supported;
       null when TYPE is tableIndexStatistic
        <LI><B>CARDINALITY</B> int => When TYPE is tableIndexStatistic, then
       this is the number of rows in the table; otherwise, it is the
       number of unique values in the index.
        <LI><B>PAGES</B> int => When TYPE is  tableIndexStatisic then
       this is the number of pages used for the table, otherwise it
       is the number of pages used for the current index.
        <LI><B>FILTER_CONDITION</B> String => Filter condition, if any.
       (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param unique when true, return only indices for unique values;
      when false, return indices regardless of whether unique or not
  @param approximate when true, result is allowed to reflect approximate
      or out of data values; when false, results are requested to be
      accurate
  @return <code>ResultSet</code> - each row is an index column description
}
function TZMsSqlDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  Is_Unique, Accuracy: string;
begin
  Result:=inherited UncachedGetIndexInfo(Catalog, Schema, Table, Unique, Approximate);

  if Unique then
    Is_Unique := '''Y'''
  else Is_Unique := '''N''';
  if Approximate then
    Accuracy := '''Q'''
  else Accuracy := '''E''';

  with GetStatement.ExecuteQuery(
    Format('exec sp_statistics %s, %s, %s, ''%%'', %s, %s',
    [ComposeObjectString(Table), ComposeObjectString(Schema), ComposeObjectString(Catalog), Is_Unique, Accuracy])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_QUALIFIER'));
      Result.UpdateString(SchemaNameIndex, GetStringByName('TABLE_OWNER'));
      Result.UpdateString(TableNameIndex, GetStringByName('TABLE_NAME'));
      Result.UpdateBoolean(IndexInfoColNonUniqueIndex, GetSmallByName('NON_UNIQUE') = 1);
      Result.UpdateString(IndexInfoColIndexQualifierIndex, GetStringByName('INDEX_QUALIFIER'));
      Result.UpdateString(IndexInfoColIndexNameIndex, GetStringByName('INDEX_NAME'));
      Result.UpdateSmall(IndexInfoColTypeIndex, GetSmallByName('TYPE'));
      Result.UpdateSmall(IndexInfoColOrdPositionIndex, GetSmallByName('SEQ_IN_INDEX'));
      Result.UpdateString(IndexInfoColColumnNameIndex, GetStringByName('COLUMN_NAME'));
      Result.UpdateString(IndexInfoColAscOrDescIndex, GetStringByName('COLLATION'));
      Result.UpdateInt(IndexInfoColCardinalityIndex, GetIntByName('CARDINALITY'));
      Result.UpdateInt(IndexInfoColPagesIndex, GetIntByName('PAGES'));
      Result.UpdateString(IndexInfoColFilterConditionIndex, GetStringByName('FILTER_CONDITION'));
      Result.InsertRow;
    end;
    Close;
  end;
  Result.BeforeFirst;
end;

{ TZSybaseDatabaseMetadata }

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZSybaseDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZSybaseDatabaseInfo.Create(Self);
end;

{**
  Gets a description of the stored procedures available in a
  catalog.

  <P>Only procedure descriptions matching the schema and
  procedure name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM, and PROCEDURE_NAME.

  <P>Each procedure description has the the following columns:
   <OL>
        <LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
        <LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
        <LI><B>PROCEDURE_NAME</B> String => procedure name
   <LI> reserved for future use
   <LI> reserved for future use
   <LI> reserved for future use
        <LI><B>REMARKS</B> String => explanatory comment on the procedure
        <LI><B>PROCEDURE_TYPE</B> short => kind of procedure:
       <UL>
       <LI> procedureResultUnknown - May return a result
       <LI> procedureNoResult - Does not return a result
       <LI> procedureReturnsResult - Returns a result
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @return <code>ResultSet</code> - each row is a procedure description
  @see #getSearchStringEscape
}
function TZSybaseDatabaseMetadata.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
begin
    Result:=inherited UncachedGetProcedures(Catalog, SchemaPattern, ProcedureNamePattern);

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_stored_procedures %s, %s, %s',
      [ComposeObjectString(Catalog), ComposeObjectString(SchemaPattern), ComposeObjectString(ProcedureNamePattern)])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(CatalogNameIndex, GetStringByName('PROCEDURE_CAT'));
        Result.UpdateString(SchemaNameIndex, GetStringByName('PROCEDURE_SCHEM'));
        Result.UpdateString(ProcedureNameIndex, GetStringByName('PROCEDURE_NAME'));
        Result.UpdateString(ProcedureRemarksIndex, GetStringByName('REMARKS'));
        Result.UpdateSmall(ProcedureTypeIndex, GetSmallByName('PROCEDURE_TYPE'));
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a description of a catalog's stored procedure parameters
  and result columns.

  <P>Only descriptions matching the schema, procedure and
  parameter name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM and PROCEDURE_NAME. Within this, the return value,
  if any, is first. Next are the parameter descriptions in call
  order. The column descriptions follow in column number order.

  <P>Each row in the <code>ResultSet</code> is a parameter description or
  column description with the following fields:
   <OL>
        <LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
        <LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
        <LI><B>PROCEDURE_NAME</B> String => procedure name
        <LI><B>COLUMN_NAME</B> String => column/parameter name
        <LI><B>COLUMN_TYPE</B> Short => kind of column/parameter:
       <UL>
       <LI> procedureColumnUnknown - nobody knows
       <LI> procedureColumnIn - IN parameter
       <LI> procedureColumnInOut - INOUT parameter
       <LI> procedureColumnOut - OUT parameter
       <LI> procedureColumnReturn - procedure return value
       <LI> procedureColumnResult - result column in <code>ResultSet</code>
       </UL>
   <LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
        <LI><B>TYPE_NAME</B> String => SQL type name, for a UDT type the
   type name is fully qualified
        <LI><B>PRECISION</B> int => precision
        <LI><B>LENGTH</B> int => length in bytes of data
        <LI><B>SCALE</B> short => scale
        <LI><B>RADIX</B> short => radix
        <LI><B>NULLABLE</B> short => can it contain NULL?
       <UL>
       <LI> procedureNoNulls - does not allow NULL values
       <LI> procedureNullable - allows NULL values
       <LI> procedureNullableUnknown - nullability unknown
       </UL>
        <LI><B>REMARKS</B> String => comment describing parameter/column
   </OL>

  <P><B>Note:</B> Some databases may not return the column
  descriptions for a procedure. Additional columns beyond
  REMARKS can be defined by the database.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row describes a stored procedure parameter or
       column
  @see #getSearchStringEscape
}
function TZSybaseDatabaseMetadata.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  ProcNamePart: string;
  NumberPart: string;
  status2: Integer;
begin
  Result:=inherited UncachedGetProcedureColumns(Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);

  with GetStatement.ExecuteQuery(
    Format('exec sp_jdbc_getprocedurecolumns %s, %s, %s, %s',
    [ComposeObjectString(Catalog), ComposeObjectString(SchemaPattern), ComposeObjectString(ProcedureNamePattern), ComposeObjectString(ColumnNamePattern)])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetStringByName('PROCEDURE_CAT'));
      Result.UpdateString(SchemaNameIndex, GetStringByName('PROCEDURE_SCHEM'));
      Result.UpdateString(ProcColProcedureNameIndex, GetStringByName('PROCEDURE_NAME'));
      Result.UpdateString(ProcColColumnNameIndex, GetStringByName('COLUMN_NAME'));
      case GetSmallByName('COLUMN_TYPE') of
        0, 1: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctIn));
        2: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctInOut));
        3: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctUnknown));
        4: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctInOut));
        5: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctReturn));
      else
        Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctUnknown));
      end;
      Result.UpdateSmall(ProcColDataTypeIndex, Ord(ConvertODBCToSqlType(
        GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
      Result.UpdateString(ProcColTypeNameIndex, GetStringByName('TYPE_NAME'));
      Result.UpdateInt(ProcColPrecisionIndex, GetIntByName('PRECISION'));
      Result.UpdateInt(ProcColLengthIndex, GetIntByName('LENGTH'));
      Result.UpdateSmall(ProcColScaleIndex, GetSmallByName('SCALE'));
      Result.UpdateSmall(ProcColRadixIndex, GetSmallByName('RADIX'));
      Result.UpdateSmall(ProcColNullableIndex, GetSmallByName('NULLABLE'));
      Result.UpdateString(ProcColRemarksIndex, GetStringByName('REMARKS'));
      Result.InsertRow;
    end;
    Close;
  end;
  Result.BeforeFirst;

  NumberPart := '1';
  ProcNamePart := '';
  if AnsiPos(';', ProcNamePart) > 0 then
  begin
    NumberPart := Copy(ProcNamePart, LastDelimiter(';', ProcNamePart) + 1,
      Length(ProcNamePart));
    if NumberPart = '' then
      NumberPart := '1';

    ProcNamePart := Copy(ProcNamePart, 1, LastDelimiter(';', ProcNamePart));
    if ProcNamePart[Length(ProcNamePart)] = ';' then
      Delete(ProcNamePart, Length(ProcNamePart), 1);
  end;
//status2 is added in sybase ASE 12.5 to store the storedprocedure parameters
// input/output type this column does not exists in prior versions.
// In prior versions there is no way to determine between input or output type.
  with GetStatement.ExecuteQuery(
    Format('select c.* from syscolumns c inner join sysobjects o on'
    + ' (o.id = c.id) where o.name = %s and c.number = %s order by colid',
    [AnsiQuotedStr(ProcNamePart, ''''), NumberPart])) do
  begin
    Result.Next;//Skip return parameter
    while Next do
    begin
      Result.Next;
      if FindColumn('status2') >= 1 then
        status2 := GetSmallByName('status2')
      else
        status2 := 0;
      case status2 of
        0, 1: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctIn));
        2: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctInOut));
        3: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctUnknown));
        4: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctInOut));
        5: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctReturn));
      else
        Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctUnknown));
      end;
      Result.UpdateRow;
    end;
    Close;
  end;
end;

{**
  Gets a description of tables available in a catalog.

  <P>Only table descriptions matching the catalog, schema, table
  name and type criteria are returned.  They are ordered by
  TABLE_TYPE, TABLE_SCHEM and TABLE_NAME.

  <P>Each table description has the following columns:
   <OL>
        <LI><B>TABLE_CAT</B> String => table catalog (may be null)
        <LI><B>TABLE_SCHEM</B> String => table schema (may be null)
        <LI><B>TABLE_NAME</B> String => table name
        <LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
                        "VIEW", "SYSTEM TABLE", "GLOBAL TEMPORARY",
                        "LOCAL TEMPORARY", "ALIAS", "SYNONYM".
        <LI><B>REMARKS</B> String => explanatory comment on the table
   </OL>

  <P><B>Note:</B> Some databases may not return information for
  all tables.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param types a list of table types to include; null returns all types
  @return <code>ResultSet</code> - each row is a table description
  @see #getSearchStringEscape
}
function TZSybaseDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  I: Integer;
  TableTypes: string;
  StatementResult: IZResultSet;
begin
  Result:=inherited UncachedGetTables(Catalog, SchemaPattern, TableNamePattern, Types);

  TableTypes := '';
  for I := 0 to Length(Types) - 1 do
  begin
    if TableTypes <> '' then
      TableTypes := TableTypes + ',';
    TableTypes := TableTypes + AnsiQuotedStr(Types[I], '''');
  end;

  StatementResult := GetStatement.ExecuteQuery(Format('exec sp_jdbc_tables %s, %s, %s, %s',
    [ComposeObjectString(TableNamePattern), ComposeObjectString(SchemaPattern), ComposeObjectString(Catalog), ComposeObjectString(TableTypes)]));
  if Assigned(StatementResult) then with StatementResult do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_CAT'));
      Result.UpdateString(SchemaNameIndex, GetStringByName('TABLE_SCHEM'));
      Result.UpdateString(TableNameIndex, GetStringByName('TABLE_NAME'));
      Result.UpdateString(TableColumnsSQLType, GetStringByName('TABLE_TYPE'));
      Result.UpdateString(TableColumnsRemarks, GetStringByName('REMARKS'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Gets the schema names available in this database.  The results
  are ordered by schema name.

  <P>The schema column is:
   <OL>
        <LI><B>TABLE_SCHEM</B> String => schema name
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  schema name
}
function TZSybaseDatabaseMetadata.UncachedGetSchemas: IZResultSet;
begin
  Result:=inherited UncachedGetSchemas;

  with GetStatement.ExecuteQuery('exec sp_jdbc_getschemas') do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(SchemaColumnsTableSchemaIndex, GetStringByName('TABLE_SCHEM'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Gets the catalog names available in this database.  The results
  are ordered by catalog name.

  <P>The catalog column is:
   <OL>
        <LI><B>TABLE_CAT</B> String => catalog name
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  catalog name
}
function TZSybaseDatabaseMetadata.UncachedGetCatalogs: IZResultSet;
begin
  Result:=inherited UncachedGetCatalogs;

  with GetStatement.ExecuteQuery('exec sp_jdbc_getcatalogs') do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_CAT'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Gets the table types available in this database.  The results
  are ordered by table type.

  <P>The table type is:
   <OL>
        <LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
                        "VIEW", "SYSTEM TABLE", "GLOBAL TEMPORARY",
                        "LOCAL TEMPORARY", "ALIAS", "SYNONYM".
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  table type
}
function TZSybaseDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  TableTypes: array[0..2] of string = ('SYSTEM TABLE', 'TABLE', 'VIEW');
var
  I: Integer;
begin
  Result:=inherited UncachedGetTableTypes;

  for I := 0 to 2 do
  begin
    Result.MoveToInsertRow;
    Result.UpdateString(TableTypeColumnTableTypeIndex, TableTypes[I]);
    Result.InsertRow;
  end;
end;

{**
  Gets a description of table columns available in
  the specified catalog.

  <P>Only column descriptions matching the catalog, schema, table
  and column name criteria are returned.  They are ordered by
  TABLE_SCHEM, TABLE_NAME and ORDINAL_POSITION.

  <P>Each column description has the following columns:
   <OL>
        <LI><B>TABLE_CAT</B> String => table catalog (may be null)
        <LI><B>TABLE_SCHEM</B> String => table schema (may be null)
        <LI><B>TABLE_NAME</B> String => table name
        <LI><B>COLUMN_NAME</B> String => column name
        <LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
        <LI><B>TYPE_NAME</B> String => Data source dependent type name,
   for a UDT the type name is fully qualified
        <LI><B>COLUMN_SIZE</B> int => column size.  For char or date
            types this is the maximum number of characters, for numeric or
            decimal types this is precision.
        <LI><B>BUFFER_LENGTH</B> is not used.
        <LI><B>DECIMAL_DIGITS</B> int => the number of fractional digits
        <LI><B>NUM_PREC_RADIX</B> int => Radix (typically either 10 or 2)
        <LI><B>NULLABLE</B> int => is NULL allowed?
       <UL>
       <LI> columnNoNulls - might not allow NULL values
       <LI> columnNullable - definitely allows NULL values
       <LI> columnNullableUnknown - nullability unknown
       </UL>
        <LI><B>REMARKS</B> String => comment describing column (may be null)
        <LI><B>COLUMN_DEF</B> String => default value (may be null)
        <LI><B>SQL_DATA_TYPE</B> int => unused
        <LI><B>SQL_DATETIME_SUB</B> int => unused
        <LI><B>CHAR_OCTET_LENGTH</B> int => for char types the
        maximum number of bytes in the column
        <LI><B>ORDINAL_POSITION</B> int => index of column in table
       (starting at 1)
        <LI><B>IS_NULLABLE</B> String => "NO" means column definitely
       does not allow NULL values; "YES" means the column might
       allow NULL values.  An empty string means nobody knows.
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row is a column description
  @see #getSearchStringEscape
}
function TZSybaseDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  TempCatalog, TempSchema, TempTable, TempColumn: String;
begin
  Result := inherited UncachedGetColumns(Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);
  TempCatalog := RemoveQuotesFromIdentifier(Catalog);
  TempSchema := RemoveQuotesFromIdentifier(SchemaPattern);
  TempTable := RemoveQuotesFromIdentifier(TableNamePattern);
  TempColumn := RemoveQuotesFromIdentifier(ColumnNamePattern);

  with GetStatement.ExecuteQuery('exec '+GetSP_Prefix(Catalog, SchemaPattern)+
    'sp_jdbc_columns '+ComposeObjectString(TempTable)+', '+
    ComposeObjectString(TempSchema)+', '+ComposeObjectString(TempCatalog)+', '+
    ComposeObjectString(TempColumn)) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_CAT'));
      Result.UpdateString(SchemaNameIndex, GetStringByName('TABLE_SCHEM'));
      Result.UpdateString(TableNameIndex, GetStringByName('TABLE_NAME'));
      Result.UpdateString(ColumnNameIndex, GetStringByName('COLUMN_NAME'));
//The value in the resultset will be used
//      Result.UpdateSmall(TableColColumnTypeIndex,
//        Ord(ConvertODBCToSqlType(GetSmallByName('DATA_TYPE'))));
      Result.UpdateString(TableColColumnTypeNameIndex, GetStringByName('TYPE_NAME'));
      Result.UpdateInt(TableColColumnSizeIndex, GetIntByName('COLUMN_SIZE'));
      Result.UpdateInt(TableColColumnBufLengthIndex, GetIntByName('BUFFER_LENGTH'));
      Result.UpdateInt(TableColColumnDecimalDigitsIndex, GetIntByName('DECIMAL_DIGITS'));
      Result.UpdateInt(TableColColumnNumPrecRadixIndex, GetSmallByName('NUM_PREC_RADIX'));
      Result.UpdateSmall(TableColColumnNullableIndex, GetSmallByName('NULLABLE'));
      Result.UpdateString(TableColColumnRemarksIndex, GetStringByName('REMARKS'));
      Result.UpdateString(TableColColumnColDefIndex, GetStringByName('COLUMN_DEF'));
      Result.UpdateSmall(TableColColumnSQLDataTypeIndex, GetSmallByName('SQL_DATA_TYPE'));
      Result.UpdateSmall(TableColColumnSQLDateTimeSubIndex, GetSmallByName('SQL_DATETIME_SUB'));
      Result.UpdateInt(TableColColumnCharOctetLengthIndex, GetIntByName('CHAR_OCTET_LENGTH'));
      Result.UpdateInt(TableColColumnOrdPosIndex, GetIntByName('ORDINAL_POSITION'));
      Result.UpdateString(TableColColumnIsNullableIndex, GetStringByName('IS_NULLABLE'));
      Result.UpdateBoolean(TableColColumnCaseSensitiveIndex, IC.GetIdentifierCase(GetStringByName('TYPE_NAME'), True) in [icMixed, icSpecial]);
      Result.InsertRow;
    end;
    Close;
  end;
  Result.BeforeFirst;
  with GetStatement.ExecuteQuery(
    Format('select c.colid, c.name, c.type, c.prec, c.scale, c.status'
    + ' from syscolumns c inner join sysobjects o on (o.id = c.id)'
    + ' where o.name like %s order by colid', [ComposeObjectString(TempTable)])) do
  begin
    while Next do
    begin
      Result.Next;
      Result.UpdateBoolean(TableColColumnAutoIncIndex, (GetSmallByName('status') and $80) <> 0);
      Result.UpdateBoolean(TableColColumnSearchableIndex, not (GetSmallByName('type') in [34, 35]));
      Result.UpdateBoolean(TableColColumnWritableIndex, ((GetSmallByName('status') and $80) = 0)
        (*and (GetSmallByName('type') <> 37)*));   // <<<< *DEBUG WARUM?
      Result.UpdateBoolean(TableColColumnDefinitelyWritableIndex, Result.GetBooleanByName('WRITABLE'));
      Result.UpdateBoolean(TableColColumnReadonlyIndex, not Result.GetBooleanByName('WRITABLE'));
      if Result.GetBoolean(TableColColumnAutoIncIndex) then
      begin
        Result.UpdateSmall(TableColColumnNullableIndex, 1);
        Result.UpdateString(TableColColumnIsNullableIndex, 'YES');
      end;
      Result.UpdateRow;
    end;
    Close;
  end;
end;

{**
  Gets a description of the access rights for a table's columns.

  <P>Only privileges matching the column name criteria are
  returned.  They are ordered by COLUMN_NAME and PRIVILEGE.

  <P>Each privilige description has the following columns:
   <OL>
        <LI><B>TABLE_CAT</B> String => table catalog (may be null)
        <LI><B>TABLE_SCHEM</B> String => table schema (may be null)
        <LI><B>TABLE_NAME</B> String => table name
        <LI><B>COLUMN_NAME</B> String => column name
        <LI><B>GRANTOR</B> => grantor of access (may be null)
        <LI><B>GRANTEE</B> String => grantee of access
        <LI><B>PRIVILEGE</B> String => name of access (SELECT,
       INSERT, UPDATE, REFRENCES, ...)
        <LI><B>IS_GRANTABLE</B> String => "YES" if grantee is permitted
       to grant to others; "NO" if not; null if unknown
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row is a column privilege description
  @see #getSearchStringEscape
}
function TZSybaseDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
begin
  Result:=inherited UncachedGetColumnPrivileges(Catalog, Schema, Table, ColumnNamePattern);

  with GetStatement.ExecuteQuery(
    Format('exec sp_jdbc_getcolumnprivileges %s, %s, %s, %s',
    [ComposeObjectString(Catalog), ComposeObjectString(Schema), ComposeObjectString(Table),
     ComposeObjectString(ColumnNamePattern, '''%''')])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_CAT'));
      Result.UpdateString(SchemaNameIndex, GetStringByName('TABLE_SCHEM'));
      Result.UpdateString(TableNameIndex, GetStringByName('TABLE_NAME'));
      Result.UpdateString(ColumnNameIndex, GetStringByName('COLUMN_NAME'));
      Result.UpdateString(TableColPrivGrantorIndex, GetStringByName('GRANTOR'));
      Result.UpdateString(TableColPrivGranteeIndex, GetStringByName('GRANTEE'));
      Result.UpdateString(TableColPrivPrivilegeIndex, GetStringByName('PRIVILEGE'));
      Result.UpdateString(TableColPrivIsGrantableIndex, GetStringByName('IS_GRANTABLE'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Gets a description of the access rights for each table available
  in a catalog. Note that a table privilege applies to one or
  more columns in the table. It would be wrong to assume that
  this priviledge applies to all columns (this may be true for
  some systems but is not true for all.)

  <P>Only privileges matching the schema and table name
  criteria are returned.  They are ordered by TABLE_SCHEM,
  TABLE_NAME, and PRIVILEGE.

  <P>Each privilige description has the following columns:
   <OL>
        <LI><B>TABLE_CAT</B> String => table catalog (may be null)
        <LI><B>TABLE_SCHEM</B> String => table schema (may be null)
        <LI><B>TABLE_NAME</B> String => table name
        <LI><B>GRANTOR</B> => grantor of access (may be null)
        <LI><B>GRANTEE</B> String => grantee of access
        <LI><B>PRIVILEGE</B> String => name of access (SELECT,
       INSERT, UPDATE, REFRENCES, ...)
        <LI><B>IS_GRANTABLE</B> String => "YES" if grantee is permitted
       to grant to others; "NO" if not; null if unknown
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @return <code>ResultSet</code> - each row is a table privilege description
  @see #getSearchStringEscape
}
function TZSybaseDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
begin
  Result:=inherited UncachedGetTablePrivileges(Catalog, SchemaPattern, TableNamePattern);

  with GetStatement.ExecuteQuery(
    Format('exec sp_jdbc_gettableprivileges %s, %s, %s',
    [ComposeObjectString(Catalog), ComposeObjectString(SchemaPattern), ComposeObjectString(TableNamePattern)])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_CAT'));
      Result.UpdateString(SchemaNameIndex, GetStringByName('TABLE_SCHEM'));
      Result.UpdateString(TableNameIndex, GetStringByName('TABLE_NAME'));
      Result.UpdateString(TablePrivGrantorIndex, GetStringByName('GRANTOR'));
      Result.UpdateString(TablePrivGranteeIndex, GetStringByName('GRANTEE'));
      Result.UpdateString(TablePrivPrivilegeIndex, GetStringByName('PRIVILEGE'));
      Result.UpdateString(TablePrivIsGrantableIndex, GetStringByName('IS_GRANTABLE'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Gets a description of a table's columns that are automatically
  updated when any value in a row is updated.  They are
  unordered.

  <P>Each column description has the following columns:
   <OL>
        <LI><B>SCOPE</B> short => is not used
        <LI><B>COLUMN_NAME</B> String => column name
        <LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
        <LI><B>TYPE_NAME</B> String => Data source dependent type name
        <LI><B>COLUMN_SIZE</B> int => precision
        <LI><B>BUFFER_LENGTH</B> int => length of column value in bytes
        <LI><B>DECIMAL_DIGITS</B> short  => scale
        <LI><B>PSEUDO_COLUMN</B> short => is this a pseudo column
       like an Oracle ROWID
       <UL>
       <LI> versionColumnUnknown - may or may not be pseudo column
       <LI> versionColumnNotPseudo - is NOT a pseudo column
       <LI> versionColumnPseudo - is a pseudo column
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a column description
  @exception SQLException if a database access error occurs
}
function TZSybaseDatabaseMetadata.UncachedGetVersionColumns(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result:=inherited UncachedGetVersionColumns(Catalog, Schema, Table);

  with GetStatement.ExecuteQuery(
    Format('exec sp_jdbc_getversioncolumns %s, %s, %s',
    [ComposeObjectString(Catalog), ComposeObjectString(Schema), ComposeObjectString(Table)])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateSmall(TableColVerScopeIndex, GetSmallByName('SCOPE'));
      Result.UpdateString(TableColVerColNameIndex, GetStringByName('COLUMN_NAME'));
      Result.UpdateSmall(TableColVerDataTypeIndex,
        Ord(ConvertODBCToSqlType(GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
      Result.UpdateString(TableColVerTypeNameIndex, GetStringByName('TYPE_NAME'));
      Result.UpdateInt(TableColVerColSizeIndex, GetIntByName('COLUMN_SIZE'));
      Result.UpdateInt(TableColVerBufLengthIndex, GetIntByName('BUFFER_LENGTH'));
      Result.UpdateInt(TableColVerDecimalDigitsIndex, GetIntByName('DECIMAL_DIGITS'));
      Result.UpdateSmall(TableColVerPseudoColumnIndex, GetSmallByName('PSEUDO_COLUMN'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Removes Quotes from Identifier Names if they are passed with quotes.

  @param Identifier The Identifier where the quotes are to be removed
  @return The identifier without quotes
}
function TZSybaseDatabaseMetadata.RemoveQuotesFromIdentifier(const Identifier: String): String;
var
  QuoteStr: String;
begin
  QuoteStr := GetDatabaseInfo.GetIdentifierQuoteString;
  if Length(Identifier) > 0 then begin
    if (Identifier[1] = QuoteStr) and (Identifier[Length(Identifier)] = QuoteStr)
    then Result := Copy(Identifier, 2, length(Identifier) - 2)
    else Result := Identifier;
  end else begin
    Result := Identifier;
  end;
end;

{**
  Gets a description of a table's primary key columns.  They
  are ordered by COLUMN_NAME.

  <P>Each primary key column description has the following columns:
   <OL>
        <LI><B>TABLE_CAT</B> String => table catalog (may be null)
        <LI><B>TABLE_SCHEM</B> String => table schema (may be null)
        <LI><B>TABLE_NAME</B> String => table name
        <LI><B>COLUMN_NAME</B> String => column name
        <LI><B>KEY_SEQ</B> short => sequence number within primary key
        <LI><B>PK_NAME</B> String => primary key name (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a primary key column description
  @exception SQLException if a database access error occurs
}
function TZSybaseDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  TempCatalog, TempSchema, TempTable: String;
begin
  Result:=inherited UncachedGetPrimaryKeys(Catalog, Schema, Table);
  TempCatalog := RemoveQuotesFromIdentifier(Catalog);
  TempSchema := RemoveQuotesFromIdentifier(Schema);
  TempTable := RemoveQuotesFromIdentifier(Table);

  with GetStatement.ExecuteQuery(
    Format('exec sp_jdbc_primarykey %s, %s, %s',
    [ComposeObjectString(TempCatalog), ComposeObjectString(TempSchema), ComposeObjectString(TempTable)])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_CAT'));
      Result.UpdateString(SchemaNameIndex, GetStringByName('TABLE_SCHEM'));
      Result.UpdateString(TableNameIndex, GetStringByName('TABLE_NAME'));
      Result.UpdateString(PrimaryKeyColumnNameIndex, GetStringByName('COLUMN_NAME'));
      Result.UpdateSmall(PrimaryKeyKeySeqIndex, GetSmallByName('KEY_SEQ'));
      Result.UpdateString(PrimaryKeyPKNameIndex, GetStringByName('PK_NAME'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Gets a description of the primary key columns that are
  referenced by a table's foreign key columns (the primary keys
  imported by a table).  They are ordered by PKTABLE_CAT,
  PKTABLE_SCHEM, PKTABLE_NAME, and KEY_SEQ.

  <P>Each primary key column description has the following columns:
   <OL>
        <LI><B>PKTABLE_CAT</B> String => primary key table catalog
       being imported (may be null)
        <LI><B>PKTABLE_SCHEM</B> String => primary key table schema
       being imported (may be null)
        <LI><B>PKTABLE_NAME</B> String => primary key table name
       being imported
        <LI><B>PKCOLUMN_NAME</B> String => primary key column name
       being imported
        <LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
        <LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
        <LI><B>FKTABLE_NAME</B> String => foreign key table name
        <LI><B>FKCOLUMN_NAME</B> String => foreign key column name
        <LI><B>KEY_SEQ</B> short => sequence number within foreign key
        <LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
        <LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
        <LI><B>FK_NAME</B> String => foreign key name (may be null)
        <LI><B>PK_NAME</B> String => primary key name (may be null)
        <LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a primary key column description
  @see #getExportedKeys
}
function TZSybaseDatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result:=inherited UncachedGetImportedKeys(Catalog, Schema, Table);

  with GetStatement.ExecuteQuery(
    Format('exec sp_jdbc_importkey %s, %s, %s',
    [ComposeObjectString(Catalog), ComposeObjectString(Schema), ComposeObjectString(Table)])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(ImportedKeyColPKTableCatalogIndex, GetStringByName('PKTABLE_CAT'));
      Result.UpdateString(ImportedKeyColPKTableSchemaIndex, GetStringByName('PKTABLE_SCHEM'));
      Result.UpdateString(ImportedKeyColPKTableNameIndex, GetStringByName('PKTABLE_NAME'));
      Result.UpdateString(ImportedKeyColPKColumnNameIndex, GetStringByName('PKCOLUMN_NAME'));
      Result.UpdateString(ImportedKeyColFKTableCatalogIndex, GetStringByName('FKTABLE_CAT'));
      Result.UpdateString(ImportedKeyColFKTableSchemaIndex, GetStringByName('FKTABLE_SCHEM'));
      Result.UpdateString(ImportedKeyColFKTableNameIndex, GetStringByName('FKTABLE_NAME'));
      Result.UpdateString(ImportedKeyColFKColumnNameIndex, GetStringByName('FKCOLUMN_NAME'));
      Result.UpdateSmall(ImportedKeyColKeySeqIndex, GetSmallByName('KEY_SEQ'));
      Result.UpdateSmall(ImportedKeyColUpdateRuleIndex, GetSmallByName('UPDATE_RULE'));
      Result.UpdateSmall(ImportedKeyColDeleteRuleIndex, GetSmallByName('DELETE_RULE'));
      Result.UpdateString(ImportedKeyColFKNameIndex, GetStringByName('FK_NAME'));
      Result.UpdateString(ImportedKeyColPKNameIndex, GetStringByName('PK_NAME'));
      Result.UpdateInt(ImportedKeyColDeferrabilityIndex, GetIntByName('DEFERRABILITY'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Gets a description of the foreign key columns that reference a
  table's primary key columns (the foreign keys exported by a
  table).  They are ordered by FKTABLE_CAT, FKTABLE_SCHEM,
  FKTABLE_NAME, and KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
        <LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
        <LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
        <LI><B>PKTABLE_NAME</B> String => primary key table name
        <LI><B>PKCOLUMN_NAME</B> String => primary key column name
        <LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
        <LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
        <LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
        <LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
        <LI><B>KEY_SEQ</B> short => sequence number within foreign key
        <LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
        <LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
        <LI><B>FK_NAME</B> String => foreign key name (may be null)
        <LI><B>PK_NAME</B> String => primary key name (may be null)
        <LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZSybaseDatabaseMetadata.UncachedGetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result := inherited UncachedGetExportedKeys(Catalog, Schema, Table);

  with GetStatement.ExecuteQuery(
    Format('exec sp_jdbc_exportkey %s, %s, %s',
    [ComposeObjectString(Catalog), ComposeObjectString(Schema), ComposeObjectString(Table)])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(ExportedKeyColPKTableCatalogIndex, GetStringByName('PKTABLE_CAT'));
      Result.UpdateString(ExportedKeyColPKTableSchemaIndex, GetStringByName('PKTABLE_SCHEM'));
      Result.UpdateString(ExportedKeyColPKTableNameIndex, GetStringByName('PKTABLE_NAME'));
      Result.UpdateString(ExportedKeyColPKColumnNameIndex, GetStringByName('PKCOLUMN_NAME'));
      Result.UpdateString(ExportedKeyColFKTableCatalogIndex, GetStringByName('FKTABLE_CAT'));
      Result.UpdateString(ExportedKeyColFKTableSchemaIndex, GetStringByName('FKTABLE_SCHEM'));
      Result.UpdateString(ExportedKeyColFKTableNameIndex, GetStringByName('FKTABLE_NAME'));
      Result.UpdateString(ExportedKeyColFKColumnNameIndex, GetStringByName('FKCOLUMN_NAME'));
      Result.UpdateSmall(ExportedKeyColKeySeqIndex, GetSmallByName('KEY_SEQ'));
      Result.UpdateSmall(ExportedKeyColUpdateRuleIndex, GetSmallByName('UPDATE_RULE'));
      Result.UpdateSmall(ExportedKeyColDeleteRuleIndex, GetSmallByName('DELETE_RULE'));
      Result.UpdateString(ExportedKeyColFKNameIndex, GetStringByName('FK_NAME'));
      Result.UpdateString(ExportedKeyColPKNameIndex, GetStringByName('PK_NAME'));
      Result.UpdateInt(ExportedKeyColDeferrabilityIndex, GetIntByName('DEFERRABILITY'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Gets a description of the foreign key columns in the foreign key
  table that reference the primary key columns of the primary key
  table (describe how one table imports another's key.) This
  should normally return a single foreign key/primary key pair
  (most tables only import a foreign key from a table once.)  They
  are ordered by FKTABLE_CAT, FKTABLE_SCHEM, FKTABLE_NAME, and
  KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
        <LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
        <LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
        <LI><B>PKTABLE_NAME</B> String => primary key table name
        <LI><B>PKCOLUMN_NAME</B> String => primary key column name
        <LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
        <LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
        <LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
        <LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
        <LI><B>KEY_SEQ</B> short => sequence number within foreign key
        <LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
        <LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
        <LI><B>FK_NAME</B> String => foreign key name (may be null)
        <LI><B>PK_NAME</B> String => primary key name (may be null)
        <LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param primaryCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param primarySchema a schema name; "" retrieves those
  without a schema
  @param primaryTable the table name that exports the key
  @param foreignCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param foreignSchema a schema name; "" retrieves those
  without a schema
  @param foreignTable the table name that imports the key
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZSybaseDatabaseMetadata.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
var
  Statement: String;
begin
  Result:=inherited UncachedGetCrossReference(PrimaryCatalog, PrimarySchema, PrimaryTable,
                                      ForeignCatalog, ForeignSchema, ForeignTable);

  statement :=
    Format('exec sp_jdbc_getcrossreferences %s, %s, %s, %s, %s, %s',
    [ComposeObjectString(PrimaryCatalog), ComposeObjectString(PrimarySchema), ComposeObjectString(PrimaryTable),
     ComposeObjectString(ForeignCatalog), ComposeObjectString(ForeignSchema), ComposeObjectString(ForeignTable)]);

  with GetStatement.ExecuteQuery(Statement
       ) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CrossRefKeyColPKTableCatalogIndex, GetStringByName('PKTABLE_CAT'));
      Result.UpdateString(CrossRefKeyColPKTableSchemaIndex, GetStringByName('PKTABLE_SCHEM'));
      Result.UpdateString(CrossRefKeyColPKTableNameIndex, GetStringByName('PKTABLE_NAME'));
      Result.UpdateString(CrossRefKeyColPKColumnNameIndex, GetStringByName('PKCOLUMN_NAME'));
      Result.UpdateString(CrossRefKeyColFKTableCatalogIndex, GetStringByName('FKTABLE_CAT'));
      Result.UpdateString(CrossRefKeyColFKTableSchemaIndex, GetStringByName('FKTABLE_SCHEM'));
      Result.UpdateString(CrossRefKeyColFKTableNameIndex, GetStringByName('FKTABLE_NAME'));
      Result.UpdateString(CrossRefKeyColFKColumnNameIndex, GetStringByName('FKCOLUMN_NAME'));
      Result.UpdateSmall(CrossRefKeyColKeySeqIndex, GetSmallByName('KEY_SEQ'));
      Result.UpdateSmall(CrossRefKeyColUpdateRuleIndex, GetSmallByName('UPDATE_RULE'));
      Result.UpdateSmall(CrossRefKeyColDeleteRuleIndex, GetSmallByName('DELETE_RULE'));
      Result.UpdateString(CrossRefKeyColFKNameIndex, GetStringByName('FK_NAME'));
      Result.UpdateString(CrossRefKeyColPKNameIndex, GetStringByName('PK_NAME'));
      Result.UpdateInt(CrossRefKeyColDeferrabilityIndex, GetIntByName('DEFERRABILITY'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Gets a description of all the standard SQL types supported by
  this database. They are ordered by DATA_TYPE and then by how
  closely the data type maps to the corresponding JDBC SQL type.

  <P>Each type description has the following columns:
   <OL>
        <LI><B>TYPE_NAME</B> String => Type name
        <LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
        <LI><B>PRECISION</B> int => maximum precision
        <LI><B>LITERAL_PREFIX</B> String => prefix used to quote a literal
       (may be null)
        <LI><B>LITERAL_SUFFIX</B> String => suffix used to quote a literal
        (may be null)
        <LI><B>CREATE_PARAMS</B> String => parameters used in creating
       the type (may be null)
        <LI><B>NULLABLE</B> short => can you use NULL for this type?
       <UL>
       <LI> typeNoNulls - does not allow NULL values
       <LI> typeNullable - allows NULL values
       <LI> typeNullableUnknown - nullability unknown
       </UL>
        <LI><B>CASE_SENSITIVE</B> boolean=> is it case sensitive?
        <LI><B>SEARCHABLE</B> short => can you use "WHERE" based on this type:
       <UL>
       <LI> typePredNone - No support
       <LI> typePredChar - Only supported with WHERE .. LIKE
       <LI> typePredBasic - Supported except for WHERE .. LIKE
       <LI> typeSearchable - Supported for all WHERE ..
       </UL>
        <LI><B>UNSIGNED_ATTRIBUTE</B> boolean => is it unsigned?
        <LI><B>FIXED_PREC_SCALE</B> boolean => can it be a money value?
        <LI><B>AUTO_INCREMENT</B> boolean => can it be used for an
       auto-increment value?
        <LI><B>LOCAL_TYPE_NAME</B> String => localized version of type name
       (may be null)
        <LI><B>MINIMUM_SCALE</B> short => minimum scale supported
        <LI><B>MAXIMUM_SCALE</B> short => maximum scale supported
        <LI><B>SQL_DATA_TYPE</B> int => unused
        <LI><B>SQL_DATETIME_SUB</B> int => unused
        <LI><B>NUM_PREC_RADIX</B> int => usually 2 or 10
   </OL>

  @return <code>ResultSet</code> - each row is an SQL type description
}
function TZSybaseDatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
begin
  Result:=inherited UncachedGetTypeInfo;

  with GetStatement.ExecuteQuery('exec sp_jdbc_datatype_info') do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(TypeInfoTypeNameIndex, GetStringByName('TYPE_NAME'));
      Result.UpdateSmall(TypeInfoDataTypeIndex,
        Ord(ConvertODBCToSqlType(GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
      Result.UpdateInt(TypeInfoPecisionIndex, GetIntByName('PRECISION'));
      Result.UpdateString(TypeInfoLiteralPrefixIndex, GetStringByName('LITERAL_PREFIX'));
      Result.UpdateString(TypeInfoLiteralSuffixIndex, GetStringByName('LITERAL_SUFFIX'));
      Result.UpdateString(TypeInfoCreateParamsIndex, GetStringByName('CREATE_PARAMS'));
      Result.UpdateSmall(TypeInfoNullAbleIndex, GetSmallByName('NULLABLE'));
      Result.UpdateBoolean(TypeInfoCaseSensitiveIndex, GetSmallByName('CASE_SENSITIVE') = 1);
      Result.UpdateSmall(TypeInfoSearchableIndex, GetSmallByName('SEARCHABLE'));
      Result.UpdateBoolean(TypeInfoUnsignedAttributeIndex, GetSmallByName('UNSIGNED_ATTRIBUTE') = 1);
      Result.UpdateBoolean(TypeInfoFixedPrecScaleIndex, GetSmallByName('FIXED_PREC_SCALE') = 1);
      Result.UpdateBoolean(TypeInfoAutoIncrementIndex, GetSmallByName('AUTO_INCREMENT') = 1);
      Result.UpdateString(TypeInfoLocaleTypeNameIndex, GetStringByName('LOCAL_TYPE_NAME'));
      Result.UpdateSmall(TypeInfoMinimumScaleIndex, GetSmallByName('MINIMUM_SCALE'));
      Result.UpdateSmall(TypeInfoMaximumScaleIndex, GetSmallByName('MAXIMUM_SCALE'));
      Result.UpdateSmall(TypeInfoSQLDataTypeIndex, GetSmallByName('SQL_DATA_TYPE'));
      Result.UpdateSmall(TypeInfoSQLDateTimeSubIndex, GetSmallByName('SQL_DATETIME_SUB'));
      Result.UpdateSmall(TypeInfoNumPrecRadix, GetSmallByName('NUM_PREC_RADIX'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Gets a description of a table's indices and statistics. They are
  ordered by NON_UNIQUE, TYPE, INDEX_NAME, and ORDINAL_POSITION.

  <P>Each index column description has the following columns:
   <OL>
        <LI><B>TABLE_CAT</B> String => table catalog (may be null)
        <LI><B>TABLE_SCHEM</B> String => table schema (may be null)
        <LI><B>TABLE_NAME</B> String => table name
        <LI><B>NON_UNIQUE</B> boolean => Can index values be non-unique?
       false when TYPE is tableIndexStatistic
        <LI><B>INDEX_QUALIFIER</B> String => index catalog (may be null);
       null when TYPE is tableIndexStatistic
        <LI><B>INDEX_NAME</B> String => index name; null when TYPE is
       tableIndexStatistic
        <LI><B>TYPE</B> short => index type:
       <UL>
       <LI> tableIndexStatistic - this identifies table statistics that are
            returned in conjuction with a table's index descriptions
       <LI> tableIndexClustered - this is a clustered index
       <LI> tableIndexHashed - this is a hashed index
       <LI> tableIndexOther - this is some other style of index
       </UL>
        <LI><B>ORDINAL_POSITION</B> short => column sequence number
       within index; zero when TYPE is tableIndexStatistic
        <LI><B>COLUMN_NAME</B> String => column name; null when TYPE is
       tableIndexStatistic
        <LI><B>ASC_OR_DESC</B> String => column sort sequence, "A" => ascending,
       "D" => descending, may be null if sort sequence is not supported;
       null when TYPE is tableIndexStatistic
        <LI><B>CARDINALITY</B> int => When TYPE is tableIndexStatistic, then
       this is the number of rows in the table; otherwise, it is the
       number of unique values in the index.
        <LI><B>PAGES</B> int => When TYPE is  tableIndexStatisic then
       this is the number of pages used for the table, otherwise it
       is the number of pages used for the current index.
        <LI><B>FILTER_CONDITION</B> String => Filter condition, if any.
       (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param unique when true, return only indices for unique values;
      when false, return indices regardless of whether unique or not
  @param approximate when true, result is allowed to reflect approximate
      or out of data values; when false, results are requested to be
      accurate
  @return <code>ResultSet</code> - each row is an index column description
}
function TZSybaseDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  Is_Unique, Accuracy: string;
begin
  Result:=inherited UncachedGetIndexInfo(Catalog, Schema, Table, Unique, Approximate);

  Is_Unique := AnsiQuotedStr(BoolStrInts[Unique], '''');
  Accuracy := AnsiQuotedStr(BoolStrInts[Approximate], '''');

  with GetStatement.ExecuteQuery(
    Format('exec sp_jdbc_getindexinfo %s, %s, %s, %s, %s',
    [ComposeObjectString(Catalog), ComposeObjectString(Schema), ComposeObjectString(Table), Is_Unique, Accuracy])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetStringByName('TABLE_CAT'));
      Result.UpdateString(SchemaNameIndex, GetStringByName('TABLE_SCHEM'));
      Result.UpdateString(TableNameIndex, GetStringByName('TABLE_NAME'));
      Result.UpdateBoolean(IndexInfoColNonUniqueIndex, GetSmallByName('NON_UNIQUE') = 1);
      Result.UpdateString(IndexInfoColIndexQualifierIndex, GetStringByName('INDEX_QUALIFIER'));
      Result.UpdateString(IndexInfoColIndexNameIndex, GetStringByName('INDEX_NAME'));
      Result.UpdateSmall(IndexInfoColTypeIndex, GetSmallByName('TYPE'));
      Result.UpdateSmall(IndexInfoColOrdPositionIndex, GetSmallByName('ORDINAL_POSITION'));
      Result.UpdateString(IndexInfoColColumnNameIndex, GetStringByName('COLUMN_NAME'));
      Result.UpdateString(IndexInfoColAscOrDescIndex, GetStringByName('ASC_OR_DESC'));
      Result.UpdateInt(IndexInfoColCardinalityIndex, GetIntByName('CARDINALITY'));
      Result.UpdateInt(IndexInfoColPagesIndex, GetIntByName('PAGES'));
      Result.UpdateString(IndexInfoColFilterConditionIndex, GetStringByName('FILTER_CONDITION'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**

  Gets a description of the user-defined types defined in a particular
  schema.  Schema-specific UDTs may have type JAVA_OBJECT, STRUCT,
  or DISTINCT.

  <P>Only types matching the catalog, schema, type name and type
  criteria are returned.  They are ordered by DATA_TYPE, TYPE_SCHEM
  and TYPE_NAME.  The type name parameter may be a fully-qualified
  name.  In this case, the catalog and schemaPattern parameters are
  ignored.

  <P>Each type description has the following columns:
   <OL>
        <LI><B>TYPE_CAT</B> String => the type's catalog (may be null)
        <LI><B>TYPE_SCHEM</B> String => type's schema (may be null)
        <LI><B>TYPE_NAME</B> String => type name
   <LI><B>CLASS_NAME</B> String => Java class name
        <LI><B>DATA_TYPE</B> String => type value defined in java.sql.Types.
   One of JAVA_OBJECT, STRUCT, or DISTINCT
        <LI><B>REMARKS</B> String => explanatory comment on the type
   </OL>

  <P><B>Note:</B> If the driver does not support UDTs, an empty
  result set is returned.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param typeNamePattern a type name pattern; may be a fully-qualified name
  @param types a list of user-named types to include (JAVA_OBJECT,
  STRUCT, or DISTINCT); null returns all types
  @return <code>ResultSet</code> - each row is a type description
}
function TZSybaseDatabaseMetadata.UncachedGetUDTs(const Catalog: string;
  const SchemaPattern: string; const TypeNamePattern: string;
  const Types: TIntegerDynArray): IZResultSet;
var
  I: Integer;
  UDTypes: string;
begin
  Result:=inherited UncachedGetUDTs(Catalog, SchemaPattern, TypeNamePattern, Types);

  UDTypes := '';
  for I := 0 to Length(Types) - 1 do
  begin
    if Length(UDTypes) > 0 then
      UDTypes := UDTypes + ',';
    UDTypes := UDTypes + AnsiQuotedStr(ZFastCode.IntToStr(Types[I]), '''');
  end;

  with GetStatement.ExecuteQuery(
    Format('exec sp_jdbc_getudts %s, %s, %s, %s',
    [ComposeObjectString(Catalog), ComposeObjectString(SchemaPattern, '''%'''),
     ComposeObjectString(TypeNamePattern, '''%'''), ComposeObjectString(UDTypes)])) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetStringByName('TYPE_CAT'));
      Result.UpdateString(SchemaNameIndex, GetStringByName('TYPE_SCHEM'));
      Result.UpdateString(UDTColTypeNameIndex, GetStringByName('TYPE_NAME'));
      Result.UpdateString(UDTColClassNameIndex, GetStringByName('JAVA_CLASS'));
      Result.UpdateSmall(UDTColDataTypeIndex, Ord(ConvertODBCToSqlType(
        GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
      Result.UpdateString(UDTColRemarksIndex, GetStringByName('REMARKS'));
      Result.InsertRow;
    end;
    Close;
  end;
end;

end.



