{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                           and Sergey Merkuriev          }
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

unit ZDbcMySqlMetadata;

interface

{$I ZDbc.inc}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {%H-}ZClasses, ZSysUtils, ZDbcIntfs, ZDbcMetadata, ZCompatibility,
  ZURL, ZDbcConnection;

type

  // technobot 2008-06-26 - methods moved as is from TZMySQLDatabaseMetadata:
  {** Implements MySQL Database Information. }
  TZMySQLDatabaseInfo = class(TZAbstractDatabaseInfo)
  protected
    procedure GetVersion(var MajorVersion, MinorVersion: integer);
  public
    constructor Create(const Metadata: TZAbstractDatabaseMetadata);

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
//    function SupportsMixedCaseIdentifiers: Boolean; override; -> Not implemented
//    function SupportsMixedCaseQuotedIdentifiers: Boolean; override; -> Not implemented
//    function SupportsAlterTableWithAddColumn: Boolean; override; -> Not implemented
//    function SupportsAlterTableWithDropColumn: Boolean; override; -> Not implemented
//    function SupportsColumnAliasing: Boolean; override; -> Not implemented
//    function SupportsConvert: Boolean; override; -> Not implemented
//    function SupportsConvertForTypes(FromType: TZSQLType; ToType: TZSQLType):
//      Boolean; override; -> Not implemented
//    function SupportsTableCorrelationNames: Boolean; override; -> Not implemented
//    function SupportsDifferentTableCorrelationNames: Boolean; override; -> Not implemented
//    function SupportsExpressionsInOrderBy: Boolean; override; -> Not implemented
    function SupportsOrderByUnrelated: Boolean; override;
//    function SupportsGroupBy: Boolean; override; -> Not implemented
    function SupportsGroupByUnrelated: Boolean; override;
    function SupportsGroupByBeyondSelect: Boolean; override;
//    function SupportsLikeEscapeClause: Boolean; override; -> Not implemented
    function SupportsMultipleResultSets: Boolean; override;
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
//    function SupportsSchemasInDataManipulation: Boolean; override; -> Not implemented
//    function SupportsSchemasInProcedureCalls: Boolean; override; -> Not implemented
//    function SupportsSchemasInTableDefinitions: Boolean; override; -> Not implemented
//    function SupportsSchemasInIndexDefinitions: Boolean; override; -> Not implemented
//    function SupportsSchemasInPrivilegeDefinitions: Boolean; override; -> Not implemented
    function SupportsCatalogsInDataManipulation: Boolean; override;
//    function SupportsCatalogsInProcedureCalls: Boolean; override; -> Not implemented
    function SupportsCatalogsInTableDefinitions: Boolean; override;
//    function SupportsCatalogsInIndexDefinitions: Boolean; override; -> Not implemented
//    function SupportsCatalogsInPrivilegeDefinitions: Boolean; override; -> Not implemented
//    function SupportsPositionedDelete: Boolean; override; -> Not implemented
//    function SupportsPositionedUpdate: Boolean; override; -> Not implemented
//    function SupportsSelectForUpdate: Boolean; override; -> Not implemented
//    function SupportsStoredProcedures: Boolean; override; -> Not implemented
    function SupportsSubqueriesInComparisons: Boolean; override;
//    function SupportsSubqueriesInExists: Boolean; override; -> Not implemented
//    function SupportsSubqueriesInIns: Boolean; override; -> Not implemented
//    function SupportsSubqueriesInQuantifieds: Boolean; override; -> Not implemented
//    function SupportsCorrelatedSubqueries: Boolean; override; -> Not implemented
//    function SupportsUnion: Boolean; override; -> Not implemented
    function SupportsUnionAll: Boolean; override;
//    function SupportsOpenCursorsAcrossCommit: Boolean; override; -> Not implemented
//    function SupportsOpenCursorsAcrossRollback: Boolean; override; -> Not implemented
    function SupportsOpenStatementsAcrossCommit: Boolean; override;
    function SupportsOpenStatementsAcrossRollback: Boolean; override;
//    function SupportsTransactions: Boolean; override; -> Not implemented
    function SupportsTransactionIsolationLevel(const Level: TZTransactIsolationLevel): Boolean; override;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; override;
    function SupportsDataManipulationTransactionsOnly: Boolean; override;
//    function SupportsResultSetType(_Type: TZResultSetType): Boolean; override; -> Not implemented
//    function SupportsResultSetConcurrency(_Type: TZResultSetType;
//      Concurrency: TZResultSetConcurrency): Boolean; override; -> Not implemented
//    function SupportsBatchUpdates: Boolean; override; -> Not implemented
    function SupportsMilliSeconds: Boolean; override;

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
//    function GetMaxSchemaNameLength: Integer; override; -> Not implemented
//    function GetMaxProcedureNameLength: Integer; override; -> Not implemented
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
//    function StoresUpperCaseIdentifiers: Boolean; override; -> Not implemented
    function StoresLowerCaseIdentifiers: Boolean; override;
    function StoresMixedCaseIdentifiers: Boolean; override;
//    function StoresUpperCaseQuotedIdentifiers: Boolean; override; -> Not implemented
//    function StoresLowerCaseQuotedIdentifiers: Boolean; override; -> Not implemented
//    function StoresMixedCaseQuotedIdentifiers: Boolean; override; -> Not implemented
    function GetDefaultTransactionIsolation: TZTransactIsolationLevel; override;
//    function DataDefinitionCausesTransactionCommit: Boolean; override; -> Not implemented
//    function DataDefinitionIgnoredInTransactions: Boolean; override; -> Not implemented

    // interface details (terms, keywords, etc):
    function GetSchemaTerm: string; override;
    function GetProcedureTerm: string; override;
    function GetCatalogTerm: string; override;
//    function GetCatalogSeparator: string; override; -> Not implemented
    function GetSQLKeywords: string; override;
    function GetNumericFunctions: string; override;
    function GetStringFunctions: string; override;
    function GetSystemFunctions: string; override;
    function GetTimeDateFunctions: string; override;
    function GetSearchStringEscape: string; override;
    function GetExtraNameCharacters: string; override;
  end;

  IZMySQLDatabaseMetadata = interface(IZDatabaseMetadata)
    ['{204A7ABF-36B2-4753-9F48-4942619C31FA}']
    procedure SetMySQL_FieldType_Bit_1_IsBoolean(Value: Boolean);
    procedure SetDataBaseName(const Value: String);
  end;
  {** Implements MySQL Database Metadata. }
  TZMySQLDatabaseMetadata = class(TZAbstractDatabaseMetadata, IZMySQLDatabaseMetadata)
  private
    FInfo: TStrings;
    FMySQL_FieldType_Bit_1_IsBoolean: Boolean;
    FBoolCachedResultSets: IZCollection;
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-26

    procedure GetCatalogAndNamePattern(const Catalog, SchemaPattern,
      NamePattern: string; out OutCatalog, OutNamePattern: string);
    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
//    function UncachedGetSchemas: IZResultSet; override; -> Not implemented
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
//     function UncachedGetSequences(const Catalog: string; const SchemaPattern: string;
//      const SequenceNamePattern: string): IZResultSet; override; -> Not implemented
    function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet; override;
    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string):
      IZResultSet; override;
    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetTypeInfo: IZResultSet; override;
//    function UncachedGetUDTs(const Catalog: string; const SchemaPattern: string;
//      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet; override;
    function UncachedGetCollationAndCharSet(const Catalog, SchemaPattern,
      TableNamePattern, ColumnNamePattern: string): IZResultSet; override; //EgonHugeist
    function UncachedGetCharacterSets: IZResultSet; override; //EgonHugeist
  public
    constructor Create(Connection: TZAbstractConnection; const Url: TZURL); override;
    destructor Destroy; override;
  public
    procedure SetMySQL_FieldType_Bit_1_IsBoolean(Value: Boolean);
    procedure SetDataBaseName(const Value: String);
    procedure ClearCache; override;
  end;

implementation

uses
  Math,
  ZFastCode, ZMessages, ZDbcMySqlUtils, ZDbcUtils, ZDbcMySql, ZCollections,
  ZSelectSchema;

{ TZMySQLDatabaseInfo }

{**
  Constructs this object.
  @param Metadata the interface of the correpsonding database metadata object
}
constructor TZMySQLDatabaseInfo.Create(const Metadata: TZAbstractDatabaseMetadata);
begin
  inherited Create(MetaData, '`');
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What's the name of this database product?
  @return database product name
}
function TZMySQLDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := 'MySQL';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZMySQLDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '3+';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZMySQLDatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for MySQL';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZMySQLDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZMySQLDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 1;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZMySQLDatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := True; //https://dev.mysql.com/doc/refman/5.7/en/identifier-case-sensitivity.html
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := False; //https://dev.mysql.com/doc/refman/5.7/en/identifier-case-sensitivity.html
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZMySQLDatabaseInfo.GetSQLKeywords: string;
begin
  Result := 'AUTO_INCREMENT,BINARY,BLOB,ENUM,INFILE,LOAD,MEDIUMINT,OPTION,'
    + 'OUTFILE,REPLACE,SET,TEXT,UNSIGNED,ZEROFILL';
  { mdaems : added all reserved words indicated by mysql documentation (up to mysql 5.1)}
  Result := Result + 'ACCESSIBLE,ADD,ALL,ANALYZE,AND,ASC,ASENSITIVE,'
    + ' BEFORE,BETWEEN,BIGINT,BOTH,CALL,CASCADE,CASE,CHANGE,CHARACTER,CHECK,'
    + 'COLLATE,CONDITION,CONSTRAINT,CONTINUE,CONVERT,CROSS,CURSOR,'
    + 'DATABASE,DATABASES,DAY_HOUR,DAY_MICROSECOND,DAY_MINUTE,DAY_SECOND,'
    + 'DEC,DECIMAL,DECLARE,DEFAULT,DELAYED,DESC,DESCRIBE,DETERMINISTIC,'
    + 'DISTINCT,DISTINCTROW,DIV,DOUBLE,DUAL,EACH,ELSE,ELSEIF,ENCLOSED,'
    + 'ESCAPED,EXISTS,EXIT,EXPLAIN,FALSE,FETCH,FLOAT,FLOAT4,FLOAT8,'
    + 'FORCE,FOREIGN,FULLTEXT,GENERAL,GRANT,HIGH_PRIORITY,HOUR_MICROSECOND,'
    + 'HOUR_MINUTE,HOUR_SECOND,IF,IGNORE,IGNORE_SERVER_IDS,IN,INNER,INOUT,INSENSITIVE,INT,'
    + 'INT1,INT2,INT3,INT4,INT8,INTERVAL,ITERATE,JOIN,KEYS,KILL,LEADING,'
    + 'LEAVE,LEFT,LIKE,LIMIT,LINEAR,LINES,LOCK,'
    + 'LONG,LONGBLOB,LONGTEXT,LOOP,LOW_PRIORITY,MASTER_HEARTBEAT_PERIOD,MASTER_SSL_VERIFY_SERVER_CERT,'
    + 'MATCH,MAXVALUE,MEDIUMBLOB,MEDIUMTEXT,MIDDLEINT,MINUTE_MICROSECOND,MINUTE_SECOND,'
    + 'MOD,MODIFIES,NATURAL,NOT,NO_WRITE_TO_BINLOG,NUMERIC,OPTIMIZE,'
    + 'OPTIONALLY,OR,OUT,OUTER,PRECISION,PROCEDURE,PURGE,RANGE,READ,READS,'
    + 'READ_ONLY,READ_WRITE,REAL,REFERENCES,REGEXP,RELEASE,RENAME,REPEAT,'
    + 'REQUIRE,RESIGNAL SIGNAL,RESTRICT,RETURN,REVOKE,RIGHT,RLIKE,SCHEMA,SCHEMAS,'
    + 'SECOND_MICROSECOND,SENSITIVE,SEPARATOR,SHOW,SLOW,SMALLINT,SPATIAL,'
    + 'SPECIFIC,SQL,SQLEXCEPTION,SQLSTATE,SQLWARNING,SQL_BIG_RESULT,'
    + 'SQL_CALC_FOUND_ROWS,SQL_SMALL_RESULT,SSL,STARTING,STRAIGHT_JOIN,'
    + 'TERMINATED,THEN,TINYBLOB,TINYINT,TINYTEXT,TO,TRAILING,TRIGGER,'
    + 'TRUE,UNDO,UNION,UNIQUE,UNLOCK,USAGE,USE,USING,UTC_DATE,UTC_TIME,'
    + 'UTC_TIMESTAMP,VARBINARY,VARCHARACTER,VARYING,WHEN,WHILE,WITH,'
    + 'WRITE,X509,XOR,YEAR_MONTH,ACCESSIBLE,LINEAR,'
    + 'MASTER_SSL_VERIFY_SERVER_CERT,RANGE,READ_ONLY,READ_WRITE';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZMySQLDatabaseInfo.GetNumericFunctions: string;
begin
  Result := 'ABS,ACOS,ASIN,ATAN,ATAN2,BIT_COUNT,CEILING,COS,COT,DEGREES,EXP,'
    + 'FLOOR,LOG,LOG10,MAX,MIN,MOD,PI,POW,POWER,RADIANS,RAND,ROUND,SIN,SQRT,'
    + 'TAN,TRUNCATE';
  { mdaems : added all numeric functions indicated by mysql documentation (up to mysql 5.1)}
  Result := Result + 'BIT_COUNT,CEIL,CRC32,LN,LOG2,SIGN,UUID';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZMySQLDatabaseInfo.GetStringFunctions: string;
begin
  Result := 'ASCII,CHAR,CHAR_LENGTH,CHARACTER_LENGTH,CONCAT,ELT,FIELD,'
    + 'FIND_IN_SET,INSTR,INTERVAL,LCASE,LEFT,LENGTH,LOCATE,LOWER,LTRIM,'
    + 'MID,POSITION,OCTET_LENGTH,REPEAT,REPLACE,REVERSE,RIGHT,RTRIM,SPACE,'
    + 'SOUNDEX,SUBSTRING,SUBSTRING_INDEX,TRIM,UCASE,UPPER';
  { mdaems : added all string functions indicated by mysql documentation (up to mysql 5.1)}
  Result := Result + 'AES_DECRYPT,AES_ENCRYPT,BIN,BIT_LENGTH,CHARSET,'
    + 'COERCIBILITY,COLLATION,COMPRESS,CONCAT_WS,DECODE,DES_DECRYPT,DES_ENCRYPT,'
    + 'ENCODE,ENCRYPT,EXPORT_SET,FORMAT,HEX,LOAD_FILE,LPAD,MAKE_SET,MD5,OCT,ORD,'
    + 'QUOTE,RPAD,STRCMP,SHA,SHA1,SUBSTR,UNHEX,EXTRACTVALUE,UPDATEXML,'
    + 'UNCOMPRESS,UNCOMPRESSED_LENGTH';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZMySQLDatabaseInfo.GetSystemFunctions: string;
begin
  Result := 'USER,SYSTEM_USER,SESSION_USER,PASSWORD,'
    + 'LAST_INSERT_ID,VERSION';
  { mdaems : added all system functions indicated by mysql documentation (up to mysql 5.1)}
  Result := Result + 'BENCHMARK,CONNECTION_ID,CURRENT_USER,DEFAULT,FOUND_ROWS,'
    + 'GET_LOCK,INET_ATON,INET_NTOA,IS_FREE_LOCK,IS_USED_LOCK,MASTER_POS_WAIT,'
    + 'NAME_CONST,OLD_PASSWORD,RELEASE_LOCK,ROW_COUNT,SCHEMA,SLEEP';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZMySQLDatabaseInfo.GetTimeDateFunctions: string;
begin
  Result := 'DAYOFWEEK,WEEKDAY,DAYOFMONTH,DAYOFYEAR,MONTH,DAYNAME,MONTHNAME,'
    + 'QUARTER,WEEK,YEAR,HOUR,MINUTE,SECOND,PERIOD_ADD,PERIOD_DIFF,TO_DAYS,'
    + 'FROM_DAYS,DATE_FORMAT,TIME_FORMAT,CURDATE,CURRENT_DATE,CURTIME,'
    + 'CURRENT_TIME,NOW,SYSDATE,CURRENT_TIMESTAMP,UNIX_TIMESTAMP,FROM_UNIXTIME,'
    + 'SEC_TO_TIME,TIME_TO_SEC';
  { mdaems : added all time and date functions indicated by mysql documentation (up to mysql 5.1)}
  Result := Result + 'ADDDATE,ADDTIME,CONVERT_TZ,DATE_ADD,'
    + 'DATE_SUB,DATE,DATEDIFF,GET_FORMAT,LAST_DAY,LOCALTIME,'
    + 'LOCALTIMESTAMP,MAKEDATE,MAKETIME,MICROSECOND,STR_TO_DATE,SUBDATE,SUBTIME,'
    + 'TIMEDIFF,TIMESTAMP,TIMESTAMPADD,TIMESTAMPDIFF,UTC_DATE,UTC_TIME,'
    + 'UTC_TIMESTAMP,WEEKOFYEAR,YEARWEEK';
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
function TZMySQLDatabaseInfo.GetSearchStringEscape: string;
begin
  Result := '\';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZMySQLDatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsOrderByUnrelated: Boolean;
var
  MajorVersion: Integer;
  MinorVersion: Integer;
begin
  GetVersion(MajorVersion{%H-}, MinorVersion{%H-});
  // changed from False by mdaems. After testing with lower versions, please correct.
  Result := MajorVersion >= 5;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := False;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := True;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := False;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZMySQLDatabaseInfo.GetSchemaTerm: string;
begin
  Result := '';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZMySQLDatabaseInfo.GetProcedureTerm: string;
begin
  Result := '';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZMySQLDatabaseInfo.GetCatalogTerm: string;
begin
  Result := 'Database';
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
var
  MajorVersion: Integer;
  MinorVersion: Integer;
begin
  GetVersion(MajorVersion{%H-}, MinorVersion{%H-});
  Result := ((MajorVersion = 3) and (MinorVersion >= 22)) or (MajorVersion > 3);
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZMySQLDatabaseInfo.SupportsTransactionIsolationLevel(
  const Level: TZTransactIsolationLevel): Boolean;
begin
  Result := Level in [tiReadUncommitted, tiReadCommitted,
    tiRepeatableRead, tiSerializable]
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsUnionAll: Boolean;
var
  MajorVersion: Integer;
  MinorVersion: Integer;
begin
  GetVersion(MajorVersion{%H-}, MinorVersion{%H-});
  Result := MajorVersion >= 4;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZMySQLDatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZMySQLDatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
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
function TZMySQLDatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 16777208;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 16777208;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := 64;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 256;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 512;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxCursorNameLength: Integer;
begin
  Result := 64;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := 32;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxRowSize: Integer;
begin
  Result := 2147483639;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := True;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxStatementLength: Integer;
begin
  Result := 65531;
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
    a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
    a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := 64;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 256;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
    a result of zero means that there is no limit or the limit is not known
}
function TZMySQLDatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := 16;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZMySQLDatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  //https://dev.mysql.com/doc/refman/5.7/en/innodb-transaction-isolation-levels.html
  Result := tiRepeatableRead;
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsDataManipulationTransactionsOnly: Boolean;
begin
  case Metadata.GetConnection.GetTransactionIsolation of
    tiReadUncommitted: Result := True;
    tiReadCommitted: Result := True;
    tiRepeatableRead: Result := True;
    tiSerializable: Result := True;
  else
    Result := False;
  end;
end;

function TZMySQLDatabaseInfo.SupportsMilliSeconds: Boolean;
begin
  Result := False;
end;

{**
  Are multiple <code>ResultSet</code> from a single execute supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLDatabaseInfo.SupportsMultipleResultSets: Boolean;
begin
  Result := True;
end;

{**
  Gets the MySQL version info.
  @param MajorVesion the major version of MySQL server.
  @param MinorVersion the minor version of MySQL server.
}
procedure TZMySQLDatabaseInfo.GetVersion(var MajorVersion,
  MinorVersion: Integer);
var
  VersionList: TStrings;
  Subversion : integer;
begin
  DecodeSqlVersioning(Metadata.GetConnection.GetHostVersion,
    MajorVersion,MinorVersion, Subversion);
  if (Majorversion < 4) or ((MajorVersion=4) and (MinorVersion = 0)) then
   with Metadata.GetConnection.CreateStatement.ExecuteQuery('SELECT VERSION()') do
    begin
      VersionList := SplitString(String(GetString(FirstDbcIndex)), '.-');
      try
        if VersionList.Count >= 2 then
        begin
          MajorVersion := StrToIntDef(VersionList.Strings[0], 0);
          MinorVersion := StrToIntDef(VersionList.Strings[1], 0);
        end;
      finally
        VersionList.Free;
      end;
      Close;
    end;
end;

{ TZMySQLDatabaseMetadata }

procedure TZMySQLDatabaseMetadata.ClearCache;
begin
  FBoolCachedResultSets.Clear;
  inherited ClearCache;
end;

constructor TZMySQLDatabaseMetadata.Create(Connection: TZAbstractConnection;
  const Url: TZURL);
begin
  inherited Create(Connection, Url);
  FInfo := TStringList.Create;
  FInfo.Assign(Url.Properties);
  FInfo.Values['UseResult'] := 'True';
  FBoolCachedResultSets := TZCollection.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZMySQLDatabaseMetadata.Destroy;
begin
  FreeAndNil(FInfo);
  inherited Destroy;
end;

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZMySQLDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZMySQLDatabaseInfo.Create(Self);
end;

procedure TZMySQLDatabaseMetadata.GetCatalogAndNamePattern(const Catalog,
  SchemaPattern, NamePattern: string; out OutCatalog, OutNamePattern: string);
begin
  if Catalog = '' then
  begin
    if SchemaPattern <> '' then
      OutCatalog := NormalizePatternCase(SchemaPattern)
    else
      OutCatalog := NormalizePatternCase(FDatabase);
  end
  else
    OutCatalog := NormalizePatternCase(Catalog);

  if NamePattern = '' then
    OutNamePattern := '%'
  else
    OutNamePattern := NormalizePatternCase(NamePattern);
end;

procedure TZMySQLDatabaseMetadata.SetDataBaseName(const Value: String);
begin
  FDatabase := Value;
end;

procedure TZMySQLDatabaseMetadata.SetMySQL_FieldType_Bit_1_IsBoolean(Value: Boolean);
var I, Idx: Integer;
begin
  if Value <> FMySQL_FieldType_Bit_1_IsBoolean then begin
    FMySQL_FieldType_Bit_1_IsBoolean := Value;
    for i := FBoolCachedResultSets.Count -1 downto 0 do begin
      Idx := CachedResultSets.Values.IndexOf(FBoolCachedResultSets[i]);
      if Idx > -1 then
        CachedResultSets.Remove(CachedResultSets.Keys[idx]);
      FBoolCachedResultSets.Delete(i);
    end;
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
 			"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
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
function TZMySQLDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  Len: NativeUInt;
  LCatalog, LTableNamePattern: string;
begin
    Result := inherited UncachedGetTables(Catalog, SchemaPattern, TableNamePattern, Types);

    GetCatalogAndNamePattern(Catalog, SchemaPattern, TableNamePattern,
      LCatalog, LTableNamePattern);

    with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(
      Format('SHOW TABLES FROM %s LIKE ''%s''',
      [IC.Quote(LCatalog), LTableNamePattern])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(CatalogNameIndex, LCatalog);
        Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(FirstDbcIndex, Len), @Len);
        Result.UpdateString(TableColumnsSQLType, 'TABLE');
        Result.InsertRow;
      end;
      Close;
    end;

    // If a table was specified but not found, check if it could be a temporary table
    if not Result.First and (LTableNamePattern <> '%') then
    begin
      try
        EnterSilentMySQLError;
        try
          if GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(
            Format('SHOW COLUMNS FROM %s.%s',
            [IC.Quote(LCatalog),
             IC.Quote(LTableNamePattern)])).Next then
          begin
            Result.MoveToInsertRow;
            Result.UpdateString(CatalogNameIndex, LCatalog);
            Result.UpdateString(TableNameIndex, LTableNamePattern);
            Result.UpdateString(TableColumnsSQLType, 'TABLE');
            Result.InsertRow;
          end;
        finally
          LeaveSilentMySQLError;
        end;
      except
        on EZMySQLSilentException do ;
        on EZSQLException do ;
      end;
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
function TZMySQLDatabaseMetadata.UncachedGetCatalogs: IZResultSet;
var
  Len: NativeUInt;
begin
    Result:=inherited UncachedGetCatalogs;

    with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery('SHOW DATABASES') do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(FirstDbcIndex, Len), @Len);
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
 			"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 			"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  table type
}
function TZMySQLDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
begin
    Result:=inherited UncachedGetTableTypes;

    Result.MoveToInsertRow;
    Result.UpdateString(TableTypeColumnTableTypeIndex, 'TABLE');
    Result.InsertRow;
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
 	<LI><B>ORDINAL_POSITION</B> int	=> index of column in table
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
function TZMySQLDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  Len: NativeUInt;
  I: Integer;
  MySQLType: TZSQLType;
  TempCatalog, TempColumnNamePattern, TempTableNamePattern: string;

  TypeName, TypeInfoSecond, DefaultValue: RawByteString;
  Nullable: String;
  HasDefaultValue, AddToBoolCache: Boolean;
  ColumnSize, ColumnDecimals: Integer;
  OrdPosition: Integer;

  TableNameList: TStrings;
  TableNameLength: Integer;
  ColumnIndexes : Array[1..6] of integer;
begin
    Result := inherited UncachedGetColumns(Catalog, SchemaPattern,
      TableNamePattern, ColumnNamePattern);

    GetCatalogAndNamePattern(Catalog, SchemaPattern, ColumnNamePattern,
      TempCatalog, TempColumnNamePattern);

    TableNameLength := 0;
    TableNameList := TStringList.Create;
    AddToBoolCache := False;
    try
      with GetTables(Catalog, SchemaPattern, TableNamePattern, nil) do begin
        while Next do begin
          TableNameList.Add(GetString(TableNameIndex)); //TABLE_NAME
          TableNameLength := Max(TableNameLength, Length(TableNameList[TableNameList.Count - 1]));
        end;
        Close;
      end;

      for I := 0 to TableNameList.Count - 1 do
      begin
        OrdPosition := 1;
        TempTableNamePattern := TableNameList.Strings[I];

        with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(
          Format('SHOW FULL COLUMNS FROM %s.%s LIKE ''%s''',
          [IC.Quote(TempCatalog),
          IC.Quote(TempTableNamePattern),
          TempColumnNamePattern])) do
        begin
          ColumnIndexes[1] := FindColumn('Field');
          ColumnIndexes[2] := FindColumn('Type');
          ColumnIndexes[3] := FindColumn('Null');
          ColumnIndexes[4] := FindColumn('Extra');
          ColumnIndexes[5] := FindColumn('Default');
          ColumnIndexes[6] := FindColumn('Collation');
          while Next do
          begin
            {initialise some variables}
            Result.MoveToInsertRow;
            Result.UpdateString(CatalogNameIndex, TempCatalog);
            Result.UpdateString(SchemaNameIndex, '');
            Result.UpdateString(TableNameIndex, TempTableNamePattern) ;
            Result.UpdatePAnsiChar(ColumnNameIndex, GetPAnsiChar(ColumnIndexes[1], Len), @Len);

            TypeName := GetRawByteString(ColumnIndexes[2]);
            ConvertMySQLColumnInfoFromString(TypeName, ConSettings,
              TypeInfoSecond, MySQLType, ColumnSize, ColumnDecimals, fMySQL_FieldType_Bit_1_IsBoolean);
            if TypeName = 'enum'
            then AddToBoolCache := AddToBoolCache or ((TypeInfoSecond = '''Y'''#0'''N''') or (TypeInfoSecond = '''N'''#0'''Y'''))
            else if TypeName = 'bit'
            then AddToBoolCache := AddToBoolCache or (TypeInfoSecond = '1');

            Result.UpdateInt(TableColColumnTypeIndex, Ord(MySQLType));
            Result.UpdateRawByteString(TableColColumnTypeNameIndex, TypeName);
            Result.UpdateInt(TableColColumnSizeIndex, ColumnSize);

            Result.UpdateInt(TableColColumnDecimalDigitsIndex, ColumnDecimals);
            Result.UpdateNull(TableColColumnNumPrecRadixIndex);

            { Sets nullable fields. }
            Nullable := GetString(ColumnIndexes[3]);
            if Nullable <> '' then
              if Nullable = 'YES' then
              begin
                Result.UpdateInt(TableColColumnNullableIndex, Ord(ntNullable));
                Result.UpdateString(TableColColumnIsNullableIndex, 'YES');
              end
              else
              begin
                Result.UpdateInt(TableColColumnNullableIndex, Ord(ntNoNulls));
                Result.UpdateString(TableColColumnIsNullableIndex, 'NO');
              end
            else
            begin
              Result.UpdateInt(TableColColumnNullableIndex, 0);
              Result.UpdateString(TableColColumnIsNullableIndex, 'NO');
            end;
            Result.UpdatePAnsiChar(TableColColumnRemarksIndex, GetPAnsiChar(ColumnIndexes[4], Len), @Len);
            // MySQL is a bit bizarre.
            if IsNull(ColumnIndexes[5]) then
            begin
              // MySQL bizarity 1:
              // NULL actually means that the default is NULL.
              // Superfluous, since there's a NULL / NOT NULL flag to control whether the field may have no value.
              // So we just ignore this, the field gets set to NULL if nothing was specified...
              HasDefaultValue := false;
              DefaultValue := '';
            end else begin
              DefaultValue := GetRawByteString(ColumnIndexes[5]);
              if not (DefaultValue = '') then
                 HasDefaultValue := true
              else begin
                // MySQL bizarity 2:
                // For CHAR, BLOB, TEXT and SET types, '' either means: default value is '' or: no default value
                // There's absolutely no way of telling when using SHOW COLUMNS FROM,
                // the correct information can /only/ be discerned by using information_schema.
                // TODO: For now, just use '' as default value for these types, but this should really be fixed to use information_schema.
                // For ENUM types, '' means: default value is first value in enum set
                // For other types, '' means: no default value
                HasDefaultValue := false;
                if MySQLType in [stAsciiStream, stUnicodeStream, stBinaryStream] then HasDefaultValue := true;
                if EndsWith(TypeName, RawByteString('char')) then HasDefaultValue := true;
                if 'set' = TypeName then HasDefaultValue := true;
                if 'enum' = TypeName then begin
                  HasDefaultValue := true;
                  DefaultValue := Copy(TypeInfoSecond, 2,length(TypeInfoSecond)-1);
                  DefaultValue := Copy(DefaultValue, 1, ZFastCode.Pos({$IFDEF UNICODE}RawByteString{$ENDIF}(''''), DefaultValue) - 1);
                end;
              end;
            end;
            if HasDefaultValue then
            begin
              // String values in the 'Default value' field are not escaped with apostrophes.
              // Guess this makes it impossible to specify a function call or similar via default values.
              if (MySQLType in [stString, stUnicodeString, stBinaryStream, stAsciiStream]) then
              begin
                // Since we changed date/time-related columntypes to be presented
                // as strings, we need to move the CURRENT_TIMESTAMP-check to here.
                // Also left the other line in order to minimize the changes in ZeosLib
                if DefaultValue <> 'CURRENT_TIMESTAMP' then
                DefaultValue := '''' + DefaultValue + ''''
              end
              else if (MySQLType in [stDate, stTime, stTimestamp]) then
              begin
                if DefaultValue <> 'CURRENT_TIMESTAMP' then
                  DefaultValue := '''' + DefaultValue + ''''
              end
              else if (MySQLType = stBoolean) and (TypeName = 'enum') then
              begin
                DefaultValue := BoolStrIntsRaw[ (DefaultValue = 'y') or (DefaultValue = 'Y') ];
              end;
              Result.UpdateRawByteString(TableColColumnColDefIndex, DefaultValue);
            end;
            if MySQLType = stString then begin
              Result.UpdateInt(TableColColumnBufLengthIndex, ColumnSize * ConSettings^.ClientCodePage^.CharWidth +1);
              Result.UpdateInt(TableColColumnCharOctetLengthIndex, ColumnSize * ConSettings^.ClientCodePage^.CharWidth);
            end else if MySQLType = stUnicodeString then begin
              Result.UpdateInt(TableColColumnBufLengthIndex, (ColumnSize+1) shl 1);
              Result.UpdateInt(TableColColumnCharOctetLengthIndex, ColumnSize shl 1);
            end else if MySQLType in [stBytes, stAsciiStream, stUnicodeStream, stBinaryStream] then
              Result.UpdateInt(TableColColumnBufLengthIndex, ColumnSize)
            else
              Result.UpdateInt(TableColColumnBufLengthIndex, ZSQLTypeToBuffSize(MySQLType));
            //Result.UpdateNull(TableColColumnSQLDataTypeIndex);
            //Result.UpdateNull(TableColColumnSQLDateTimeSubIndex);
            Result.UpdateInt(TableColColumnOrdPosIndex, OrdPosition);

            Result.UpdateBoolean(TableColColumnAutoIncIndex, //AUTO_INCREMENT
              Trim(LowerCase(GetString(ColumnIndexes[4]))) = 'auto_increment'); //Extra
            Result.UpdateBoolean(TableColColumnCaseSensitiveIndex, //CASE_SENSITIVE
              IC.IsCaseSensitive(GetString(ColumnIndexes[1])));//Field
            Result.UpdateBoolean(TableColColumnSearchableIndex, True);  //SEARCHABLE
            Result.UpdateBoolean(TableColColumnWritableIndex, True);  //WRITABLE
            Result.UpdateBoolean(TableColColumnDefinitelyWritableIndex, True);  //DEFINITELYWRITABLE
            Result.UpdateBoolean(TableColColumnReadonlyIndex, False); //READONLY

            Inc(OrdPosition);
            Result.InsertRow;
          end;
          Close;
        end;
      end;
      if AddToBoolCache then
        FBoolCachedResultSets.Add(Result);
    finally
      TableNameList.Free;
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
function TZMySQLDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
const
  host_Index            = FirstDbcIndex + 0;
  db_Index              = FirstDbcIndex + 1;
  grantor_Index         = FirstDbcIndex + 2;
  user_Index            = FirstDbcIndex + 3;
  {%H-}table_name_Index = FirstDbcIndex + 4;
  column_name_Index     = FirstDbcIndex + 5;
  column_priv_Index     = FirstDbcIndex + 6;
var
  Len: NativeUInt;
  I: Integer;
  Host, User, FullUser: String;
  AllPrivileges, Privilege: String;
  PrivilegesList: TStrings;
  ColumnNameCondition, TableNameCondition, SchemaCondition: string;
begin
  Result:=inherited UncachedGetColumnPrivileges(Catalog, Schema, Table, ColumnNamePattern);

    If Catalog = '' then
      If Schema <> '' then
      SchemaCondition := ConstructNameCondition(Schema,'c.db')
      else
      SchemaCondition := ConstructNameCondition(FDatabase,'c.db')
    else
      SchemaCondition := ConstructNameCondition(Catalog,'c.db');
    TableNameCondition := ConstructNameCondition(Table,'c.table_name');
    ColumnNameCondition := ConstructNameCondition(ColumnNamePattern,'c.column_name');
    If SchemaCondition <> '' then
      SchemaCondition := ' and ' + SchemaCondition;
    If TableNameCondition <> '' then
      TableNameCondition := ' and ' + TableNameCondition;
    If ColumnNameCondition <> '' then
      ColumnNameCondition := ' and ' + ColumnNameCondition;

    PrivilegesList := TStringList.Create;
    try
      with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(
        'SELECT c.host, c.db, t.grantor, c.user, c.table_name,'
        + ' c.column_name, c.column_priv FROM mysql.columns_priv c,'
        + ' mysql.tables_priv t WHERE c.host=t.host AND c.db=t.db'
        + ' AND c.table_name=t.table_name'
        + SchemaCondition + TableNameCondition + ColumnNameCondition
      ) do
      begin
        while Next do
        begin
          Host := GetString(host_Index);
          User := GetString(user_Index);
          if User = '' then
            User := '%';
          if Host <> '' then
            FullUser := User + '@' + Host;

          AllPrivileges := GetString(column_priv_Index);
          PutSplitString(PrivilegesList, AllPrivileges, ',');

          for I := 0 to PrivilegesList.Count - 1 do
          begin
            Result.MoveToInsertRow;
            Privilege := Trim(PrivilegesList.Strings[I]);
            Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(db_Index, Len), @Len);
            //Result.UpdateNull(SchemaNameIndex);
            Result.UpdateString(TableNameIndex, Table);
            Result.UpdatePAnsiChar(ColumnNameIndex, GetPAnsiChar(column_name_Index, Len), @Len);
            Result.UpdatePAnsiChar(TableColPrivGrantorIndex, GetPAnsiChar(grantor_Index, Len), @Len);
            Result.UpdateString(TableColPrivGranteeIndex, FullUser);
            Result.UpdateString(TableColPrivPrivilegeIndex, Privilege);
            //Result.UpdateNull(TableColPrivIsGrantableIndex);
            Result.InsertRow;
          end;
        end;
        Close;
      end;
    finally
      PrivilegesList.Free;
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
function TZMySQLDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
const
  host_Index        = FirstDbcIndex + 0;
  db_Index          = FirstDbcIndex + 1;
  table_name_Index  = FirstDbcIndex + 2;
  grantor_Index     = FirstDbcIndex + 3;
  user_Index        = FirstDbcIndex + 4;
  column_priv_Index = FirstDbcIndex + 5;
var
  I: Integer;
  Len: NativeUInt;
  Host, User, FullUser: String;
  AllPrivileges, Privilege: String;
  PrivilegesList: TStrings;
  TableNameCondition, SchemaCondition: string;
begin
    Result:=inherited UncachedGetTablePrivileges(Catalog, SchemaPattern, TableNamePattern);

    If Catalog = '' then
      If SchemaPattern <> '' then
      SchemaCondition := ConstructNameCondition(SchemaPattern,'db')
      else
      SchemaCondition := ConstructNameCondition(FDatabase,'db')
    else
      SchemaCondition := ConstructNameCondition(Catalog,'db');
    TableNameCondition := ConstructNameCondition(TableNamePattern,'table_name');
    If SchemaCondition <> '' then
      SchemaCondition := ' and ' + SchemaCondition;
    If TableNameCondition <> '' then
      TableNameCondition := ' and ' + TableNameCondition;

    PrivilegesList := TStringList.Create;
    try
      with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(
        'SELECT host,db,table_name,grantor,user,table_priv'
        + ' from mysql.tables_priv WHERE 1=1'
        + SchemaCondition + TableNameCondition
      ) do
      begin
        while Next do
        begin
          Host := GetString(host_Index);
          User := GetString(user_Index);
          if User = '' then
            User := '%';
          if Host <> '' then
            FullUser := User + '@' + Host;

          AllPrivileges := GetString(column_priv_Index);
          PutSplitString(PrivilegesList, AllPrivileges, ',');

          for I := 0 to PrivilegesList.Count - 1 do
          begin
            Result.MoveToInsertRow;
            Privilege := Trim(PrivilegesList.Strings[I]);
            Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(db_Index, Len), @Len);
            //Result.UpdateNull(SchemaNameIndex);
            Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(table_name_Index, Len), @Len);
            Result.UpdatePAnsiChar(TablePrivGrantorIndex, GetPAnsiChar(grantor_Index, Len), @Len);
            Result.UpdateString(TablePrivGranteeIndex, FullUser);
            Result.UpdateString(TablePrivPrivilegeIndex, Privilege);
            //Result.UpdateNull(TablePrivIsGrantableIndex);
            Result.InsertRow;
          end;
        end;
        Close;
      end;
    finally
      PrivilegesList.Free;
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
function TZMySQLDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  Len: NativeUInt;
  KeyType: string;
  LCatalog, LTable: string;
  ColumnIndexes : Array[1..3] of integer;
begin
    if Table = '' then
      raise Exception.Create(STableIsNotSpecified); //CHANGE IT!

    Result:=inherited UncachedGetPrimaryKeys(Catalog, Schema, Table);

    GetCatalogAndNamePattern(Catalog, Schema, Table,
      LCatalog, LTable);

    with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(
      Format('SHOW KEYS FROM %s.%s',
      [IC.Quote(LCatalog),
      IC.Quote(LTable)])) do
    begin
      ColumnIndexes[1] := FindColumn('Key_name');
      ColumnIndexes[2] := FindColumn('Column_name');
      ColumnIndexes[3] := FindColumn('Seq_in_index');
      while Next do
      begin
        KeyType := UpperCase(GetString(ColumnIndexes[1]));
        KeyType := Copy(KeyType, 1, 3);
        if KeyType = 'PRI' then
        begin
          Result.MoveToInsertRow;
          Result.UpdateString(CatalogNameIndex, LCatalog);
          Result.UpdateString(SchemaNameIndex, '');
          Result.UpdateString(TableNameIndex, Table);
          Result.UpdatePAnsiChar(PrimaryKeyColumnNameIndex, GetPAnsiChar(ColumnIndexes[2], Len), @Len);
          Result.UpdateInt(PrimaryKeyKeySeqIndex, GetInt(ColumnIndexes[3]));
          Result.UpdateNull(PrimaryKeyPKNameIndex);
          Result.InsertRow;
        end;
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
function TZMySQLDatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  I: Integer;
  KeySeq: Integer;
  LCatalog, LTable: string;
  TableType, Comment, Keys: String;
  CommentList, KeyList: TStrings;
  ColumnIndexes : Array[1..2] of integer;
begin
    if Table = '' then
      raise Exception.Create(STableIsNotSpecified); //CHANGE IT!

    Result := inherited UncachedGetImportedKeys(Catalog, Schema, Table);

    GetCatalogAndNamePattern(Catalog, Schema, Table,
      LCatalog, LTable);

    KeyList := TStringList.Create;
    CommentList := TStringList.Create;
    try
      with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(
        Format('SHOW TABLE STATUS FROM %s LIKE ''%s''',
        [IC.Quote(LCatalog), LTable])) do
      begin
        ColumnIndexes[1] := FindColumn('Type');
        ColumnIndexes[2] := FindColumn('Comment');
        while Next do
        begin
          TableType := GetString(ColumnIndexes[1]);
          if (TableType <> '') and (LowerCase(TableType) = 'innodb') then
          begin
            Comment := GetString(ColumnIndexes[2]);
            if Comment <> '' then
            begin
              PutSplitString(CommentList, Comment, ';');
              KeySeq := 0;

              if CommentList.Count > 4 then
                for I := 0 to CommentList.Count - 1 do
                begin
                  Keys := CommentList.Strings[1];
                  Result.MoveToInsertRow;
                  PutSplitString(KeyList, Keys, '() /');

                  Result.UpdateString(ImportedKeyColPKTableCatalogIndex, KeyList.Strings[2]);
                  //Result.UpdateNull(ImportedKeyColPKTableSchemaIndex);
                  Result.UpdateString(ImportedKeyColPKTableNameIndex, KeyList.Strings[3]);
                  Result.UpdateString(ImportedKeyColPKColumnNameIndex, KeyList.Strings[4]);
                  Result.UpdateString(ImportedKeyColFKTableCatalogIndex, LCatalog);
                  //Result.UpdateNull(ImportedKeyColFKTableSchemaIndex);
                  Result.UpdateString(ImportedKeyColFKTableNameIndex, Table);
                  Result.UpdateString(ImportedKeyColFKColumnNameIndex, KeyList.Strings[0]);

                  Result.UpdateInt(ImportedKeyColKeySeqIndex, KeySeq);
                  Result.UpdateInt(ImportedKeyColUpdateRuleIndex, Ord(ikSetDefault));
                  Result.UpdateInt(ImportedKeyColDeleteRuleIndex, Ord(ikSetDefault));
                  //Result.UpdateNull(ImportedKeyColFKNameIndex);
                  //Result.UpdateNull(ImportedKeyColPKNameIndex);
                  Result.UpdateInt(ImportedKeyColDeferrabilityIndex, Ord(ikSetDefault));
                  Inc(KeySeq);
                  Result.InsertRow;
                end;
            end;
          end;
        end;
        Close;
      end;
    finally
      KeyList.Free;
      CommentList.Free;
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
function TZMySQLDatabaseMetadata.UncachedGetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  I: Integer;
  Len: NativeUInt;
  KeySeq: Integer;
  LCatalog, LTable: string;
  TableType, Comment, Keys: String;
  CommentList, KeyList: TStrings;
  ColumnIndexes : Array[1..3] of integer;
begin
    if Table = '' then
      raise Exception.Create(STableIsNotSpecified); //CHANGE IT!

    Result:=inherited UncachedGetExportedKeys(Catalog, Schema, Table);

    GetCatalogAndNamePattern(Catalog, Schema, Table,
      LCatalog, LTable);

    KeyList := TStringList.Create;
    CommentList := TStringList.Create;
    try
      with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(
        Format('SHOW TABLE STATUS FROM %s',
        [IC.Quote(LCatalog)])) do
      begin
        ColumnIndexes[1] := FindColumn('Type');
        ColumnIndexes[2] := FindColumn('Comment');
        ColumnIndexes[3] := FindColumn('Name');
        while Next do
        begin
          TableType := GetString(ColumnIndexes[1]);
          if (TableType <> '') and (LowerCase(TableType) = 'innodb') then
          begin
            Comment := GetString(ColumnIndexes[2]);
            if Comment <> '' then
            begin
              PutSplitString(CommentList, Comment, ';');
              KeySeq := 0;
              if CommentList.Count > 4 then
              begin
                for I := 0 to CommentList.Count-1 do
                begin
                  Keys := CommentList.Strings[1];
                  Result.MoveToInsertRow;
                  PutSplitString(KeyList, Keys, '() /');

                  Result.UpdateString(ExportedKeyColPKTableCatalogIndex, KeyList.Strings[2]);
                  Result.UpdateString(ExportedKeyColPKTableNameIndex, Table);
                  Result.UpdateString(ExportedKeyColFKTableCatalogIndex, LCatalog);
                  Result.UpdatePAnsiChar(ExportedKeyColFKTableNameIndex, GetPAnsiChar(ColumnIndexes[3], Len), @Len);
                  Result.UpdateString(ExportedKeyColFKColumnNameIndex, KeyList.Strings[0]);
                  Result.UpdateInt(ExportedKeyColKeySeqIndex, KeySeq);
                  Result.UpdateInt(ExportedKeyColUpdateRuleIndex, Ord(ikSetDefault));
                  Result.UpdateInt(ExportedKeyColDeleteRuleIndex, Ord(ikSetDefault));
                  Result.UpdateInt(ExportedKeyColDeferrabilityIndex, Ord(ikSetDefault));
                  Inc(KeySeq);
                  Result.InsertRow;
                end;
              end;
            end;
          end;
        end;
        Close;
      end;
    finally
      KeyList.Free;
      CommentList.Free;
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
function TZMySQLDatabaseMetadata.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
var
  I: Integer;
  KeySeq: Integer;
  LForeignCatalog: string;
  TableType, Comment, Keys: string;
  CommentList, KeyList: TStrings;
  ColumnIndexes : Array[1..3] of integer;
begin
    if PrimaryTable = '' then
      raise Exception.Create(STableIsNotSpecified); //CHANGE IT!

    Result:=inherited UncachedGetCrossReference(PrimaryCatalog, PrimarySchema, PrimaryTable,
                                                ForeignCatalog, ForeignSchema, ForeignTable);

    if ForeignCatalog = '' then
      LForeignCatalog := FDatabase
    else
      LForeignCatalog := ForeignCatalog;

    KeyList := TStringList.Create;
    CommentList := TStringList.Create;
    try
      with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(
        Format('SHOW TABLE STATUS FROM %s',
        [IC.Quote(LForeignCatalog)])) do
      begin
        ColumnIndexes[1] := FindColumn('Type');
        ColumnIndexes[2] := FindColumn('Comment');
        ColumnIndexes[3] := FindColumn('Name');
        while Next do
        begin
          TableType := GetString(ColumnIndexes[1]);
          if (TableType <> '') and (LowerCase(TableType) = 'innodb') then
          begin
            Comment := GetString(ColumnIndexes[2]);
            if Comment = '' then
            begin
              PutSplitString(CommentList, Comment, ';');
              KeySeq := 0;
              if CommentList.Count > 4 then
              begin
                for I := 0 to CommentList.Count-1 do
                begin
                  Keys := CommentList.Strings[1];
                  Result.MoveToInsertRow;
                  PutSplitString(KeyList, Keys, '() /');

                  Result.UpdateString(CrossRefKeyColPKTableCatalogIndex, KeyList.Strings[2]);
                  if PrimarySchema = '' then
                    Result.UpdateNull(CrossRefKeyColPKTableSchemaIndex)
                  else
                    Result.UpdateString(CrossRefKeyColPKTableSchemaIndex, PrimarySchema);

                  if PrimaryTable = KeyList.Strings[3] then
                    Continue;

                  Result.UpdateString(CrossRefKeyColPKTableNameIndex, PrimaryTable);
                  Result.UpdateString(CrossRefKeyColPKColumnNameIndex, KeyList.Strings[4]);
                  Result.UpdateString(CrossRefKeyColFKTableCatalogIndex, LForeignCatalog);
                  if ForeignSchema = '' then
                    Result.UpdateNull(CrossRefKeyColFKTableSchemaIndex)
                  else
                    Result.UpdateString(CrossRefKeyColFKTableSchemaIndex, ForeignSchema);
                  if ForeignTable <> GetString(ColumnIndexes[3]) then
                    Continue
                  else
                    Result.UpdateString(CrossRefKeyColFKTableNameIndex, GetString(ColumnIndexes[3]));
                  Result.UpdateString(CrossRefKeyColFKColumnNameIndex, KeyList.Strings[0]);
                  Result.UpdateInt(CrossRefKeyColKeySeqIndex, KeySeq);
                  Result.UpdateInt(CrossRefKeyColUpdateRuleIndex, Ord(ikSetDefault));
                  Result.UpdateInt(CrossRefKeyColDeleteRuleIndex, Ord(ikSetDefault));
                  Result.UpdateNull(CrossRefKeyColFKNameIndex);
                  Result.UpdateNull(CrossRefKeyColPKNameIndex);
                  Result.UpdateInt(CrossRefKeyColDeferrabilityIndex, Ord(ikSetDefault)); // DEFERRABILITY
                  Inc(KeySeq);
                  Result.InsertRow;
                end;
              end;
            end;
          end;
        end;
        Close;
      end;
    finally
      KeyList.Free;
      CommentList.Free;
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
function TZMySQLDatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
const
  MaxTypeCount = 33;
  TypeNames: array[1..MaxTypeCount] of string = (
    'BIT', 'BOOL', 'TINYINT', 'BIGINT', 'MEDIUMBLOB', 'LONG VARBINARY',
    'LONGBLOB', 'BLOB', 'TINYBLOB', 'VARBINARY', 'BINARY',
    'LONG VARCHAR', 'MEDIUMTEXT', 'LONGTEXT', 'TEXT', 'TINYTEXT',
    'CHAR', 'VARCHAR', 'NUMERIC', 'DECIMAL', 'INTEGER', 'INT',
    'MEDIUMINT', 'SMALLINT', 'DOUBLE', 'FLOAT', 'REAL', 'ENUM', 'SET',
    'DATE', 'TIME', 'DATETIME', 'TIMESTAMP');
  TypeCodes: array[1..MaxTypeCount] of TZSQLType = (
    stByte, stBoolean, stSmall, stLong, stBinaryStream, stBinaryStream,
    stBinaryStream, stBinaryStream, stBinaryStream, stBytes, stBytes,
    stString, stAsciiStream, stAsciiStream, stAsciiStream, stAsciiStream,
    stString, stString, stBigDecimal, stBigDecimal, stInteger, stInteger,
    stInteger, stSmall, stDouble, stFloat, stFloat, stString, stString,
    stDate, stTime, stTimestamp, stTimestamp);
  TypePrecision: array[1..MaxTypeCount] of Integer = (
    1, -1, 4, 16, 16777215, 16777215, MAXBUF, 65535, 255, 255, 255,
    16777215, 16777215, 2147483647, 65535, 255, 255, 255, 17, 17, 10, 10,
    7, 4, 17, 10, 10, 65535, 64, -1, -1, -1, -1);
var
  I: Integer;
begin
    Result:=inherited UncachedGetTypeInfo;

    for I := 1 to MaxTypeCount do
    begin
      Result.MoveToInsertRow;

      Result.UpdateString(TypeInfoTypeNameIndex, TypeNames[I]);
      Result.UpdateByte(TypeInfoDataTypeIndex, Ord(TypeCodes[I]));
      if TypePrecision[I] >= 0 then
        Result.UpdateInt(TypeInfoPecisionIndex, TypePrecision[I])
      else
        Result.UpdateNull(TypeInfoPecisionIndex);
      if TypeCodes[I] in [stString, stBytes, stDate, stTime,
        stTimeStamp, stBinaryStream, stAsciiStream] then
      begin
        Result.UpdateString(TypeInfoLiteralPrefixIndex, '''');
        Result.UpdateString(TypeInfoLiteralSuffixIndex, '''');
      end
      {else
      begin
        Result.UpdateNull(TypeInfoLiteralPrefixIndex);
        Result.UpdateNull(TypeInfoLiteralSuffixIndex);
      end};
      //Result.UpdateNull(TypeInfoCreateParamsIndex);
      Result.UpdateInt(TypeInfoNullAbleIndex, Ord(ntNullable));
      Result.UpdateBoolean(TypeInfoCaseSensitiveIndex, False);
      Result.UpdateBoolean(TypeInfoSearchableIndex, False);
      //Result.UpdateNull(TypeInfoUnsignedAttributeIndex);
      Result.UpdateBoolean(TypeInfoFixedPrecScaleIndex, False);
      Result.UpdateBoolean(TypeInfoAutoIncrementIndex, TypeNames[I] = 'INTEGER');
      //Result.UpdateNull(TypeInfoLocaleTypeNameIndex);
      //Result.UpdateNull(TypeInfoMinimumScaleIndex);
      //Result.UpdateNull(TypeInfoMaximumScaleIndex);
      //Result.UpdateNull(TypeInfoSQLDataTypeIndex);
      //Result.UpdateNull(TypeInfoSQLDateTimeSubIndex);
      Result.UpdateInt(TypeInfoNumPrecRadix, 10);

      Result.InsertRow;
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
function TZMySQLDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  Len: NativeUInt;
  LCatalog, LTable: string;
  ColumnIndexes : Array[1..7] of integer;
begin
    if Table = '' then
      raise Exception.Create(STableIsNotSpecified); //CHANGE IT!

    Result:=inherited UncachedGetIndexInfo(Catalog, Schema, Table, Unique, Approximate);

    GetCatalogAndNamePattern(Catalog, Schema, Table,
      LCatalog, LTable);

    with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(
      Format('SHOW INDEX FROM %s.%s',
      [IC.Quote(LCatalog),
      IC.Quote(LTable)])) do
    begin
      ColumnIndexes[1] := FindColumn('Table');
      ColumnIndexes[2] := FindColumn('Non_unique');
      ColumnIndexes[3] := FindColumn('Key_name');
      ColumnIndexes[4] := FindColumn('Seq_in_index');
      ColumnIndexes[5] := FindColumn('Column_name');
      ColumnIndexes[6] := FindColumn('Collation');
      ColumnIndexes[7] := FindColumn('Cardinality');
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateString(CatalogNameIndex, LCatalog);
        //Result.UpdateNull(SchemaNameIndex);
        Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(ColumnIndexes[1], Len), @Len);
        if GetInt(ColumnIndexes[2]) = 0 then
          Result.UpdateString(IndexInfoColNonUniqueIndex, 'true')
        else
          Result.UpdateString(IndexInfoColNonUniqueIndex, 'false');
        //Result.UpdateNull(IndexInfoColIndexQualifierIndex);
        Result.UpdatePAnsiChar(IndexInfoColIndexNameIndex, GetPAnsiChar(ColumnIndexes[3], Len), @Len);
        Result.UpdateByte(IndexInfoColTypeIndex, Ord(tiOther));
        Result.UpdateInt(IndexInfoColOrdPositionIndex, GetInt(ColumnIndexes[4]));
        Result.UpdatePAnsiChar(IndexInfoColColumnNameIndex, GetPAnsiChar(ColumnIndexes[5], Len), @Len);
        Result.UpdatePAnsiChar(IndexInfoColAscOrDescIndex, GetPAnsiChar(ColumnIndexes[6], Len), @Len);
        Result.UpdatePAnsiChar(IndexInfoColCardinalityIndex, GetPAnsiChar(ColumnIndexes[7], Len), @Len);
        Result.UpdateInt(IndexInfoColPagesIndex, 0);
        //Result.UpdateNull(IndexInfoColFilterConditionIndex);
        Result.InsertRow;
      end;
      Close;
    end;
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
function TZMySQLDatabaseMetadata.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
var
  SQL: string;
  ProcedureNameCondition, SchemaCondition: string;
begin
  If Catalog = '' then
    If SchemaPattern <> ''
    then SchemaCondition := ConstructNameCondition(SchemaPattern,'p.db')
    else SchemaCondition := ConstructNameCondition(FDatabase,'p.db')
  else
    SchemaCondition := ConstructNameCondition(Catalog,'p.db');
  ProcedureNameCondition := ConstructNameCondition(ProcedureNamePattern,'p.name');
  If SchemaCondition <> '' then
    SchemaCondition := ' and ' + SchemaCondition;
  If ProcedureNameCondition <> '' then
    ProcedureNameCondition := ' and ' + ProcedureNameCondition;

  SQL := 'SELECT NULL AS PROCEDURE_CAT, p.db AS PROCEDURE_SCHEM, '+
      'p.name AS PROCEDURE_NAME, NULL AS RESERVED1, NULL AS RESERVED2, '+
      'NULL AS RESERVED3, p.comment AS REMARKS, '+
      ZFastCode.IntToStr(Ord(ProcedureReturnsResult))+' AS PROCEDURE_TYPE  from  mysql.proc p '+
      'WHERE 1=1' + SchemaCondition + ProcedureNameCondition+
      ' ORDER BY p.db, p.name';
    Result := CopyToVirtualResultSet(
    GetConnection.CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(ProceduresColumnsDynArray));
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
function TZMySQLDatabaseMetadata.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
const
  {%H-}PROCEDURE_CAT_index  = FirstDbcIndex + 0;
  PROCEDURE_SCHEM_index     = FirstDbcIndex + 1;
  PROCEDURE_NAME_Index      = FirstDbcIndex + 2;
  PARAMS_Index              = FirstDbcIndex + 3;
  {%H-}REMARKS_Index        = FirstDbcIndex + 4;
  {%H-}PROCEDURE_TYPE_Index = FirstDbcIndex + 5;
  RETURN_VALUES_Index       = FirstDbcIndex + 6;
var
  Len: NativeUInt;
  SQL: String;
  TypeName, Temp: RawByteString;
  ParamList, Params, Names, Returns: TStrings;
  I, ColumnSize, Precision: Integer;
  FieldType: TZSQLType;
  ProcedureNameCondition, SchemaCondition: string;

  function GetNextName(const AName: String; NameEmpty: Boolean = False): String;
  var N: Integer;
  begin
    if (Names.IndexOf(AName) = -1) and not NameEmpty then
    begin
      Names.Add(AName);
      Result := AName;
    end
    else
      for N := 1 to MaxInt do
        if Names.IndexOf(AName+ZFastCode.IntToStr(N)) = -1 then
        begin
          Names.Add(AName+ZFastCode.IntToStr(N));
          Result := AName+ZFastCode.IntToStr(N);
          Break;
        end;
  end;

  function DecomposeParamFromList(AList: TStrings): String;
  var
    J, I, N: Integer;
    Temp, TypeName: String;
    procedure AddTempString(Const Value: String);
    begin
      if Temp = '' then
        Temp := Trim(Value)
      else
        Temp := Temp + LineEnding+ Trim(Value);
    end;

  begin
    J := 0;
    Temp := '';
    for I := 0 to AList.Count -1 do
      if J < AList.Count then
      begin
        if (ZFastCode.Pos('(', (AList[J])) > 0) and (ZFastCode.Pos(')', (AList[J])) = 0) then
          if ( ZFastCode.Pos('real', LowerCase(AList[J])) > 0 ) or
             ( ZFastCode.Pos('float', LowerCase(AList[J])) > 0 ) or
             ( ZFastCode.Pos('decimal', LowerCase(AList[J])) > 0 ) or
             ( ZFastCode.Pos('numeric', LowerCase(AList[J])) > 0 ) or
             ( ZFastCode.Pos('double', LowerCase(AList[J])) > 0 ) then
          begin
            AddTempString(AList[j]+','+AList[j+1]);
            Inc(j);
          end
          else
            if ( ZFastCode.Pos('set', LowerCase(AList[J])) > 0 ) and
              ( ZFastCode.Pos(')', LowerCase(AList[J])) = 0 ) then
            begin
              TypeName := AList[J];
              for N := J+1 to AList.Count-1 do
              begin
                TypeName := TypeName +','+AList[N];
                if ZFastCode.Pos(')', AList[N]) > 0 then
                  Break;
              end;
              AddTempString(TypeName);
              J := N;
            end
            else
              AddTempString(AList[j])
        else
          if not (AList[j] = '') then
            AddTempString(AList[j]);
        Inc(J);
      end;
    Result := Temp;
  end;
begin
  If Catalog = '' then
    If SchemaPattern <> '' then
    SchemaCondition := ConstructNameCondition(SchemaPattern,'p.db')
    else
    SchemaCondition := ConstructNameCondition(FDatabase,'p.db')
  else
    SchemaCondition := ConstructNameCondition(Catalog,'p.db');
  ProcedureNameCondition := ConstructNameCondition(ProcedureNamePattern,'p.name');
  If SchemaCondition <> '' then
    SchemaCondition := ' and ' + SchemaCondition;
  If ProcedureNameCondition <> '' then
    ProcedureNameCondition := ' and ' + ProcedureNameCondition;

  Result := inherited UncachedGetProcedureColumns(Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);

  SQL := 'SELECT NULL AS PROCEDURE_CAT, p.db AS PROCEDURE_SCHEM, '+
      'p.name AS PROCEDURE_NAME, p.param_list AS PARAMS, p.comment AS REMARKS, '+
    ZFastCode.IntToStr(Ord(ProcedureReturnsResult))+' AS PROCEDURE_TYPE, p.returns AS RETURN_VALUES '+
    ' from  mysql.proc p where 1 = 1'+SchemaCondition+ProcedureNameCondition+
    ' ORDER BY p.db, p.name';

    try
      with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(SQL) do
      begin
        ParamList := TStringList.Create;
        Params := TStringList.Create;
        Names := TStringList.Create;
        Returns := TStringList.Create;
        while Next do
        begin
          PutSplitString(ParamList, Trim(GetString(PARAMS_Index)), ',');
          PutSplitString(ParamList, DecomposeParamFromList(ParamList), LineEnding);

          PutSplitString(Returns, Trim(GetString(RETURN_VALUES_Index)), ',');
          PutSplitString(Returns, DecomposeParamFromList(Returns), LineEnding);

          for I := 0 to Returns.Count-1 do
          begin
            Returns[i] := 'RETURNS '+Returns[i];
            ParamList.Add(Returns[i]);
          end;

          for i := 0 to ParamList.Count -1 do
          begin
            PutSplitString(Params, ParamList[i], ' ');
            if Params.Count = 2 then {no name available}
              if Params[0] = 'RETURNS' then
                Params.Insert(1,'')
              else
                if (UpperCase(Params[1]) = 'IN') or
                    (UpperCase(Params[1]) = 'INOUT') or
                    (UpperCase(Params[1]) = 'OUT') then
                  Params.Insert(1,'')
                else
                  Params.Insert(0,'IN'); //Function in value

            Result.MoveToInsertRow;
            Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(PROCEDURE_SCHEM_index, Len), @Len); //PROCEDURE_CAT
            //Result.UpdateNull(SchemaNameIndex); //PROCEDURE_SCHEM
            Result.UpdatePAnsiChar(ProcColProcedureNameIndex, GetPAnsiChar(PROCEDURE_NAME_Index, Len), @Len); //PROCEDURE_NAME
            TypeName := ConSettings^.ConvFuncs.ZStringToRaw(Params[2], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
            ConvertMySQLColumnInfoFromString(TypeName, ConSettings, Temp, FieldType, ColumnSize, Precision,
              fMySQL_FieldType_Bit_1_IsBoolean);
            { process COLUMN_NAME }
            if Params[1] = '' then
              if Params[0] = 'RETURNS' then
                Result.UpdateString(ProcColColumnNameIndex, 'ReturnValue')
              else
                Result.UpdateString(ProcColColumnNameIndex, GetNextName('$', True))
            else
              Result.UpdateString(ProcColColumnNameIndex, GetNextName(DecomposeObjectString(Params[1])));
            { COLUMN_TYPE }
            if UpperCase(Params[0]) = 'OUT' then
              Result.UpdateByte(ProcColColumnTypeIndex, Ord(pctOut))
            else
              if UpperCase(Params[0]) = 'INOUT' then
                Result.UpdateByte(ProcColColumnTypeIndex, Ord(pctInOut))
              else
                if UpperCase(Params[0]) = 'IN' then
                  Result.UpdateByte(ProcColColumnTypeIndex, Ord(pctIn))
                else
                  if UpperCase(Params[0]) = 'RETURNS' then
                    Result.UpdateByte(ProcColColumnTypeIndex, Ord(pctReturn))
                  else
                    Result.UpdateByte(ProcColColumnTypeIndex, Ord(pctUnknown));

            { DATA_TYPE }
            Result.UpdateByte(ProcColDataTypeIndex, Ord(FieldType));
            { TYPE_NAME }
            Result.UpdateRawByteString(ProcColTypeNameIndex, TypeName);
            { PRECISION }
            Result.UpdateInt(ProcColPrecisionIndex, ColumnSize);
            { LENGTH }
            Result.UpdateInt(ProcColLengthIndex, Precision);

            //Result.UpdateNull(ProcColScaleIndex);
            //Result.UpdateNull(ProcColRadixIndex);
            Result.UpdateInt(ProcColNullableIndex, Ord(ntNullableUnknown));
            //Result.UpdateNull(ProcColRemarksIndex);
            Result.InsertRow;
          end;
        end;
        Close;
      end;
    finally
      FreeAndNil(Names);
      FreeAndNil(Params);
      FreeAndNil(ParamList);
      FreeAndNil(Returns);
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
 	<LI><B>DECIMAL_DIGITS</B> short	 => scale
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
function TZMySQLDatabaseMetadata.UncachedGetVersionColumns(const Catalog, Schema,
  Table: string): IZResultSet;
begin
    Result:=inherited UncachedGetVersionColumns(Catalog, Schema, Table);

    Result.MoveToInsertRow;
    Result.UpdateNull(FirstDbcIndex);
    Result.UpdateString(FirstDbcIndex + 1, 'ctid');
  //  Result.UpdateInt(FirstDbcIndex + 2, GetSQLType('tid')); //FIX IT
    Result.UpdateString(FirstDbcIndex + 3, 'tid');
    Result.UpdateNull(FirstDbcIndex + 4);
    Result.UpdateNull(FirstDbcIndex + 5);
    Result.UpdateNull(FirstDbcIndex + 6);
    Result.UpdateInt(FirstDbcIndex + 7, Ord(vcPseudo));
    Result.InsertRow;
end;

{**
  Gets the used Collation and CharacterSet of spezified Object.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" and Catolog "" retrieves nothing
  @param table a table name; "" retrieves the Schema Colloation and CharacterSet
  @param ColumnNamePattern ColumnPattern;"" retrieves the
    Table(if @param TablePattern is set) or
    Schema(if @param TablePattern is NULL)
      Colloation and CharacterSet
  @return <code>ResultSet</code> - each row is a Collation, CharacterSet, ID,
    and ByteLength per Char of speziefied Object
}
function TZMySQLDatabaseMetadata.UncachedGetCollationAndCharSet(const Catalog, SchemaPattern,
  TableNamePattern, ColumnNamePattern: string): IZResultSet; //EgonHugeist
const
  COLLATION_NAME_Index     = FirstDbcIndex + 0;
  CHARACTER_SET_NAME_Index = FirstDbcIndex + 1;
  MAXLEN_Index             = FirstDbcIndex + 2;
var
  Len: NativeUInt;
  SQL, LCatalog: string;
  ColumnNameCondition, TableNameCondition, SchemaCondition: string;
begin
    if Catalog = '' then
    begin
      if SchemaPattern <> '' then
        LCatalog := SchemaPattern
      else
        LCatalog := FDatabase;
    end
    else
      LCatalog := Catalog;
  If Catalog = '' then
    If SchemaPattern <> '' then
      SchemaCondition := ConstructNameCondition(SchemaPattern,'TABLE_SCHEMA')
    else
      SchemaCondition := ConstructNameCondition(FDatabase,'TABLE_SCHEMA')
  else
    SchemaCondition := ConstructNameCondition(Catalog,'TABLE_SCHEMA');
  TableNameCondition := ConstructNameCondition(TableNamePattern,'TABLE_NAME');
  ColumnNameCondition := ConstructNameCondition(ColumnNamePattern,'COLUMN_NAME');
  If SchemaCondition <> '' then
    SchemaCondition := ' and ' + SchemaCondition;
  If TableNameCondition <> '' then
    TableNameCondition := ' and ' + TableNameCondition;
  If ColumnNameCondition <> '' then
    ColumnNameCondition := ' and ' + ColumnNameCondition;

  Result:=inherited UncachedGetCollationAndCharSet(Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);

  if SchemaCondition <> '' then
  begin
    if TableNamePattern <> '' then
    begin
      if ColumnNamePattern <> '' then
      begin
        SQL := 'SELECT CLMS.COLLATION_NAME, CLMS.CHARACTER_SET_NAME, CS.MAXLEN '+
          'FROM INFORMATION_SCHEMA.COLUMNS CLMS '+
          'LEFT JOIN INFORMATION_SCHEMA.CHARACTER_SETS CS '+
          'ON CS.DEFAULT_COLLATE_NAME = CLMS.COLLATION_NAME '+
          'WHERE 1=1'+ SchemaCondition + TableNameCondition + ColumnNameCondition;
        with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(SQL) do
        begin
          if Next then
          begin
            Result.MoveToInsertRow;
            Result.UpdateString(CatalogNameIndex, LCatalog);   //COLLATION_CATALOG
            Result.UpdateString(SchemaNameIndex, LCatalog);   //COLLATION_SCHEMA
            Result.UpdateString(TableNameIndex, TableNamePattern); //COLLATION_TABLE
            Result.UpdateString(ColumnNameIndex, ColumnNamePattern);//COLLATION_COLUMN
            Result.UpdatePAnsiChar(CollationNameIndex, GetPAnsiChar(COLLATION_NAME_Index, Len), @Len); //COLLATION_NAME
            Result.UpdatePAnsiChar(CharacterSetNameIndex, GetPAnsiChar(CHARACTER_SET_NAME_Index, Len), @Len); //CHARACTER_SET_NAME
            Result.UpdateSmall(CharacterSetSizeIndex, GetSmall(MAXLEN_Index)); //CHARACTER_SET_SIZE
            Result.InsertRow;
          end;
          Close;
        end;
      end
      else
      begin
        SQL := 'SELECT TBLS.TABLE_COLLATION, CS.CHARACTER_SET_NAME, CS.MAXLEN '+
          'FROM INFORMATION_SCHEMA.TABLES TBLS LEFT JOIN '+
          'INFORMATION_SCHEMA.CHARACTER_SETS CS ON '+
          'TBLS.TABLE_COLLATION = CS.DEFAULT_COLLATE_NAME '+
          'WHERE 1=1'+ SchemaCondition + TableNameCondition;
        with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(SQL) do
        begin
          if Next then
          begin
            Result.MoveToInsertRow;
            Result.UpdateString(CatalogNameIndex, LCatalog);
            Result.UpdateString(SchemaNameIndex, LCatalog);
            Result.UpdateString(TableNameIndex, TableNamePattern);
            Result.UpdatePAnsiChar(CollationNameIndex, GetPAnsiChar(COLLATION_NAME_Index, Len), @Len);  //COLLATION_NAME
            Result.UpdatePAnsiChar(CharacterSetNameIndex, GetPAnsiChar(CHARACTER_SET_NAME_Index, Len), @Len); //CHARACTER_SET_NAME
            Result.UpdateSmall(CharacterSetSizeIndex, GetSmall(MAXLEN_Index)); //CHARACTER_SET_SIZE
            Result.InsertRow;
          end;
          Close;
        end;
      end;
    end
    else
    begin
      SchemaCondition := ConstructNameCondition(LCatalog, 'and SCHEMA_NAME');
      SQL := 'SELECT S.DEFAULT_COLLATION_NAME, S.DEFAULT_CHARACTER_SET_NAME, '+
        'CS.MAXLEN FROM INFORMATION_SCHEMA.SCHEMATA S '+
        'LEFT JOIN INFORMATION_SCHEMA.CHARACTER_SETS CS '+
        'ON CS.DEFAULT_COLLATE_NAME = S.DEFAULT_COLLATION_NAME '+
        'WHERE 1=1 '+ SchemaCondition;
      with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(SQL) do
      begin
        if Next then
        begin
          Result.MoveToInsertRow;
          Result.UpdateString(CatalogNameIndex, LCatalog);
          Result.UpdateString(SchemaNameIndex, LCatalog);
          Result.UpdatePAnsiChar(CollationNameIndex, GetPAnsiChar(COLLATION_NAME_Index, Len), @Len);
          Result.UpdatePAnsiChar(CharacterSetNameIndex, GetPAnsiChar(CHARACTER_SET_NAME_Index, Len), @Len);
          Result.UpdateNull(CharacterSetIDIndex); //CHARACTER_SET_ID
          Result.UpdateSmall(CharacterSetSizeIndex, GetSmall(MAXLEN_Index)); //CHARACTER_SET_SIZE
          Result.InsertRow;
        end;
        Close;
      end;
    end;
  end;
end;

{**
  Gets the supported CharacterSets:
  @return <code>ResultSet</code> - each row is a CharacterSetName and it's ID
}
function TZMySQLDatabaseMetadata.UncachedGetCharacterSets: IZResultSet; //EgonHugeist
var Len: NativeUInt;
begin
  Result:=inherited UncachedGetCharacterSets;

  with GetConnection.CreateStatementWithParams(FInfo).ExecuteQuery(
    'SELECT CHARACTER_SET_NAME '+
    'FROM INFORMATION_SCHEMA.CHARACTER_SETS') do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdatePAnsiChar(CharacterSetsNameIndex, GetPAnsiChar(FirstDbcIndex, Len), @Len);
      Result.InsertRow;
    end;
    Close;
  end;
end;

end.
