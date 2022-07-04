{   The unit SqlitePassConst defines all the constants used by
    the SqlitePass Database Objects Project.

   ---------------------------------------------------------------------------
   
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 2.1 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   ---------------------------------------------------------------------------

    Messages localisation unit
    Author : Luc DAVID Email: luckylazarus@free.fr
    2006-2010

    Major changes are indicated in the \Documentation\changes.pdf file
    Last update : 09.05.2010

   --------------------------------------------------------------------------- }


Unit SqlitePassConst;

Interface

Const

{ Components version }
SqlitePassPackageVersion = '0.51';
SqlitePassDatabaseVersion = '0.51';
SqlitePassDatasetVersion = '0.51';

{ Storage version }
SqlitePassStorageVersion = '0.40';

{ Default sqlite library file }
{$IFDEF WIN32}
  DefaultSQLiteLibrary = 'sqlite3.dll';
  SqlitePassPathSeparator = '\';
{$ELSE}
  DefaultSQLiteLibrary = 'libsqlite3.so';
  SqlitePassPathSeparator = '/';
{$ENDIF}

{ TFields }
DefaultStringFieldSize = 50;

{ Date }
JulianEpoch = -2415018.5;
UnixEpoch = JulianEpoch + 2440587.5;

{ System tables }
SqlitePassDbSettingsTable = 'SQLitePass__DbSettings';
SqlitePassSQLStmtDefsTable = 'SQLitePass__SQLStmts';

{ Internal record status flag position in byte (rsEnabled, rsModified, rsInserted, rsDeleted, rsVisible, 0,0,0) }
rsEnabledFlag  = 0;
rsVisibleFlag  = 1;
rsModifiedFlag = 2;
rsInsertedFlag = 3;
rsDeletedFlag  = 4;

{ Default value when testing record status }
rsEnabled     = 0; { Default : At loading time, AllocMem initialize memory to 0 = value is Enabled }
rsNotEnabled  = 1;
rsInserted    = 1;
rsNotInserted = 0; { Default : At loading time, AllocMem initialize memory to 0 = value is not Inserted }
rsModified    = 1;
rsNotModified = 0; { Default : At loading time, AllocMem initialize memory to 0 = value is not Modified }
rsDeleted     = 1;
rsNotDeleted  = 0; { Default : At loading time, AllocMem initialize memory to 0 = value is not Deleted }
rsVisible     = 0; { Default : At loading time, AllocMem initialize memory to 0 = value is Visible }
rsNotVisible  = 1;

{ Internal Field status flag position in byte (fsNullFlag,fsModifiedFlag,0,0,0,0,0,0) }
fsNullFlag    = 0;
fsModifiedFlag = 1;

{ Default value when testing field status }
fsNull         = 1;
fsNotNull      = 0; { Default : At loading time, AllocMem initialize memory to 0 = value is not null }
fsModified     = 1;
fsNotModified  = 0; { Default : At loading time, AllocMem initialize memory to 0 = value is not modified }

{ Verbose Level (For error handling) }
vlSilent     = 0;
vlLog        = 1;
vlShow       = 2;
vlLogAndShow = 3;

{  Global Keywords definition }
kwSQLEnd  = ';' ;
KwASAlias = 'AS';
KwSpace = ' ';
KwAnd = 'AND';
kwLineBreak = Char($A);
KwLineReturn = Char($D);
kwLineEnd = KwLineReturn + KwLineBreak;

{ Keywords definition for [create] statement }
kwCreate = 'CREATE';
kwCreateTable = 'CREATE TABLE';
kwCreateTempTable = 'CREATE TEMP TABLE';
kwCreateTemporaryTable = 'CREATE TEMPORARY TABLE';

{ Keywords definition for [select] statement }
kwSelect  = 'SELECT';
kwAll = 'ALL';
kwDistinct = 'DISTINCT';
kwFrom    = 'FROM';
kwWhere   = 'WHERE';
kwGroupBy = 'GROUP BY';
kwHaving  = 'HAVING';
kwOrderBy = 'ORDER BY';
kwLimit   = 'LIMIT';
kwLike    = 'LIKE';
KwAllFields = '*';
kwComma = ',';
kwDot = '.';
kwBraceOpen = '(';
kwBraceClose = ')';
kwEqual = '=';

{ Keywords for Quotes }
kwBracketOpen = '[';
kwBracketClose = ']';
kwSingleQuote = '''';
kwGraveQuote = '`';
kwDoubleQuote = '"';

SelectStmtKeywords: array[0..7] of String = (kwSelect, kwFrom, kwWhere, kwGroupBy, kwHaving, kwOrderBy, kwLimit, kwSQLEnd);
SelectStmtTableDefs: array[0..3] of String = (kwFrom, kwAsAlias, kwComma, KwSQLEnd);
SelectStmtFieldDefs: array[0..4] of String = (kwSelect, kwAll, KwDistinct, kwComma, KwSQLEnd);

{ Keywords definition for [Insert] statement }
kwInsert = 'INSERT';

{ Keywords definition for [update] statement }
kwUpdate = 'UPDATE';

{ Keywords definition for [delete] statement }
kwDelete = 'DELETE';

{ }
kwPragmaTableInfo = 'PRAGMA TABLE_INFO';

QuoteCharsArray: Array[0..4] of String = (KwSingleQuote, KwDoubleQuote, kwGraveQuote, kwBracketOpen, kwBracketClose);
QuoteChars = [KwSingleQuote, KwDoubleQuote, kwBracketOpen, kwBracketClose];
BeforeIdentifierSeparator = [KwSpace, KwComma, kwLineBreak, kwBraceOpen];
AfterIdentifierSeparator = [KwSpace, KwComma, kwLineReturn, kwBraceClose, KwSqlEnd];

{ Keywords definitions for statement type checking }
StmtTypeKeywords: array[0..7] of String = (kwCreateTemporaryTable, kwCreateTempTable, kwCreateTable, kwCreate,
                                           kwSelect, {kwPragmaTableInfo,} KwInsert, kwUpdate, kwDelete);

IdentifierChar: Set of Char = ['0'..'9', 'a'..'z', 'A'..'Z', '_', '.', 'é', 'è', 'ê'];

SqlitePassSQLTokenTypeAsString: Array[0..41] of String =
  ('ttKeyword', 'ttIdentifier', 'ttDatabaseIdentifier', 'ttTableIdentifier', 'ttFieldIdentifier', 'ttFunctionIdentifier',
   'ttStar', 'ttNumber', 'ttDateTime', 'ttSingleQuote', 'ttDoubleQuote', 'ttGraveQuote', 'ttHash', 'ttPercent', 'ttBraceOpen', 'ttBraceClose', 'ttSquareOpen', 'ttSquareClose',
   'ttRoundOpen', 'ttRoundClose', 'ttWhitespace', 'ttComment', 'ttComma', 'ttColon', 'ttSemiColon', 'ttEqual', 'ttNotEqual',
   'ttGreater', 'ttGreaterOrEqual', 'ttLesser', 'ttLesserOrEqual', 'ttNul', 'ttNotNull', 'ttPlus', 'ttMinus', 'ttAnd', 'ttOr', 'ttDot', 'ttSlash', 'ttSeparator',
   'ttEOF', 'ttUnknown');


{ ----- Special declarations for Kexi ----- }

{ Kexi system tables }
KexiSystemTables:array[0..7] of string =
           ('kexi__db',
            'kexi__fields',
            'kexi__final',
            'kexi__objects',
            'kexi__objectdata',
            'kexi__parts',
            'kexi__useractions',
            'kexi__blobs');

{ Kexi field constraints info }
   (*
   NoConstraints = 0,
   AutoInc = 1, ?
   Unique = 2,  ?
   PrimaryKey = 4, ?
   ForeignKey = 8, -> FieldType
   NotNull = 16,   ?
   NotEmpty = 32, ? //!< only legal for string-like and blob fields
   Indexed = 64 -> IsIndexField
   *)

KexiFieldConstraints = 'NoConstraints,AutoInc,Unique,PrimaryKey,ForeignKey,NotNull,NotEmpty,Indexed';

{ SQL Stqtements used to create system tables needed by Kexi (used by TSqlitePassDatabase.CreateDatabase) }
KexiDb_CreateSQLStmt = 'CREATE TABLE kexi__db (db_property Text(32), db_value CLOB);';
KexiParts_CreateSQLStmt = 'CREATE TABLE kexi__parts (p_id INTEGER PRIMARY KEY, p_name Text(200), p_mime Text(200), p_url Text(200));';
KexiObjects_CreateSQLStmt = 'CREATE TABLE kexi__objects (o_id INTEGER PRIMARY KEY, o_type Byte UNSIGNED, o_name Text(200), o_caption Text(200), o_desc CLOB);';
KexiObjectData_CreateSQLStmt = 'CREATE TABLE kexi__objectdata (o_id Integer UNSIGNED NOT NULL, o_data CLOB, o_sub_id Text(200));';
KexiFields_CreateSQLStmt = 'CREATE TABLE kexi__fields (t_id Integer UNSIGNED, f_type Byte UNSIGNED, f_name Text(200), f_length Integer, f_precision Integer, f_constraints Integer, f_options Integer, f_default Text(200), f_order Integer, f_caption Text(200), f_help CLOB);';
KexiBlobs_CreateSQLStmt = 'CREATE TABLE kexi__blobs (o_id INTEGER PRIMARY KEY, o_data BLOB, o_name Text(200), o_caption Text(200), o_mime Text(200) NOT NULL, o_folder_id Integer UNSIGNED);';

Implementation
end.
