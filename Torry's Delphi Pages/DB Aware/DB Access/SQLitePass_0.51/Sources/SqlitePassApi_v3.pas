{   The unit SqlitePassApi_v3 defines all the external functions calls to
    the sqlite database engine library.


   ---------------------------------------------------------------------------

    This library is free software; you can redistribute it and/or modify
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

    Sqlite3 library translation for use with fpc compiler
    Original code from Mike Cariotoglou Email: mike@deltasingular.gr
    Modified by Luc DAVID Email: luckylazarus@free.fr
    2006 - 2010
    
    Major changes are indicated in the \Documentation\changes.pdf file
    Last update 27.10.2009

  --------------------------------------------------------------------------- }

unit SqlitePassApi_v3;
{$i SqlitePassDbo.inc}

interface

uses
Classes, SysUtils, SqlitePassErrorLang, SqlitePassUtils
{$IFDEF WIN32}
,Windows
{$ELSE}
,Dynlibs
{$ENDIF};
const

{ ----- SQLite declarations ----- }

SQLITE_OK         =  0;   // Successful result
SQLITE_ERROR      =  1;   // SQL error or missing database
SQLITE_INTERNAL   =  2;   // An internal logic error in SQLite
SQLITE_PERM       =  3;   // Access permission denied
SQLITE_ABORT      =  4;   // Callback routine requested an abort
SQLITE_BUSY       =  5;   // The database file is locked
SQLITE_LOCKED     =  6;   // A table in the database is locked
SQLITE_NOMEM      =  7;   // A malloc() failed
SQLITE_READONLY   =  8;   // Attempt to write a readonly database
SQLITE_INTERRUPT  =  9;   // Operation terminated by sqlite_interrupt()
SQLITE_IOERR      = 10;   // Some kind of disk I/O error occurred
SQLITE_CORRUPT    = 11;   // The database disk image is malformed
SQLITE_NOTFOUND   = 12;   // (Internal Only) Table or record not found
SQLITE_FULL       = 13;   // Insertion failed because database is full
SQLITE_CANTOPEN   = 14;   // Unable to open the database file
SQLITE_PROTOCOL   = 15;   // Database lock protocol error
SQLITE_EMPTY      = 16;   // (Internal Only) Database table is empty
SQLITE_SCHEMA     = 17;   // The database schema changed
SQLITE_TOOBIG     = 18;   // Too much data for one row of a table
SQLITE_CONSTRAINT = 19;   // Abort due to contraint violation
SQLITE_MISMATCH   = 20;   // Data type mismatch
SQLITE_MISUSE     = 21;   // Library used incorrectly
SQLITE_NOLFS      = 22;   // Uses OS features not supported on host
SQLITE_AUTH       = 23;   // Authorization denied
SQLITE_FORMAT     = 24;   // Auxiliary database format error
SQLITE_RANGE      = 25;   // 2nd parameter to sqlite_bind out of range
SQLITE_NOTADB     = 26;   // File opened that is not a database file
SQLITE_ROW        = 100;  // sqlite_step() has another row ready
SQLITE_DONE       = 101;  // sqlite_step() has finished executing

SQLITE_INTEGER    = 1;
SQLITE_FLOAT      = 2;
SQLITE_TEXT       = 3;
SQLITE_BLOB       = 4;
SQLITE_NULL       = 5;

{
** These are the allowed values for the eTextRep argument to
** sqlite3_create_collation and sqlite3_create_function.
}
SQLITE_UTF8       = 1;
SQLITE_UTF16LE    = 2;
SQLITE_UTF16BE    = 3;
SQLITE_UTF16      = 4;    // Use native byte order
SQLITE_ANY        = 5;    // sqlite3_create_function only

SQLITE_STATIC     = 0;
SQLITE_TRANSIENT  = -1;

{
** These bit values are intended for use in the 3rd parameter to the sqlite3_open_v2()
** interface and in the 4th parameter to the xOpen method of the sqlite3_vfs object.
}
SQLITE_OPEN_READONLY       = 1;
SQLITE_OPEN_READWRITE      = 2;
SQLITE_OPEN_CREATE         = 4;
SQLITE_OPEN_DELETEONCLOSE  = 8;
SQLITE_OPEN_EXCLUSIVE      = 10;
SQLITE_OPEN_MAIN_DB        = 100;
SQLITE_OPEN_TEMP_DB        = 200;
SQLITE_OPEN_TRANSIENT_DB   = 400;
SQLITE_OPEN_MAIN_JOURNAL   = 800;
SQLITE_OPEN_TEMP_JOURNAL   = 1000;
SQLITE_OPEN_SUBJOURNAL     = 2000;
SQLITE_OPEN_MASTER_JOURNAL = 4000;
SQLITE_OPEN_NOMUTEX        = 8000;
SQLITE_OPEN_FULLMUTEX      = 10000;

type
TDestructor=procedure(data:pointer); cdecl;
TExecCallback = function(Sender: pointer; ColumnsCount: Integer; ColumnValues: ppAnsiChar; ColumnNames: ppAnsiChar): integer; cdecl;
TBusyHandler=function(user:pointer; count:integer):integer; cdecl;
TFuncHandler=procedure(context:pointer; nArgs:integer; args:ppSqliteValue); cdecl;
TFuncFinalizer=procedure(context:pointer); cdecl;
TUserCollation=function(user:pointer;
                        lenA:integer;
                        a:pChar;
                        lenB:integer;
                        b:pChar):integer; cdecl;

TUserCollationNeeded=procedure(user:pointer;
                               db:pointer;
                               eTextRep:integer;
                               zName:pChar); cdecl;


var

{ Keeps track of engines using the library }
EnginesCount: Integer;

{ ----- Global Info functions ----- }
{.$IFDEF WIN32}
SqliteDbv3_SqliteLibVersion: function(): pAnsiChar; cdecl;
{.$ENDIF}
SqliteDbv3_SqliteLibVersionNumber: function(): Integer; cdecl;
SqliteDbv3_SqliteLibSourceId: function(): pAnsiChar; cdecl;
SqliteDbv3_compileoption_used: function(P: pChar): Integer; cdecl;

{ ----- Database functions ----- }
SqliteDbv3_close: function(db: Pointer):integer; cdecl;

{ Always ansi utf8 encoded }
SqliteDbv3_exec: function(db: Pointer;
                       SQLStatement: pUTF8String;
                       CallbackPtr: TExecCallBack;
                       CbParam: pointer;
                       ErrMsg: PPAnsiChar): integer; cdecl;

SqliteDbv3_sql: function(stmt: pointer):pChar; cdecl;
SqliteDbv3_last_insert_rowid: function(db: Pointer): int64; cdecl;
SqliteDbv3_changes: function(db: Pointer): integer; cdecl;
SqliteDbv3_total_changes: function(db: Pointer): integer; cdecl;
SqliteDbv3_interrupt: procedure(db: Pointer); cdecl;
SqliteDbv3_complete: function(P: pChar): integer; cdecl;
SqliteDbv3_busy_handler: function(db: Pointer;
                               CallbackPtr:TBusyHandler;
                               user:pointer):integer; cdecl;

SqliteDbv3_busy_timeout: function(db: Pointer; TimeOut: integer):integer; cdecl;
SqliteDbv3_free: procedure(P: pChar); cdecl;
SqliteDbv3_free_table: procedure(Table: pChar); cdecl;

{ Open a database and store the handle in 'db' }
SqliteDbv3_open: function(dbname: pAnsiChar; var db:pointer; flags: integer; vfs: pAnsiChar):integer; cdecl;
SqliteDbv3_open16: function(dbname: pWideChar; var db:pointer):integer; cdecl;

{ Error Messages }
SqliteDbv3_errcode:function(db:pointer):integer; cdecl;
SqliteDbv3_errmsg:function(db:pointer):pAnsiChar; cdecl;
SqliteDbv3_errmsg16:function(db:pointer):pWideChar; cdecl;

SqliteDbv3_get_table: function(db: Pointer; SQLStatement: pChar; var ResultPtr: Pointer;
      var RowCount: cardinal; var ColCount: cardinal; var ErrMsg: pChar): integer; cdecl;

{ ----- Query-Dataset functions ----- }

{ Deprecated functions }
(*SqliteDbv3_prepare:function(db:pointer;
                         Sql:pChar;
                         nBytes:integer;
                         var stmt:pointer;
                         var pzTail:pChar):integer; cdecl;  *)

{ ---- }

SqliteDbv3_prepare_v2:function(db:pointer;
                         Sql:pAnsiChar;
                         nBytes:integer;
                         var stmt:pointer;
                         var pzTail:pAnsiChar):integer; cdecl;

SqliteDbv3_prepare16_v2:function(db:pointer;
                         Sql:pWideChar;
                         nBytes:integer;
                         var stmt:pointer;
                         var pzTail:pWideChar):integer; cdecl;

{ Binding values to parameters }
SqliteDbv3_clear_binding:function(stmt:pointer):integer; cdecl;
SqliteDbv3_bind_double:function(stmt:pointer; idx:integer; value:double):integer; cdecl;
SqliteDbv3_bind_int:function(stmt:pointer; idx:integer; value:integer):integer; cdecl;
SqliteDbv3_bind_int64:function(stmt:pointer; idx:integer; value:int64):integer; cdecl;
SqliteDbv3_bind_null:function(stmt:pointer; idx:integer):integer; cdecl;
//SqliteDbv3_bind_value:function(stmt:pointer; idx:integer; value:pointer):integer; cdecl;
SqliteDbv3_bind_text:function(stmt:pointer;
                           idx:integer;
                           value:pAnsiChar;
                           size:integer;
                           xDel:Integer):integer; cdecl;
SqliteDbv3_bind_text16:function(stmt:pointer;
                           idx:integer;
                           value:pWideChar;
                           size:integer;
                           xDel:Integer):integer; cdecl;
SqliteDbv3_bind_blob:function(stmt:pointer;
                           idx:integer;
                           value:pointer;
                           size:integer;
                           xDel:integer):integer; cdecl;

SqliteDbv3_bind_parameter_count:function(stmt:pointer):integer; cdecl;
SqliteDbv3_bind_parameter_name:function(stmt:pointer; idx:integer):pChar; cdecl;
SqliteDbv3_bind_parameter_index:function(stmt:pointer; zName:pChar):integer; cdecl;

{ Column information }
SqliteDbv3_column_count:function(pStmt:pointer):integer; cdecl;
SqliteDbv3_column_name:function(pStmt:pointer; idx:integer):pAnsiChar; cdecl;
SqliteDbv3_column_name16:function(pStmt:pointer; idx:integer):pWideChar; cdecl;
SqliteDbv3_column_origin_name:function(pStmt:pointer; idx:integer):pAnsiChar; cdecl;
SqliteDbv3_column_origin_name16:function(pStmt:pointer; idx:integer):pWideChar; cdecl;
SqliteDbv3_column_table_name:function(pStmt:pointer; idx:integer):pAnsiChar; cdecl;
SqliteDbv3_column_table_name16:function(pStmt:pointer; idx:integer):pWideChar; cdecl;

{ returns text which is the declared type of the column in the CREATE TABLE statement }
SqliteDbv3_column_decltype:function(pStmt:pointer; idx:integer):pAnsiChar; cdecl;
SqliteDbv3_column_decltype16:function(pStmt:pointer; idx:integer):pWideChar; cdecl;

SqliteDbv3_step:function(pStmt:pointer):integer; cdecl;

SqliteDbv3_data_count:function(pStmt:pointer):integer; cdecl;

SqliteDbv3_column_blob:function(pStmt:pointer; col:integer):pointer; cdecl;
SqliteDbv3_column_bytes:function(pStmt:pointer; col:integer):integer; cdecl;
SqliteDbv3_column_double:function(pStmt:pointer; col:integer):double; cdecl;
SqliteDbv3_column_int:function(pStmt:pointer; col:integer):integer; cdecl;
SqliteDbv3_column_int64:function(pStmt:pointer; col:integer):int64; cdecl;
SqliteDbv3_column_text:function(pStmt:pointer; col:integer):pAnsiChar; cdecl;
SqliteDbv3_column_text16:function(pStmt:pointer; col:integer):pWideChar; cdecl;
SqliteDbv3_column_type:function(pStmt:pointer; col:integer):integer; cdecl;

SqliteDbv3_finalize:function(pStmt:pointer):integer; cdecl;
SqliteDbv3_reset:function(pStmt:pointer):integer; cdecl;

{ user defined functions }

SqliteDbv3_create_function:function(
  db:pointer;
  zFunctionName:pAnsiChar;
  nArg:integer;
  eTextRep:integer;
  userData:pointer;
  xFunc,
  xStep:TFuncHandler;
  xFinal:TFuncFinalizer):integer; cdecl;

SqliteDbv3_create_function16:function(
  db:pointer;
  zFunctionName:pWideChar;
  nArg:integer;
  eTextRep:integer;
  userData:pointer;
  xFunc,
  xStep:TFuncHandler;
  xFinal:TFuncFinalizer):integer; cdecl;

SqliteDbv3_aggregate_count:function(SqliteDbv3_context:pointer):integer;  cdecl;

SqliteDbv3_value_blob:function(v:PSqliteValue):pointer; cdecl;
SqliteDbv3_value_bytes:function(v:PSqliteValue):integer; cdecl;
SqliteDbv3_value_double:function(v:PSqliteValue):double; cdecl;
SqliteDbv3_value_int:function(v:PSqliteValue):integer; cdecl;
SqliteDbv3_value_int64:function(v:PSqliteValue):int64; cdecl;
SqliteDbv3_value_text:function(v:PSqliteValue):pAnsiChar; cdecl;
SqliteDbv3_value_text16:function(v:PSqliteValue):pWideChar; cdecl;

SqliteDbv3_value_type:function(v:PSqliteValue):integer; cdecl;

SqliteDbv3_aggregate_context:function(context:pointer; nBytes:integer):pointer; cdecl;

SqliteDbv3_user_data:function(context:pointer):pointer; cdecl;

SqliteDbv3_get_auxdata:function(context:pointer; idx:integer):pointer; cdecl;
SqliteDbv3_set_auxdata:procedure(context:pointer; idx:integer;
                              data:pointer;
                              xDel:integer); cdecl;

SqliteDbv3_result_blob:procedure(context:pointer; value:pointer; size:integer;
                              xDel:integer); cdecl;
SqliteDbv3_result_double:procedure(context:pointer; value:double); cdecl;
SqliteDbv3_result_error:procedure(context:pointer; msg:pChar; len:integer); cdecl;
SqliteDbv3_result_int:procedure(context:pointer; value:integer); cdecl;
SqliteDbv3_result_int64:procedure(context:pointer; value:int64); cdecl;
SqliteDbv3_result_null:procedure(context:pointer); cdecl;
SqliteDbv3_result_text:procedure(context:pointer; value:pAnsiChar; len:integer;
                                 xDel:integer); cdecl;
SqliteDbv3_result_text16:procedure(context:pointer; value:pWideChar; len:integer;
                                 xDel:integer); cdecl;
SqliteDbv3_result_value:procedure(context:pointer; value:PSqliteValue); cdecl;

{ Sort functions }
SqliteDbv3_create_collation:function(db:pointer;
  zName:pChar;
  eTextRep:integer;
  userData:pointer;
  func:TUserCollation):integer; cdecl;

SqliteDbv3_collation_needed:function(db:pointer;
  userData:pointer;
  func:TUserCollationNeeded):integer; cdecl;

{ This function is called by TSqlitePassEngine to load library functions }
procedure LoadFunctions(LibraryHandle: Integer);


implementation

procedure LoadFunctions(LibraryHandle: Integer);

 function LoadFunction(FunctionName: String):Pointer;
  begin
  Result:=GetProcAddress(LibraryHandle,pChar(FunctionName));
  if not Assigned(Result)
     then raise exception.createFmt(Msg3000,[FunctionName]);
  end;

begin
Inc(EnginesCount);
{.$IFDEF WIN32}
@SqliteDbv3_SqliteLibVersion     := LoadFunction('sqlite3_libversion');
{.$ENDIF}
@SqliteDbv3_SqliteLibVersionNumber := LoadFunction('sqlite3_libversion_number');
{$IFDEF WIN32}
@SqliteDbv3_SqliteLibSourceId    := LoadFunction('sqlite3_sourceid');
@SqliteDbv3_compileoption_used   := LoadFunction('sqlite3_compileoption_used');
{$ENDIF}
@SqliteDbv3_close                := LoadFunction('sqlite3_close');
@SqliteDbv3_exec                 := LoadFunction('sqlite3_exec');
@SqliteDbv3_sql                  := LoadFunction('sqlite3_sql');
@SqliteDbv3_last_insert_rowid    := LoadFunction('sqlite3_last_insert_rowid');
@SqliteDbv3_changes              := LoadFunction('sqlite3_changes');
@SqliteDbv3_total_changes        := LoadFunction('sqlite3_total_changes');
@SqliteDbv3_interrupt            := LoadFunction('sqlite3_interrupt');
@SqliteDbv3_complete             := LoadFunction('sqlite3_complete');
@SqliteDbv3_busy_handler         := LoadFunction('sqlite3_busy_handler');
@SqliteDbv3_busy_timeout         := LoadFunction('sqlite3_busy_timeout');
@SqliteDbv3_free                 := LoadFunction('sqlite3_free');
@SqliteDbv3_open                 := LoadFunction('sqlite3_open');
@SqliteDbv3_open16               := LoadFunction('sqlite3_open16');
@SqliteDbv3_errcode              := LoadFunction('sqlite3_errcode');
@SqliteDbv3_errmsg               := LoadFunction('sqlite3_errmsg');
@SqliteDbv3_errmsg16             := LoadFunction('sqlite3_errmsg16');

@SqliteDbv3_prepare_v2           := LoadFunction('sqlite3_prepare_v2');
@SqliteDbv3_prepare16_v2         := LoadFunction('sqlite3_prepare16_v2');

@SqliteDbv3_get_table            := LoadFunction('sqlite3_get_table');
@SqliteDbv3_free_table           := LoadFunction('sqlite3_free_table');
{ binding values to parameters }
@SqliteDbv3_clear_binding        := LoadFunction('sqlite3_clear_bindings');
@SqliteDbv3_bind_parameter_index := LoadFunction('sqlite3_bind_parameter_index');
@SqliteDbv3_bind_double          := LoadFunction('sqlite3_bind_double');
@SqliteDbv3_bind_int             := LoadFunction('sqlite3_bind_int');
@SqliteDbv3_bind_int64           := LoadFunction('sqlite3_bind_int64');
@SqliteDbv3_bind_null            := LoadFunction('sqlite3_bind_null');
//@SqliteDbv3_bind_value:= LoadFunction('sqlite3_bind_value');
@SqliteDbv3_bind_text            := LoadFunction('sqlite3_bind_text');
@SqliteDbv3_bind_text16          := LoadFunction('sqlite3_bind_text16');
@SqliteDbv3_bind_blob            := LoadFunction('sqlite3_bind_blob');

@SqliteDbv3_column_count         := LoadFunction('sqlite3_column_count');
@SqliteDbv3_column_name          := LoadFunction('sqlite3_column_name');
@SqliteDbv3_column_name16        := LoadFunction('sqlite3_column_name16');
@SqliteDbv3_column_decltype      := LoadFunction('sqlite3_column_decltype');
@SqliteDbv3_column_decltype16    := LoadFunction('sqlite3_column_decltype16');
@SqliteDbv3_step                 := LoadFunction('sqlite3_step');
@SqliteDbv3_data_count           := LoadFunction('sqlite3_data_count');
@SqliteDbv3_column_blob          := LoadFunction('sqlite3_column_blob');
@SqliteDbv3_column_bytes         := LoadFunction('sqlite3_column_bytes');
@SqliteDbv3_column_double        := LoadFunction('sqlite3_column_double');
@SqliteDbv3_column_int           := LoadFunction('sqlite3_column_int');
@SqliteDbv3_column_int64         := LoadFunction('sqlite3_column_int64');
@SqliteDbv3_column_text          := LoadFunction('sqlite3_column_text');
@SqliteDbv3_column_text16        := LoadFunction('sqlite3_column_text16');
@SqliteDbv3_column_type          := LoadFunction('sqlite3_column_type');

{ This API is only available if the library was compiled with
  the SQLITE_ENABLE_COLUMN_METADATA preprocessor symbol defined. }
@SqliteDbv3_column_origin_name   := LoadFunction('sqlite3_column_origin_name');
@SqliteDbv3_column_origin_name16 := LoadFunction('sqlite3_column_origin_name16');
@SqliteDbv3_column_table_name    := LoadFunction('sqlite3_column_table_name');
@SqliteDbv3_column_table_name16  := LoadFunction('sqlite3_column_table_name16');

@SqliteDbv3_finalize             := LoadFunction('sqlite3_finalize');
@SqliteDbv3_reset                := LoadFunction('sqlite3_reset');
@SqliteDbv3_create_function      := LoadFunction('sqlite3_create_function');
@SqliteDbv3_create_function16    := LoadFunction('sqlite3_create_function16');
@SqliteDbv3_aggregate_count      := LoadFunction('sqlite3_aggregate_count');
@SqliteDbv3_value_blob           := LoadFunction('sqlite3_value_blob');
@SqliteDbv3_value_bytes          := LoadFunction('sqlite3_value_bytes');
@SqliteDbv3_value_double         := LoadFunction('sqlite3_value_double');
@SqliteDbv3_value_int            := LoadFunction('sqlite3_value_int');
@SqliteDbv3_value_int64          := LoadFunction('sqlite3_value_int64');
@SqliteDbv3_value_text           := LoadFunction('sqlite3_value_text');
@SqliteDbv3_value_text16         := LoadFunction('sqlite3_value_text16');
@SqliteDbv3_value_type           := LoadFunction('sqlite3_value_type');
@SqliteDbv3_aggregate_context    := LoadFunction('sqlite3_aggregate_context');
@SqliteDbv3_user_data            := LoadFunction('sqlite3_user_data');
@SqliteDbv3_get_auxdata          := LoadFunction('sqlite3_get_auxdata');
@SqliteDbv3_set_auxdata          := LoadFunction('sqlite3_set_auxdata');
@SqliteDbv3_result_blob          := LoadFunction('sqlite3_result_blob');
@SqliteDbv3_result_double        := LoadFunction('sqlite3_result_double');
@SqliteDbv3_result_error         := LoadFunction('sqlite3_result_error');
@SqliteDbv3_result_int           := LoadFunction('sqlite3_result_int');
@SqliteDbv3_result_int64         := LoadFunction('sqlite3_result_int64');
@SqliteDbv3_result_null          := LoadFunction('sqlite3_result_null');
@SqliteDbv3_result_text          := LoadFunction('sqlite3_result_text');
@SqliteDbv3_result_text16          := LoadFunction('sqlite3_result_text16');
@SqliteDbv3_result_value         := LoadFunction('sqlite3_result_value');
@SqliteDbv3_create_collation     := LoadFunction('sqlite3_create_collation');
@SqliteDbv3_collation_needed     := LoadFunction('sqlite3_collation_needed');
@SqliteDbv3_bind_parameter_count := LoadFunction('sqlite3_bind_parameter_count');
@SqliteDbv3_bind_parameter_name  := LoadFunction('sqlite3_bind_parameter_name');
@SqliteDbv3_bind_parameter_index := LoadFunction('sqlite3_bind_parameter_index');
end;

initialization
{ Initialize a counter to keep the library loaded while one or more engine(s) are using it }
EnginesCount := 0;
end.





