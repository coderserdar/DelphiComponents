// To enable debugging remove the dot. Do NOT forget to re-insert before
// deploying to production since this feature will slow down this component
// significantly
{.$DEFINE DEBUG_ENABLED } // Enables Debug information
 {.$DEFINE DEBUG_VERY_LOUD}
 {.$DEFINE DEBUG_LOUD}

// Disable this for ignoring IProvider interface (for D4)
{$DEFINE IPROVIDER}

// enable this if you want to link the SQLite library statically. (No need for dll)
// DON'T FORGET TO APPLY CORRECT OBJ VERSION IN THIS SOURCE ON LINE {$L 'OBJ\SQLite<<somerversion>>.obj'}
{.$DEFINE SQLite_Static}

{$I asqlite_def.inc}

unit ASGSQLite3;
{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Albert Drent
Description:  SQLite 3 DataSet class (encapsulates the Delphi DataSet Class)
Target:       Delphi 4, 5, 6 and 7; Delphi 2005, 2006, 2007;
              Borland C++ 5, 6, 2007
Creation:     November 2003
Version:      2008.06.D beta
EMail:        a.drent@aducom.com (www.aducom.com, www.aducom.nl, www.aducomportal.nl)
Support:      support@aducom.com (supportforum on www.aducom.com)
              Please post any questions, remarks etc. to the support forum. We
              useually answer questions within days, mostly hours. 
              Unsollicited mail to support will be intercepted by our spamfilters
              and probabely never be heard of. 
Legal issues: Copyright (C) 2003..2008 by Aducom Software

              Aducom Software
              Eckhartstr 61
              9746 BN  Groningen
              Netherlands

              (For those who like to send a postcard, my kids love the foreign stamps)

              Open Source licence (BSD: http://www.opensource.org/licenses/bsd-license.php)

              Copyright (c) 2003..2008, Aducom Software
              All rights reserved.

              Redistribution and use in source and binary forms, with or without modification,
              are permitted provided that the following conditions are met:

              Redistributions of source code must retain the above copyright notice,
              this list of conditions and the following disclaimer.
              Redistributions in binary form must reproduce the above copyright notice,
              this list of conditions and the following disclaimer in the documentation
              and/or other materials provided with the distribution.
              Neither the name of Aducom Software nor the names of its contributors
              may be used to endorse or promote products derived from this software
              without specific prior written permission.
              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
              TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
              COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
              INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
              DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
              GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
              HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
              STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
              IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
              POSSIBILITY OF SUCH DAMAGE.


Acknowledgement
              These components were written for our own needs. Since SQLite is
              a freeware component we like to donate this one to the community
              too. Parts of the code is adapted from several sources, but mainly
              from a sample and the vcl sources of Borland itself. And, of
              course, we did a lot and still are...
To Do
              A lot...(?)
              We are very busy, but will develop on our needs. If anyone can
              contribute, please feel welcome. Alter the source with lots of comments
              and mail it to me. If it works right I will add it to the official
              source and add your credit here below. Before you start, please
              put a request on the forum. It would be a shame and a waste of your
              time if you develop something which already is... and I need to set
              the spamfilter right to let you pass through.
History:
              See releasenotes.txt

*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }


interface

uses
  DB,
  DBCommon,
  Dialogs,
  Classes,
  Windows,
  SysUtils,
{$IFDEF ASQLITE_D6PLUS}
  SqlTimSt,
  Variants,
{$ENDIF}
  ASGRout3;
const
  SQLiteVersion     = 'ASGSQLite V2008.06.D beta';
  MAX_FIELDS        = 2048;

  MaxBuf            = 30000;            // max stringbuffer for record (length) (excluding blob's)
  SQLITE_OK         = 0;                // Successful result */
  SQLITE_ERROR      = 1;                // SQL error or missing database */
  SQLITE_INTERNAL   = 2;                // An internal logic error in SQLite */
  SQLITE_PERM       = 3;                // Access permission denied */
  SQLITE_ABORT      = 4;                // Callback routine requested an abort */
  SQLITE_BUSY       = 5;                // The database file is locked */
  SQLITE_LOCKED     = 6;                // A table in the database is locked */
  SQLITE_NOMEM      = 7;                // A malloc() failed */
  SQLITE_READONLY   = 8;                // Attempt to write a readonly database */
  SQLITE_INTERRUPT  = 9;                // Operation terminated by sqlite_interrupt() */
  SQLITE_IOERR      = 10;               // Some kind of disk I/O error occurred */
  SQLITE_CORRUPT    = 11;               // The database disk image is malformed */
  SQLITE_NOTFOUND   = 12;               // (Internal Only) Table or record not found */
  SQLITE_FULL       = 13;               // Insertion failed because database is full */
  SQLITE_CANTOPEN   = 14;               // Unable to open the database file */
  SQLITE_PROTOCOL   = 15;               // Database lock protocol error */
  SQLITE_EMPTY      = 16;               // (Internal Only) Database table is empty */
  SQLITE_SCHEMA     = 17;               // The database schema changed */
  SQLITE_TOOBIG     = 18;               // Too much data for one row of a table */
  SQLITE_CONSTRAINT = 19;               // Abort due to contraint violation */
  SQLITE_MISMATCH   = 20;               // Data type mismatch */
  SQLITE_MISUSE     = 21;               // Library used incorrectly */
  SQLITE_NOLFS      = 22;               // Uses OS features not supported on host */
  SQLITE_AUTH       = 23;               // Authorization denied */
  SQLITE_ROW        = 100;              // sqlite_step() has another row ready */
  SQLITE_DONE       = 101;              // sqlite_step() has finished executing */

  SQLITE_CREATE_INDEX = 1;              // Index Name      Table Name      */
  SQLITE_CREATE_TABLE = 2;              // Table Name      NULL            */
  SQLITE_CREATE_TEMP_INDEX = 3;         // Index Name      Table Name      */
  SQLITE_CREATE_TEMP_TABLE = 4;         // Table Name      NULL            */
  SQLITE_CREATE_TEMP_TRIGGER = 5;       // Trigger Name    Table Name      */
  SQLITE_CREATE_TEMP_VIEW = 6;          // View Name       NULL            */
  SQLITE_CREATE_TRIGGER = 7;            // Trigger Name    Table Name      */
  SQLITE_CREATE_VIEW = 8;               // View Name       NULL            */
  SQLITE_DELETE     = 9;                // Table Name      NULL            */
  SQLITE_DROP_INDEX = 10;               // Index Name      Table Name      */
  SQLITE_DROP_TABLE = 11;               // Table Name      NULL            */
  SQLITE_DROP_TEMP_INDEX = 12;          // Index Name      Table Name      */
  SQLITE_DROP_TEMP_TABLE = 13;          // Table Name      NULL            */
  SQLITE_DROP_TEMP_TRIGGER = 14;        // Trigger Name    Table Name      */
  SQLITE_DROP_TEMP_VIEW = 15;           // View Name       NULL            */
  SQLITE_DROP_TRIGGER = 16;             // Trigger Name    Table Name      */
  SQLITE_DROP_VIEW  = 17;               // View Name       NULL            */
  SQLITE_INSERT     = 18;               // Table Name      NULL            */
  SQLITE_PRAGMA     = 19;               // Pragma Name     1st arg or NULL */
  SQLITE_READ       = 20;               // Table Name      Column Name     */
  SQLITE_SELECT     = 21;               // NULL            NULL            */
  SQLITE_TRANSACTION = 22;              // NULL            NULL            */
  SQLITE_UPDATE     = 23;               // Table Name      Column Name     */
  SQLITE_ATTACH     = 24;               // Filename        NULL            */
  SQLITE_DETACH     = 25;               // Database Name   NULL            */

  SQLITE_DENY       = 1;                // Abort the SQL statement with an error */
  SQLITE_IGNORE     = 2;                // Don't allow access, but don't generate an error */

  SQLITE_STATIC      =pointer(0);
  SQLITE_TRANSIENT   =pointer(-1);

  Crlf              : string = #13#10;
  Q                 = '''';

type
  pInteger = ^integer;
  pPointer = ^Pointer;
  pSmallInt = ^smallint;
  pFloat = ^extended;
  pBoolean = ^boolean;


  TConvertBuffer = array[1..255] of char;

  TSQLite3_Callback = function(UserData: Pointer; ColumnCount: Integer; ColumnValues, ColumnNames: PPointer): Integer; cdecl;
//  TSQLiteExecCallback = function(Sender: TObject; Columns: integer; ColumnValues: Pointer; ColumnNames: Pointer): integer of object; cdecl;
  TSQLiteBusyCallback = function(Sender: TObject; ObjectName: PAnsiChar; BusyCount: integer): integer of object; cdecl;
  TOnData = procedure(Sender: TObject; Columns: integer; ColumnNames, ColumnValues: string) of object;
  TOnBusy = procedure(Sender: TObject; ObjectName: string; BusyCount: integer; var Cancel: boolean) of object;
  TOnQueryComplete = procedure(Sender: TObject) of object;
  TASQLite3NotifyEvent = procedure(Sender: TObject) of object;

  // structure for holding field information. It is used by GetTableInfo

  TASQLite3Field = class
  public
    FieldNumber: integer;
    FieldName: string;
    FieldType: string;
    FieldNN: integer;                   // 1 if notnull
    FieldDefault: string;
    FieldPK: integer;                   // 1 if primary key
  end;

  // object to 'play' with SQLite's default settings

  TASQLite3Pragma = class(TComponent)
  private
    FTempCacheSize: integer;
    FDefaultCacheSize: integer;
    FDefaultSynchronous: string;
    FDefaultTempStore: string;
    FTempStore: string;
    FSynchronous: string;
  protected
    function GetTempCacheSize: string;
    function GetDefaultCacheSize: string;
    function GetDefaultSynchronous: string;
    function GetDefaultTempStore: string;
    function GetTempStore: string;
    function GetSynchronous: string;
  published
    { Published declarations }
    property TempCacheSize: integer read FTempCacheSize write FTempCacheSize;
    property DefaultCacheSize: integer read FDefaultCacheSize write FDefaultCacheSize;
    property DefaultSynchronous: string read FDefaultSynchronous
      write FDefaultSynchronous;
    property DefaultTempStore: string read FDefaultTempStore write FDefaultTempStore;
    property TempStore: string read FTempStore write FTempStore;
    property Synchronous: string read FSynchronous write FSynchronous;
  end;

  // component to log messages
  // it's for debugging purpose and may be obsolete due
  // to the event implementation. not sure yet...

  TASQLite3Log = class(TComponent)
  private
    FLogFile: string;
    FLogDebugOut: boolean;
    FAppend: boolean;
    FLogSQL: boolean;
    FLogInt: boolean;
  protected
  public
    procedure Display(Msg: string);
  published
    { Published declarations }
    property LogFile: string read FLogFile write FLogFile;
    property LogDebugOut: boolean read FLogDebugOut write FLogDebugOut; // 20040225
    property Append: boolean read FAppend write FAppend;
    property LogSQL: boolean read FLogSQL write FLogSQL;
    property LogInternals: boolean read FLogInt write FLogInt;
  end;

// This component can be used to store sql outside the pascal source.
// It is useful for automatically creating tables on open of a temporary database
// (i.e. in-memory database)

  TASQLite3InlineSQL = class(TComponent)
  private
    FSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
    function GetSQL: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SQL: TStrings read GetSQL write SetSQL;
  end;

  { Basic Database component }
  TASQLite3DB = class;
  TCompareFunc = function(db : TASQLite3DB; s1Len : integer; s1 : PAnsiChar; s2Len : integer; s2 : PAnsiChar) : integer; cdecl;

  TASQLite3DB = class(TComponent)
  private
    { Private declarations }
    FAfterConnect: TASQLite3NotifyEvent;
    FBeforeConnect: TASQLite3NotifyEvent;
    FAfterDisconnect: TASQLite3NotifyEvent;
    FBeforeDisconnect: TASQLite3NotifyEvent;
    FOnBusy: TASQLite3NotifyEvent;
    function FGetDefaultExt: string;
    function FGetDriverDLL: string;
  protected
    { Protected declarations }
    FInlineSQL: TASQLite3InlineSQL;
    FExecuteInlineSQL: boolean;
    FDatabase: string;
    FTransactionType: string;
    FInTransaction : boolean;
    FSQLiteVersion: string;
    FDefaultExt: string;
    FDefaultDir: string;
    FDriverDll: string;
    FConnected: boolean;
    FMustExist: boolean;
    FTimeOut : integer;
    FVersion: string;
    FCharEnc: string;
    FSQL : string;
    FUtf8: boolean;
    DBHandle: Pointer;
    FASQLitePragma: TASQLite3Pragma;
    FASQLiteLog: TASQLite3Log;
    FLastError: string;
    SQLite3_Open: function(dbname: PAnsiChar; var db: pointer): integer; cdecl;
    SQLite3_Close: function(db: pointer): integer; cdecl;
    SQLite3_Exec: function(DB: Pointer; SQLStatement: PAnsiChar; Callback: TSQLite3_Callback;
                           UserDate: Pointer; var ErrMsg: PAnsiChar): Integer; cdecl;
    SQLite3_LibVersion: function(): PAnsiChar; cdecl;
    SQLite3_ErrorString: function(db: pointer): PAnsiChar; cdecl;
    SQLite3_GetTable: function(db: Pointer; SQLStatement: PAnsiChar; var ResultPtr: Pointer;
      var RowCount: cardinal; var ColCount: cardinal; var ErrMsg: PAnsiChar): integer; cdecl;
    SQLite3_FreeTable: procedure(Table: PAnsiChar); cdecl;
    SQLite3_FreeMem: procedure(P: PAnsiChar); cdecl;
    SQLite3_Complete: function(P: PAnsiChar): boolean; cdecl;
    SQLite3_LastInsertRow: function(db: Pointer): integer; cdecl;
    SQLite3_Cancel: procedure(db: Pointer); cdecl;
    SQLite3_BusyHandler: procedure(db: Pointer; CallbackPtr: Pointer; Sender: TObject); cdecl;
    SQLite3_BusyTimeout: procedure(db: Pointer; TimeOut: integer); cdecl;
    SQLite3_Changes: function(db: Pointer): integer; cdecl;
    SQLite3_Prepare: function(db: Pointer; SQLStatement: PAnsiChar; nBytes: integer;
      var hstatement: pointer; var Tail: PAnsiChar): integer; cdecl;
    SQLite3_Finalize: function(hstatement: pointer): integer; cdecl;
    SQLite3_Reset: function(hstatement: pointer): integer; cdecl;
    SQLite3_Step: function(hstatement: pointer): integer; cdecl;
    SQLite3_Column_blob: function(hstatement: pointer; iCol: integer): pointer; cdecl;
    SQLite3_Column_bytes: function(hstatement: pointer; iCol: integer): integer; cdecl;
    SQLite3_Column_bytes16: function(hstatement: pointer; iCol: integer): integer; cdecl;
    SQLite3_Column_count: function(hstatement: pointer): integer; cdecl;
    SQLite3_Column_decltype: function(hstatement: pointer; iCol: integer): PAnsiChar; cdecl;
    SQLite3_Column_double: function(hstatement: pointer; iCol: integer): double; cdecl;
    SQLite3_Column_int: function(hstatement: pointer; iCol: integer): integer; cdecl;
    SQLite3_Column_int64: function(hstatement: pointer; iCol: integer): int64; cdecl;
    SQLite3_Column_name: function(hstatement: pointer; iCol: integer): PAnsiChar; cdecl;
    SQLite3_Column_text: function(hstatement: pointer; iCol: integer): PAnsiChar; cdecl;
    SQLite3_Column_text16: function(hstatement: pointer; iCol: integer): PWideChar; cdecl;
    SQLite3_Column_type: function(hstatement: pointer; iCol: integer): integer; cdecl;
    SQLite3_Bind_Null: function(hstatement: pointer; iCol: integer): integer; cdecl;
    SQLite3_Bind_Blob: function(hstatement: pointer; iCol: integer; buf: PAnsiChar; n: integer; DestroyPtr: Pointer): integer; cdecl;
    SQLite3_Bind_Int: function(hstatement: pointer; iCol: integer; n: integer): integer; cdecl;
    SQLite3_Bind_Double: function(hstatement: pointer; iCol: integer; d: double): integer; cdecl;
    SQLite3_Bind_Text: function(hstatement: pointer; iCol: integer; buf: pointer; n: integer; DestroyPtr: Pointer): integer; cdecl;
    SQLite3_Bind_Value: function(hstatement: pointer; iCol: integer; buf: pointer): integer; cdecl;
    SQLite3_Bind_Text16: function(hstatement: pointer; iCol: integer; buf: pointer; n: integer; DestroyPtr: Pointer): integer; cdecl;
    SQLite3_Bind_Parameter_Count: function(hstatement: pointer): integer; cdecl;
    SQLite3_Bind_Parameter_Name : function(hstatement : pointer; iCol : integer) : PAnsiChar; cdecl;
    SQLite3_create_collation: function(db: Pointer; zName : PChar; pref16 : integer; data : pointer; cmp : TCompareFunc): integer; cdecl;//\\\
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DBConnect(Connected: boolean);
    function SQLite3_PrepareResult(DB: Pointer; TheStatement: string; Sender: TObject) : pointer;
    function SQLite3_GetNextResult(DB: Pointer; TheStatement: pointer; Sender: TObject) : pointer;
    procedure SQLite3_CloseResult(TheStatement : pointer);
    procedure SetTimeOut(const Value: integer); // sean
  public
    DLLHandle: THandle;
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadLibs: boolean;
    procedure FSetDatabase(Database: string);
    function RowsAffected: integer;
    function  TableExists(const ATableName: AnsiString): Boolean;
    procedure ExecStartTransaction(TransType: string);
    procedure StartTransaction;
    procedure StartDeferredTransaction;
    procedure StartImmediateTransaction;
    procedure StartExclusiveTransaction;
    procedure Open;
    procedure Close;
    procedure Commit;
    procedure RollBack;
    procedure Vacuum;
    procedure ShowDatabases(List: TStrings);
    procedure GetTableNames(List: TStrings; SystemTables: boolean = false; TempTables: boolean = false);
    procedure GetTableInfo(TableName: string; List: TList);
    procedure GetIndexNames(List: TStrings; SystemTables: boolean = false);
    procedure GetIndexFieldNames(IndexName: string; List: TStrings);
    procedure GetFieldNames(TableName: string; List: TStrings);
    procedure GetPrimaryKeys(TableName: string; List: TStrings);
    procedure GetTableIndexNames(TableName: string; List: TStrings);
    procedure ExecPragma;
//    function SQLite_XExec(db: Pointer; SQLStatement: PAnsiChar;
//      CallbackPtr: Pointer; Sender: TObject; var ErrMsg: PAnsiChar): integer; cdecl;
    function SQLite3_Execute(db: Pointer; TheStatement: string; Params: TParams; Sender: TObject): integer;
    function SQLite3_ExecSQL(TheStatement: string; Fields: TFields=nil): integer;
    function SQLite3_ExecSQL_Params(TheStatement: string; Params: TParams=nil): integer;
    procedure ShowError;
    function GetUserVersion(database : string=''): integer;
    procedure SetUserVersion(Version : integer; Database : string='');
    function GetSchemaVersion(database : string=''): integer;
    procedure SetSchemaVersion(Version : integer; Database : string='');
    function InTransaction : boolean;
  published
    { Published declarations }
    property TimeOut: integer read FTimeOut write SetTimeOut;
    property CharacterEncoding: string read FCharEnc write FCharEnc;
    property TransactionType: string read FTransactionType write FTransactionType;
    property Database: string read FDatabase write FSetDatabase;
    property ASQLitePragma: TASQLite3Pragma read FASQLitePragma write FASQLitePragma;
    property ASQLiteLog: TASQLite3Log read FASQLiteLog write FASQLiteLog;
    property DefaultExt: string read FGetDefaultExt write FDefaultExt;
    property DefaultDir: string read FDefaultDir write FDefaultDir;
    property Version: string read FVersion write FVersion;
//  property CharacterEncoding: string Read FCharEncoding Write FCharEncoding;
    property DriverDLL: string read FGetDriverDLL write FDriverDLL;
    property Connected: boolean read FConnected write DBConnect;
    property MustExist: boolean read FMustExist write FMustExist;
    property ASQLiteInlineSQL: TASQLite3InlineSQL read FInlineSQL write FInlineSQL;
    property ExecuteInlineSQL: boolean read FExecuteInlineSQL write FExecuteInlineSQL;
    property AfterConnect: TASQLite3NotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeConnect: TASQLite3NotifyEvent read FBeforeConnect write FBeforeConnect;
    property AfterDisconnect: TASQLite3NotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property BeforeDisconnect: TASQLite3NotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property OnBusy : TASQLite3NotifyEvent read FOnBusy write FOnBusy;
  end;

  AsgError = class(Exception);

{ TRecInfo }

{   This structure is used to access additional information stored in
  each record buffer which follows the actual record data.

    Buffer: PAnsiChar;
   ||
   \/
    -------------------------------------------------
    |NULL|  Record Data  | Bookmark | Bookmark Flag |
    -------------------------------------------------
                    ^-- PRecInfo = Buffer + FRecInfoOfs

  Keep in mind that this is just an example of how the record buffer
  can be used to store additional information besides the actual record
  data.  There is no requirement that TDataSet implementations do it this
  way.

  For the purposes of this demo, the bookmark format used is just an integer
  value.  For an actual implementation the bookmark would most likely be
  a native bookmark type (as with BDE), or a fabricated bookmark for
  data providers which do not natively support bookmarks (this might be
  a variant array of key values for instance).

  The BookmarkFlag is used to determine if the record buffer contains a
  valid bookmark and has special values for when the dataset is positioned
  on the "cracks" at BOF and EOF. }

  PRecInfo = ^TRecInfo;

  TRecInfo = packed record
    Bookmark : integer;
    BookmarkFlag : TBookmarkFlag;
//    Nulls :
  end;

 //============================================================================== TFResult
 // The TFResult class is used to maintain the resultlist in memory. This
 // will only be the case for 'normal' data. Blobs and Clobs will be treated
 // differently, but they are not supported yet.
 //==============================================================================
  TASQLite3BaseQuery = class;

  TFResult = class
  protected
    Data: TList;
    BookMark: TList;
    RowId: TList;
    FLastBookmark: integer;
    FBufSize: integer;
    FDataSet: TASQLite3BaseQuery;
  public
    constructor Create(TheDataSet: TASQLite3BaseQuery);
    destructor Destroy; override;
    procedure FreeBlobs;
    procedure SetBufSize(TheSize: integer);
    procedure Add(TheBuffer: PAnsiChar; TheRowId: integer);
    procedure Insert(Index: integer; TheBuffer: Pointer; TheRowId: integer);
    procedure Delete(Index: integer);
    function GetData(Index: integer): Pointer;
    function Count: integer;
    function IndexOf(TheBookMark: pointer): integer;
    function GetBookmark(Index: integer): integer;
    function GetRowId(Index: integer): integer;
  end;

//============================================================================== TASQLite3UpdateSQL
  TASQLite3UpdateSQL = class(TComponent)
  private
    FInsertSQL: TStrings;
    FUpdateSQL: TStrings;
    FDeleteSQL: TStrings;
    procedure SetInsertSQL(const Value: TStrings);
    procedure SetUpdateSQL(const Value: TStrings);
    procedure SetDeleteSQL(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property InsertSQL: TStrings read FInsertSQL write SetInsertSQL;
    property UpdateSQL: TStrings read FUpdateSQL write SetUpdateSQL;
    property DeleteSQL: TStrings read FDeleteSQL write SetDeleteSQL;
  end;

//============================================================================== TASQLite3Output

  TASQLite3Output = class(TComponent)
  private
    FActive: boolean;
    FOutputType: string;
    FTableClass: string;
    FHeaderClass: string;
    FCellClass: string;
    FOutput: TStrings;
    FSeparator: string;
    FDataSource: TDataSource;
    procedure SetOutput(const Value: TStrings);
    procedure SetFActive(Active: boolean);
    function GetOutput: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(MyDataSet: TDataSet);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Active: boolean read FActive write SetFActive;
    property DataSource: TDataSource read FDataSource write FDataSource;
    property OutputType: string read FOutputType write FOutputType;
    property TableClass: string read FTableClass write FTableClass;
    property HeaderClass: string read FHeaderClass write FHeaderClass;
    property CellClass: string read FCellClass write FCellClass;
    property Output: TStrings read GetOutput write SetOutput;
    property FieldSeparator: string read FSeparator write FSeparator;
  end;

//============================================================================== TASQLite3BaseQuery
  TASQLite3BaseQuery = class(TDataSet)
  private
//    FOrderBy : string;
    FParams: TParams;
    FTypeLess: boolean;
    FNoResults: boolean;                // suppresses the creation of a result list
    FAutoCommit: boolean;
    FTransactionType: string;
    FTableDateFormat: string;
    FSQLiteDateFormat: boolean;
    FResult: TFResult;
    FSQL: TStrings;
    FSQLCursor: boolean;
    FPrepared: string;
    FRecBufSize: integer;
    FRecInfoOfs: integer;
    FCurRec: integer;
    FMasterFields: string;
    FMasterSource: TDataSource;
    FSaveChanges: boolean;
    MaxStrLen: integer;
    FConnection: TASQLite3DB;
    FReadOnly: boolean;
    FMaxResults: integer;
    FStartResult: integer;
    FUniDir : boolean;
    FStatement : pointer;
    CurrentRowId: integer;
    SQLStr: string;
    ResultStr: PAnsiChar;
    RowId : integer;
    RowIdCol : integer;
    DetailList: TList;
    procedure SetSQL(const Value: TStrings);
//    function UnpackBuffer(Buffer: PAnsiChar; FieldType: TFieldType): TConvertBuffer;
    procedure UnpackBuffer(Buffer: PAnsiChar; FieldType: TFieldType; var RetBuffer : TConvertBuffer);
    procedure SetDataSource(Value: TDataSource);
  protected
    MDFilter : string ; // master-detail filter 2008 Aducom
    function BuildFilter : string;
//    function SetQueryParams(InStr: string): string; //***
    procedure SetParamsList(Value: TParams);
    function GetParamsCount: word;
    procedure RegisterDetailDataset(DetailDataSet: TASQLite3BaseQuery);
    procedure LoadQueryData;
    function GetActiveBuffer(var Buffer: PAnsiChar): boolean;
    function GetDataSource: TDataSource; override;
    procedure NotifySQLiteMasterChanged;
//    function GetFieldValue(const AField: TField; const Blobs: TList = nil): string; // added by Donnie

    { Overriden abstract methods (required) }
    function AllocRecordBuffer: PAnsiChar; override;
    procedure FreeRecordBuffer(var Buffer: PAnsiChar); override;
    procedure GetBookmarkData(Buffer: PAnsiChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PAnsiChar): TBookmarkFlag; override;
    function GetRecord(Buffer: PAnsiChar; GetMode: TGetMode;
      DoCheck: boolean): TGetResult; override;
    function GetRecordSize: word; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: boolean); override;
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PAnsiChar); override;
    procedure InternalRefresh; override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: PAnsiChar); override;
    procedure OpenCursor(InfoQuery: Boolean); override; // GPA
    function IsCursorOpen: boolean; override;
    procedure SetBookmarkFlag(Buffer: PAnsiChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PAnsiChar; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    function GetFieldSize(FieldNo: integer): integer; overload;
    function GetFieldSize(Field: TField): integer; overload;
    function GetNativeFieldSize(FieldNo: integer): integer;
    function GetFieldOffset(FieldNo: integer): integer;
    function GetCalcFieldOffset(Field: TField): integer;
    function isNullSrc(SrcBuffer: pAnsiChar; fieldNo : integer) : boolean; overload;
    function isNullSrc(SrcBuffer: pWideChar; fieldNo : integer) : boolean; overload;
    procedure setNullSrc(SrcBuffer: pAnsiChar; fieldNo : integer; aNull : boolean);
    function GetMasterFields: string;
    procedure SetMasterFields(const Value: string);
    { Additional overrides (optional) }
    function GetRecordCount: integer; override;
    function GetRecNo: integer; override;
    procedure SetRecNo(Value: integer); override;
    property BaseSQL: TStrings read FSQL write SetSQL;
    procedure SetSQLiteDateFormat(const Value: boolean);
    procedure SetFilterText(const Value: string); override;
    procedure DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean); override;//\\\
    function CalcFieldInList(const List: string): Boolean;                      // John Lito

   {$IFDEF IPROVIDER}
   {***** IProviderSupport - Begin *****}
    //-----| These are not necessary until the moment!
    // procedure PSGetAttributes(List: TList); virtual;
    // function PSGetDefaultOrder: TIndexDef; virtual;
    // function PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs; virtual;
    //-----| These are necessary to support IProvider
    procedure PSEndTransaction(Commit: Boolean); override;
    procedure PSExecute; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer = nil): Integer; override;
    function PSGetParams: TParams; override;
    function PSGetTableName: string; override;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; override;
    function PSInTransaction: Boolean; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSReset; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
    procedure PSStartTransaction; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;
    function PSGetQuoteChar: string; override;
    function PSGetKeyFields: string; override;
   {***** IProviderSupport - End *****}
   {$ENDIF}
  public
    FOrderBy : string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecSQL;
    procedure StartTransaction;
    procedure StartDeferredTransaction;
    procedure StartImmediateTransaction;
    procedure StartExclusiveTransaction;
    procedure Commit;
    procedure RollBack;
    procedure SetFiltered(Value: Boolean); override;
    procedure SQLiteMasterChanged; virtual;
    function GetFieldData(Field: TField; Buffer: Pointer): boolean; override;
    function GetFieldData(FieldNo: integer; Buffer: Pointer): boolean; override; // 20040225
    function GetLastInsertRow: integer;
{$IFDEF ASQLITE_D6PLUS}
//    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: boolean): boolean; override;
{$ENDIF}

    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override; //MS
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function Locate(const KeyFields: string; const KeyValues: variant; Options: TLocateOptions): boolean; override;
    function BookmarkValid(Bookmark: Pointer): boolean; override;
//    function    LocateNearest(const KeyFields: String; const KeyValues: Variant; Options: TLocateOptions): Boolean;
    property Params: TParams read FParams write SetParamsList stored false;
    function Lookup(const KeyFields: string; const KeyValues: Variant;          // John Lito
                    const ResultFields: string): Variant; override;             // John Lito
    function GetFieldBufferIndex(Field: TField): integer; // Sean

  published
    property AutoCommit: boolean read FAutoCommit write FAutoCommit default true;
    property TransactionType: string read FTransactionType write FTransactionType;
    property SQLiteDateFormat: boolean read FSQLiteDateFormat write SetSQLiteDateFormat;
    property TableDateFormat: string read FTableDateFormat write FTableDateFormat;
    property Connection: TASQLite3DB read FConnection write FConnection;
    property MaxResults: integer read FMaxResults write FMaxResults;
    property StartResult: integer read FStartResult write FStartResult;
    property TypeLess: boolean read FTypeLess write FTypeLess;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    property MasterSource: TDataSource read GetDataSource write SetDataSource;
    property SQLCursor: boolean read FSQLCursor write FSQLCursor;
    property ReadOnly: boolean read FreadOnly write FReadOnly;
    property UniDirectional : boolean read FUniDir write FUniDir;
    property AutoCalcFields;
    property Filter;
    property Filtered;
    property Active;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
{$IFDEF ASQLITE_D6PLUS}
    property BeforeRefresh;
    property AfterRefresh;
{$ENDIF}
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
  end;
//============================================================================== TASQLite3Query

  TASQLite3Query = class(TASQLite3BaseQuery)
  private
    FUpdateSQL: TASQLite3UpdateSQL;
    FRawSQL: boolean;
    procedure SetSQL(const Value: TStrings);
    function GetSQL: TStrings;
    procedure QueryChanged(Sender: TObject);
  protected
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalDelete; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InternalClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
//    property Params: TParams Read FParams Write SetParamsList Stored false;
    procedure SQLiteMasterChanged; override;
  published
    property RawSQL: boolean read FRawSQL write FRawSQL;
    property SQL: TStrings read GetSQL write SetSQL;
    property UpdateSQL: TASQLite3UpdateSQL read FUpdateSQL write FUpdateSQL;
  end;

//============================================================================== TASQLite3Table

  TASQLite3Table = class(TASQLite3BaseQuery)
  private
    FTableName: string;
//    FOrderBy : string;
    FPrimaryAutoInc: boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: boolean); override;
    procedure InternalDelete; override;
    procedure SetFTableName(TableName : string);
    procedure SetFOrderBy(OrderBy : string);
  public
    procedure SQLiteMasterChanged; override;
  published
    property TableName: string read FTableName write SetFTableName;
    property PrimaryAutoInc: boolean read FPrimaryAutoInc write FPrimaryAutoInc;
    property OrderBy : string read FOrderBy write SetFOrderBy;
  end;

 //============================================================================== TASQLite3BlobStream

  TASQLite3BlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TASQLite3BaseQuery;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    FOpened: Boolean;
    procedure LoadBlobData;
    procedure SaveBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;


implementation

uses
  Math
{$IFDEF ASQLITE_D6PLUS}
  , StrUtils
{$endif}
  ;

function systemNoCaseCompare(db : TASQLite3DB; s1Len : integer; s1 : PAnsiChar; s2Len : integer; s2 : PAnsiChar) : integer; cdecl;
begin
  result := AnsiCompareText(s1, s2);
end;

function systemCompare(db : TASQLite3DB; s1Len : integer; s1 : PAnsiChar; s2Len : integer; s2 : PAnsiChar) : integer; cdecl;
begin
  result := AnsiCompareStr(s1, s2);
end;

// GPA - Static Link Start
{$IFDEF SQLite_Static}
Var
 __HandlerPtr:Pointer;

// Starting from 3.5.1, instead of C modules Memory Manager,can be used independent one
// (Borland's, FastMM4, etc, which is used in customers main project) with staticaly     // linked C modules
// SZ - Please enable static version of SQLite you prefer

{$I \SzUtils\SQLite_OBJs_3_5_1.inc}

  function  _sqlite3_open(dbname: PAnsiChar; var db: pointer): integer; cdecl; external;
  function  _sqlite3_close(db: pointer): integer; cdecl; external;
  function  _sqlite3_exec(DB: Pointer; SQLStatement: PAnsiChar; Callback: TSQLite3_Callback;
                          UserDate: Pointer; var ErrMsg: PAnsiChar): Integer; cdecl; external;
  function  _sqlite3_libversion: PAnsiChar; cdecl; external;
  function  _sqlite3_errmsg(db: pointer): PAnsiChar; cdecl; external;
  function  _sqlite3_get_table(db: Pointer; SQLStatement: PAnsiChar; var ResultPtr: Pointer;
                              var RowCount: cardinal; var ColCount: cardinal; var ErrMsg: PAnsiChar): integer; cdecl; external;
  procedure _sqlite3_free_table(Table: PAnsiChar); cdecl; external;
  procedure _sqlite3_free(P: PAnsiChar); cdecl; external;
  function  _sqlite3_complete(P: PAnsiChar): boolean; cdecl; external;
  function  _sqlite3_last_insert_rowid(db: Pointer): integer; cdecl; external;
  procedure _sqlite3_interrupt(db: Pointer); cdecl; external;
  procedure _sqlite3_busy_handler(db: Pointer; CallbackPtr: Pointer; Sender: TObject); cdecl; external;
  procedure _sqlite3_busy_timeout(db: Pointer; TimeOut: integer); cdecl; external;
  function  _sqlite3_changes(db: Pointer): integer; cdecl; external;
  function  _sqlite3_prepare(db: Pointer; SQLStatement: PAnsiChar; nBytes: integer;
                             var hstatement: pointer; var Tail: PAnsiChar): integer; cdecl; external;
  function  _sqlite3_finalize(hstatement: pointer): integer; cdecl; external;
  function  _sqlite3_reset(hstatement: pointer): integer; cdecl; external;
  function  _sqlite3_step(hstatement: pointer): integer; cdecl; external;
  function  _sqlite3_column_blob(hstatement: pointer; iCol: integer): pointer; cdecl; external;
  function  _sqlite3_column_bytes(hstatement: pointer; iCol: integer): integer; cdecl; external;
  function  _sqlite3_column_bytes16(hstatement: pointer; iCol: integer): integer; cdecl; external;
  function  _sqlite3_column_count(hstatement: pointer): integer; cdecl; external;
  function  _sqlite3_column_decltype(hstatement: pointer; iCol: integer): PAnsiChar; cdecl; external;
  function  _sqlite3_column_double(hstatement: pointer; iCol: integer): double; cdecl; external;
  function  _sqlite3_column_int(hstatement: pointer; iCol: integer): integer; cdecl; external;
  function  _sqlite3_column_int64(hstatement: pointer; iCol: integer): int64; cdecl; external;
  function  _sqlite3_column_name(hstatement: pointer; iCol: integer): PAnsiChar; cdecl; external;
  function  _sqlite3_column_text(hstatement: pointer; iCol: integer): PAnsiChar; cdecl; external;
  function  _sqlite3_column_type(hstatement: pointer; iCol: integer): integer; cdecl; external;
  function  _sqlite3_Bind_Null(hstatement: pointer; iCol: integer): integer; cdecl;
  function  _sqlite3_Bind_Blob(hstatement: pointer; iCol: integer; buf: PAnsiChar; n: integer; DestroyPtr: Pointer): integer; cdecl;
  function  _sqlite3_Bind_Int(hstatement: pointer; iCol: integer; n: integer): integer; cdecl;
  function  _sqlite3_Bind_Double(hstatement: pointer; iCol: integer; d: double): integer; cdecl;
  function  _sqlite3_Bind_Text(hstatement: pointer; iCol: integer; buf: pointer; n: integer; DestroyPtr: Pointer): integer; cdecl;
  function  _sqlite3_Bind_Value(hstatement: pointer; iCol: integer; buf: pointer): integer; cdecl;
  function  _sqlite3_Bind_Text16(hstatement: pointer; iCol: integer; buf: pointer; n: integer; DestroyPtr: Pointer): integer; cdecl;
  function  _sqlite3_Bind_Parameter_Count(hstatement: pointer): integer; cdecl;
  function  _sqlite3_Bind_Parameter_Name(hstatement : pointer; iCol : integer) : PAnsiChar; cdecl;
  function  _sqlite3_create_collation(db: Pointer; zName : PChar; pref16 : integer; data : pointer; cmp : TCompareFunc): integer; cdecl; external;
  function _sqlite3_column_text16(hstatement: pointer; iCol: integer): PWideChar; cdecl; external;
  function _sqlite3_bind_text16(hstatement: pointer; iCol: integer; buf: pointer; n: integer; DestroyPtr: Pointer): integer; cdecl; external;
  function _sqlite3_bind_parameter_count(hstatement: pointer): integer; cdecl; external;

{$ENDIF}
// GPA - Static Link End


{$IFDEF DEBUG_ENABLED}
var
  DebugSpaces       : Integer = 0;
{$ENDIF}

{$IFNDEF ASQLITE_D6PLUS} //Art Register - Function sign not provided in Delphi 5
function Sign(I: Integer) : Integer ;
begin
 if (I > 0) then
   Result := 1
 else
 begin
   if (I < 0) then
     Result := -1
   else
     Result := 0;
 end;
end;
{$ENDIF}

procedure Debug(const S: string);
begin
{$IFDEF DEBUG_ENABLED}
  OutputDebugString(PAnsiChar(StringOfChar(' ', DebugSpaces) + S));
{$ENDIF}
end;

procedure DebugEnter(const S: string);
begin
{$IFDEF DEBUG_ENABLED}
  OutputDebugString(PAnsiChar(StringOfChar(' ', DebugSpaces) + 'Enter ' + S));
  inc(DebugSpaces);
{$ENDIF}
end;

procedure DebugLeave(const S: string);
begin
{$IFDEF DEBUG_ENABLED}
  dec(DebugSpaces);
  OutputDebugString(PAnsiChar(StringOfChar(' ', DebugSpaces) + 'Leave ' + S));
{$ENDIF}
end;

//==============================================================================
// SyntaxCheck. This routine is used to check if words match the sql syntax
//              It is called where sql statements are parsed and generated
//==============================================================================

function SyntaxCheck(LWord, RWord: string): boolean;
begin
  DebugEnter('SyntaxCheck');
  try
    if AnsiCompareText(LWord, RWord) <> 0 then begin
      SyntaxCheck := false;
      raise AsgError.Create('SQL macro syntax error on sql, expected ' + RWord)
    end else
      SyntaxCheck := true;
  finally
    DebugLeave('SyntaxCheck');
  end;
end;

//==============================================================================
// Parse the SQL fielddescription and return the Delphi Field types, length etc.
//==============================================================================

procedure GetFieldInfo(FieldInfo: string; var FieldType: TFieldType;
  var FieldLen, FieldDec: integer);
var
  p1, p2, pn        : integer;
  vt                : string;
begin
  DebugEnter('GetFieldInfo');
  FieldType := ftString;                // just a default;
  FieldLen := 255;
  FieldDec := 0;

  p1 := pos('(', FieldInfo);
  if p1 <> 0 then
  begin
    p2 := pos(')', FieldInfo);
    if p2 <> 0 then
    begin
      vt := LowerCase(Copy(FieldInfo, 1, p1 - 1));
      if (vt = 'varchar') or (vt = 'char') or (vt = 'varchar2') then begin
        FieldType := ftString;
        FieldLen := StrToInt(Copy(FieldInfo, p1 + 1, p2 - p1 - 1));
      end else if (vt = 'nvarchar') or (vt = 'nchar') or (vt = 'nvarchar2') then begin
        FieldType := ftWideString;
        FieldLen := StrToInt(Copy(FieldInfo, p1 + 1, p2 - p1 - 1));
        FieldLen := FieldLen * 2;
      end else if (vt = 'numeric') then begin
        vt := Copy(FieldInfo, p1 + 1, p2 - p1 - 1);
        pn := pos('.', vt); if pn = 0 then pn := pos(',', vt);
        FieldType := ftFloat;
        if pn = 0 then begin
          FieldLen := StrToInt(vt);
          FieldDec := 0;
        end else begin
          FieldLen := StrToInt(Copy(vt, 1, pn - 1));
          FieldDec := StrToInt(Copy(vt, pn + 1, 2));
        end;
      end;
    end
    else
      FieldLen := 256;
  end
  else
  begin
    vt := LowerCase(FieldInfo);
    if vt = 'date' then
    begin
      FieldType := ftDate;
      FieldLen := 10;
    end
    else if vt = 'datetime' then
    begin
      FieldType := ftDateTime;          // fpierce original ftDate
      FieldLen := 24;                   // aducom
    end
    else if vt = 'time' then
    begin
      FieldType := ftTime;
      FieldLen := 12;
    end
{$IFDEF ASQLITE_D6PLUS}
    else if vt = 'timestamp' then
    begin
      FieldType := ftTimeStamp;
      FieldLen := 12;
    end
{$ENDIF}
    else if (vt = 'integer') or (vt = 'int')  then
    begin
      FieldType := ftInteger;
      FieldLen := 12;
    end
    else if (vt = 'float') or (vt = 'real') then
    begin
      FieldType := ftFloat;
      FieldLen := 12;
    end
    else if (vt = 'boolean') or (vt = 'logical') then
    begin
      FieldType := ftBoolean;
      FieldLen := 2;
    end
    else if (vt = 'char') or (vt = 'byte') then
    begin
      FieldType := ftString;
      FieldLen := sizeof(Char);
    end
    else if (vt = 'shorttext') or (vt = 'string') then
    begin
      FieldType := ftString;
      FieldLen := 255;
    end
    else if (vt = 'widetext') or (vt = 'widestring') then
    begin
      FieldType := ftWideString;
      FieldLen := 512;
    end
    else if (vt = 'currency') or (vt = 'financial') or (vt = 'money') then
    begin
      FieldType := ftCurrency;
      FieldLen := 10;
    end
    else if (vt = 'blob') then
    begin
      FieldType := ftBlob;
      FieldLen := SizeOf(Pointer);
    end
    else if (vt = 'graphic') then
    begin
      FieldType := ftGraphic;
      FieldLen := SizeOf(Pointer);
    end
    else if (vt = 'clob') or (vt = 'memo') or (vt = 'text') or (vt = 'longtext') then
    begin
      FieldType := ftMemo;
      FieldLen := SizeOf(Pointer);
    end
    else if (vt = 'nclob') or (vt = 'nmemo') or (vt = 'ntext') or (vt = 'nlongtext') then
    begin
      FieldType := ftWideString;
      FieldLen := 60;
    end;
  end;
  DebugLeave('GetFieldInfo: ' + vt);
end;

 //==============================================================================
 // Convert TDateTime to TDateTimeRec
 //==============================================================================

function DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
var
  TimeStamp         : TTimeStamp;
begin
  DebugEnter('DateTimeToNative');
  TimeStamp := DateTimeToTimeStamp(Data);
  case DataType of
    ftDate: Result.Date := TimeStamp.Date;
    ftTime: Result.Time := TimeStamp.Time;
  else
    Result.DateTime := TimeStampToMSecs(TimeStamp);
  end;
  DebugLeave('DateTimeToNative');
end;

procedure ApplicationHandleException(Sender: TObject);
begin
{$IFDEF ASQLITE_D6PLUS}
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(Sender);
{$ENDIF}
end;

//============================================================================== TASQLite3LOG

procedure TASQLite3Log.Display(Msg: string);
var
  fn                : Textfile;
begin
  DebugEnter('TASQLite3Log.Display');
  if FileExists(FLogFile) then
  begin
    if FAppend then
    begin
      AssignFile(fn, FLogFile);
      System.Append(fn);
    end
    else
    begin
      SysUtils.DeleteFile(FLogFile);
      AssignFile(fn, FLogFile);
      Rewrite(fn);
    end;
  end
  else
  begin
    AssignFile(fn, FLogFile);
    Rewrite(fn);
  end;
  Writeln(fn, FormatDateTime('yyyy mmm dd (hh:nn:ss) ', now) + Msg);
  CloseFile(fn);
  DebugLeave('TASQLite3Log.Display');
end;

//============================================================================== TASQLite3PRAGMA

function TASQLite3Pragma.GetTempCacheSize: string;
begin
  DebugEnter('TASQLite3Pragma.GetTempCacheSize');
  GetTempCacheSize := 'pragma cache_size=' + IntToStr(FTempCacheSize);
  DebugLeave('TASQLite3Pragma.GetTempCacheSize');
end;

function TASQLite3Pragma.GetDefaultCacheSize: string;
begin
  DebugEnter('TASQLite3Pragma.GetDefaultCacheSize');
  GetDefaultCacheSize := 'pragma default_cache_size=' + IntToStr(FDefaultCacheSize);
  DebugLeave('TASQLite3Pragma.GetDefaultCacheSize');
end;

function TASQLite3Pragma.GetDefaultSynchronous: string;
begin
  DebugEnter('TASQLite3Pragma.GetDefaultSynchronous');
  GetDefaultSynchronous := 'pragma default_synchronous=' + FDefaultSynchronous;
  DebugLeave('TASQLite3Pragma.GetDefaultSynchronous');
end;

function TASQLite3Pragma.GetDefaultTempStore: string;
begin
  DebugEnter('TASQLite3Pragma.GetDefaultTempStore');
  GetDefaultTempStore := 'pragma default_temp_store=' + FDefaultTempStore;
  DebugLeave('TASQLite3Pragma.GetDefaultTempStore');
end;

function TASQLite3Pragma.GetTempStore: string;
begin
  DebugEnter('TASQLite3Pragma.GetTempStore');
  GetTempStore := 'pragma temp_store=' + FTempStore;
  DebugLeave('TASQLite3Pragma.GetTempStore');
end;

function TASQLite3Pragma.GetSynchronous: string;
begin
  DebugEnter('TASQLite3Pragma.GetSynchronous');
  GetSynchronous := 'pragma synchronous=' + FSynchronous;
  DebugLeave('TASQLite3Pragma.GetSynchronous');
end;

 //============================================================================== TFRESULT
 // TResult is a representation of an internal pointerlist of results.
 // Only 'normal' results will be stored internally within a fixed memory block
 // depending on calculated length internally. This is not the case
 // for blobs and clobs. In this case only the handle is stored in the fixed
 // structure and a separate memory handle is retrieved to store the blob and
 // clob data. This is because the blobs are stored as null terminated 'strings'
 // and thus have different lengths. No more memory is allocated this way than
 // strictly necessary.
 // KEEP IN MIND: ...
 //     This resultset is NOT used for unidirectional search results. You can
 //     however, open a dbgrid containing griddata. You cannot update your data
 //     if you use the unidirectional the result is read-only by default and cannot
 //     be changed.
 //     The unidirectional dataset improves performance on large resultsets and
 //     is to be considered for query-only components...
 //==============================================================================

constructor TFResult.Create(TheDataSet: TASQLite3BaseQuery);
begin
  DebugEnter('TFResult.Create');
  Data := TList.Create;
  Bookmark := TList.Create;
  RowId := TList.Create;
  FDataSet := TheDataset;
  FLastBookmark := -1;                  // 2004-14-09 (rps) 0 -> -1 (otherwise insert in an empty table gives index out of range)
  DebugLeave('TFResult.Create');
end;

destructor TFResult.Destroy;
var
  ptr               : Pointer;
  i                 : integer;
begin
  DebugEnter('TFResult.Destroy');
  FreeBlobs;
  if Assigned(Data) then begin
    for i := 0 to Data.Count - 1 do begin
      ptr := Data.Items[i];
      if Assigned(ptr) then FreeMem(ptr, FBufSize);
    end;
    Data.Free;                          // D4 compatibility, otherwise FreeAndNil could be used
    Data := nil;
  end;

  if Assigned(Bookmark) then begin
    Bookmark.Free;
    Bookmark := nil;
  end;

  if Assigned(RowId) then begin
    RowId.Free;
    RowId := nil;
  end;

  DebugLeave('TFResult.Destroy');
end;

procedure TFResult.FreeBlobs;
var i, j            : integer;
  offset            : integer;
  ptr               : PAnsiChar;
  stream            : TMemoryStream;
begin
  if not Assigned(FDataSet) then exit;
  if not Assigned(FDataSet.FieldList) then exit;
  for j := 0 to Data.Count - 1 do begin
    ptr := GetData(j);
    for i := 0 to FDataSet.FieldList.Count - 1 do begin
      if FDataSet.FieldList[i].DataType in [ftMemo, ftFmtMemo, ftGraphic, ftBlob, ftVariant] then begin
        Offset := FDataset.GetFieldOffset(FDataSet.FieldList[i].FieldNo);
        Move((ptr + Offset)^, Pointer(Stream), sizeof(Pointer));
        Stream.Free;
      end;
    end;
  end;
end;

procedure TFResult.SetBufSize(TheSize: integer);
begin
  DebugEnter('TFResult.SetBufSize');
  FBufSize := TheSize;
  DebugLeave('TFResult.SetBufSize');
end;

//==============================================================================
// Adds a row of data to the resultset.
//==============================================================================

procedure TFResult.Add(TheBuffer: PAnsiChar; TheRowId: integer);
var
  ptr               : PAnsiChar;
//  i:   integer;
begin
{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TFResult.Add');
{$ENDIF}
  Inc(FLastBookmark);
  GetMem(Ptr, FBufSize);
  move(TheBuffer^, ptr^, FBufSize);
  Data.Add(Ptr);
  Bookmark.Add(Pointer(FLastBookMark));
  if TheRowId >= 0 then
    RowId.Add(Pointer(TheRowId))
  else
    RowId.Add(Pointer(RowId.Count));
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TFResult.Add');
{$ENDIF}
end;

//==============================================================================
// Inserts a row of date into the resultset
//==============================================================================

procedure TFResult.Insert(Index: integer; TheBuffer: pointer; TheRowId: integer);
var
  ptr               : Pointer;
begin
  DebugEnter('TFResult.Insert');
  Inc(FLastBookmark);
  GetMem(Ptr, FBufSize);
  move(TheBuffer^, ptr^, FBufSize);
  if Data.Count < Index then begin
    Data.Add(Ptr);
    Bookmark.Add(Pointer(FLastBookMark));
    RowId.Add(Pointer(TheRowId));
  end else begin
    Data.Insert(Index, Ptr);
    Bookmark.Insert(Index, Pointer(FLastBookMark));
    RowId.Insert(Index, Pointer(TheRowId));
  end;
  DebugLeave('TFResult.Insert');
end;

//==============================================================================
// Deletes a row of data from the resultset
//==============================================================================

procedure TFResult.Delete(Index: integer);
var
  ptr               : pointer;
begin
  DebugEnter('TFResult.Delete');
  if not ((Index < 0) or (Index >= Data.Count)) then
  begin
    ptr := Data.Items[Index];
    if ptr <> nil then
      FreeMem(ptr, FBufSize);
    Data.Delete(Index);
    Bookmark.Delete(Index);
    Rowid.Delete(Index);
  end;
  DebugLeave('TFResult.Delete');
end;

//==============================================================================
// Returns a row from the resultset
//==============================================================================

function TFResult.GetData(Index: integer): Pointer;
begin
  DebugEnter('TFResult.GetData');
  if (Index < 0) or (Index >= Data.Count) then
    GetData := nil
  else
    GetData := Data.Items[Index];
  DebugLeave('TFResult.GetData');
end;

function TFResult.GetBookmark(Index: integer): integer;
begin
  DebugEnter('TFResult.GetBookmark');
  if (Index < 0) or (Index >= Data.Count) then
    GetBookmark := -1
  else
    GetBookmark := integer(Bookmark.Items[Index]);
  DebugLeave('TFResult.GetBookmark');
end;

function TFResult.GetRowId(Index: integer): integer;
begin
  DebugEnter('TFResult.GetRowId');
  if (Index < 0) or (Index >= RowId.Count) then
    GetRowId := -1
  else
    GetRowId := integer(RowId.Items[Index]);
  DebugLeave('TFResult.GetRowId');
end;

function TFResult.Count: integer;
begin
  Count := Data.Count;
end;

function TFResult.IndexOf(TheBookMark: pointer): integer;
begin
  Result := BookMark.IndexOf(TheBookmark);
end;

//============================================================================== ASQLITEDB

procedure TASQLite3DB.Vacuum;
begin
  SQLite3_ExecSQL('VACUUM', nil);
end;

procedure TASQLite3DB.SetTimeOut(const Value: integer);
begin
  FTimeOut := Value;
  if Connected then
     SQLite3_BusyTimeout(dbhandle, Ftimeout);
end;

procedure TASQLite3DB.Notification(AComponent: TComponent; Operation: TOperation);
begin
{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TASQLite3DB.Notification');
{$ENDIF}
//  Application.ProcessMessages;
  if Assigned(AComponent) then
  begin
    if (Operation = opRemove) then
    begin
      if (AComponent is TASQLite3Pragma) then begin
        if Assigned(FASQLitePragma) then begin
          if TASQLite3Pragma(AComponent) = FASQLitePragma then
            FASQLitePragma := nil;
        end;
      end
      else if (AComponent is TASQLite3Log) then
      begin
        if Assigned(FASQLiteLog) then begin
          if TASQLite3Log(AComponent) = FASQLiteLog then
            FASQLiteLog := nil;
        end;
      end
      else if (AComponent is TASQLite3InlineSQL) then
      begin
        if Assigned(FInlineSQL) then begin
          if TASQLite3InlineSQL(AComponent) = FInlineSQL then
            FInlineSQL := nil;
        end;
      end;
    end;
  end;
  inherited;
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TASQLite3DB.Notification');
{$ENDIF}
end;

function TASQLite3DB.LoadLibs: boolean;
begin
  try
    DebugEnter('TASQLite3DB.LoadLibs');
    if not(DecimalSeparator in ['.',',']) then
       DecimalSeparator := '.';

    Debug('loading sqlite lib');
{$IFNDEF SQLite_Static}
    Debug(PAnsiChar(DriverDLL));
    Result := false;
    DLLHandle := LoadLibrary(PAnsiChar(DriverDLL));  //JohnLito
    if DLLHandle <> 0 then
    begin
      @SQLite3_Open := GetProcAddress(DLLHandle, 'sqlite3_open');
      @SQLite3_Close := GetProcAddress(DLLHandle, 'sqlite3_close');
      @SQLite3_Exec := GetProcAddress(DLLHandle, 'sqlite3_exec');
      @SQLite3_LibVersion := GetProcAddress(DLLHandle, 'sqlite3_libversion');
      @SQLite3_ErrorString := GetProcAddress(DLLHandle, 'sqlite3_errmsg');
      @SQLite3_GetTable := GetProcAddress(DLLHandle, 'sqlite3_get_table');
      @SQLite3_FreeTable := GetProcAddress(DLLHandle, 'sqlite3_free_table');
      @SQLite3_FreeMem := GetProcAddress(DLLHandle, 'sqlite3_free');
      @SQLite3_Complete := GetProcAddress(DLLHandle, 'sqlite3_complete');
      @SQLite3_LastInsertRow := GetProcAddress(DLLHandle, 'sqlite3_last_insert_rowid');
      @SQLite3_Cancel := GetProcAddress(DLLHandle, 'sqlite3_interrupt');
      @SQLite3_BusyTimeout := GetProcAddress(DLLHandle, 'sqlite3_busy_timeout');
      @SQLite3_BusyHandler := GetProcAddress(DLLHandle, 'sqlite3_busy_handler');
      @SQLite3_Changes := GetProcAddress(DLLHandle, 'sqlite3_changes');
      @SQLite3_Prepare := GetProcAddress(DLLHandle, 'sqlite3_prepare');
      @SQLite3_Finalize := GetProcAddress(DLLHandle, 'sqlite3_finalize');
      @SQLite3_Reset := GetProcAddress(DLLHandle, 'sqlite3_reset');
      @SQLite3_Step := GetProcAddress(DLLHandle, 'sqlite3_step');
      @SQLite3_Column_blob := GetProcAddress(DLLHandle, 'sqlite3_column_blob');
      @SQLite3_Column_bytes := GetProcAddress(DLLHandle, 'sqlite3_column_bytes');
      @SQLite3_Column_bytes16 := GetProcAddress(DLLHandle, 'sqlite3_column_bytes16');
      @SQLite3_Column_count := GetProcAddress(DLLHandle, 'sqlite3_column_count');
      @SQLite3_Column_decltype := GetProcAddress(DLLHandle, 'sqlite3_column_decltype');
      @SQLite3_Column_double := GetProcAddress(DLLHandle, 'sqlite3_column_double');
      @SQLite3_Column_int := GetProcAddress(DLLHandle, 'sqlite3_column_int');
      @SQLite3_Column_int64 := GetProcAddress(DLLHandle, 'sqlite3_column_int64');
      @SQLite3_Column_name := GetProcAddress(DLLHandle, 'sqlite3_column_name');
      @SQLite3_Column_text := GetProcAddress(DLLHandle, 'sqlite3_column_text');
      @SQLite3_Column_text16 := GetProcAddress(DLLHandle, 'sqlite3_column_text16');
      @SQLite3_Column_type := GetProcAddress(DLLHandle, 'sqlite3_column_type');
      @SQLite3_Bind_Blob := GetProcAddress(DLLHandle, 'sqlite3_bind_blob');
      @SQLite3_Bind_Double:= GetProcAddress(DLLHandle, 'sqlite3_bind_double');
      @SQLite3_Bind_Null := GetProcAddress(DLLHandle, 'sqlite3_bind_null');
      @SQLite3_Bind_Value := GetProcAddress(DLLHandle, 'sqlite3_bind_value');
      @SQLite3_Bind_Int := GetProcAddress(DLLHandle, 'sqlite3_bind_int');
      @SQLite3_Bind_Text := GetProcAddress(DLLHandle, 'sqlite3_bind_text');
      @SQLite3_Bind_Text16 := GetProcAddress(DLLHandle, 'sqlite3_bind_text16');
      @SQLite3_Bind_Parameter_Count := GetProcAddress(DLLHandle, 'sqlite3_bind_parameter_count');
      @SQLite3_Bind_Parameter_Name := GetProcAddress(DLLHandle, 'sqlite3_bind_parameter_name');
      @SQLite3_create_collation := GetProcAddress(DLLHandle, 'sqlite3_create_collation');

      if not Assigned(@SQLite3_Open) then raise AsgError.Create('DLL::SQlite3Open not found')
      else if not Assigned(@SQLite3_Close) then AsgError.Create('DLL::SQlite3Close not found')
      else if not Assigned(@SQLite3_Exec) then AsgError.Create('DLL::SQlite3Exe not found')
      else if not Assigned(@SQLite3_LibVersion) then AsgError.Create('DLL::SQliteLibversion not found')
      else if not Assigned(@SQLite3_ErrorString) then AsgError.Create('DLL::SQlite3ErrorStringnot found')
      else if not Assigned(@SQLite3_GetTable) then AsgError.Create('DLL::SQlite3GetTable not found')
      else if not Assigned(@SQLite3_FreeTable) then AsgError.Create('DLL::SQlite3FreeTable not found')
      else if not Assigned(@SQLite3_FreeMem) then AsgError.Create('DLL::SQlite3FreeMem not found')
      else if not Assigned(@SQLite3_Complete) then AsgError.Create('DLL::SQlite3Complete not found')
      else if not Assigned(@SQLite3_LastInsertRow) then AsgError.Create('DLL::SQlite3LastInsertRow not found')
      else if not Assigned(@SQLite3_Cancel) then AsgError.Create('DLL::SQlite3Cancel not found')
      else if not Assigned(@SQLite3_BusyTimeout) then AsgError.Create('DLL::SQlite3Busytimeout not found')
      else if not Assigned(@SQLite3_BusyHandler) then AsgError.Create('DLL::SQlite3BusyHandler not found')
      else if not Assigned(@SQLite3_Changes) then AsgError.Create('DLL::SQlite3Changes not found')
      else if not Assigned(@SQLite3_Prepare) then AsgError.Create('DLL::SQlite3Prepare not found')
      else if not Assigned(@SQLite3_Finalize) then AsgError.Create('DLL::SQlite3Finalize not found')
      else if not Assigned(@SQLite3_Reset) then AsgError.Create('DLL::SQlite3Reset not found')
      else if not Assigned(@SQLite3_Step) then AsgError.Create('DLL::SQlite3Step not found')
      else if not Assigned(@SQLite3_Column_blob) then AsgError.Create('DLL::SQlite3ColumnBlob not found')
      else if not Assigned(@SQLite3_Column_bytes) then AsgError.Create('DLL::SQlite3ColumnBytes not found')
      else if not Assigned(@SQLite3_Column_bytes16) then AsgError.Create('DLL::SQlite3ColumnBytes16 not found')
      else if not Assigned(@SQLite3_Column_Count) then AsgError.Create('DLL::SQlite3ColumnCount not found')
      else if not Assigned(@SQLite3_Column_decltype) then AsgError.Create('DLL::SQlite3ColumnDeclType not found')
      else if not Assigned(@SQLite3_Column_double) then AsgError.Create('DLL::SQlite3ColumnDouble not found')
      else if not Assigned(@SQLite3_Column_int) then AsgError.Create('DLL::SQlite3ColumnInt not found')
      else if not Assigned(@SQLite3_Column_int64) then AsgError.Create('DLL::SQlite3ColumnInt64 not found')
      else if not Assigned(@SQLite3_Column_name) then AsgError.Create('DLL::SQlite3ColumnName not found')
      else if not Assigned(@SQLite3_Column_text) then AsgError.Create('DLL::SQlite3ColumnText not found')
      else if not Assigned(@SQLite3_Column_text16) then AsgError.Create('DLL::SQlite3ColumnText16 not found')
      else if not Assigned(@SQLite3_Column_type) then AsgError.Create('DLL::SQlite3COlumnTypenot found')
      else if not Assigned(@SQLite3_Bind_blob) then AsgError.Create('DLL::SQlite3BindBlob not found')
      else if not Assigned(@SQLite3_Bind_Value) then AsgError.Create('DLL::SQlite3BindValue not found')
      else if not Assigned(@SQLite3_Bind_int) then AsgError.Create('DLL::SQlite3BindInt not found')
      else if not Assigned(@SQLite3_Bind_double) then AsgError.Create('DLL::SQlite3BindDouble not found')
      else if not Assigned(@SQLite3_Bind_null) then AsgError.Create('DLL::SQlite3BindNull not found')
      else if not Assigned(@SQLite3_Bind_text) then AsgError.Create('DLL::SQlite3BindText not found')
      else if not Assigned(@SQLite3_Bind_Text16) then AsgError.Create('DLL::SQlite3BindText16 not found')
      else if not Assigned(@SQLite3_Bind_Parameter_Count) then AsgError.Create('DLL::SQlite3BindParameterCount not found')
      else if not Assigned(@SQLite3_Bind_Parameter_Name) then AsgError.Create('DLL::SQlite3BindParameterName not found')
      else if not Assigned(@SQLite3_create_collation) then AsgError.Create('DLL::SQlite3CreateCollation not found');
      Result := true;
    end;
    {$ELSE}
      DllHandle := 1;
      @SQLite3_Open := @_sqlite3_open;
      @SQLite3_Close := @_sqlite3_close;
      @SQLite3_Exec := @_sqlite3_exec;
      @SQLite3_LibVersion := @_sqlite3_libversion;
      @SQLite3_ErrorString := @_sqlite3_errmsg;
      @SQLite3_GetTable := @_sqlite3_get_table;
      @SQLite3_FreeTable := @_sqlite3_free_table;
      @SQLite3_FreeMem := @_sqlite3_free;
      @SQLite3_Complete := @_sqlite3_complete;
      @SQLite3_LastInsertRow := @_sqlite3_last_insert_rowid;
      @SQLite3_Cancel := @_sqlite3_interrupt;
      @SQLite3_BusyTimeout := @_sqlite3_busy_timeout;
      @SQLite3_BusyHandler := @_sqlite3_busy_handler;
      @SQLite3_Changes := @_sqlite3_changes;
      @SQLite3_Prepare := @_sqlite3_prepare;
      @SQLite3_Finalize := @_sqlite3_finalize;
      @SQLite3_Reset := @_sqlite3_reset;
      @SQLite3_Step := @_sqlite3_step;
      @SQLite3_Column_blob := @_sqlite3_column_blob;
      @SQLite3_Column_bytes := @_sqlite3_column_bytes;
      @SQLite3_Column_bytes16 := @_sqlite3_column_bytes16;
      @SQLite3_Column_count := @_sqlite3_column_count;
      @SQLite3_Column_decltype := @_sqlite3_column_decltype;
      @SQLite3_Column_double := @_sqlite3_column_double;
      @SQLite3_Column_int := @_sqlite3_column_int;
      @SQLite3_Column_int64 := @_sqlite3_column_int64;
      @SQLite3_Column_name := @_sqlite3_column_name;
      @SQLite3_Column_text := @_sqlite3_column_text;
      @SQLite3_Column_type := @_sqlite3_column_type;
      @SQLite3_Bind_Blob := @_sqlite3_bind_blob;
      @SQLite3_create_collation := @_sqlite3_create_collation;
      @SQLite3_Column_text16 := @_SQLite3_Column_text16;
      @SQLite3_Bind_Text16 := @_SQLite3_Bind_Text16;
      @sqlite3_bind_parameter_count := @_sqlite3_bind_parameter_count;
      Result := true;
    {$ENDIF}
  finally
    DebugLeave('TASQLite3DB.LoadLibs');
  end;
end;

procedure TASQLite3DB.ShowError;
var msg             : PAnsiChar;
begin
  msg := SQLite3_ErrorString(DBHandle);
  raise EDatabaseError.Create(msg+#10+#10+FSQL);
end;

function TASQLite3DB.SQLite3_ExecSQL_Params(TheStatement: string; Params: TParams): integer;
var
 PF: PAnsiChar;
 //TheField : AnsiString;
 p: Pointer;
 i: Integer;
 b: Integer;
// m: TMemoryStream;
begin
// TheStatement := StringReplace(TheStatement, #2, '?', [rfReplaceAll, rfIgnoreCase]);
 FSQL := TheStatement;
{$IFDEF ASQLITE_D6PLUS}
 if FUtf8 then
    PF := PAnsiChar(AnsiToUtf8(TheStatement))
  else
{$ENDIF}
    PF := PAnsiChar(TheStatement);

 repeat
   Result := SQLite3_Prepare(DBHandle, PF, -1, p, PF);
   if Result = SQLITE_OK then
     begin
         for i := 0 to Params.Count - 1 do Begin
             b := i + 1;
             if Params[i].DataType = ftInteger then
                SQLite3_Bind_Int(p, b, Params[i].AsInteger)
             else if Params[i].DataType = ftFloat then
                SQLite3_Bind_Double(p, b, Params[i].AsFloat)
             else if Params[i].DataType = ftCurrency then
                SQLite3_Bind_Double(p, b, Params[i].AsFloat)
             else if Params[i].DataType = ftString then
                SQLite3_Bind_Text(p, b, PChar(Params[i].AsString), Params[i].GetDataSize, nil)
             else if Params[i].DataType = ftBoolean then
                SQLite3_Bind_Text(p, b, PChar(Params[i].AsString), Params[i].GetDataSize, nil)
             else if Params[i].DataType = ftWideString then
                SQLite3_Bind_Text16(p, b, PWideChar(Params[i].AsWideString), Params[i].GetDataSize, nil)
             else if Params[i].DataType = ftDateTime then
                SQLite3_Bind_Text(p, b, PChar(FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Params[i].AsDateTime)), 23, nil)
             else if Params[i].DataType = ftTimeStamp then
                SQLite3_Bind_Text(p, b, PChar(FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Params[i].AsDateTime)), 23, nil)
             else if Params[i].DataType = ftDate then
                SQLite3_Bind_Text(p, b, PChar(FormatDateTime('yyyy-mm-dd', Params[i].AsDateTime)), 10, nil)
             else if Params[i].DataType = ftTime then
                SQLite3_Bind_Text(p, b, PChar(FormatDateTime('hh:nn:ss:zzz', Params[i].AsDateTime)), 10, nil)
             else if Params[i].DataType in [ftMemo, ftGraphic, ftBlob, ftVariant] then
                SQLite3_Bind_Blob(p, b, PChar(Params[i].AsBlob), Params[i].GetDataSize, SQLITE_TRANSIENT);
         End;
         repeat until SQLite3_Step(p) in [SQLITE_DONE, SQLITE_ERROR, SQLITE_MISUSE];
       end else ShowError;



       Result := SQLite3_Finalize(p);
       if Result <> SQLITE_OK then ShowError;
 until PF^ = #0;
end;

function TASQLite3DB.SQLite3_ExecSQL(TheStatement: string; Fields : TFields): integer;
var
 PF: PAnsiChar;
 TheField : AnsiString;
 MyField : TField;
 p: Pointer;
 pc: Integer;
 b,I: Integer;
 dbl : Double;
// m: TMemoryStream;
begin
// TheStatement := StringReplace(TheStatement, #2, '?', [rfReplaceAll, rfIgnoreCase]);
 FSQL := TheStatement;
{$IFDEF ASQLITE_D6PLUS}
 if FUtf8 then
    PF := PAnsiChar(AnsiToUtf8(TheStatement))
  else
{$ENDIF}
    PF := PAnsiChar(TheStatement);

//    showmessage(inttostr(sizeof(double)));
 repeat
   Result := SQLite3_Prepare(DBHandle, PF, -1, p, PF);
   if Result = SQLITE_OK then
     begin
       pc := sqlite3_bind_parameter_count(p);
       if pc > 0 then begin
         for b := 1 to pc do Begin
             TheField := sqlite3_bind_parameter_name(p, b);
              // remove the leading ':' and bind
             Delete(TheField,1,1); if TheField='' then continue;

             MyField := Fields.FindField(TheField);
             I := Fields.IndexOf(MyField);

             if MyField=nil then begin
                showmessage(TheField+' not found');
                exit;
             end;

             if (MyField.IsNull) and (not (MyField.DataType in [ftMemo, ftGraphic, ftBlob, ftVariant])) then
                SQLite3_Bind_null(p, b)
             else begin
               if MyField.DataType = ftInteger then
                  SQLite3_Bind_Int(p, b, MyField.AsInteger)
               else if MyField.DataType = ftFloat then begin
                  dbl := MyField.AsFloat;
                  SQLite3_Bind_Double(p, b, dbl)
               end else if MyField.DataType = ftCurrency then
                  SQLite3_Bind_Double(p, b, MyField.AsFloat)
               else if MyField.DataType = ftString then
                  SQLite3_Bind_Text(p, b, PChar(MyField.AsString), Length(MyField.AsString), nil)
               else if MyField.DataType = ftBoolean then
                  SQLite3_Bind_Text(p, b, PChar(MyField.AsString), Length(MyField.AsString), nil)
               else if MyField.DataType = ftWideString then
                  SQLite3_Bind_Text16(p, b, PWideChar(MyField.AsWideString), MyField.Size, nil)
               else if MyField.DataType = ftDateTime then
                  SQLite3_Bind_Text(p, b, PChar(FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', MyField.AsDateTime)), 23, nil)
               else if MyField.DataType = ftTimeStamp then
                  SQLite3_Bind_Text(p, b, PChar(FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', MyField.AsDateTime)), 23, nil)
               else if MyField.DataType = ftDate then
                  SQLite3_Bind_Text(p, b, PChar(FormatDateTime('yyyy-mm-dd', MyField.AsDateTime)), 10, nil)
               else if MyField.DataType = ftTime then
                  SQLite3_Bind_Text(p, b, PChar(FormatDateTime('hh:nn:ss:zzz', MyField.AsDateTime)), 10, nil)
               else if MyField.DataType in [ftMemo, ftGraphic, ftBlob, ftVariant] then
                  SQLite3_Bind_Blob(p, b, PChar(Fields[i].AsString), TBlobField(Fields[i]).BlobSize, SQLITE_TRANSIENT);
           End;
         End;
       end;

       repeat until SQLite3_Step(p) in [SQLITE_DONE, SQLITE_ERROR, SQLITE_MISUSE];

       Result := SQLite3_Finalize(p);
       if Result <> SQLITE_OK then ShowError;
     end
   else
     ShowError;
 until PF^ = #0;
end;

function TASQLite3DB.SQLite3_PrepareResult(DB: Pointer; TheStatement: string; Sender: TObject): Pointer;
var
  i,tmpi: Integer;
  t: PAnsiChar;
  RV: Integer;
//  RowIdCol: Integer; // column containing rowid
//  RowId: Integer; // current record row id (to be stored in resultset)
  colname, coltype: PChar;
  tmpcolname:string;
  FieldType: TFieldType;
  FieldLen: Integer;
  FieldDec: Integer;
//  bFirst: Boolean;
  //wildcard: Integer;
begin
  if not (Sender is TASQLite3BaseQuery) then Exit;

  with (Sender as TASQLite3BaseQuery) do begin
         // if there are blob fields then we need to bind the blob variable
        RowId := -1;
        RowIdCol := -1;
//        TheStatement := StringReplace(TheStatement, #2, '?', [rfReplaceAll, rfIgnoreCase]);

//        bFirst := True;
{$IFDEF ASQLITE_D6PLUS}
        if FUtf8 then
           RV := SQLite3_Prepare(DBHandle, PAnsiChar(AnsiToUtf8(TheStatement)), -1, result, t)
        else
{$endif}
           RV := SQLite3_Prepare(DBHandle, PAnsiChar(TheStatement), -1, result, t);

          if RV <> 0 then ShowError else begin
                  if SQLite3_Column_count(result) > 0 then FieldDefs.Clear;
                  for i := 0 to SQLite3_Column_count(result) - 1 do begin
                      colname := SQLite3_Column_name(result, i);

                  // the second field named "ID", change in "ID_1" (like InterBase)
                      if (FieldDefs.IndexOf(colname) >= 0) then begin           // Mirko
                         tmpColName := colname;                                 // Mirko
                         tmpI := 0;                                             // Mirko
                         while (FieldDefs.IndexOf(tmpcolname) >= 0) do begin    // Mirko
                           inc(tmpI);                                           // Mirko
                           tmpColName := colname + '_' + inttostr(tmpI);        // Mirko
                         end;                                                   // Mirko
                         colName := PChar(tmpColName);                          // Mirko
                      end;                                                      // Mirko

                      if AnsiCompareText(colname, 'rowid') = 0 then begin
                          RowIdCol := i;
                        end else begin
                          coltype := SQLite3_Column_decltype(result, i);
                          //SQl: select max(CurID) from Items, sqlite3_column_decltype returns null.. it's probably SQLite bug
                          // better is to use max(CurID) as something from .... Aducom
                          if coltype = nil then
                            GetFieldInfo('string', FieldType, FieldLen, FieldDec) //OL
                          else
                            GetFieldInfo(coltype, FieldType, FieldLen, FieldDec);

                          if TypeLess then begin
                              with FieldDefs.AddFieldDef do begin
                                  Name := colname;
                                  DataType := ftString;
                                  Size := FieldLen;
                              end;
                          end else begin
                              if (FieldType <> ftString) and (FieldType <> ftWideString) then begin
                                  with FieldDefs.AddFieldDef do begin
                                     Name := colname;
                                     DataType := FieldType;
                                     if FieldType = ftFloat then
                                        Precision := FieldDec;
                                  end
                              end else begin
                                  with FieldDefs.AddFieldDef do begin
                                      Name := colname;
                                      DataType := FieldType;
                                      size := FieldLen;
                                  end;
                              end;
                          end;
                          MaxStrLen := MaxStrLen + GetNativeFieldSize(i + 1)+1; // compensate for nulls
                          FResult.SetBufSize(MaxStrLen + 1 + SizeOf(TBookMark)+1);
                        end;
                    end;
                end;
             //nd;
          end;
end;

function TASQLite3DB.SQLite3_GetNextResult(DB: Pointer; TheStatement: pointer; Sender: TObject) : pointer;
var
  i : integer;
  RV: Integer;
  mv: Integer;
  convertbuf: TConvertBuffer;
  pData: PAnsiChar;
  pWideData : PWideChar;
  BlobStream: TMemoryStream;
begin
  result := nil;
  with (Sender as TASQLite3BaseQuery) do begin
    FillChar(ResultStr^, MaxBuf, 0);
    RV := SQLite3_Step(theStatement);
    if RV = SQLITE_ROW then begin
      for i := 0 to SQLite3_Column_count(theStatement) - 1 do begin
        if i = RowIdCol then begin // just save rowid
          RowId := SQLite3_Column_int(theStatement, i);
        end
        else begin
          //Jure - Add null marker. This will fill it with #0 when null, #1 when not null
          setNullSrc(ResultStr, i + 1, SQLite3_column_type(theStatement, i) = 5);

          pData := SQLite3_Column_text(theStatement, i);
          if pData = nil then pData := '';

          if FTypeLess then begin
            mv := GetNativeFieldSize(i + 1);
            if StrLen(pData) < Cardinal(mv) then
              mv := StrLen(pData);
            Move(pData^, (ResultStr + GetFieldOffset(i + 1))^, mv);
          end
          else begin
            case FieldDefs[i].DataType of
              ftWideString : begin
                  pWideData := SQLite3_Column_text16(theStatement, i);
                  if pWideData=nil then pWideData := '';

                  mv := FieldDefs[i].Size +1;
                  Move(pWideData^, (ResultStr + GetFieldOffset(i + 1))^, mv);
              end;

              ftString: begin // DI
                mv := GetNativeFieldSize(i + 1);
                  if StrLen(pData) < Cardinal(mv) then
                    mv := StrLen(pData)+1;
                    Move(pData^, (ResultStr + GetFieldOffset(i + 1))^, mv);
                end;

              ftFmtMemo : begin // unicode!!! 'dirty' trick for tntWare
                pWideData := SQLite3_Column_Text16(theStatement, i);
                MessageBoxW(HWND(nil),pWideData, '', MB_OK);
                BlobStream := TMemoryStream.Create;
                pData := pAnsiChar(pWideData);
                MessageBoxW(HWND(nil),pWideChar(pData), '', MB_OK);
                  BlobStream.Write(pData^, 100);//SQLite3_Column_bytes16(theStatement, i))
                Move(BlobStream, (ResultStr + GetFieldOffset(i + 1))^, SizeOf(BlobStream)+1); //2007
              end;

              ftMemo, ftGraphic, ftBlob, ftVariant: begin// DI
                // create memory stream to save blob;
                pData := SQLite3_Column_blob(theStatement, i);
                BlobStream := TMemoryStream.Create;
                BlobStream.Write(pData^, SQLite3_Column_bytes(theStatement, i));
                Move(BlobStream, (ResultStr + GetFieldOffset(i + 1))^, SizeOf(BlobStream)+1); //2007
              end; // DI
              else // DI
              begin // DI
                UnpackBuffer(pData, FieldDefs[i].DataType, ConvertBuf);
                Move(convertbuf, (ResultStr + GetFieldOffset(i + 1))^, GetFieldSize(i + 1)+1);
              end;
            end;
          end;
        end;
      end;
      Result := ResultStr;
    end;
    if RV in [SQLITE_DONE] then result := nil
    else if RV in [SQLITE_ERROR, SQLITE_MISUSE, SQLITE_BUSY] then //f.e. inserting NULL in field declared as NOT NULL
      if (RV=SQLITE_BUSY) and (Assigned(FOnBusy)) then
          FOnBusy(self)
      else
          ShowError;
  end;
end;

procedure TASQLite3DB.SQLite3_CloseResult(TheStatement : pointer);
var RV : integer;
begin
    if TheStatement <> nil then begin
       SQLite3_Reset(TheStatement);
       RV := SQLite3_Finalize(TheStatement);
       if RV <> 0 then raise AsgError.Create('SQLiteExecute error: ' + IntToStr(RV));
    end;
end;

function TASQLite3DB.SQLite3_Execute(DB: Pointer; TheStatement: string; Params: TParams; Sender: TObject): Integer;
var
  p: Pointer;
  i,b: integer;
{$IFDEF ASQLITE_D6PLUS}
  Cursor: TDBScreenCursor;
{$endif}
begin
  SQLite3_Execute := 0;
  FSQL := TheStatement;
{$IFDEF ASQLITE_D6PLUS}
     Cursor := dcrDefault; //Jure: eliminates a warning
{$endif}

  if not (Sender is TASQLite3BaseQuery) then Exit;
  try
    with (Sender as TASQLite3BaseQuery) do begin
{$IFDEF ASQLITE_D6PLUS}
     if Assigned(DBScreen) and (FSQLCursor) then begin
       Cursor := DBScreen.Cursor;
       DBScreen.Cursor := dcrSQLWait;
     end;
{$endif}
    RowId := -1;

    FStatement := Connection.SQLite3_PrepareResult(Connection.DBHandle, PAnsiChar(TheStatement), Sender);
    p := FStatement;
    for i := 0 to Params.Count - 1 do Begin
       b := i + 1;
       if Params[i].DataType = ftInteger then
         SQLite3_Bind_Int(p, b, Params[i].AsInteger)
       else if Params[i].DataType = ftFloat then
         SQLite3_Bind_Double(p, b, Params[i].AsFloat)
       else if Params[i].DataType = ftCurrency then
         SQLite3_Bind_Double(p, b, Params[i].AsFloat)
       else if Params[i].DataType = ftString then
         SQLite3_Bind_Text(p, b, PChar(Params[i].AsString), Length(Params[i].AsString), nil)
       else if Params[i].DataType = ftBoolean then
         SQLite3_Bind_Text(p, b, PChar(Params[i].AsString), Length(Params[i].AsString), nil)
       else if Params[i].DataType = ftWideString then
         SQLite3_Bind_Text16(p, b, PWideChar(Params[i].AsWideString), Params[i].GetDataSize, nil)
       else if Params[i].DataType = ftDateTime then
         SQLite3_Bind_Text(p, b, PChar(FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Params[i].AsDateTime)), 23, nil)
       else if Params[i].DataType = ftTimeStamp then
         SQLite3_Bind_Text(p, b, PChar(FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', Params[i].AsDateTime)), 23, nil)
       else if Params[i].DataType = ftDate then
         SQLite3_Bind_Text(p, b, PChar(FormatDateTime('yyyy-mm-dd', Params[i].AsDateTime)), 10, nil)
       else if Params[i].DataType = ftTime then
         SQLite3_Bind_Text(p, b, PChar(FormatDateTime('hh:nn:ss:zzz', Params[i].AsDateTime)), 10, nil)
       else if Params[i].DataType in [ftMemo, ftGraphic, ftBlob, ftVariant] then
         SQLite3_Bind_Blob(p, b, PChar(Params[i].AsBlob), Params[i].GetDataSize, SQLITE_TRANSIENT);
    end;

    repeat
       p := Connection.SQLite3_GetNextResult(Connection.DBHandle, FStatement, Sender);
       if p <> nil then
          FResult.Add(ResultStr, RowId);
    until p = nil;

    Connection.SQLite3_CloseResult(FStatement);
    FStatement := nil;
    end;
  finally
{$IFDEF ASQLITE_D6PLUS}
    if Assigned(DBScreen) and ((Sender as TASQLite3BaseQuery).FSQLCursor) then begin
        DBScreen.Cursor := Cursor;
        if DBScreen.Cursor = dcrSQLWait then
           DBScreen.Cursor := dcrDefault;
    end;
{$endif}

  end;
end;

function TASQLite3DB.FGetDriverDLL: string;
begin
  DebugEnter('TASQLite3DB.FGetDriverDLL');
  if FDriverDLL = '' then
    FDriverDLL := 'SQLite3.dll';
  FGetDriverDLL := FDriverDLL;
  DebugLeave('TASQLite3DB.FGetDriverDLL');
end;

function TASQLite3DB.FGetDefaultExt: string;
begin
  DebugEnter('TASQLite3DB.FGetDefaultExt');
  if FDefaultExt = '' then
    FDefaultExt := '.sqb';
  FGetDefaultExt := FDefaultExt;
  DebugLeave('TASQLite3DB.FGetDefaultExt');
end;

procedure TASQLite3DB.FSetDatabase(Database: string);
begin
  DebugEnter('TASQLite3DB.FSetDatabase ' + Database);
  FDatabase := Trim(Database);
  if (ExtractFileExt(FDataBase)='') and (AnsiCompareText(':memory:',FDataBase)<>0) Then  // GPA
     FDatabase:=FDataBase+FDefaultExt;  
  DebugLeave('TASQLite3DB.FSetDatabase');
end;

procedure TASQLite3DB.ShowDatabases(List: TStrings);
var
  sr                : TSearchRec;
begin
  DebugEnter('TASQLite3DB.ShowDatabases');
  if DefaultExt = '' then
    DefaultExt := '.sqb';
  if DefaultExt[1] <> '.' then
    DefaultExt := '.' + DefaultExt;
  if DefaultDir <> '' then
    if DefaultDir[Length(DefaultDir)] <> '\' then
      DefaultDir := DefaultDir + '\';
  if FindFirst(FDefaultDir + '*' + DefaultExt, faAnyFile, sr) = 0 then
  begin
    repeat
      List.Add(sr.Name);
    until FindNext(sr) <> 0;
    SysUtils.FindClose(sr);
  end;
  DebugLeave('TASQLite3DB.ShowDatabases');
end;

procedure TASQLite3DB.GetTableNames(List: TStrings; SystemTables: boolean = false; TempTables: boolean = false);
var
  ResultPtr         : Pointer;
  ResultStr         : ^Pointer;
  RowCount          : cardinal;
  ColCount          : cardinal;
  ErrMsg            : PAnsiChar;
  i                 : integer;
begin
  DebugEnter('TASQLite3DB.GetTableNames');
//  Close; Open;

  if not FConnected then
     Connected := true;
     
  if FConnected then
  begin
    if TempTables then
    begin
        SQLite3_GetTable(DBHandle, PAnsiChar(
          'SELECT name FROM sqlite_master WHERE type="table" UNION SELECT name FROM sqlite_temp_master WHERE type="table" ORDER BY name '),
          ResultPtr, RowCount, ColCount, ErrMsg);
    end
    else
    begin
        SQLite3_GetTable(DBHandle, PAnsiChar(
          'SELECT name FROM sqlite_master WHERE type="table" ORDER BY name'),
          ResultPtr, RowCount, ColCount, ErrMsg);
    end;

    ResultStr := ResultPtr;
    List.Clear;
    Inc(ResultStr);                     // ignore header
    for i := 1 to RowCount do
    begin
      if (AnsiCompareText('name', PAnsiChar(ResultStr^)) <> 0) then
        List.Add(PAnsiChar(ResultStr^));
      Inc(ResultStr);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
  DebugLeave('TASQLite3DB.GetTableNames');
end;


procedure TASQLite3DB.GetIndexFieldNames(IndexName: string; List: TStrings);
var
  ResultPtr         : Pointer;
  ResultStr         : ^Pointer;
  RowCount          : cardinal;
  ColCount          : cardinal;
  ErrMsg            : PAnsiChar;
  i                 : integer;
begin
  DebugEnter('TASQLite3DB.GetIndexFieldNames');
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, PAnsiChar(
      'PRAGMA  index_info("' + IndexName + '");'),
      ResultPtr, RowCount, ColCount, ErrMsg);

    ResultStr := ResultPtr;
    List.Clear;
    Inc(ResultStr, 5);
    for i := 1 to RowCount do
    begin
      List.Insert(0, PAnsiChar(ResultStr^));
      Inc(ResultStr, 3);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
  DebugLeave('TASQLite3DB.GetIndexFieldNames');
end;

procedure TASQLite3DB.GetIndexNames(List: TStrings; SystemTables: boolean = false);
var
  ResultPtr         : Pointer;
  ResultStr         : ^Pointer;
  RowCount          : cardinal;
  ColCount          : cardinal;
  ErrMsg            : PAnsiChar;
  i                 : integer;
begin
  DebugEnter('TASQLite3DB.GetIndexNames');
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, PAnsiChar(
      'SELECT name FROM sqlite_master WHERE type="index" ORDER BY name'),
      ResultPtr, RowCount, ColCount, ErrMsg);

    ResultStr := ResultPtr;
    List.Clear;
    Inc(ResultStr);                     // ignore header
    for i := 1 to RowCount do
    begin
      List.Add(PAnsiChar(ResultStr^));
      Inc(ResultStr);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
  DebugLeave('TASQLite3DB.GetIndexNames');
end;

procedure TASQLite3DB.GetFieldNames(TableName: string; List: TStrings);
var
  ResultPtr         : Pointer;
  ResultStr         : ^Pointer;
  RowCount          : cardinal;
  ColCount          : cardinal;
  ErrMsg            : PAnsiChar;
  i                 : integer;
begin
  DebugEnter('TASQLite3DB.GetFieldNames ' + Tablename);
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, PAnsiChar('PRAGMA table_info("' + TableName + '");'),
      ResultPtr, RowCount, ColCount, ErrMsg);
    ResultStr := ResultPtr;
    List.Clear;
    Inc(ResultStr, 6);                  // headers can be ignored
    for i := 1 to RowCount do
    begin
      Inc(ResultStr);
      List.Add(PAnsiChar(ResultStr^));      // the second field contains the fieldname
      Inc(ResultStr, 5);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
  DebugLeave('TASQLite3DB.GetFieldNames');
end;

procedure TASQLite3DB.GetPrimaryKeys(TableName: string; List: TStrings);
var
  ResultPtr         : Pointer;
  ResultStr         : ^Pointer;
//  PK:     ^Pointer;
  RowCount          : cardinal;
  ColCount          : cardinal;
  ErrMsg            : PAnsiChar;
  Temp              : string;
  i                 : integer;
begin
  DebugEnter('TASQLite3DB.GetPrimaryKeys ' + Tablename);
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, PAnsiChar('PRAGMA table_info("' + TableName + '");'),
      ResultPtr, RowCount, ColCount, ErrMsg);
    ResultStr := ResultPtr;
    List.Clear;
    Inc(ResultStr, 6);                  // headers can be ignored
    for i := 1 to RowCount do
    begin
      Inc(ResultStr);
      Temp := PAnsiChar(ResultStr^);        // the second field contains the fieldname
      Inc(ResultStr, 4);
            // the last field reveils a indicator for primary key
      if PAnsiChar(ResultStr^) = '1' then
        List.Add(Temp);
      Inc(ResultStr);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
  DebugLeave('TASQLite3DB.GetPrimaryKeys');
end;

procedure TASQLite3DB.GetTableInfo(TableName: string; List: TList);
var
  ResultPtr         : Pointer;
  ResultStr         : ^Pointer;
  RowCount          : cardinal;
  ColCount          : cardinal;
  ErrMsg            : PAnsiChar;
  Field             : TASQLite3Field;
  i                 : integer;
begin
  DebugEnter('TASQLite3DB.GetTableInfo ' + Tablename);
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, PAnsiChar('PRAGMA table_info("' + TableName + '");'),
      ResultPtr, RowCount, ColCount, ErrMsg);
    ResultStr := ResultPtr;
    while List.Count > 0 do
    begin
      TASQLite3Field(List[0]).Free;
      List.Delete(0);
    end;
    List.Clear;

    Inc(ResultStr,6);
    for i := 1 to RowCount do
    begin
      Field := TASQLite3Field.Create;
      with Field do
      begin
        FieldNumber := StrToIntX(PAnsiChar(ResultStr^));
        Inc(ResultStr);
        FieldName := PAnsiChar(ResultStr^);
        Inc(ResultStr);
        FieldType := PAnsiChar(ResultStr^);
        Inc(ResultStr);
        FieldNN := StrToIntX(PAnsiChar(ResultStr^));
        Inc(ResultStr);
        FieldDefault := PAnsiChar(ResultStr^);
        Inc(ResultStr);
        FieldPK := StrToIntX(PAnsiChar(ResultStr^));
        Inc(ResultStr);
      end;
      List.Add(Field);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
  DebugLeave('TASQLite3DB.GetTableInfo');
end;

// retrieves the user version
function TASQLite3DB.GetUserVersion(database : string=''): integer;
var
  ResultPtr         : Pointer;
  ResultStr         : ^Pointer;
  RowCount          : cardinal;
  ColCount          : cardinal;
  ErrMsg            : PAnsiChar;
begin
  DebugEnter('TASQLite3DB.GetTableIndexNames');
  GetUserVersion := -1;
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    if database <> '' then database := database +'.';
    SQLite3_GetTable(DBHandle, PAnsiChar(
      'PRAGMA '+Database+'user_version'),
      ResultPtr, RowCount, ColCount, ErrMsg);
    ResultStr := ResultPtr;
    Inc(ResultStr);
    GetUserVersion := StrToIntX(PAnsiChar(ResultStr^));
  end;
end;

// sets user version.
procedure TASQLite3DB.SetUserVersion(Version : integer; Database : string='');
begin
  if Database <> '' then Database := Database +'.';
  SQLite3_ExecSQL('PRAGMA '+Database+'user_version='+IntToStr(Version),nil);
end;

function TASQLite3DB.GetSchemaVersion(database : string=''): integer;
var
  ResultPtr         : Pointer;
  ResultStr         : ^Pointer;
  RowCount          : cardinal;
  ColCount          : cardinal;
  ErrMsg            : PAnsiChar;
begin
  DebugEnter('TASQLite3DB.GetSchemaVersion');
  GetSchemaVersion := -1;
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    if database <> '' then database := database +'.';
    SQLite3_GetTable(DBHandle, PAnsiChar(
      'PRAGMA '+Database+'schema_version'),
      ResultPtr, RowCount, ColCount, ErrMsg);
    ResultStr := ResultPtr;
    Inc(ResultStr);
    GetSchemaVersion := StrToIntX(PAnsiChar(ResultStr^));
  end;
end;

// sets user version.
procedure TASQLite3DB.SetSchemaVersion(Version : integer; Database : string='');
begin
  if Database <> '' then Database := Database +'.';
  SQLite3_ExecSQL('PRAGMA '+Database+'schema_version='+IntToStr(Version),nil);
end;

procedure TASQLite3DB.GetTableIndexNames(TableName: string; List: TStrings);
var
  ResultPtr         : Pointer;
  ResultStr         : ^Pointer;
  RowCount          : cardinal;
  ColCount          : cardinal;
  ErrMsg            : PAnsiChar;
  i                 : integer;
begin
  DebugEnter('TASQLite3DB.GetTableIndexNames');
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, PAnsiChar(
      'PRAGMA  index_list("' + TableName + '");'),
      ResultPtr, RowCount, ColCount, ErrMsg);

    ResultStr := ResultPtr;
    List.Clear;
    Inc(ResultStr, 4);                  // Skip header + 1st col.
    for i := 1 to RowCount do
    begin
      List.Insert(0, PAnsiChar(ResultStr^));
      Inc(ResultStr, 3);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
  DebugLeave('TASQLite3DB.GetTableIndexNames');
end;

procedure TASQLite3DB.DBConnect(Connected: boolean);
var
//  ErrMsg            : PAnsiChar;
  DBMS              : string;
  rv                : integer;
  i                 : integer; // GPA
begin
  DebugEnter('TASQLite3DB.DBConnect');

  if (AnsiCompareText(FCharEnc,'utf8')=0) or (FCharEnc='') then
     FUtf8 := true
  else
     FUtf8 := false;

  if (Connected) and (FDatabase = '') then
  begin
    DebugLeave('TASQLite3DB.DBConnect Exit');
    raise AsgError.Create('Missing database property');
//    SQLite3_FreeMem(ErrMsg);
    FConnected := false;
    exit;
  end;


  if not Connected then begin
    if FConnected then
    begin
      if DLLHandle <> 0 then begin
         Debug('freeing sqlite dll');
        if Assigned(FBeforeDisconnect) then
          FBeforeDisconnect(self);
          // if closed then all Datasets must be closed (GPA)
          if Assigned(Owner) Then
            For I:=0 to Owner.ComponentCount-1 do
              if Owner.Components[I] is TASQLite3BaseQuery Then
                TASQLite3BaseQuery(Owner.Components[I]).Active:=False;

        if Assigned(@SQLite3_Close) then
          SQLite3_Close(DBHandle);

      {$IFNDEF SQLite_Static}
          FreeLibrary(DLLHandle);
      {$ENDIF}

        DLLHandle := 0;
        if Assigned(FAfterDisconnect) then
          FAfterDisconnect(self);
      end;
      FConnected := false;
      DebugLeave('TASQLite3DB.DBConnect');
      exit;
    end else begin
       exit; // quit if disconnect w.o. being connected
    end;
  end
  else
  begin

    if AnsiCompareText(':memory:', Database) <> 0 then begin
       if DefaultDir <> '' then begin
         if DefaultDir[Length(DefaultDir)] <> '\' then
           DefaultDir := DefaultDir + '\';
         DBMS := DefaultDir + Database;
       end else begin
         if Pos('\', Database) = 0 then
           DBMS := GetCurrentDir + '\' + DataBase
         else
           DBMS := Database;
       end;

       if FMustExist then begin
         if not FileExists(DBMS) then begin
          DebugLeave('TASQLite3DB.DBConnect ' + 'Database ' + DBMS + ' does not exist');
          raise EDatabaseError.Create('Database ' + DBMS + ' does not exist');
         end;
      end;
    end else DBMS := Database; // in memory database

    if DLLHandle = 0 then
    begin

      if not LoadLibs then
      begin

        FConnected := false;
        DebugLeave('TASQLite3DB.DBConnect ' + 'Could Not load SQLite Library ('+DriverDLL+')');
        raise AsgError.Create('Could not load SQLite library('+DriverDLL+')');
      end;
    end;


    FConnected := true;
    FVersion := SQLite3_LibVersion;

    DBHandle := nil;
//    ErrMsg := nil;
    if Assigned(FBeforeConnect) then
      FBeforeConnect(self);

{$IFDEF ASQLITE_D6PLUS}
    if Assigned(@SQLite3_Open) then
      if FVersion > '3.2.5' then
        rv := SQLite3_Open(PAnsiChar(AnsiToUTF8(DBMS)), DBHandle)
      else
{$endif}
        rv := SQLite3_Open(PAnsiChar(DBMS), DBHandle);

    if DBHandle = nil then begin
       FConnected := false;
       raise AsgError.Create('Cannot open database');
    end;

    SQLite3_create_collation(DBHandle, 'systemnocase', 1, self, systemNoCaseCompare);
    SQLite3_create_collation(DBHandle, 'system',       1, self, systemCompare);

    if Assigned(FAfterConnect) then
      FAfterConnect(self);

    if Assigned(FASQLitePragma) then
      ExecPragma;

//    FLastError := ErrMsg;
//    if ErrMsg <> nil then
//      SQLite3_FreeMem(ErrMsg);
 // GPA Added to execute InlineSQL in case of use Connected:=True instead of Open
    if ExecuteInlineSQL and Assigned(FInlineSQL) then Try    // GPA
      ExecStartTransaction('');                              // GPA
      SQLite3_ExecSQL(FInlineSQL.FSQL.Text,nil);                 // GPA
      Commit;                                                // GPA
    finally                                                  // GPA
      ExecuteInlineSQL:=False;  //GPA Assure just one execution in case of reopen
    end;                                                     // GPA

    if FConnected and (FTimeOut > 0) then
       TimeOut:= FTimeOut;
  end;
  DebugLeave('TASQLite3DB.DBConnect');
end;

function TASQLite3DB.RowsAffected: integer;
begin
  DebugEnter('TASQLite3DB.RowsAffected');
  if not FConnected then
    Result := -1
  else
    Result := SQLite3_Changes(DBHandle);
  DebugLeave('TASQLite3DB.RowsAffected');
end;

//------------------------------------------------------------------------------
// By Ralf, The Delphi Inspiration
//------------------------------------------------------------------------------

function TableExistsCallback(UserData: Pointer; ColumnCount: Integer; ColumnValues, ColumnNames: PPointer): Integer; cdecl;
begin
  if AnsiStrIComp(UserData, ColumnValues^) <> 0 then
    Result := 0
  else
    Result := 1; // Abort
end;

//------------------------------------------------------------------------------

function TASQLite3DB.TableExists(const ATableName: AnsiString): Boolean;
var
  ErrMsg: PAnsiChar;
begin
  try
    { No WHERE clause is used in the SQL statement below.
      Instead, the callback function compares without case sensitivity. }
    Result := SQLite3_Exec(DBHandle, 'SELECT name FROM sqlite_master',
                           TableExistsCallback,Pointer(ATableName), ErrMsg) = SQLITE_ABORT;
  finally
    if ErrMsg <> nil then
      begin
        SQLite3_FreeMem(ErrMsg);
        ShowError;
      end;
  end;
end;

//------------------------------------------------------------------------------
// Support for several transaction types, used if transaction set to automatic
//
procedure TASQLite3DB.ExecStartTransaction(TransType: string);
begin
// if no transaction type available then use default from asqlitedb
  if (TransType = '') then TransType := FTransactionType;
  if ((TransType = '') or (AnsiCompareText(TransType, 'DEFAULT') = 0)) then StartTransaction
  else if (AnsiCompareText(TransType, 'DEFERRED') = 0) then StartDeferredTransaction
  else if (AnsiCompareText(TransType, 'IMMEDIATE') = 0) then StartImmediateTransaction
  else if (AnsiCompareText(TransType, 'EXCLUSIVE') = 0) then StartExclusiveTransaction
  else StartTransaction;
end;

function TASQLite3DB.InTransaction : boolean;
begin
  Result := FInTransaction;
end;

procedure TASQLite3DB.StartTransaction;
begin
  DebugEnter('TASQLite3DB.StartTransaction');
  if not FConnected then                // open database if necessary
    Connected := true;                  // trigger the 'dbconnect' event
  if FConnected then begin
     SQLite3_ExecSQL('begin transaction',nil);
     FInTransaction := True;
  end;
  DebugLeave('TASQLite3DB.StartTransaction');
end;

procedure TASQLite3DB.StartDeferredTransaction;
begin
  if not FConnected then                // open database if necessary
    Connected := true;                  // trigger the 'dbconnect' event
  if FConnected then begin
    SQLite3_ExecSQL('begin deferred transaction',nil);
    FInTransaction := True;
  end;
end;

procedure TASQLite3DB.StartImmediateTransaction;
begin
  if not FConnected then                // open database if necessary
     Connected := true;                 // trigger the 'dbconnect' event
  if FConnected then begin
     SQLite3_ExecSQL('begin immediate transaction',nil);
     FInTransaction := True;
  end;
end;

procedure TASQLite3DB.StartExclusiveTransaction;
begin
  if not FConnected then                // open database if necessary
    Connected := true;                  // trigger the 'dbconnect' event
  if FConnected then begin
     SQLite3_ExecSQL('begin exclusive transaction',nil);
     FInTransaction := True;
  end;
end;

procedure TASQLite3DB.Open;
begin
  DebugEnter('TASQLite3DB.Open');
  Connected := true;

  if DLLHandle = 0 then
    Connected := false;

  DebugLeave('TASQLite3DB.Open');
end;

procedure TASQLite3DB.Close;
begin
  DebugEnter('TASQLite3DB.Close');
  Connected := false;
  DebugLeave('TASQLite3DB.Close');
end;

procedure TASQLite3DB.ExecPragma;
var
  Cmd               : string;
begin
  DebugEnter('TASQLite3DB.ExecPragma');
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    if FASQLitePragma.FTempCacheSize <> 0 then
    begin
      cmd := FASQLitePragma.GetTempCacheSize;
      SQLite3_ExecSQL(cmd,nil);
    end;
    if FASQLitePragma.FDefaultCacheSize <> 0 then
    begin
      cmd := FASQLitePragma.GetDefaultCacheSize;
      SQLite3_ExecSQL(cmd,nil);
    end;

    if FASQLitePragma.FDefaultSynchronous <> '' then
    begin
      cmd := FASQLitePragma.GetDefaultSynchronous;
      SQLite3_ExecSQL(cmd);
    end;

    if FASQLitePragma.FDefaultTempStore <> '' then
    begin
      cmd := FASQLitePragma.GetDefaultTempStore;
      SQLite3_ExecSQL(cmd);
    end;

    if FASQLitePragma.FTempStore <> '' then
    begin
      cmd := FASQLitePragma.GetTempStore;
      SQLite3_ExecSQL(cmd);
    end;

    if FASQLitePragma.FSynchronous <> '' then
    begin
      cmd := FASQLitePragma.GetSynchronous;
      SQLite3_ExecSQL(cmd);
    end;
  end;
  DebugLeave('TASQLite3DB.ExecPragma');
end;

procedure TASQLite3DB.Commit;
begin
  DebugEnter('TASQLite3DB.Commit');
  if not FConnected then
    Connected := true;
  if FConnected then begin
     SQLite3_ExecSQL('commit transaction');
     FInTransaction := False;
  end;
  DebugLeave('TASQLite3DB.Commit');
end;

procedure TASQLite3DB.RollBack;
begin
  DebugEnter('TASQLite3DB.RollBack');
  if not FConnected then
    Connected := true;
  if FConnected then begin
     SQLite3_ExecSQL('rollback transaction');
     FInTransaction := false;
  end;
  DebugLeave('TASQLite3DB.RollBack');
end;

constructor TASQLite3DB.Create(AOwner: TComponent);
begin
  DebugEnter('TASQLite3DB.Create');
  FTimeOut:= 0;
  Connected := false;
  FInTransaction := false;
  ASQLiteLog := nil;
  ASQLitePragma := nil;
  inherited Create(AOwner);
  DebugLeave('TASQLite3DB.Create');
end;

destructor TASQLite3DB.Destroy;
begin
  DebugEnter('TASQLite3DB.Destroy');
  Connected := false;
  ASQLiteLog := nil;
  ASQLitePragma := nil;
  inherited Destroy;
  DebugLeave('TASQLite3DB.Destroy');
end;

//============================================================================== TASQLite3BaseQuery

procedure TASQLite3BaseQuery.InternalRefresh;
var Rec : integer;
begin
  Rec := GetRecNo;
  close;
  open;
  SetRecNo(Rec);
end;

function TASQLite3BaseQuery.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; //MS
begin
 if (Bookmark1 =nil) or (Bookmark2 = nil) then
     result := 0
 else
   result := sign(integer(Bookmark1^)-integer(Bookmark2^));
end;

// Is one or more fields a calculated field?  (John Lito)
function TASQLite3BaseQuery.CalcFieldInList(const List: string): Boolean;
var i: Integer;
    Fields: TList;
begin
  if Pos(';', List) <> 0 then
  begin
    Result := False;
    Fields := TList.Create;
    try
      GetFieldList(Fields, List);
      for i := 0 to Fields.Count - 1 do
        if TField(Fields[I]).FieldKind in [fkCalculated, fkLookup] then Result := True;
    finally
      Fields.Free;
    end;
  end else
    Result := (FieldByName(List).FieldKind in [fkCalculated, fkLookup]);
end;

function TASQLite3BaseQuery.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
var OldState: TDataSetState;
begin
  Result := '';
  if Locate(KeyFields, KeyValues, []) then
  begin
    if CalcFieldInList(ResultFields) then GetCalcFields(PChar(FResult.GetData(FCurRec)));
    OldState := SetTempState(dsFilter);
    try
      Result := FieldValues[ResultFields];
    finally
      RestoreState(OldState);
    end;
  end;
end;

{
  support routine for UTF16
}

procedure TASQlite3BaseQuery.DataConvert(Field: TField; Source, Dest: Pointer;
  ToNative: Boolean);
begin
  if Field.DataType = ftWideString then begin
     if ToNative then begin
        WStrCopy(PWideChar(Dest), PWideChar(Source))
     end else begin
        WStrCopy(PWideChar(Dest), PWideChar(Source))
     end;//ftWideString
  end else
       inherited DataConvert(Field, Source, Dest, ToNative);
 end;//DataConvert

procedure TASQlite3BaseQuery.DataEvent(Event: TDataEvent; Info: Longint);
var i : integer;
begin

 if Event = deDataSetChange then begin
    if Fields[0].IsNull then begin
       for i := 0 to DetailList.Count - 1 do
        begin
         TASQLite3BaseQuery(DetailList[i]).Close;
        end;
    end else begin
       for i := 0 to DetailList.Count - 1 do
        begin
         TASQLite3BaseQuery(DetailList[i]).Open;
        end;
    end;
 end;
 inherited;
end;
{
 Register detail dataset for a master-detail relationship
}
procedure TASQLite3BaseQuery.RegisterDetailDataset(DetailDataSet: TASQLite3BaseQuery);
var
  i                 : integer;
begin
  DebugEnter('TASQLite3BaseQuery.RegisterDetailDataset');
  try
    if MDFilter <> '' then
       DetailDataset.FPrepared := DetailDataSet.FPrepared + ' and '+MDFilter
    else
       DetailDataset.FPrepared := DetailDataSet.FPrepared + ' where '+DetailDataSet.BuildFilter;

    for i := 0 to DetailList.Count - 1 do
      if DetailList[i] = DetailDataset then exit;
    DetailList.Add(DetailDataSet);
  finally
    DebugLeave('TASQLite3BaseQuery.RegisterDetailDataset');
  end;
end;

{ compatibility isue }
function TASQLite3BaseQuery.BuildFilter : string;
var
  r, s              : string;
  m, d              : string;
  p                 : integer;
  cAnd              : string;
begin
  DebugEnter('TASQLite3BaseQuery.BuildFilter');
  cAnd := '';

  r := FMasterFields;
  Result := '';
  while r <> '' do
  begin
    p := pos(';', r);
    if p = 0 then
    begin
      if Trim(r) <> '' then
        s := r;
      r := '';
    end
    else
    begin
      s := Trim(Copy(r, 1, p - 1));
      System.Delete(r, 1, p);
    end;

    p := pos('=', s);
    if p = 0 then
    begin
      raise AsgError.Create('Syntax error: Masterfields not build of a=b;... pairs');
    end
    else
    begin
      d := copy(s, 1, p - 1);
      m := copy(s, p + 1, 99);
    end;
    Result := Result + cAnd + d + '=' + QuotedStr(FMasterSource.DataSet.FieldByName(m).AsString) ;
    cAnd := ' and ';
  end;
  DebugLeave('TASQLite3BaseQuery.Buildfilter');
end;

procedure TASQLite3BaseQuery.SQLiteMasterChanged;
begin
  DebugEnter('TASQLite3BaseQuery.SQLiteMasterChanged');
  Close;
  MDFilter := BuildFilter;
  if (MDFilter <> '') then begin
    Open;
  end;
  DebugLeave('TASQLite3BaseQuery.SQLiteMasterChanged');
end;

{
 notify that the master has changed and a requery on the detail has
 to be done
}

procedure TASQLite3BaseQuery.NotifySQLiteMasterChanged;
var
  i                 : integer;
begin
  DebugEnter('TASQLite3BaseQuery.NotifySQLiteMasterChanged');
  for i := 0 to DetailList.Count - 1 do
  begin
    TASQLite3BaseQuery(DetailList[i]).SQLiteMasterChanged;
  end;
  DebugLeave('TASQLite3BaseQuery.NotifySQLiteMasterChanged');
end;

{
   This function returns a string representing the value of the specified field
   in SQLite format. Floating point values always use '.' as a decimal separator.
   Date values use 'yyyy-mm-dd' format, unless SQLiteDateFormat is set to false,
   which results in using TableDateFormat, or system dependent ShortDateFormat
   if TableDateFormat is not set. Same goes for DateTime and Time values, for
   which default formats are 'yyyy-mm-ss hh:nn:ss' and 'hh:nn:ss.zzz', respectively.
   Setting SQLiteDateFormat to false is discouraged.
   Result is quoted when necessary.
}
// added by Donnie

//function TASQLite3BaseQuery.GetFieldValue(const AField: TField; const Blobs: TList = nil): string;
//var
//  MS: TMemoryStream;
//  DateTimeFormat: string;
//begin
//  if (AField.DataSet <> Self) then
//    raise EInvalidArgument.Create('Only own fields are accepted');
//  if aField.IsNull then
//    result := 'null'
//  else
//  case AField.DataType of
//    ftString:
//{$IFDEF ASQLITE_D6PLUS}
//      if Connection.FUtf8 then
//        Result := QuotedStr(UTF8Encode(VarToWideStr(AField.Value)))
//      else
//{$endif}
//        Result := QuotedStr(AField.AsString);
//    ftSmallint, ftInteger, ftWord:
//      Result := AField.AsString;
//    ftFloat:
//      if DecimalSeparator <> '.' then
//{$IFDEF ASQLITE_D6PLUS}
//        Result := AnsiReplaceStr(AField.AsString, DecimalSeparator, '.')
//{$else}
//        Result := StringReplace(AField.AsString, DecimalSeparator, '.', [rfReplaceAll])
//{$endif}
//      else
//        Result := AField.AsString;
//    ftDate: begin
//      if FSQLiteDateFormat then
//        DateTimeFormat := 'yyyy"-"mm"-"dd'
//      else if TableDateFormat <> '' then
//        DateTimeFormat := TableDateFormat
//      else
//        DateTimeFormat := ShortDateFormat;
//      Result := QuotedStr(FormatDateTime(DateTimeFormat, AField.AsDateTime));
//    end;
//{$IFDEF ASQLITE_D6PLUS}
//    ftTimeStamp,
//{$ENDIF}
//    ftDateTime: begin
//      if FSQLiteDateFormat then
//        DateTimeFormat := 'yyyy"-"mm"-"dd" "hh":"nn":"ss"."zzz'
//      else if TableDateFormat <> '' then
//        DateTimeFormat := TableDateFormat
//      else
//        DateTimeFormat := ShortDateFormat + '" "' + LongTimeFormat;
//      Result := QuotedStr(FormatDateTime(DateTimeFormat, AField.AsDateTime));
//    end;
//    ftTime: begin
//      if FSQLiteDateFormat then
//        DateTimeFormat := 'hh":"nn":"ss"."zzz'
//      else if TableDateFormat <> '' then
//        DateTimeFormat := TableDateFormat
//      else
//        DateTimeFormat := LongTimeFormat;
//      Result := QuotedStr(FormatDateTime(DateTimeFormat, AField.AsDateTime));
//    end;
//    ftBlob, ftGraphic, ftMemo, ftFmtMemo, ftVariant: begin
//      if Blobs = nil then
//        raise EInvalidArgument.Create('No place to store a blob field');
//      MS := TMemoryStream.Create;
//      TBlobField(AField).SaveToStream(MS);
//      Result := #2 + IntToStr(1 + Blobs.Add(MS));
//    end
//    else
//      Result := QuotedStr(AField.AsString);
//  end;
//end; // GetFieldValue

{
   Unpack the buffer (if necessary) and convert it to a valid representation
   this is necessary for sqlite since it it typeless. If typed has been
   defined then the fields have to be converted to the appropiate datatype
}

procedure TASQLite3BaseQuery.UnpackBuffer(Buffer: PAnsiChar; FieldType: TFieldType; var RetBuffer : TConvertBuffer);
//function TASQLite3BaseQuery.UnpackBuffer(Buffer: PAnsiChar; FieldType: TFieldType): TConvertBuffer;
var
  TempInt           : integer;
  TempDouble        : double;
  TempBool          : wordbool;
  TempT             : TDateTimeRec;
  SaveDateFormat    : string;
begin
{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TASQLite3BaseQuery.UnpackBuffer: ' + Buffer);
{$ENDIF}
  case FieldType of
    ftString:
      begin
{$IFDEF DEBUG_VERY_LOUD}
        DebugLeave('TASQLite3BaseQuery.UnpackBuffer');
{$ENDIF}
        exit;
      end;
    ftInteger, ftSmallInt:
      begin
        TempInt := StrToIntX(Buffer);
//        Move(TempInt, result, sizeof(TempInt));
        Move(TempInt, RetBuffer, sizeof(TempInt));
      end;
    ftTime:
      begin
        if FSQLiteDateFormat then begin
           shorttimeformat := 'hh":"nn":"ss"."zzz';
           TempT := DateTimeToNative(FieldType, StrToTimeX(Buffer))                 // aducom 2006
        end else begin
           shorttimeformat := 'hh":"nn":"ss"';
           TempT := DateTimeToNative(FieldType, StrToTimeX(Buffer));                 // aducom 2006
        end;
//        Move(TempT, result, sizeof(TDateTime));
        Move(TempT, RetBuffer, sizeof(TDateTime));
      end;
    ftDate:
      begin
        if FSQLiteDateFormat then begin
           savedateformat := shortdateformat;
           shortdateformat := 'yyyy-mm-dd';
           if (Buffer = nil) or (Trim(buffer)='') then
              TempT := DateTimeToNative(FieldType, StrToDateX('1900-01-01'))
           else
              TempT := DateTimeToNative(FieldType, StrToDateX(Buffer));
           shortdateformat := savedateformat;
        end
        else if fTableDateFormat <> '' then begin
           savedateformat := shortdateformat;
           shortdateformat := fTableDateFormat;
           if (Buffer = nil) or (Trim(buffer)='') then
              TempT := DateTimeToNative(FieldType, StrToDateX('1900-01-01'))
           else
              TempT := DateTimeToNative(FieldType, StrToDateX(Buffer));
           shortdateformat := savedateformat;
        end
        else
           TempT := DateTimeToNative(FieldType, StrToDateX(Buffer));
//        Move(TempT, result, sizeof(TDateTime));
        Move(TempT, RetBuffer, sizeof(TDateTime));
      end;
    ftDateTime:
      begin
        if FSQLiteDateFormat then       // aducom
          TempT := DateTimeToNative(FieldType, YYYYMMDDParser(Buffer)) // jpierce
        else
          TempT := DateTimeToNative(FieldType, StrToDateTimeX(Buffer));
//        Move(TempT, result, sizeof(TDateTime));
        Move(TempT, RetBuffer, sizeof(TDateTime));
      end;

{$IFDEF ASQLITE_D6PLUS}
    ftTimeStamp:
      begin
        if FSQLiteDateFormat then       // aducom
           TSQLTimeStamp((@RetBuffer)^) := DateTimeToSQLTimeStamp(YYYYMMDDParser(Buffer)) // jpierce
        else
           TSQLTimeStamp((@RetBuffer)^) := DateTimeToSQLTimeStamp(StrToDateTimeX(Buffer));
      end;
    ftFloat, ftBCD, ftCurrency:
      begin
        TempDouble := StrToFloatX(FloatParser(Buffer));
//        Move(TempDouble, result, sizeof(TempDouble));
        Move(TempDouble, RetBuffer, sizeof(TempDouble));
      end;
{$ENDIF}

{$IFDEF ASQLITE_D6PLUS}
    ftBoolean:
      begin
        if not assigned(buffer) or (buffer^ = #0) then //Will not except on null fields, but will not detect a bad value when not null
          tempBool := false
        else
          TempBool := StrToBool(Buffer);
//        Move(TempBool, result, sizeof(TempBool));
        Move(TempBool, RetBuffer, sizeof(TempBool));
      end;
{$ENDIF}
    ftMemo, ftGraphic, ftBlob, ftFMTMemo, ftVariant: // pointer to stream
      begin
        TempInt := StrToInt(Buffer);
//        Move(TempInt, result, sizeof(TempInt));
        Move(TempInt, RetBuffer, sizeof(TempInt));
      end;
  end;
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TASQLite3BaseQuery.UnpackBuffer');
{$ENDIF}
end;

{ This method is called by TDataSet.Open and also when FieldDefs need to
  be updated (usually by the DataSet designer).  Everything which is
  allocated or initialized in this method should also be freed or
  uninitialized in the InternalClose method. }

constructor TASQLite3BaseQuery.Create(AOwner: TComponent);
begin
  DebugEnter('TASQLite3BaseQuery.Create');
  MaxStrLen := 0;
  FSQL          := TStringList.Create;
  FParams       := TParams.Create(Self);
  DetailList    := TList.Create;
  FConnection   := nil;
  FResult       := nil;
  GetMem(ResultStr, MaxBuf);
  SQLCursor := true;
  SQLiteDateFormat := true;
  TypeLess := false;
  ReadOnly := false;
  inherited;
  DebugLeave('TASQLite3BaseQuery.Create');
end;

function TASQLite3BaseQuery.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TASQLite3BlobStream.Create(Field as TBlobField, Mode);
end;

destructor TASQLite3BaseQuery.Destroy;
begin
  DebugEnter('TASQLite3BaseQuery.Destroy');
  Close;

  if Assigned(FSQL) then begin
    TStringList(FSQL).OnChange := nil;
    FSQL.Free;
  end;
  FSQL := nil;

  if Assigned(FParams) then
  begin
    FParams.Free;
    FParams := nil;
  end;

  if Assigned(DetailList) then
    DetailList.Free;
  DetailList := nil;

  if Assigned(FConnection) then
    FConnection := nil;

  if Assigned(ResultStr) then
    FreeMem(ResultStr);
  ResultStr := nil;

  if Assigned(FResult) then
    FResult.Free;
  FResult := nil;

  inherited;
  DebugLeave('TASQLite3BaseQuery.Destroy');
end;

procedure TASQLite3BaseQuery.StartTransaction;
begin
  if Assigned(FConnection) then
    FConnection.ExecStartTransaction(FTransActionType);
end;

procedure TASQLite3BaseQuery.StartDeferredTransaction;
begin
  if Assigned(FConnection) then
    FConnection.StartDeferredTransaction;
end;

procedure TASQLite3BaseQuery.StartImmediateTransaction;
begin
  if Assigned(FConnection) then
    FConnection.StartImmediateTransaction;
end;

procedure TASQLite3BaseQuery.StartExclusiveTransaction;
begin
  if Assigned(FConnection) then
    FConnection.StartExclusiveTransaction;
end;

procedure TASQLite3BaseQuery.Commit;
begin
  if Assigned(FConnection) then
    FConnection.Commit;
end;

procedure TASQLite3BaseQuery.RollBack;
begin
  if Assigned(FConnection) then
    FConnection.RollBack;
end;

//function TASQLite3BaseQuery.LocateNearest(const KeyFields: String; const KeyValues: Variant; Options: TLocateOptions): Boolean;
 //begin
 //end;

// implementation by J Bannon, implementing partial key too.
function TASQLite3BaseQuery.Locate(const KeyFields: string;
  const KeyValues: variant; Options: TLocateOptions): boolean;
//loCaseInsensitive, loPartialKey
var
  bOk               : boolean;
  i, j, p           : integer;
  Fields, SearchValue: string;          //Variable SearchValue added by bobmitch
  FieldList         : TStringList;
  DebugStr          : string;
  DoEnableControls  : boolean;
begin
  DebugEnter('TASQLite3BaseQuery.Locate ' + Keyfields);
  DoEnableControls := not ControlsDisabled; {used to determine whether to EnableControls at end of function}
  DisableControls;
  FieldList := TStringList.Create;
  bOk := false;
  try
    Fields := KeyFields;
    p := pos(';', Fields);
    while p > 0 do
    begin
      FieldList.Add(Copy(Fields, 1, p - 1));
      System.Delete(Fields, 1, p);
      p := pos(';', Fields);
    end;
    if Fields <> '' then
      FieldList.Add(Fields);

    First;
    for i := 1 to FResult.Data.Count do
    begin
      SetRecNo(i);
      bOk := true;
      for j := 0 to FieldList.Count - 1 do
      begin
        if loCaseInsensitive in Options then
        begin
          if FieldList.Count = 1 then
          begin                         //Lines 2303 - 2338 by bobmitch, replaces original lines 2303 - 2336
            SearchValue := VarToStr(KeyValues);
            if (loPartialKey in Options) and (Length(SearchValue) <= Length(FieldByName(FieldList[j]).AsString)) then
              bOk := AnsiCompareText(Copy(FieldByName(FieldList[j]).AsString, 1, Length(SearchValue)), SearchValue) = 0
            else
              bOk := AnsiCompareText(FieldByName(FieldList[j]).AsString, SearchValue) = 0
          end                           {end loCaseInsensitive in Options AND FieldList.Count = 1}
          else
          begin
            SearchValue := VarToStr(KeyValues[j]);
            if (loPartialKey in Options) and (Length(SearchValue) <= Length(FieldByName(FieldList[j]).AsString)) then
              bOk := AnsiCompareText(Copy(FieldByName(FieldList[j]).AsString, 1, Length(SearchValue)), SearchValue) = 0
            else
              bOk := AnsiCompareText(FieldByName(FieldList[j]).AsString, SearchValue) = 0
          end                           {end loCaseInsensitive in Options AND FieldList.Count greater than 1}
        end                             {end loCaseInsensitive in Options}
        else
        begin                           {begin loCaseInsensitive NOT in Options}
          if FieldList.Count = 1 then
          begin
            SearchValue := VarToStr(KeyValues);
            if (loPartialKey in Options) and (Length(SearchValue) <= Length(FieldByName(FieldList[j]).AsString)) then
              bOk := Copy(FieldByName(FieldList[j]).AsString, 1, Length(SearchValue)) = SearchValue
            else
              bOk := FieldByName(FieldList[j]).AsString = SearchValue
          end                           {end loCaseInsensitive NOT in Options AND FieldList.Count = 1}
          else
          begin
            SearchValue := VarToStr(KeyValues[j]);
            if (loPartialKey in Options) and (Length(SearchValue) <= Length(FieldByName(FieldList[j]).AsString)) then
              bOk := Copy(FieldByName(FieldList[j]).AsString, 1, Length(SearchValue)) = SearchValue
            else
              bOk := FieldByName(FieldList[j]).AsString = SearchValue
          end;                          {end loCaseInsensitive NOT in Options AND FieldList.Count greater than 1}
        end;                            {end loCaseInsensitive NOT in Options}
        if bOk = false then
          break;
      end;                              {end for j := 0 to FieldList.Count - 1}
      if bOk then
      begin
        break;
      end;
    end;                                {end for i := 1 to FResult.Data.Count}
    if bOk then
    begin
      Locate := true;
      DebugStr := 'TASQLite3BaseQuery.Locate true';
    end
    else
    begin
      Locate := false;
      DebugStr := 'TASQLite3BaseQuery.Locate false';
    end;
  finally
    FieldList.Free;
    if DoEnableControls then            {restore original state of the controls}
      EnableControls;
    DebugLeave(DebugStr);
  end;
end;


function TASQLite3BaseQuery.GetDataSource: TDataSource;

begin
  DebugEnter('TASQLite3BaseQuery.GetDataSource '+FSQL.Text);
  Result := FMasterSource;
  DebugLeave('TASQLite3BaseQuery.GetDataSource');
end;

procedure TASQLite3BaseQuery.SetSQLiteDateFormat(const Value: boolean);
begin
  FSQLiteDateFormat := Value;
end;

procedure TASQLite3BaseQuery.SetDataSource(Value: TDataSource);
begin
  DebugEnter('TASQLite3BaseQuery.SetDataSource');
  if IsLinkedTo(Value) then
    DatabaseError('circular references are not allowed', Self);
  FMasterSource := Value;
  DebugLeave('TASQLite3BaseQuery.SetDataSource');
end;

function TASQLite3BaseQuery.GetMasterFields: string;
begin
  DebugEnter('TASQLite3BaseQuery.GetMasterFields');
  Result := FMasterFields;              //FMasterLink.FieldNames;
  DebugLeave('TASQLite3BaseQuery.GetMasterFields');
end;

procedure TASQLite3BaseQuery.SetMasterFields(const Value: string);
begin
  DebugEnter('TASQLite3BaseQuery.SetMasterFields ' + Value);
  FMasterFields := Value;               //  FMasterLink.FieldNames := Value;
  DebugLeave('TASQLite3BaseQuery.SetMasterFields');
end;
 //Checks the State and Results a defined Buffer;

function TASQLite3BaseQuery.GetActiveBuffer(var Buffer: PAnsiChar): boolean;
begin
{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TASQLite3BaseQuery.GetActiveBuffer');
{$ENDIF}
  case State of
    dsBrowse: if IsEmpty then
        Buffer := nil
      else
        Buffer := ActiveBuffer;

    dsEdit: Buffer := ActiveBuffer;
    dsInsert: Buffer := ActiveBuffer;
    dsFilter: Buffer := ActiveBuffer;   //FFilterBuffer;
    dsCalcFields: Buffer := CalcBuffer;
  else
    Buffer := nil;
  end;
  Result := Buffer <> nil;
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TASQLite3BaseQuery.GetActiveBuffer ' + PAnsiChar(Buffer));
{$ENDIF}
end;

function TASQLite3BaseQuery.GetNativeFieldSize(FieldNo: integer): integer;
begin
  DebugEnter('TASQLite3BaseQuery.GetNativeFieldSize');
  Result := 0;
  case FieldDefs.Items[FieldNo - 1].Datatype of
    ftString: Result := FieldDefs.Items[FieldNo - 1].Size + 1;
    ftWideString: Result := (FieldDefs.Items[FieldNo - 1].Size + 2);
    ftInteger, ftSmallInt, ftDate, ftTime: Result := 12;
    ftDateTime: Result := 20;
{$IFDEF ASQLITE_D6PLUS}
    ftTimeStamp: inc(Result, 23);
{$ENDIF}
    ftFloat, ftBCD, ftCurrency: Result := 12;
    ftBoolean: Result := 12;
    ftGraphic, ftMemo, ftBlob, ftFmtMemo, ftVariant: Result := 12; // space for memory handles
  else
    raise AsgError.Create('Fieldtype of Field "' + FieldDefs.Items[FieldNo - 1].Name +
      '" not supported!');
  end;
  DebugLeave('TASQLite3BaseQuery.GetNativeFieldSize');
end;

function TASQLite3BaseQuery.GetFieldSize(FieldNo: integer): integer;
begin
{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TASQLite3BaseQuery.GetFieldSize');
{$ENDIF}
 // try
  Result := 0;
  if FieldNo > FieldDefs.Count then exit;

  case FieldDefs.Items[FieldNo - 1].Datatype of
    ftString: Result := FieldDefs.Items[FieldNo - 1].Size + 1 ;  // GPA - Warning UTF-8 length can be potentially > Ansi length
    ftWideString: Result := (FieldDefs.Items[FieldNo - 1].Size) +  2 ;
    ftInteger, ftSmallInt, ftDate, ftTime: Inc(Result, sizeof(integer));
    ftDateTime: Inc(Result, sizeof(TDateTime));
{$IFDEF ASQLITE_D6PLUS}
    ftTimeStamp: inc(Result, Sizeof(TTimeStamp));
{$ENDIF}
    ftFloat, ftBCD, ftCurrency: Inc(Result, sizeof(double));
    ftBoolean: Inc(Result, sizeof(wordbool));
    ftGraphic, ftMemo, ftBlob, ftFmtMemo, ftVariant: Inc(Result, sizeof(pointer));
  else
    raise AsgError.Create('Fieldtype of Field "' + FieldDefs.Items[FieldNo - 1].Name +
      '" not supported!');
  end;
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TASQLite3BaseQuery.GetFieldSize');
{$ENDIF}
end;

function TASQLite3BaseQuery.GetFieldSize(Field: TField): integer;
begin
{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TASQLiteBaseQuery.GetFieldSize');
{$ENDIF}
 // try
  Result := 0;
  case Field.DataType of
    ftString: Result := Field.Size + 1;
    ftWideString: Result := Field.Size + 2;
    ftInteger, ftSmallInt, ftDate, ftTime: Inc(Result, sizeof(integer));
    ftDateTime: Inc(Result, sizeof(TDateTime));
    ftFloat, ftBCD, ftCurrency: Inc(Result, sizeof(double));
    ftBoolean: Inc(Result, sizeof(wordbool));
    ftGraphic, ftMemo, ftBlob, ftFmtMemo, ftVariant: Inc(Result, sizeof(pointer));
  else
    raise AsgError.Create('Fieldtype of Field "' + Field.FieldName +
      '" not supported!');
  end;
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TASQLiteBaseQuery.GetFieldSize');
{$ENDIF}
end;

function TASQLite3BaseQuery.GetFieldOffset(FieldNo: integer): integer;
var
  i                 : integer;
  Offset            : integer;
begin
{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TASQLite3BaseQuery.GetFieldOffset');
{$ENDIF}
  Offset := 1;
  if FieldNo > 1 then
  begin
    for i := 1 to FieldNo - 1 do
      OffSet := OffSet + GetFieldSize(i) + 1; //Null marker skipped use isNullSrc to check for null
  end;
  GetFieldOffset := Offset;
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TASQLite3BaseQuery.GetFieldOffset');
{$ENDIF}
end;

function TASQLite3BaseQuery.isNullSrc(SrcBuffer: pAnsiChar; fieldNo : integer) : boolean;
begin
  result := (srcBuffer + getFieldOffset(fieldNo) - 1)^ = #0;
end;

function TASQLite3BaseQuery.isNullSrc(SrcBuffer: pWideChar; fieldNo : integer) : boolean;
begin
  result := (srcBuffer + getFieldOffset(fieldNo) - 1)^ = #0;
end;

procedure TASQLite3BaseQuery.setNullSrc(SrcBuffer: pAnsiChar; fieldNo : integer; aNull : boolean);
begin
  (srcBuffer + getFieldOffset(fieldNo) - 1)^ := chr(ord(not aNull));
end;

function TASQLite3BaseQuery.GetCalcFieldOffset(Field: TField): integer;
var
  i                 : integer;
  Offset            : integer;
begin

// calcfieldoffset is appended to record (after bookmarkinfo)

{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TASQLiteBaseQuery.GetCalcFieldOffset');
{$ENDIF}
  Offset := FRecBufSize + sizeof(TRecInfo); // startlocation of offsetbuffer
  for i := 0 to FieldList.Count - 1 do begin
    if AnsiCompareText(FieldList[i].FieldName, Field.FieldName) = 0 then begin
      GetCalcFieldOffset := Offset+1;
      exit;
    end;
    if (FieldList[i].Calculated) or (FieldList[i].Lookup) then // gorn 20070912
      OffSet := OffSet + GetFieldSize(Field)+3;//aidee
  end;
  GetCalcFieldOffset := Offset;
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TASQLiteBaseQuery.GetCalcFieldOffset');
{$ENDIF}
end;

procedure TASQLite3BaseQuery.SetSQL(const Value: TStrings);
begin
  DebugEnter('TASQLite3BaseQuery.SetSQL');
  Close;
  if Assigned(FSQL) then
    FSQL.Assign(Value)
  else
    FSQL := Value;
  DebugLeave('TASQLite3BaseQuery.SetSQL');
end;

procedure TASQLite3BaseQuery.LoadQueryData;
begin
  DebugEnter('TASQLite3BaseQuery.LoadQueryData');
  if Connection.FConnected then begin
    Connection.SQLite3_execute(Connection.DBHandle, PAnsiChar(FPrepared), FParams, self);
  end;
  DebugLeave('TASQLite3BaseQuery.LoadQueryData');
end;

procedure TASQLite3BaseQuery.InternalOpen;
begin
  DebugEnter('TASQLite3BaseQuery.InternalOpen');

{$IFDEF ASQLITE_D6PLUS}
  if UniDirectional then
     SetUnidirectional(true)
  else
     SetUniDirectional(false);
{$endif}
  MaxStrLen := 0;
  if (Connection = nil) then
  begin                                 // check to see if a valid database
    raise AsgError.Create('no database connection');
  end
  else
  begin

    if Connection.Connected = false then // open database if necessary
      Connection.Connected := true;     // trigger the 'dbconnect' event

    if (Connection.Connected) and (Connection.DLLHandle <> 0) then
      if Assigned(MasterSource) then
      begin                             // notify master about existance!
        if (MasterSource.DataSet <> nil) then
        begin
          if AnsiCompareText(Copy(MasterSource.DataSet.ClassName, 1, 9), 'TASQLite3') = 0 then
          begin
            TASQLite3BaseQuery(MasterSource.DataSet).RegisterDetailDataset(TASQLite3BaseQuery(Self));
          end
          else
          begin
            raise AsgError.Create('master dataset ' + MasterSource.DataSet.ClassName +
              ' is not of TSQLiteBaseQuery type');
            DebugLeave('TASQLite3BaseQuery.InternalOpen');
            exit;
          end;
        end
        else
        begin
          raise AsgError.Create('master dataset undefined');
          DebugLeave('TASQLite3BaseQuery.InternalOpen');
          exit;
        end;
      end;

    if FMaxResults <> 0 then
       FPrepared := FPrepared + ' limit ' + IntToStr(FMaxResults);

    if FStartResult <> 0 then
       FPrepared := FPrepared + ' offset ' + IntToStr(FStartResult);

    if FOrderBy <> '' then
       FPrepared := FPrepared + ' order by '+FOrderBy;

    if not FUniDir then begin
  { Load the result into a resultlist }
       if not FNoResults then
          FResult := TFResult.Create(Self);
       LoadQueryData;
    end else begin
       if not FNoResults then
          FResult := TFResult.Create(Self);
       FStatement := Connection.SQLite3_PrepareResult(Connection.DBHandle, PAnsiChar(FPrepared), self);
//       ptr := Connection.SQLite3_GetNextResult(Connection.DBHandle, FStatement, FParams,self);
    end;

    if FNoResults then begin
       Self.Close;
       exit;
    end;

  { Initialize our internal position.
    We use -1 to indicate the "crack" before the first record. }
    FCurRec := -1;

  { Initialize an offset value to find the TRecInfo in each buffer }
    FRecInfoOfs := MaxStrLen;

  { Calculate the size of the record buffers.
    Note: This is NOT the same as the RecordSize property which
    only gets the size of the data in the record buffer }
    FRecBufSize := FRecInfoOfs + SizeOf(TRecInfo)+1;//2007

  { Tell TDataSet how big our Bookmarks are (REQUIRED) }
    BookmarkSize := SizeOf(integer);

  { Initialize the FieldDefs }
    InternalInitFieldDefs;

  { Create TField components when no persistent fields have been created }
    if DefaultFields then
      CreateFields;

  { Bind the TField components to the physical fields }
    BindFields(true);

  end;
  DebugLeave('TASQLite3BaseQuery.InternalOpen');
end;

procedure TASQLite3BaseQuery.InternalClose;
begin
  DebugEnter('TASQLite3BaseQuery.InternalClose');

   if (FUniDir) and (FStatement <> nil) then begin // and (active) then begin *Peter Fejfar 20070917
      Connection.SQLite3_CloseResult(FStatement);
      FStatement := nil;
   end;

  if Assigned(FResult) then
  begin
    FResult.Free;
    FResult := nil;
  end;

  { Destroy the TField components if no persistent fields }
  if DefaultFields then
    DestroyFields;

  { Reset these internal flags }
  //  FLastBookmark := 0;
  FCurRec := -1;
  DebugLeave('TASQLite3BaseQuery.InternalClose');
end;

{ This property is used while opening the dataset.
  It indicates if data is available even though the
  current state is still dsInActive. }

function TASQLite3BaseQuery.IsCursorOpen: boolean;
begin
  Result := Assigned(FResult);
end;

procedure TASQLite3BaseQuery.OpenCursor(InfoQuery: Boolean);
begin  
  if InfoQuery then  
    Begin  
      if Assigned(FConnection) Then Begin  
        InternalOpen;
        InternalClose;  
      End;  
    End  
  else if State <> dsOpening then  
    inherited OpenCursor(InfoQuery);  
end;  

procedure TASQLite3BaseQuery.InternalInitFieldDefs;
begin
//  Just here for compatibility
end;

{ This is the exception handler which is called if an exception is raised
  while the component is being stream in or streamed out.  In most cases this
  should be implemented useing the application exception handler as follows. }

procedure TASQLite3BaseQuery.InternalHandleException;
begin
  DebugEnter('TASQLite3BaseQuery.InternalHandleException');
  ApplicationHandleException(Self);
  DebugLeave('TASQLite3BaseQuery.InternalHandleException');
end;

 { Bookmarks }
 { ========= }

{ In this sample the bookmarks are stored in the Object property of the
  TStringList holding the data.  Positioning to a bookmark just requires
  finding the offset of the bookmark in the TStrings.Objects and using that
  value as the new current record pointer. }

procedure TASQLite3BaseQuery.InternalGotoBookmark(Bookmark: Pointer);
var
  Index             : integer;
begin
  DebugEnter('TASQLite3BaseQuery.InternalGotoBookmark');
//  inherited;
  Index := FResult.IndexOf(TObject(PInteger(Bookmark)^));
  if Index <> -1 then
    FCurRec := Index
  else
    if not FUniDir then DatabaseError('Bookmark not found');
  DebugLeave('TASQLite3BaseQuery.InternalGotoBookmark');
end;

function TASQLite3BaseQuery.BookmarkValid(Bookmark: Pointer): boolean;
var
  Index             : integer;
begin
  DebugEnter('TASQLite3BaseQuery.BookmarkValid');
  Index := FResult.IndexOf(TObject(PInteger(Bookmark)^));
  if Index <> -1 then
    BookmarkValid := true
  else
    BookmarkValid := false;
  DebugLeave('TASQLite3BaseQuery.BookmarkValid');
end;

{ This function does the same thing as InternalGotoBookmark, but it takes
  a record buffer as a parameter instead }

procedure TASQLite3BaseQuery.InternalSetToRecord(Buffer: PAnsiChar);
begin
  DebugEnter('TASQLite3BaseQuery.InternalSetToRecord');
  InternalGotoBookmark(@PRecInfo(Buffer + FRecInfoOfs).Bookmark);
//  NotifySQLiteMasterChanged;
  DebugLeave('TASQLite3BaseQuery.InternalSetToRecord');
end;

{ Bookmark flags are used to indicate if a particular record is the first
  or last record in the dataset.  This is necessary for "crack" handling.
  If the bookmark flag is bfBOF or bfEOF then the bookmark is not actually
  used; InternalFirst, or InternalLast are called instead by TDataSet. }

function TASQLite3BaseQuery.GetBookmarkFlag(Buffer: PAnsiChar): TBookmarkFlag;
begin
  DebugEnter('TASQLite3BaseQuery.GetBookmarkFlag');
  Result := PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag;
  DebugLeave('TASQLite3BaseQuery.GetBookmarkFlag');
end;

procedure TASQLite3BaseQuery.SetBookmarkFlag(Buffer: PAnsiChar; Value: TBookmarkFlag);
begin
  DebugEnter('TASQLite3BaseQuery.SetBookmarkFlag');
  PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag := Value;
  DebugLeave('TASQLite3BaseQuery.SetBookmarkFlag');
end;

{ These methods provide a way to read and write bookmark data into the
  record buffer without actually repositioning the current record }

procedure TASQLite3BaseQuery.GetBookmarkData(Buffer: PAnsiChar; Data: Pointer);
begin
  DebugEnter('TASQLite3BaseQuery.GetBookmarkData for '+FSQL.Text);
  PInteger(Data)^ := PRecInfo(Buffer + FRecInfoOfs).Bookmark;
  DebugLeave('TASQLite3BaseQuery.GetBookmarkData');
end;

procedure TASQLite3BaseQuery.SetBookmarkData(Buffer: PAnsiChar; Data: Pointer);
begin
  DebugEnter('TASQLite3BaseQuery.SetBookmarkData');
  PRecInfo(Buffer + FRecInfoOfs).Bookmark := PInteger(Data)^;
  DebugLeave('TASQLite3BaseQuery.SetBookmarkData');
end;

 { Record / Field Access }
 { ===================== }

{ This method returns the size of just the data in the record buffer.
  Do not confuse this with RecBufSize which also includes any additonal
  structures stored in the record buffer (such as TRecInfo). }

function TASQLite3BaseQuery.GetRecordSize: word;
begin
  DebugEnter('TASQLite3BaseQuery.GetRecordSize');
  Result := MaxStrLen;
  DebugLeave('TASQLite3BaseQuery.GetRecordSize');
end;

{ TDataSet calls this method to allocate the record buffer.  Here we use
  FRecBufSize which is equal to the size of the data plus the size of the
  TRecInfo structure. }

function TASQLite3BaseQuery.AllocRecordBuffer: PAnsiChar;
begin
  DebugEnter('TASQLiteBaseQuery.AllocRecordBuffer');
  GetMem(Result, FRecBufSize + CalcFieldsSize + sizeof(TRecinfo) + 5);
  FillChar(Result^, FRecBufSize + CalcFieldsSize + sizeof(TRecinfo) + 5, 0);
//  FillChar(Result^, GetRecordSize+CalcFieldsSize+10, 0);
  DebugLeave('TASQLiteBaseQuery.AllocRecordBuffer ' + intToHex(cardinal(result), 8));
end;

{ Again, TDataSet calls this method to free the record buffer.
  Note: Make sure the value of FRecBufSize does not change before all
  allocated buffers are freed. }

procedure TASQLite3BaseQuery.FreeRecordBuffer(var Buffer: PAnsiChar);
begin
  DebugEnter('TASQLiteBaseQuery.FreeRecordBuffer ' + intToHex(cardinal(buffer), 8));
  try
    FreeMem(Buffer);                  //, FRecBufSize+CalcFieldsSize+sizeof(TRecinfo));
  except end;
//  Buffer := nil;
  DebugLeave('TASQLiteBaseQuery.FreeRecordBuffer');
end;

{ This multi-purpose function does 3 jobs.  It retrieves data for either
  the current, the prior, or the next record.  It must return the status
  (TGetResult), and raise an exception if DoCheck is True. }

function TASQLite3BaseQuery.GetRecord(Buffer: PAnsiChar; GetMode: TGetMode;
  DoCheck: boolean): TGetResult;
var
  ptr               : pointer;
begin
  DebugEnter('TASQLite3BaseQuery.GetRecord');
//  if Active then CheckBrowseMode;
  if (not (FUniDir)) and (FResult.Count < 1) then
    Result := grEOF
  else
  begin
    Result := grOK;
    case GetMode of
      gmNext:
        if FUniDir then begin
//          ptr := Connection.SQLite3_GetNextResult(Connection.DBHandle, FStatement, FParams,self);
//          if ptr <> nil then
//             Move(ptr^, Buffer^, MaxStrLen)
//          else
//             Result := grEOF;
        end else begin
           if FCurRec >= RecordCount - 1 then
              Result := grEOF
           else
             Inc(FCurRec);
        end;
      gmPrior: begin
                if FUniDir then
//                   Result := grOK
                   raise AsgError.Create('operation PRIOR not allowed on unidirectional dataset')
                else begin
                   if FCurRec <= 0 then
                      Result := grBOF
                   else
                      Dec(FCurRec);
                   end;
               end;
      gmCurrent:
        begin
          if (FCurRec < 0) or (FCurRec >= RecordCount) then
            Result := grError;
        end;
    end;
    if Result = grOK then
    begin
      if FUniDir then begin
          ptr := Connection.SQLite3_GetNextResult(Connection.DBHandle, FStatement, self);
          if ptr <> nil then begin
             Move(ptr^, ActiveBuffer^, MaxStrLen);
          end else Result := grEOF;
      end else begin
          ptr := FResult.GetData(FCurRec);
          if FResult.Count = 0 then
             InternalInitRecord(Buffer)
          else
             if ptr <> nil then Move(ptr^, Buffer^, MaxStrLen); // albert 17/11/2004
      end;

      with PRecInfo(Buffer + FRecInfoOfs)^ do
      begin
        BookmarkFlag := bfCurrent;
        Bookmark := FResult.GetBookMark(FCurRec);
      end;

      if CalcFieldsSize > 0 then
        GetCalcFields(Buffer)

    end
    else if (Result = grError) and DoCheck then
      DatabaseError('No Records');
  end;
  DebugLeave('TASQLite3BaseQuery.GetRecord: ' + Buffer);
end;

{ This routine is called to initialize a record buffer. }

procedure TASQLite3BaseQuery.InternalInitRecord(Buffer: PAnsiChar);
var
  i                 : integer;
  TempT             : TDateTimeRec;
  Stream            : TMemoryStream;
begin
  DebugEnter('TASQLite3BaseQuery.InternalInitRecord');

  for i := 0 to Fields.Count - 1 do
  begin
    if not ((Fields[i].Calculated) or (Fields[i].Lookup)) then begin
      setNullSrc(Buffer, i + 1, true); //Jure: field is null
      case FieldDefs.Items[i].Datatype of
        ftMemo, ftGraphic, ftBlob, ftFmtMemo, ftVariant: begin
            Stream := TMemoryStream.Create;
            Move(Pointer(Stream), (Buffer + GetFieldOffset(i + 1))^, sizeof(Pointer)+1); //2007
        end;
        ftString: PAnsiChar(Buffer + GetFieldOffset(i + 1))^ := #0;
        ftWideString: PWideChar(Buffer + GetFieldOffset(i + 1))^ := #0;
        ftBoolean: pBoolean(Buffer + GetFieldOffset(i + 1))^ := false;
        ftFloat: pFloat(Buffer + GetFieldOffset(i + 1))^ := 0;
        ftSmallInt: pSmallInt(Buffer + GetFieldOffset(i + 1))^ := 0;
        ftInteger: pInteger(Buffer + GetFieldOffset(i + 1))^ := integer(nil);
        ftCurrency: pFloat(Buffer + GetFieldOffset(i + 1))^ := 0;
        ftDate:
          begin
            TempT := DateTimeToNative(ftDate, now);
            Move(TempT, (Buffer + GetFieldOffset(i + 1))^, sizeof(TDateTime));
          end;
        ftTime:
          begin
            TempT := DateTimeToNative(ftTime, now);
            Move(TempT, (Buffer + GetFieldOffset(i + 1))^, sizeof(TDateTime));
          end;
        ftDateTime:
          begin
            TempT := DateTimeToNative(ftDateTime, now);
            Move(TempT, (Buffer + GetFieldOffset(i + 1))^, sizeof(TDateTime));
          end;
      end;
    end;
  end;

  DebugLeave('TASQLite3BaseQuery.InternalInitRecord');
end;

{ Here we copy the data from the record buffer into a field's buffer.
  This function, and SetFieldData, are more complex when supporting
  calculated fields, filters, and other more advanced features.
  See TBDEDataSet for a more complete example. }

function TASQLite3BaseQuery.GetFieldData(Field: TField; Buffer: Pointer): boolean;
var
  MyBuf : string;
  MyWideBuf : widestring;
  SrcBuffer         : PAnsiChar;
  Ansi_Len : integer;

  i : integer;
//  fld : string;
begin
{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TASQLite3BaseQuery.GetFieldData');
{$ENDIF}
    Result := true;                     // indicates NotNull

    if Field.FieldNo > 0 then begin
    // load masterfield data if there's a master-detail relationship
    // key-data should not be NULL!!

    if pos(UpperCase(Field.FieldName), UpperCase(FMasterFields)) > 0 then begin // Aducom 20070912
       for i := 0 to MasterSource.DataSet.Fields.Count - 1 do begin
          if pos(UpperCase(MasterSource.DataSet.Fields[i].DisplayName), UpperCase(FMasterFields))> 0 then begin
             Result := MasterSource.DataSet.GetFieldData(Field, Buffer);
//               Result := MasterSource.DataSet.GetFieldData(MasterSource.DataSet.Fields[i], Buffer);
             exit;
          end;
       end;
       exit;
    end;

    if GetActiveBuffer(SrcBuffer) then begin
      if (Assigned(Buffer)) and (Assigned(SrcBuffer)) then begin
        Move((SrcBuffer + GetFieldOffset(Field.FieldNo))^, Buffer^, GetFieldSize(Field.FieldNo));
        if Field.DataType = ftWideString then begin
           MyWideBuf := PWideChar(Buffer);
           ansi_len := Field.DataSize*2+2;
           Move(MyWideBuf[1], Buffer^, ansi_len);

        end else if Field.DataType = ftString then begin // GPA
           MyBuf := PChar(Buffer);
{$IFDEF ASQLITE_D6PLUS}
           if Connection.FUtf8 then begin
              ansi_len := Length(Utf8ToAnsi(MyBuf));
              Move(Utf8ToAnsi(MyBuf)[1], Buffer^, ansi_len); // GPA - Warning UTF-8 length can be potentially > Ansi length
              PAnsiChar(PAnsiChar(buffer) + ansi_len)^ := #0;
           end else
{$endif}
           begin
              Move(MyBuf[1], Buffer^, Length(MyBuf)); 
              PAnsiChar(PAnsiChar(Buffer) + GetFieldSize(Field.FieldNo))^ := #0; // dev
           end;
        end;
        if Field.DataType <> ftWideString then
           Result := not isNullSrc(SrcBuffer, Field.fieldNo) //Null marker
        else Result := true;
        exit;
      end;
      if Assigned(SrcBuffer) then
         Result := not isNullSrc(SrcBuffer, Field.fieldNo); //Null marker
//        if (Field.DataType <> ftDateTime)
//            and (Field.DataType <> ftDate) and (Field.DataType <> ftTime)       // aducom 2006
//            and ((SrcBuffer + GetFieldOffset(Field.FieldNo))^ = #0) then
//          Result := false
    end else begin
      if assigned(Buffer) then PAnsiChar(Buffer)^ := #0;
      Result := false;
    end;
  end else begin                        {calcfields}
    //Jure: Maybe I should also do something here?
    Result := GetActiveBuffer(SrcBuffer);
    if Result and (State in [dsBrowse, dsEdit, dsInsert, dsCalcFields, dsBlockRead]) then begin
      if (Assigned(Buffer)) then
        Move((SrcBuffer + GetCalcFieldOffset(Field))^, Buffer^, GetFieldSize(Field));
    end;
  end;
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TASQLite3BaseQuery.GetFieldData: ' + PAnsiChar(Buffer));
{$ENDIF}
end;

// The next two functions are added to increase compatibility with
// components that require it (like DevExpress)

function TASQLite3BaseQuery.GetFieldData(FieldNo: integer; Buffer: Pointer): boolean;
begin
  Result := GetFieldData(FieldByNumber(FieldNo), Buffer);
end;

{$IFDEF ASQLITE_D6PLUS}
//function TASQLite3BaseQuery.GetFieldData(Field: TField; Buffer: Pointer;
//  NativeFormat: boolean): boolean;
//begin
//  Result := GetFieldData(Field, Buffer);
//end;
{$ENDIF}

{ returns the field data back to callee }

procedure TASQLite3BaseQuery.SetFieldData(Field: TField; Buffer: Pointer);
var
  DestBuffer        : PAnsiChar;
  MyBuf             : string;
  MyWBuf            : widestring;
begin
//Jure!!!: There is probably something to be done here
  DebugEnter('TASQLite3BaseQuery.SetFieldData');
  GetActiveBuffer(DestBuffer);
  if (Field.FieldNo > 0) then begin
    if (Assigned(Buffer)) and (Assigned(DestBuffer)) then begin
      setNullSrc(DestBuffer, Field.FieldNo, false);
      if Field.DataType = ftWideString then begin
          MyWBuf := PWideChar(Buffer);
          Move(MyWBuf[1], (DestBuffer + GetFieldOffset(Field.FieldNo))^, Length(MyWBuf)+1) // GPA - Warning UTF-8 length can be potentially > Ansi length
      end else if Field.DataType = ftString then
        Begin // GPA
          MyBuf := PChar(Buffer);
{$IFDEF ASQLITE_D6PLUS}
          if Connection.FUtf8 then
             MyBuf := AnsiToUTF8(MyBuf);
{$endif}
        //Jure: Potential buffer overrun fix
          if length(MyBuf) > GetFieldSize(Field.FieldNo) - 1 then
             setLength(MyBuf, GetFieldSize(Field.FieldNo) - 1);
          if Length(MyBuf)>0 then
             Move(MyBuf[1], (DestBuffer + GetFieldOffset(Field.FieldNo))^, Length(MyBuf)+1) // GPA - Warning UTF-8 length can be potentially > Ansi length
          else begin
             MyBuf := #0;
             Move(MyBuf[1], (DestBuffer + GetFieldOffset(Field.FieldNo))^, Length(MyBuf)); // GPA - Warning UTF-8 length can be potentially > Ansi length
          end;
        End
      else
        Move(Buffer^, (DestBuffer + GetFieldOffset(Field.FieldNo))^, GetFieldSize(Field.FieldNo));
    end
    else if assigned (destBuffer) then
      setNullSrc(DestBuffer, Field.FieldNo, true);
  end else {fkCalculated, fkLookup}  begin
    if (State in [dsBrowse, dsEdit, dsInsert, dsCalcFields, dsBlockRead]) then begin
      if (Field.FieldNo < 0) then begin
        if (Assigned(Buffer)) and (Assigned(DestBuffer)) then
          Move(Buffer^, (CalcBuffer + GetCalcFieldOffset(Field))^, GetFieldSize(Field))
        else
          setNullSrc(DestBuffer, Field.FieldNo, true);
      end;
    end;
  end;

  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));
  DebugLeave('TASQLite3BaseQuery.SetFieldData');
end;

{ Record Navigation / Editing }
{ =========================== }

{ This method is called by TDataSet.First.  Crack behavior is required.
  That is we must position to a special place *before* the first record.
  Otherwise, we will actually end up on the second record after Resync
  is called. }

procedure TASQLite3BaseQuery.InternalFirst;
begin
  DebugEnter('TASQLite3BaseQuery.InternalFirst');
  FCurRec := -1;
  DebugLeave('TASQLite3BaseQuery.InternalFirst');
end;

{ Again, we position to the crack *after* the last record here. }

procedure TASQLite3BaseQuery.InternalLast;
begin
  DebugEnter('TASQLite3BaseQuery.InternalLast');
  FCurRec := FResult.Count;
  DebugLeave('TASQLite3BaseQuery.InternalLast');
end;

function TASQLite3BaseQuery.GetLastInsertRow: integer;
begin
  if Assigned(Connection) then
    result := Connection.SQLite3_LastInsertRow(Connection.DBHandle)
  else
    result := -1;
end;

{ This method is called by TDataSet.Post. }

procedure TASQLite3BaseQuery.InternalPost;
var
  ptr               : Pointer;
begin
  DebugEnter('TASQLite3BaseQuery.InternalPost');
  FSaveChanges := true;
  { For inserts, just update the data in the string list }
  if State = dsEdit then
  begin
    if FUniDir then
       Connection.SQLite3_GetNextResult(Connection.DBHandle, FStatement, self)
    else begin
       ptr := FResult.GetData(FCurrec);
       if ptr <> nil then
          move(ActiveBuffer^, ptr^, FRecBufSize); // albert 17/11/2004
    end;
  end
  else
  begin
    { If inserting (or appending), increment the bookmark counter and
      store the data }
    FResult.Insert(FCurRec, ActiveBuffer,
      Connection.SQLite3_LastInsertRow(Connection.DBHandle));
  end;
  DebugLeave('TASQLite3BaseQuery.InternalPost');
end;

{ This method is similar to InternalPost above, but the operation is always
  an insert or append and takes a pointer to a record buffer as well. }

procedure TASQLite3BaseQuery.InternalAddRecord(Buffer: Pointer; Append: boolean);
begin
  DebugEnter('TASQLite3BaseQuery.InternalAddRecord');
  if FReadOnly then
     raise AsgError.Create('Cannot write to a read-only dataset');
  FSaveChanges := true;
  if Append then
    InternalLast;
  Post;
  DebugLeave('TASQLite3BaseQuery.InternalAddRecord');
end;

procedure TASQLite3Table.InternalAddRecord(Buffer: Pointer; Append: boolean);
begin
 Inherited; 
end;

{ This method is called by TDataSet.Delete to delete the current record }

procedure TASQLite3BaseQuery.InternalDelete;
begin
  DebugEnter('TASQLite3BaseQuery.InternalDelete');
  FSaveChanges := true;
  FResult.Delete(FCurRec);
  if FCurRec >= FResult.Count then
    Dec(FCurRec);
  DebugLeave('TASQLite3BaseQuery.InternalDelete');
end;

 { Optional Methods }
 { ================ }

{ The following methods are optional.  When provided they will allow the
  DBGrid and other data aware controls to track the current cursor postion
  relative to the number of records in the dataset.  Because we are dealing
  with a small, static data store (a stringlist), these are very easy to
  implement.  However, for many data sources (SQL servers), the concept of
  record numbers and record counts do not really apply. }

function TASQLite3BaseQuery.GetRecordCount: longint;
begin
  DebugEnter('TASQLite3BaseQuery.GetRecordCount');
  Result := FResult.Count;
  DebugLeave('TASQLite3BaseQuery.GetRecordCount ' + IntToStr(Result));
end;

function TASQLite3BaseQuery.GetRecNo: longint;
begin
  DebugEnter('TASQLite3BaseQuery.GetRecNo');
  UpdateCursorPos;
  if (FCurRec = -1) and (RecordCount > 0) then
    Result := 1
  else
    Result := FCurRec + 1;
  NotifySQLiteMasterChanged;            //20040819
  DebugLeave('TASQLite3BaseQuery.GetRecNo');
end;

procedure TASQLite3BaseQuery.SetRecNo(Value: integer);
begin
  DebugEnter('TASQLite3BaseQuery.SetRecNo');
  if (Value >= 0) and (Value < FResult.Count + 2) then // value < resultetc
  begin
    FCurRec := Value - 1;
    Resync([]);
  end;
  DebugLeave('TASQLite3BaseQuery.SetRecNo');
end;

procedure TASQLite3BaseQuery.SetFiltered(Value: Boolean);
begin
  inherited;
end;

procedure TASQLite3BaseQuery.SetFilterText(const Value: string);
begin
  DebugEnter('TASQLite3BaseQuery.SetFilterText ' + Value);
  if Active then begin
     Close;
     inherited;
     Open;
  end else Inherited;
  DebugLeave('TASQLite3BaseQuery.SetFilterText');
end;

//function TASQLite3BaseQuery.SetQueryParams(InStr: string): string;
//var
//  i                 : integer;
//  TempParam         : string;
//  ThisDateFormat    : string;
//  DateOnlyFormat    : string;
//  TimeOnlyFormat    : string;
//begin
//  if FSQLiteDateFormat then begin
//    ThisDateFormat := 'yyyy"-"mm"-"dd hh":"nn":"ss"."zzz';
//    DateOnlyFormat := 'yyyy"-"mm"-"dd';
//    TimeOnlyFormat := 'hh":"nn":"ss"."zzz'
//  end else if (FTableDateFormat <> '') then begin
//    ThisDateFormat := FTableDateFormat;
//    DateOnlyFormat := FTableDateFormat;
//    TimeOnlyFormat := FTableDateFormat;
//  end else begin
//    ThisDateFormat := ShortDateFormat;
//    DateOnlyFormat := ShortDateFormat;
//    TimeOnlyFormat := ShortTimeFormat;
//  end;

//  for i := 0 to FParams.Count - 1 do begin
//    if (FParams.Items[i].DataType <> ftBlob) and
//      (FParams.Items[i].DataType <> ftGraphic) then begin
//      TempParam := Fparams.Items[i].AsString;
//      if (TempParam = '') and (FParams.Items[i].bound) then begin
//        InStr := StringReplace(Instr, '?', 'NULL', []);
//      end
//      else begin
//        //Here we'll replace legitimate '?' characters with an unprintable character
//        TempParam := StringReplace(TempParam, '?', #1, [rfReplaceAll]);
//
//        //Okay, we need to check string dates and times and other types...
//        case Params[i].DataType of
//          ftDate: TempParam := QuotedStr(FormatDateTime(DateOnlyFormat, Fparams[i].AsDateTime));
//          ftTime: TempParam := QuotedStr(FormatDateTime(TimeOnlyFormat, Fparams[i].AsDateTime));
//          ftDateTime: begin
//                  TempParam := QuotedStr(FormatDateTime(ThisDateFormat, Fparams[i].AsDateTime));
//          end;
//          ftFloat:    begin // solve decimal point comma isue as reported by jaime
//                  if DecimalSeparator <> '.' then
//                     TempParam := AnsiReplaceStr(Fparams[i].AsString, DecimalSeparator, '.');
//          end;
//{$IFDEF ASQLITE_D6PLUS}
//          ftFMTBcd: ; // do not quote these types!
//{$ENDIF}
//          ftSmallint, ftInteger, ftWord,
//          ftCurrency, ftBCD,
//          ftAutoInc, ftLargeint: // do not default quote these types!
//        else
//          // all other types will be quoted
//          TempParam := QuotedStr(TempParam);
//        end;
//
//        InStr := StringReplace(Instr, '?', TempParam, [rfIgnoreCase]);
//      end;
//    end else begin                      // BLOB !!
//        //Here we'll replace legitimate '?' characters with an unprintable character
//      InStr := StringReplace(Instr, '?', #2, [rfIgnoreCase]);
//    end;
//  end;
//    //Here we'll restore legitimate '?' characters
//  InStr := StringReplace(Instr, #1, '?', [rfReplaceAll]);
//  SetQueryParams := InStr;
//end;
// ============================================================================= TASQLite3 UPDATE SQL

constructor TASQLite3UpdateSQL.Create(AOWner: TComponent);
begin
  DebugEnter('TASQLite3UpdateSQL.Create');
  inherited Create(AOwner);
  FInsertSQL := TStringList.Create;
  FUpdateSQL := TStringList.Create;
  FDeleteSQL := TStringList.Create;
  DebugLeave('TASQLite3UpdateSQL.Create');
end;

destructor TASQLite3UpdateSQL.Destroy;
begin
  DebugEnter('TASQLite3UpdateSQL.Destroy');
  inherited;
  if Assigned(FInsertSQL) then
    FInsertSQL.Free;
  if Assigned(FUpdateSQL) then
    FUpdateSQL.Free;
  if Assigned(FDeleteSQL) then
    FDeleteSQL.Free;
  DebugLeave('TASQLite3UpdateSQL.Destroy');
end;

procedure TASQLite3UpdateSQL.SetInsertSQL(const Value: TStrings);
begin
  DebugEnter('TASQLite3UpdateSQL.SetInsertSQL');
  if Assigned(FInsertSQL) then
    FInsertSQL.Assign(Value)
  else
    FInsertSQL := Value;
  DebugLeave('TASQLite3UpdateSQL.SetInsertSQL');
end;

procedure TASQLite3UpdateSQL.SetUpdateSQL(const Value: TStrings);
begin
  DebugEnter('TASQLite3UpdateSQL.SetUpdateSQL');
  if Assigned(FUpdateSQL) then
    FUpdateSQL.Assign(Value)
  else
    FUpdateSQL := Value;
  DebugLeave('TASQLite3UpdateSQL.SetUpdateSQL');
end;

procedure TASQLite3UpdateSQL.SetDeleteSQL(const Value: TStrings);
begin
  DebugEnter('TASQLite3UpdateSQL.SetDeleteSQL');
  if Assigned(FDeleteSQL) then
    FDeleteSQL.Assign(Value)
  else
    FDeleteSQL := Value;
  DebugLeave('TASQLite3UpdateSQL.SetDeleteSQL');
end;
// ============================================================================= TASQLite3 QUERY

constructor TASQLite3Query.Create(AOwner: TComponent);
begin
  DebugEnter('TASQLite3Query.Create');
  inherited Create(AOwner);
//  FParams := TParams.Create(Self);
  TStringList(FSQL).OnChange := QueryChanged;
  DebugLeave('TASQLite3Query.Create');
end;

destructor TASQLite3Query.Destroy;
begin
  DebugEnter('TASQLite3Query.Destroy');

  if Assigned(FSQL) then
    TStringList(FSQL).OnChange := nil;

  inherited Destroy;
  DebugLeave('TASQLite3Query.Destroy');
end;

procedure TASQLite3Query.Notification(AComponent: TComponent; Operation: TOperation);
begin
{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TASQLite3Query.Notification');
{$ENDIF}
//  Application.ProcessMessages;
  if Assigned(AComponent) then
  begin
    if (Operation = opRemove) then begin
      if Assigned(FUpdateSQL) and (AComponent is TASQLite3UpdateSQL) then begin
        if TASQLite3UpdateSQL(AComponent) = FUpdateSQL then
          FUpdateSQL := nil;
      end else

        if Assigned(FConnection) then begin
          if (AComponent is TASQLite3DB) and
            (TASQLite3Db(AComponent) = FConnection) then begin
            Close;
            Connection := nil;
          end;
        end else

    end;
  end;
  inherited;
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TASQLite3Query.Notification');
{$ENDIF}
end;

procedure TASQLite3Query.QueryChanged(Sender: TObject);
begin
  DebugEnter('TASQLite3Query.QueryChanged');
  FNoResults := false;
  Close;
  if not FRawSQL then begin
    if assigned(FParams) then FParams.Clear; // new
    SQLStr := FParams.ParseSQL(SQL.Text, true)
  end else SQLStr := SQL.Text;
  DebugLeave('TASQLite3Query.QueryChanged');
end;

procedure TASQLite3Query.SetSQL(const Value: TStrings);
begin
  DebugEnter('TASQLite3Query.SetSQL');
  FNoResults := false;
  Close;
  if Assigned(FSQL) then
    FSQL.Assign(Value)
  else
    FSQL := Value;
//  FText := FParams.ParseSQL(SQL.Text, False);
  DebugLeave('TASQLite3Query.SetSQL');
end;

function TASQLite3Query.GetSQL: TStrings;
begin
  DebugEnter('TASQLite3Query.GetSQL');
  GetSQL := FSQL;
  DebugLeave('TASQLite3Query.GetSQL');
end;

procedure TASQLite3Query.InternalDelete;
var
  MySQL             : string;
  //TempSQL           : string;
  SQLStr            : string; // added by Donnie
  TheWord           : string;
  TableId           : string;
  //FieldId           : string;
  startpos          : integer;
  vartype           : integer;
//  p                 : integer;
//  Blobs             : TList; // added by Donnie
begin
 try
  DebugEnter('TASQLite3Query.InternalDelete');
  if FReadOnly then
    raise AsgError.Create('Cannot delete from a read-only dataset');

  if Connection.FConnected then
  begin
    if FAutoCommit then
      Connection.ExecStartTransaction(FTransactionType);

    if not Assigned(FUpdateSQL) then
    begin
      raise AsgError.Create('Missing TASQLite3UpdateSQL component');
      exit;
    end;
 //     MyFieldList := TStringList.Create;
 //     MyFieldValues := TStringList.Create;
//  Blobs := TList.Create;
    MySQL := FUpdateSQL.FDeleteSQL.Text;
    startpos := 1;

    TheWord := GetWord(MySQL, startpos, vartype); // delete
    if not SyntaxCheck(TheWord, 'delete') then
      raise AsgError.Create('Syntax error: "from" expected');


    TheWord := GetWord(MySQL, startpos, vartype); // from
    if not SyntaxCheck(TheWord, 'from') then
      raise AsgError.Create('Syntax error: "from" expected');

    Tableid := GetWord(MySQL, startpos, vartype); // tablename

    TheWord := GetWord(MySQL, startpos, vartype); // where
    if not SyntaxCheck(TheWord, 'where') then
      raise AsgError.Create('Syntax error: "where" expected');


    SQLStr := 'delete from ' + TableId + ' where '+Copy(MySQL, startpos, 9999);
//    SQL.Text := SQLStr; // trigger fieldlist 
    try
      Connection.SQLite3_ExecSQL(SQLStr, Fields);
      if FAutoCommit then
        Connection.Commit;
    except
      if FAutoCommit then begin
        Connection.RollBack;
        raise;
      end;
    end;
//  if Assigned(Blobs) then begin
//    for p := 0 to Blobs.Count - 1 do
//      TMemoryStream(Blobs.Items[p]).Free;
//    Blobs.Free;
//  end;
    inherited InternalDelete;
  end;
 finally
  DebugLeave('TASQLite3Query.InternalDelete');
 end;
end;

 //==============================================================================
 // This is probabely the most difficult thing about these components.
 // To be able to have a live resultset a tupdatequery must be used to
 // supply the correct sql on the events. In the internalpost the insert and
 // update are handled. The routine will take the given sql and remodel it
 // to a workable sql which is executed. Keep in mind that this routine
 // is far more difficult then the TASQLite3Table, since the last one is depending
 // on a unique rownumber, available in the resultset, which might not be
 // available to user queries
 // There are several syntaxes allowed:
 //
 // insert into table *
 // this will generate an insert statement for each field and values
 // i.e. insert into table a,b,c values :a, :b, :c;
 //
 // insert into table (a, b, c) values *
 // this will generate an insert statement like
 // insert into table (a, b, c) values (:a, :b, :c);
 //
 // insert into table (a, b, c) values (:a, :b, :c);
 // insert into table (a, b, c) values (:a, "bvalue", :c) etc.
 //
 // update table set * where <criteria>
 // this will generate a update for all fields like
 // update a=:a, b=:b, c=:c where <criteria>
 //
 //==============================================================================

procedure TASQLite3Query.InternalPost;
var
  i                 : integer;
  //p                 : integer;
  f : TField;
  startpos          : integer;
  MySQL             : string;
  TheWord           : string;
  TempSQL           : string;
  SQLStr            : string; // added by Donnie
  TableId           : string;
  FieldId           : string;
  varType           : integer;
  Blobs             : TList; // added by Donnie
begin
  DebugEnter('TASQLite3Query.InternalPost');
  if FReadOnly then
    raise AsgError.Create('Cannot post into a read-only dataset');

  for i := 0 to Fields.Count - 1 do begin
    f:= Fields[i];
    if f.Required and f.IsNull then // Sean change, raise error before sending to Sqlite
    raise EDatabaseError.CreateFmt('%s is required.', [f.DisplayName]);
  end;

  Blobs := nil; //Jure: Eliminates a warning
  try
    if not Connection.FConnected then
    begin
      DebugLeave('TASQLite3Query.InternalPost');
      exit;
    end;
    if FAutoCommit then
      Connection.ExecStartTransaction(FTransactionType);
    if not Assigned(FUpdateSQL) then
    begin
      DebugLeave('TASQLite3Query.InternalPost Exception');
      raise AsgError.Create('Missing TASQLite3UpdateSQL component');
    end;

    Blobs := TList.Create;
    if (State = dsEdit) and (FResult.Count > 0) then
    begin
      MySQL := FUpdateSQL.FUpdateSQL.Text;
      startpos := 1;
      TheWord := GetWord(MySQL, startpos, vartype); // update
      if not SyntaxCheck(TheWord, 'update') then
        exit;

      Tableid := GetWord(MySQL, startpos, vartype); // tablename

      TheWord := GetWord(MySQL, startpos, vartype); // set or '*'
      SQLStr := 'update '+ Tableid + ' set ';

      if TheWord = '*' then
      begin
        for i := 0 to FieldList.Count - 1 do
          SQLStr := SQLStr +FieldList[i].FieldName+'='+':'+ FieldList[i].FieldName+',';
        System.Delete(SQLStr, Length(SQLStr), 1);

        SQLStr := SQLStr + Copy(MySQL, startpos, 9999);
      end else begin
        if not SyntaxCheck(TheWord, 'set') then
           raise AsgError.Create('Syntax error: "set" expected');

        repeat
          FieldId := GetWord(MySQL, startpos, vartype); // fieldname
          SQLStr := SQLStr + FieldId;

          TheWord := GetWord(MySQL, startpos, vartype); // '='
          if not SyntaxCheck(TheWord, '=') then
             raise AsgError.Create('Syntax error: "=" expected')
          else SQLStr := SQLStr + '=';

          TheWord := GetWord(MySQL, startpos, vartype); // 2004-14-09 (rps) ':' or 'where'  --->
          if vartype = vtcDelimiter then //                                 <---
            TheWord := GetWord(MySQL, startpos, vartype); // fieldvalue
          if TheWord = '*' then
             SQLStr := SQLStr +':' + FieldId
          else
             SQLStr := SQLStr +':' + TheWord;

          TheWord := GetWord(MySQL, startpos, vartype); // , or 'where'
        until AnsiCompareText(TheWord, 'where') = 0;
      end;

      if not SyntaxCheck(TheWord, 'where') then
         raise AsgError.Create('Syntax error: "where" expected')
      else
         SQLStr := SQLStr + ' where ';


      SQLStr := SQLStr + Copy(MySQL, startpos, 9999);
      Connection.SQLite3_ExecSQL(SQLStr, Fields);
      inherited InternalPost;           // rework internals
    end
    else
    begin
    { If inserting (or appending), increment the bookmark counter and
      store the data. Sytax should be: insert into <table> * or
      insert into <table> (field, field) values (field, field) | *
      The sql is parsed and a new (valid) sql generated
    }
      MySQL := FUpdateSQL.FInsertSQL.Text;
      startpos := 1;
      TheWord := GetWord(MySQL, startpos, vartype); // insert
      if not SyntaxCheck(TheWord, 'insert') then
         raise AsgError.Create('Syntax error: "insert" expected');


      TheWord := GetWord(MySQL, startpos, vartype); // into
      if not SyntaxCheck(TheWord, 'into') then
         raise AsgError.Create('Syntax error: "into" expected');


      Tableid := GetWord(MySQL, startpos, vartype); // tablename
      SQLStr := 'insert into ' + TableId + ' (';

      TheWord := GetWord(MySQL, startpos, vartype); // ( or *
      if TheWord = '*' then begin

         TempSQL := '';
         for i := 0 to FieldList.Count - 1 do begin
             SQLStr := SQLStr + FieldList[i].FieldName +',';
             TempSQL := TempSQL + ':'+FieldList[i].FieldName +',';
         end;

        system.delete(SQLStr, Length(SQLStr),1);
        system.delete(TempSQL, Length(TempSQL),1);
        SQLStr := SQLStr + ') values ('+TempSQL+')';
      end else if TheWord = '(' then begin
        repeat
          TheWord := GetWord(MySQL, startpos, vartype); // fieldname
          SQLStr := SQLStr + TheWord +',';
          TheWord := GetWord(MySQL, startpos, vartype); // ',' or ')'
        until theword = ')';
        TheWord := GetWord(MySQL, startpos, vartype); // values
        TheWord := GetWord(MySQL, startpos, vartype); // '(' or '*'
        if TheWord = '*' then
           SQLStr := SQLStr + TempSQL
        else begin
          repeat
            TheWord := GetWord(MySQL, startpos, vartype); // ':' or fieldname
            if vartype = vtcDelimiter then begin
              TheWord := GetWord(MySQL, startpos, vartype); // fieldname !!
              SQLStr := SQLStr +':' + TheWord;
            end else
              SQLStr := SQLStr + TheWord;
            TheWord := GetWord(MySQL, startpos, vartype); // ',' or ')'
          until theword = ')';
          SQLStr := SQLStr +')';
        end;
      end else begin
        raise AsgError.Create('SQL macro syntax error on insertsql, expected ( or *');
      end;

      Connection.SQLite3_ExecSQL(SQLStr, Fields);
      if FResult.Count = 0 then
        Inc(FCurrec);
      inherited InternalPost;           // rework internals
    end;
    if FAutoCommit then
    begin
      try
        Connection.Commit;
      except
        Connection.RollBack;
        raise;
      end;
    end;
  finally

    if Assigned(Blobs) then begin
      for i := 0 to Blobs.Count - 1 do
        try
          TMemoryStream(Blobs.Items[i]).Free;
        except
        end;
      Blobs.Free;
    end;
  end;
  DebugLeave('TASQLite3Query.InternalPost');
end;

procedure TASQLite3Query.InternalClose;
begin
  DebugEnter('TASQLite3Query.InternalClose');
  FPrepared := '';
  inherited;
  DebugLeave('TASQLite3Query.InternalClose');
end;

procedure TASQLite3Query.InternalOpen;
//var
//  p                 : integer;
begin
  DebugEnter('TASQLite3Query.InternalOpen');
  if Trim(FSQL.Text) = '' then
  begin
    raise AsgError.Create('no query specified');
    abort;
  end;

  if (FMaxResults = 0) and (FStartResult <> 0) then
    FMaxResults := -1;

 // SQLStr contains the 'raw' interpreted SQL, with ? as parameterlist
 // This string has to be preserved, since it was parsed on entering the sql.
 // On close and open (i.e. in case of master-detail) the parsed data still
 // must be available

 // We'll prepare the SQL statement into FPrepared. This is also the var
 // containing the SQL statement to be executed.
  FPrepared := Trim(Pchar(SQLStr));

  if (Filtered) and (Filter <> '') then
  begin
    //in order to let a filter work we use a little trick:
    //select * from (my select statement)
    FPrepared := 'select * from (' + FPrepared + ') where ' + Filter;
  end;

//  if FParams.Count > 0 then
//    FPrepared := SetQueryParams(FPrepared);

  inherited;
  DebugLeave('TASQLite3Query.InternalOpen');
end;

 // =============================================================================
 // The master-detail is implemented through the filter object
 // in the future perhaps a separate filter object will be used allowing
 // to add your own criteria too, but for the time being..
 //==============================================================================

procedure TASQLite3Query.SQLiteMasterChanged;
begin
 inherited;
end;

 //==============================================================================
 // execsql is used for sql statements which do not require cursors. For this
 // reason the fnoresults is set, to prevent building a result set
 //==============================================================================

procedure TASQLite3BaseQuery.ExecSQL;
begin
  DebugEnter('TASQLite3BaseQuery.ExecSQL');
  FNoResults := true;

  if not Connection.FConnected then
     Connection.Connected := true;

  Close;
  if FAutoCommit then
  begin
    Connection.ExecStartTransaction(FTransactionType);
    Open;
    try
      Connection.Commit
    except
      Connection.RollBack;
      raise;
    end;
  end
  else begin
     Connection.SQLite3_ExecSQL_Params(FSQL.Text, FParams);
  end;

//  Open;
  DebugLeave('TASQLite3BaseQuery.ExecSQL');
end;

procedure TASQLite3BaseQuery.SetParamsList(Value: TParams);
begin
  DebugEnter('TASQLite3BaseQuery.SetParamsList');
  FParams.AssignValues(Value);
  DebugLeave('TASQLite3BaseQuery.SetParamsList');
end;

function TASQLite3BaseQuery.GetParamsCount: word;
begin
  DebugEnter('TASQLite3BaseQuery.GetParamsCount');
  Result := FParams.Count;
  DebugLeave('TASQLite3BaseQuery.GetParamsCount');
end;

procedure TASQLite3Table.SetFOrderBy(OrderBy : string);
begin
 if FOrderBy <> OrderBy then begin
    Close;
    FOrderBy := OrderBy;
 end;
end;

procedure TASQLite3Table.SetFTableName(TableName : string);
begin
  Close;
  FTableName := TableName;
end;

procedure TASQLite3Table.Notification(AComponent: TComponent; Operation: TOperation);
begin
{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TASQLite3Table.Notification');
{$ENDIF}
//  Application.ProcessMessages;
  if Assigned(AComponent) then
  begin
    if (Operation = opRemove) then
    begin
      if (AComponent is TASQLite3DB) and Assigned(FConnection) then
      begin
        if TASQLite3DB(AComponent) = FConnection then begin
          Close;
          FConnection := nil;
        end;
      end else

    end;
  end;
  inherited;
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TASQLite3Table.Notification');
{$ENDIF}
end;

procedure TASQLite3Table.InternalOpen;
begin
  DebugEnter('TASQLite3Table.InternalOpen');

  if FTableName = '' then
  begin
    raise AsgError.Create('no table specified');
    exit;
  end;
  FSQL.Clear;
  FSQL.Add('select *, rowid as rowid from ' + TableName);
  if Filtered then
    if Filter <> '' then
      FSQL.Add(' where ' + Filter);
  if (FMaxResults = 0) and (FStartResult <> 0) then
    FMaxResults := -1;
  //if FMaxResults <> 0 then
//    FSQL.Add(' limit ' + IntToStr(FMaxResults));
//  if FStartResult <> 0 then
//    FSQL.Add(' offset ' + IntToStr(FStartResult));
//  if FOrderBy <> '' then
//    FSQL.Add(' order by '+FOrderBy);
  SQLStr := FSQL.Text;
  FPrepared := Trim(Pchar(SQLStr));
  inherited;

  DebugLeave('TASQLite3Table.InternalOpen');
end;

procedure TASQLite3Table.SQLiteMasterChanged;
begin
  inherited;
  DebugLeave('TASQLite3Table.SQLiteMasterChanged');
end;

procedure TASQLite3Table.InternalDelete;
begin
  DebugEnter('TASQLite3Table.InternalDelete');
  if FReadOnly then
    raise AsgError.Create('Cannot delete from a read-only dataset');

  if not Connection.FConnected then
    exit;
  if FAutoCommit then
    Connection.ExecStartTransaction(FTransactionType);

  SQLStr := '';
  CurrentRowId := FResult.GetRowId(FCurRec);
  FSQL.Clear;
  FSQL.Add('delete from ' + Tablename + ' where rowid=' + QuotedStr(IntToStr(CurrentRowId)));
//  SQLStr := StringReplace(FSQL.Text, crlf, #10, [rfReplaceAll, rfIgnoreCase]); // albert
  SQLStr := FSQL.Text;
  Connection.SQLite3_execute(Connection.DBHandle, PAnsiChar(SQLStr), FParams, self);

  inherited InternalDelete;

  if FAutoCommit then
  begin
    try
      Connection.Commit;
    except
      Connection.RollBack;
      raise;
    end;
  end;
  DebugLeave('TASQLite3Table.InternalDelete');
end;

procedure TASQLite3Table.InternalPost;
var
  i : integer;
  n: Integer;
  ThisDateFormatLong, ThisDateFormatShort : string;
  tmpMasterDetail, MasterField, chDelim: string;
  slDetail, slValues: TStringList;
  lsBlobs: TList; //GPA
  M:TMemoryStream; //GPA

  // this function will return the fielvalue of an indicated fieldbyordinalnumber
  // if the fieldtype is tdatetime it is transfered to the right date notation as
  // indicated by jpierce.

//  function GetFieldValue(const AField: TField): ansistring; // DI
//  var MyStr : AnsiString;
//      Temp : WideString;
//  begin // DI
//    if (FieldByName(AField.FieldName).isNull) and (AField.IsBlob = false)then  // even if blob is null then the pointer to it will not be...
//      result := 'null'
//    else begin
//      if AField.DataType = ftDate then //Jure - Errors with date conversions. How can DUnit tests go through an any version of null support?
//        result := QuotedStr(FormatDateTime(ThisDateFormatShort, FieldByName(AField.FieldName).AsDateTime)) // DI
//      else if AField.DataType = ftDateTime then // DI
//        result := QuotedStr(FormatDateTime(ThisDateFormatLong, FieldByName(AField.FieldName).AsDateTime)) // DI
//      else if (AField.DataType = ftBlob) or (AField.DataType = ftMemo) or
//              (AField.DataType = ftFmtMemo) or (AField.DataType = ftGraphic) or
//              (AField.DataType = ftVariant) then //GPA
//        begin //GPA
//          M:=TMemoryStream.Create; //GPA
//          TBlobField(FieldByName(AField.FieldName)).SaveToStream(M);
//          result := #2+IntToStr(1+lsBlobs.Add(Pointer(M))) //GPA
//        end
//      else if (AField.DataType=ftWideString) then
//       begin
//         Temp := AFIeld.Value;
//         Result := Temp;
//        result := AnsiQuotedStr(FieldByName(AField.FieldName).AsString,''''); // DI
//       end
//      else
//        result := AnsiQuotedStr(FieldByName(AField.FieldName).AsString,''''); // DI
//    end;
//  end; // DI

var
  f: TField; // DI
  OldDecimalSeparator: ansiChar; // DI
begin
  DebugEnter('TASQLite3Table.InternalPost');

  if FReadOnly then
    raise AsgError.Create('Cannot post into a read-only dataset');

  for i := 0 to Fields.Count - 1 do begin
   f:= Fields[i];
   if f.Required and f.IsNull then // Sean change, raise error before sending to Sqlite
   raise EDatabaseError.CreateFmt('%s is required.', [f.DisplayName]);
  end;

  // determine datetime style of dataset (if any)

  if FSQLiteDateFormat then begin
    ThisDateFormatLong  := 'yyyy-mm-dd hh:nn:ss.zzz';
    ThisDateFormatShort := 'yyyy-mm-dd';
  end
  else if (FTableDateFormat <> '') then begin
    ThisDateFormatLong  := FTableDateFormat;
    ThisDateFormatShort := FTableDateFormat;
  end
  else begin
    ThisDateFormatLong  := ShortDateFormat;
    ThisDateFormatShort := ShortDateFormat;
  end;

  if not Connection.FConnected then Exit;
  if FAutoCommit then Connection.StartTransaction;

  lsBlobs := TList.Create; //GPA
  OldDecimalSeparator := DecimalSeparator; // DI
  try // DI
    DecimalSeparator := '.'; // DI: Force Delphi's DecimalSeparator to SQL style syntax.

    if (State = dsEdit) and (FResult.Count > 0) then
      begin
        CurrentRowId := FResult.GetRowId(FCurRec);
        FSQL.Clear;
        FSQL.Add('update ' + TableName + ' set ');
        SQLStr := '';
        for i := 0 to FieldList.Count - 1 do begin
            f := FieldList[i]; // DI
            if not (f.Calculated or f.Lookup) then // DI
              SQLStr := SQLStr + f.FieldName + '=:'+f.FieldName+','; // DI
          end;
        SQLStr[Length(SQLStr)] := ' ';
        FSQL.Add(SQLStr);
        FSQL.Add(' where rowid=' + QuotedStr(IntToStr(CurrentRowId)));

        SQLStr := FSQL.Text; // DI
        // DI SQLStr := StringReplace(FSQL.Text, CRLF, #10, [rfReplaceAll, rfIgnoreCase]);

        Connection.SQLite3_ExecSQL(SQLStr,Fields);
        inherited InternalPost; // rework internals
      end
    else
      begin
        { If inserting (or appending), increment the bookmark counter and
        store the data }
        SQLStr := 'insert into ' + TableName + ' (';

        for i := 0 to FieldList.Count - 1 do begin
            if not (FieldList[i].Calculated or FieldList[i].Lookup) then // aducom
              SQLStr := SQLStr + FieldList[i].FieldName + ',';
          end;

        SQLStr[Length(SQLStr)] := ')';
        SQLStr := SQLStr + ' values (';

        slDetail := TStringList.Create;
        slValues := TStringList.Create;
        i := 0; chDelim := ';';
        if FMasterSource <> nil then
          begin
            while i < Length(FMasterFields) do
              begin
                tmpMasterDetail := GetWordByDelim(FMasterFields, i, chDelim);
                n := Pos('=', tmpMasterDetail);
                if n <> 0 then
                  begin
                    slDetail.Add(Copy(tmpMasterDetail, 1, n - 1));
                    MasterField := Copy(tmpMasterDetail, n + 1, Length(tmpMasterDetail) - n);
                    slValues.Add(FMasterSource.DataSet.FieldByName(MasterField).AsString);
                  end else inc(i);        //200801
              end;
          end;

        if (FPrimaryAutoInc) and (FieldDefs[0].DataType = ftInteger) then begin
          SQLStr := SQLStr + 'null,';
          n := 1;                                                               // aducom
        end else begin                                                          // aducom
          n := 0;                                                               // aducom
        end;                                                                    // aducom

        for i := n to FieldList.Count - 1 do                                    // aducom
          begin // DI
            f := FieldList[i];
            if not (f.Calculated or f.Lookup) then // DI
              if slDetail.Find(f.FieldName, n) then // DI
                SQLStr := SQLStr + QuotedStr(slValues.Strings[n]) + ','
              else
                SQLStr := SQLStr + ':'+f.FieldName+','; // DI
          end; // DI

        slDetail.Free;
        slValues.Free;

        SQLStr[Length(SQLStr)] := ')';

        Connection.SQLite3_ExecSQL(SQLStr, Fields);
        if FPrimaryAutoInc then
//          if FieldDefs[0].DataType = ftInteger then
            FieldByName(FieldList[0].FieldName).AsInteger := Connection.SQLite3_LastInsertRow(Connection.DBHandle);
        if FResult.Count = 0 then
          Inc(FCurRec);
        inherited InternalPost; // rework internals
      end;

  finally // DI
    DecimalSeparator := OldDecimalSeparator; // DI
    For I:=0 to lsBlobs.Count-1 do begin // GPA
      M:=TMemoryStream(lsBlobs.Items[I]); // GPA
      M.Free; // GPA
    end; // GPA
    lsBlobs.Free; // GPA

  end; // DI

  if FAutoCommit then
    begin
      try
        Connection.Commit;
      except
        Connection.RollBack;
        raise;
      end;
    end;
  DebugLeave('TASQLite3Table.InternalPost');
end;

// Blobfields in SQLite are in fact CLOB fields. However, since it is a large
// chunk of data for all types the ftBlob is used. Keep in mind that blobs are
// stored separately of TResult. Within the result structure only the memory
// handle of the blob is stored.

constructor TASQLite3BlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
//  inherited Create;
  FField := Field;
  FMode := Mode;
  FDataSet := FField.DataSet as TASQLite3BaseQuery;
  if Mode <> bmWrite then
    LoadBlobData;
end;

destructor TASQLite3BlobStream.Destroy;
begin
  DebugEnter('TASQLite3BlobStream.Destroy');
  if FModified then
    SaveBlobData;
  inherited Destroy;
end;

function TASQLite3BlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  DebugEnter('ASQLiteBlobStream.Read');
  Result := inherited Read(Buffer, Count);
  FOpened := True;
end;

function TASQLite3BlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  DebugEnter('ASQLiteBlobStream.Write');
  Result := inherited Write(Buffer, Count);
  FModified := True;
  FDataSet.SetModified(true);
end;

procedure TASQLite3BlobStream.LoadBlobData;
var
  Stream            : TMemoryStream;
  Offset            : Integer;
  RecBuffer         : PAnsiChar;
begin
  DebugEnter('ASQLiteBlobStream.LoadBlobData');
  Self.Size := 0;
  FDataset.GetActiveBuffer(RecBuffer);

//  recbuffer := nil;

  if RecBuffer <> nil then
  begin
    Offset := FDataset.GetFieldOffset(FField.FieldNo);
    Move((RecBuffer + Offset)^, Pointer(Stream), sizeof(Pointer));
    Self.CopyFrom(Stream, 0);
  end;
  Position := 0;
end;

procedure TASQLite3BlobStream.SaveBlobData;
var
  Stream            : TMemoryStream;
  Offset            : Integer;
  RecBuffer         : PAnsiChar;
begin
  DebugEnter('ASQLiteBlobStream.SaveBlobData');
  FDataset.GetActiveBuffer(RecBuffer);
  if RecBuffer <> nil then
  begin
    Offset := FDataset.GetFieldOffset(FField.FieldNo);
    Move((RecBuffer + Offset)^, Pointer(Stream), sizeof(Pointer));
    Stream.Size := 0;
    Stream.CopyFrom(Self, 0);
    Stream.Position := 0;
  end;
end;

// Inline sql can be used to store sqlstatements outside of the pascal source.
// it prevents large 'sql.add' rows. Also it can be used to generate an in-memory
// database structure if needed

constructor TASQLite3InlineSQL.Create;
begin
  inherited;
  FSQL := TStringList.Create;
end;

destructor TASQLite3InlineSQL.Destroy;
begin
  if Assigned(FSQL) then FSQL.Free;
  inherited;
end;

procedure TASQLite3InlineSQL.SetSQL(const Value: TStrings);
begin
  if Assigned(FSQL) then
    FSQL.Assign(Value)
  else
    FSQL := Value;
end;

function TASQLite3InlineSQL.GetSQL: TStrings;
begin
  GetSQL := FSQL;
end;

// save resultset as text, html or xml. Depending on type the following
// will happen:
//
// text: all rows will be output, separated by the given separation symbol
// xml: all rows will be output, tags are the fieldnames
//      <queryname>
//              <rowid>
//                      <fieldid> fieldvalue </fieldid>
//                      ....
//
// html: a table will be generated with the given classnames (if available)

constructor TASQLite3Output.Create;
begin
  inherited;
  FOutput := TStringList.Create;
end;

destructor TASQLite3Output.Destroy;
begin
  if Assigned(FOutput) then FOutput.Free;
  inherited;
end;

procedure TASQLite3Output.SetFActive(Active: boolean);
begin
  FActive := Active;
  if FActive = false then begin
  end else begin
    if Assigned(FDataSource) then begin
      if Assigned(FDataSource.DataSet) then begin
        Execute(FDataSource.DataSet);
      end else raise AsgError.Create('Missing Datasource.Dataset');
    end else raise AsgError.Create('Missing Datasource');
  end;
end;

procedure TASQLite3Output.SetOutput(const Value: TStrings);
begin
  if Assigned(FOutput) then
    FOutput.Assign(Value)
  else
    FOutput := Value;
end;

function TASQLite3Output.GetOutput: TStrings;
begin
  GetOutput := FOutput;
end;

procedure TASQLite3Output.Notification(AComponent: TComponent; Operation: TOperation);
begin
{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TASQLite3Output.Notification');
{$ENDIF}
  if Assigned(AComponent) then begin
    if (Operation = opRemove) then begin
      if (AComponent is TDataSource) then begin
        if Assigned(FDataSource) then begin
          if TDataSource(AComponent) = FDataSource then
            FDataSource := nil;
        end;
      end
    end;
  end;
  inherited;
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TASQLite3DB.Notification');
{$ENDIF}
end;

procedure TASQLite3Output.Execute(MyDataSet: TDataSet);
const eXML          = 0;
  eHTML             = 1;
  eTXT              = 2;
var FType           : integer;
  i                 : integer;
  Line              : string;
  Sep               : string;
//    Indent : integer;
begin
  if Assigned(MyDataset) then begin
    if MyDataSet.Active = false then MyDataSet.Open;
    Output.Clear;
    FType := ETxt;
    Line := '';

    if AnsiCompareText(FOutputType[1], 'X') = 0 then begin
      FType := eXML;
      Line := Line + '<table id="' + MyDataSet.Name + '" CreationDate="' + DateToStr(Now) + '">' + #10;
    end else if AnsiCompareText(FOutputType[1], 'H') = 0 then begin
      FType := eHTML;
      Line := Line + '<html>' + #10 + '<head>' + #10 +
        '<title>Table ' + MyDataSet.Name + '</title>' + #10 +
        '<meta name="generator" content="Aducom Software">' + #10 +
        '<table class="' + FTableClass + '"cellpadding="0" cellspacing="0">' + #10 +
        '<tr>' + #10;
    end else if AnsiCompareText(FOutputType[1], 'T') = 0 then begin
      FType := eTXT;
    end;

    Sep := '';

    for i := 0 to MyDataSet.FieldDefs.Count - 1 do begin
      case FType of
        eXML: begin
          end;
        eHTML: begin
            Line := Line + '<td>' + MyDataSet.FieldDefs[i].Name + '</td>';
          end;
        eTXT: begin
            Line := Line + Sep + MyDataSet.FieldDefs[i].Name;
          end;
      end;
      Sep := FSeparator;
    end;

    Output.Add(Line); Line := ''; Sep := '';
    MyDataSet.First;

//    Indent := 0;
    while not MyDataSet.Eof do begin

      case FType of
        eXML: Line := Line + '   <record>' + #10;
        eHTML: Line := Line + '<tr>' + #10;
      end;

      for i := 0 to MyDataSet.FieldDefs.Count - 1 do begin
        case FType of
          eXML: begin
              Line := Line + '      <' + MyDataSet.FieldDefs[i].Name + '>' +
                MyDataSet.FieldByName(MyDataSet.FieldDefs[i].Name).AsString +
                '</' + MyDataSet.FieldDefs[i].Name + '>' + #10;
            end;
          eHTML: begin
              Line := Line + '<td>' + MyDataSet.FieldByName(MyDataSet.FieldDefs[i].Name).AsString + '</td>';
            end;
          eTXT: begin
              Line := Line + Sep + MyDataSet.FieldByName(MyDataSet.FieldDefs[i].Name).AsString;
            end;
        end;
        Sep := FSeparator;
      end;

      case FType of
        eXML: Line := Line + '   </record>' + #10;
        eHTML: Line := Line + '</tr>' + #10;
      end;

      Output.Add(Line); Line := ''; Sep := '';
      MyDataSet.Next;
    end;

    case FType of
      eXML: Line := Line + '   </table>' + #10;
      eHTML: Line := Line + '</table>' + #10 + '</body>' + #10 + '</html>' + #10;
    end;
    Output.Add(Line);
  end;
end;

{$IFDEF IPROVIDER}
procedure TASQLite3BaseQuery.PSEndTransaction(Commit: Boolean);
begin
  // qui non sono molto sicuro...
  if Assigned(Connection) then
    if Commit then
      Connection.Commit
    else
      Connection.RollBack;
end;

procedure TASQLite3BaseQuery.PSExecute;
begin
  UniDirectional := true;
  ExecSQL;
end;

function TASQLite3BaseQuery.PSExecuteStatement(const ASQL: string;
  AParams: TParams; ResultSet: Pointer): Integer;
var
  AsqlQry           : TASQLite3BaseQuery;
begin
  if Assigned(ResultSet) then
  begin
    TDataSet(ResultSet^) := TASQLite3Query.Create(nil);
{$IFDEF ASQLITE_D6PLUS}
    TASQlite3Query(ResultSet^).SetUniDirectional(true);// := true; // just store data in provider.
{$endif}
    with TASQLite3BaseQuery(ResultSet^) do begin
      Connection := self.Connection;
      FSQL.Text := ASql;
      FParams.Assign(AParams);
      Open;
      Result := Connection.RowsAffected;
    end;
  end
  else
  begin
    AsqlQry := TASQLite3BaseQuery.Create(nil);
    try
      with AsqlQry do begin
        Connection := self.Connection;
        FSQL.Text := ASql;
        FParams.Assign(AParams);
        ExecSQL;
        Result := Connection.RowsAffected;
      end;
    finally
      AsqlQry.Free;
    end;
  end;
end;

function TASQLite3BaseQuery.GetFieldBufferIndex(Field: TField): integer;
var i: integer;
begin
  if Field.FieldKind = fkData then
    result:= Field.FieldNo - 1
  else
  begin
    result:= FieldDefs.Count;
    for i := 0 to Field.Index - 1 do begin
      if Fields[i].FieldKind <> fkData then
        inc(result);
    end;
  end;
  Assert((result >= 0) and (result < MAX_FIELDS), 'Invalid result in GetFieldBufferIndex: ' + IntToStr(result));
end;

function TASQLite3BaseQuery.PSGetParams: TParams;
begin
  Result := Params;
end;

function TASQLite3BaseQuery.PSGetQuoteChar: string;
begin
  Result := '"';
end;

function TASQLite3BaseQuery.PSGetTableName: string;
begin
  Result := GetTableNameFromSQL(FSQL.Text);
end;

function TASQLite3BaseQuery.PSInTransaction: Boolean;
begin
  Result := Assigned(Connection);
end;

function TASQLite3BaseQuery.PSIsSQLBased: Boolean;
begin
  Result := True;
end;

function TASQLite3BaseQuery.PSIsSQLSupported: Boolean;
begin
  Result := True;
end;

procedure TASQLite3BaseQuery.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then begin
    FSQL.Text := CommandText;
    FPrepared := CommandText;
  end;
end;

procedure TASQLite3BaseQuery.PSSetParams(AParams: TParams);
begin
  if AParams.Count <> 0 then
    Params.Assign(AParams);
  Close;
end;

procedure TASQLite3BaseQuery.PSStartTransaction;
begin
  StartTransaction;
end;

procedure TASQLite3BaseQuery.PSReset;
begin
  if Active then
  begin
    Close;
    Open;
  end;
end;

function TASQLite3BaseQuery.PSGetUpdateException(e: Exception; Prev: EUpdateError): EUpdateError;
var
  PrevErr           : Integer;
begin
  // Generates an EUpdateError object based on another exception object.
  if e is ASGError then begin
    if Prev = nil then
      PrevErr := Prev.errorCode
    else
      PrevErr := 0;
    with ASGError(e) do
      Result := EUpdateError.Create(e.Message, '', -1, PrevErr, e);
  end else
    Result := EUpdateError.Create(e.Message, '', -1, -1, e);
end;

function TASQLite3BaseQuery.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
begin
  // OnUpdateRecord is not supported
  Result := False;
end;

function TASQlite3BaseQuery.PSGetKeyFields: string;
var
  i                 : integer;
begin
  Result := '';
  for i := 0 to (Fields.Count - 1) do begin
    if pfInKey in Fields[i].ProviderFlags then begin
      if Result <> '' then
        Result := Result + ';';
      Result := Result + Fields[i].FieldName;
    end;
  end;
end;

{$ENDIF}
end.
