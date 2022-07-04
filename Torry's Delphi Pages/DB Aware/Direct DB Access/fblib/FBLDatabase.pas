{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File:FBLDatabase.pas
   Copyright (c) 2002-2004 Alessandro Batisti
   fblib@altervista.org
   http://fblib.altervista.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
}
{$I fbl.inc}
{
@abstract(Managing database connection and info routines)
@author(Alessandro Batisti <fblib@altervista.org>)
FbLib - Firebird Library @html(<br>)
FBLDatabase.pas unit provides connection to firebird database
}
unit FBLDatabase;


interface

uses
  SysUtils, Classes,
  {$IFDEF D6P}
  Types,
  {$ENDIF}
  {$IFDEF FBL_THREADSAFE}
  SyncObjs,
  {$ENDIF}
  ibase_h;


type
  {@EXCLUDE}
  IFBLDbEvent = interface
    ['{08324E34-B790-D611-9B47-AA8BBB4BE1E4}']
    procedure DoOnDataBaseDisconnect;
    procedure DoOnDestroy;
  end;
  {Connection protocol values}
  TProtocolType = (ptLocal,ptTcpIp,ptNetBeui);
  {Provider Info}
  TProviderInfo = (piUnknow, piInterbase, piFirebird);
  {Server Info}
  TServerInfo = (siUnKnow, siSuperServer, siClassicServer);

  {@abstract(encapsulates the properties and methods for connect to firebird database)}
  TFBLDatabase = class(TComponent)
  private
    FDBHandle: TISC_DB_HANDLE;
    FUser: string;
    FPassword: string;
    FRole: string;
    FHost: string;
    FTcpPort: word;                     // default 3050
    FProtocol: TProtocolType;
    FDBFile: string;
    FConnectString: string;
    FCharacterSet: string;
    FAttachObjs: TList;                 // List Of Attached Objects (Transactiom ,Events)
    FSQLDialect: integer;               // Sql client Dialect
    FBackoutCount: TStringList;         // isc_info_backout_count
    FDeleteCount: TStringList;          // isc_info_delete_count
    FExpungeCount: TStringList;         // isc_info_expunge_count
    FInsertCount: TStringList;          // isc_info_insert_count
    FPurgeCount: TStringList;           // isc_info_purge_count
    FReadIdxCount: TStringList;         // isc_info_read_idx_count
    FReadSeqCount: TStringList;         // isc_info_read_seq_count
    FUpdateCount: TStringList;          // isc_info_update_count
    FUserNames: TStringList;
    FAutoSetClientDialect: boolean;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    {$IFDEF FBL_THREADSAFE}
    FCSection: TCriticalSection;
    {$ENDIF}
    function GetDbHandle: PISC_DB_HANDLE;
    function GetConnected: boolean;
    procedure SetSQLDialect(Value: integer);
    procedure CheckConnected;
    {  database info  routines }
    function GetDBSQLDialect: integer;
    function GetVersion: string;
    function GetDBSiteName: string;
    function GetDBFileName: string;
    function GetLocalConnection: boolean;
    function GetPageSize: integer;
    function GetDBInfoInt2(isc_info: integer; var ok: boolean): integer;
    function GetDBInfoInt(isc_info: integer): integer;
    function GetProviderInfo: TProviderInfo;
    function GetServerInfo: TServerInfo;
    function GetReads: integer;
    function GetWrites: integer;
    function GetFetches: integer;
    function GetODSVersion: integer;
    function GetODSMinorVersion: integer;
    function GetBaseLevel: integer;
    function GetImplementationNumber: integer;
    function GetImplementationClass: integer;
    function GetCurrentMemory: integer;
    function GetMaxMemory: integer;
    function GetAllocation: integer;
    function GetNumBuffers: integer;
    function GetSweepInterval: integer;
    function GetBackoutCount: TStringList;       // isc_info_backout_count
    function GetDeleteCount: TStringList;        // isc_info_delete_count
    function GetExpungeCount: TStringList;       // isc_info_expunge_count
    function GetInsertCount: TStringList;        // isc_info_insert_count
    function GetPurgeCount: TStringList;         // isc_info_purge_count
    function GetReadIdxCount: TStringList;       // isc_info_read_idx_count
    function GetReadSeqCount: TStringList;       // isc_info_read_seq_count
    function GetUpdateCount: TStringList;        // isc_info_update_count
    function GetOperationCounts(DBInfoCommand: integer;
      FOperation: TStringList): TStringList;
    procedure SetConnectString;
    function GetClientVersion: string;
    function GetClientMajorVersion: integer;
    function GetClientMinorVersion: integer;
    function GetUserNames: TStringList;
  public
    {Create an instance  of a TFBLDatabase}
    constructor Create(AOwner: TComponent); override;
    {Free up  all resources associated with this instance}
    destructor Destroy; override;

    {$IFDEF FBL_THREADSAFE}
     {@EXCLUDE}
    procedure Lock;
     {@EXCLUDE}
    procedure UnLock;
    {$ENDIF}

    {@EXCLUDE}
    procedure AddAttachObj(AObj: IFBLDbEvent);
    {@EXCLUDE}
    procedure RemoveAttachObj(AObj: IFBLDbEvent);
    {Connect to database
     @longCode(#
     //Examples
     uses
       FBLDatabase,FBLExcept;
     //.......
     var
       myDb : TFBLDatabase;
     myDB := TFBLDatabase.Create; // create an instance of TFDBDatabase;
     //...
     myDB.Host := 'myhost';          //set host name where run my db server
     myDB.Protocol := ptTcpIp;       //set protocol type tcpip
     myDb.DBFile := 'c:\db\mydb.fdb' //set database file name or alias (fb 1.5)
     myDB.CharacterSet := 'NONE';    //set optionally  CharacterSet
     //alternative method for connect to database
     //is to set property myDB.Protocol := ptLocal
     //and to set connection string in
     //DbFile  property
     //Example
     //MyDb.DBFile := 'myhost:c:\db\mydb.fdb'; //tcpip
     myDb.User := 'sysdba';         //set db user name
     MyDb.Password := 'masterkey';  // set db password
     //connect to database
     try
       myDB.Connect;
       // MyDB.Connected  is true
     except
       //if connection failure raise exception
       on E:EFBLError do   //EFBLError declared in FBLExcept.pas
         WriteLn(E.Message);
     end;
     // ....disconnect
     myDB.Disconnect;
     #)
     see also @link(Disconnect) @link(Host) @link(Protocol) @link(DBFile)
     @link(User)  @link(Password)  @link(Role) @link(CharacterSet)
     }
    procedure Connect;
    {disconnect a connected database @html(<br>)
    see also @link(Connect)
    }
    procedure Disconnect;
    {create a local database
    @longCode(#
    //Examples ...
    // myDb is an instance of TFBLDatabase
    myDB.CreateDatabase('c:\db\mydb.fdb,  //database name
                       'sysdba',          //user name
                       'masterkey',       //password
                       3,                 //sqldialect
                       4096,              //pagesize
                       'NONE'             //CharacterSet
                       );
    #)
    }
    procedure CreateDatabase(const AFilename, AUser, APassword: string;
      ADialect: word = 3; APagesize: integer = 4096; ACharSet: string = '');
    {Return @True if database is connected}
    property Connected: boolean read GetConnected;
    {The firebird database handle , this is used in all calls to firebird api}
    property DBHandle: PISC_DB_HANDLE read GetDBHandle;
    {Value of the db server sql dialect}
    property DBSqlDialect: integer read GetDBSqlDialect;
    {Version identification string of the database implementation}
    property Version: string read GetVersion;
    {Host name where the database is attached}
    property DBSiteName: string read GetDBSiteName;
    {File name of the attached database}
    property DBFileName: string read GetDBFileName;
    {Number of byte per page of the attached database}
    property PageSize: integer read GetPageSize;
    {Info of the database server (firebird,interbase..)}
    property ProviderInfo: TProviderInfo read GetProviderInfo;
    {Type of architecture of database server (super-server or classic-server)}
    property ServerInfo: TServerInfo read GetServerInfo;
    {Return @True if is local connection}
    property IsLocalConnection: boolean read GetLocalConnection;
    {Number of page reads}
    property Reads: integer read GetReads;
    {Number of page writes}
    property Writes: integer read GetWrites;
    {Number of reads from the memory buffer cache}
    property Fetches: integer read GetFetches;
    {ODS major version number}
    property ODSVersion: integer read GetODSVersion;
    {On-disk structure (ODS) minor version number}
    property ODSMinorVersion: integer read GetODSMinorVersion;
    {Database version (level) number}
    property BaseLevel: integer read GetBaseLevel;
    {Database implementation number}
    property ImplementationNumber: integer read GetImplementationNumber;
    {Database implementation Class}
    property ImplementationClass: integer read GetImplementationClass;
    {Amount of server memory (in bytes) currently in use}
    property CurrentMemory: integer read GetCurrentMemory;
    {Maximum amount of memory (in bytes) used at one time
    since the first process attached to the database}
    property MaxMemory: integer read GetMaxMemory;
    {Number of database pages allocated}
    property Allocation: integer read GetAllocation;
    {Number of memory buffers currently allocated}
    property NumBuffers: integer read GetNumBuffers;
    {Number of transactions that are committed between "sweeps" to
     remove database record versions that are no longer needed}
    property SweepInterval: integer read GetSweepInterval;
    {Number of removals of a version of record}
    property BackoutCount: TStringList read GetBackoutCount;
    {Number of database deletes since the database was last attached}
    property DeleteCount: TStringList read GetDeleteCount;
    {Number of removals of a record and all of its ancestor,for
     records whose deletions have been committed}
    property ExpungeCount: TStringList read GetExpungeCount;
    {Number of inserts into the database since the database was last attached}
    property InsertCount: TStringList read GetInsertCount;
    {Number of removals of old versions of fully mature records(records
     that are committed,so that older ancestor versions are no longer needed)}
    property PurgeCount: TStringList read GetPurgeCount;
    {Number of reads done via an index since the database was last attached}
    property ReadIdxCount: TStringList read GetReadIdxCount;
    {Number of sequential table scans(row reads)done on each table since the database was last attached}
    property ReadSeqCount: TStringList read GetReadSeqCount;
    {Number of database uddates since the database was last attached}
    property UpdateCount: TStringList read GetUpdateCount;
    {implementation of the client library version in string format (only firebird 1.5 or above) }
    property ClientVersion: string read GetClientVersion;
    {Value of the client major version library  (only firebird 1.5 or above) }
    property ClientMajorVersion: integer read GetClientMajorVersion;
    {Value of the client ninor version library (only firebird 1.5 or above) }
    property ClientMinorVersion: integer read GetClientMinorVersion;
    {List of user attacched to database (only super-version server)}
    property UserNames: TStringList read GetUserNames;
  published
    {Database name or alias name
    @html(<br>) see also @link(Connect)}
    property DBFile: string read FDBFile write FDBFile;
    {Host name of database server
    @html(<br>) see also @link(Connect)}
    property Host: string read FHost write FHost;
    {Protocol of connection
    @html(<br>) see also @link(Connect)}
    property Protocol: TProtocolType read FProtocol write FProtocol;
    {User name connection
    @html(<br>) see also @link(Connect)}
    property User: string read FUser write FUser;
    {Password connection
    @html(<br>) see also @link(Connect)}
    property Password: string read FPassword write FPassword;
    {Role name}
    property Role: string read FRole write FRole;
    {Tcp port default 3050}
    property TcpPort: word read FTcpPort write FTcpPort default 3050;
    {Occurs after database is connected}
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    {Occurs after database is disconnected}
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    {Set  sqldialect for client default value = 3
     @html(<br>) see also @link(AutoSetClientDialect)}
    property SQLDialect: integer read FSQLDialect write SetSqlDialect;
    {This property set automatically client sql dialet to server dialect after
    database is connected
    @html(<br>) see also @link(SQLDialect)}
    property AutoSetClientDialect: boolean read FAutosetClientDialect
      write FAutoSetClientDialect default True;
    {String specifyng the character set to be utilized , default('NONE')
    valid values are:
    @HTML(<pre>
    NONE, OCTETS,
    ASCII, UNICODE_FSS,
    SJIS_0208, EUCJ_0208,
    DOS437, DOS850,
    DOS865, ISO8859_1,
    DOS852, DOS857,
    DOS860, DOS861,
    DOS863, CYRL,
    WIN1250, WIN1251,
    WIN1252, WIN1253,
    WIN1254, NEXT,
    KSC_5601, BIG_5,
    GB_2312 </pre>)
    see also @LINK(Connect)}
    property CharacterSet: string read FCharacterSet write FCharacterSet;
  end;



implementation

uses FBLExcept, FBLmixf, FBLConst;

(* TFBLDatabase *)

constructor TFBLDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAttachObjs := TList.Create;
  FDBHandle := nil;
  FSQLDialect := 1;
  FConnectString := '';
  FCharacterSet := '';
  FTcpPort := 3050;
  FAutoSetClientDialect := True;
  FBackoutCount := nil;
  FDeleteCount := nil;
  FExpungeCount := nil;
  FInsertCount := nil;
  FPurgeCount := nil;
  FReadIdxCount := nil;
  FReadSeqCount := nil;
  FUpdateCount := nil;
  FUserNames := nil;
  {$IFDEF FBL_THREADSAFE}
  FCSection := TCriticalSection.Create;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

destructor TFBLDatabase.Destroy;
var
  i: integer;
begin
  if FDBHandle <> nil then Disconnect;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    for i := 0 to FAttachObjs.Count - 1 do
      IFBLDbEvent(FAttachObjs[i]).DoOnDestroy;
    FAttachObjs.Free;
    if Assigned(FBackoutCount) then FBackoutCount.Free;
    if Assigned(FDeleteCount) then FDeleteCount.Free;
    if Assigned(FExpungeCount) then FExpungeCount.Free;
    if Assigned(FInsertCount) then FInsertCount.Free;
    if Assigned(FPurgeCount) then FPurgeCount.Free;
    if Assigned(FReadIdxCount) then FReadIdxCount.Free;
    if Assigned(FReadSeqCount) then FReadSeqCount.Free;
    if Assigned(FUpdateCount) then FUpdateCount.Free;
    if Assigned(FUserNames) then FUserNames.Free;
  {$IFDEF FBL_THREADSAFE}
  finally
    UnLock;
  end;
  {$ENDIF}
  {$IFDEF FBL_THREADSAFE}
  FCSection.Free;
  {$ENDIF}
  inherited Destroy;
end;

//------------------------------------------------------------------------------

{$IFDEF FBL_THREADSAFE}
procedure TFBLDatabase.Lock;
begin
  FCSection.Enter;
end;

//------------------------------------------------------------------------------

procedure TFBLDatabase.UnLock;
begin
  FCSection.Leave;
end;
{$ENDIF}

//------------------------------------------------------------------------------

function TFBLDatabase.GetDbHandle: PISC_DB_HANDLE;
begin
  Result := @FDBhandle;
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetConnected;
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    Result := (FDBHandle <> nil);
  {$IFDEF FBL_THREADSAFE}
  finally
    UnLock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDatabase.SetConnectString;
begin
  case FProtocol of
    ptLocal:
      FConnectString := FDBFile;
    ptTcpIp:
      if FHost = '' then
        FBLError(E_DB_NO_HOST)
      else
        if FtcpPort <> 3050 then
          //FConnectString := FHost + '/' + IntToStr(FtcpPort) + ':' + FDBFile
          FConnectString := Format('%s/%d:%s', [FHost, FtcpPort, FDBFile])
        else
          FConnectString := FHost + ':' + FDBFile;
        ptNetBeui:
      if FHost = '' then
        FBLError(E_DB_NO_HOST)
      else
        FConnectString := '\\' + FHost + '\' + FDBFile;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLDatabase.CheckConnected;
begin
  if FDBHandle = nil then
    FBLError(E_DB_NOACTIVE_CON);
end;

//------------------------------------------------------------------------------

procedure TFBLDatabase.Connect;
var
  Status_vector: ISC_STATUS_VECTOR;
  DpbBuffer: PChar;
  DpbIdx: Short;
  LenBuffer: integer;
begin
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    CheckFbClientLoaded;
    if FDBHandle <> nil then
      FBLError(E_DB_ALREADY_CON);
    SetConnectString;
    DpbBuffer := nil;
    DpbIdx := 0;
    LenBuffer := 3 + Length(FUser) + 2 + Length(FPassword);
    if FRole <> '' then
      Inc(LenBuffer, 2 + Length(FRole));
    if FCharacterSet <> '' then
      Inc(LenBuffer, 2 + Length(FCharacterSet));
    FBLmalloc(DpbBuffer, LenBuffer);
    try
      DpbBuffer[DpbIdx] := char(isc_dpb_version1);
      Inc(DpbIdx);
      // set user name
      DpbBuffer[DpbIdx] := char(isc_dpb_user_name);
      Inc(DpbIdx);
      DpbBuffer[DpbIdx] := char(Length(FUser));
      Inc(DpbIdx);
      If Length(FUser) > 0 then 
        Move(FUser[1], DpbBuffer[DpbIdx], Length(FUser));
      Inc(DpbIdx, Length(FUser));
      // set password
      DpbBuffer[DpbIdx] := char(isc_dpb_password);
      Inc(DpbIdx);
      DpbBuffer[DpbIdx] := char(Length(FPassword));
      Inc(DpbIdx);
      If Length(FPassword) > 0 then 
        Move(FPassword[1], DpbBuffer[DpbIdx], Length(FPassword));
      Inc(DpbIdx, Length(FPassword));
      if FRole <> '' then
      begin
        DpbBuffer[DpbIdx] := char(isc_dpb_sql_role_name);
        Inc(DpbIdx);
        DpbBuffer[DpbIdx] := char(Length(FRole));
        Inc(DpbIdx);
        Move(FRole[1], DpbBuffer[DpbIdx], Length(FRole));
        Inc(DpbIdx, Length(FRole));
      end;
      if FCharacterSet <> '' then
      begin
        DpbBuffer[DpbIdx] := char(isc_dpb_lc_ctype);
        Inc(DpbIdx);
        DpbBuffer[DpbIdx] := char(Length(FCharacterSet));
        Inc(DpbIdx);
        Move(FCharacterSet[1], DpbBuffer[DpbIdx], Length(FCharacterSet));
        Inc(DpbIdx, Length(FCharacterSet));
      end;
      isc_attach_database(@Status_vector, Short(Length(FConnectString)),
        PChar(FConnectString), @FDBHandle,
        DpbIdx, DpbBuffer);
      if (Status_vector[0] = 1) and (Status_vector[1] <> 0) then
        FBLShowError(@Status_vector);
      if FAutoSetClientDialect then SQLDialect := DBSqlDialect;
      if Assigned(FOnConnect) then
        FOnConnect(self);
    finally
      FBLFree(DpbBuffer);
    end;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDatabase.Disconnect;
var
  Status_vector: ISC_STATUS_VECTOR;
  i: integer;
begin
  CheckConnected;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    for i := 0 to FAttachObjs.Count - 1 do
      IFBLDbEvent(FAttachObjs[i]).DoOnDataBaseDisconnect;
    isc_detach_database(@Status_vector, @FDBhandle);
    if FDBHandle <> nil then FBLShowError(@Status_vector);
    if Assigned(FOnDisconnect) then
      FOnDisconnect(self);
    {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TFBLDatabase.CreateDatabase(const AFilename, AUser, APassword: string;
  ADialect: word = 3; APagesize: integer = 4096;
  ACharset: string = '');
var
  Status_vector: ISC_STATUS_VECTOR;
  DBh: TISC_DB_HANDLE;
  TRh: TISC_TR_HANDLE;
  Params: string;
begin
  DBh := nil;
  TRh := nil;
  CheckFbClientLoaded;
  //Params := 'USER ''' + AUser + ''' ' + 'PASSWORD ''' +
  //APassword + ''' ' + 'PAGE_SIZE ' + IntToStr(APageSize);
  Params := 'USER ' + QuotedStr(AUser) + ' PASSWORD ' +
    QuotedStr(APassword) + ' PAGE_SIZE ' + IntToStr(APageSize);
  if ACharSet <> '' then
    Params := Params + ' DEFAULT CHARACTER SET ' + ACharSet;
  isc_dsql_execute_immediate(@Status_Vector, @DBh, @TRh, 0,
    PChar('CREATE DATABASE ''' + AFileName + ''' ' +
    Params), ADialect, nil);
  if (Status_vector[0] = 1) and (Status_vector[1] <> 0) then
    FBLShowError(@Status_vector);
end;

//------------------------------------------------------------------------------

procedure TFBLDatabase.SetSQLDialect(Value: integer);
begin
  if (Value < 1) then FBLError(E_DB_SQLDIALECT_INVALID);
  if (FDBHandle = nil) then
    FSQLDialect := Value
  else if Value <= DBSQLDialect then
    FSQLDialect := Value
  else
    FBLError(E_DB_SQLDIALECT_INVALID)
end;

//------------------------------------------------------------------------------

procedure TFBLDatabase.AddAttachObj(AObj: IFBLDbEvent);
begin
  FAttachObjs.Add(Pointer(AObj));
end;

//------------------------------------------------------------------------------

procedure TFBLDatabase.RemoveAttachObj(AObj: IFBLDbEvent);
var
  i: integer;
begin
  i := FAttachObjs.IndexOf(Pointer(AObj));
  if (i > -1) then
    FAttachObjs.Delete(i);
end;

//------------------------------------------------------------------------------

(* ---------------------------------------------------------------------------*)
(*                   Db info                                                  *)
(*----------------------------------------------------------------------------*)

function TFBLDatabase.GetDBSQLDialect: integer;
var
  bOk: boolean;
begin
  Result := GetDBInfoInt2(isc_info_db_sql_Dialect, bOk);
  if not bOk then  Result := 1;
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetVersion: string;
var
  Status_vector: ISC_STATUS_VECTOR;
  Buffer: array[0..1023] of char;
  DBInfo: char;
begin
  CheckConnected;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    DBInfo := char(isc_info_version);
    if isc_database_info(@Status_Vector, @FDBHandle, 1, @DBInfo,
      SizeOf(buffer), Buffer) <> 0 then
        FBLShowError(@Status_vector);
    SetLength(Result, integer(buffer[4]));
    Move(Buffer[5], Result[1], integer(Buffer[4]));
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetDBSiteName: string;
var
  Status_vector: ISC_STATUS_VECTOR;
  buffer: array[0..1023] of char;
  DBInfo: char;
  Pos, Len: integer;
begin
  CheckConnected;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    DBInfo := char(isc_info_db_id);
    if isc_database_info(@Status_Vector, @FDBHandle, 1, @DBInfo,
      SizeOf(buffer), buffer) <> 0 then
        FBLShowError(@Status_vector);

    //in buffer[4] length of DBfilename
    //in buffer[5 + (integer(buffer[4])] length DBSiteName
    len := integer(buffer[5 + integer(buffer[4])]);
    pos := 6 + integer(buffer[4]);
    SetLength(Result, len);
    Move(Buffer[pos], Result[1], len);
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetDBFileName: string;
var
  Status_vector: ISC_STATUS_VECTOR;
  buffer: array[0..1023] of char;
  DBInfo: char;
begin
  CheckConnected;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    DBInfo := char(isc_info_db_id);
    if isc_database_info(@Status_Vector, @FDBHandle, 1, @DBInfo,
      SizeOf(buffer), buffer) <> 0 then
       FBLShowError(@Status_vector);
    //in buffer[4] length of DBfilename
    SetLength(Result, integer(buffer[4]));
    Move(Buffer[5], Result[1], integer(buffer[4]));
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetLocalConnection: boolean;
var
  Status_vector: ISC_STATUS_VECTOR;
  Buffer: array[0..1023] of char;
  DBInfo: char;
begin
  CheckConnected;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    DBinfo := char(isc_info_db_id);
    if isc_database_info(@Status_Vector, @FDBHandle, 1, @DBInfo,
      Short(Sizeof(buffer)), Buffer) <> 0 then
       FBLShowError(@Status_vector);
    if Buffer[3] = #2 then   // '#2 for local connection
      Result := True       //  #4 for remote connection
    else
      Result := False;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetDBInfoInt2(isc_info: integer; var Ok: boolean): integer;
var
  Status_vector: ISC_STATUS_VECTOR;
  Buffer: array[0..511] of char;
  Length: integer;
  DBInfo: char;
begin
  CheckConnected;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    DBInfo := char(isc_info);
    if isc_database_info(@Status_Vector, @FDBHandle, 1, @DBInfo,
      Sizeof(Buffer), Buffer) <> 0 then
       FBLShowError(@Status_vector);
    Ok := (Buffer[0] = char(isc_info));
    Length := isc_vax_integer(@Buffer[1], 2);
    Result := isc_vax_integer(@Buffer[3], Short(Length));
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetDBInfoInt(isc_info: integer): integer;
var
  bOk: boolean;
begin
  Result := GetDbInfoInt2(isc_info, bOk);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetPageSize: integer;
begin
  Result := GetDBInfoInt(isc_info_page_size);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetProviderInfo: TProviderInfo;
var
  bOk: boolean;
  Res: integer;
begin
  Result := piUnknow;
  Res := GetDBInfoInt2(isc_info_db_provider, bOk);
  if bOk then
  begin
    case Res of
      isc_info_db_code_interbase: Result := piInterBase;
      isc_info_db_code_firebird: Result := piFireBird;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetServerInfo: TServerInfo;
var
  bOk: boolean;
  Res: integer;
begin
  Result := siUnknow;
  Res := GetDBInfoInt2(isc_info_db_class, bOk);
  if bOk then
  begin
    case Res of
      isc_info_db_class_classic_access: Result := siClassicServer;
      isc_info_db_class_server_access: Result := siSuperServer;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetReads: integer;
begin
  Result := GetDBInfoInt(isc_info_reads);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetWrites: integer;
begin
  Result := GetDBInfoInt(isc_info_writes);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetFetches: integer;
begin
  Result := GetDBInfoInt(isc_info_fetches);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetODSVersion: integer;
begin
  Result := GetDBInfoInt(isc_info_ods_version);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetODSMinorVersion: integer;
begin
  Result := GetDBInfoInt(isc_info_ods_minor_version);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetBaseLevel: integer;
var
  buffer: array[0..511] of char;
  DBInfo: char;
  Status_vector: ISC_STATUS_VECTOR;
begin
  CheckConnected;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    DBInfo := char(isc_info_base_level);
    if isc_database_info(@Status_Vector, @FDBHandle, 1, @DBInfo,
      sizeof(buffer), buffer) <> 0 then
        FBLShowError(@Status_vector);
    Result := isc_vax_integer(@buffer[4], 1);
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetImplementationNumber: integer;
var
  buffer: array[0..511] of char;
  DBInfo: char;
  Status_vector: ISC_STATUS_VECTOR;
begin
  CheckConnected;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    DBInfo := char(isc_info_implementation);
    if isc_database_info(@Status_Vector, @FDBHandle, 1, @DBInfo,
      Sizeof(buffer), buffer) <> 0 then
        FBLShowError(@Status_vector);
    Result := isc_vax_integer(@buffer[3], 1);
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetImplementationClass: integer;
var
  buffer: array[0..511] of char;
  DBInfo: char;
  Status_vector: ISC_STATUS_VECTOR;
begin
  CheckConnected;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    DBInfo := char(isc_info_implementation);
    if isc_database_info(@Status_Vector, @FDBHandle, 1, @DBInfo,
      sizeof(buffer), buffer) <> 0 then
       FBLShowError(@Status_vector);
    Result := isc_vax_integer(@buffer[4], 1);
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetCurrentMemory: integer;
begin
  Result := GetDBInfoInt(isc_info_current_memory);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetMaxMemory: integer;
begin
  Result := GetDBInfoInt(isc_info_max_memory);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetAllocation: integer;
begin
  Result := GetDBInfoInt(isc_info_allocation);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetNumBuffers: integer;
begin
  Result := GetDBInfoInt(isc_info_num_buffers);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetSweepInterval: integer;
begin
  Result := GetDBInfoInt(isc_info_sweep_interval);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetOperationCounts(DBInfocommand: integer;
  FOperation: TStringList): TStringList;
var
  buffer: array[0..10239] of char;
  DBInfo: char;
  i, qtd_tables, id_table, qtd_operations: integer;
  Status_vector: ISC_STATUS_VECTOR;
begin
  CheckConnected;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    if FOperation = nil then FOperation := TStringList.Create;
    Result := FOperation;
    DBInfo := char(DBInfoCommand);
    if isc_database_info(@Status_Vector, @FDBHandle, 1, @DBInfo,
      sizeof(buffer), buffer) <> 0 then
        FBLShowError(@Status_vector);
     FOperation.Clear;
    // 1. 1 byte specifying the item type requested (e.g., isc_info_insert_count).
    // 2. 2 bytes telling how many bytes compose the subsequent value pairs.
    // 3. A pair of values for each table in the database on wich the requested
    //    type of operation has occurred since the database was last attached.
    // Each pair consists of:
    // 1. 2 bytes specifying the table ID.
    // 2. 4 bytes listing the number of operations (e.g., inserts) done on that table.
    qtd_tables := integer(Trunc(isc_vax_integer(@buffer[1], 2) div 6));
    for i := 0 to qtd_tables - 1 do
    begin
      id_table := isc_vax_integer(@buffer[3 + (i * 6)], 2);
      qtd_operations := isc_vax_integer(@buffer[5 + (i * 6)], 4);
      FOperation.Add(IntToStr(id_table) + '=' + IntToStr(qtd_operations));
    end;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetBackoutCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_backout_count, FBackoutCount);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetDeleteCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_delete_count, FDeleteCount);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetExpungeCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_expunge_count, FExpungeCount);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetInsertCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_insert_count, FInsertCount);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetPurgeCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_purge_count, FPurgeCount);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetReadIdxCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_read_idx_count, FReadIdxCount);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetReadSeqCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_read_seq_count, FReadSeqCount);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetUpdateCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_update_count, FUpdateCount);
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetClientVersion: string;
var
  Buffer: array[0..255] of char;
begin
  CheckConnected;
  Result := '';
  if GetFbClientVersion = 7 then
  begin
    isc_get_client_version(Buffer);
    Result := string(Buffer);
  end;
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetClientMajorVersion: integer;
begin
  CheckConnected;
  Result := 0;
  if GetFbClientVersion = 7 then
    Result := isc_get_client_major_version;
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetClientMinorVersion: integer;
begin
  CheckConnected;
  Result := 0;
  if GetFbClientVersion = 7 then
    Result := isc_get_client_minor_version;
end;

//------------------------------------------------------------------------------

function TFBLDatabase.GetUserNames: TStringList;
var
  Status_vector: ISC_STATUS_VECTOR;
  UserName: string;
  i, UserNameLength: integer;
  Buffer: array [0..16383] of char;
  DBInfo: char;
begin
  CheckConnected;
  {$IFDEF FBL_THREADSAFE}
  Lock;
  try
  {$ENDIF}
    if FUserNames = nil then FUserNames := TStringList.Create;
    Result := FUserNames;
    DBInfo := char(isc_info_user_names);

    if isc_database_info(@Status_Vector, @FDBHandle, 1, @DBInfo,
      Short(SizeOf(Buffer)), Buffer) <> 0 then
        FBLShowError(@Status_vector);

    FUserNames.Clear;
    i := 0;
    while Buffer[i] = char(isc_info_user_names) do
    begin
      Inc(i, 3);
      UserNameLength := integer(Buffer[i]);
      SetLength(UserName, UserNameLength);
      Inc(i, 1);
      Move(Buffer[i], UserName[1], UserNameLength);
      Inc(i, UserNameLength);
      FUserNames.Add(UserName);
    end;
  {$IFDEF FBL_THREADSAFE}
  finally
    Unlock;
  end;
  {$ENDIF}
end;

end.
