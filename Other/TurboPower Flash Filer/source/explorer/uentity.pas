{*********************************************************}
{* Classes for server, database, and table lists         *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit uentity;

interface

uses
  Classes,
  Controls,
  Consts,
  DB,
  Dialogs,
  Forms,
  SysUtils,
  Windows,
  ffclbase,
  ffllbase,
  fflldict,
  ffllprot,
  ffclreng,
  ffdb,
  fflllgcy,
  fflllog,
  fflogdlg,
  ffsrbde;

type
  TffexpSession = class(TffSession)
    protected
      procedure FFELogin(aSource : TObject;
                     var aUserName : TffName;
                     var aPassword : TffName;
                     var aResult : Boolean);
    public
      ffePassword : string;
      ffeUserName : string;
    public
      constructor Create(AOwner : TComponent); override;
  end;
  TffexpDatabase = class(TffDatabase);
  TffexpTable = class(TffTable);

type
  TffeEntityType = (etServer, etDatabase, etTable);
  TffeServerList = class;
  TffeDatabaseList = class;
  TffeDatabaseItem = class;
  TffeTableList = class;
  TffeTableItem = class;

  TffeEntityItem = class(TffListItem)
    protected { private}
      FEntityType: TffeEntityType;
      FEntityName: TffNetAddress;
      FEntitySerialKey: DWORD;
    public
      constructor Create(aEntityType: TffeEntityType; aEntityName: TffShStr);

      function Compare(aKey : Pointer): Integer; override;
        {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
          equal, >0 otherwise}

      function Key: Pointer; override;
        {-return a pointer to this item's key: it'll be a pointer to a
          shortstring}

      property EntityType: TffeEntityType
        read FEntityType;

      property EntityName: TffNetAddress
        read FEntityName;

      property EntitySerialKey: DWORD
        read FEntitySerialKey;
  end;

  TffeEntityList = class(TffList)
    protected
      function GetItem(aIndex: LongInt): TffeEntityItem;
    public
      function IndexOfName(const aName: TffShStr): LongInt;
        {-return the index of the entry whose name is given}

      function IndexOfSerialKey(aSerialKey: DWORD): LongInt;
        {-return the list index for a given entity identified by its
          serial key (the outline control keeps track of entities
          by the serial key) }

      property Items[aIndex: LongInt]: TffeEntityItem
        read GetItem;
  end;

  TffeServerNotifyEvent = procedure(aServerIndex: LongInt) of object;

  TffeServerItem = class(TffeEntityItem)
    protected
      FClient : TffClient;
      FDatabaseList: TffeDatabaseList;
      FProtocol : TffProtocolType;
      FServerEngine : TffRemoteServerEngine;
      FSession : TffexpSession;
      FTransport : TffLegacyTransport;

      procedure siCheckAttached;
      function siGetDatabaseCount : longInt;
      function siGetDatabase(const anIndex : longInt) : TffeDatabaseItem;

    public
      ServerID: LongInt;
      Attached: Boolean;

      constructor Create(aServerName: TffNetAddress;
                         aProtocol : TffProtocolType);
      destructor Destroy; override;
      procedure AddAlias(aAlias      : TffName;
                         aPath       : TffPath;
                         aCheckSpace : Boolean);                       {!!.11}
      function AddDatabase(aAlias : TffName) : TffeDatabaseItem;
      function Attach(aLog : TffBaseLog): TffResult;
      procedure Detach;
      procedure DropDatabase(aDatabaseName : TffName);
      procedure GetAliases(aList : TStrings);
      function GetAutoInc(aTable : TffTable) : TffWord32;
      procedure LoadDatabases;

      property DatabaseCount : longInt read siGetDatabaseCount;
      property Databases[const anIndex : longInt] : TffeDatabaseItem
        read siGetDatabase;
      property ServerName: TffNetAddress read FEntityName;
      property Session : TffexpSession read FSession;
      property Protocol : TffProtocolType read FProtocol;              {!!.10}
      property Client : TffClient read FClient;                        {!!.11}
  end;

  TffeServerList = class(TffeEntityList)
    protected {private}
      FClient : TffClient;
      FOnAttach: TffeServerNotifyEvent;
      FOnDetach: TffeServerNotifyEvent;
      FServerEngine : TffRemoteServerEngine;
      FTransport : TffLegacyTransport;

      function GetItem(aIndex: LongInt): TffeServerItem;
    public
      constructor Create(aLog : TffBaseLog);
      destructor Destroy; override;
      procedure DetachAll;
      function Insert(aItem: TffeServerItem): Boolean;
      procedure Load;
      procedure LoadRegisteredServers;                                

      property Items[aIndex: LongInt]: TffeServerItem
        read GetItem; 

      property OnAttach: TffeServerNotifyEvent
        read FOnAttach write FOnAttach;
      property OnDetach: TffeServerNotifyEvent
        read FOnDetach write FOnDetach;
  end;

  TffeDatabaseItem = class(TffeEntityItem)
    protected
      FDatabase : TffexpDatabase;
      FServer : TffeServerItem;
      FTableList : TffeTableList;
      diParentList: TffeDatabaseList;

      function diGetIsOpen: Boolean;
      function diGetServer: TffeServerItem;
      function diGetTable(const anIndex : longInt) : TffeTableItem;
      function diGetTableCount : longInt;

    public
      DatabaseID: LongInt;     { FF internal DB Identifier }
      constructor Create(aServer : TffeServerItem; aAliasName: TffName);
      destructor Destroy; override;
      procedure Close;
      function AddTable(const aTableName : TffTableName) : longInt;
      procedure CreateTable(const aTableName: TffTableName; aDict: TffDataDictionary);
      procedure DropTable(const anIndex : longInt);
        { Drop the specified table from the list of tables. }
      procedure GetTableNames(Tables: TStrings);
      function IndexOf(aTable : TffeTableItem) : longInt;
      procedure LoadTables;
      procedure Open;
      procedure Rename(aNewName: TffNetAddress);
      property Database : TffexpDatabase read FDatabase;
      property DatabaseName: TffNetAddress read FEntityName;
      property IsOpen: Boolean read diGetIsOpen;
      property Server: TffeServerItem read diGetServer;
      property TableCount : longInt read diGetTableCount;
      property Tables[const anIndex : longInt] : TffeTableItem
        read diGetTable;
  end;

  TffeDatabaseList = class(TffeEntityList)
    protected
      FServer : TffeServerItem;

      function GetItem(aIndex: LongInt): TffeDatabaseItem;
    public
      constructor Create(aServer : TffeServerItem);
      destructor Destroy; override;
      function Add(const aDatabaseName: TffName): TffeDatabaseItem;
      procedure DropDatabase(aIndex: LongInt);
      function Insert(aItem: TffeDatabaseItem): Boolean;
      procedure Load;
        { Load the aliases for the server. }

      property Items[aIndex: LongInt]: TffeDatabaseItem
        read GetItem;
  end;

  TffeTableItem = class(TffeEntityItem)
    protected {private}
      FParent : TffeDatabaseItem;
    protected
      tiParentList: TffeTableList;
      procedure AfterOpenEvent(aDataset: TDataset);
      function GetDatabase: TffeDatabaseItem;
      function GetDictionary: TffDataDictionary;
      function GetRebuilding: Boolean;
      function GetRecordCount: TffWord32;
      function GetServer: TffeServerItem;
    public
      Table: TffexpTable;
      DatabaseIndex: LongInt;
      CursorID: LongInt;
      TaskID: LongInt;
      constructor Create(aDatabase : TffeDatabaseItem; aTableName: TffName);
      destructor Destroy; override;

      procedure CheckRebuildStatus(var aCompleted: Boolean;
                                   var aStatus: TffRebuildStatus);
      function GetAutoInc : TffWord32;
      procedure Pack;
      procedure Reindex(aIndexNum: Integer);
      procedure Rename(aNewTableName: TffName);
      procedure Restructure(aDictionary: TffDataDictionary; aFieldMap: TStrings);
      procedure SetAutoIncSeed(aValue : LongInt);
      procedure Truncate;
      procedure CopyRecords(aSrcTable : TffDataSet; aCopyBLOBs : Boolean); {!!.10}

      property Database: TffeDatabaseItem
        read GetDatabase;
      property Dictionary: TffDataDictionary
        read GetDictionary;
      property Rebuilding: Boolean
        read GetRebuilding;
      property RecordCount: TffWord32
        read GetRecordCount;
      property Server: TffeServerItem
        read GetServer;
      property TableName: TffNetAddress
        read FEntityName;
  end;

  TffeTableList = class(TffeEntityList)
    protected
      FDatabase : TffeDatabaseItem;
      function GetItem(aIndex: LongInt): TffeTableItem;
    public
      constructor Create(aDatabase : TffeDatabaseItem);
      destructor Destroy; override;
      function Add(const aTableName: TffName): longInt;
      procedure DropTable(aIndex: LongInt);
      function Insert(aItem: TffeTableItem): Boolean;
      procedure Load;

      property Items[aIndex: LongInt]: TffeTableItem
        read GetItem;
  end;

const
  ffcConnectTimeout : longInt = 2000;
    {-Number of milliseconds we will wait for servers to respond to our
      broadcast. }

implementation

uses
  ffclcfg,
  ffdbbase,
  ffllcomm,
  ffllcomp,
  fflleng,
  ubase,
  uconsts,
  {$IFDEF DCC6ORLATER}                                                 {!!.03}
  RTLConsts,                                                           {!!.03}
  {$ENDIF}                                                             {!!.03}
  uconfig;

const
  ffcLogName = 'ffe.log';
  ffcDatabaseClosed = 'Cannot perform this operation on a closed database';

var
  NextEntitySerialKey: DWORD;

{=====TffeEntityItem methods=====}

constructor TffeEntityItem.Create(aEntityType: TffeEntityType; aEntityName: TffShStr);
begin
  inherited Create;
  FEntityType := aEntityType;
  FEntityName := aEntityName;
  FEntitySerialKey := NextEntitySerialKey;
  Inc(NextEntitySerialKey);
end;

function TffeEntityItem.Compare(aKey: Pointer): Integer;
begin
  Result := FFCmpShStr(PffShStr(aKey)^, EntityName, 255);
end;

function TffeEntityItem.Key: Pointer;
begin
  Result := @FEntityName;
end;

{=====TffeEntityList methods=====}

function TffeEntityList.GetItem(aIndex: LongInt): TffeEntityItem;
begin
  if (aIndex < 0) or (aIndex >= Count) then
    raise EListError.Create(SListIndexError);
  Result := TffeEntityItem(inherited Items[aIndex]);
end;

function TffeEntityList.IndexOfName(const aName: TffShStr): LongInt;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].EntityName = aName then Exit;
  Result := -1;
end;

function TffeEntityList.IndexOfSerialKey(aSerialKey: DWORD): LongInt;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].EntitySerialKey = aSerialKey then Exit;
  Result := -1;
end;

{===TffeServerItem===================================================}
constructor TffeServerItem.Create(aServerName: TffNetAddress;
                                  aProtocol : TffProtocolType);
begin
  inherited Create(etServer, FFShStrTrim(aServerName));
  FDatabaseList := TffeDatabaseList.Create(Self);
  FProtocol := aProtocol;
  Attached := False;
end;
{--------}
destructor TffeServerItem.Destroy;
begin
  Detach;
  FDatabaseList.Free;
  inherited Destroy;
end;
{--------}
procedure TffeServerItem.AddAlias(aAlias      : TffName;
                                  aPath       : TffPath;
                                  aCheckSpace : Boolean);              {!!.11}
begin
  FSession.AddAlias(aAlias, aPath, aCheckSpace);                       {!!.11}
end;
{--------}
function TffeServerItem.AddDatabase(aAlias : TffName) : TffeDatabaseItem;
begin
  Result := FDatabaseList.Add(aAlias);
end;
{--------}
function TffeServerItem.Attach(aLog : TffBaseLog): TffResult;
var
  OldCursor: TCursor;
begin
  Result := DBIERR_NONE;

  { If we're already attached, then we don't need to do anything }
  if Attached then Exit;

  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try

    if not assigned(FTransport) then
      FTransport := TffLegacyTransport.Create(nil);
    with FTransport do begin
      Mode := fftmSend;
      Enabled := True;
      Protocol := FProtocol;
      EventLog := aLog;
      EventLogEnabled := True;
      EventLogOptions := [fftpLogErrors];
      ServerName := FEntityName;
    end;

    if not assigned(FServerEngine) then
      FServerEngine := TffRemoteServerEngine.Create(nil);
    with FServerEngine do begin
      Transport := FTransport;
    end;

    if not assigned(FClient) then
      FClient := TffClient.Create(nil);
    with FClient do begin
      TimeOut := Config.DefaultTimeout;                                {!!.11}
      ServerEngine := FServerEngine;
      AutoClientName := True;
      Active := True;
    end;

    if not assigned(FSession) then
      FSession := TffexpSession.Create(nil);
    with FSession do begin
      ClientName := FClient.ClientName;
      AutoSessionName := True;
      Active := True;
    end;

    Attached := FSession.Active;
    if Attached then begin
      { Automatically load up all the databases for this server }
      if not assigned(FDatabaseList) then
        FDatabaseList := TffeDatabaseList.Create(Self);
      FDatabaseList.Load;

      { Run the event-handler if any }
      with ServerList do
        if Assigned(FOnAttach) then
          FOnAttach(Index(FEntityName));
    end;
  finally;
    Screen.Cursor := OldCursor;
  end;
end;
{--------}
procedure TffeServerItem.Detach;
var
  S: TffNetAddress;
begin

  if assigned(FDatabaseList) then begin
    FDatabaseList.Free;
    FDatabaseList := nil;
  end;

  if assigned(FSession) then begin
    FSession.Active := False;
    FSession.Free;
    FSession := nil;
  end;

  if assigned(FClient) then begin
    FClient.Active := False;
    FClient.Free;
    FClient := nil;
  end;

  if assigned(FTransport) then begin
    FTransport.State := ffesInactive;
    FTransport.Free;
    FTransport := nil;
  end;

  if assigned(FServerEngine) then begin
    FServerEngine.Free;
    FServerEngine := nil;
  end;

  Attached := False;

  S := ServerName;
  with ServerList do
    if Assigned(FOnDetach) then
      FOnDetach(Index(S));
end;
{--------}
procedure TffeServerItem.DropDatabase(aDatabaseName : TffName);
begin
  siCheckAttached;
  FDatabaseList.DropDatabase(FDatabaseList.IndexOfName(aDatabaseName));
end;
{--------}
procedure TffeServerItem.GetAliases(aList : TStrings);
begin
  siCheckAttached;
  FSession.GetAliasNames(aList);
end;
{--------}
function TffeServerItem.GetAutoInc(aTable : TffTable) : TffWord32;
begin
  Result := 1;
  FServerEngine.TableGetAutoInc(aTable.CursorID, Result);
end;
{--------}
procedure TffeServerItem.LoadDatabases;
begin
  siCheckAttached;
  FDatabaseList.Load;
end;
{--------}
procedure TffeServerItem.siCheckAttached;
begin
  if not Attached then
    Attach(nil);
end;
{--------}
function TffeServerItem.siGetDatabaseCount : longInt;
begin
  Result := FDatabaseList.Count;
end;
{--------}
function TffeServerItem.siGetDatabase(const anIndex : Longint)
                                                    : TffeDatabaseItem;
begin
  Result := TffeDatabaseItem(FDatabaseList[anIndex]);
end;
{====================================================================}

{===TffeServerList===================================================}
constructor TffeServerList.Create(aLog : TffBaseLog);
begin
  inherited Create;
  Sorted := True;

  { The transport will be left inactive.  Its sole purpose is to
    broadcast for servers using the protocol identified in the registry. }
  FTransport := TffLegacyTransport.Create(nil);
  with FTransport do begin
    Mode := fftmSend;
    Enabled := True;
    Protocol := ptRegistry;
    EventLog := aLog;
    EventLogEnabled := True;
    EventLogOptions := [fftpLogErrors];
    Name := 'ffeTransport';
  end;

  FServerEngine := TffRemoteServerEngine.Create(nil);
  with FServerEngine do begin
    Transport := FTransport;
    Name := 'ffeServerEngine';
  end;

  FClient := TffClient.Create(nil);
  with FClient do begin
    ServerEngine := FServerEngine;
    Name := 'ffeClient';
    ClientName := Name;
    Timeout := ffcConnectTimeout;
    Active := True;
  end;

end;
{--------}
destructor TffeServerList.Destroy;
begin
  Empty;
  FClient.Active := False;
  FClient.Free;
  FServerEngine.Free;
  FTransport.State := ffesInactive;
  FTransport.Free;
  inherited Destroy;
end;
{--------}
procedure TffeServerList.DetachAll;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
      if Attached then Detach;
end;
{--------}
function TffeServerList.Insert(aItem: TffeServerItem): Boolean;
begin
  Result := inherited Insert(aItem);
end;
{--------}
function TffeServerList.GetItem(aIndex: LongInt): TffeServerItem;
begin
  Result := TffeServerItem(inherited Items[aIndex]);
end;
{--------}
procedure TffeServerList.Load;
var
  Servers: TStringList;
  I: Integer;
  tryProt: TffProtocolType;                                            {!!.10}

function ServerRegistered(const ServerName : string) : Boolean;        {begin !!.06}
var
  Idx : Integer;
begin
  Result := False;
  with Config do
    for Idx := 0 to Pred(RegisteredServers.Count) do
      if FFAnsiCompareText(ServerName, RegisteredServers[Idx]) = 0 then begin {!!.10}
        Result := True;
        Exit;
      end;
end;                                                                   {end !!.06}

begin
  Empty;

//  if not (Config.Protocol = TffSingleUserProtocol) then              {!!.06}
  LoadRegisteredServers;

  {Begin !!.07}
  { added loop to try all protocols. we no longer let the user
    select protocol, but instead list all servers on all protocols. }
  { Broadcast for currently active servers }
  Servers := TStringList.Create;
  try
    for tryProt := ptSingleUser to ptIPXSPX do begin
      try
        FTransport.Enabled := False;
        FTransport.Protocol := tryProt;
        FClient.GetServerNames(Servers);
        for I := 0 to Servers.Count - 1 do
          if not ServerRegistered(Servers[I]) then                     {!!.06}
            Insert(TffeServerItem.Create(Servers[I], tryProt));
      except
        { swallow all errors. assume that the particular protocol failed. }
      end;
    end;
  {End !!.07}
  finally
    Servers.Free;
  end;
end;
{--------}
procedure TffeServerList.LoadRegisteredServers;
var
  I: Integer;
begin
  with Config.RegisteredServers do
    for I := 0 to Count - 1 do
      Self.Insert(TffeServerItem.Create(Strings[I], ptTCPIP));         {!!.10} {changed protocol type}
end;
{=====================================================================}

{== TffeDatabaseItem =================================================}
constructor TffeDatabaseItem.Create(aServer    : TffeServerItem;
                                    aAliasName : TffName);
begin
  inherited Create(etDatabase, aAliasName);
  FServer := aServer;
  DatabaseID := -1;
  diParentList := nil;
  FDatabase := TffexpDatabase.Create(nil);
  FTableList := TffeTableList.Create(Self);
  with FDatabase do begin
    DatabaseName := 'exp' + aAliasName;
    SessionName := aServer.Session.SessionName;
    AliasName := aAliasName;
  end;
end;
{--------}
destructor TffeDatabaseItem.Destroy;
begin
  if IsOpen then Close;
  FTableList.Free;
  FDatabase.Free;
  inherited Destroy;
end;
{--------}
procedure TffeDatabaseItem.Close;
begin
  FDatabase.Connected := False;
end;
{--------}
function TffeDatabaseItem.AddTable(const aTableName : TffTableName)
                                                    : Longint;
begin
  Result := FTableList.Add(aTableName);
end;
{--------}
procedure TffeDatabaseItem.CreateTable(const aTableName : TffTableName;
                                             aDict      : TffDataDictionary);
begin
  if not IsOpen then
    Open;

  Check(FDatabase.CreateTable(False, aTableName, aDict));
end;
{--------}
procedure TffeDatabaseItem.DropTable(const anIndex : longInt);
begin
  FTableList.DropTable(anIndex);
end;
{--------}
function TffeDatabaseItem.diGetIsOpen: Boolean;
begin
  Result := FDatabase.Connected;
end;
{--------}
function TffeDatabaseItem.diGetServer: TffeServerItem;
begin
  Result := FServer;
end;
{--------}
function TffeDatabaseItem.diGetTable(const anIndex : longInt) : TffeTableItem;
begin
  Result := TffeTableItem(FTableList[anIndex]);
end;
{--------}
function TffeDatabaseItem.diGetTableCount : longInt;
begin
  Result := FTableList.Count;
end;
{--------}
procedure TffeDatabaseItem.GetTableNames(Tables: TStrings);
begin
  if Tables is TStringList then
    TStringList(Tables).Sorted := True;
  FDatabase.GetTableNames(Tables);
end;
{--------}
function TffeDatabaseItem.IndexOf(aTable : TffeTableItem) : longInt;
begin
  Result := FTableList.IndexOfName(aTable.TableName);
end;
{--------}
procedure TffeDatabaseItem.LoadTables;
{ Find all the tables in the database and add to the table list. }
var
  Tables: TStringList;
  I: Integer;
begin
  Tables := TStringList.Create;
  try
    FTableList.Empty;
//    try
      FDatabase.GetTableNames(Tables);
      for I := 0 to Tables.Count - 1 do
        FTableList.Add(Tables[I]);
{    except
      on EffDatabaseError do
        {do nothing}
{      else
        raise;
    end;}
  finally
    Tables.Free;
  end;
end;
{--------}
procedure TffeDatabaseItem.Open;
begin
  FDatabase.Connected := True;
end;
{--------}
procedure TffeDatabaseItem.Rename(aNewName: TffNetAddress);
begin
  FDatabase.Close;
  Check(FServer.Session.ModifyAlias(FEntityName, aNewName, '', False)); {!!.11}
  FEntityName := aNewName;
end;
{=====================================================================}

{== TffeDatabaseList =================================================}
constructor TffeDatabaseList.Create(aServer : TffeServerItem);
begin
  inherited Create;
  FServer := aServer;
  Sorted := False;
end;
{--------}
destructor TffeDatabaseList.Destroy;
begin
  { Close all databases. }
  Empty;
  inherited Destroy;
end;
{--------}
function TffeDatabaseList.Add(const aDatabaseName : TffName)
                                                  : TffeDatabaseItem;
begin
  Result := TffeDatabaseItem.Create(FServer, aDatabaseName);
  Insert(Result);
end;
{--------}
procedure TffeDatabaseList.DropDatabase(aIndex: LongInt);
begin
  with Items[aIndex] do begin
    FDatabase.Connected := False;
    FServer.Session.DeleteAlias(DatabaseName);
  end;
  DeleteAt(aIndex);
end;
{--------}
function TffeDatabaseList.GetItem(aIndex: LongInt): TffeDatabaseItem;
begin
  Result := TffeDatabaseItem(inherited Items[aIndex]);
end;
{--------}
function TffeDatabaseList.Insert(aItem: TffeDatabaseItem): Boolean;
begin
  aItem.diParentList := Self;
  Result := inherited Insert(AItem);
end;
{--------}
procedure TffeDatabaseList.Load;
var
  Aliases : TStringList;
  Index : longInt;
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  Aliases := TStringList.Create;
  Screen.Cursor := crHourglass;
  try
    Empty;
    FServer.GetAliases(Aliases);
    for Index := 0 to pred(Aliases.Count) do begin
      Add(Aliases[Index]);
    end;
  finally
    Aliases.Free;
    Screen.Cursor := OldCursor;
  end;
end;
{=====================================================================}

{== TffeTableItem ====================================================}
constructor TffeTableItem.Create(aDatabase  : TffeDatabaseItem;
                                 aTableName : TffName);
begin
  inherited Create(etTable, aTableName);
  FParent := aDatabase;
  CursorID := -1;
  TaskID := -1;
  tiParentList := nil;
  Table := TffexpTable.Create(nil);
  with Table do begin
    SessionName := aDatabase.Server.Session.SessionName;
    DatabaseName := aDatabase.Database.DatabaseName;
    TableName := aTableName;
    ReadOnly := False;
    AfterOpen := AfterOpenEvent;
  end;
end;
{--------}
destructor TffeTableItem.Destroy;
begin
  Table.Free;
  inherited Destroy;
end;
{--------}
procedure TffeTableItem.CheckRebuildStatus(var aCompleted: Boolean;
                                           var aStatus: TffRebuildStatus);
var
  WasOpen : Boolean;
begin
  WasOpen := Database.IsOpen;
  if not Database.IsOpen then
    Database.Open;

  try
    Check(FParent.Server.Session.GetTaskStatus(TaskID, aCompleted, aStatus));
    if aCompleted then
      TaskID := -1;
  except
    TaskID := -1;
  end;
  if not WasOpen then
    Database.Close;
end;
{--------}
function TffeTableItem.GetAutoInc : TffWord32;
var
  WasOpen : Boolean;
begin
  WasOpen := Table.Active;
  if not Table.Active then
    Table.Open;

  Result := FParent.Server.GetAutoInc(Table);

  if not WasOpen then
    Table.Close;
end;
{--------}
procedure TffeTableItem.AfterOpenEvent(aDataset: TDataset);
var
  I: Integer;
begin
  with aDataset do
    for I := 0 to FieldCount - 1 do
      case Fields[I].DataType of
        ftString: TStringField(Fields[I]).Transliterate := False;
        ftMemo: TMemoField(Fields[I]).Transliterate := False;
      end;
end;
{--------}
function TffeTableItem.GetDatabase: TffeDatabaseItem;
begin
  Result := FParent;
end;
{--------}
function TffeTableItem.GetDictionary: TffDataDictionary;
var
  WasOpen : Boolean;
begin
  WasOpen := Table.Active;
  if not Table.Active then
    Table.Open;

  Result := Table.Dictionary;

  if not WasOpen then
    Table.Close;
end;
{--------}
function TffeTableItem.GetRebuilding: Boolean;
begin
  Result := TaskID <> -1;
end;
{--------}
function TffeTableItem.GetRecordCount: TffWord32;
var                                                                    {!!.06}
  WasOpen : Boolean;                                                   {!!.06}
begin                                                                  {!!.06}
  WasOpen := Table.Active;
  if not Table.Active then
    Table.Open;

  Result := Table.RecordCount;

  if WasOpen then                                                      {!!.06}
    Table.Close;                                                       {!!.06}
end;
{--------}
function TffeTableItem.GetServer: TffeServerItem;
begin
  Result := FParent.Server;
end;
{--------}
procedure TffeTableItem.Pack;
var
  WasOpen : Boolean;
begin
  WasOpen := Database.IsOpen;
  if not Database.IsOpen then
    Database.Open;

  Check(Database.FDatabase.PackTable(Table.TableName, TaskID));

  if not WasOpen then
    Database.Close;
end;
{--------}
procedure TffeTableItem.Reindex(aIndexNum: Integer);
var
  WasOpen: Boolean;
begin
  WasOpen := Database.IsOpen;
  if not Database.IsOpen then
    Database.Open;

  if Table.Active then Table.Close;
  Check(Database.FDatabase.ReindexTable(Table.TableName, aIndexNum, TaskID));

  if not WasOpen then
    Database.Close;
end;
{--------}
procedure TffeTableItem.Rename(aNewTableName: TffName);
begin
  with Table do begin
    if Active then Close;
    RenameTable(aNewTableName);
    FEntityName := aNewTableName;
  end;
end;
{--------}
procedure TffeTableItem.Restructure(aDictionary: TffDataDictionary; aFieldMap: TStrings);
var
  Result: TffResult;
  WasOpen: Boolean;
begin
  WasOpen := Database.IsOpen;
  if not Database.IsOpen then
    Database.Open;

  Table.Close;

  Result := Database.FDatabase.RestructureTable
              (Tablename, aDictionary, aFieldMap, TaskID);
  if Result = DBIERR_INVALIDRESTROP then
    raise Exception.Create('Cannot preserve data if user-defined indexes have been added or changed')
  else Check(Result);

  if not WasOpen then
    Database.Close;
end;
{--------}
procedure TffeTableItem.SetAutoIncSeed(aValue: Integer);
var
  WasOpen : Boolean;
begin
  WasOpen := Table.Active;
  if not Table.Active then
    Table.Open;

  Check(Table.SetTableAutoIncValue(aValue));

  if not WasOpen then
    Table.Close;
end;
{--------}
procedure TffeTableItem.Truncate;
begin
  { Make sure we suck in the dictionary before the table gets deleted }
  GetDictionary;
  with Table do begin
    Close;
    DeleteTable;
  end;
  Database.CreateTable(TableName, Dictionary);
end;
{--------}
procedure TffeTableItem.CopyRecords(aSrcTable: TffDataSet;
  aCopyBLOBs: Boolean);
var
  WasOpen : Boolean;
begin
  WasOpen := Table.Active;
  if not Table.Active then
    Table.Open;
  Table.CopyRecords(aSrcTable, aCopyBLOBs);
  if not WasOpen then
    Table.Close;
end;
{=====================================================================}

{== TffeTableList ====================================================}
constructor TffeTableList.Create(aDatabase : TffeDatabaseItem);
begin
  inherited Create;
  FDatabase := aDatabase;
  Sorted := False;
end;
{--------}
destructor TffeTableList.Destroy;
begin
  Empty;
  inherited Destroy;
end;
{--------}
function TffeTableList.Add(const aTableName: TffName): longInt;
var
  aTable : TffeTableItem;
begin
  aTable := TffeTableItem.Create(FDatabase, aTableName);
  Insert(aTable);
  Result := pred(Count);
end;
{--------}
procedure TffeTableList.DropTable(aIndex: LongInt);
begin
  with Items[aIndex].Table do begin
    if Active then
      Close;
    DeleteTable;
  end;

  DeleteAt(aIndex);
end;
{--------}
function TffeTableList.GetItem(aIndex: LongInt): TffeTableItem;
begin
  Result := TffeTableItem(inherited Items[aIndex]);
end;
{--------}
function TffeTableList.Insert(aItem: TffeTableItem): Boolean;
begin
  aItem.tiParentList := Self;
  Result := inherited Insert(aItem);
end;
{--------}
procedure TffeTableList.Load;
var
  I: Integer;
  OldCursor: TCursor;
  Tables: TStringList;
begin
  Tables := TStringList.Create;
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    { Remove any existing tables for this database }
    Empty;
    FDatabase.GetTableNames(Tables);
    for I := 0 to Tables.Count - 1 do
      Add(Tables[I]);
  finally
    Screen.Cursor := OldCursor;
    Tables.Free;
  end;
end;
{=====================================================================}

{ TffexpSession }

constructor TffexpSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnLogin := FFELogin;
  ffePassword := '';
  ffeUserName := '';
end;

procedure TffexpSession.FFELogin(aSource: TObject; var aUserName,
  aPassword: TffName; var aResult: Boolean);
var
  FFLoginDialog : TffLoginDialog;
begin
  FFLoginDialog := TFFLoginDialog.Create(nil);
  try
    with FFLoginDialog do begin
      UserName := aUserName;
      Password := aPassword;
      ShowModal;
      aResult := ModalResult = mrOK;
      if aResult then begin
        aUserName := UserName;
        ffeUserName := UserName;
        aPassword := Password;
        ffePassword := Password;
        aResult := True;
      end;
    end;
  finally
    FFLoginDialog.Free;
  end;
end;

initialization
  NextEntitySerialKey := 0;
end.

