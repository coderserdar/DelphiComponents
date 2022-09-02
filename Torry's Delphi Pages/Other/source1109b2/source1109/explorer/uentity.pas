{$I fsdefine.inc}

Unit uentity;

Interface

Uses
  Classes,
  Controls,
  Consts,
  DB,
  Dialogs,
  Forms,
  SysUtils,
  Windows,
  fsclbase,
  fsllbase,
  fslldict,
  fsllprot,
  fsserverremoteclass,
  fsdb,
  fslllgcy,
  fslllog,
  fslogdlg,
  fssrbde;

Type
  TffexpSession = Class(TFSSession)
  Protected
    Procedure FFELogin(aSource: TObject;
      Var aUserName: TffName;
      Var aPassword: TffName;
      Var aResult: Boolean);
  Public
    ffePassword: String;
    ffeUserName: String;
  Public
    Constructor Create(AOwner: TComponent); Override;
  End;
  TffexpTable = Class(TFSTable);

Type
  TffeEntityType = (etServer, etDatabase, etTable);
  TffeServerList = Class;
  TffeDatabaseList = Class;
  TffeDatabaseItem = Class;
  TffeTableList = Class;
  TffeTableItem = Class;

  TffeEntityItem = Class(TFSSpecListItem)
  Protected { private}
    FEntityType: TffeEntityType;
    FEntityName: TffNetAddress;
    FEntitySerialKey: DWORD;
  Public
    Constructor Create(aEntityType: TffeEntityType; aEntityName: TffShStr);

    Function Compare(aKey: Pointer): Integer; Override;
    {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
      equal, >0 otherwise}

    Function Key: Pointer; Override;
    {-return a pointer to this item's key: it'll be a pointer to a
      shortstring}

    Property EntityType: TffeEntityType
      Read FEntityType;

    Property EntityName: TffNetAddress
      Read FEntityName;

    Property EntitySerialKey: DWORD
      Read FEntitySerialKey;
  End;

  TffeEntityList = Class(TFSNormalList)
  Protected
    Function GetItem(aIndex: Longint): TffeEntityItem;
  Public
    Function IndexOfName(Const aName: TffShStr): Longint;
    {-return the index of the entry whose name is given}

    Function IndexOfSerialKey(aSerialKey: DWORD): Longint;
    {-return the list index for a given entity identified by its
      serial key (the outline control keeps track of entities
      by the serial key) }

    Property Items[aIndex: Longint]: TffeEntityItem
    Read GetItem;
  End;

  TffeServerNotifyEvent = Procedure(aServerIndex: Longint) Of Object;

  TffeServerItem = Class(TffeEntityItem)
  Protected
    FClient: TFSClient;
    FDatabaseList: TffeDatabaseList;
    FProtocol: TfsProtocolType;
    FServerEngine: TFSRemoteServer;
    FSession: TffexpSession;
    FTransport: TFSParamConnect;

    Procedure siCheckAttached;
    Function siGetDatabaseCount: Longint;
    Function siGetDatabase(Const anIndex: Longint): TffeDatabaseItem;

  Public
    ServerID: Longint;
    Attached: Boolean;

    Constructor Create(aServerName: TffNetAddress;
      aProtocol: TfsProtocolType);
    Destructor Destroy; Override;
    Procedure AddAlias(aAlias: TffName;
      aPath: TffPath;
      aCheckSpace: Boolean); {!!.11}
    Function AddDatabase(aAlias: TffName): TffeDatabaseItem;
    Function Attach(aLog: TFSBaseLog): TffResult;
    Procedure Detach;
    Procedure DropDatabase(aDatabaseName: TffName);
    Procedure GetAliases(aList: TStrings);
    Function GetAutoInc(aTable: TFSTable; Var aStep: Longint): Int64;
    Function GetMaxRecords(aTable: TFSTable): LongWord;
    Function GetTableFlags(aTable: TFSTable): Word;
    Procedure LoadDatabases;

    Property DatabaseCount: Longint Read siGetDatabaseCount;
    Property Databases[Const anIndex: Longint]: TffeDatabaseItem
    Read siGetDatabase;
    Property ServerName: TffNetAddress Read FEntityName;
    Property Session: TffexpSession Read FSession;
    Property Protocol: TfsProtocolType Read FProtocol; {!!.10}
    Property Client: TFSClient Read FClient; {!!.11}
  End;

  TffeServerList = Class(TffeEntityList)
  Protected {private}
    FClient: TFSClient;
    FOnAttach: TffeServerNotifyEvent;
    FOnDetach: TffeServerNotifyEvent;
    FServerEngine: TFSRemoteServer;
    FTransport: TFSParamConnect;

    Function GetItem(aIndex: Longint): TffeServerItem;
  Public
    Constructor Create(aLog: TFSBaseLog);
    Destructor Destroy; Override;
    Procedure DetachAll;
    Function Insert(aItem: TffeServerItem): Boolean;
    Procedure Load;
    Procedure LoadRegisteredServers;

    Property Items[aIndex: Longint]: TffeServerItem
    Read GetItem;

    Property OnAttach: TffeServerNotifyEvent
      Read FOnAttach Write FOnAttach;
    Property OnDetach: TffeServerNotifyEvent
      Read FOnDetach Write FOnDetach;
  End;

  TffeDatabaseItem = Class(TffeEntityItem)
  Protected
    FDatabase: TfsDatabase;
    FServer: TffeServerItem;
    FTableList: TffeTableList;
    diParentList: TffeDatabaseList;

    Function diGetIsOpen: Boolean;
    Function diGetServer: TffeServerItem;
    Function diGetTable(Const anIndex: Longint): TffeTableItem;
    Function diGetTableCount: Longint;

  Public
    DatabaseID: Longint; { FF internal DB Identifier }
    Constructor Create(aServer: TffeServerItem; aAliasName: TffName);
    Destructor Destroy; Override;
    Procedure Close;
    Function AddTable(Const aTableName: TfsTableName): Longint;
    Procedure CreateTable(Const aTableName: TfsTableName; aDict: TFSInfoDict);
    Procedure DropTable(Const anIndex: Longint);
    { Drop the specified table from the list of tables. }
    Procedure GetTableNames(Tables: TStrings);
    Function IndexOf(aTable: TffeTableItem): Longint;
    Procedure LoadTables;
    Procedure Open;
    Procedure Rename(aNewName: TffNetAddress);
    Property Database: TfsDatabase Read FDatabase;
    Property DataBaseName: TffNetAddress Read FEntityName;
    Property IsOpen: Boolean Read diGetIsOpen;
    Property Server: TffeServerItem Read diGetServer;
    Property TableCount: Longint Read diGetTableCount;
    Property Tables[Const anIndex: Longint]: TffeTableItem
    Read diGetTable;
  End;

  TffeDatabaseList = Class(TffeEntityList)
  Protected
    FServer: TffeServerItem;

    Function GetItem(aIndex: Longint): TffeDatabaseItem;
  Public
    Constructor Create(aServer: TffeServerItem);
    Destructor Destroy; Override;
    Function Add(Const aDatabaseName: TffName): TffeDatabaseItem;
    Procedure DropDatabase(aIndex: Longint);
    Function Insert(aItem: TffeDatabaseItem): Boolean;
    Procedure Load(SesName: String);
    { Load the aliases for the server. }

    Property Items[aIndex: Longint]: TffeDatabaseItem
    Read GetItem;
  End;

  TffeTableItem = Class(TffeEntityItem)
  Protected {private}
    FParent: TffeDatabaseItem;
  Protected
    tiParentList: TffeTableList;
    Procedure AfterOpenEvent(aDataset: TDataset);
    Function GetDatabase: TffeDatabaseItem;
    Function GetDictionary: TFSInfoDict;
    Function GetRebuilding: Boolean;
    Function GetRecordCount: TffWord32;
    Function GetServer: TffeServerItem;
  Public
    Table: TffexpTable;
    DatabaseIndex: Longint;
    CursorID: Longint;
    TaskID: Longint;
    Constructor Create(aDatabase: TffeDatabaseItem; aTableName: TffName);
    Destructor Destroy; Override;

    Procedure CheckRebuildStatus(Var aCompleted: Boolean;
      Var aStatus: TffRebuildStatus);
    Function GetAutoInc(Var aStep: Longint): Int64;
    Function GetMaxRecords: Longint;
    Function GetTableFlags: Word;
    Procedure Pack(UndeleteRecords: Boolean; OnlyDeleted: boolean);
    Procedure Reindex(aIndexNum: Integer);
    Procedure Rename(aNewTableName: TffName);
    Procedure Restructure(aDictionary: TFSInfoDict; aFieldMap: TStrings; aRangeError: Boolean);
    Procedure SetAutoIncSeed(aValue: Int64; aStep: Longint);
    Procedure SetMaxRecordsSeed(aValue: Longint);
    Procedure SetTableFlagsSeed(aValue: Word);
    Procedure SetTablePassword(aOldValue, aNewValue: String);
    Procedure SetTablePasswordRest(aOldValue, aNewValue: String);
    Procedure Truncate;
    Procedure CopyRecords(aSrcTable: TfsDataSet; aCopyBLOBs: Boolean; CountPerTrans: Longint); {!!.10}

    Property Database: TffeDatabaseItem
      Read GetDatabase;
    Property Dictionary: TFSInfoDict
      Read GetDictionary;
    Property Rebuilding: Boolean
      Read GetRebuilding;
    Property RecordCount: TffWord32
      Read GetRecordCount;
    Property Server: TffeServerItem
      Read GetServer;
    Property TableName: TffNetAddress
      Read FEntityName;
  End;

  TffeTableList = Class(TffeEntityList)
  Protected
    FDatabase: TffeDatabaseItem;
    Function GetItem(aIndex: Longint): TffeTableItem;
  Public
    Constructor Create(aDatabase: TffeDatabaseItem);
    Destructor Destroy; Override;
    Function Add(Const aTableName: TffName): Longint;
    Procedure DropTable(aIndex: Longint);
    Function Insert(aItem: TffeTableItem): Boolean;
    Procedure Load;

    Property Items[aIndex: Longint]: TffeTableItem
    Read GetItem;
  End;

Const
  ffcConnectTimeout: Longint = 2000;
  {-Number of milliseconds we will wait for servers to respond to our
    broadcast. }

Implementation

Uses
  fsclcfg,
  fsdbbase,
  fsllcomm,
  fsllcomp,
  fslleng,
  ubase,
  uconsts,
  {$IFDEF DCC6ORLATER} {!!.03}
  RTLConsts, {!!.03}
  {$ENDIF} {!!.03}
  uconfig;

Const
  ffcLogName = 'fse.lg';
  ffcDatabaseClosed = 'Cannot perform this operation on a closed database';

Var
  NextEntitySerialKey: DWORD;

  {=====TffeEntityItem methods=====}

Constructor TffeEntityItem.Create(aEntityType: TffeEntityType; aEntityName: TffShStr);
Begin
  Inherited Create;
  FEntityType := aEntityType;
  FEntityName := aEntityName;
  FEntitySerialKey := NextEntitySerialKey;
  Inc(NextEntitySerialKey);
End;

Function TffeEntityItem.Compare(aKey: Pointer): Integer;
Begin
  Result := FFCmpShStr(PffShStr(aKey)^, EntityName, 255);
End;

Function TffeEntityItem.Key: Pointer;
Begin
  Result := @FEntityName;
End;

{=====TffeEntityList methods=====}

Function TffeEntityList.GetItem(aIndex: Longint): TffeEntityItem;
Begin
  If (aIndex < 0) Or (aIndex >= Count) Then
    Raise EListError.Create(SListIndexError);
  Result := TffeEntityItem(Inherited Items[aIndex]);
End;

Function TffeEntityList.IndexOfName(Const aName: TffShStr): Longint;
Begin
  For Result := 0 To Count - 1 Do
    If Items[Result].EntityName = aName Then Exit;
  Result := -1;
End;

Function TffeEntityList.IndexOfSerialKey(aSerialKey: DWORD): Longint;
Begin
  For Result := 0 To Count - 1 Do
    If Items[Result].EntitySerialKey = aSerialKey Then Exit;
  Result := -1;
End;

{===TffeServerItem===================================================}

Constructor TffeServerItem.Create(aServerName: TffNetAddress;
  aProtocol: TfsProtocolType);
Begin
  Inherited Create(etServer, FFShStrTrim(aServerName));
  FDatabaseList := TffeDatabaseList.Create(Self);
  FProtocol := aProtocol;
  Attached := False;
End;
{--------}

Destructor TffeServerItem.Destroy;
Begin
  Detach;
  FDatabaseList.Free;
  Inherited Destroy;
End;
{--------}

Procedure TffeServerItem.AddAlias(aAlias: TffName;
  aPath: TffPath;
  aCheckSpace: Boolean); {!!.11}
Begin
  FSession.AddAlias(aAlias, aPath, aCheckSpace); {!!.11}
End;
{--------}

Function TffeServerItem.AddDatabase(aAlias: TffName): TffeDatabaseItem;
Begin
  Result := FDatabaseList.Add(aAlias);
End;
{--------}

Function TffeServerItem.Attach(aLog: TFSBaseLog): TffResult;
Var
  OldCursor: TCursor;
Begin
  Result := DBIERR_NONE;

  { If we're already attached, then we don't need to do anything }
  If Attached Then Exit;

  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  Try

    If Not assigned(FTransport) Then
      FTransport := TFSParamConnect.Create(Nil);
    With FTransport Do
      Begin
        Mode := fstmSend;
        Enabled := True;
        Protocol := FProtocol;
        EventLog := aLog;
        EventLogEnabled := True;
        EventLogOptions := [fstpLogErrors];
        ServerName := FEntityName;
      End;

    If Not assigned(FServerEngine) Then
      FServerEngine := TFSRemoteServer.Create(Nil);
    With FServerEngine Do
      Begin
        Transport := FTransport;
      End;

    If Not assigned(FClient) Then
      FClient := TFSClient.Create(Nil);
    With FClient Do
      Begin
        TimeOut := Config.DefaultTimeout; {!!.11}
        ServerEngine := FServerEngine;
        AutoClientName := True;
        Active := True;
      End;

    If Not assigned(FSession) Then
      FSession := TffexpSession.Create(Nil);
    With FSession Do
      Begin
        ClientName := FClient.ClientName;
        AutoSessionName := True;
        Active := True;
      End;

    Attached := FSession.Active;
    If Attached Then
      Begin
        { Automatically load up all the databases for this server }
        If Not assigned(FDatabaseList) Then
          FDatabaseList := TffeDatabaseList.Create(Self);
        FDatabaseList.Load(FSession.SessionName);

        { Run the event-handler if any }
        With ServerList Do
          If Assigned(FOnAttach) Then
            FOnAttach(Index(FEntityName));
      End;
  Finally;
    Screen.Cursor := OldCursor;
  End;
End;
{--------}

Procedure TffeServerItem.Detach;
Var
  S: TffNetAddress;
Begin

  If assigned(FDatabaseList) Then
    Begin
      FDatabaseList.Free;
      FDatabaseList := Nil;
    End;

  If assigned(FSession) Then
    Begin
      FSession.Active := False;
      FSession.Free;
      FSession := Nil;
    End;

  If assigned(FClient) Then
    Begin
      FClient.Active := False;
      FClient.Free;
      FClient := Nil;
    End;

  If assigned(FTransport) Then
    Begin
      FTransport.State := fsesInactive;
      FTransport.Free;
      FTransport := Nil;
    End;

  If assigned(FServerEngine) Then
    Begin
      FServerEngine.Free;
      FServerEngine := Nil;
    End;

  Attached := False;

  S := ServerName;
  With ServerList Do
    If Assigned(FOnDetach) Then
      FOnDetach(Index(S));
End;
{--------}

Procedure TffeServerItem.DropDatabase(aDatabaseName: TffName);
Begin
  siCheckAttached;
  FDatabaseList.DropDatabase(FDatabaseList.IndexOfName(aDatabaseName));
End;
{--------}

Procedure TffeServerItem.GetAliases(aList: TStrings);
Begin
  siCheckAttached;
  FSession.GetAliasNames(aList);
End;
{--------}

Function TffeServerItem.GetAutoInc(aTable: TFSTable; Var aStep: Longint): Int64;
Begin
  Result := 1;
  FServerEngine.TableGetAutoInc(aTable.CursorID, Result, aStep);
End;

Function TffeServerItem.GetMaxRecords(aTable: TFSTable): LongWord;
Begin
  Result := 0;
  FServerEngine.TableGetMaxRecords(aTable.CursorID, Result);
End;

Function TffeServerItem.GetTableFlags(aTable: TFSTable): Word;
Begin
  Result := 0;
  FServerEngine.TableGetTableFlags(aTable.CursorID, Result);
End;
{--------}

Procedure TffeServerItem.LoadDatabases;
Begin
  siCheckAttached;
  FDatabaseList.Load(FSession.SessionName);
End;
{--------}

Procedure TffeServerItem.siCheckAttached;
Begin
  If Not Attached Then
    Attach(Nil);
End;
{--------}

Function TffeServerItem.siGetDatabaseCount: Longint;
Begin
  Result := FDatabaseList.Count;
End;
{--------}

Function TffeServerItem.siGetDatabase(Const anIndex: Longint)
  : TffeDatabaseItem;
Begin
  Result := TffeDatabaseItem(FDatabaseList[anIndex]);
End;
{====================================================================}

{===TffeServerList===================================================}

Constructor TffeServerList.Create(aLog: TFSBaseLog);
Begin
  Inherited Create;
  Sorted := True;

  { The transport will be left inactive.  Its sole purpose is to
    broadcast for servers using the protocol identified in the registry. }
  FTransport := TFSParamConnect.Create(Nil);
  With FTransport Do
    Begin
      Mode := fstmSend;
      Enabled := True;
      Protocol := ptRegistry;
      EventLog := aLog;
      EventLogEnabled := True;
      EventLogOptions := [fstpLogErrors];
      Name := 'ffeTransport';
    End;

  FServerEngine := TFSRemoteServer.Create(Nil);
  With FServerEngine Do
    Begin
      Transport := FTransport;
      Name := 'ffeServerEngine';
    End;

  FClient := TFSClient.Create(Nil);
  With FClient Do
    Begin
      ServerEngine := FServerEngine;
      Name := 'ffeClient';
      ClientName := Name;
      Timeout := ffcConnectTimeout;
      Active := True;
    End;

End;
{--------}

Destructor TffeServerList.Destroy;
Begin
  Empty;
  FClient.Active := False;
  FClient.Free;
  FServerEngine.Free;
  FTransport.State := fsesInactive;
  FTransport.Free;
  Inherited Destroy;
End;
{--------}

Procedure TffeServerList.DetachAll;
Var
  I: Integer;
Begin
  For I := 0 To Count - 1 Do
    With Items[I] Do
      If Attached Then Detach;
End;
{--------}

Function TffeServerList.Insert(aItem: TffeServerItem): Boolean;
Begin
  Result := Inherited Insert(aItem);
End;
{--------}

Function TffeServerList.GetItem(aIndex: Longint): TffeServerItem;
Begin
  Result := TffeServerItem(Inherited Items[aIndex]);
End;
{--------}

Procedure TffeServerList.Load;
Var
  Servers: TStringList;
  I: Integer;
  tryProt: TfsProtocolType; {!!.10}

  Function ServerRegistered(Const ServerName: String): Boolean; {begin !!.06}
  Var
    Idx: Integer;
  Begin
    Result := False;
    With Config Do
      For Idx := 0 To Pred(RegisteredServers.Count) Do
        If FFAnsiCompareText(ServerName, RegisteredServers[Idx]) = 0 Then
          Begin {!!.10}
            Result := True;
            Exit;
          End;
  End; {end !!.06}

Begin
  Empty;

  //  if not (Config.Protocol = TffSingleUserProtocol) then              {!!.06}
  LoadRegisteredServers;

  {Begin !!.07}
  { added loop to try all protocols. we no longer let the user
    select protocol, but instead list all servers on all protocols. }
  { Broadcast for currently active servers }
  Servers := TStringList.Create;
  Try
    For tryProt := ptSingleUser To ptIPXSPX Do
      Begin
        Try
          FTransport.Enabled := False;
          FTransport.Enabled := True;
          FTransport.Enabled := False;
          FTransport.Protocol := tryProt;
          FTransport.Enabled := True;
          FClient.Active := True;
          FClient.GetServerNames(Servers);
          For I := 0 To Servers.Count - 1 Do
            If Not ServerRegistered(Servers[I]) Then {!!.06}
              Insert(TffeServerItem.Create(Servers[I], tryProt));
        Except
          { swallow all errors. assume that the particular protocol failed. }
        End;
      End;
    {End !!.07}
  Finally
    Servers.Free;
  End;
End;
{--------}

Procedure TffeServerList.LoadRegisteredServers;
Var
  I: Integer;
Begin
  With Config.RegisteredServers Do
    For I := 0 To Count - 1 Do
      Self.Insert(TffeServerItem.Create(Strings[I], ptTCPIP)); {!!.10} {changed protocol type}
End;
{=====================================================================}

{== TffeDatabaseItem =================================================}

Constructor TffeDatabaseItem.Create(aServer: TffeServerItem;
  aAliasName: TffName);
Begin
  Inherited Create(etDatabase, aAliasName);
  FServer := aServer;
  DatabaseID := -1;
  diParentList := Nil;
  FDatabase := TfsDatabase.Create(Nil);
  FTableList := TffeTableList.Create(Self);
  With FDatabase Do
    Begin
      SessionName := aServer.Session.SessionName;
      DataBaseName := 'exp' + aAliasName;
      AliasName := aAliasName;
    End;
End;
{--------}

Destructor TffeDatabaseItem.Destroy;
Begin
  If IsOpen Then Close;
  FTableList.Free;
  FDatabase.Free;
  Inherited Destroy;
End;
{--------}

Procedure TffeDatabaseItem.Close;
Begin
  FDatabase.Connected := False;
End;
{--------}

Function TffeDatabaseItem.AddTable(Const aTableName: TfsTableName)
  : Longint;
Begin
  Result := FTableList.Add(aTableName);
End;
{--------}

Procedure TffeDatabaseItem.CreateTable(Const aTableName: TfsTableName;
  aDict: TFSInfoDict);
Begin
  If Not IsOpen Then
    Open;

  Check(FDatabase.CreateTable(False, aTableName, aDict));
End;
{--------}

Procedure TffeDatabaseItem.DropTable(Const anIndex: Longint);
Begin
  FTableList.DropTable(anIndex);
End;
{--------}

Function TffeDatabaseItem.diGetIsOpen: Boolean;
Begin
  Result := FDatabase.Connected;
End;
{--------}

Function TffeDatabaseItem.diGetServer: TffeServerItem;
Begin
  Result := FServer;
End;
{--------}

Function TffeDatabaseItem.diGetTable(Const anIndex: Longint): TffeTableItem;
Begin
  Result := TffeTableItem(FTableList[anIndex]);
End;
{--------}

Function TffeDatabaseItem.diGetTableCount: Longint;
Begin
  Result := FTableList.Count;
End;
{--------}

Procedure TffeDatabaseItem.GetTableNames(Tables: TStrings);
Begin
  If Tables Is TStringList Then
    TStringList(Tables).Sorted := True;
  FDatabase.GetTableNames(Tables);
End;
{--------}

Function TffeDatabaseItem.IndexOf(aTable: TffeTableItem): Longint;
Begin
  Result := FTableList.IndexOfName(aTable.TableName);
End;
{--------}

Procedure TffeDatabaseItem.LoadTables;
{ Find all the tables in the database and add to the table list. }
Var
  Tables: TStringList;
  I: Integer;
Begin
  Tables := TStringList.Create;
  Try
    FTableList.Empty;
    //    try
    FDatabase.GetTableNames(Tables);
    For I := 0 To Tables.Count - 1 Do
      FTableList.Add(Tables[I]);
    {    except
          on EffDatabaseError do
            {do nothing}
    {      else
            raise;
        end;}
  Finally
    Tables.Free;
  End;
End;
{--------}

Procedure TffeDatabaseItem.Open;
Begin
  FDatabase.Connected := True;
End;
{--------}

Procedure TffeDatabaseItem.Rename(aNewName: TffNetAddress);
Begin
  FDatabase.Close;
  Check(FServer.Session.ModifyAlias(FEntityName, aNewName, '', False)); {!!.11}
  FEntityName := aNewName;
End;
{=====================================================================}

{== TffeDatabaseList =================================================}

Constructor TffeDatabaseList.Create(aServer: TffeServerItem);
Begin
  Inherited Create;
  FServer := aServer;
  Sorted := False;
End;
{--------}

Destructor TffeDatabaseList.Destroy;
Begin
  { Close all databases. }
  Empty;
  Inherited Destroy;
End;
{--------}

Function TffeDatabaseList.Add(Const aDatabaseName: TffName)
  : TffeDatabaseItem;
Begin
  Result := TffeDatabaseItem.Create(FServer, aDatabaseName);
  Insert(Result);
End;
{--------}

Procedure TffeDatabaseList.DropDatabase(aIndex: Longint);
Begin
  With Items[aIndex] Do
    Begin
      FDatabase.Connected := False;
      FServer.Session.DeleteAlias(DataBaseName);
    End;
  DeleteAt(aIndex);
End;
{--------}

Function TffeDatabaseList.GetItem(aIndex: Longint): TffeDatabaseItem;
Begin
  Result := TffeDatabaseItem(Inherited Items[aIndex]);
End;
{--------}

Function TffeDatabaseList.Insert(aItem: TffeDatabaseItem): Boolean;
Begin
  aItem.diParentList := Self;
  Result := Inherited Insert(AItem);
End;
{--------}

Procedure TffeDatabaseList.Load(SesName: String);
Var
  Aliases: TStringList;
  Index: Longint;
  OldCursor: TCursor;
Begin
  OldCursor := Screen.Cursor;
  Aliases := TStringList.Create;
  Screen.Cursor := crHourglass;
  Try
    Empty;
    FServer.GetAliases(Aliases);
    For Index := 0 To pred(Aliases.Count) Do
      Begin
        Add(Aliases[Index]);
      End;
  Finally
    Aliases.Free;
    Screen.Cursor := OldCursor;
  End;
End;
{=====================================================================}

{== TffeTableItem ====================================================}

Constructor TffeTableItem.Create(aDatabase: TffeDatabaseItem;
  aTableName: TffName);
Begin
  Inherited Create(etTable, aTableName);
  FParent := aDatabase;
  CursorID := -1;
  TaskID := -1;
  tiParentList := Nil;
  Table := TffexpTable.Create(Nil);
  Table.FilterEval := fseServer;
  With Table Do
    Begin
      SessionName := aDatabase.Server.Session.SessionName;
      DataBaseName := aDatabase.Database.DataBaseName;
      TableName := aTableName;
      ReadOnly := False;
      AfterOpen := AfterOpenEvent;
    End;
End;
{--------}

Destructor TffeTableItem.Destroy;
Begin
  Table.Free;
  Inherited Destroy;
End;
{--------}

Procedure TffeTableItem.CheckRebuildStatus(Var aCompleted: Boolean;
  Var aStatus: TffRebuildStatus);
Var
  WasOpen: Boolean;
Begin
  WasOpen := Database.IsOpen;
  If Not Database.IsOpen Then
    Database.Open;

  Try
    Check(FParent.Server.Session.GetTaskStatus(TaskID, aCompleted, aStatus));
    If aCompleted Then
      TaskID := -1;
  Except
    TaskID := -1;
  End;
  If Not WasOpen Then
    Database.Close;
End;
{--------}

Function TffeTableItem.GetAutoInc(Var aStep: Longint): Int64;
Var
  WasOpen: Boolean;
Begin
  WasOpen := Table.Active;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Table.Active Then
    Table.Open;

  Result := FParent.Server.GetAutoInc(Table, aStep);

  If Not WasOpen Then
    Table.Close;
End;

Function TffeTableItem.GetMaxRecords: Longint;
Var
  WasOpen: Boolean;
Begin
  WasOpen := Table.Active;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Table.Active Then
    Table.Open;

  Result := FParent.Server.GetMaxRecords(Table);

  If Not WasOpen Then
    Table.Close;
End;

Function TffeTableItem.GetTableFlags: Word;
Var
  WasOpen: Boolean;
Begin
  WasOpen := Table.Active;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Table.Active Then
    Table.Open;

  Result := FParent.Server.GetTableFlags(Table);

  If Not WasOpen Then
    Table.Close;
End;
{--------}

Procedure TffeTableItem.AfterOpenEvent(aDataset: TDataset);
Var
  I: Integer;
Begin
  With aDataset Do
    For I := 0 To FieldCount - 1 Do
      Case Fields[I].DataType Of
        ftString: TStringField(Fields[I]).Transliterate := False;
        ftMemo: TMemoField(Fields[I]).Transliterate := False;
      End;
End;
{--------}

Function TffeTableItem.GetDatabase: TffeDatabaseItem;
Begin
  Result := FParent;
End;
{--------}

Function TffeTableItem.GetDictionary: TFSInfoDict;
Var
  WasOpen: Boolean;
Begin
  WasOpen := Table.Active;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Table.Active Then
    Table.Open;

  Result := Table.Dictionary;

  If Not WasOpen Then
    Table.Close;
End;
{--------}

Function TffeTableItem.GetRebuilding: Boolean;
Begin
  Result := TaskID <> -1;
End;
{--------}

Function TffeTableItem.GetRecordCount: TffWord32;
Var {!!.06}
  WasOpen: Boolean; {!!.06}
Begin {!!.06}
  WasOpen := Table.Active;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Table.Active Then
    Table.Open;

  Result := Table.RecordCount;

  If WasOpen Then {!!.06}
    Table.Close; {!!.06}
End;
{--------}

Function TffeTableItem.GetServer: TffeServerItem;
Begin
  Result := FParent.Server;
End;
{--------}

Procedure TffeTableItem.Pack(UndeleteRecords: Boolean; OnlyDeleted: boolean);
Var
  WasOpen: Boolean;
Begin
  WasOpen := Database.IsOpen;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Database.IsOpen Then
    Database.Open;

  Check(Database.FDatabase.PackTable(Table.TableName, TaskID, UndeleteRecords, OnlyDeleted));

  If Not WasOpen Then
    Database.Close;
End;
{--------}

Procedure TffeTableItem.Reindex(aIndexNum: Integer);
Var
  WasOpen: Boolean;
Begin
  WasOpen := Database.IsOpen;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Database.IsOpen Then
    Database.Open;

  If Table.Active Then Table.Close;
  Check(Database.FDatabase.ReindexTable(Table.TableName, aIndexNum, TaskID));

  If Not WasOpen Then
    Database.Close;
End;
{--------}

Procedure TffeTableItem.Rename(aNewTableName: TffName);
Begin
  With Table Do
    Begin
      If Active Then Close;
      RenameTable(aNewTableName);
      FEntityName := aNewTableName;
    End;
End;
{--------}

Procedure TffeTableItem.Restructure(aDictionary: TFSInfoDict; aFieldMap: TStrings; aRangeError: Boolean);
Var
  Result: TffResult;
  WasOpen: Boolean;
Begin
  WasOpen := Database.IsOpen;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Database.IsOpen Then
    Database.Open;

  Table.Close;

  Result := Database.FDatabase.RestructureTable(Tablename, aDictionary, aFieldMap, TaskID, aRangeError);
  If Result = DBIERR_INVALIDRESTROP Then
    Raise Exception.Create('Cannot preserve data if user-defined indexes have been added or changed')
  Else
    Check(Result);

  If Not WasOpen Then
    Database.Close;
End;
{--------}

Procedure TffeTableItem.SetAutoIncSeed(aValue: Int64; aStep: Longint);
Var
  WasOpen: Boolean;
Begin
  WasOpen := Table.Active;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Table.Active Then
    Table.Open;

  Check(Table.SetTableAutoIncValue(aValue, aStep));

  If Not WasOpen Then
    Table.Close;
End;

Procedure TffeTableItem.SetMaxRecordsSeed(aValue: Longint);
Var
  WasOpen: Boolean;
Begin
  WasOpen := Table.Active;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Table.Active Then
    Table.Open;

  Check(Table.SetTableMaxRecordsValue(aValue));

  If Not WasOpen Then
    Table.Close;
End;

Procedure TffeTableItem.SetTableFlagsSeed(aValue: Word);
Var
  WasOpen: Boolean;
Begin
  WasOpen := Table.Active;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Table.Active Then
    Table.Open;

  Check(Table.SetTableFlagsValue(aValue));

  If Not WasOpen Then
    Table.Close;
End;

Procedure TffeTableItem.SetTablePassword(aOldValue, aNewValue: String);
Var
  WasOpen: Boolean;
Begin
  WasOpen := Table.Active;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Table.Active Then
    Table.Open;

  Check(Table.SetTablePassword(aOldValue, aNewValue));

  If Not WasOpen Then
    Table.Close;
End;

Procedure TffeTableItem.SetTablePasswordRest(aOldValue, aNewValue: String);
Var
  WasOpen: Boolean;
Begin
  WasOpen := Table.Active;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Table.Active Then
    Table.Open;

  Check(Table.SetTablePasswordRest(aOldValue, aNewValue));

  If Not WasOpen Then
    Table.Close;
End;
{--------}

Procedure TffeTableItem.Truncate;
Begin
  { Make sure we suck in the dictionary before the table gets deleted }
  GetDictionary;
  With Table Do
    Begin
      Close;
      DeleteTable;
    End;
  Database.CreateTable(TableName, Dictionary);
End;
{--------}

Procedure TffeTableItem.CopyRecords(aSrcTable: TfsDataSet;
  aCopyBLOBs: Boolean; CountPerTrans: Longint);
Var
  WasOpen: Boolean;
Begin
  WasOpen := Table.Active;
  Table.session.Active := False;
  Table.session.Active := True;
  If Not Table.Active Then
    Table.Open;
  Table.CopyRecords(aSrcTable, aCopyBLOBs, CountPerTrans);
  If Not WasOpen Then
    Table.Close;
End;
{=====================================================================}

{== TffeTableList ====================================================}

Constructor TffeTableList.Create(aDatabase: TffeDatabaseItem);
Begin
  Inherited Create;
  FDatabase := aDatabase;
  Sorted := False;
End;
{--------}

Destructor TffeTableList.Destroy;
Begin
  Empty;
  Inherited Destroy;
End;
{--------}

Function TffeTableList.Add(Const aTableName: TffName): Longint;
Var
  aTable: TffeTableItem;
Begin
  aTable := TffeTableItem.Create(FDatabase, aTableName);
  Insert(aTable);
  Result := pred(Count);
End;
{--------}

Procedure TffeTableList.DropTable(aIndex: Longint);
Begin
  With Items[aIndex].Table Do
    Begin
      If Active Then
        Close;
      DeleteTable;
    End;

  DeleteAt(aIndex);
End;
{--------}

Function TffeTableList.GetItem(aIndex: Longint): TffeTableItem;
Begin
  Result := TffeTableItem(Inherited Items[aIndex]);
End;
{--------}

Function TffeTableList.Insert(aItem: TffeTableItem): Boolean;
Begin
  aItem.tiParentList := Self;
  Result := Inherited Insert(aItem);
End;
{--------}

Procedure TffeTableList.Load;
Var
  I: Integer;
  OldCursor: TCursor;
  Tables: TStringList;
Begin
  Tables := TStringList.Create;
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  Try
    { Remove any existing tables for this database }
    Empty;
    FDatabase.GetTableNames(Tables);
    For I := 0 To Tables.Count - 1 Do
      Add(Tables[I]);
  Finally
    Screen.Cursor := OldCursor;
    Tables.Free;
  End;
End;
{=====================================================================}

{ TffexpSession }

Constructor TffexpSession.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);

  OnLogin := FFELogin;
  ffePassword := '';
  ffeUserName := '';
End;

Procedure TffexpSession.FFELogin(aSource: TObject; Var aUserName,
  aPassword: TffName; Var aResult: Boolean);
Var
  FFLoginDialog: TfsLoginDialog;
Begin
  FFLoginDialog := TFsLoginDialog.Create(Nil);
  Try
    With FFLoginDialog Do
      Begin
        UserName := aUserName;
        Password := aPassword;
        ShowModal;
        aResult := ModalResult = mrOK;
        If aResult Then
          Begin
            aUserName := UserName;
            ffeUserName := UserName;
            aPassword := Password;
            ffePassword := Password;
            aResult := True;
          End;
      End;
  Finally
    FFLoginDialog.Free;
  End;
End;

Initialization
  NextEntitySerialKey := 0;
End.

