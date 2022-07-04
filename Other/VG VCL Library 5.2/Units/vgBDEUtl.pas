{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         vgBDEUtl unit                                 }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L- }

unit vgBDEUtl;

interface
uses BDE, Classes, Controls, Forms, DB, vgBDE, vgDBUtl, DBTables;

{ Locking }
procedure LockDatabase(Database: TDatabase);
procedure UnLockDatabase(Database: TDatabase);
{ Locking is required in multithreaded applications }

procedure InitLockDBDataSet(DataSetClass: TDBDataSetClass);
procedure InitLockDBDataSets(DataSetClasses: array of TDBDataSetClass);
procedure DoneLockDBDataSet(DataSetClass: TDBDataSetClass);
procedure DoneLockDBDataSets(DataSetClasses: array of TDBDataSetClass);

{ --- Transactions }
{ Transaction handling mechanism is designed to override       }
{ default BDE transaction to make BDE transaction ReadCommited }

function TransActive(Database: TDatabase): Boolean;
{ Returns True of Database has active transaction }

procedure DefaultStartTransaction(Database: TDatabase);
{ Starts transaction if Database not in transaction yet }

procedure DefaultCommitEx(Database: TDatabase; Restart: Boolean);
{ Commits transaction and start onother one             }

procedure DefaultRollbackEx(Database: TDatabase; Restart: Boolean);
{ Fetches all open queries, rollback current transaction }
{ and start onother one                                  }

procedure DefaultCommit(Database: TDatabase);
procedure DefaultRollback(Database: TDatabase);

{ --- Fetching }
procedure DefaultBDEFetchDataSet(DataSet: TDataSet);
{ Fetches all records in dataset                   }
{ Note that FetchAll method doesn't work sometimes }

procedure FetchQueries(Database: TDatabase);
{ Fetches all records in all open datasets of Database }

{ --- Posting and deleting }
function DefaultDataSetPost(ADataSet: TDataSet; Apply: Boolean): Boolean;
{ Starts transaction and posts changes in DataSet }

function DefaultDataSetDelete(ADataSet: TDataSet; Confirm: Boolean): Boolean;
{ Starts transaction and tries to delete current record }

{ -- Execute in transaction }
function DataSetExecSQL(DataSet: TDBDataSet): Integer;
procedure DataSetExecProc(DataSet: TDBDataSet);

{ --- BDE Services }
procedure GetAliasNames(const SessionName: string; ConfigMode: TConfigMode; List: TStrings);

{ --- Queries on-the-fly }
function FindDatabase(const ASessionName, ADatabaseName: String): TDatabase;
function FindDataSetDatabase(DataSet: TDBDataSet): TDatabase;

function CreateQuery(ADatabase: TDatabase; ASQL: String;
  ParamName: String; ParamValues: Variant): TQuery;

function ExecWithParam(ADatabase: TDatabase; ASQL: String;
  ParamName: String; ParamValues: Variant): Integer;

function ExecWithParamA(Query: TQuery;
  ParamName: String; ParamValues: Variant): Integer;

function ExecQuery(ADatabase: TDatabase; ASQL: String): Integer;

function ExecQueryA(Query: TQuery): Integer;

function QueryWithParam(ADatabase: TDatabase;
  ASQL: String; FieldName, ParamName: String; ParamValues: Variant): Variant;

function QueryWithParamA(Query: TQuery;
  FieldName, ParamName: String; ParamValues: Variant): Variant;

function QueryValue(ADatabase: TDatabase;
  ASQL: String; FieldName: String): Variant;

function QueryValueA(Query: TQuery;
  FieldName: String): Variant;

{ Distinct databases }
procedure ChangeToDatabase(DataSet: TDBDataSet; ADatabase: TDatabase);
{ Changes DatabaseName and SessionName of DataSet }

function CloneToDatabaseOwner(Source: TDBDataSet; AOwner: TComponent; ADatabase: TDatabase): TDBDataSet;
{ Creates a clone of DataSet that can be opened within ADatabase }

function CloneToDatabase(Source: TQuery; ADatabase: TDatabase): TDBDataSet;
{ CloneToDatabaseOwner macro }

{ --- UpdateSQL runtime building }
function BuildUpdateSQLEx(DataSet: TDataSet; TableName: String; ExcludeFields, KeyFieldNames: String; UpdateSQL: TUpdateSQL): TDataSetUpdateObject;
function BuildUpdateSQL(DataSet: TDataSet; TableName: String; KeyFieldNames: String; UpdateSQL: TUpdateSQL): TDataSetUpdateObject;
procedure SetUpdateQueryParams(Query: TQuery; DataSet: TDataSet);
procedure SetUpdateSQLParams(UpdateSQL: TUpdateSQL; DataSet: TDataSet);

{ Edit dataset dialog }
function DefaultEditKeyFieldsCU(DataSet, CUDataSet: TDataSet; KeyFields: String;
  FormClass: TFormClass; InsertCaption, EditCaption: String; IsNewRecord: Boolean): Boolean;

function DefaultEditCU(DataSet, CUDataSet: TDataSet;
  FormClass: TFormClass; InsertCaption, EditCaption: String; IsNewRecord: Boolean): Boolean;

function DefaultDeleteKeyFieldsCU(DataSet, CUDataSet: TDataSet; KeyFields: String; Confirm: Boolean): Boolean;

function DefaultDeleteCU(DataSet, CUDataSet: TDataSet; Confirm: Boolean): Boolean;

{ These functions works with two datasets }
{ DataSet is TDataSet that user navigates through DBGrid                          }
{ CUDataSet is Query that selects only one record with CachedUpdates = True }
{ For editing or deleting you should create CUDataSet and call one of this  }
{ functions. After editing/deleting you should reopen DataSet and find      }
{ edited record                                                             }

{ Cached updates processing }
procedure DefaultUpdateRecord(DataSet: TDataSet; UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
procedure DefaultUpdateError(DataSet: TDataSet; E: EDatabaseError; UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);

{ --- Key generation in SP }
procedure HandleNewRecord(GenIDProc: TStoredProc; FieldID: TField);
{ If FieldID is Null then generates new ID for it }

{ --- Blobs }
procedure WriteBlob(Field: TField);
procedure WriteBlobs(DataSet: TDataSet);
{ Creates TBlobStream object(s) and writes data }

{ --- Exceptions }
procedure DBExceptionTranslate(E: EDBEngineError);
{ Translates error code in E into verbal message              }
{ Note that function doesn't call Application.HandleException }

{ --- Misc }
function DBSet(DataSource: TDataSource): TDBDataSet;
procedure SetTableRange(Table: TTable; const AIndexFields: String; IndexValues: Variant);

{ --- Batch operations }
procedure BatchMove(Source: TDataSet; Dest: TDBDataSet; Mode: TBatchMode; AbortOnKeyViol, AbortOnProblem: Boolean;
  const ChangedTableName, KeyViolTableName, ProblemTableName: string; Records, CommitCount: Integer;
  var ChangedCount, KeyViolCount, ProblemCount, MovedCount: Integer);

{ --- BDE }
function BDEVersion(Version: TStrings): SYSVersion;
{ Returns BDE installed version }

procedure KillCursorChangeCallback(var OldCBData: Longint; var OldCBBuf: Pointer;
  var OldCBBufLen: Word; var OldCBFunc: pfDBICallBack);
procedure RestoreCursorChangeCallback(OldCBData: Longint; OldCBBuf: Pointer;
  OldCBBufLen: Word; OldCBFunc: pfDBICallBack);
{ Kills and restores TBDECallback object that handles screen cursor changes   }
{ in long pauses                                                              }
{ Note that in Delphi 3.0 or higher you can use Session.SQLHourglass property }

function GetQuoteChar(Database: TDatabase): string;

procedure BDELock;
procedure BDEUnlock;

var
  LockDatabaseProc  : TDatabaseProc = nil;
  UnLockDatabaseProc: TDatabaseProc = nil;
  TransRestart      : Boolean       = False;
  StartTransaction  : TDatabaseProc = DefaultStartTransaction;
  CommitEx          : TEndTransProc = DefaultCommitEx;
  RollbackEx        : TEndTransProc = DefaultRollbackEx;
  Commit            : TDatabaseProc = DefaultCommit;
  Rollback          : TDatabaseProc = DefaultRollback;
  QueryClass        : TQueryClass   = TQuery;

implementation
uses Windows, {$IFNDEF _D3_}DBConsts,{$ENDIF} vgDBRes, {$IFDEF _D3_}BdeConst,{$ENDIF}
  SysUtils, vgUtils, vgVCLUtl, vgDBPrms;

var
  FBDELock: TRTLCriticalSection;

procedure BDELock;
begin
  EnterCriticalSection(FBDELock);
end;

procedure BDEUnlock;
begin
  LeaveCriticalSection(FBDELock);
end;

procedure LockDatabase(Database: TDatabase);
begin
  if Database is TvgDatabase then
    TvgDatabase(Database).Lock
  else if Assigned(LockDatabaseProc) then
    LockDatabaseProc(Database) else BDELock;
end;

procedure UnLockDatabase(Database: TDatabase);
begin
  if Database is TvgDatabase then
    TvgDatabase(Database).Unlock
  else if Assigned(LockDatabaseProc) then
    UnLockDatabaseProc(Database) else BDEUnlock;
end;

type

{$IFNDEF _D4_}
{$IFDEF _D3_}
  TBDEDataSetHack = class(TDataSet)
  public
    FHandle: HDBICur;
    FRecProps: RecProps;
    FLocale: TLocale;
    FExprFilter: HDBIFilter;
    FFuncFilter: HDBIFilter;
    FFilterBuffer: PChar;
    FIndexFieldMap: DBIKey;
    FExpIndex: Boolean;
    FCaseInsIndex: Boolean;
    FCachedUpdates: Boolean;
    FInUpdateCallback: Boolean;
    FCanModify: Boolean;
    FCacheBlobs: Boolean;
    FKeySize: Word;
    FUpdateCBBuf: PDELAYUPDCbDesc;
    FUpdateCallback: TBDECallback;
    FAsyncCallback: TBDECallback;
  end;
{$ELSE}
  TBDEDataSetHack = class(TComponent)
  private
    FFields: TList;
    FDataSources: TList;
    FFieldDefs: TFieldDefs;
    FBuffers: PBufferList;
    FBufListSize: Integer;
    FBufferCount: Integer;
    FRecordCount: Integer;
    FActiveRecord: Integer;
    FCurrentRecord: Integer;
    FHandle: HDBICur;
    FBOF: Boolean;
    FEOF: Boolean;
    FState: TDataSetState;
    FAutoCalcFields: Boolean;
    FDefaultFields: Boolean;
    FCanModify: Boolean;
    FModified: Boolean;
    FStreamedActive: Boolean;
    FInfoQueryMode: Boolean;
    FDisableState: TDataSetState;
    FEnableEvent: TDataEvent;
    FFiltered: Boolean;
    FFound: Boolean;
    FRecProps: RecProps;
    FRawFieldCount: Integer;
    FRecordSize: Word;
    FBookmarkSize: Word;
    FRecInfoOfs: Word;
    FBookmarkOfs: Word;
    FRecNoStatus: TRecNoStatus;
    FKeySize: Word;
    FExpIndex: Boolean;
    FCaseInsIndex: Boolean;
    FCalcFieldsSize: Word;
    FRecBufSize: Word;
    FDisableCount: Integer;
    FFirstDataLink: TDataLink;
    FLocale: TLocale;
    FDesigner: TDataSetDesigner;
    FKeyBuffers: array[TKeyIndex] of PKeyBuffer;
    FKeyBuffer: PKeyBuffer;
    FCalcBuffer: PChar;
    FFilterText: string;
    FFilterOptions: TFilterOptions;
    FExprFilter: HDBIFilter;
    FFuncFilter: HDBIFilter;
    FFilterBuffer: PChar;
    FIndexFieldCount: Integer;
    FIndexFieldMap: DBIKey;
    FBDECalcFields: Boolean;
    FCachedUpdates: Boolean;
    FUpdateCBBuf: PDELAYUPDCbDesc;
    FUpdateCallback: TBDECallback;
    FInUpdateCallback: Boolean;
    FUpdateErrCode: DBIResult;
    FAsyncCallback: TBDECallback;
  end;
{$ENDIF}
{$ENDIF}

  PSetDBFlagData = ^TSetDBFlagData;
  TSetDBFlagData = record
    DataSetClass: TDBDataSetClass;
    ProcSetDBFlag, ProcOpenCursor, ProcGetProviderAttributes: Pointer;
  end;

var
  SetDBFlags: TList = nil;

function FindSetDBFlagData(DataSetClass: TDBDataSetClass): PSetDBFlagData;
var
  I: Integer;
begin
  if Assigned(SetDBFlags) then
  begin
    for I := 0 to SetDBFlags.Count - 1 do
    begin
      Result := SetDBFlags[I];
      if Result^.DataSetClass = DataSetClass then Exit;
    end;
  end;
  Result := nil;
end;

procedure FreeSetDBFlags;
var
  I: Integer;
  P: PSetDBFlagData;
begin
  if Assigned(SetDBFlags) then
  begin
    for I := SetDBFlags.Count - 1 downto 0  do
    begin
      P := SetDBFlags[I];
      DoneLockDBDataSet(P^.DataSetClass);
    end;
  end;
end;

type
  TSetDBFlagLocker = class(TObject)
{$IFDEF _D4_}
    function SetDBFlag(Flag: Integer; Value: Boolean): Boolean; virtual;
    procedure GetProviderAttributes(List: TList); virtual;
{$ELSE}
    procedure SetDBFlag(Flag: Integer; Value: Boolean); virtual;
  {$IFNDEF _D4_}
    procedure OpenCursor{$IFDEF _D3_}(InfoQuery: Boolean){$ENDIF}; virtual;
  {$ENDIF}
{$ENDIF}
  end;

{$IFDEF _D4_}
function TSetDBFlagLocker.SetDBFlag(Flag: Integer; Value: Boolean): Boolean;
{$ELSE}
procedure TSetDBFlagLocker.SetDBFlag(Flag: Integer; Value: Boolean);
{$ENDIF}
var
  Data: PSetDBFlagData;
  DataSet: TDBDataSet;
  DataSetClass: TDBDataSetClass;
  SaveFlag: Integer;
  SaveValue: Integer;
  Database: TDatabase;
begin
  asm
    mov    DataSet, eax
    mov    SaveFlag, edx
    mov    SaveValue, ecx
  end;

{$IFDEF _D4_}
  Result := False;
{$ENDIF}

  DataSetClass :=TDBDataSetClass(Self.ClassType);
  Data := FindSetDBFlagData(DataSetClass);
  Database := FindDataSetDatabase(DataSet);
  LockDatabase(Database);
  try
    asm
      mov    edx, SaveFlag
      mov    ecx, SaveValue
      push   ebx
      mov    ebx, Data
      mov    ebx, [ebx + 4]
      mov    eax, DataSet
      call   ebx
      pop    ebx
    end;
  finally
    UnlockDatabase(Database);
  end;
end;

{$IFNDEF _D4_}
procedure TSetDBFlagLocker.OpenCursor{$IFDEF _D3_}(InfoQuery: Boolean){$ENDIF};
var
  DataSet: TDBDataSet;
  DataSetClass: TDBDataSetClass;
  Data: PSetDBFlagData;
{$IFDEF _D3_}
  SaveInfoQuery: Integer;
{$ENDIF}
begin
  asm
    mov    DataSet, eax
{$IFDEF _D3_}
    mov    SaveInfoQuery, edx
{$ENDIF}
  end;

  DataSetClass :=TDBDataSetClass(Self.ClassType);
  Data := FindSetDBFlagData(DataSetClass);

  TBDEDataSetHack(DataSet).FAsyncCallback := TBDECallback(TObject.Create);

  asm
{$IFDEF _D3_}
    mov    edx, SaveInfoQuery
{$ENDIF}
    push   ebx
    mov    ebx, Data
    mov    ebx, [ebx + 8]
    mov    eax, DataSet
    call   ebx
    pop    ebx
  end;
end;
{$ENDIF}

{$IFDEF _D4_}
procedure TSetDBFlagLocker.GetProviderAttributes(List: TList);
var
  Data: PSetDBFlagData;
  DataSet: TDBDataSet;
  DataSetClass: TDBDataSetClass;
  SaveList: Integer;
begin
  asm
    mov    DataSet, eax
    mov    SaveList, edx
  end;

  DataSetClass :=TDBDataSetClass(Self.ClassType);
  Data := FindSetDBFlagData(DataSetClass);

  BDELock;
  try
    asm
      push   ebx
      mov    ebx, Data
      mov    ebx, [ebx + 12]
      mov    eax, DataSet
      mov                edx, SaveList
      call   ebx
      pop    ebx
    end;
  finally
    BDEUnlock;
  end;
end;
{$ENDIF}

{$IFDEF _D5_}
type
  TDataSetHack = class(TDataSet);
{$ENDIF}

procedure InitLockDBDataSet(DataSetClass: TDBDataSetClass);
var
  I, J: Integer;
  Addr: Pointer;
  Data: PSetDBFlagData;
begin
  if FindSetDBFlagData(DataSetClass) <> nil then Exit;
  if not Assigned(SetDBFlags) then SetDBFlags := TList.Create;
  GetMem(Data, SizeOf(TSetDBFlagData));
  try
    Data^.DataSetClass := DataSetClass;

    I := FindVirtualMethodIndex(TDBDataSetHack, @TDBDataSetHack.SetDBFlag);
    J := FindVirtualMethodIndex(TSetDBFlagLocker, @TSetDBFlagLocker.SetDBFlag);
    Addr := GetVirtualMethodAddress(TSetDBFlagLocker, J);
    Data^.ProcSetDBFlag := GetVirtualMethodAddress(DataSetClass, I);
    SetVirtualMethodAddress(DataSetClass, I, Addr);

 {$IFNDEF _D4_}
    I := FindVirtualMethodIndex(TDBDataSetHack, @TDBDataSetHack.OpenCursor);
    J := FindVirtualMethodIndex(TSetDBFlagLocker, @TSetDBFlagLocker.OpenCursor);
    Addr := GetVirtualMethodAddress(TSetDBFlagLocker, J);
    Data^.ProcOpenCursor := GetVirtualMethodAddress(DataSetClass, I);
    SetVirtualMethodAddress(DataSetClass, I, Addr);
{$ENDIF}

{$IFDEF _D4_}
  {$IFDEF _D5_}
    I := FindVirtualMethodIndex(TDBDataSetHack, @TDataSetHack.PSGetAttributes);
  {$ELSE}
    I := FindVirtualMethodIndex(TDBDataSetHack, @TDBDataSetHack.GetProviderAttributes);
  {$ENDIF}
    J := FindVirtualMethodIndex(TSetDBFlagLocker, @TSetDBFlagLocker.GetProviderAttributes);
    Addr := GetVirtualMethodAddress(TSetDBFlagLocker, J);
    Data^.ProcGetProviderAttributes := GetVirtualMethodAddress(DataSetClass, I);
    SetVirtualMethodAddress(DataSetClass, I, Addr);
{$ENDIF}

    SetDBFlags.Add(Data);
  except
    FreeMem(Data);
    raise;
  end;
end;

procedure DoneLockDBDataSet(DataSetClass: TDBDataSetClass);
var
  J: Integer;
  P: PSetDBFlagData;
begin
  P := FindSetDBFlagData(DataSetClass);
  if not Assigned(P) then Exit;

  J := FindVirtualMethodIndex(TDBDataSetHack, @TDBDataSetHack.SetDBFlag);
  SetVirtualMethodAddress(P^.DataSetClass, J, P^.ProcSetDBFlag);

{$IFNDEF _D4_}
  J := FindVirtualMethodIndex(TDBDataSetHack, @TDBDataSetHack.OpenCursor);
  SetVirtualMethodAddress(P^.DataSetClass, J, P^.ProcOpenCursor);
{$ENDIF}

{$IFDEF _D4_}
  {$IFDEF _D5_}
  J := FindVirtualMethodIndex(TDBDataSetHack, @TDataSetHack.PSGetAttributes);
  {$ELSE}
  J := FindVirtualMethodIndex(TDBDataSetHack, @TDBDataSetHack.GetProviderAttributes);
  {$ENDIF}
  SetVirtualMethodAddress(P^.DataSetClass, J, P^.ProcGetProviderAttributes);
{$ENDIF}

  SetDBFlags.Remove(P);
  if SetDBFlags.Count = 0 then
  begin
    SetDBFlags.Free;
    SetDBFlags := nil;
  end;
  FreeMem(P);
end;

procedure InitLockDBDataSets(DataSetClasses: array of TDBDataSetClass);
var
  I: Integer;
begin
  for I := Low(DataSetClasses) to High(DataSetClasses) do
    InitLockDBDataSet(DataSetClasses[I]);
end;

procedure DoneLockDBDataSets(DataSetClasses: array of TDBDataSetClass);
var
  I: Integer;
begin
  for I := Low(DataSetClasses) to High(DataSetClasses) do
    DoneFieldAutoSizer(DataSetClasses[I]);
end;

function TransActive(Database: TDatabase): Boolean;
{ Copied from RX library }
var
  Info: XInfo;
  S: hDBISes;
begin
  Result := False;
  if DbiGetCurrSession(S) <> DBIERR_NONE then Exit;
  Result := (Database.Handle <> nil) and
    (DbiGetTranInfo(Database.Handle, nil, @Info) = DBIERR_NONE) and
    (Info.exState = xsActive);
  DbiSetCurrSession(S);
end;

procedure DefaultStartTransaction(Database: TDatabase);
begin
  if not TransActive(Database) then
  begin
    if Database is TvgDatabase then
      TvgDatabase(Database).StartTransaction else
      Database.StartTransaction;
  end;
end;

procedure RestartTransaction(Database: TDatabase; Restart: Boolean);
begin
  if Database is TvgDatabase then Restart := TvgDatabase(Database).TransRestart;
  if Restart then StartTransaction(Database);
end;

procedure DefaultCommitEx(Database: TDatabase; Restart: Boolean);
begin
  if TransActive(Database) then
  begin
    if Database is TvgDatabase then
      TvgDatabase(Database).Commit else
      Database.Commit;
  end;
  RestartTransaction(Database, Restart);
end;

procedure DefaultRollbackEx(Database: TDatabase; Restart: Boolean);
begin
  FetchQueries(Database);
  if TransActive(Database) then
  begin
    if Database is TvgDatabase then
      TvgDatabase(Database).Rollback else
      Database.Rollback;
  end;
  RestartTransaction(Database, Restart);
end;

procedure DefaultCommit(Database: TDatabase);
begin
  CommitEx(Database, TransRestart);
end;

procedure DefaultRollback(Database: TDatabase);
begin
  RollbackEx(Database, TransRestart);
end;

procedure FetchQueries(Database: TDatabase);
var
  I: Integer;
  DataSet: TDataSet;
begin
  for I := 0 to Database.DataSetCount - 1 do
  begin
    DataSet := Database.DataSets[I];
    if (DataSet is TQuery) and (DataSet.State = dsBrowse) then
      FetchDataSet(DataSet);
  end;
end;

procedure DefaultBDEFetchDataSet(DataSet: TDataSet);
var
  DS: TDataSource;
  DL: TDataLink;
begin
  if (DataSet.State <> dsBrowse) then Exit;
  if (DataSet is TBDEDataSet) then
  begin
    TBDEDataSet(DataSet).FetchAll;
    { There is some bug in DB.PAS, so...}
    DS := TDataSource.Create(nil);
    try
      DL := TDataLink.Create;
      try
        DL.DataSource := DS;
        DS.DataSet := DataSet;
        DL.BufferCount := 2;
      finally
        DL.Free;
      end;
    finally
      DS.Free;
    end
  end else
    DefaultFetchDataSet(DataSet);
end;

function DefaultDataSetPost(ADataSet: TDataSet; Apply: Boolean): Boolean;
var
  DataSet: TDBDataSet absolute ADataSet;
  Field: TField;
  ID: Variant;
  OldBeforePost: TDataSetNotifyEvent;
begin
  with DataSet do
  begin
    Result := True;
    if not ((State in dsEditModes) or CachedUpdates and UpdatesPending) then Exit;
    if Apply then with DataSet.Database do
    begin
      AppSetCursor(crSQLWait);
      try
        DisableControls;
        try
          vgBDEUtl.StartTransaction(DataSet.Database);
          try
            OldBeforePost := DataSet.BeforePost;
            try
              DataSet.BeforePost := nil;
              if Assigned(OldBeforePost) and (State in dsEditModes) then
                OldBeforePost(DataSet);
              Field := FindFieldID(DataSet);
              if Assigned(Field) then ID := Field.Value;
              if (State in dsEditModes) then DataSet.Post;
            finally
              DataSet.BeforePost := OldBeforePost;
            end;
            if CachedUpdates then
            try
              DataSet.ApplyUpdates;
            except
              SysUtils.Abort;
            end;
            vgBDEUtl.Commit(DataSet.Database);
            if CachedUpdates then DataSet.CommitUpdates;
            if Assigned(Field) then Locate(Field.FieldName, ID, []);
          except
            vgBDEUtl.Rollback(DataSet.Database);
            if DataSet.State = dsBrowse then DataSet.Edit;
            raise;
          end;
        finally
          EnableControls;
        end;
      finally
        AppRestoreCursor;
      end;
    end else begin
      DataSet.Cancel;
      Result := True;
    end;
  end;
end;

function DefaultDataSetDelete(ADataSet: TDataSet; Confirm: Boolean): Boolean;
var
  DataSet: TDBDataSet absolute ADataSet;
begin
  Result := False;
  if not Confirm or ConfirmDelete then
  with DataSet do
  begin
    AppSetCursor(crSQLWait);
    try
      DisableControls;
      try
        StartTransaction(Database);
        try
          Delete;
          if CachedUpdates then
          try
            DataSet.ApplyUpdates;
          except
            SysUtils.Abort;
          end;
          Commit(Database);
          if CachedUpdates then DataSet.CommitUpdates;
          Result := True;
        except
          Rollback(Database);
          raise;
        end;
      finally
        EnableControls;
      end;
    finally
      AppRestoreCursor;
    end;
  end;
end;

function DataSetExecSQL(DataSet: TDBDataSet): Integer;
var
  DB: TDatabase;
begin
  with (DataSet as TQuery) do
  begin
    AppSetCursor(crSQLWait);
    try
      DB := FindDatabase(SessionName, DatabaseName);
      Result := 0;
      vgBDEUtl.StartTransaction(DB);
      try
        Prepare;
        ExecSQL;
        Result := RowsAffected;
        vgBDEUtl.Commit(DB);
      except
        vgBDEUtl.Rollback(DB);
        raise;
      end;
    finally
      AppRestoreCursor;
    end;
  end;
end;

procedure DataSetExecProc(DataSet: TDBDataSet);
var
  DB: TDatabase;
begin
  with (DataSet as TStoredProc) do
  begin
    AppSetCursor(crSQLWait);
    try
      DB := FindDatabase(SessionName, DatabaseName);
      Prepare;
      vgBDEUtl.StartTransaction(DB);
      try
        ExecProc;
        vgBDEUtl.Commit(DB);
      except
        vgBDEUtl.Rollback(DB);
        raise;
      end;
    finally
      AppRestoreCursor;
    end;
  end;
end;

procedure GetAliasNames(const SessionName: string; ConfigMode: TConfigMode; List: TStrings);
var
  Session: TSession;
  CfgMode: TConfigMode;
begin
  Session := Sessions.FindSession(SessionName);
  if not Assigned(Session) then Exit;
  CfgMode := Session.ConfigMode;
  Session.ConfigMode := ConfigMode;
  try
   Session.GetAliasNames(List);
  finally
    Session.ConfigMode := CfgMode;
  end;
end;

function FindDatabase(const ASessionName, ADatabaseName: String): TDatabase;
var
  ASession: TSession;
begin
  ASession := Sessions.FindSession(ASessionName);
  if not Assigned(ASession) then ASession := Session;
  Result := ASession.FindDatabase(ADatabaseName);
end;

function FindDataSetDatabase(DataSet: TDBDataSet): TDatabase;
begin
  Result := DataSet.Database;
  if not Assigned(Result) then
    Result := FindDatabase(DataSet.SessionName, DataSet.DatabaseName);
end;

function CreateQuery(ADatabase: TDatabase; ASQL: String;
  ParamName: String; ParamValues: Variant): TQuery;
const
  QueryID: Integer = 0;
begin
  Result := QueryClass.Create(nil);
  with Result do
  try
    Name := Format('quQuery%d', [QueryID]);
    Inc(QueryID);
    if Assigned(ADatabase) then
    begin
      if ADatabase.SessionName <> Session.SessionName then
        SessionName := ADatabase.SessionName;
      DatabaseName := ADatabase.DatabaseName;
    end;
    SQL.Text := ASQL;
    SetParams(Params, ParamName, ParamValues);
  except
    Free;
    raise;
  end;
end;

function SQLWithParam(ADatabase: TDatabase; ASQL: String;
  FieldName, ParamName: String; ParamValues: Variant; Execute: Boolean): Variant;
var
  Query: TQuery;
begin
  Query := CreateQuery(ADatabase, ASQL, ParamName, ParamValues);
  Result := Null;
  with Query do
  try
    if Execute then
    begin
      ExecSQL;
      Result := RowsAffected;
    end else begin
      Open;
      if (FieldName <> '') then
        Result := FieldValues[FieldName] else
        Result := Fields[0].Value;
    end;
  finally
    Free;
  end;
end;

function SQLWithParamA(Query: TQuery; FieldName, ParamName: String;
  ParamValues: Variant; Execute: Boolean): Variant;
begin
  Result := Null;
  with Query do
  try
    SetParams(Params, ParamName, ParamValues);

    Prepare;
    if Execute then
    begin
      ExecSQL;
      Result := RowsAffected;
    end else begin
      Open;
      if (FieldName <> '') then
        Result := FieldValues[FieldName] else
        Result := Fields[0].Value;
    end;
  finally
    Close;
  end;
end;

function ExecWithParam(ADatabase: TDatabase; ASQL: String;
  ParamName: String; ParamValues: Variant): Integer;
begin
  Result := SQLWithParam(ADatabase, ASQL, '', ParamName, ParamValues, True);
end;

function ExecWithParamA(Query: TQuery;
  ParamName: String; ParamValues: Variant): Integer;
begin
  Result := SQLWithParamA(Query, '', ParamName, ParamValues, True);
end;

function ExecQuery(ADatabase: TDatabase; ASQL: String): Integer;
begin
  Result := ExecWithParam(ADatabase, ASQL, '', Unassigned);
end;

function ExecQueryA(Query: TQuery): Integer;
begin
  Result := ExecWithParamA(Query, '', Unassigned);
end;

function QueryWithParam(ADatabase: TDatabase; ASQL: String;
  FieldName, ParamName: String; ParamValues: Variant): Variant;
begin
  Result := SQLWithParam(ADatabase, ASQL, FieldName, ParamName, ParamValues, False);
end;

function QueryWithParamA(Query: TQuery;
  FieldName, ParamName: String; ParamValues: Variant): Variant;
begin
  Result := SQLWithParamA(Query, FieldName, ParamName, ParamValues, False);
end;

function QueryValue(ADatabase: TDatabase; ASQL: String;
  FieldName: String): Variant;
begin
  Result := QueryWithParam(ADatabase, ASQL, FieldName, '', Null);
end;

function QueryValueA(Query: TQuery; FieldName: String): Variant;
begin
  Result := QueryWithParamA(Query, FieldName, '', Null);
end;

procedure ChangeToDatabase(DataSet: TDBDataSet; ADatabase: TDatabase);
begin
  DataSet.Close;
  DataSet.SessionName := ADatabase.SessionName;
  DataSet.DatabaseName := ADatabase.DatabaseName;
end;

function CloneToDatabaseOwner(Source: TDBDataSet; AOwner: TComponent; ADatabase: TDatabase): TDBDataSet;
begin
  Result := TDBDataSet(CreateCloneOwner(Source, AOwner));
  try
    ChangeToDatabase(Result, ADatabase);
  except
    Result.Free;
    raise;
  end;
end;

function CloneToDatabase(Source: TQuery; ADatabase: TDatabase): TDBDataSet;
begin
  Result := CloneToDatabaseOwner(Source, Source.Owner, ADatabase);
end;

function BuildUpdateSQLEx(DataSet: TDataSet; TableName: String; ExcludeFields, KeyFieldNames: String; UpdateSQL: TUpdateSQL): TDataSetUpdateObject;
var
  I: TUpdateKind;
  Auto: Boolean;
begin
  if not Assigned(UpdateSQL) then
  begin
    Result := TUpdateSQL.Create(DataSet);
    Auto := True;
  end else begin
    Auto := False;
    Result := UpdateSQL;
  end;
  try
    for I := Low(TUpdateKind) to High(TUpdateKind) do
      TUpdateSQL(Result).SQL[I].Text := GetDataSetUpdateSQLEx(DataSet, TableName, ExcludeFields, KeyFieldNames, I);
    if Auto then TBDEDataSet(DataSet).UpdateObject := Result;
  except
    if Auto then Result.Free;
    raise;
  end;
end;

function BuildUpdateSQL(DataSet: TDataSet; TableName: String; KeyFieldNames: String; UpdateSQL: TUpdateSQL): TDataSetUpdateObject;
var
  I: TUpdateKind;
  Auto: Boolean;
begin
  if not Assigned(UpdateSQL) then
  begin
    Result := TUpdateSQL.Create(DataSet);
    Auto := True;
  end else begin
    Auto := False;
    Result := UpdateSQL;
  end;
  try
    for I := Low(TUpdateKind) to High(TUpdateKind) do
      TUpdateSQL(Result).SQL[I].Text := GetDataSetUpdateSQL(DataSet, TableName, KeyFieldNames, I);
    if Auto then TBDEDataSet(DataSet).UpdateObject := Result;
  except
    if Auto then Result.Free;
    raise;
  end;
end;

procedure SetUpdateQueryParams(Query: TQuery; DataSet: TDataSet);
begin
  SetUpdateParams(Query.Params, DataSet);
end;

procedure SetUpdateSQLParams(UpdateSQL: TUpdateSQL; DataSet: TDataSet);
var
  I: TUpdateKind;
begin
  for I := Low(TUpdateKind) to High(TUpdateKind) do
    SetUpdateQueryParams(UpdateSQL.Query[I], DataSet);
end;

function DefaultEditKeyFieldsCU(DataSet, CUDataSet: TDataSet; KeyFields: String;
  FormClass: TFormClass; InsertCaption, EditCaption: String; IsNewRecord: Boolean): Boolean;
begin
  with TQuery(CUDataSet) do
  try
    if not Active then
    begin
      AssignParams(Params, DataSet);
      Prepare;
      Open;
    end;
    if not IsNewRecord and EOF then RecordNotFound;
    Result := DataSetEdit(CUDataSet, FormClass, InsertCaption, EditCaption, IsNewRecord);
    with DataSet do
    begin
      DisableControls;
      try
        ReopenDataSetEx(DataSet, KeyFields, CUDataSet.FieldValues[KeyFields]);
      finally
        EnableControls;
      end;
    end;
  finally
    Close;
  end;
end;

function DefaultEditCU(DataSet, CUDataSet: TDataSet;
  FormClass: TFormClass; InsertCaption, EditCaption: String; IsNewRecord: Boolean): Boolean;
begin
  Result := DefaultEditKeyFieldsCU(DataSet, CUDataSet, DefaultKeyFields,
    FormClass, InsertCaption, EditCaption, IsNewRecord);
end;

function DefaultDeleteKeyFieldsCU(DataSet, CUDataSet: TDataSet; KeyFields: String; Confirm: Boolean): Boolean;
begin
  with TQuery(CUDataSet) do
  try
    if not Active then
    begin
      Close;
      AssignParams(Params, DataSet);
      Prepare;
      Open;
    end;
    CheckNotEmpty(CUDataSet);
    Result := DataSetDelete(TDBDataSet(CUDataSet), Confirm);
    if Result then
    with DataSet do
    begin
      DisableControls;
      try
        if DataSet.EOF then
        begin
          DataSet.Prior;
        end else begin
          DataSet.Next;
          if DataSet.EOF then DataSet.Prior;
        end;
        ReopenDataSet(DataSet, KeyFields);
      finally
        EnableControls;
      end;
    end;
  finally
    Close;
  end;
end;

function DefaultDeleteCU(DataSet, CUDataSet: TDataSet; Confirm: Boolean): Boolean;
begin
  Result := DefaultDeleteKeyFieldsCU(DataSet, CUDataSet, DefaultKeyFields, Confirm);
end;

type
  TUpdateObjectHack = class(TDataSetUpdateObject)
  end;

procedure DefaultUpdateRecord(DataSet: TDataSet; UpdateKind: TUpdateKind;
  var UpdateAction: TUpdateAction);
begin
  with DataSet as TBDEDataSet do
    TUpdateObjectHack(UpdateObject).Apply(UpdateKind);
  UpdateAction := uaApplied;
end;

procedure DefaultUpdateError(DataSet: TDataSet; E: EDatabaseError; UpdateKind: TUpdateKind;
  var UpdateAction: TUpdateAction);
begin
  Application.HandleException(E);
  UpdateAction := uaFail;
end;

procedure HandleNewRecord(GenIDProc: TStoredProc; FieldID: TField);
begin
  with FieldID do
  if (DataSet.State = dsInsert) and IsNull then
  begin
    GenIDProc.ExecProc;
    Value := GenIDProc.Params[0].Value;
  end;
end;

procedure DBExceptionTranslate(E: EDBEngineError);
  function OriginalMessage: String;
  var
    I: Integer;
    DBErr: TDBError;
    S: String;
  begin
    Result := '';
    for I := 0 to E.ErrorCount - 1 do
    begin
      DBErr := E.Errors[I];
      case DBErr.NativeError of
        -836: { Intebase exception }
          begin
            S := DBErr.Message;
            Result := #13#10 + Copy(S, Pos(#10, S) + 1, Length(S));
            Exit;
          end;
      end;
      S := Trim(DBErr.Message);
      if S <> '' then Result := Result + #13#10 + S;
    end;
  end;
begin
  case E.Errors[0].ErrorCode of
    DBIERR_KEYORRECDELETED:
      E.Message := LoadStr(SKeyDeleted);
    DBIERR_UNKNOWNDB, DBIERR_INVALIDUSRPASS:
      E.Message := LoadStr(SInvalidUserName);
    DBIERR_DEADLOCK:
      E.Message := LoadStr(SDeadlock);
    DBIERR_KEYVIOL:
      E.Message := LoadStr(SKeyViol);
    DBIERR_FORIEGNKEYERR:
      E.Message := LoadStr(SFKViolation) + OriginalMessage;
    else begin
      E.Message := Format(LoadStr(SErrorCodeFmt), [E.Errors[0].ErrorCode]) + OriginalMessage;
    end;
  end;
end;

procedure WriteBlob(Field: TField);
var
  BlobStream: TBlobStream;
  Buffer: Pointer;
begin
  if not (Field is TBlobField) then Exit;

  BlobStream := TBlobStream.Create(TBlobField(Field), bmWrite);
  try
    Buffer := nil;
    try
      ReallocMem(Buffer, Field.DataSize);
      Field.GetData(Buffer);
      BlobStream.Write(Buffer, Field.DataSize);
    finally
      ReallocMem(Buffer, 0);
    end;
  finally
    BlobStream.Free;
  end;
end;

procedure WriteBlobs(DataSet: TDataSet);
var
  I: Integer;
  Field: TField;
begin
  for I := 0 to DataSet.FieldCount - 1 do
  begin
    Field := DataSet.Fields[I];
    if Field is TBlobField then WriteBlob(Field);
  end;
end;

function DBSet(DataSource: TDataSource): TDBDataSet;
begin
  Result := TDBDataSet(DataSource.DataSet);
end;

procedure SetTableRange(Table: TTable; const AIndexFields: String; IndexValues: Variant);
begin
  with Table do
  begin
    IndexFieldNames := AIndexFields;
    EditRangeStart;
    FieldValues[AIndexFields] := IndexValues;
    EditRangeEnd;
    FieldValues[AIndexFields] := IndexValues;
    ApplyRange;
  end;
end;

procedure DefaultReopenTable(DataSet: TDataSet; FieldsPK: String; ValuesPK: Variant);
begin
  TTable(DataSet).Refresh;
end;

procedure BatchMove(Source: TDataSet; Dest: TDBDataSet; Mode: TBatchMode; AbortOnKeyViol, AbortOnProblem: Boolean;
  const ChangedTableName, KeyViolTableName, ProblemTableName: string; Records, CommitCount: Integer;
  var ChangedCount, KeyViolCount, ProblemCount, MovedCount: Integer);

  procedure CreateTable(const TableName: string; var Table: TTable; var Fields: TList);
  begin
    if TableName = '' then
    begin
      Table := nil;
      Fields := nil;
      Exit;
    end;

    Table := TTable.Create(nil);
    try
      Table.FieldDefs := Source.FieldDefs;
      Table.TableName := TableName;
      Table.SessionName := Dest.SessionName;
      Table.DatabaseName := Dest.DatabaseName;
      Table.CreateTable;
      Table.Open;
      Fields := TList.Create;
      try
        GetFieldList(Table, Fields, '', True);
      except
        Fields.Free;
        Fields := nil;
        raise;
      end;
    except
      Table.Free;
      Table := nil;
      raise;
    end;
  end;

var
  Done, TrimAnalized, TrimExpected: Boolean;
  Changed, KeyViol, Problem: TTable;
  DestFields, SourceFields, SourceKeyFields: TList;
  ChangedFields, KeyViolFields, ProblemFields: TList;
  PostCount: Integer;
  KeyFieldNames: string;

  function FindRecord: Boolean;
  begin
    if SourceKeyFields.Count > 1 then
      Result := Dest.Locate(KeyFieldNames, GetFieldListValues(SourceKeyFields), [])
    else if SourceKeyFields.Count = 1 then
      with TField(SourceKeyFields[0]) do
        Result := Dest.Locate(FieldName,Value, [])
    else
      Result := False;
  end;

  procedure ErrorRecord(DataSet: TDataSet; DstFields: TList);
  begin
    if not Assigned(DataSet) then Exit;

    DataSet.Insert;
    try
      AssignFields(DstFields, SourceFields);
      DataSet.Post;
    except
      DataSet.Cancel;
      raise;
    end;
  end;

  procedure ChangeRecord;
  begin
    ErrorRecord(Changed, ChangedFields);
    Inc(ChangedCount);
  end;

  procedure CheckProblem;
  var
    I: Integer;
    Field1, Field2: TField;
  begin
    if not TrimAnalized then
    begin
      for I := 0 to SourceFields.Count - 1 do
      begin
        Field1 := SourceFields[I];
        Field2 := DestFields[I];
        if (Field2.DataSize > 0) and (Field2.DataSize < Field1.DataSize) then
        begin
          TrimExpected := True;
          Break;
        end;
      end;
      TrimAnalized := True;
    end;

    if TrimExpected then
    begin
      if not AbortOnProblem then
        ErrorRecord(Problem, ProblemFields) else
        Done := True;
      Inc(ProblemCount);
    end;
  end;

  procedure InsertRecord;
  begin
    CheckProblem;
    if Done then Exit;
    Dest.Insert;
    try
      AssignFields(DestFields, SourceFields);
      Dest.Post;
      Inc(PostCount);
      CheckProblem;
    except
      Dest.Cancel;
      raise;
    end;
  end;

  procedure UpdateRecord;
  begin
    CheckProblem;
    if Done then Exit;
    Dest.Edit;
    try
      AssignFields(DestFields, SourceFields);
      Dest.Post;
      Inc(PostCount);
      ChangeRecord;
    except
      Dest.Cancel;
      raise;
    end;
  end;

  procedure DeleteRecord;
  begin
    Dest.Delete;
    Inc(PostCount);
    ChangeRecord;
  end;

  procedure ProcessRecord;
  begin
    try
      case Mode of
        batAppend:
          InsertRecord;
        batAppendUpdate:
          if FindRecord then UpdateRecord else InsertRecord;
        batDelete:
          if FindRecord then DeleteRecord;
        batUpdate:
          if FindRecord then UpdateRecord;
      end;
    except
      on E: EDBEngineError do
        case E.Errors[0].ErrorCode of
          DBIERR_KEYVIOL,
          DBIERR_FORIEGNKEYERR,
          DBIERR_DETAILRECORDSEXIST,
          DBIERR_MASTEREXISTS:
            begin
              if not AbortOnKeyViol then
                ErrorRecord(KeyViol, KeyViolFields) else
                Done := True;
              Inc(KeyViolcount);
            end;
        end;
    end;
  end;

  procedure GetKeyFields;
  var
    I: Integer;
    Field: TField;
  begin
    for I := 0 to SourceFields.Count - 1 do
    begin
      Field := SourceFields[I];
      with Field do
      begin
        if (DataType in [ftBytes, ftVarBytes]) or IsBlobField(Field)
           {$IFDEF _D4_}or (Field is TObjectField){$ENDIF} then Continue;
        case TDBDataSetHack(Dest).UpdateMode of
          upWhereKeyOnly:
            if IsIndexField or (CompareText(Field.FieldName, DefaultKeyFields) = 0) then
              SourceKeyFields.Add(Field);
          upWhereAll, upWhereChanged:
            SourceKeyFields.Add(Field);
        end;
      end;
      KeyFieldNames := GetFieldNamesFromList(SourceKeyFields)
    end;
  end;
var
  Database: TDatabase;
  TransactionStarted: Boolean;
  RecordSize: Integer;
begin
  Database := Dest.Database;
  TransactionStarted := not TransActive(Database);
  { Start a transaction if user hasn't already }
  if TransactionStarted then
  begin
    if not Database.IsSQLBased then
      Database.TransIsolation := tiDirtyRead;
    StartTransaction(Database);
  end;
  try
    Changed := nil; ChangedFields := nil;
    DestFields := nil; SourceFields := nil;
    SourceKeyFields := nil;
    KeyViol := nil; KeyViolFields := nil;
    Problem := nil; ProblemFields := nil;
    try
      if Mode = batCopy then
      begin
        if Dest is TTable then
        begin
          TTable(Dest).IndexDefs.Clear;
          TTable(Dest).CreateTable;
        end;
        Mode := batAppend;
      end;

      DestFields := TList.Create; SourceFields := TList.Create;
      SourceKeyFields := TList.Create;
      GetFieldList(Dest, DestFields, '', True);
      GetFieldList(Source, SourceFields, '', True);
      GetKeyFields;

      CreateTable(ChangedTableName, Changed, ChangedFields);
      CreateTable(KeyViolTableName, KeyViol, KeyViolFields);
      CreateTable(ProblemTableName, Problem, ProblemFields);

      if Records <= 0 then
        Records := MaxInt else Source.First;
      MovedCount := 0;
      PostCount := 0;
      RecordSize := Source.RecordSize;
      if not TransactionStarted then
        CommitCount := MaxInt
      else if CommitCount = 0 then
        CommitCount := Max(1, 32 * 1024 div RecordSize);

      Done := False;
      TrimAnalized := False;
      TrimExpected := False;
      while not Source.EOF and (MovedCount < Records) and not Done do
      begin
        ProcessRecord;
        Inc(MovedCount);
        if PostCount = CommitCount then
        begin
          Commit(Database);
          StartTransaction(Database);
          PostCount := 0;
        end;
        Source.Next;
      end;
    finally
      SourceKeyFields.Free;
      DestFields.Free; SourceFields.Free;
      Changed.Free; ChangedFields.Free;
      KeyViol.Free; KeyViolFields.Free;
      Problem.Free; ProblemFields.Free;
    end;
    if TransactionStarted then Commit(Database);
  except
    if TransactionStarted then Rollback(Database);
    raise;
  end;
end;

function BDEVersion(Version: TStrings): SYSVersion;
var
  Month, Day, iHour, iMin, iSec: Word;
  Year: SmallInt;
begin
  Check(DbiGetSysVersion(Result));
  if Version <> nil then
  begin
    with Version do
    begin
      BeginUpdate;
      try
        Clear;
        Add(Format('ENGINE VERSION=%d', [Result.iVersion]));
        Add(Format('INTERFACE LEVEL=%d', [Result.iIntfLevel]));
        Check(DbiDateDecode(Result.dateVer, Month, Day, Year));
        Add(Format('VERSION DATE=%s', [DateToStr(EncodeDate(Year, Month, Day))]));
        Check(DbiTimeDecode(Result.timeVer, iHour, iMin, iSec));
        Add(Format('VERSION TIME=%s', [TimeToStr(EncodeTime(iHour, iMin,
            iSec div 1000, iSec div 100))]));
      finally
        EndUpdate;
      end;
    end;
  end;
end;

procedure KillCursorChangeCallback(var OldCBData: Longint; var OldCBBuf: Pointer;
  var OldCBBufLen: Word; var OldCBFunc: pfDBICallBack);
begin
  Check(DbiGetCallBack(nil, cbSERVERCALL, @OldCBData, @OldCBBufLen, @OldCBBuf, OldCBFunc));
  if Assigned(OldCBFunc) then
    Check(DbiRegisterCallback(nil, cbSERVERCALL, 0, 0, nil, nil));
end;

procedure RestoreCursorChangeCallback(OldCBData: Longint; OldCBBuf: Pointer;
  OldCBBufLen: Word; OldCBFunc: pfDBICallBack);
begin
  if Assigned(OldCBFunc) then
    Check(DbiRegisterCallback(nil, cbSERVERCALL, OldCBData, OldCBBufLen, OldCBBuf, OldCBFunc));
end;

function GetQuoteChar(Database: TDatabase): string;

{$IFNDEF _D3_}
const
  dbQUOTECHAR = $0404000A;
{$ENDIF}

var
  Len: word;
  Q: Char;
begin
  if Database.IsSQLBased then
  begin
    Q := #0;
    DbiGetProp(HDBIObj(Database.Handle), dbQUOTECHAR, @Q, SizeOf(Q), Len);
    if Q = #0 then
      Result := '' else
      Result := Q;
  end else
    Result := '"';
end;

initialization
  InitializeCriticalSection(FBDELock);
  FetchDataSet    := DefaultBDEFetchDataSet;

  RegisterRefreshProc(TTable, @DefaultReopenTable);
  RegisterPostProc(TDBDataSet, @DefaultDataSetPost);
  RegisterDeleteProc(TDBDataSet, @DefaultDataSetDelete);

finalization
  FreeSetDBFlags;
  DeleteCriticalSection(FBDELock);

end.


