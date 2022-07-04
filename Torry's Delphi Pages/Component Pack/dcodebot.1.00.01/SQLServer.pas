
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit SQLServer;

interface

{$I STD.INC}

uses
  ActiveX, ComObj, SQLDMO, Windows, SysUtils, OleTools;

{ TCell }

type
  TQueryResults = class;

  TCellKind = (ckUnknown, ckChar, ckText, ckVarchar, ckVarBinary, ckBinary,
    ckImage, ckSingle, ckDouble, ckByte, ckWord, ckLongWord, ckMoney,
    ckDateTime, ckDate, ckBit, ckUnicodeChar, ckUnicodeVarchar, ckUnicodeText,
    ckGUID);

  TCell = class(TObject)
  private
    FQueryResults: TQueryResults;
    FName: string;
    FKind: TCellKind;
    FSize: Integer;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsGuid: TGUID;
    function GetAsInteger: Integer;
    function GetAsSafeArray: PSafeArray;
    function GetAsString: string;
  protected
    procedure Update;
  public
    constructor Create(QueryResults: TQueryResults);
    property AsBoolean: Boolean read GetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime;
    property AsFloat: Double read GetAsFloat;
    property AsGuid: TGUID read GetAsGuid;
    property AsInteger: Integer read GetAsInteger;
    property AsSafeArray: PSafeArray read GetAsSafeArray;
    property AsString: string read GetAsString;
    property Name: string read FName;
    property Kind: TCellKind read FKind;
    property Size: Integer read FSize;
  end;

{ TQueryResults }

  TQueryResults = class(TObject)
  private
    FCell: TCell;
    FCol: Integer;
    FIndex: Integer;
    FQueryResults: IQueryResults;
    FRow: Integer;
    function GetCell(Col, Row: Integer): TCell;
    function GetColCount: Integer;
    procedure SetIndex(Value: Integer);
    function GetCount: Integer;
    function GetRowCount: Integer;
  public
    constructor Create(QueryResults: IQueryResults);
    destructor Destroy; override;
    property Cell[Col, Row: Integer]: TCell read GetCell; default;
    property ColCount: Integer read GetColCount;
    property Count: Integer read GetCount;
    property Index: Integer read FIndex write SetIndex;
    property QueryResults: IQueryResults read FQueryResults;
    property RowCount: Integer read GetRowCount;
  end;

{ TSQLServer }

  TServerStatusElement = (ssNormal, ssLoading, ssRecovering, ssSuspect,
     ssOffline, ssInaccessible, ssEmergency, ssStandby);
  TServerStatus = set of TServerStatusElement;

  TSQLServer = class(TObject)
  private
    FDatabase: IDatabase;
    FDatabaseName: string;
    FPassword: string;
    FQueryResults: TQueryResults;
    FScript: string;
    FSecure: Boolean;
    FServer: ISQLServer;
    FServerName: string;
    FTransactionName: string;
    FTransactionRef: Integer;
    FUserName: string;
    function GetConnected: Boolean;
    function GetStatus: TServerStatus;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginTransaction;
    procedure Clear;
    procedure Connect;
    procedure Disconnect;
    procedure EndTransaction;
    procedure EnumAttributes;
    procedure EnumProcesses;
    procedure Execute;
    procedure ExecuteWithResults;
    property Secure: Boolean read FSecure write FSecure;
    procedure Reconnect;
    procedure RollbackTransaction;
    property Connected: Boolean read GetConnected;
    property Database: IDatabase read FDatabase;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property Password: string read FPassword write FPassword;
    property QueryResults: TQueryResults read FQueryResults;
    property Server: ISQLServer read FServer;
    property ServerName: string read FServerName write FServerName;
    property Script: string read FScript write FScript;
    property Status: TServerStatus read GetStatus;
    property UserName: string read FUserName write FUserName;
  end;

implementation

{ TCell }

constructor TCell.Create(QueryResults: TQueryResults);
begin
  inherited Create;
  FQueryResults := QueryResults;
end;

procedure TCell.Update;
const
  DataTypes: array[TCellKind] of Cardinal =(DMO_DTypeUnknown,
    DMO_DTypeChar, DMO_DTypeText, DMO_DTypeVarchar, DMO_DTypeVarBinary,
    DMO_DTypeBinary, DMO_DTypeImage, DMO_DTypeFloat4, DMO_DTypeFloat8,
    DMO_DTypeInt1, DMO_DTypeInt2, DMO_DTypeInt4, DMO_DTypeMoney,
    DMO_DTypeDateTime, DMO_DTypeDateTime4, DMO_DTypeBit, DMO_DTypeUChar,
    DMO_DTypeUVarchar, DMO_DTypeNText, DMO_DTypeGUID);
var
  DataType: LongWord;
  I: TCellKind;
begin
  with FQueryResults do
  begin
    FName := QueryResults.ColumnName[FCol];
    DataType := QueryResults.ColumnType[FCol];
    for I := Low(TCellKind) to High(TCellKind) do
      if DataType = DataTypes[I] then
      begin
        FKind := I;
        Break;
      end;
    case FKind of
      ckChar: FSize := SizeOf(Char);
      ckSingle: FSize := SizeOf(Single);
      ckDouble: FSize := SizeOf(Double);
      ckByte: FSize := SizeOf(Byte);
      ckWord: FSize := SizeOf(Word);
      ckLongWord: FSize := SizeOf(LongWord);
      ckMoney: FSize := SizeOf(Double);
      ckDateTime: FSize := SizeOf(TDateTime);
      ckDate: FSize := SizeOf(Integer);
      ckBit: FSize := SizeOf(Byte);
      ckUnicodeChar: FSize := SizeOf(WideChar);
      ckGUID: FSize := SizeOf(TGUID);
    else
      FSize := QueryResults.GetColumnBinaryLength(FRow, FCol);
    end;
  end;
end;

function TCell.GetAsBoolean: Boolean;
begin
  with FQueryResults do
    Result := UpperCase(AsString) = 'Y';
end;

function TCell.GetAsDateTime: TDateTime;
begin
  with FQueryResults do
    Result := QueryResults.GetColumnDate(FRow, FCol);
end;

function TCell.GetAsFloat: Double;
begin
  with FQueryResults do
    Result := QueryResults.GetColumnDouble(FRow, FCol);
end;

function TCell.GetAsGuid: TGUID;
var
  SafeArray: PSafeArray;
  Guid: PGUID;
begin
  FillChar(Result, SizeOf(TGUID), #0);
  with FQueryResults do
    SafeArray := QueryResults.GetColumnGuid(FRow, FCol);
  if SafeArray <> nil then
  begin
    OleCheck(SafeArrayAccessData(SafeArray, Pointer(Guid)));
    try
      Result := Guid^;
    finally
      OleCheck(SafeArrayUnaccessData(SafeArray));
    end;
  end;
end;

function TCell.GetAsInteger: Integer;
begin
  with FQueryResults do
    Result := QueryResults.GetColumnLong(FRow, FCol);
end;

function TCell.GetAsSafeArray: PSafeArray;
begin
  with FQueryResults do
    Result := QueryResults.GetColumnBinary(FRow, FCol);
end;

function TCell.GetAsString: string;
begin
  with FQueryResults do
    Result := QueryResults.GetColumnString(FRow, FCol);
end;

{ TQueryResults }

constructor TQueryResults.Create(QueryResults: IQueryResults);
begin
  inherited Create;
  FQueryResults := QueryResults;
  FCell := TCell.Create(Self);
end;

destructor TQueryResults.Destroy;
begin
  FCell.Free;
  inherited Destroy;
end;

function TQueryResults.GetCell(Col, Row: Integer): TCell;
var
  CanUpdate: Boolean;
begin
  CanUpdate := (FCol <> Col + 1) or (FRow <> Row + 1);
  FCol := Col + 1;
  FRow := Row + 1;
  if CanUpdate then
    FCell.Update;
  Result := FCell;
end;

function TQueryResults.GetColCount: Integer;
begin
  Result := FQueryResults.Columns;
end;

procedure TQueryResults.SetIndex(Value: Integer);
begin
  FQueryResults.CurrentResultSet := Value + 1;
  FIndex := Value;
  FCol := -1;
  FRow := -1;
end;

function TQueryResults.GetCount: Integer;
begin
  Result := FQueryResults.ResultSets;
end;

function TQueryResults.GetRowCount: Integer;
begin
  Result := FQueryResults.Rows;
end;

{ TSQLServer }

constructor TSQLServer.Create;
var
  GUID: TGUID;
begin
  OleCheck(CoCreateGuid(GUID));
  FTransactionName := GuidToStr(GUID);
  FServer := CreateCOMObject(CLASS_SQLServer) as ISQLServer;
  inherited Create;
end;

destructor TSQLServer.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TSQLServer.BeginTransaction;
begin
  if Connected then
  begin
    if FTransactionRef = 0 then
      FServer.BeginTransaction(FTransactionName);
    Inc(FTransactionRef);
  end;
end;

procedure TSQLServer.EndTransaction;
begin
  if Connected then
  begin
    Dec(FTransactionRef);
    if FTransactionRef = 0 then
      FServer.CommitTransaction(FTransactionName);
  end;
end;

procedure TSQLServer.Execute;
begin
  FQueryResults.Free;
  FQueryResults := nil;
  if Connected then
    FDatabase.ExecuteImmediate(FScript, DMOExec_Default, Unassigned);
end;

procedure TSQLServer.ExecuteWithResults;
begin
  FQueryResults.Free;
  FQueryResults := nil;
  if Connected then
    FQueryResults := TQueryResults.Create(FDatabase.ExecuteWithResults(FScript, ''));
end;

procedure TSQLServer.Clear;
begin
  FQueryResults.Free;
  FQueryResults := nil;
  FScript := '';
end;

procedure TSQLServer.Connect;
var
  Logins: OleVariant;
begin
  Disconnect;
  FServer.LoginSecure := FSecure;
  FServer.Connect(FServerName, FUserName, FPassword);
  if FDatabaseName = '' then
  begin
    Logins := FServer.Logins;
    FDatabaseName := Logins.Item(FServer.Login).Database;
  end;
  FDatabase := FServer.Databases.Item(FDatabaseName, '');
end;

procedure TSQLServer.Disconnect;
begin
  FQueryResults.Free;
  FQueryResults := nil;
  FDatabase := nil;
  if FServer.VerifyConnection(DMOConn_CurrentState) then
    FServer.DisConnect;
end;

procedure TSQLServer.Reconnect;
begin
  if not Connected then
  begin
    FServer.Reconnect;
    FDatabase := FServer.Databases.Item(FDatabaseName, '');
  end;
end;

procedure TSQLServer.RollbackTransaction;
begin
  if Connected and (FTransactionRef > 0) then
  begin
    FServer.RollbackTransaction(FTransactionName);
    FTransactionRef := 0;
  end;
end;

procedure TSQLServer.EnumAttributes;
begin
  FQueryResults.Free;
  FQueryResults := nil;
  FScript := '';
  if Connected then
    FQueryResults := TQueryResults.Create(FServer.EnumServerAttributes);
end;

procedure TSQLServer.EnumProcesses;
begin
  FQueryResults.Free;
  FQueryResults := nil;
  FScript := '';
  if Connected then
    FQueryResults := TQueryResults.Create(FServer.EnumProcesses(''));
end;

function TSQLServer.GetConnected: Boolean;
begin
  Result := FServer.VerifyConnection(DMOConn_CurrentState) and
    (FDatabase <> nil);
  if not Result then
  begin
    Disconnect;
    FTransactionRef := 0;
  end;
end;

function TSQLServer.GetStatus;
const
  StatusElements: array[TServerStatusElement] of DMO_DBSTATUS_TYPE = (
    DMODBStat_Normal, DMODBStat_Loading, DMODBStat_Recovering, DMODBStat_Suspect,
    DMODBStat_Offline, DMODBStat_Inaccessible, DMODBStat_EmergencyMode,
    DMODBStat_Standby);
var
  DatabaseStatus: DMO_DBSTATUS_TYPE;
  I: TServerStatusElement;
begin
  Result := [];
  if Connected then
  begin
    DatabaseStatus := FDatabase.Status;
    if DatabaseStatus = DMODBStat_Normal then
      Result := [ssNormal]
    else for I := ssRecovering to High(TServerStatusElement) do
      if DatabaseStatus and StatusElements[I] = StatusElements[I] then
        Include(Result, I);
  end;
end;

end.
