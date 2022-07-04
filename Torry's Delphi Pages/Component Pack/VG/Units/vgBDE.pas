{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Data access components: BDE                   }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgBDE;

interface
uses Windows, Classes, DB, vgDB, DBTables;

type
{$IFNDEF _D3_ }
  TBDEDataSet     = TDataSet;
{$ENDIF}

{ TBDEDataSetHook }
  TBDEDataSetHook = class(TDataSetHook)
  private
    { Redefined events }
{$IFNDEF _D4_}
    FOnServerYield: TOnServerYieldEvent;
{$ENDIF}
    FOnUpdateError: TUpdateErrorEvent;
    FOnUpdateRecord: TUpdateRecordEvent;
    { Saved events }
{$IFNDEF _D4_}
    FSaveOnServerYield: TOnServerYieldEvent;
{$ENDIF}
    FSaveOnUpdateError: TUpdateErrorEvent;
    FSaveOnUpdateRecord: TUpdateRecordEvent;
    function GetDataSet: TBDEDataSet;
    procedure SetDataSet(Value: TBDEDataSet);
  protected
    { New handlers }
{$IFNDEF _D4_}
    procedure DoOnServerYield(DataSet: TDataSet; var AbortQuery: Boolean);
{$ENDIF}
    procedure DoOnUpdateError (DataSet: TDataSet; E: EDatabaseError;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    procedure DoOnUpdateRecord(DataSet: TDataSet; UpdateKind: TUpdateKind;
      var UpdateAction: TUpdateAction);
  protected
    { Protected declarations }
    procedure HookObject; override;
    procedure UnHookObject; override;
  public
    { Public declarations }
    procedure AssignEventsTo(DataHook: TCustomDataSetHook); override;
  published
    { Published declarations }
    property DataSet: TBDEDataSet read GetDataSet write SetDataSet;
{$IFNDEF _D4_}
    property OnServerYield: TOnServerYieldEvent read FOnServerYield write FOnServerYield;
{$ENDIF}
    property OnUpdateError: TUpdateErrorEvent read FOnUpdateError write FOnUpdateError;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
  end;

{ TvgDatabase }
  TvgDatabase = class(TDatabase)
  private
    FLock: TRTLCriticalSection;
    FTransLock: Boolean;
    FTransRestart: Boolean;
    function GetParam(Index: Integer): string;
    procedure SetParam(Index: Integer; Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
  published
    property DBPassword: string index 0 read GetParam write SetParam stored False;
    property DBUserName: string index 1 read GetParam write SetParam stored False;
    property TransLock: Boolean read FTransLock write FTransLock default True;
    property TransRestart: Boolean read FTransRestart write FTransRestart default False;
  end;

  TDBDataSetHack = class(TDBDataSet)
  public
{$IFDEF _D4_}
    function SetDBFlag(Flag: Integer; Value: Boolean): Boolean; override;
{$ELSE}
  {$IFDEF CBuilder}
    function SetDBFlag(Flag: Integer; Value: Boolean): Boolean; override;
  {$ELSE}
    procedure SetDBFlag(Flag: Integer; Value: Boolean); override;
  {$ENDIF}
{$ENDIF}
    procedure OpenCursor{$IFDEF _D3_}(InfoQuery: Boolean){$ENDIF}; override;
    procedure CloseCursor; override;
    property UpdateMode;
  end;

{$IFDEF _D3_}
  TBDEDataSetObject = class(TDataSetObject)
  protected
    function DoExecute: Integer; override;
    function DoGetParamValue(const ParamName: string): OleVariant; override;
    procedure DoSetParamValue(const ParamName: string; Value: OleVariant); override;
  end;
{$ENDIF}

  TDBDataSetClass = class of TDBDataSet;
  TQueryClass     = class of TQuery;

  TDatabaseProc   = procedure (Database: TDatabase);
  TEndTransProc   = procedure (Database: TDatabase; StartNew: Boolean);

implementation
uses BDE, vgBDEUtl;

function TBDEDataSetHook.GetDataSet: TBDEDataSet;
begin
   Result := TBDEDataSet(inherited DataSet);
end;

procedure TBDEDataSetHook.SetDataSet(Value: TBDEDataSet);
begin
   inherited DataSet := Value;
end;

procedure TBDEDataSetHook.HookObject;
begin
  if (csDesigning in ComponentState) then Exit;
  with DataSet do
  begin
    { Saving }
{$IFNDEF _D4_}
    FSaveOnServerYield := OnServerYield;
{$ENDIF}
    FSaveOnUpdateError := OnUpdateError;
    FSaveOnUpdateRecord := OnUpdateRecord;

    { Setting hooks }
{$IFNDEF _D4_}
    OnServerYield := DoOnServerYield;
{$ENDIF}
    OnUpdateError := DoOnUpdateError;
    OnUpdateRecord := DoOnUpdateRecord;
  end;
  inherited;
end;

procedure TBDEDataSetHook.UnHookObject;
begin
  if (csDesigning in ComponentState) then Exit;

  with DataSet do
  begin
    { Restoring }
{$IFNDEF _D4_}
    OnServerYield := FSaveOnServerYield;
{$ENDIF}
    OnUpdateError := FSaveOnUpdateError;
    OnUpdateRecord := FSaveOnUpdateRecord;
  end;
end;

procedure TBDEDataSetHook.AssignEventsTo(DataHook: TCustomDataSetHook);
begin
  inherited;
  with DataHook as TBDEDataSetHook do
  begin
{$IFNDEF _D4_}
    Self.FOnServerYield := OnServerYield;
{$ENDIF}
    Self.FOnUpdateError := OnUpdateError;
    Self.FOnUpdateRecord := OnUpdateRecord;
  end;
end;

{$IFNDEF _D4_}
procedure TBDEDataSetHook.DoOnServerYield(DataSet: TDataSet; var AbortQuery: Boolean);
begin
  if Assigned(FOnServerYield) then FOnServerYield(DataSet, AbortQuery);
end;
{$ENDIF}

procedure TBDEDataSetHook.DoOnUpdateError(DataSet: TDataSet; E: EDatabaseError;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  if Assigned(FOnUpdateError) then
    FOnUpdateError(DataSet, E, UpdateKind, UpdateAction) else
    DefaultUpdateError(DataSet, E, UpdateKind, UpdateAction);
end;

procedure TBDEDataSetHook.DoOnUpdateRecord(DataSet: TDataSet; UpdateKind: TUpdateKind;
  var UpdateAction: TUpdateAction);
begin
  if Assigned(FOnUpdateRecord) then
    FOnUpdateRecord(DataSet, UpdateKind, UpdateAction) else
    DefaultUpdateRecord(DataSet, UpdateKind, UpdateAction);
end;

{ TvgDatabase }
constructor TvgDatabase.Create(AOwner: TComponent);
begin
  inherited;
  InitializeCriticalSection(FLock);
  FTransLock := True;
end;

destructor TvgDatabase.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TvgDatabase.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TvgDatabase.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

procedure TvgDatabase.StartTransaction;
begin
  if FTransLock then Lock;
  try
    inherited StartTransaction;
  except
    if FTransLock then UnLock;
    raise;
  end;
end;

procedure TvgDatabase.Commit;
begin
  try
    inherited Commit;
  finally
    if FTransLock then UnLock;
  end;
end;

procedure TvgDatabase.Rollback;
begin
  try
    inherited Rollback;
  finally
    if FTransLock then UnLock;
  end;
end;

function TvgDatabase.GetParam(Index: Integer): string;
begin
  case Index of
    0: Result := Params.Values[szPASSWORD];
    1: Result := Params.Values[szUSERNAME];
  end;
end;

procedure TvgDatabase.SetParam(Index: Integer; Value: string);
begin
  case Index of
    0: Params.Values[szPASSWORD] := Value;
    1: Params.Values[szUSERNAME] := Value;
  end;
end;

{$IFDEF _D4_}
function TDBDataSetHack.SetDBFlag(Flag: Integer; Value: Boolean): Boolean;
{$ELSE}
{$IFDEF CBuilder}
function TDBDataSetHack.SetDBFlag(Flag: Integer; Value: Boolean): Boolean; 
{$ELSE}
procedure TDBDataSetHack.SetDBFlag(Flag: Integer; Value: Boolean);
{$ENDIF}
{$ENDIF}
begin
{$IFDEF _D4_}
  Result := False;
{$ENDIF}
{$IFDEF CBuilder}
  Result := False;
{$ENDIF}
end;

procedure TDBDataSetHack.OpenCursor{$IFDEF _D3_}(InfoQuery: Boolean){$ENDIF};
begin
  inherited;
end;

procedure TDBDataSetHack.CloseCursor;
begin
  inherited;
end;

{$IFDEF _D3_}
{ TBDEDataSetObject }
function TBDEDataSetObject.DoExecute: Integer;
begin
  if DataSet is TQuery then
    with (DataSet as TQuery) do
    begin
      ExecSQL;
      Result := RowsAffected;
    end
  else if (DataSet is TStoredProc) then
    with (DataSet as TStoredProc) do
    begin
      ExecProc;
      Result := -1;
    end
  else
    Result := -1;
end;

function TBDEDataSetObject.DoGetParamValue(const ParamName: string): OleVariant;
var
  Params: TParams;
begin
  if DataSet is TQuery then
    Params := (DataSet as TQuery).Params
  else if DataSet is TStoredProc then
    Params := (DataSet as TStoredProc).Params
  else
    Params := nil;
  if Assigned(Params) then Result := Params.ParamValues[ParamName];
end;

procedure TBDEDataSetObject.DoSetParamValue(const ParamName: string; Value: OleVariant);
var
  Params: TParams;
begin
  if DataSet is TQuery then
    Params := (DataSet as TQuery).Params
  else if DataSet is TStoredProc then
    Params := (DataSet as TStoredProc).Params
  else
    Params := nil;
  if Assigned(Params) then Params.ParamValues[ParamName] := Value;
end;
{$ENDIF}

end.
