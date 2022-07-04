{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         TvgQuery, TvgSQLScript, TvgUpdateScript       }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgQuery;

interface

uses SysUtils, Classes, Graphics, DB, DBTables, RxQuery,
  vgBDE;

type
  TvgQuery = class(TRxQuery)
  private
    { Private declarations }
    FAutoRefresh: Boolean;
    FIgnoreInactive: Boolean;
    FUpdateCount: Integer;
    FParamsChanged: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetValue(Params: TParams; const Name: String; Value: Variant);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Changed; virtual;
    procedure BeginUpdateParams;
    procedure EndUpdateParams;
    procedure SetMacro(const MacroName: String; Value: Variant);
    procedure SetParam(const ParamName: String; Value: Variant);
    property ParamsChanged: Boolean read FParamsChanged;
    property UpdateCount: Integer read FUpdateCount write FUpdateCount;
  published
    { Published declarations }
    property AutoRefresh: Boolean read FAutoRefresh write FAutoRefresh default True;
    property IgnoreInactive: Boolean read FIgnoreInactive write FIgnoreInactive default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TExecStatementEvent = procedure (Sender: TObject; LineNo, StatementNo: Integer) of object;

{ TvgSQLScript }
{ Most of TvgSQLScript's code was copied from RxQuery.pas unit of RX library }
  TvgSQLScript = class(TComponent)
  private
    FMacroChar: Char;
    FSQL: TStrings;
    FParams: array[0..1] of TParams;
    FQuery: TvgQuery;
    FTransaction: Boolean;
    FSemicolonTerm: Boolean;
    FIgnoreParams: Boolean;
    FTerm: Char;
    FOnBeforeExec, FOnAfterExec: TNotifyEvent;
    FOnBeforeExecStatement, FOnAfterExecStatement: TExecStatementEvent;
    FOnScriptError: TScriptErrorEvent;
    function GetSessionName: string;
    procedure SetSessionName(const Value: string);
    function GetDBSession: TSession;
    function GetText: string;
{$IFDEF _D4_}
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
    procedure ReadMacroData(Reader: TReader);
    procedure WriteMacroData(Writer: TWriter);
{$ENDIF _D4_}
    function GetDatabase: TDatabase;
    function GetDatabaseName: string;
    procedure SetDatabaseName(const Value: string);
    procedure CreateParams(List: TParams; const Value: PChar; Macro: Boolean; Delimeter: Char);
    procedure QueryChanged(Sender: TObject);
    procedure SetMacroChar(Value: Char);
    procedure SetQuery(Value: TStrings);
    procedure SetParamsList(Index: Integer; Value: TParams);
    function GetParamsCount(Index: Integer): Integer;
  protected
{$IFDEF _D4_}
    procedure DefineProperties(Filer: TFiler); override;
{$ENDIF _D4_}
    procedure CheckExecQuery(LineNo, StatementNo: Integer);
    procedure ExecuteScript(StatementNo: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecSQL;
    procedure ExecStatement(StatementNo: Integer);
    function ParamByName(const Value: string): TParam;
    function MacroByName(const Value: string): TParam;
    property DBSession: TSession read GetDBSession;
    property Text: string read GetText;
    property Database: TDatabase read GetDatabase;
    property ParamCount: Integer index 0 read GetParamsCount;
    property MacroCount: Integer index 1 read GetParamsCount;
    property Query: TvgQuery read FQuery;
  published
    property DatabaseName: string read GetDatabaseName write SetDatabaseName;
    property IgnoreParams: Boolean read FIgnoreParams write FIgnoreParams default False;
    property MacroChar: Char read FMacroChar write SetMacroChar default DefaultMacroChar;
    property SemicolonTerm: Boolean read FSemicolonTerm write FSemicolonTerm default True;
    property SessionName: string read GetSessionName write SetSessionName;
    property Term: Char read FTerm write FTerm default DefaultTermChar;
    property SQL: TStrings read FSQL write SetQuery;
    property Params: TParams index 0 read FParams[0] write SetParamsList {$IFDEF _D4_} stored False {$ENDIF};
    property Macros: TParams index 1 read FParams[1] write SetParamsList {$IFDEF _D4_} stored False {$ENDIF};
    property Transaction: Boolean read FTransaction write FTransaction default False;
    property OnBeforeExec: TNotifyEvent read FOnBeforeExec write FOnBeforeExec;
    property OnAfterExec: TNotifyEvent read FOnAfterExec write FOnAfterExec;
    property OnBeforeExecStatement: TExecStatementEvent read FOnBeforeExecStatement write FOnBeforeExecStatement;
    property OnAfterExecStatement: TExecStatementEvent read FOnAfterExecStatement write FOnAfterExecStatement;
    property OnScriptError: TScriptErrorEvent read FOnScriptError write FOnScriptError;
  end;

{ TUpdateParamsMacros }
  TUpdateParamsMacros = class(TPersistent)
  private
    function GetParamsByIndex(Index: Integer): TParams;
    procedure SetParams(Index: Integer; Value: TParams);
{$IFDEF _D4_}
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
{$ENDIF _D4_}
  protected
    function GetParams(UpdateKind: TUpdateKind; Macros: Boolean): TParams; virtual; abstract;
{$IFDEF _D4_}
    procedure DefineProperties(Filer: TFiler); override;
{$ENDIF _D4_}
  public
    procedure Assign(Source: TPersistent); override;
    property Params[UpdateKind: TUpdateKind; Macros: Boolean]: TParams read GetParams;
  published
    property ModifyParams: TParams index 0 read GetParamsByIndex write SetParams {$IFDEF _D4_} stored False {$ENDIF};
    property ModifyMacros: TParams index 1 read GetParamsByIndex write SetParams {$IFDEF _D4_} stored False {$ENDIF};
    property InsertParams: TParams index 2 read GetParamsByIndex write SetParams {$IFDEF _D4_} stored False {$ENDIF};
    property InsertMacros: TParams index 3 read GetParamsByIndex write SetParams {$IFDEF _D4_} stored False {$ENDIF};
    property DeleteParams: TParams index 4 read GetParamsByIndex write SetParams {$IFDEF _D4_} stored False {$ENDIF};
    property DeleteMacros: TParams index 5 read GetParamsByIndex write SetParams {$IFDEF _D4_} stored False {$ENDIF};
  end;

  TUpdateScriptNotifyEvent = procedure (Sender: TObject;
    DataSet: TDataSet; UpdateKind: TUpdateKind; Script: TvgSQLScript) of object;

  TUpdateScriptStatementEvent = procedure (Sender: TObject;
    DataSet: TDataSet; UpdateKind: TUpdateKind; Script: TvgSQLScript;
    LineNo, StatementNo: Integer) of object;

  TUpdateScriptErrorEvent = procedure (Sender: TObject;
    DataSet: TDataSet; UpdateKind: TUpdateKind; Script: TvgSQLScript;
    E: EDatabaseError; LineNo, StatementNo: Integer; var Action: TScriptAction) of object;

{ TvgUpdateScript }
  TvgUpdateScript = class(TDataSetUpdateObject)
  private
    FBindMacros: Boolean;
    FMacroChar: Char;
    FParamsMacros: TUpdateParamsMacros;
    FCheckRowsAffected: Boolean;
    FDataSet: TBDEDataSet;
    FScripts: array[TUpdateKind] of TvgSQLScript;
    FSQLText: array[TUpdateKind] of TStrings;
    FUpdateKind: TUpdateKind;
    FOnBeforeUpdate, FOnAfterUpdate: TUpdateScriptNotifyEvent;
    FOnBeforeUpdateStatement, FOnAfterUpdateStatement: TUpdateScriptStatementEvent;
    FOnUpdateScriptError: TUpdateScriptErrorEvent;
    function GetSQLScript(UpdateKind: TUpdateKind): TvgSQLScript;
    function GetSQL(UpdateKind: TUpdateKind): TStrings;
    function GetSQLIndex(Index: Integer): TStrings;
    procedure SetMacroChar(Value: Char);
    procedure SetParamsMacros(Value: TUpdateParamsMacros);
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    procedure SetSQLIndex(Index: Integer; Value: TStrings);
    procedure BeforeExec(Sender: TObject);
    procedure AfterExec(Sender: TObject);
    procedure BeforeExecStatement(Sender: TObject; LineNo, StatementNo: Integer);
    procedure AfterExecStatement(Sender: TObject; LineNo, StatementNo: Integer);
    procedure ScriptError(Sender: TObject; E: EDatabaseError;
      LineNo, StatementNo: Integer; var Action: TScriptAction);
  protected
    function GetDataSet: TBDEDataSet; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDataSet(ADataSet: TBDEDataSet); override;
    procedure SQLChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Apply(UpdateKind: TUpdateKind); override;
    procedure ExecSQL(UpdateKind: TUpdateKind);
    procedure SetParams(UpdateKind: TUpdateKind);
    property SQLScript[UpdateKind: TUpdateKind]: TvgSQLScript read GetSQLScript;
    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
  published
    property BindMacros: Boolean read FBindMacros write FBindMacros default False;
    property CheckRowsAffected: Boolean read FCheckRowsAffected write FCheckRowsAffected default True;
    property DataSet;
    property MacroChar: Char read FMacroChar write SetMacroChar default DefaultMacroChar;
    property ModifySQL: TStrings index 0 read GetSQLIndex write SetSQLIndex;
    property InsertSQL: TStrings index 1 read GetSQLIndex write SetSQLIndex;
    property DeleteSQL: TStrings index 2 read GetSQLIndex write SetSQLIndex;
    property ParamsMacros: TUpdateParamsMacros read FParamsMacros write SetParamsMacros;
    property OnBeforeUpdate: TUpdateScriptNotifyEvent read FOnBeforeUpdate write FOnBeforeUpdate;
    property OnAfterUpdate: TUpdateScriptNotifyEvent read FOnAfterUpdate write FOnAfterUpdate;
    property OnBeforeUpdateStatement: TUpdateScriptStatementEvent read FOnBeforeUpdateStatement write FOnBeforeUpdateStatement;
    property OnAfterUpdateStatement: TUpdateScriptStatementEvent read FOnAfterUpdateStatement write FOnAfterUpdateStatement;
    property OnUpdateScriptError: TUpdateScriptErrorEvent read FOnUpdateScriptError write FOnUpdateScriptError;
  end;

implementation
uses Consts, {$IFDEF _D3_}BDEConst{$ELSE}DBConsts{$ENDIF}, vgUtils, Forms, vgDBUtl,
  vgBDEUtl, vgDBPrms;

constructor TvgQuery.Create(AOwner: TComponent);
begin
  inherited;
  FAutoRefresh := True;
  FIgnoreInactive := True;
end;

procedure TvgQuery.BeginUpdateParams;
begin
  Inc(FUpdateCount);
  DisableControls;
end;

procedure TvgQuery.Changed;
begin
  if csDestroying in ComponentState then Exit;

  DisableControls;
  try
    if Assigned(FOnChange) then
      FOnChange(Self)
    else if Active then RefreshDataSet(Self) else Open;
  finally
    FParamsChanged := False;
    EnableControls;
  end;
end;

procedure TvgQuery.EndUpdateParams;
begin
  Dec(FUpdateCount);
  try
    if (FUpdateCount = 0) then
      if FParamsChanged and FAutoRefresh and
        (Active or FIgnoreInactive) then Changed;
  finally
    EnableControls;
  end;
end;

procedure TvgQuery.SetMacro(const MacroName: String; Value: Variant);
begin
  SetValue(Macros, MacroName, Value);
end;

procedure TvgQuery.SetParam(const ParamName: String; Value: Variant);
begin
  SetValue(Params, ParamName, Value);
end;

procedure TvgQuery.SetValue(Params: TParams; const Name: String; Value: Variant);
var
  Param: TParam;
begin
  BeginUpdateParams;
  try
    Param := Params.ParamByName(Name);
    FParamsChanged := FParamsChanged or
      (VarType(Param.Value) <> VarType(Value)) or (Param.Value <> Value);
    if Value = Null then
      Param.Clear else
      Param.Value := Value;
  finally
    EndUpdateParams;
  end;
end;

{ TvgSQLScript}

constructor TvgSQLScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMacroChar := DefaultMacroChar;
  FSQL := TStringList.Create;
  TStringList(SQL).OnChange := QueryChanged;
  FParams[0] := TParams.Create{$IFDEF _D4_}(Self){$ENDIF};
  FParams[1] := TParams.Create{$IFDEF _D4_}(Self){$ENDIF};
  FQuery := TvgQuery.Create(nil);
  FSemicolonTerm := True;
  FTerm := DefaultTermChar;
end;

destructor TvgSQLScript.Destroy;
begin
  FQuery.Free;
  FSQL.Free;
  FParams[0].Free; FParams[1].Free;
  inherited Destroy;
end;

function TvgSQLScript.GetDatabase: TDatabase;
begin
  Result := FQuery.Database;
end;

function TvgSQLScript.GetDatabaseName: string;
begin
  Result := FQuery.DatabaseName;
end;

procedure TvgSQLScript.SetDatabaseName(const Value: string);
begin
  FQuery.DatabaseName := Value;
end;

function TvgSQLScript.GetSessionName: string;
begin
  Result := FQuery.SessionName;
end;

procedure TvgSQLScript.SetSessionName(const Value: string);
begin
  FQuery.SessionName := Value;
end;

function TvgSQLScript.GetDBSession: TSession;
begin
  Result := FQuery.DBSession;
end;

procedure TvgSQLScript.CheckExecQuery(LineNo, StatementNo: Integer);
var
  Done: Boolean;
  Action: TScriptAction;
  S: string;
begin
  Done := False;
  repeat
    try
      if IgnoreParams then
      begin
        FQuery.Prepare;
        if Assigned(FOnBeforeExecStatement) then FOnBeforeExecStatement(Self, LineNo, StatementNo);
        FQuery.ExecDirect
      end else begin
        FQuery.Params.AssignValues(FParams[0]);
        FQuery.Macros.AssignValues(FParams[1]);
        if Assigned(FOnBeforeExecStatement) then FOnBeforeExecStatement(Self, LineNo, StatementNo);
        FQuery.Prepare;
        FQuery.ExecSQL;
      end;
      if Assigned(FOnAfterExecStatement) then FOnAfterExecStatement(Self, LineNo, StatementNo);
      Done := True;
    except
      on E: EDatabaseError do begin
        Action := saFail;
        S := Format(ResStr(SParseError), [ResStr(SMsgDlgError), LineNo]);
        if E is EDBEngineError then
          TDBError.Create(EDBEngineError(E), 0, LineNo, PChar(S))
        else begin
          if E.Message <> '' then E.Message := E.Message + '. ';
          E.Message := E.Message + S;
        end;
        if Assigned(FOnScriptError) then
          FOnScriptError(Self, E, LineNo, StatementNo, Action);
        if Action = saFail then raise;
        if Action = saAbort then SysUtils.Abort;
        if Action = saContinue then begin
          Application.HandleException(Self);
          Done := True;
        end else
          if Action = RxQuery.saIgnore then Done := True;
      end;
    end;
  until Done;
end;

procedure TvgSQLScript.ExecuteScript(StatementNo: Integer);
var
  S, LastStr: string;
  IsTrans, SQLFilled, StmtFound: Boolean;
  I, P, CurrStatement: Integer;
  TmpSQL: TStrings;
begin
  IsTrans := FTransaction and not Database.InTransaction and (StatementNo < 0);
  LastStr := '';
  try
    if IsTrans then
    begin
      if not Database.IsSQLBased then
        Database.TransIsolation := tiDirtyRead;
      StartTransaction(Database);
    end;
  except
    IsTrans := False;
  end;
  try
    I := 0;
    CurrStatement := 0;
    StmtFound := False;
    while I < SQL.Count do
    begin
      TmpSQL := TStringList.Create;
      try
        TmpSQL.Clear;
        SQLFilled := False;
        repeat
          if LastStr <> '' then begin
            TmpSQL.Add(LastStr);
            LastStr := '';
          end;
          if I < SQL.Count then
          begin
            S := Trim(SQL[I]);
            Inc(I);
            P := Pos(';', S);
            if (P > 0) and FSemicolonTerm then
            begin
              LastStr := Trim(Copy(S, P + 1, MaxInt));
              S := Copy(S, 1, P - 1);
              if S <> '' then TmpSQL.Add(S);
              SQLFilled := True;
            end else begin
              if (S = Term) then SQLFilled := True
              else if S <> '' then TmpSQL.Add(S);
            end;
          end else
            SQLFilled := True;
        until SQLFilled;

        S := TmpSQL.Text;
        if Query.SQL.Text <> S then Query.SQL.Assign(TmpSQL);
      finally
        TmpSQL.Free;
      end;
      if FQuery.SQL.Count > 0 then
      begin
        if (StatementNo < 0) or (StatementNo = CurrStatement) then
        begin
          StmtFound := True;
          CheckExecQuery(I - 1, CurrStatement);
          if StatementNo = CurrStatement then Break;
        end;
        Inc(CurrStatement);
      end;
    end;
    if not StmtFound then
    begin
{$IFDEF _D3_}
      DatabaseError(Format(SListIndexError, [StatementNo]));
{$ELSE}
      DatabaseError(Format('%s: %d', [LoadStr(SListIndexError), StatementNo]));
{$ENDIF}
    end;
    if IsTrans then Commit(Database);
  except
    if IsTrans then Rollback(Database);
    raise;
  end;
end;

procedure TvgSQLScript.ExecStatement(StatementNo: Integer);
begin
  if FSQL.Count = 0 then {$IFDEF _D3_}DatabaseError{$ELSE}DBError{$ENDIF}(SEmptySQLStatement);
  TDBDataSetHack(FQuery).SetDBFlag(dbfExecScript, True);
  try
    if not Database.Connected then {$IFDEF _D3_}DatabaseError{$ELSE}DBError{$ENDIF}(SDatabaseClosed);
    if Assigned(FOnBeforeExec) then FOnBeforeExec(Self);
    ExecuteScript(StatementNo);
    if Assigned(FOnAfterExec) then FOnAfterExec(Self);
  finally
    TDBDataSetHack(FQuery).SetDBFlag(dbfExecScript, False);
  end;
end;

procedure TvgSQLScript.ExecSQL;
begin
  ExecStatement(-1);
end;

procedure TvgSQLScript.CreateParams(List: TParams; const Value: PChar; Macro: Boolean; Delimeter: Char);
begin
  vgDBPrms.CreateParams(List, Value, Macro, Delimeter, []);
end;

procedure TvgSQLScript.SetQuery(Value: TStrings);
begin
  TStringList(SQL).OnChange := nil;
  FSQL.Assign(Value);
  TStringList(SQL).OnChange := QueryChanged;
  QueryChanged(nil);
end;

function TvgSQLScript.GetText: string;
begin
  Result := SQL.Text;
end;

procedure TvgSQLScript.QueryChanged(Sender: TObject);
  procedure CreateParamList(var AParams: TParams; Macro: Boolean; Delimeter: Char);
  var
    List: TParams;
  begin
  {$IFDEF _D4_}
    if not (csReading in ComponentState) then
    begin
  {$ENDIF}
      List := TParams.Create{$IFDEF _D4_}(Self){$ENDIF};
      try
        CreateParams(List, PChar(Text), Macro, Delimeter);
        List.AssignValues(AParams);
    {$IFDEF _D4_}
        AParams.Clear;
        AParams.Assign(List);
      finally
    {$ELSE}
        AParams.Free;
        AParams := List;
      except
    {$ENDIF}
        List.Free;
      end;
  {$IFDEF _D4_}
    end else begin
      AParams.Clear;
      CreateParams(AParams, PChar(Text), Macro, Delimeter);
    end;
  {$ENDIF _D4_}
  end;
begin
  CreateParamList(FParams[0], False, ':');
  CreateParamList(FParams[1], True, FMacroChar);
end;

procedure TvgSQLScript.SetMacroChar(Value: Char);
begin
  if Value <> FMacroChar then
  begin
    FMacroChar := Value;
    QueryChanged(nil);
  end;
end;

function TvgSQLScript.ParamByName(const Value: string): TParam;
begin
  Result := FParams[0].ParamByName(Value);
end;

function TvgSQLScript.MacroByName(const Value: string): TParam;
begin
  Result := FParams[1].ParamByName(Value);
end;

procedure TvgSQLScript.SetParamsList(Index: Integer; Value: TParams);
begin
  FParams[Index].AssignValues(Value);
end;

function TvgSQLScript.GetParamsCount(Index: Integer): Integer;
begin
  Result := FParams[Index].Count;
end;

{$IFDEF _D4_}
procedure TvgSQLScript.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, True);
  Filer.DefineProperty('MacroData', ReadMacroData, WriteMacroData, True);
end;

procedure TvgSQLScript.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams[0]);
end;

procedure TvgSQLScript.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(FParams[0]);
end;

procedure TvgSQLScript.ReadMacroData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams[1]);
end;

procedure TvgSQLScript.WriteMacroData(Writer: TWriter);
begin
  Writer.WriteCollection(FParams[1]);
end;
{$ENDIF}

{ TUpdateParamsMacros }
procedure TUpdateParamsMacros.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TUpdateParamsMacros then
    for I := 0 to 5 do GetParamsByIndex(I).Assign(TUpdateParamsMacros(Source).GetParamsByIndex(I))
  else
    inherited Assign(Source);
end;

function TUpdateParamsMacros.GetParamsByIndex(Index: Integer): TParams;
begin
  Result := Params[TUpdateKind(Index div (ord(High(Boolean)) + 1)),
    Boolean(Index mod (ord(High(Boolean)) + 1))];
end;

procedure TUpdateParamsMacros.SetParams(Index: Integer; Value: TParams);
begin
  GetParamsByIndex(Index).AssignValues(Value);
end;

{$IFDEF _D4_}
procedure TUpdateParamsMacros.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Data', ReadData, WriteData, True);
end;

procedure TUpdateParamsMacros.ReadData(Reader: TReader);
var
  I: Integer;
begin
  for I := 0 to 5 do
  begin
    Reader.ReadValue;
    Reader.ReadCollection(GetParamsByIndex(I));
  end;
end;

procedure TUpdateParamsMacros.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  for I := 0 to 5 do Writer.WriteCollection(GetParamsByIndex(I));
end;
{$ENDIF}

{ TUpdateScriptParamsMacros }
type
  TUpdateScriptParamsMacros = class(TUpdateParamsMacros)
  private
    FUpdateScript: TvgUpdateScript;
  protected
    function GetParams(UpdateKind: TUpdateKind; Macros: Boolean): TParams; override;
  end;

function TUpdateScriptParamsMacros.GetParams(UpdateKind: TUpdateKind; Macros: Boolean): TParams;
begin
  if Macros then
    Result := FUpdateScript.SQLScript[UpdateKind].Macros else
    Result := FUpdateScript.SQLScript[UpdateKind].Params;
end;

{ TvgUpdateScript }
constructor TvgUpdateScript.Create(AOwner: TComponent);
var
  UpdateKind: TUpdateKind;
begin
  inherited;
  FCheckRowsAffected := True;
  FMacroChar := DefaultMacroChar;
  FParamsMacros := TUpdateScriptParamsMacros.Create;
  TUpdateScriptParamsMacros(FParamsMacros).FUpdateScript := Self;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FSQLText[UpdateKind] := TStringList.Create;
    TStringList(FSQLText[UpdateKind]).OnChange := SQLChanged;
  end;
end;

destructor TvgUpdateScript.Destroy;
var
  UpdateKind: TUpdateKind;
begin
  if Assigned(FDataSet) and (FDataSet.UpdateObject = Self) then
    FDataSet.UpdateObject := nil;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[UpdateKind].Free;
  inherited;
end;

procedure TvgUpdateScript.Apply(UpdateKind: TUpdateKind);
begin
  SetParams(UpdateKind);
  ExecSQL(UpdateKind);
end;

procedure TvgUpdateScript.ExecSQL(UpdateKind: TUpdateKind);
begin
  FUpdateKind := UpdateKind;
  if (FDataSet is TDBDataSet) then
  begin
    SQLScript[UpdateKind].SessionName := TDBDataSet(FDataSet).SessionName;
    SQLScript[UpdateKind].DatabaseName := TDBDataSet(FDataSet).DataBaseName;
  end;
  SQLScript[UpdateKind].ExecSQL;
end;

procedure TvgUpdateScript.SetParams(UpdateKind: TUpdateKind);
begin
  SetUpdateParams(SQLScript[UpdateKind].Params, FDataSet);
  if FBindMacros then
    SetUpdateParams(SQLScript[UpdateKind].Macros, FDataSet);
end;

procedure TvgUpdateScript.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDataSet) then FDataSet := nil;
end;

function TvgUpdateScript.GetSQLScript(UpdateKind: TUpdateKind): TvgSQLScript;
begin
  if not Assigned(FScripts[UpdateKind]) then
  begin
    FScripts[UpdateKind] := TvgSQLScript.Create(Self);
    with FScripts[UpdateKind] do
    begin
      MacroChar := Self.FMacroChar;
      SQL.Assign(FSQLText[UpdateKind]);
      OnBeforeExec := Self.BeforeExec;
      OnAfterExec := Self.AfterExec;
      OnBeforeExecStatement := Self.BeforeExecStatement;
      OnAfterExecStatement := Self.AfterExecStatement;
      OnScriptError := Self.ScriptError;
    end;
  end;
  Result := FScripts[UpdateKind];
end;

procedure TvgUpdateScript.BeforeExec(Sender: TObject);
begin
  if Assigned(FOnBeforeUpdate) then
    FOnBeforeUpdate(Self, DataSet, FUpdateKind, SQLScript[FUpdateKind]);
end;

procedure TvgUpdateScript.AfterExec(Sender: TObject);
begin
  if Assigned(FOnAfterUpdate) then
    FOnAfterUpdate(Self, DataSet, FUpdateKind, SQLScript[FUpdateKind])
  else begin
    if FCheckRowsAffected then
      vgDBUtl.CheckRowsAffected(SQLScript[FUpdateKind].Query.RowsAffected, 1);
  end;
end;

procedure TvgUpdateScript.BeforeExecStatement(Sender: TObject; LineNo, StatementNo: Integer);
begin
  if Assigned(FOnBeforeUpdateStatement) then
    FOnBeforeUpdateStatement(Self, DataSet, FUpdateKind, SQLScript[FUpdateKind], LineNo, StatementNo);
end;

procedure TvgUpdateScript.AfterExecStatement(Sender: TObject; LineNo, StatementNo: Integer);
begin
  if Assigned(FOnAfterUpdateStatement) then
    FOnAfterUpdateStatement(Self, DataSet, FUpdateKind, SQLScript[FUpdateKind], LineNo, StatementNo);
end;

procedure TvgUpdateScript.ScriptError(Sender: TObject; E: EDatabaseError;
  LineNo, StatementNo: Integer; var Action: TScriptAction);
begin
  if Assigned(FOnUpdateScriptError) then
    FOnUpdateScriptError(Self, DataSet, FUpdateKind, SQLScript[FUpdateKind], E, LineNo, StatementNo, Action);
end;

function TvgUpdateScript.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FSQLText[UpdateKind];
end;

function TvgUpdateScript.GetSQLIndex(Index: Integer): TStrings;
begin
  Result := FSQLText[TUpdateKind(Index)];
end;

procedure TvgUpdateScript.SetMacroChar(Value: Char);
var
  I: TUpdateKind;
begin
  for I := Low(TUpdateKind) to High(TUpdateKind) do
    if Assigned(FScripts[I]) then FScripts[I].MacroChar := Value;
  FMacroChar := Value;
end;

procedure TvgUpdateScript.SetParamsMacros(Value: TUpdateParamsMacros);
begin
  FParamsMacros.Assign(Value);
end;

procedure TvgUpdateScript.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  FSQLText[UpdateKind].Assign(Value);
end;

procedure TvgUpdateScript.SetSQLIndex(Index: Integer; Value: TStrings);
begin
  SetSQL(TUpdateKind(Index), Value);
end;

function TvgUpdateScript.GetDataSet: TBDEDataSet;
begin
  Result := FDataSet;
end;

procedure TvgUpdateScript.SetDataSet(ADataSet: TBDEDataSet);
var
  OldDataSet: TBDEDataSet;
begin
  if (FDataSet <> ADataSet) then
  begin
    OldDataSet := FDataSet;
    FDataSet := ADataSet;
    if Assigned(FDataSet) then
    begin
      FreeNotification(FDataSet);
      FDataSet.UpdateObject := Self;
    end else
      OldDataSet.UpdateObject := nil;
  end;
end;

procedure TvgUpdateScript.SQLChanged(Sender: TObject);
var
  UpdateKind: TUpdateKind;
begin
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    if Sender = FSQLText[UpdateKind] then
    begin
      if Assigned(FScripts[UpdateKind]) then
      begin
        FScripts[UpdateKind].Params.Clear;
        FScripts[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
      end;
      Break;
    end;
end;

end.
