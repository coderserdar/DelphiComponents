{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         TDBConverter, TDBConvertItem                  }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgDBConv;

interface
uses SysUtils, Classes, vgTools, DB;

type
  TDBConvertItem = class;
  TDBConverter = class;

  TTableMode    = (tmTableName, tmDataSet);
  TRecordAction = (raInsert, raEdit, raSkip);
  TUpdateResult = (urFail, urAbort, urSkip, urRetry, urApplied);

  TItemNotifyEvent = procedure (Item: TDBConvertItem) of object;
  TRecordActionEvent = procedure (Item: TDBConvertItem; var Action: TRecordAction) of object;
  TRecordErrorEvent = procedure (Item: TDBConvertItem; Action: TRecordAction;
    Error: Exception; var UpdateResult: TUpdateResult) of object;
  TProcessRecordEvent = procedure (Item: TDBConvertItem; Action: TRecordAction) of object;
  TFindRecordEvent = procedure (Item: TDBConvertItem; var Found: Boolean) of object;
  TQueryEOFEvent = procedure (Item: TDBConvertItem; var EOF: Boolean) of object;
  TInitDataSetEvent = procedure (Item: TDBConvertItem; TableMode: TTableMode;
    TableName: String; TableDataSet: TDataSet; Dest: Boolean; var DataSet: TDataSet) of object;

{ TDBConverterDesigner }

{ TDBConvertItem }
  TDBConvertItem = class(TItem)
  private
    FActive: Boolean;
    FKeyFields: String;
    FTableNames: array[0..1] of String;
    FTableDataSets: array [0..1] of TDataSet;
    FDataSets: array [0..1] of TDataSet;
    FTableModes: array [0..1] of TTableMode;
    FOnFindRecord: TFindRecordEvent;
    FOnNextRecord: TItemNotifyEvent;
    FOnProcessRecord: TProcessRecordEvent;
    FOnEditRecord: TRecordActionEvent;
    FOnCopyRecord: TRecordActionEvent;
    FOnPostRecord: TRecordActionEvent;
    FOnQueryRecordAction: TRecordActionEvent;
    FOnQuerySourceEOF: TQueryEOFEvent;
    FOnRecordError: TRecordErrorEvent;
    FOnInitDataSet: TInitDataSetEvent;
    { OnProcess }
    FOnBeginInit, FOnEndInit: TItemNotifyEvent;
    FOnBeginRun, FOnEndRun: TItemNotifyEvent;
    FOnBeginDone, FOnEndDone: TItemNotifyEvent;
    FOnBeforeInit, FOnAfterInit: TItemNotifyEvent;
    FOnBeforeRun, FOnAfterRun: TItemNotifyEvent;
    FOnBeforeDone, FOnAfterDone: TItemNotifyEvent;
    FOnInit: TItemNotifyEvent;
    FOnRun: TItemNotifyEvent;
    FOnDone: TItemNotifyEvent;
    FOnOpen: TItemNotifyEvent;
    FOnClose: TItemNotifyEvent;
    FOnBeginProcessDataSet, FOnEndProcessDataSet: TItemNotifyEvent;
    FOnBeginProcessRecord, FOnEndProcessRecord: TItemNotifyEvent;
    FOnBeginEditRecord, FOnEndEditRecord: TItemNotifyEvent;
    FOnBeginCopyRecord, FOnEndCopyRecord: TItemNotifyEvent;
    FOnBeginPostRecord, FOnEndPostRecord: TItemNotifyEvent;
    { OnBefore, OnAfter }
    FOnBeforeProcessDataSet, FOnAfterProcessDataSet: TItemNotifyEvent;
    FOnBeforeProcessRecord, FOnAfterProcessRecord: TRecordActionEvent;
    FOnBeforeEditRecord, FOnAfterEditRecord: TRecordActionEvent;
    FOnBeforeCopyRecord, FOnAfterCopyRecord: TRecordActionEvent;
    FOnBeforePostRecord, FOnAfterPostRecord: TRecordActionEvent;
    function IsKeyFieldsStored: Boolean;
    function GetConverter: TDBConverter;
    procedure SetConverter(Value: TDBConverter);
  protected
    { OnProcess }
    procedure BeginInit; virtual;
    procedure DoInit; virtual;
    procedure EndInit; virtual;
    procedure BeginRun; virtual;
    procedure DoRun; virtual;
    procedure EndRun; virtual;
    procedure BeginDone; virtual;
    procedure DoDone; virtual;
    procedure EndDone; virtual;
    procedure BeginProcessDataSet; virtual;
    procedure EndProcessDataSet; virtual;
    procedure BeginProcessRecord; virtual;
    procedure EndProcessRecord; virtual;
    procedure BeginEditRecord; virtual;
    procedure EndEditRecord; virtual;
    procedure BeginCopyRecord; virtual;
    procedure EndCopyRecord; virtual;
    procedure BeginPostRecord; virtual;
    procedure EndPostRecord; virtual;
    function RecordError(Action: TRecordAction; Error: Exception): TUpdateResult; virtual;
    { OnBefore, OnAfter }
    procedure BeforeInit; virtual;
    procedure AfterInit; virtual;
    procedure BeforeRun; virtual;
    procedure AfterRun; virtual;
    procedure BeforeDone; virtual;
    procedure AfterDone; virtual;
    procedure BeforeProcessDataSet; virtual;
    procedure AfterProcessDataSet; virtual;
    procedure BeforeProcessRecord(Action: TRecordAction); virtual;
    procedure AfterProcessRecord(Action: TRecordAction); virtual;
    procedure BeforeEditRecord(Action: TRecordAction); virtual;
    procedure AfterEditRecord(Action: TRecordAction); virtual;
    procedure BeforeCopyRecord(Action: TRecordAction); virtual;
    procedure AfterCopyRecord(Action: TRecordAction); virtual;
    procedure BeforePostRecord(Action: TRecordAction); virtual;
    procedure AfterPostRecord(Action: TRecordAction); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Handling procedures }
    procedure Execute;
    procedure ProcessDataSet;
    procedure ProcessRecord(Action: TRecordAction);
    procedure EditRecord(Action: TRecordAction);
    procedure CopyRecord(Action: TRecordAction);
    procedure PostRecord(Action: TRecordAction);
    procedure NextRecord;
    function FindRecord: Boolean;
    function QueryRecordAction: TRecordAction;
    function QuerySourceEOF: Boolean;
    { Convert }
    function InitDataSet(TableMode: TTableMode; TableName: String; TableDataSet: TDataSet; Dest: Boolean): TDataSet;
    procedure Init; virtual;
    procedure Run; virtual;
    procedure Done; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    { Default handlers }
    procedure DefaultOpen; virtual;
    procedure DefaultClose; virtual;
    procedure DefaultInit; virtual;
    procedure DefaultDone; virtual;
    function DefaultSourceEOF: Boolean;
    procedure DefaultNextRecord;
    function DefaultFindRecord: Boolean; virtual;
    procedure DefaultEditRecord(Action: TRecordAction); virtual;
    procedure DefaultCopyField(Source, Dest: String; Action: TRecordAction);
    procedure DefaultCopyRecord(Action: TRecordAction); virtual;
    procedure DefaultPostRecord(Action: TRecordAction); virtual;
    procedure DefaultInitDataSet(TableMode: TTableMode; TableName: String;
      TableDataSet: TDataSet; Dest: Boolean; var DataSet: TDataSet); virtual;
    { Public properties }
    property Converter: TDBConverter read GetConverter write SetConverter;
    property DataSetSource: TDataSet index 0 read FDataSets[0] write FDataSets[0];
    property DataSetDest: TDataSet index 1 read FDataSets[1] write FDataSets[1];
    { Log }
    procedure IncLogLevel;
    procedure DecLogLevel;
    procedure WriteLog(Msg: String);
  published
    property Active: Boolean read FActive write FActive default True;
    property KeyFields: String read FKeyFields write FKeyFields stored IsKeyFieldsStored;
    property TableNameSource: String index 0 read FTableNames[0] write FTableNames[0];
    property TableNameDest: String index 1 read FTableNames[1] write FTableNames[1];
    property TableModeSource: TTableMode index 0 read FTableModes[0] write FTableModes[0] default tmTableName;
    property TableModeDest: TTableMode index 1 read FTableModes[1] write FTableModes[1] default tmTableName;
    property TableDataSetSource: TDataSet index 0 read FTableDataSets[0] write FTableDataSets[0];
    property TableDataSetDest: TDataSet index 1 read FTableDataSets[1] write FTableDataSets[1];
    property Index stored False;
    property OnFindRecord: TFindRecordEvent read FOnFindRecord write FOnFindRecord;
    property OnNextRecord: TItemNotifyEvent read FOnNextRecord write FOnNextRecord;
    property OnProcessRecord: TProcessRecordEvent read FOnProcessRecord write FOnProcessRecord;
    property OnEditRecord: TRecordActionEvent read FOnEditRecord write FOnEditRecord;
    property OnPostRecord: TRecordActionEvent read FOnPostRecord write FOnPostRecord;
    property OnQueryRecordAction: TRecordActionEvent read FOnQueryRecordAction write FOnQueryRecordAction;
    property OnQuerySourceEOF: TQueryEOFEvent read FOnQuerySourceEOF write FOnQuerySourceEOF;
    property OnRecordError: TRecordErrorEvent read FOnRecordError write FOnRecordError;
    property OnInitDataSet: TInitDataSetEvent read FOnInitDataSet write FOnInitDataSet;
    { OnProcess }
    property OnInit: TItemNotifyEvent read FOnInit write FOnInit;
    property OnRun: TItemNotifyEvent read FOnRun write FOnRun;
    property OnDone: TItemNotifyEvent read FOnDone write FOnDone;
    property OnOpen: TItemNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TItemNotifyEvent read FOnClose write FOnClose;
    property OnBeginInit: TItemNotifyEvent read FOnBeginInit write FOnBeginInit;
    property OnEndInit: TItemNotifyEvent read FOnEndInit write FOnEndInit;
    property OnBeginRun: TItemNotifyEvent read FOnBeginRun write FOnBeginRun;
    property OnEndRun: TItemNotifyEvent read FOnEndRun write FOnEndRun;
    property OnBeginDone: TItemNotifyEvent read FOnBeginDone write FOnBeginDone;
    property OnEndDone: TItemNotifyEvent read FOnEndDone write FOnEndDone;
    property OnBeginProcessDataSet: TItemNotifyEvent read FOnBeginProcessDataSet write FOnBeginProcessDataSet;
    property OnEndProcessDataSet: TItemNotifyEvent read FOnEndProcessDataSet write FOnEndProcessDataSet;
    property OnBeginProcessRecord: TItemNotifyEvent read FOnBeginProcessRecord write FOnBeginProcessRecord;
    property OnEndProcessRecord: TItemNotifyEvent read FOnEndProcessRecord write FOnEndProcessRecord;
    property OnBeginEditRecord: TItemNotifyEvent read FOnBeginEditRecord write FOnBeginEditRecord;
    property OnEndEditRecord: TItemNotifyEvent read FOnEndEditRecord write FOnEndEditRecord;
    property OnBeginPostRecord: TItemNotifyEvent read FOnBeginPostRecord write FOnBeginPostRecord;
    property OnEndPostRecord: TItemNotifyEvent read FOnEndPostRecord write FOnEndPostRecord;
    { OnBefore, OnAfter }
    property OnBeforeInit: TItemNotifyEvent read FOnBeforeInit write FOnBeforeInit;
    property OnAfterInit: TItemNotifyEvent read FOnAfterInit write FOnAfterInit;
    property OnBeforeRun: TItemNotifyEvent read FOnBeforeRun write FOnBeforeRun;
    property OnAfterRun: TItemNotifyEvent read FOnAfterRun write FOnAfterRun;
    property OnBeforeDone: TItemNotifyEvent read FOnBeforeDone write FOnBeforeDone;
    property OnAfterDone: TItemNotifyEvent read FOnAfterDone write FOnAfterDone;
    property OnBeforeProcessDataSet: TItemNotifyEvent read FOnBeforeProcessDataSet write FOnBeforeProcessDataSet;
    property OnAfterProcessDataSet: TItemNotifyEvent read FOnAfterProcessDataSet write FOnAfterProcessDataSet;
    property OnBeforeProcessRecord: TRecordActionEvent read FOnBeforeProcessRecord write FOnBeforeProcessRecord;
    property OnAfterProcessRecord: TRecordActionEvent read FOnAfterProcessRecord write FOnAfterProcessRecord;
    property OnBeforeEditRecord: TRecordActionEvent read FOnBeforeEditRecord write FOnBeforeEditRecord;
    property OnAfterEditRecord: TRecordActionEvent read FOnAfterEditRecord write FOnAfterEditRecord;
    property OnBeforePostRecord: TRecordActionEvent read FOnBeforePostRecord write FOnBeforePostRecord;
    property OnAfterPostRecord: TRecordActionEvent read FOnAfterPostRecord write FOnAfterPostRecord;
  end;

{ TDBConverter }
  TLogFormat = (lfNone, lfTime, lfDateTime);

  TWriteLogEvent = procedure (Sender: TObject; Msg: String) of object;

  TDBConverter = class(TItemList)
  private
    FLog: TStrings;
    FLogFormat: TLogFormat;
    FLogLevel: Integer;
    FLogLevels: Boolean;
    FOnWriteLog: TWriteLogEvent;
    function GetItem(Index: Integer): TDBConvertItem;
    procedure SetLog(Value: TStrings);
  protected
    procedure DoWriteLog(Msg: String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Process }
    procedure InitItems;
    procedure RunItems;
    procedure DoneItems;
    { Log }
    procedure ClearLog;
    procedure IncLogLevel;
    procedure DecLogLevel;
    procedure WriteLog(Msg: String);
    property Items[Index: Integer]: TDBConvertItem read GetItem; default;
    property LogLevel: Integer read FLogLevel write FLogLevel;
  published
    property Log: TStrings read FLog write SetLog;
    property LogFormat: TLogFormat read FLogFormat write FLogFormat default lfNone;
    property LogLevels: Boolean read FLogLevels write FLogLevels default True;
    property OnWriteLog: TWriteLogEvent read FOnWriteLog write FOnWriteLog;
  end;

  TDBConvertItemClass = class of TDBConvertItem;
  TRegisterConvertItemsProc = procedure (const ConvertClasses: array of TDBConvertItemClass);


var
  RegisterConvertItemsProc: TRegisterConvertItemsProc = nil;

procedure RegisterConvertItems(const ConvertClasses: array of TDBConvertItemClass);

implementation
uses vgDBUtl;

const
  Registered: Boolean     = False;
  DefKeyFields            = 'ID';

procedure RegisterConvertItems(const ConvertClasses: array of TDBConvertItemClass);
begin
  if Assigned(RegisterConvertItemsProc) then RegisterConvertItemsProc(ConvertClasses);
end;

{ TDBConvertItem }
constructor TDBConvertItem.Create(AOwner: TComponent);
begin
  inherited;
  FActive := True;
  FKeyFields := DefKeyFields;
end;

destructor TDBConvertItem.Destroy;
begin
  DoDone;
  inherited;
end;

procedure TDBConvertItem.Execute;
begin
  Init;
  try
    Run;
  finally
    Done;
  end;
end;

procedure TDBConvertItem.Init;
begin
  BeginInit;
  try
    BeforeInit;
    DoInit;
    AfterInit;
  finally
    EndInit;
  end;
end;

procedure TDBConvertItem.Run;
begin
  BeginRun;
  try
    BeforeRun;
    DoRun;
    AfterRun;
  finally
    EndRun;
  end;
end;

procedure TDBConvertItem.Done;
begin
  BeginDone;
  try
    BeforeDone;
    DoDone;
    AfterDone;
  finally
    EndDone;
  end;
end;

procedure TDBConvertItem.Open;
begin
  if Assigned(FOnOpen) then FOnOpen(Self) else DefaultOpen;
end;

procedure TDBConvertItem.Close;
begin
  if Assigned(FOnClose) then FOnClose(Self) else DefaultClose;
end;

procedure TDBConvertItem.ProcessDataSet;
var
  Action: TRecordAction;
begin
  BeginProcessDataSet;
  try
    BeforeProcessDataSet;
    while not QuerySourceEOF do
    begin
      Action := QueryRecordAction;
      try
        ProcessRecord(Action);
      except
        case RecordError(Action, Exception(ExceptObject)) of
          urFail:
            raise;
          urAbort:
            Break;
          urSkip:
            begin NextRecord; Continue; end;
          urRetry:
            Continue;
          urApplied:
            ;
        end
      end;
      NextRecord;
    end;
    AfterProcessDataSet;
  finally
    EndProcessDataSet;
  end;
end;

procedure TDBConvertItem.ProcessRecord(Action: TRecordAction);
begin
  BeginProcessRecord;
  try
    BeforeProcessRecord(Action);
    if Assigned(FOnProcessRecord) then
      FOnProcessRecord(Self, Action)
    else if Action <> raSkip then begin
      EditRecord(Action);
      PostRecord(Action);
    end;
    AfterProcessRecord(Action);
  finally
    EndProcessRecord;
  end;
end;

function TDBConvertItem.DefaultSourceEOF: Boolean;
begin
  Result := DataSetSource.EOF;
end;

procedure TDBConvertItem.DefaultNextRecord;
begin
  DataSetSource.Next;
end;

function TDBConvertItem.DefaultFindRecord: Boolean;
var
  FieldValues: Variant;
begin
  FieldValues := DataSetSource.FieldValues[FKeyFields];
  try
    Result := DataSetDest.Locate(FKeyFields, FieldValues, []);
  except
    Result := False;
  end;
end;

procedure TDBConvertItem.DefaultEditRecord(Action: TRecordAction);
begin
  if Action = raInsert then DataSetDest.Insert else DataSetDest.Edit;
end;

procedure TDBConvertItem.DefaultCopyField(Source, Dest: String; Action: TRecordAction);
var
  Field1, Field2: TField;
begin
  Field1 := DataSetDest.FieldByName(Dest);
  Field2 := DataSetSource.FindField(Source);
  if Assigned(Field2) then Field1.Value := Field2.Value;
end;

procedure TDBConvertItem.DefaultCopyRecord(Action: TRecordAction);
var
  I: Integer;
  Field: TField;
begin
  for I := 0 to DataSetDest.FieldCount - 1 do
  begin
    Field := DataSetDest.Fields[I];
    if not Field.ReadOnly then
      DefaultCopyField(Field.FieldName, Field.FieldName, Action);
  end;
end;

procedure TDBConvertItem.DefaultPostRecord(Action: TRecordAction);
begin
  DataSetDest.Post;
end;

procedure TDBConvertItem.DefaultInitDataSet(TableMode: TTableMode; TableName: String;
  TableDataSet: TDataSet; Dest: Boolean; var DataSet: TDataSet);
begin
end;

function TDBConvertItem.InitDataSet(TableMode: TTableMode; TableName: String; TableDataSet: TDataSet; Dest: Boolean): TDataSet;
begin
  if TableMode = tmDataSet then Result := TableDataSet else Result := nil;
  if Assigned(FOnInitDataSet) then
    FOnInitDataSet(Self, TableMode, TableName, TableDataSet, Dest, Result) else
    DefaultInitDataSet(TableMode, TableName, TableDataSet, Dest, Result);
end;

procedure TDBConvertItem.DefaultInit;
begin
  DataSetSource := InitDataSet(TableModeSource, TableNameSource, TableDataSetSource, False);
  DataSetDest := InitDataSet(TableModeDest, TableNameDest, TableDataSetDest, True);
  Open;
end;

procedure TDBConvertItem.DefaultDone;
begin
  if Assigned(DataSetSource) and (DataSetSource.Owner = Self) then
  begin
    DataSetSource.Free;
    DataSetSource := nil;
  end;

  if Assigned(DataSetDest) and (DataSetDest.Owner = Self) then
  begin
    DataSetDest.Free;
    DataSetDest := nil;
  end;
end;

procedure TDBConvertItem.NextRecord;
begin
  if Assigned(FOnNextRecord) then
    FOnNextRecord(Self) else
    DefaultNextRecord;
end;

function TDBConvertItem.FindRecord: Boolean;
begin
  Result := False;
  if Assigned(FOnFindRecord) then
    FOnFindRecord(Self, Result)
  else
    Result := DefaultFindRecord;
end;

procedure TDBConvertItem.EditRecord(Action: TRecordAction);
begin
  BeginEditRecord;
  try
    BeforeEditRecord(Action);
    if Assigned(FOnEditRecord) then
      FOnEditRecord(Self, Action)
    else begin
      DefaultEditRecord(Action);
      CopyRecord(Action);
    end;
    AfterEditRecord(Action);
  finally
    EndEditRecord;
  end;
end;

procedure TDBConvertItem.CopyRecord(Action: TRecordAction);
begin
  BeginCopyRecord;
  try
    BeforeCopyRecord(Action);
    if Assigned(FOnCopyRecord) then
      FOnCopyRecord(Self, Action) else
      DefaultCopyRecord(Action);
    AfterCopyRecord(Action);
  finally
    EndCopyRecord;
  end;
end;

procedure TDBConvertItem.PostRecord(Action: TRecordAction);
begin
  BeginPostRecord;
  try
    BeforePostRecord(Action);
    if Assigned(FOnPostRecord) then
      FOnPostRecord(Self, Action) else
      DefaultPostRecord(Action);
    AfterPostRecord(Action);
  finally
    EndPostRecord;
  end;
end;

function TDBConvertItem.QueryRecordAction: TRecordAction;
begin
  if Assigned(FOnQueryRecordAction) then
  begin
    Result := raSkip;
    FOnQueryRecordAction(Self, Result)
  end else begin
    if FindRecord then
      Result := raEdit else
      Result := raInsert;
  end;
end;

function TDBConvertItem.QuerySourceEOF: Boolean;
begin
  Result := False;
  if Assigned(FOnQuerySourceEOF) then
    FOnQuerySourceEOF(Self, Result) else
    Result := DefaultSourceEOF;
end;

function TDBConvertItem.RecordError(Action: TRecordAction; Error: Exception): TUpdateResult;
begin
  Result := urAbort;
  if Assigned(FOnRecordError) then FOnRecordError(Self, Action, Error, Result);
end;


function TDBConvertItem.GetConverter: TDBConverter;
begin
  Result := TDBConverter(ItemList);
end;

function TDBConvertItem.IsKeyFieldsStored: Boolean;
begin
  Result := AnsiCompareText(FKeyFields, DefKeyFields) <> 0;
end;

procedure TDBConvertItem.SetConverter(Value: TDBConverter);
begin
  ItemList := Value;
end;

procedure TDBConvertItem.IncLogLevel;
begin
  if Assigned(Converter) then Converter.IncLogLevel;
end;

procedure TDBConvertItem.DecLogLevel;
begin
  if Assigned(Converter) then Converter.DecLogLevel;
end;

procedure TDBConvertItem.WriteLog(Msg: String);
begin
  if Assigned(Converter) then Converter.WriteLog(Msg);
end;

procedure TDBConvertItem.BeginInit;
begin
  if Assigned(FOnBeginInit) then FOnBeginInit(Self);
end;

procedure TDBConvertItem.DoInit;
begin
  if Assigned(FOnInit) then FOnInit(Self) else DefaultInit;
end;

procedure TDBConvertItem.EndInit;
begin
  if Assigned(FOnEndInit) then FOnEndInit(Self);
end;

procedure TDBConvertItem.BeginRun;
begin
  if Assigned(FOnBeginRun) then FOnBeginRun(Self);
end;

procedure TDBConvertItem.DoRun;
begin
  if Assigned(FOnRun) then FOnRun(Self) else ProcessDataSet;
end;

procedure TDBConvertItem.EndRun;
begin
  if Assigned(FOnEndRun) then FOnEndRun(Self);
end;

procedure TDBConvertItem.BeginDone;
begin
  if Assigned(FOnBeginDone) then FOnBeginDone(Self);
end;

procedure TDBConvertItem.DoDone;
begin
  if Assigned(FOnDone) then FOnDone(Self) else DefaultDone;
end;

procedure TDBConvertItem.EndDone;
begin
  if Assigned(FOnEndDone) then FOnEndDone(Self);
end;

procedure TDBConvertItem.DefaultOpen;
begin
  if Assigned(FDataSets[0]) then FDataSets[0].Open;
  if Assigned(FDataSets[1]) then
  begin
    FDataSets[1].Open;
    ClearRequired(FDataSets[1]);
  end;
end;

procedure TDBConvertItem.DefaultClose;
begin
  if Assigned(FDataSets[0]) then FDataSets[0].Close;
  if Assigned(FDataSets[1]) then FDataSets[1].Close;
end;

procedure TDBConvertItem.BeginProcessDataSet;
begin
  IncLogLevel;
  if Assigned(FOnBeginProcessDataSet) then FOnBeginProcessDataSet(Self);
end;

procedure TDBConvertItem.EndProcessDataSet;
begin
  DecLogLevel;
  if Assigned(FOnEndProcessDataSet) then FOnEndProcessDataSet(Self);
end;

procedure TDBConvertItem.BeginProcessRecord;
begin
  IncLogLevel;
  if Assigned(FOnBeginProcessRecord) then FOnBeginProcessRecord(Self);
end;

procedure TDBConvertItem.EndProcessRecord;
begin
  DecLogLevel;
  if Assigned(FOnEndProcessRecord) then FOnEndProcessRecord(Self);
end;

procedure TDBConvertItem.BeginEditRecord;
begin
  if Assigned(FOnBeginEditRecord) then FOnBeginEditRecord(Self);
end;

procedure TDBConvertItem.EndEditRecord;
begin
  if Assigned(FOnEndEditRecord) then FOnEndEditRecord(Self);
end;

procedure TDBConvertItem.BeginCopyRecord;
begin
  if Assigned(FOnBeginCopyRecord) then FOnBeginCopyRecord(Self);
end;

procedure TDBConvertItem.EndCopyRecord;
begin
  if Assigned(FOnEndCopyRecord) then FOnEndCopyRecord(Self);
end;

procedure TDBConvertItem.BeginPostRecord;
begin
  if Assigned(FOnBeginPostRecord) then FOnBeginPostRecord(Self);
end;

procedure TDBConvertItem.EndPostRecord;
begin
  if Assigned(FOnEndPostRecord) then FOnEndPostRecord(Self);
end;

procedure TDBConvertItem.BeforeInit;
begin
  if Assigned(FOnBeforeInit) then FOnBeforeInit(Self);
end;

procedure TDBConvertItem.AfterInit;
begin
  if Assigned(FOnAfterInit) then FOnAfterInit(Self);
end;

procedure TDBConvertItem.BeforeRun;
begin
  if Assigned(FOnBeforeRun) then FOnBeforeRun(Self);
end;

procedure TDBConvertItem.AfterRun;
begin
  if Assigned(FOnAfterRun) then FOnAfterRun(Self);
end;

procedure TDBConvertItem.BeforeDone;
begin
  if Assigned(FOnBeforeDone) then FOnBeforeDone(Self);
end;

procedure TDBConvertItem.AfterDone;
begin
  if Assigned(FOnAfterDone) then FOnAfterDone(Self);
end;

procedure TDBConvertItem.BeforeProcessDataSet;
begin
  if Assigned(FOnBeforeProcessDataSet) then FOnBeforeProcessDataSet(Self);
end;

procedure TDBConvertItem.AfterProcessDataSet;
begin
  if Assigned(FOnAfterProcessDataSet) then FOnAfterProcessDataSet(Self);
end;

procedure TDBConvertItem.BeforeProcessRecord(Action: TRecordAction);
begin
  if Assigned(FOnBeforeProcessRecord) then FOnBeforeProcessRecord(Self, Action);
end;

procedure TDBConvertItem.AfterProcessRecord(Action: TRecordAction);
begin
  if Assigned(FOnAfterProcessRecord) then FOnAfterProcessRecord(Self, Action);
end;

procedure TDBConvertItem.BeforeEditRecord(Action: TRecordAction);
begin
  if Assigned(FOnBeforeEditRecord) then FOnBeforeEditRecord(Self, Action);
end;

procedure TDBConvertItem.AfterEditRecord(Action: TRecordAction);
begin
  if Assigned(FOnAfterEditRecord) then FOnAfterEditRecord(Self, Action);
end;

procedure TDBConvertItem.BeforeCopyRecord(Action: TRecordAction);
begin
  if Assigned(FOnBeforeCopyRecord) then FOnBeforeCopyRecord(Self, Action);
end;

procedure TDBConvertItem.AfterCopyRecord(Action: TRecordAction);
begin
  if Assigned(FOnAfterCopyRecord) then FOnAfterCopyRecord(Self, Action);
end;

procedure TDBConvertItem.BeforePostRecord(Action: TRecordAction);
begin
  if Assigned(FOnBeforePostRecord) then FOnBeforePostRecord(Self, Action);
end;

procedure TDBConvertItem.AfterPostRecord(Action: TRecordAction);
begin
  if Assigned(FOnAfterPostRecord) then FOnAfterPostRecord(Self, Action);
end;

{ TDBConverter }
constructor TDBConverter.Create(AOwner: TComponent);
begin
  inherited;
  FLogLevels := True;
  FLog := TStringList.Create;
  if not Registered then
  begin
    RegisterClasses([TDBConvertItem]);
    Registered := True;
  end;
end;

destructor TDBConverter.Destroy;
begin
  FLog.Free;
  inherited;
end;

procedure TDBConverter.InitItems;
var
  I: Integer;
  Item: TDBConvertItem;
begin
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if Item.Active then Item.Init;
  end;
end;

procedure TDBConverter.RunItems;
var
  I: Integer;
  Item: TDBConvertItem;
begin
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if Item.Active then Item.Run;
  end;
end;

procedure TDBConverter.DoneItems;
var
  I: Integer;
  Item: TDBConvertItem;
begin
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if Item.Active then Item.Done;
  end;
end;

procedure TDBConverter.ClearLog;
begin
  FLog.Clear;
  FLogLevel := 0;
end;

procedure TDBConverter.IncLogLevel;
begin
  Inc(FLogLevel);
end;

procedure TDBConverter.DecLogLevel;
begin
  Dec(FLogLevel);
end;

procedure TDBConverter.DoWriteLog(Msg: String);
begin
  if Assigned(FOnWriteLog) then FOnWriteLog(Self, Msg) else FLog.Add(Msg);
end;

procedure TDBConverter.WriteLog(Msg: String);
var
  F, S: String;
  I: Integer;
begin
  if FLogFormat <> lfNone then
  begin
    case FLogFormat of
      lfTime:
        F := 'hh:mm:ss';
      lfDateTime:
        F := 'dd.mm.yy hh:mm:ss';
    end;
    DateTimeToString(S, F, Now);
  end else
    S := '';


  if FLogLevels then
    for I := 1 to FLogLevel do S := #32#32 + S;

  if S <> '' then S := S + ' ' + Msg else S := Msg;
  DoWriteLog(S);
end;

function TDBConverter.GetItem(Index: Integer): TDBConvertItem;
begin
  Result := inherited Items[Index];
end;

procedure TDBConverter.SetLog(Value: TStrings);
begin
  FLog.Assign(Value);
end;

end.



