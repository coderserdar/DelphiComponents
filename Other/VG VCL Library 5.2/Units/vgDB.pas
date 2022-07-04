{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Data access components                        }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgDB;

interface
uses BDE, Classes, vgCtrls, DB, {$IFDEF _D3_}DBCtrls, vgI0_TLB, ComObj {$IFDEF _D5_}, DBCommon{$ENDIF}{$ELSE}DBTables{$ENDIF};

type
{ TvgFieldDataLink }
  TvgFieldDataLink = class(TFieldDataLink)
  private
    FOnFieldChanged: TNotifyEvent;
  protected
    procedure FieldChanged; virtual;
    procedure RecordChanged(Field: TField); override;
  public
    property OnFieldChanged: TNotifyEvent read FOnFieldChanged write FOnFieldChanged;
  end;

{ TFieldSource }
  TFieldIndexEvent = procedure (Sender: TObject; Field: TField; Index: Integer) of object;

  TFieldSource = class(TComponent)
  private
    { Private declarations }
    FDataField: string;
    FDataLinks: TList;
    FDataSource: TDataSource;
    FOnActiveChange, FOnDataChange, FOnFieldChanged: TNotifyEvent;
    FOnFieldIndexActiveChange, FOnFieldIndexDataChange, FOnFieldIndexFieldChanged: TFieldIndexEvent;
    function GetActive: Boolean;
    function GetField: TField;
    function GetFieldCount: Integer;
    function GetFields(Index: Integer): TField;
    procedure SetDataField(Value: string);
    procedure SetDataSource(Value: TDataSource);
  protected
    { Protected declarations }
    procedure DataLinkEvent(DataLink: TDataLink;
      FieldEvent: TNotifyEvent; FieldIndexEvent: TFieldIndexEvent);
    procedure ActiveChanged(Sender: TObject); virtual;
    procedure DataChanged(Sender: TObject); virtual;
    procedure FieldChanged(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Active: Boolean read GetActive;
    property Field: TField read GetField;
    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TField read GetFields; default;
  published
    { Published declarations }
    property DataField: string read FDataField write SetDataField;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnFieldChanged: TNotifyEvent read FOnFieldChanged write FOnFieldChanged;
    property OnFieldIndexActiveChange: TFieldIndexEvent read FOnFieldIndexActiveChange write FOnFieldIndexActiveChange;
    property OnFieldIndexDataChange: TFieldIndexEvent read FOnFieldIndexDataChange write FOnFieldIndexDataChange;
    property OnFieldIndexFieldChanged: TFieldIndexEvent read FOnFieldIndexFieldChanged write FOnFieldIndexFieldChanged;
  end;

{ TOpenTables }
  TOpenMode = (omNormal, omReference);

  TOpenTables = class;

  TOpenEntry  = class(TComponent)
  private
    FDataSet: TDataSet;
    FRefs: TList;
    FUseCount: array [TOpenMode] of  Integer;
    FOpenTables: TOpenTables;
    procedure SetOpenTables(Value: TOpenTables);
    procedure InsertRef(AOpenEntry: TOpenEntry);
    procedure RemoveRef(AOpenEntry: TOpenEntry);
  public
    destructor Destroy; override;
    property DataSet: TDataSet read FDataSet;
    property RefsCount: Integer read FUseCount[omReference];
    property UsedCount: Integer read FUseCount[omNormal];
    property OpenTables: TOpenTables read FOpenTables write SetOpenTables;
  end;

{TOpenTables}
  TOpenTables = class(TComponent)
  private
    { Private declarations }
    FEntryList: TList;
    FOnOpenDataSet, FOnCloseDataSet: TDataSetNotifyEvent;
    procedure OpenDataSet(DataSet: TDataSet; Mode: TOpenMode);
    procedure CloseDataSet(DataSet: TDataSet; Mode: TOpenMode);
    procedure InsertEntry(AOpenEntry: TOpenEntry);
    procedure RemoveEntry(AOpenEntry: TOpenEntry);
    function GetCount: Integer;
    function GetOpenEntry(Index: Integer): TOpenEntry;
    function GetEntry(DataSet: TDataSet): TOpenEntry;
  protected
    { Protected declarations }
    procedure DoOpenDataSet(DataSet: TDataSet); virtual;
    procedure DoCloseDataSet(DataSet: TDataSet); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    // Use in program directly
    procedure OpenDataSets(DataSets: array of TDataSet);
    procedure CloseDataSets(DataSets: array of TDataSet);
    // Use in BeforeOpen event handler. DataSets is array of Lookups f.e.
    procedure OpenRefsDataSets(DataSet: TDataset; DataSets: array of TDataSet);
    // Analitic helpers 
    function FindEntry(DataSet: TDataSet): TOpenEntry;
    property Count: Integer read GetCount;
    property OpenEntry[Index: Integer]: TOpenEntry read GetOpenEntry; default;
  published
    { Published declarations }
    property OnOpenDataSet: TDataSetNotifyEvent read FOnOpenDataSet write FOnOpenDataSet;
    property OnCloseDataSet: TDataSetNotifyEvent read FOnCloseDataSet write FOnCloseDataSet;
  end;

{ TvgDateTimeField }
  TDateTimeMode = (dtmBoth, dtmDate, dtmTime);

  TvgDateTimeField = class(TDateTimeField)
  private
    FMode: TDateTimeMode;
    procedure SetMode(Value: TDateTimeMode);
  protected
    function GetAsDateTime: TDateTime; override;
    function GetAsVariant: Variant; override;
    { Getting and setting values }
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    function GetValue(var Value: TDateTime): Boolean;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsString(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Mode: TDateTimeMode read FMode write SetMode default dtmBoth;
  end;

{ TvgDateField }
  TvgDateField = class(TvgDateTimeField)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Mode default dtmDate;
  end;

{ TvgTimeField }
  TvgTimeField = class(TvgDateTimeField)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Mode default dtmTime;
  end;

{ TCustomDataSetHook }
  TCustomDataSetHook = class(TCustomHook)
  private
    function GetDataSet: TDataSet;
    procedure SetDataSet(Value: TDataSet);
  protected
    property DataSet: TDataSet read GetDataSet write SetDataSet;
  end;

{ TDataSetHook }
  TDataSetHook = class(TCustomDataSetHook)
  private
    { Redefined events }
    FBeforeOpen: TDataSetNotifyEvent;
    FAfterOpen: TDataSetNotifyEvent;
    FBeforeClose: TDataSetNotifyEvent;
    FAfterClose: TDataSetNotifyEvent;
    FBeforeInsert: TDataSetNotifyEvent;
    FAfterInsert: TDataSetNotifyEvent;
    FBeforeEdit: TDataSetNotifyEvent;
    FAfterEdit: TDataSetNotifyEvent;
    FBeforePost: TDataSetNotifyEvent;
    FAfterPost: TDataSetNotifyEvent;
    FBeforeCancel: TDataSetNotifyEvent;
    FAfterCancel: TDataSetNotifyEvent;
    FBeforeDelete: TDataSetNotifyEvent;
    FAfterDelete: TDataSetNotifyEvent;
    FOnNewRecord: TDataSetNotifyEvent;
    FOnCalcFields: TDataSetNotifyEvent;
    FOnFilterRecord: TFilterRecordEvent;
    FOnEditError: TDataSetErrorEvent;
    FOnPostError: TDataSetErrorEvent;
    FOnDeleteError: TDataSetErrorEvent;
    { Saved events }
    FSaveBeforeOpen: TDataSetNotifyEvent;
    FSaveAfterOpen: TDataSetNotifyEvent;
    FSaveBeforeClose: TDataSetNotifyEvent;
    FSaveAfterClose: TDataSetNotifyEvent;
    FSaveBeforeInsert: TDataSetNotifyEvent;
    FSaveAfterInsert: TDataSetNotifyEvent;
    FSaveBeforeEdit: TDataSetNotifyEvent;
    FSaveAfterEdit: TDataSetNotifyEvent;
    FSaveBeforePost: TDataSetNotifyEvent;
    FSaveAfterPost: TDataSetNotifyEvent;
    FSaveBeforeCancel: TDataSetNotifyEvent;
    FSaveAfterCancel: TDataSetNotifyEvent;
    FSaveBeforeDelete: TDataSetNotifyEvent;
    FSaveAfterDelete: TDataSetNotifyEvent;
    FSaveOnNewRecord: TDataSetNotifyEvent;
    FSaveOnCalcFields: TDataSetNotifyEvent;
    FSaveOnFilterRecord: TFilterRecordEvent;
    FSaveOnEditError: TDataSetErrorEvent;
    FSaveOnPostError: TDataSetErrorEvent;
    FSaveOnDeleteError: TDataSetErrorEvent;
    { New handlers }
    procedure DoBeforeOpen(DataSet: TDataSet);
    procedure DoAfterOpen(DataSet: TDataSet);
    procedure DoBeforeClose(DataSet: TDataSet);
    procedure DoAfterClose(DataSet: TDataSet);
    procedure DoBeforeInsert(DataSet: TDataSet);
    procedure DoAfterInsert(DataSet: TDataSet);
    procedure DoBeforeEdit(DataSet: TDataSet);
    procedure DoAfterEdit(DataSet: TDataSet);
    procedure DoBeforePost(DataSet: TDataSet);
    procedure DoAfterPost(DataSet: TDataSet);
    procedure DoBeforeCancel(DataSet: TDataSet);
    procedure DoAfterCancel(DataSet: TDataSet);
    procedure DoBeforeDelete(DataSet: TDataSet);
    procedure DoAfterDelete(DataSet: TDataSet);
    procedure DoOnNewRecord(DataSet: TDataSet);
    procedure DoOnCalcFields(DataSet: TDataSet);
    procedure DoOnFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    procedure DoOnEditError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure DoOnPostError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure DoOnDeleteError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
  protected
    { Protected declarations }
    procedure HookObject; override;
    procedure UnHookObject; override;
  public
    { Public declarations }
    procedure AssignEventsTo(DataHook: TCustomDataSetHook); virtual;
  published
    { Published declarations }
    property Active;
    property DataSet;
    property BeforeOpen: TDataSetNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TDataSetNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeClose: TDataSetNotifyEvent read FBeforeClose write FBeforeClose;
    property AfterClose: TDataSetNotifyEvent read FAfterClose write FAfterClose;
    property BeforeInsert: TDataSetNotifyEvent read FBeforeInsert write FBeforeInsert;
    property AfterInsert: TDataSetNotifyEvent read FAfterInsert write FAfterInsert;
    property BeforeEdit: TDataSetNotifyEvent read FBeforeEdit write FBeforeEdit;
    property AfterEdit: TDataSetNotifyEvent read FAfterEdit write FAfterEdit;
    property BeforePost: TDataSetNotifyEvent read FBeforePost write FBeforePost;
    property AfterPost: TDataSetNotifyEvent read FAfterPost write FAfterPost;
    property BeforeCancel: TDataSetNotifyEvent read FBeforeCancel write FBeforeCancel;
    property AfterCancel: TDataSetNotifyEvent read FAfterCancel write FAfterCancel;
    property BeforeDelete: TDataSetNotifyEvent read FBeforeDelete write FBeforeDelete;
    property AfterDelete: TDataSetNotifyEvent read FAfterDelete write FAfterDelete;
    property OnNewRecord: TDataSetNotifyEvent read FOnNewRecord write FOnNewRecord;
    property OnCalcFields: TDataSetNotifyEvent read FOnCalcFields write FOnCalcFields;
    property OnFilterRecord: TFilterRecordEvent read FOnFilterRecord write FOnFilterRecord;
    property OnEditError: TDataSetErrorEvent read FOnEditError write FOnEditError;
    property OnPostError: TDataSetErrorEvent read FOnPostError write FOnPostError;
    property OnDeleteError: TDataSetErrorEvent read FOnDeleteError write FOnDeleteError;
  end;

{ TDBParam }
{$IFNDEF _D4_}
  TBlobData = string;
{$ENDIF}

  TDBParamType = (ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);

  TDBParams = class;

  TDBParam = class(TCollectionItem)
  private
    FParamRef: TDBParam;
    FData: Variant;
    FNull: Boolean;
    FName: string;
    FDataType: TFieldType;
    FBound: Boolean;
    FParamType: TDBParamType;
    procedure DataTypeUnknown;
    procedure DataTypeUnsupported;
    function ParamRef: TDBParam;
    function GetDataSet: TDataSet;
    function IsParamStored: Boolean;
    function GetDataType: TFieldType;
    function GetParamType: TDBParamType;
    procedure SetParamType(Value: TDBParamType);
    procedure ReadValue(Reader: TReader);
{$IFNDEF _D4_}
    procedure WriteValue(Writer: TWriter);
{$ENDIF}
  protected
    procedure AssignParam(Param: TDBParam);
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed;
    procedure DefineProperties(Filer: TFiler); override;
    function GetAsBCD: Currency;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double;
    function GetAsInteger: Longint;
    function GetAsMemo: string;
    function GetAsString: string;
    function GetAsVariant: Variant;
    function GetIsNull: Boolean;
    function IsEqual(Value: TDBParam): Boolean;
    procedure SetAsBCD(const Value: Currency);
    procedure SetAsBlob(const Value: TBlobData);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(Value: Longint);
    procedure SetAsMemo(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsSmallInt(Value: LongInt);
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsVariant(const Value: Variant);
    procedure SetAsWord(Value: LongInt);
    procedure SetDataType(Value: TFieldType);
    procedure SetText(const Value: string);
    function GetDisplayName: string; {$IFDEF _D3_} override; {$ENDIF}
    property DataSet: TDataSet read GetDataSet;
  public
    constructor Create(Collection: TCollection); override;
    constructor CreateNew(AParams: TDBParams; AParamType: TDBParamType);
    procedure Assign(Source: TPersistent); override;
    procedure AssignField(Field: TField);
    procedure AssignFieldValue(Field: TField; const Value: Variant);
    procedure Clear;
    procedure GetData(Buffer: Pointer);
    function GetDataSize: Integer;
    procedure LoadFromFile(const FileName: string; BlobType: TBlobType);
    procedure LoadFromStream(Stream: TStream; BlobType: TBlobType);
    procedure SetBlobData(Buffer: Pointer; Size: Integer);
    procedure SetData(Buffer: Pointer);
    property AsBCD: Currency read GetAsBCD write SetAsBCD;
    property AsBlob: TBlobData read GetAsString write SetAsBlob;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: LongInt read GetAsInteger write SetAsInteger;
    property AsSmallInt: LongInt read GetAsInteger write SetAsSmallInt;
    property AsMemo: string read GetAsMemo write SetAsMemo;
    property AsString: string read GetAsString write SetAsString;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsWord: LongInt read GetAsInteger write SetAsWord;
    property Bound: Boolean read FBound write FBound;
    property IsNull: Boolean read GetIsNull;
    property Text: string read GetAsString write SetText;
  published
    property DataType: TFieldType read GetDataType write SetDataType;
    property Name: string read FName write FName;
    property ParamType: TDBParamType read GetParamType write SetParamType;
    property Value: Variant read GetAsVariant write SetAsVariant stored IsParamStored;
  end;

{ TDBParams }
  TDBParams = class(TCollection)
  private
    FOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetDataSet: TDataSet;
    function GetParamValue(const ParamName: string): Variant;
    procedure SetParamValue(const ParamName: string;
      const Value: Variant);
    function GetItem(Index: Integer): TDBParam;
    procedure SetItem(Index: Integer; Value: TDBParam);
    procedure ReadBinaryData(Stream: TStream);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetOwner: TPersistent; {$IFDEF _D3_} override; {$ENDIF}
    procedure Update(Item: TCollectionItem); override;
  public
    constructor CreateOwned(Owner: TPersistent);
    procedure AssignValues(Value: TDBParams);
    { Create, AddParam, RemoveParam and CreateParam are in for backward compatibility }
    constructor Create;
    procedure AddParam(Value: TDBParam);
    procedure RemoveParam(Value: TDBParam);
    function CreateParam(FldType: TFieldType; const ParamName: string;
      ParamType: TDBParamType): TDBParam;
    procedure GetParamList(List: TList; const ParamNames: string);
    function IsEqual(Value: TDBParams): Boolean;
    function ParseSQLEx(SQL: string; Delimeter: Char; DoCreate: Boolean; const DefaultValue: Variant): string;
    function ParseSQL(SQL: string; Delimeter: Char; DoCreate: Boolean): string;
    function ParamByName(const Value: string): TDBParam;
    function FindParam(const Value: string): TDBParam;
    property DataSet: TDataSet read GetDataSet;
    property Items[Index: Integer]: TDBParam read GetItem write SetItem; default;
    property ParamValues[const ParamName: string]: Variant read GetParamValue write SetParamValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TUpdateObject }
  TUpdateObject = class(TComponent)
  private
    FParams: array [TUpdateKind, 0..1] of TDBParams;
    function GetMacros(UpdateKind: TUpdateKind): TDBParams;
    function GetParams(UpdateKind: TUpdateKind): TDBParams;
    function GetParamsIndex(Index: Integer): TDBParams;
    procedure SetMacros(UpdateKind: TUpdateKind; Value: TDBParams);
    procedure SetParams(UpdateKind: TUpdateKind; Value: TDBParams);
    procedure SetParamsIndex(Index: Integer; Value: TDBParams);
    function StoreParams(Index: Integer): Boolean;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetUpdateParams(DataSet: TDataSet; UpdateKind: TUpdateKind);
    property Params[UpdateKind: TUpdateKind]: TDBParams read GetParams write SetParams;
    property Macros[UpdateKind: TUpdateKind]: TDBParams read GetMacros write SetMacros;
  published
    property DeleteMacros: TDBParams index 5 read GetParamsIndex write SetParamsIndex stored StoreParams;
    property DeleteParams: TDBParams index 4 read GetParamsIndex write SetParamsIndex stored StoreParams;
    property InsertMacros: TDBParams index 3 read GetParamsIndex write SetParamsIndex stored StoreParams;
    property InsertParams: TDBParams index 2 read GetParamsIndex write SetParamsIndex stored StoreParams;
    property ModifyMacros: TDBParams index 1 read GetParamsIndex write SetParamsIndex stored StoreParams;
    property ModifyParams: TDBParams index 0 read GetParamsIndex write SetParamsIndex stored StoreParams;
  end;

const
  DefaultMacroChar  = '%';
  DefaultTermChar   = '/';

type
{ TUpdateSQLObject }
  TUpdateSQLObject = class(TUpdateObject)
  private
    FMacroChar: Char;
    FSQLText: array[TUpdateKind] of TStrings;
    procedure CreateParams(UpdateKind: TUpdateKind; Macro: Boolean);
    function GetSQL(UpdateKind: TUpdateKind): TStrings;
    function GetSQLIndex(Index: Integer): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    procedure SetSQLIndex(Index: Integer; Value: TStrings);
    procedure SQLTextChanged(Sender: TObject);
  protected
    procedure SetMacroChar(Value: Char); virtual;
    procedure SQLChanged(UpdateKind: TUpdateKind); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
  published
    property DeleteSQL: TStrings index 2 read GetSQLIndex write SetSQLIndex;
    property InsertSQL: TStrings index 1 read GetSQLIndex write SetSQLIndex;
    property ModifySQL: TStrings index 0 read GetSQLIndex write SetSQLIndex;
    property MacroChar: Char read FMacroChar write SetMacroChar default DefaultMacroChar;
  end;


{$IFDEF _D3_}
{ TDataSetObject }
  TDataSetObject = class(TAutoIntfObject, IvgDataSet)
  private
    FDataSet: TDataSet;
  protected
    function DoExecute: Integer; virtual;
    function DoGetParamValue(const ParamName: string): OleVariant; virtual;
    procedure DoSetParamValue(const ParamName: string; Value: OleVariant); virtual;
  public
    constructor Create(ADataSet: TDataSet; AParams: OleVariant);
    { IvgDataSet }
    procedure First; safecall;
    procedure Last; safecall;
    procedure MoveBy(Offset: Integer); safecall;
    function BOF: WordBool; safecall;
    function EOF: WordBool; safecall;
    procedure Open; safecall;
    procedure Close; safecall;
    function Execute: Integer; safecall;
    function Locate(const FieldNames: WideString; FieldValues: OleVariant;
      CaseIns, PartialKey: WordBool): WordBool; safecall;
    procedure Insert; safecall;
    procedure Append; safecall;
    procedure Edit; safecall;
    procedure Post; safecall;
    procedure Cancel; safecall;
    procedure Delete; safecall;
    function Get_Active: WordBool; safecall;
    procedure Set_Active(Value: WordBool); safecall;
    function Get_Empty: WordBool; safecall;
    function GetFieldValue(const FieldName: WideString): OleVariant; safecall;
    procedure SetFieldValue(const FieldName: WideString; Value: OleVariant); safecall;
    function GetParamValue(const ParamName: WideString): OleVariant; safecall;
    procedure SetParamValue(const ParamName: WideString; Value: OleVariant); safecall;
  public
    property DataSet: TDataSet read FDataSet;
  end;
{$ENDIF}

{$IFNDEF _D4_}
  TUpdateMode = (upWhereAll, upWhereChanged, upWhereKeyOnly);
{$ENDIF}

{$IFNDEF _D5_}
  TFieldMap = array[TFieldType] of Byte;
{$ENDIF}

  TFieldTypes = set of TFieldType;

const
  StringFieldTypes: TFieldTypes = [ftString{$IFDEF _D4_}, ftFixedChar, ftWideString {$IFDEF _D5_}, ftGuid {$ENDIF}{$ENDIF}];
  BlobFieldTypes: TFieldTypes = [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle,
    ftTypedBinary{$IFDEF _D5_}, ftOraBlob, ftOraClob{$ENDIF}];

  FieldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL,
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES,
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB,
    fldBLOB, fldBLOB
{$IFDEF _D3_}
    , fldCURSOR
  {$IFDEF _D4_}
    , fldZSTRING, fldZSTRING, fldINT64, fldADT, fldArray, fldREF, fldTABLE
    {$IFDEF _D5_}
    , fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN, fldUNKNOWN, fldZSTRING
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
    );

implementation
uses vgVCLRes, Consts, DBConsts, {$IFDEF _D3_}BDEConst,{$ENDIF}
  SysUtils , vgUtils, vgDBPrms, vgDBUtl, Graphics, Forms;

{ TvgFieldDataLink }
procedure TvgFieldDataLink.FieldChanged;
begin
  if Assigned(FOnFieldChanged) then FOnFieldChanged(Self);
end;

procedure TvgFieldDataLink.RecordChanged(Field: TField);
begin
  inherited;
  if Assigned(Field) and (Field = Self.Field) then FieldChanged;
end;

constructor TFieldSource.Create(AOwner: TComponent);
begin
  inherited;
  FDataLinks := TList.Create;
end;

destructor TFieldSource.Destroy;
begin
  Destroying;
  SetDataField('');
  SetDataSource(nil);
  FDataLinks.Free;
  inherited;
end;

procedure TFieldSource.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = DataSource) then
    SetDataSource(nil);
end;

function TFieldSource.GetActive: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FDataLinks.Count - 1 do
    if not TDataLink(FDataLinks[I]).Active then Exit;
  Result := True;
end;

function TFieldSource.GetField: TField;
begin
  if FDataLinks.Count > 0 then
    Result := TFieldDataLink(FDataLinks[0]).Field else
    Result := nil;
end;

function TFieldSource.GetFieldCount: Integer;
begin
  Result := FDataLinks.Count;
end;

function TFieldSource.GetFields(Index: Integer): TField;
begin
  Result := TFieldDataLink(FDataLinks[Index]).Field;
end;

procedure TFieldSource.SetDataField(Value: string);
var
  I: Integer;
  List, OldList: TList;
  S: string;
  DataLink: TDataLink;
begin
  if (AnsiCompareText(FDataField, Value) <> 0) then
  begin
    List := TList.Create;
    OldList := List;
    try
      I := 1;
      while I <= Length(Value) do
      begin
        S := ExtractFieldName(Value, I);
        DataLink := TvgFieldDataLink.Create;
        with TvgFieldDataLink(DataLink) do
        begin
          FieldName := S;
          DataSource := Self.DataSource;
          OnActiveChange := Self.ActiveChanged;
          OnDataChange := Self.DataChanged;
          OnFieldChanged := Self.FieldChanged;
        end;
        List.Add(DataLink);
      end;
      OldList := FDataLinks;
      FDataLinks := List;
      FDataField := Value;
    finally
      for I := 0 to OldList.Count - 1 do
      begin
        DataLink := OldList[I];
        DataLink.Free;
      end;
      OldList.Free;
    end;
  end;
end;

procedure TFieldSource.SetDataSource(Value: TDataSource);
var
  SaveDataField: string;
  SaveDataSource: TDataSource;
begin
  if (FDataSource <> Value) then
  begin
    if Assigned(Value) then FreeNotification(Value);
    SaveDataSource := FDataSource;
    try
      SaveDataField := FDataField;
      SetDataField('');
      FDataSource := Value;
      SetDataField(SaveDataField);
    except
      Application.HandleException(Self);
      FDataSource := SaveDataSource;
      SetDataField(SaveDataField);
    end;
  end;
end;

procedure TFieldSource.DataLinkEvent(DataLink: TDataLink;
  FieldEvent: TNotifyEvent; FieldIndexEvent: TFieldIndexEvent);
var
  I: Integer;
  Field: TField;
begin
  if not (csDestroying in ComponentState) then
  begin
    I := FDataLinks.IndexOf(DataLink);
    if (I >= 0) then
    begin
      Field := GetFields(I);
      if (Field <> nil) then
      begin
        if Assigned(FieldEvent) then FieldEvent(Self);
        if Assigned(FieldIndexEvent) then FieldIndexEvent(Self, Field, I);
      end;
    end;
  end;
end;

procedure TFieldSource.ActiveChanged(Sender: TObject);
begin
  DataLinkEvent(TDataLink(Sender), FOnActiveChange, FOnFieldIndexActiveChange);
end;

procedure TFieldSource.DataChanged(Sender: TObject);
begin
  DataLinkEvent(TDataLink(Sender), FOnDataChange, FOnFieldIndexDataChange);
end;

procedure TFieldSource.FieldChanged(Sender: TObject);
begin
  DataLinkEvent(TDataLink(Sender), FOnFieldChanged, FOnFieldIndexFieldChanged);
end;

{ TOpenTables }
destructor TOpenEntry.Destroy;
begin
  SetOpenTables(nil);
  FRefs.Free;
  inherited;
end;

procedure TOpenEntry.InsertRef(AOpenEntry: TOpenEntry);
begin
  if (FRefs = nil) then FRefs := TList.Create;
  FRefs.Add(AOpenEntry);
end;

procedure TOpenEntry.RemoveRef(AOpenEntry: TOpenEntry);
begin
  if Assigned(FRefs) then
  begin
    FRefs.Remove(AOpenEntry);
    if (FRefs.Count = 0) then
    begin
      FRefs.Free;
      FRefs := nil;
    end;
  end;
end;

procedure TOpenEntry.SetOpenTables(Value: TOpenTables);
begin
  if (FOpenTables <> Value) then
  begin
    if Assigned(FOpenTables) then FOpenTables.RemoveEntry(Self);
    if Assigned(Value) then Value.InsertEntry(Self);
  end;
end;

constructor TOpenTables.Create(AOwner: Tcomponent);
begin
  inherited;
  FEntryList := TList.Create;
end;

destructor TOpenTables.Destroy;
begin
  while FEntryList.Count > 0 do
    TOpenEntry(FEntryList.Last).Free;
  FEntryList.Free;
  inherited;
end;

procedure TOpenTables.Notification(AComponent: TComponent; Operation: TOperation);
var
  Entry: TOpenEntry;
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TDataSet) then
  begin
    Entry := FindEntry(TDataSet(AComponent));
    if Assigned(Entry) then RemoveEntry(Entry);
  end;
end;

procedure TOpenTables.DoOpenDataSet(DataSet: TDataSet);
begin
  if Assigned(FOnOpenDataSet) then
    FOnOpenDataSet(DataSet)
  else
    DataSet.Open;
end;

procedure TOpenTables.DoCloseDataSet(DataSet: TDataSet);
begin
  if Assigned(FOnCloseDataSet) then
    FOnCloseDataSet(DataSet)
  else
    DataSet.Close;
end;

procedure TOpenTables.InsertEntry(AOpenEntry: TOpenEntry);
var
  I: Integer;
begin
  I := FEntryList.IndexOf(AOpenEntry);
  if (I < 0) then
  begin
    FEntryList.Add(AOpenEntry);
    AOpenEntry.FOpenTables := Self;
  end;
end;

procedure TOpenTables.RemoveEntry(AOpenEntry: TOpenEntry);
var
  I: Integer;
  Entry: TOpenEntry;
begin
  I := FEntryList.IndexOf(AOpenEntry);
  if (I >= 0) then
  begin
    // Remove references on AOpenEntry
    FEntryList.Remove(AOpenEntry);
    AOpenEntry.FOpenTables := nil;

    for I := FEntryList.Count - 1 downto 0 do
    begin
      Entry := FEntryList[I];
      Entry.RemoveRef(AOpenEntry);
    end;

    // Remove AOpenEntry's references
    while Assigned(AOpenEntry.FRefs) do
    begin
      Entry := AOpenEntry.FRefs.Last;
      AOpenEntry.RemoveRef(Entry);
      CloseDataSet(Entry.DataSet, omReference);
    end;
  end;
end;

function TOpenTables.FindEntry(DataSet: TDataSet): TOpenEntry;
var
  I: Integer;
  Entry: TOpenEntry;
begin
  Result := nil;
  for I := 0 to FEntryList.Count - 1 do
  begin
    Entry := TOpenEntry(FEntryList[I]);
    if Entry.DataSet = DataSet then
    begin
      Result := Entry;
      Break;
    end;
  end;
end;

function TOpenTables.GetEntry(DataSet: TDataSet): TOpenEntry;
begin
  Result := FindEntry(DataSet);
  if not Assigned(Result) then
  begin
    Result := TOpenEntry.Create(Self);
    Result.FDataSet := DataSet;
    FreeNotification(DataSet);
    InsertEntry(Result);
  end;
end;

procedure TOpenTables.OpenDataSet(DataSet: TDataSet; Mode: TOpenMode);
var
  Entry: TOpenEntry;
begin
  Entry := FindEntry(DataSet);
  if not Assigned(Entry) then
  begin
    Entry := GetEntry(DataSet);
    DoOpenDataSet(DataSet);
  end;
  Inc(Entry.FUseCount[Mode]);
end;

procedure TOpenTables.CloseDataSet(DataSet: TDataSet; Mode: TOpenMode);
var
  Entry: TOpenEntry;
begin
  Entry := FindEntry(DataSet);
  if Assigned(Entry) then
  begin
    Dec(Entry.FUseCount[Mode]);
    if ((Entry.FUseCount[omNormal] <= 0) and (Entry.FUseCount[omReference] <= 0)) then
    begin
      Entry.Free;
      DoCloseDataSet(DataSet);
    end;
  end;
end;

procedure TOpenTables.OpenDataSets(DataSets: array of TDataSet);
var
  I: Integer;
  FOpenList: TList;
begin
  FOpenList := TList.Create;
  try
    try
      for I := 0 to High(DataSets) do
      begin
        FOpenList.Add(DataSets[I]);
        OpenDataSet(DataSets[I], omNormal);
      end;
    except
      for I := 0 to FOpenList.Count - 1 do
        CloseDataSet(FOpenList[I], omNormal);
      raise;
    end;
  finally
    FOpenList.Free;
  end;
end;

procedure TOpenTables.OpenRefsDataSets(DataSet: TDataset; DataSets: array of TDataSet);
var
  I: Integer;
  FOpenList: TList;
  Entry: TOpenEntry;
begin
  if not Assigned(DataSets[0]) then Exit;

  Entry := GetEntry(DataSet);
  FOpenList := TList.Create;
  try
    try
      for I := 0 to High(DataSets) do
      begin
        FOpenList.Add(DataSets[I]);
        OpenDataSet(DataSets[I], omReference)
      end;
    except
      for I := 0 to FOpenList.Count - 1 do
        CloseDataSet(FOpenList[I], omReference);
      raise;
    end;
  finally
    FOpenList.Free;
  end;

  for I := 0 to High(DataSets) do
    Entry.InsertRef(FindEntry(DataSets[I]));
end;

procedure TOpenTables.CloseDataSets(DataSets: array of TDataSet);
var
  I: Integer;
begin
  for I := 0 to High(DataSets) do
    CloseDataSet(DataSets[I], omNormal);
end;

function TOpenTables.GetCount: Integer;
begin
  Result := FEntryList.Count;
end;

function TOpenTables.GetOpenEntry(Index: Integer): TOpenEntry;
begin
  Result := FEntryList[Index];
end;

{ TvgDateTimeField }
type
  TDateTimeRec = record
    case TFieldType of
      ftDate: (Date: Longint);
      ftTime: (Time: Longint);
      ftDateTime: (DateTime: TDateTime);
  end;

constructor TvgDateTimeField.Create(AOwner: TComponent);
begin
  inherited;
  FMode := dtmBoth;
end;

procedure TvgDateTimeField.SetMode(Value: TDateTimeMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    PropertyChanged(True);
  end;
end;

function TvgDateTimeField.GetAsDateTime: TDateTime;
begin
  case FMode of
    dtmBoth: Result := inherited GetAsDateTime;
  else
    if not GetValue(Result) then Result := 0;
  end;
end;

function TvgDateTimeField.GetAsVariant: Variant;
var
  D: TDateTime;
begin
  case FMode of
    dtmBoth: Result := inherited GetAsVariant;
  else
    if GetValue(D) then Result := VarFromDateTime(D) else Result := Null;
  end;
end;

procedure TvgDateTimeField.GetText(var Text: string; DisplayText: Boolean);
var
  F: string;
  D: TDateTime;
begin
  if GetValue(D) then
  begin
    if FMode = dtmDate then
      F := ShortDateFormat else
      F := LongTimeFormat;
    DateTimeToString(Text, F, D);
  end else
    Text := '';
end;

function TvgDateTimeField.GetValue(var Value: TDateTime): Boolean;
var
  TimeStamp: TTimeStamp;
  Data: TDateTimeRec;
begin
  Result := GetData(@Data);
  if Result then
  begin
    try
      TimeStamp := MSecsToTimeStamp(Data.DateTime);
    except
      TimeStamp.Time := 0;
      TimeStamp.Date := 0;
    end;
    if FMode = dtmDate then
      Value := Trunc(TimeStampToDateTime(TimeStamp)) else
      Value := Frac(TimeStampToDateTime(TimeStamp));
  end;
end;

procedure TvgDateTimeField.SetAsDateTime(Value: TDateTime);
begin
  case FMode of
    dtmBoth: inherited SetAsDateTime(Value);
    dtmDate: inherited SetAsDateTime(Trunc(Value));
    dtmTime: inherited SetAsDateTime(Frac(Value));
  end;
end;

procedure TvgDateTimeField.SetAsString(const Value: string);
var
  DateTime: TDateTime;
begin
  if Value = '' then Clear else
  begin
    DateTime := 0;
    try
      case FMode of
        dtmBoth: DateTime := StrToDateTime(Value);
        dtmDate: DateTime := StrToDate(Value);
        dtmTime: DateTime := StrToTime(Value);
      end;
    except
      if FMode <> dtmBoth then
        DateTime := StrToDateTime(Value);
    end;
    SetAsDateTime(DateTime);
  end;
end;

{ TvgDateField }
constructor TvgDateField.Create(AOwner: TComponent);
begin
  inherited;
  Mode := dtmDate;
end;

{ TvgTimeField }
constructor TvgTimeField.Create(AOwner: TComponent);
begin
  inherited;
  Mode := dtmTime;
end;

{ TCustomDataSet }
function TCustomDataSetHook.GetDataSet: TDataSet;
begin
  Result := TDataSet(HookedObject);
end;

procedure TCustomDataSetHook.SetDataSet(Value: TDataSet);
begin
  if HookedObject <> Value then
  begin
    if IsObjectHooked(Value) then Exit;
    HookedObject := Value;
  end;
end;

{ TDataSetHook }
procedure TDataSetHook.HookObject;
begin
  if (csDesigning in ComponentState) then Exit;

  with DataSet do
  begin
    { Saving }
    FSaveBeforeOpen := BeforeOpen;
    FSaveAfterOpen := AfterOpen;
    FSaveBeforeClose := BeforeClose;
    FSaveAfterClose := AfterClose;
    FSaveBeforeInsert := BeforeInsert;
    FSaveAfterInsert := AfterInsert;
    FSaveBeforeEdit := BeforeEdit;
    FSaveAfterEdit := AfterEdit;
    FSaveBeforePost := BeforePost;
    FSaveAfterPost := AfterPost;
    FSaveBeforeCancel := BeforeCancel;
    FSaveAfterCancel := AfterCancel;
    FSaveBeforeDelete := BeforeDelete;
    FSaveAfterDelete := AfterDelete;
    FSaveOnNewRecord := OnNewRecord;
    FSaveOnCalcFields := OnCalcFields;
    FSaveOnFilterRecord := OnFilterRecord;
    FSaveOnEditError := OnEditError;
    FSaveOnPostError := OnPostError;
    FSaveOnDeleteError := OnDeleteError;

    { Setting hooks }
    BeforeOpen := DoBeforeOpen;
    AfterOpen := DoAfterOpen;
    BeforeClose := DoBeforeClose;
    AfterClose := DoAfterClose;
    BeforeInsert := DoBeforeInsert;
    AfterInsert := DoAfterInsert;
    BeforeEdit := DoBeforeEdit;
    AfterEdit := DoAfterEdit;
    BeforePost := DoBeforePost;
    AfterPost := DoAfterPost;
    BeforeCancel := DoBeforeCancel;
    AfterCancel := DoAfterCancel;
    BeforeDelete := DoBeforeDelete;
    AfterDelete := DoAfterDelete;
    OnNewRecord := DoOnNewRecord;
    OnCalcFields := DoOnCalcFields;
    OnFilterRecord := DoOnFilterRecord;
    OnEditError := DoOnEditError;
    OnPostError := DoOnPostError;
    OnDeleteError := DoOnDeleteError;
  end;
end;

procedure TDataSetHook.UnHookObject;
begin
  if (csDesigning in ComponentState) then Exit;

  with DataSet do
  begin
    { Restoring }
    BeforeOpen := FSaveBeforeOpen;
    AfterOpen := FSaveAfterOpen;
    BeforeClose := FSaveBeforeClose;
    AfterClose := FSaveAfterClose;
    BeforeInsert := FSaveBeforeInsert;
    AfterInsert := FSaveAfterInsert;
    BeforeEdit := FSaveBeforeEdit;
    AfterEdit := FSaveAfterEdit;
    BeforePost := FSaveBeforePost;
    AfterPost := FSaveAfterPost;
    BeforeCancel := FSaveBeforeCancel;
    AfterCancel := FSaveAfterCancel;
    BeforeDelete := FSaveBeforeDelete;
    AfterDelete := FSaveAfterDelete;
    OnNewRecord := FSaveOnNewRecord;
    OnCalcFields := FSaveOnCalcFields;
    OnFilterRecord := FSaveOnFilterRecord;
    OnEditError := FSaveOnEditError;
    OnPostError := FSaveOnPostError;
    OnDeleteError := FSaveOnDeleteError;
  end;
end;

procedure TDataSetHook.AssignEventsTo(DataHook: TCustomDataSetHook);
begin
  with DataHook as TDataSetHook do
  begin
    Self.FBeforeOpen := BeforeOpen;
    Self.FAfterOpen := AfterOpen;
    Self.FBeforeClose := BeforeClose;
    Self.FAfterClose := AfterClose;
    Self.FBeforeInsert := BeforeInsert;
    Self.FAfterInsert := AfterInsert;
    Self.FBeforeEdit := BeforeEdit;
    Self.FAfterEdit := AfterEdit;
    Self.FBeforePost := BeforePost;
    Self.FAfterPost := AfterPost;
    Self.FBeforeCancel := BeforeCancel;
    Self.FAfterCancel := AfterCancel;
    Self.FBeforeDelete := BeforeDelete;
    Self.FAfterDelete := AfterDelete;
    Self.FOnNewRecord := OnNewRecord;
    Self.FOnCalcFields := OnCalcFields;
    Self.FOnFilterRecord := OnFilterRecord;
    Self.FOnEditError := OnEditError;
    Self.FOnPostError := OnPostError;
    Self.FOnDeleteError := OnDeleteError;
  end;
end;

procedure TDataSetHook.DoBeforeOpen(DataSet: TDataSet);
begin
  if Assigned(FBeforeOpen) then FBeforeOpen(DataSet);
end;

procedure TDataSetHook.DoAfterOpen(DataSet: TDataSet);
begin
  if Assigned(FAfterOpen) then FAfterOpen(DataSet);
end;

procedure TDataSetHook.DoBeforeClose(DataSet: TDataSet);
begin
  if Assigned(FBeforeClose) then FBeforeClose(DataSet);
end;

procedure TDataSetHook.DoAfterClose(DataSet: TDataSet);
begin
  if Assigned(FAfterClose) then FAfterClose(DataSet);
end;

procedure TDataSetHook.DoBeforeInsert(DataSet: TDataSet);
begin
  if Assigned(FBeforeInsert) then FBeforeInsert(DataSet);
end;

procedure TDataSetHook.DoAfterInsert(DataSet: TDataSet);
begin
  if Assigned(FAfterInsert) then FAfterInsert(DataSet);
end;

procedure TDataSetHook.DoBeforeEdit(DataSet: TDataSet);
begin
  if Assigned(FBeforeEdit) then FBeforeEdit(DataSet);
end;

procedure TDataSetHook.DoAfterEdit(DataSet: TDataSet);
begin
  if Assigned(FAfterEdit) then FAfterEdit(DataSet);
end;

procedure TDataSetHook.DoBeforePost(DataSet: TDataSet);
begin
  if Assigned(FBeforePost) then FBeforePost(DataSet);
end;

procedure TDataSetHook.DoAfterPost(DataSet: TDataSet);
begin
  if Assigned(FAfterPost) then FAfterPost(DataSet);
end;

procedure TDataSetHook.DoBeforeCancel(DataSet: TDataSet);
begin
  if Assigned(FBeforeCancel) then FBeforeCancel(DataSet);
end;

procedure TDataSetHook.DoAfterCancel(DataSet: TDataSet);
begin
  if Assigned(FAfterCancel) then FAfterCancel(DataSet);
end;

procedure TDataSetHook.DoBeforeDelete(DataSet: TDataSet);
begin
  if Assigned(FBeforeDelete) then FBeforeDelete(DataSet);
end;

procedure TDataSetHook.DoAfterDelete(DataSet: TDataSet);
begin
  if Assigned(FAfterDelete) then FAfterDelete(DataSet);
end;

procedure TDataSetHook.DoOnNewRecord(DataSet: TDataSet);
begin
  if Assigned(FOnNewRecord) then FOnNewRecord(DataSet);
end;

procedure TDataSetHook.DoOnCalcFields(DataSet: TDataSet);
begin
  if Assigned(FOnCalcFields) then FOnCalcFields(DataSet);
end;

procedure TDataSetHook.DoOnFilterRecord(DataSet: TDataSet; var Accept: Boolean);
begin
  if Assigned(FOnFilterRecord) then FOnFilterRecord(DataSet, Accept)
end;

procedure TDataSetHook.DoOnEditError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  if Assigned(FOnEditError) then FOnEditError(DataSet, E, Action);
end;

procedure TDataSetHook.DoOnPostError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  if Assigned(FOnPostError) then FOnPostError(DataSet, E, Action);
end;

procedure TDataSetHook.DoOnDeleteError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  if Assigned(FOnDeleteError) then FOnDeleteError(DataSet, E, Action);
end;

{ TDBParams }
constructor TDBParams.Create;
begin
  FOwner := nil;
  inherited Create(TDBParam);
end;

constructor TDBParams.CreateOwned(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(TDBParam);
end;

procedure TDBParams.ReadBinaryData(Stream: TStream);
var
  I, Temp, NumItems: Integer;
  Buffer: array[0..2047] of Char;
  TempStr: string;
  Version: Word;
  Bool: Boolean;
begin
  Clear;
  with Stream do
  begin
    ReadBuffer(Version, SizeOf(Version));
    if Version > 2 then DatabaseError(ResStr(SInvalidVersion));
    NumItems := 0;
    if Version = 2 then
      ReadBuffer(NumItems, SizeOf(NumItems)) else
      ReadBuffer(NumItems, 2);
    for I := 0 to NumItems - 1 do
      with TDBParam(Add) do
      begin
        Temp := 0;
        if Version = 2 then
          ReadBuffer(Temp, SizeOf(Temp)) else
          ReadBuffer(Temp, 1);
        SetLength(TempStr, Temp);
        ReadBuffer(PChar(TempStr)^, Temp);
        Name := TempStr;
        ReadBuffer(FParamType, SizeOf(FParamType));
        ReadBuffer(FDataType, SizeOf(FDataType));
        if DataType <> ftUnknown then
        begin
          Temp := 0;
          if Version = 2 then
            ReadBuffer(Temp, SizeOf(Temp)) else
            ReadBuffer(Temp, 2);
          ReadBuffer(Buffer, Temp);
          if DataType in [ftBlob, ftGraphic..ftTypedBinary] then
            SetBlobData(@Buffer, Temp) else
            SetData(@Buffer);
        end;
        ReadBuffer(Bool, SizeOf(Bool));
        if Bool then FData := NULL;
        ReadBuffer(FBound, SizeOf(FBound));
      end;
  end;
end;

procedure TDBParams.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadBinaryData, nil, False);
end;

procedure TDBParams.Update(Item: TCollectionItem);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].FParamRef := nil;
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TDBParams.GetItem(Index: Integer): TDBParam;
begin
  Result := TDBParam(inherited Items[Index]);
  Result := Result.ParamRef;
end;

procedure TDBParams.SetItem(Index: Integer; Value: TDBParam);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

function TDBParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TDBParams.GetDataSet: TDataSet;
begin
  if FOwner is TDataSet then
    Result := TDataSet(FOwner) else
    Result := nil;
end;

procedure TDBParams.AssignTo(Dest: TPersistent);
begin
  if Dest is TDBParams then TDBParams(Dest).Assign(Self)
  else inherited AssignTo(Dest);
end;

procedure TDBParams.AssignValues(Value: TDBParams);
var
  I: Integer;
  P: TDBParam;
begin
  for I := 0 to Value.Count - 1 do
  begin
    P := FindParam(Value[I].Name);
    if P <> nil then
      P.Assign(Value[I]);
  end;
end;

procedure TDBParams.AddParam(Value: TDBParam);
begin
  Value.Collection := Self;
end;

procedure TDBParams.RemoveParam(Value: TDBParam);
begin
  Value.Collection := nil;
end;

function TDBParams.CreateParam(FldType: TFieldType; const ParamName: string;
  ParamType: TDBParamType): TDBParam;
begin
  Result := Add as TDBParam;
  Result.ParamType := ParamType;
  Result.Name := ParamName;
  Result.DataType :=  FldType;
end;

function TDBParams.IsEqual(Value: TDBParams): Boolean;
var
  I: Integer;
begin
  Result := Count = Value.Count;
  if Result then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].IsEqual(Value.Items[I]);
      if not Result then Break;
    end
end;

function TDBParams.ParamByName(const Value: string): TDBParam;
begin
  Result := FindParam(Value);
  if Result = nil then
{$IFDEF _D3_}
    DatabaseErrorFmt(SParameterNotFound, [Value]{$IFDEF _D4_}, DataSet{$ENDIF});
{$ELSE}
    DBErrorFmt(SParameterNotFound, [Value]);
{$ENDIF}
end;

function TDBParams.FindParam(const Value: string): TDBParam;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TDBParam(inherited Items[I]);
    if AnsiCompareText(Result.Name, Value) = 0 then Exit;
  end;
  Result := nil;
end;

function TDBParams.GetParamValue(const ParamName: string): Variant;
var
  I: Integer;
  Params: TList;
begin
  if Pos(';', ParamName) <> 0 then
  begin
    Params := TList.Create;
    try
      GetParamList(Params, ParamName);
      Result := VarArrayCreate([0, Params.Count - 1], varVariant);
      for I := 0 to Params.Count - 1 do
        Result[I] := TDBParam(Params[I]).Value;
    finally
      Params.Free;
    end;
  end else
    Result := ParamByName(ParamName).Value
end;

procedure TDBParams.SetParamValue(const ParamName: string;
  const Value: Variant);
var
  I: Integer;
  Params: TList;
begin
  if Pos(';', ParamName) <> 0 then
  begin
    Params := TList.Create;
    try
      GetParamList(Params, ParamName);
      for I := 0 to Params.Count - 1 do
        TDBParam(Params[I]).Value := Value[I];
    finally
      Params.Free;
    end;
  end else
    ParamByName(ParamName).Value := Value;
end;

procedure TDBParams.GetParamList(List: TList; const ParamNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= Length(ParamNames) do
    List.Add(ParamByName(ExtractFieldName(ParamNames, Pos)));
end;

function TDBParams.ParseSQLEx(SQL: string; Delimeter: Char; DoCreate: Boolean;
  const DefaultValue: Variant): string;
var
  Value, CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;
  Param: TDBParam;
begin
  Result := SQL;
  Value := PChar(Result);
  if DoCreate then Clear;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
{$IFDEF _D3_}
    while (CurPos^ in LeadBytes) do Inc(CurPos, 2);
{$ENDIF}
    CurChar := CurPos^;
    if (CurChar = Delimeter) and not Literal and ((CurPos + 1)^ <> Delimeter) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter(CurChar, [])) do
      begin
        Inc(CurPos);
{$IFDEF _D3_}
        while (CurPos^ in LeadBytes) do Inc(CurPos, 2);
{$ENDIF}
        CurChar := CurPos^;
        if IsLiteral(CurChar) then
        begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then
      begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end else
        Name := StrPas(StartPos + 1);
      if DoCreate then
      begin
        Param := TDBParam(Add);
        Param.Name := Name;
        if not VarIsNull(DefaultValue) then
          Param.Value := DefaultValue;
      end;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end else if (CurChar = Delimeter) and not Literal and ((CurPos + 1)^ = Delimeter) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral(CurChar) then Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

function TDBParams.ParseSQL(SQL: string; Delimeter: Char; DoCreate: Boolean): string;
begin
  Result := ParseSQLEx(SQL, Delimeter, DoCreate, Null);
end;

{ TDBParam }
constructor TDBParam.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  ParamType := ptUnknown;
  DataType := ftUnknown;
  FData := Unassigned;
  FBound := False;
  FNull := True;
end;

constructor TDBParam.CreateNew(AParams: TDBParams; AParamType: TDBParamType);
begin
  Create(AParams);
  ParamType := ParamType;
end;

procedure TDBParam.DataTypeUnknown;
begin
{$IFDEF _D3_}
  {$IFDEF _D4_}
  DatabaseErrorFmt(SUnknownFieldType, [Name], DataSet);
  {$ELSE}
  DatabaseErrorFmt(SFieldUndefinedType, [Name]);
  {$ENDIF}
{$ELSE}
  DBErrorFmt(SFieldUndefinedType, [Name]);
{$ENDIF}
end;

procedure TDBParam.DataTypeUnsupported;
begin
{$IFDEF _D3_}
  {$IFDEF _D4_}
    {$IFDEF _D5_}
    DatabaseErrorFmt(SBadFieldType, [Name], DataSet);
    {$ELSE}
    DatabaseErrorFmt(SParamBadFieldType, [Name], DataSet);
    {$ENDIF}
  {$ELSE}
    DatabaseErrorFmt(SFieldUnsupportedType, [Name]);
  {$ENDIF}
{$ELSE}
  DBErrorFmt(SFieldUnsupportedType, [Name]);
{$ENDIF}
end;

function TDBParam.IsEqual(Value: TDBParam): Boolean;
begin
  Result := (VarType(FData) = VarType(Value.FData)) and
    (FData = Value.FData) and (Name = Value.Name) and
    (DataType = Value.DataType) and (IsNull = Value.IsNull) and
    (Bound = Value.Bound) and (ParamType = Value.ParamType);
end;

function TDBParam.IsParamStored: Boolean;
begin
{$IFDEF _D4_}
  Result := Bound;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TDBParam.ParamRef: TDBParam;
begin
  if not Assigned(FParamRef) then
    if Assigned(Collection) and (Name <> '') then
      FParamRef := TDBParams(Collection).ParamByName(Name) else
      FParamRef := Self;
  Result := FParamRef;
end;

function TDBParam.GetIsNull: Boolean;
begin
  Result := FNull or VarIsNull(FData) or VarIsEmpty(FData);
end;

function TDBParam.GetParamType: TDBParamType;
begin
  Result := ParamRef.FParamType;
end;

procedure TDBParam.SetParamType(Value: TDBParamType);
begin
  ParamRef.FParamType := Value;
end;

function TDBParam.GetDataType: TFieldType;
begin
  Result := ParamRef.FDataType;
end;

procedure TDBParam.SetDataType(Value: TFieldType);
const
  VarTypeMap: array[TFieldType] of Integer = (varError, varOleStr, varSmallint,
    varInteger, varSmallint, varBoolean, varDouble, varCurrency, varCurrency,
    varDate, varDate, varDate, varOleStr, varOleStr, varInteger, varOleStr,
    varOleStr, varOleStr, varOleStr, varOleStr, varOleStr, varOleStr
{$IFDEF _D3_}
    , varError
  {$IFDEF _D4_}
    , varOleStr, varOleStr, varError, varError, varError, varError, varError
    {$IFDEF _D5_}
    ,varOleStr, varOleStr, varVariant, varUnknown, varDispatch, varOleStr
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
    );
var
  vType: Integer;
begin
  ParamRef.FDataType := Value;
  if Assigned(DataSet) and (csDesigning in DataSet.ComponentState) and
     (not ParamRef.IsNull) then
  begin
    vType := VarTypeMap[Value];
    if vType <> varError then
    try
      VarCast(ParamRef.FData, ParamRef.FData, vType);
    except
      ParamRef.Clear;
    end else
      ParamRef.Clear;
  end else
    ParamRef.Clear;
end;

function TDBParam.GetDataSize: Integer;
begin
  Result := 0;
  case DataType of
    ftUnknown: DataTypeUnknown;
    ftString, ftMemo {$IFDEF _D4},ftFixedChar{$ENDIF}: Result := Length(FData) + 1;
    ftBoolean: Result := SizeOf(WordBool);
    ftBCD: Result := 34; { sizeof FMTBCD (BDE) }
    ftDateTime,
    ftCurrency,
    ftFloat: Result := SizeOf(Double);
    ftTime,
    ftDate,
    ftAutoInc,
    ftInteger: Result := SizeOf(Integer);
    ftSmallint: Result := SizeOf(SmallInt);
    ftWord: Result := SizeOf(Word);
    ftBytes, ftVarBytes:
      if VarIsArray(FData) then
        Result := VarArrayHighBound(FData, 1) + 1 else
        Result := 0;
    ftBlob, ftGraphic..ftTypedBinary: Result := Length(FData);
{$IFDEF _D3_}
    ftCursor: Result := 0;
{$ENDIF}
  else
    DataTypeUnsupported;
  end;
end;

{$IFDEF _D3_}
type
  TDataSetHack = class(TDataSet);
{$ENDIF}

procedure TDBParam.GetData(Buffer: Pointer);
var
  P: Pointer;
begin
  case DataType of
    ftUnknown: DataTypeUnknown;
    ftString, ftMemo {$IFDEF _D4_}, ftFixedChar{$ENDIF}:
      StrMove(Buffer, PChar(GetAsString), Length(GetAsString) + 1);
    ftSmallint: SmallInt(Buffer^) := GetAsInteger;
    ftWord: Word(Buffer^) := GetAsInteger;
    ftAutoInc,
    ftInteger: Integer(Buffer^) := GetAsInteger;
    ftTime: Integer(Buffer^) := DateTimeToTimeStamp(AsDateTime).Time;
    ftDate: Integer(Buffer^) := DateTimeToTimeStamp(AsDateTime).Date;
    ftDateTime:  Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(AsDateTime));
    ftBCD:
{$IFDEF _D3_}
  {$IFDEF _D5_}
      CurrToBCD(AsBCD, TBcd(Buffer^), 32, 4);
  {$ELSE}
      TDataSetHack(DataSet).CurrToBCD(AsBCD, Buffer, 32, 4);
  {$ENDIF}
{$ELSE}
      CurrToBCD(AsBCD, pFMTBCD(Buffer)^, 32, 4);
{$ENDIF}
    ftCurrency,
    ftFloat: Double(Buffer^) := GetAsFloat;
    ftBoolean: Word(Buffer^) := Ord(GetAsBoolean);
    ftBytes, ftVarBytes:
    begin
      if VarIsArray(FData) then
      begin
        P := VarArrayLock(FData);
        try
          Move(P^, Buffer^, VarArrayHighBound(FData, 1) + 1);
        finally
          VarArrayUnlock(FData);
        end;
      end;
    end;
    ftBlob, ftGraphic..ftTypedBinary:
      Move(PChar(GetAsString)^, Buffer^, Length(GetAsString));
{$IFDEF _D3_}
    ftCursor: {Nothing};
{$ENDIF}
  else
    DataTypeUnsupported;
  end;
end;

procedure TDBParam.SetBlobData(Buffer: Pointer; Size: Integer);
var
  DataStr: string;
begin
  SetLength(DataStr, Size);
  Move(Buffer^, PChar(DataStr)^, Size);
  AsBlob := DataStr;
end;

procedure TDBParam.SetData(Buffer: Pointer);
var
  Value: Currency;
  TimeStamp: TTimeStamp;
begin
  case DataType of
    ftUnknown: DataTypeUnknown;
    ftString{$IFDEF _D4}, ftFixedChar{$ENDIF}: AsString := StrPas(Buffer);
    ftWord: AsWord := Word(Buffer^);
    ftSmallint: AsSmallInt := Smallint(Buffer^);
    ftInteger, ftAutoInc: AsInteger := Integer(Buffer^);
    ftTime:
      begin
        TimeStamp.Time := LongInt(Buffer^);
        TimeStamp.Date := DateDelta;
        AsTime := TimeStampToDateTime(TimeStamp);
      end;
    ftDate:
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Integer(Buffer^);
        AsDate := TimeStampToDateTime(TimeStamp);
      end;
    ftDateTime:
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Integer(Buffer^);
        AsDateTime := TimeStampToDateTime(MSecsToTimeStamp(Double(Buffer^)));
      end;
    ftBCD:
{$IFDEF _D3_}
  {$IFDEF _D5_}
        if BCDToCurr(TBcd(Buffer^), Value) then
  {$ELSE}
        if TDataSetHack(DataSet).BCDToCurr(Buffer, Value) then
  {$ENDIF}
{$ELSE}
        if BCDToCurr(pFMTBCD(Buffer)^, Value) then
{$ENDIF} 
          AsBCD := Value else
          AsBCD := 0;
    ftCurrency: AsCurrency := Double(Buffer^);
    ftFloat: AsFloat := Double(Buffer^);
    ftBoolean: AsBoolean := WordBool(Buffer^);
    ftMemo: AsMemo := StrPas(Buffer);
{$IFDEF _D3_}
    ftCursor: FData := 0;
{$ENDIF}
  else
    DataTypeUnsupported;
  end;
  Changed;
end;

procedure TDBParam.SetText(const Value: string);
begin
  Self.Value := Value;
end;

procedure TDBParam.Assign(Source: TPersistent);

  procedure LoadFromBitmap(Bitmap: TBitmap);
  var
    MS: TMemoryStream;
  begin
    MS := TMemoryStream.Create;
    try
      Bitmap.SaveToStream(MS);
      LoadFromStream(MS, ftGraphic);
    finally
      MS.Free;
    end;
  end;

  procedure LoadFromStrings(Source: TSTrings);
  begin
    AsMemo := Source.Text;
  end;

begin
  if Source is TDBParam then
    AssignParam(TDBParam(Source))
  else if Source is TField then
    AssignField(TField(Source))
  else if Source is TStrings then
    LoadFromStrings(TStrings(Source))
  else if Source is TBitmap then
    LoadFromBitmap(TBitmap(Source))
  else if (Source is TPicture) and (TPicture(Source).Graphic is TBitmap) then
    LoadFromBitmap(TBitmap(TPicture(Source).Graphic))
  else
    inherited Assign(Source);
end;

procedure TDBParam.AssignTo(Dest: TPersistent);
begin
  if Dest is TField then
    TField(Dest).Value := FData else
    inherited AssignTo(Dest);
end;

procedure TDBParam.AssignParam(Param: TDBParam);
begin
  if Param <> nil then
  begin
    FDataType := Param.DataType;
    if Param.IsNull then
      Clear else
      Value := Param.FData;
    FBound := Param.Bound;
    Name := Param.Name;
    if ParamType = ptUnknown then ParamType := Param.ParamType;
  end;
end;

type
  TReaderHack = class(TReader);

procedure TDBParam.ReadValue(Reader: TReader);
var
  AValue: Variant;
  procedure ReadVariantProp;
  const
    ValTtoVarT: array[TValueType] of Integer = (varNull, varError, varByte,
      varSmallInt, varInteger, varDouble, varString, varError, varBoolean,
      varBoolean, varError, varError, varString, varEmpty, varError
{$IFDEF _D4_}
      ,varError, varError, varError, varError
  {$IFDEF _D5_}
      ,varError
  {$ENDIF}
{$ENDIF}
      );
  var
    ValType: TValueType;
  begin
    ValType := TReaderHack(Reader).NextValue;
    if ValType in [vaInt8, vaInt16, vaInt32, vaExtended] then
      case DataType of
        ftCurrency:
          begin
            TVarData(AValue).VCurrency := Reader.ReadFloat;
            TVarData(AValue).VType := varCurrency;
            Exit;
          end;
        ftDate, ftTime, ftDateTime:
          begin
            TVarData(AValue).VDate := Reader.ReadFloat;
            TVarData(AValue).VType := varDate;
            Exit;
          end;
      end;

    case ValType of
      vaNil, vaNull:
      begin
        if TReaderHack(Reader).ReadValue = vaNil then
          VarClear(AValue) else
          AValue := Null;
      end;
      vaInt8: TVarData(AValue).VByte := Byte(Reader.ReadInteger);
      vaInt16: TVarData(AValue).VSmallint := Smallint(Reader.ReadInteger);
      vaInt32: TVarData(AValue).VInteger := Reader.ReadInteger;
      vaExtended: TVarData(AValue).VDouble := Reader.ReadFloat;
      vaString, vaLString: AValue := Reader.ReadString;
      vaFalse, vaTrue: TVarData(AValue).VBoolean := TReaderHack(Reader).ReadValue = vaTrue;
    else
      raise EReadError.Create(ResStr(SReadError));
    end;
    TVarData(AValue).VType := ValTtoVarT[ValType];
  end;
begin
  ReadVariantProp;
  Value := AValue;
end;

{$IFNDEF _D4_}
type
  TWriterHack = class(TWriter);

procedure TDBParam.WriteValue(Writer: TWriter);
  procedure WriteValue(Value: TValueType);
  begin
{$IFDEF _D3_}
    TWriterHack(Writer).WriteValue(vaTrue);
{$ELSE}
    Writer.Write(Value, SizeOf(Value));
{$ENDIF}
  end;
  procedure WriteVariantProp;
  var
    VType: Integer;
  begin
    if VarIsArray(Value) then
      raise EWriteError.Create(ResStr(SWriteError));
    VType := VarType(Value);
    case VType and varTypeMask of
      varEmpty: WriteValue(vaNil);
      varNull: WriteValue(vaNull);
      varOleStr, varString: Writer.WriteString(Value);
      varByte, varSmallInt, varInteger: Writer.WriteInteger(Value);
      varDate, varCurrency, varSingle, varDouble: Writer.WriteFloat(Value);
      varBoolean:
        if Value then
          WriteValue(vaTrue) else
          WriteValue(vaFalse);
    else
      try
        Writer.WriteString(Value);
      except
        raise EWriteError.Create(ResStr(SWriteError));
      end;
    end;
  end;
begin
  WriteVariantProp;
end;
{$ENDIF}

procedure TDBParam.Changed;
begin
  if Assigned(Collection) then TDBParams(Collection).Changed;
end;

procedure TDBParam.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
{$IFDEF _D4_}
  Filer.DefineProperty('Data', ReadValue, nil, False);
{$ELSE}
  Filer.DefineProperty('Data', ReadValue, WriteValue, Bound);
{$ENDIF}
end;

procedure TDBParam.AssignFieldValue(Field: TField; const Value: Variant);
begin
  if Field <> nil then
  begin
{$IFDEF _D4}
    if (Field.DataType = ftString) and TStringField(Field).FixedChar then
      DataType := ftFixedChar
    else
{$ENDIF}
    if (Field.DataType = ftMemo) and (Field.Size > 255) then
      DataType := ftString
    else
      DataType := Field.DataType;
    if VarIsNull(Value) then
      Clear else
      Self.Value := Value;
    FBound := True;
  end;
end;

procedure TDBParam.AssignField(Field: TField);
begin
  if Field <> nil then
  begin
    AssignFieldValue(Field, Field.Value);
    Name := Field.FieldName;
  end;
end;

procedure TDBParam.Clear;
begin
  FNull := True;
  FData := Unassigned;
  Changed;
end;

function TDBParam.GetDataSet: TDataSet;
begin
  if not Assigned(Collection) then
    Result := nil else
    Result := TDBParams(Collection).GetDataSet;
end;

function TDBParam.GetDisplayName: string;
begin
  if FName = '' then
    Result := ClassName else Result := FName;
end;

procedure TDBParam.SetAsBoolean(Value: Boolean);
begin
  FDataType := ftBoolean;
  Self.Value := Value;
end;

function TDBParam.GetAsBoolean: Boolean;
begin
  if IsNull then
    Result := False else
    Result := FData;
end;

procedure TDBParam.SetAsFloat(const Value: Double);
begin
  FDataType := ftFloat;
  Self.Value := Value;
end;

function TDBParam.GetAsFloat: Double;
begin
  if IsNull then
    Result := 0 else
    Result := FData;
end;

procedure TDBParam.SetAsCurrency(const Value: Currency);
begin
  FDataType := ftCurrency;
  Self.Value := Value;
end;

function TDBParam.GetAsCurrency: Currency;
begin
  if IsNull then
    Result := 0 else
    Result := FData;
end;

procedure TDBParam.SetAsBCD(const Value: Currency);
begin
  FDataType := ftBCD;
  Self.Value := Value;
end;

function TDBParam.GetAsBCD: Currency;
begin
  if IsNull then
    Result := 0 else
    Result := FData;
end;

procedure TDBParam.SetAsInteger(Value: Longint);
begin
  FDataType := ftInteger;
  Self.Value := Value;
end;

function TDBParam.GetAsInteger: Longint;
begin
  if IsNull then
    Result := 0 else
    Result := FData;
end;

procedure TDBParam.SetAsWord(Value: LongInt);
begin
  FDataType := ftWord;
  Self.Value := Value;
end;

procedure TDBParam.SetAsSmallInt(Value: LongInt);
begin
  FDataType := ftSmallint;
  Self.Value := Value;
end;

procedure TDBParam.SetAsString(const Value: string);
begin
  FDataType := ftString;
  Self.Value := Value;
end;

function TDBParam.GetAsString: string;
begin
  if IsNull then
    Result := ''
  else if DataType = ftBoolean then
  begin
    if FData then
      Result := ResStr(STextTrue) else
      Result := ResStr(STextFalse);
  end else
    Result := FData;
end;

procedure TDBParam.SetAsDate(const Value: TDateTime);
begin
  FDataType := ftDate;
  Self.Value := Value;
end;

procedure TDBParam.SetAsTime(const Value: TDateTime);
begin
  FDataType := ftTime;
  Self.Value := Value
end;

procedure TDBParam.SetAsDateTime(const Value: TDateTime);
begin
  FDataType := ftDateTime;
  Self.Value := Value
end;

function TDBParam.GetAsDateTime: TDateTime;
begin
  if IsNull then
    Result := 0 else
    Result := VarToDateTime(FData);
end;

procedure TDBParam.SetAsVariant(const Value: Variant);
begin
  if ParamRef = Self then
  begin
    FBound := not VarIsEmpty(Value);
    FNull := VarIsEmpty(Value) or VarIsNull(Value);
    if FDataType = ftUnknown then
      case VarType(Value) of
        varSmallint, varByte: FDataType := ftSmallInt;
        varInteger: FDataType := ftInteger;
        varCurrency: FDataType := ftBCD;
        varSingle, varDouble: FDataType := ftFloat;
        varDate: FDataType := ftDateTime;
        varBoolean: FDataType := ftBoolean;
        varString, varOleStr: FDataType := ftString;
      else
        FDataType := ftUnknown;
      end;
    FData := Value;
    Changed;
  end else
    ParamRef.SetAsVariant(Value);
end;

function TDBParam.GetAsVariant: Variant;
begin
  Result := ParamRef.FData;
end;

procedure TDBParam.SetAsMemo(const Value: string);
begin
  FDataType := ftMemo;
  Self.Value := Value;
end;

function TDBParam.GetAsMemo: string;
begin
  if IsNull then Result := '' else Result := FData;
end;

procedure TDBParam.SetAsBlob(const Value: TBlobData);
begin
  FDataType := ftBlob;
  Self.Value := Value;
end;

procedure TDBParam.LoadFromFile(const FileName: string; BlobType: TBlobType);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream, BlobType);
  finally
    Stream.Free;
  end;
end;

procedure TDBParam.LoadFromStream(Stream: TStream; BlobType: TBlobType);
var
  DataStr: string;
  Len: Integer;
begin
  with Stream do
  begin
    FDataType := BlobType;
    Position := 0;
    Len := Size;
    SetLength(DataStr, Len);
    ReadBuffer(Pointer(DataStr)^, Len);
    Self.Value := DataStr;
  end;
end;

{ TUpdateObject }
destructor TUpdateObject.Destroy;
var
  I: TUpdateKind;
begin
  for I := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FParams[I, 0].Free;
    FParams[I, 1].Free;
  end;
  inherited;
end;

function TUpdateObject.StoreParams(Index: Integer): Boolean;
begin
  Result := Assigned(FParams[TUpdateKind(Index div 2), Index mod 2]) and
    (FParams[TUpdateKind(Index div 2), Index mod 2].Count > 0)
end;

procedure TUpdateObject.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TUpdateObject then
    for I := 0 to 5 do
      GetParamsIndex(I).Assign(TUpdateObject(Source).GetParamsIndex(I))
  else
    inherited;
end;

procedure TUpdateObject.SetUpdateParams(DataSet: TDataSet; UpdateKind: TUpdateKind);
begin
  SetUpdateDBParams(Params[UpdateKind], DataSet);
  SetUpdateDBParams(Macros[UpdateKind], DataSet);
end;

function TUpdateObject.GetMacros(UpdateKind: TUpdateKind): TDBParams;
begin
  Result := GetParamsIndex(Integer(UpdateKind) * 2 + 1);
end;

function TUpdateObject.GetParams(UpdateKind: TUpdateKind): TDBParams;
begin
  Result := GetParamsIndex(Integer(UpdateKind) * 2);
end;

function TUpdateObject.GetParamsIndex(Index: Integer): TDBParams;
begin
  Result := FParams[TUpdateKind(Index div 2), Index mod 2];
  if not Assigned(Result) then
  begin
    Result := TDBParams.CreateOwned(Self);
    FParams[TUpdateKind(Index div 2), Index mod 2] := Result;
  end;
end;

procedure TUpdateObject.SetMacros(UpdateKind: TUpdateKind; Value: TDBParams);
begin
  GetMacros(UpdateKind).Assign(Value);
end;

procedure TUpdateObject.SetParams(UpdateKind: TUpdateKind; Value: TDBParams);
begin
  GetParams(UpdateKind).Assign(Value);
end;

procedure TUpdateObject.SetParamsIndex(Index: Integer; Value: TDBParams);
begin
  GetParamsIndex(Index).Assign(Value);
end;

{ TUpdateSQLObject }
constructor TUpdateSQLObject.Create(AOwner: TComponent);
begin
  inherited;
  FMacroChar := DefaultMacroChar;
end;

destructor TUpdateSQLObject.Destroy;
var
  I: TUpdateKind;
begin
  for I := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[I].Free;
  inherited;
end;

procedure TUpdateSQLObject.CreateParams(UpdateKind: TUpdateKind; Macro: Boolean);
var
  Tmp: TDBParams;
  SpecialChar: Char;
  Text: string;
begin
  Tmp := TDBParams.Create;
  try
    if Macro then
      SpecialChar := MacroChar else
      SpecialChar := ':';

    Text := SQL[UpdateKind].Text;
    CreateDBParams(Tmp, PChar(Text), Macro, SpecialChar, []);

    GetParamsIndex(Integer(UpdateKind) * 2 + Ord(Macro));

    Tmp.AssignValues(FParams[UpdateKind, Ord(Macro)]);
    FParams[UpdateKind, Ord(Macro)].Assign(Tmp);
  finally
    Tmp.Free;
  end;
end;

procedure TUpdateSQLObject.Assign(Source: TPersistent);
var
  I: TUpdateKind;
begin
  if Source is TUpdateSQLObject then
    for I := Low(TUpdateKind) to High(TUpdateKind) do
      SQL[I] := TUpdateSQLObject(Source).SQL[I];
  inherited;
end;

function TUpdateSQLObject.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FSQLText[UpdateKind];
  if not Assigned(Result) then
  begin
    Result := TStringList.Create;
    TStringList(Result).OnChange := SQLTextChanged;
    FSQLText[UpdateKind] := Result;
  end;
end;

function TUpdateSQLObject.GetSQLIndex(Index: Integer): TStrings;
begin
  Result := GetSQL(TUpdateKind(Index));
end;

procedure TUpdateSQLObject.SetMacroChar(Value: Char);
var
  I: TUpdateKind;
begin
  if (FMacroChar <> Value) then
  begin
    FMacroChar := Value;
    for I := Low(TUpdateKind) to High(TUpdateKind) do
      CreateParams(I, True);
  end;
end;

procedure TUpdateSQLObject.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  GetSQL(UpdateKind).Assign(Value);
end;

procedure TUpdateSQLObject.SetSQLIndex(Index: Integer; Value: TStrings);
begin
  GetSQL(TUpdateKind(Index)).Assign(Value);
end;

procedure TUpdateSQLObject.SQLChanged(UpdateKind: TUpdateKind);
begin
end;

procedure TUpdateSQLObject.SQLTextChanged(Sender: TObject);
var
  I, J: TUpdateKind;
begin
  J := Low(TUpdateKind);
  for I := Low(TUpdateKind) to High(TUpdateKind) do
    if Sender = FSQLText[I] then
    begin
      J := I;
      Break;
    end;
  SQLChanged(J);
  CreateParams(J, False);
  CreateParams(J, True);
end;

{$IFDEF _D3_}
{ TDataSetObject }
constructor TDataSetObject.Create(ADataSet: TDataSet; AParams: OleVariant);
begin
  FDataSet := ADataSet;

  if not VarIsNull(AParams) then
    SetParamValue(AParams[0], AParams[1]);
end;

function TDataSetObject.DoExecute: Integer;
begin
  Result := -1;
end;

function TDataSetObject.DoGetParamValue(const ParamName: string): OleVariant;
begin
  Result := Null;
end;

procedure TDataSetObject.DoSetParamValue(const ParamName: string; Value: OleVariant);
begin
end;

{ TDataSetObject.IvgDataSet }
procedure TDataSetObject.First;
begin
  FDataSet.First;
end;

procedure TDataSetObject.Last;
begin
  FDataSet.Last;
end;

procedure TDataSetObject.MoveBy(Offset: Integer);
begin
  FDataSet.MoveBy(Offset);
end;

function TDataSetObject.BOF: WordBool;
begin
  Result := FDataSet.BOF;
end;

function TDataSetObject.EOF: WordBool;
begin
  Result := FDataSet.EOF;
end;

procedure TDataSetObject.Open;
begin
  FDataSet.Open;
end;

procedure TDataSetObject.Close;
begin
  FDataSet.Close;
end;

function TDataSetObject.Execute: Integer;
begin
  Result := DoExecute;
end;

function TDataSetObject.Locate(const FieldNames: WideString; FieldValues: OleVariant;
  CaseIns, PartialKey: WordBool): WordBool; 
var
  Options: TLocateOptions;
begin
  Options := [];
  if CaseIns then Include(Options, loCaseInsensitive);
  if PartialKey then Include(Options, loPartialKey);
  Result := FDataSet.Locate(FieldNames, FieldValues, Options);
end;

procedure TDataSetObject.Insert;
begin
  FDataSet.Insert;
end;

procedure TDataSetObject.Append;
begin
  FDataSet.Append;
end;

procedure TDataSetObject.Edit;
begin
  FDataSet.Edit;
end;

procedure TDataSetObject.Post;
begin
  FDataSet.Post;
end;

procedure TDataSetObject.Cancel;
begin
  FDataSet.Cancel;
end;

procedure TDataSetObject.Delete;
begin
  FDataSet.Delete;
end;

function TDataSetObject.Get_Active: WordBool;
begin
  Result := FDataSet.Active;
end;

procedure TDataSetObject.Set_Active(Value: WordBool);
begin
  FDataSet.Active := Value;
end;

function TDataSetObject.Get_Empty: WordBool;
begin
  Result := IsEmpty(FDataSet);
end;

function TDataSetObject.GetFieldValue(const FieldName: WideString): OleVariant;
begin
  Result := FDataSet.FieldValues[FieldName];
end;

procedure TDataSetObject.SetFieldValue(const FieldName: WideString; Value: OleVariant);
begin
  FDataSet.FieldValues[FieldName] := Value;
end;

function TDataSetObject.GetParamValue(const ParamName: WideString): OleVariant;
begin
  Result := DoGetParamValue(ParamName);
end;

procedure TDataSetObject.SetParamValue(const ParamName: WideString; Value: OleVariant);
begin
  DoSetParamValue(ParamName, Value);
end;
{$ENDIF}

end.

