unit SQLDataPump;

{*************************************************************}
{*                 Freeware dbExpress Plus                   *}
{* Copyright (c) Business Software Systems, Inc. 1995 - 2002 *}
{*                   All rights reserved.                    *}
{*************************************************************}

{$N+,P+,S-,R-}

interface

uses
  Variants, Windows, Classes, Forms, Dialogs, SysUtils, ExtCtrls, Controls,
  Graphics, DB, DBXpress, SQLExpr, SQLMetaData;

type
  TSQLDataPump = class;

  TStatisticsPanel = class(TPanel);

  TPrimaryKeyInfo = record
    FieldName: String;
    FieldDataType: Integer;
  end;

  TAsciiDataType = (dtDate, dtDateTime, dtNumber, dtOther, dtString, dtTime);
  TAsciiFormatOption = (foNone, foBlank, foIgnore, foZeroFillNum, foCurrencyNum,
     foNumberDate, foShortDate, foLongDate, foEuroDate);
  TAsciiRecordDelimiter = (rdDOS, rdUNIX);
  TAsciiRecordFormat = (ffFixedLength, ffCommaDoubleQuote, ffFieldPerLine,
     ffTabDoubleQuote, ffCustom);
  TDataMoveMode = (dmAlwaysInsert, dmAppend, dmUpdate, dmAppendUpdate, dmDelete, dmAsciiExport);
  TDataMoveStatistic = (dmsReadWrite, dmsReadUpdateInsert, dmsNone);
  TExceptionFileAction = (efaCreate, efaAppend, efaNone);
  TFieldValueType = (fvtColumn, fvtLiteral);
  TSQLDataPumpMode = (sdpTableToTable, sdpTableToAscii, sdpAsciiToTable);

  TDoBeforeFieldPump = procedure(Index: LongInt; Sender: TObject) of object;

  TAsciiDataDefItem = class(TCollectionItem)
  private
    FAsciiDataType: TAsciiDataType;
    FAsciiFieldName: String;
    FAsciiFieldSize: LongInt;
    FAsciiFormatOption: TAsciiFormatOption;
    FExportSourceColumn: String;
    FExportSourceLiteral: String;
    FExportSourceValueType: TFieldValueType;
    FExportDataType: TFieldType;
    FValue: Variant;
    FBeforeFieldPump: TDoBeforeFieldPump;
    procedure SetAsciiDataType(Value: TAsciiDataType);
    procedure SetAsciiFieldName(Value: String);
    procedure SetAsciiFieldSize(Value: LongInt);
    procedure SetAsciiFormatOption(Value: TAsciiFormatOption);
    procedure SetExportSourceColumn(Value: String);
    procedure SetExportSourceLiteral(Value: String);
    procedure SetExportSourceValueType(Value: TFieldValueType);
    procedure SetValue(Value: Variant);
    procedure SetExportDataType(Value: TFieldType);
    procedure DoBeforeFieldPump(Index: LongInt; Sender: TObject);
  protected
    function GetDisplayName: String; override;
  public
    property Value: Variant read FValue write SetValue;
    property ExportDataType: TFieldType read FExportDataType write SetExportDataType;
  published
    property AsciiDataType: TAsciiDataType read FAsciiDataType write SetAsciiDataType;
    property AsciiFieldName: String read FAsciiFieldName write SetAsciiFieldName;
    property AsciiFieldSize: LongInt read FAsciiFieldSize write SetAsciiFieldSize;
    property AsciiFormatOption: TAsciiFormatOption read FAsciiFormatOption write SetAsciiFormatOption;
    property ExportSourceDataColumn: String read FExportSourceColumn write SetExportSourceColumn;
    property ExportSourceLiteral: String read FExportSourceLiteral write SetExportSourceLiteral;
    property ExportSourceValueType: TFieldValueType read FExportSourceValueType write SetExportSourceValueType;
    property BeforeFieldPump: TDoBeforeFieldPump read FBeforeFieldPump write FBeforeFieldPump;
  end;

  TAsciiDataDef = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TAsciiDataDefItem;
    procedure SetItem(Index: Integer; const Value: TAsciiDataDefItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);
    function Add: TAsciiDataDefItem;
    procedure AddAll;
    property Items[Index: Integer]: TAsciiDataDefItem read GetItem write SetItem; default;
  end;

  TDestinationFieldItem = class(TCollectionItem)
  private
    FFieldDataType: Integer;
    FFieldLength: LongInt;
    FFieldName: String;
    FFieldNullable: LongInt;
    FFieldPosition: LongInt;
    FFieldPrecision: LongInt;
    FFieldScale: LongInt;
    FFieldSubtype: LongInt;
    FFieldTypeName: String;
    FSourceColumn: String;
    FSourceLiteral: String;
    FSourceValueType: TFieldValueType;
    FValue: Variant;
    FBeforeFieldPump: TDoBeforeFieldPump;
    procedure SetFieldName(Value: String);
    procedure SetSourceColumn(Value: String);
    procedure SetSourceLiteral(Value: String);
    procedure SetSourceValueType(Value: TFieldValueType);
    procedure SetValue(Value: Variant);
    procedure DoBeforeFieldPump(Index: LongInt; Sender: TObject);
  protected
    function GetDisplayName: String; override;
    procedure SetFieldDataType(Value: Integer);
    procedure SetFieldLength(Value: LongInt);
    procedure SetFieldNullable(Value: LongInt);
    procedure SetFieldPosition(Value: LongInt);
    procedure SetFieldPrecision(Value: LongInt);
    procedure SetFieldScale(Value: LongInt);
    procedure SetFieldSubtype(Value: LongInt);
    procedure SetFieldTypeName(Value: String);
  public
    property FieldDataType: Integer read FFieldDataType write SetFieldDataType;
    property FieldLength: LongInt read FFieldLength;
    property FieldNullable: LongInt read FFieldNullable;
    property FieldPosition: LongInt read FFieldPosition;
    property FieldPrecision: LongInt read FFieldPrecision;
    property FieldScale: LongInt read FFieldScale;
    property FieldSubtype: LongInt read FFieldSubtype;
    property FieldTypeName: String read FFieldTypeName;
    property Value: Variant read FValue write SetValue;
  published
    property FieldName: String read FFieldName write SetFieldName;
    property SourceColumn: String read FSourceColumn write SetSourceColumn;
    property SourceLiteral: String read FSourceLiteral write SetSourceLiteral;
    property SourceValueType: TFieldValueType read FSourceValueType write SetSourceValueType;
    property BeforeFieldPump: TDoBeforeFieldPump read FBeforeFieldPump write FBeforeFieldPump;
  end;

  TDestinationFields = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TDestinationFieldItem;
    procedure SetItem(Index: Integer; const Value: TDestinationFieldItem);
    procedure SetMetaData;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);
    function Add: TDestinationFieldItem;
    procedure AddAll;
    property Items[Index: Integer]: TDestinationFieldItem read GetItem write SetItem; default;
  end;

  TSQLDataPump = class(TComponent)
  private
    FAbortOnException: Boolean;
    FAbout: String;
    FAsciiDataDef: TAsciiDataDef;
    FAsciiDateSeparator: Char;
    FAsciiDelimiter: Char;
    FAsciiExportRefTable: String;
    FAsciiFileName: String;
    FAsciiRecordDelimiter: TAsciiRecordDelimiter;
    FAsciiRecordFormat: TAsciiRecordFormat;
    FAsciiSeparator: Char;
    FClearDestination: Boolean;
    FCommitCount: LongInt;
    FConfirmClear: Boolean;
    FCurrentSQLStatement: String;
    FDeleteCount: LongInt;
    FDestinationDateFormat: String;
    FDestinationDateTimeFormat: String;
    FDestinationTimeFormat: String;
    FDestinationFields: TDestinationFields;
    FDestinationPrimaryKey: TStrings;
    FDestinationPrimaryKeyCount: Integer;
    FDestinationPrimaryKeyInfo: Array[0..15] of TPrimaryKeyInfo;
    FDestinationTable: String;
    FDataMoveMode: TDataMoveMode;
    FInsertCount: LongInt;
    FExceptionFileAction: TExceptionFileAction;
    FExceptionFileName: String;
    FQueryRecordExist: TSQLQuery;
    FQuerySource: TSQLQuery;
    FReadCount: LongInt;
    FShowRunningStatistic: TDataMoveStatistic;
    FShowSummaryStatistic: TDataMoveStatistic;
    FSQLDataPumpMode: TSQLDataPumpMode;
    FSQLDelete: TStrings;
    FSQLInsert: TStrings;
    FSQLSource: TStrings;
    FSQLUpdate: TStrings;
    FSQLMetaDataDestination: TSQLMetaData;
    FSQLMetaDataSource: TSQLMetaData;
    FStatisticsCaption: String;
    FStatisticsInterval: LongInt;
    FStatisticsPanel: TStatisticsPanel;
    FUpdateCount: LongInt;
    FUseTransaction: Boolean;
    procedure SetAbortOnException(Value: Boolean);
    procedure SetAsciiDataDef(const Value: TAsciiDataDef);
    procedure SetAsciiDelimiter(Value: Char);
    procedure SetAsciiExportRefTable(Value: String);
    procedure SetAsciiFileName(Value: String);
    procedure SetAsciiRecordFormat(Value: TAsciiRecordFormat);
    procedure SetAsciiSeparator(Value: Char);
    procedure SetCommitCount(Value: Integer);
    procedure SetDestinationDateTimeFormat(Value: String);
    procedure SetDestinationFields(const Value: TDestinationFields);
    procedure SetDestinationTable(Value: String);
    procedure SetSQLDataPumpMode(Value: TSQLDataPumpMode);
    procedure SetSQLMetaDataDestination(Value: TSQLMetaData);
    function SetSQLValue(Value: Variant; DataType: Integer): String;
    procedure SetSQLSource(Value: TStrings);
    function GetWriteCount: LongInt;
    function ExecuteTableToTable: LongInt;
    function ExecuteTableToAscii: LongInt;
    function ExecuteAsciiToTable: LongInt;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function DoRecordExist: Boolean;
    procedure SetStatisticsPanel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property InsertCount: LongInt read FInsertCount;
    property ReadCount: LongInt read FReadCount;
    property UpdateCount: LongInt read FUpdateCount;
    property WriteCount: LongInt read GetWriteCount;
    procedure SetAsciiExport(TableName: String);
    procedure SetAsciiImport(TableName: String);
    procedure SetDataPumpTable(TableName: String);
    function GetCurrentSQLStatement: String;
    function GetSourceQuery: TSQLQuery;
    function Execute: LongInt;
  published
    property AbortOnException: Boolean read FAbortOnException write SetAbortOnException;
    property About: String read FAbout;
    property AsciiDataDef: TAsciiDataDef read FAsciiDataDef write SetAsciiDataDef;
    property AsciiDateSeparator: Char read FAsciiDateSeparator write FAsciiDateSeparator;
    property AsciiDelimiter: Char read FAsciiDelimiter write SetAsciiDelimiter;
    property AsciiExportRefTable: String read FAsciiExportRefTable write SetAsciiExportRefTable;
    property AsciiFileName: String read FAsciiFileName write SetAsciiFileName;
    property AsciiRecordDelimiter: TAsciiRecordDelimiter read FAsciiRecordDelimiter write FAsciiRecordDelimiter;
    property AsciiRecordFormat: TAsciiRecordFormat read FAsciiRecordFormat write SetAsciiRecordFormat;
    property AsciiSeparator: Char read FAsciiSeparator write SetAsciiSeparator;
    property ClearDestination: Boolean read FClearDestination write FClearDestination;
    property CommitCount: LongInt read FCommitCount write SetCommitCount;
    property ConfirmClear: Boolean read FConfirmClear write FConfirmClear;
    property DestinationDateTimeFormat: String read FDestinationDateTimeFormat write SetDestinationDateTimeFormat;
    property DestinationFields: TDestinationFields read FDestinationFields write SetDestinationFields;
    property DestinationTable: String read FDestinationTable write SetDestinationTable;
    property DataMoveMode: TDataMoveMode read FDataMoveMode write FDataMoveMode;
    property ExceptionFileAction: TExceptionFileAction read FExceptionFileAction write FExceptionFileAction;
    property ExceptionFileName: String read FExceptionFileName write FExceptionFileName;
    property ShowRunningStatistics: TDataMoveStatistic read FShowRunningStatistic
       write FShowRunningStatistic;
    property ShowSummaryStatistic: TDataMoveStatistic read FShowSummaryStatistic
       write FShowSummaryStatistic;
    property SQLDataPumpMode: TSQLDataPumpMode read FSQLDataPumpMode write SetSQLDataPumpMode;
    property SQLMetaDataDestination: TSQLMetaData read FSQLMetaDataDestination
       write SetSQLMetaDataDestination;
    property SQLMetaDataSource: TSQLMetaData read FSQLMetaDataSource
       write FSQLMetaDataSource;
    property SQLSource: TStrings read FSQLSource write SetSQLSource;
    property StatisticsCaption: String read FStatisticsCaption write FStatisticsCaption;
    property StatisticsInterval: LongInt read FStatisticsInterval write FStatisticsInterval;
    property UseTransaction: Boolean read FUseTransaction write FUseTransaction;
  end;

implementation

{ TAsciiDataDefItem }

procedure TAsciiDataDefItem.SetAsciiDataType(Value: TAsciiDataType);
begin
  if FAsciiDataType <> Value then FAsciiDataType := Value;
end;

procedure TAsciiDataDefItem.SetAsciiFieldName(Value: String);
begin
  if FAsciiFieldName <> Value then FAsciiFieldName := Value;
end;

procedure TAsciiDataDefItem.SetAsciiFieldSize(Value: LongInt);
begin
  if FAsciiFieldSize <> Value then FAsciiFieldSize := Value;
end;

procedure TAsciiDataDefItem.SetAsciiFormatOption(Value: TAsciiFormatOption);
begin
  if FAsciiFormatOption <> Value then FAsciiFormatOption := Value;
end;

procedure TAsciiDataDefItem.SetExportSourceColumn(Value: String);
begin
  if FExportSourceColumn <> Value then begin
    if FExportSourceValueType = fvtColumn then begin
      FExportSourceColumn := Value;
      SetExportSourceLiteral('');
    end;
  end;
  Changed(False);
end;

procedure TAsciiDataDefItem.SetExportSourceLiteral(Value: String);
begin
  if csLoading in TComponent(TAsciiDataDef(GetOwner).FOwner).ComponentState then begin
    FExportSourceLiteral := Value;
    end
  else begin
    if (FExportSourceLiteral <> Value) and (FExportSourceValueType <> fvtColumn) then
       FExportSourceLiteral := Value;
  end;
end;

procedure TAsciiDataDefItem.SetExportSourceValueType(Value: TFieldValueType);
begin
  if FExportSourceValueType <> Value then begin
    FExportSourceValueType := Value;
    if Value = fvtColumn then begin
      SetExportSourceLiteral('');
      SetExportSourceColumn(AsciiFieldName);
      end
    else begin
      SetExportSourceColumn('');
    end;
    Changed(False);
  end;
end;

procedure TAsciiDataDefItem.SetValue(Value: Variant);
begin
  FValue := Value;
end;

procedure TAsciiDataDefItem.SetExportDataType(Value: TFieldType);
begin
  FExportDataType := Value;
end;

function TAsciiDataDefItem.GetDisplayName: string;
begin
  Result := AsciiFieldName;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TAsciiDataDefItem.DoBeforeFieldPump(Index: LongInt; Sender: TObject);
begin
  if assigned(FBeforeFieldPump) then FBeforeFieldPump(Index, Sender);
end;

{ TAsciiDataDef }

constructor TAsciiDataDef.Create(Owner: TPersistent);
begin
  inherited Create(TAsciiDataDefItem);
  FOwner := Owner;
end;

function TAsciiDataDef.GetItem(Index: Integer): TAsciiDataDefItem;
begin
  Result := TAsciiDataDefItem(inherited GetItem(Index));
end;

function TAsciiDataDef.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TAsciiDataDef.SetItem(Index: Integer; const Value: TAsciiDataDefItem);
begin
  inherited SetItem(Index, Value);
end;

function TAsciiDataDef.Add: TAsciiDataDefItem;
begin
  Result := TAsciiDataDefItem(inherited Add);
end;

procedure TAsciiDataDef.AddAll;
var
  FieldNames: TStrings;
  FieldNum: Integer;
  FieldItem: TCollectionItem;
  FieldMetaData: TFieldMetaData;
begin
  //ShowMessage('AsciiDataDef.AddAll');
  FieldNames := nil;
  try
    FieldNames := TStringList.Create;

    if TSQLDataPump(Owner).SQLMetaDataSource = nil then begin
      Application.MessageBox('Source MetaData Name Missing.',
         'Error', mb_OK + MB_DefButton1 + MB_IconStop);
      Exit;
    end;
    if not(TSQLDataPump(Owner).AsciiExportRefTable > #32) then begin
      Application.MessageBox('Source Export Reference Table MetaData Name Missing.',
         'Error', mb_OK + MB_DefButton1 + MB_IconStop);
      Exit;
    end;

    TSQLDataPump(Owner).SQLMetaDataDestination.GetFieldNames(TSQLDataPump(Owner).DestinationTable, FieldNames);

    // Set Destination Fields
    Clear;
    for FieldNum := 0 to FieldNames.Count - 1 do begin
      FieldMetaData := TSQLDataPump(Owner).SQLMetaDataSource.GetFieldMetaData(TSQLDataPump(Owner).AsciiExportRefTable, FieldNames[FieldNum]);
      FieldItem := Add;
      TAsciiDataDefItem(FieldItem).AsciiFieldName := FieldMetaData.ColumnName;
      TAsciiDataDefItem(FieldItem).AsciiFieldSize := FieldMetaData.ColumnLength;

      case FieldMetaData.ColumnDataType of
          1: // String
            begin
              TAsciiDataDefItem(FieldItem).AsciiDataType := dtString;
            end;
          5,6,7,8,12,13,14,18,19,25: // Number
            begin
              TAsciiDataDefItem(FieldItem).AsciiDataType := dtNumber;
            end;
          2: // Date
            begin
              TAsciiDataDefItem(FieldItem).AsciiDataType := dtDate;
            end;
          10: // Time
            begin
              TAsciiDataDefItem(FieldItem).AsciiDataType := dtTime;
            end;
          11,24: // DateTime
            begin
              TAsciiDataDefItem(FieldItem).AsciiDataType := dtDateTime;
            end;
        else
          TAsciiDataDefItem(FieldItem).AsciiDataType := dtOther;
        end; //  End Case

      TAsciiDataDefItem(FieldItem).ExportSourceValueType := fvtColumn;
      TAsciiDataDefItem(FieldItem).ExportSourceDataColumn := FieldNames[FieldNum];
    end;

  finally
    FieldNames.Free;
  end;
end;

{ TDestinationFieldItem }

procedure TDestinationFieldItem.SetFieldDataType(Value: Integer);
begin
  if FFieldDataType <> Value then FFieldDataType := Value;
end;

procedure TDestinationFieldItem.SetFieldName(Value: String);
begin
  if FFieldName <> Value then begin
    if (FSourceValueType = fvtColumn) and ((FFieldName = FSourceColumn) or
       (FSourceColumn = '')) then begin
      SetSourceColumn(Value);
    end;
    FFieldName := Value;
  end;
  Changed(False);
end;

procedure TDestinationFieldItem.SetFieldLength(Value: LongInt);
begin
  FFieldLength := Value;
end;

procedure TDestinationFieldItem.SetFieldNullable(Value: LongInt);
begin
  FFieldNullable := Value;
end;

procedure TDestinationFieldItem.SetFieldPosition(Value: LongInt);
begin
  FFieldPosition := Value;
end;

procedure TDestinationFieldItem.SetFieldPrecision(Value: LongInt);
begin
  FFieldPrecision := Value;
end;

procedure TDestinationFieldItem.SetFieldScale(Value: LongInt);
begin
  FFieldScale := Value;
end;

procedure TDestinationFieldItem.SetFieldSubtype(Value: LongInt);
begin
  FFieldSubtype := Value;
end;

procedure TDestinationFieldItem.SetFieldTypeName(Value: String);
begin
  FFieldTypeName := Value;
end;

procedure TDestinationFieldItem.SetSourceColumn(Value: String);
begin
  if FSourceColumn <> Value then FSourceColumn := Value;
end;

procedure TDestinationFieldItem.SetSourceLiteral(Value: String);
begin
  if csLoading in TComponent(TDestinationFields(GetOwner).FOwner).ComponentState then begin
    FSourceLiteral := Value;
    end
  else begin  
    if (FSourceLiteral <> Value) and (FSourceValueType <> fvtColumn) then
       FSourceLiteral := Value;
  end;
end;

procedure TDestinationFieldItem.SetSourceValueType(Value: TFieldValueType);
begin
  if FSourceValueType <> Value then begin
    FSourceValueType := Value;
    if Value = fvtColumn then begin
      SetSourceLiteral('');
      SetSourceColumn(FieldName);
      end
    else begin
      SetSourceColumn('');
    end;
    Changed(False);
  end;
end;

procedure TDestinationFieldItem.SetValue(Value: Variant);
begin
  FValue := Value;
end;

function TDestinationFieldItem.GetDisplayName: string;
begin
  Result := FieldName;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TDestinationFieldItem.DoBeforeFieldPump(Index: LongInt; Sender: TObject);
begin
  if assigned(FBeforeFieldPump) then FBeforeFieldPump(Index, Sender);
end;

{ TDestinationFields }

constructor TDestinationFields.Create(Owner: TPersistent);
begin
  inherited Create(TDestinationFieldItem);
  FOwner := Owner;
end;

function TDestinationFields.GetItem(Index: Integer): TDestinationFieldItem;
begin
  Result := TDestinationFieldItem(inherited GetItem(Index));
end;

function TDestinationFields.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TDestinationFields.SetItem(Index: Integer; const Value: TDestinationFieldItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TDestinationFields.SetMetaData;
var
  i: Integer;
  FieldMetaData: TFieldMetaData;
begin
  for i := 0 to Self.Count - 1 do begin
    FieldMetaData := TSQLDataPump(GetOwner).SQLMetaDataDestination.
       GetFieldMetaData(TSQLDataPump(GetOwner).DestinationTable, Self[i].FieldName);
    Self[i].SetFieldDataType(FieldMetaData.ColumnDatatype);
    Self[i].SetFieldDataType(FieldMetaData.ColumnDatatype);
    Self[i].SetFieldLength(FieldMetaData.ColumnLength);
    Self[i].SetFieldNullable(FieldMetaData.ColumnNullable);
    Self[i].SetFieldPosition(FieldMetaData.ColumnPosition);
    Self[i].SetFieldPrecision(FieldMetaData.ColumnPrecision);
    Self[i].SetFieldScale(FieldMetaData.ColumnScale);
    Self[i].SetFieldSubtype(FieldMetaData.ColumnSubtype);
    Self[i].SetFieldTypeName(FieldMetaData.ColumnTypeName);
  end;
end;

function TDestinationFields.Add: TDestinationFieldItem;
begin
  Result := TDestinationFieldItem(inherited Add);
end;

procedure TDestinationFields.AddAll;
var
  FieldNames: TStrings;
  FieldNum: Integer;
  FieldItem: TCollectionItem;
  FieldMetaData: TFieldMetaData;
begin
  FieldNames := nil;
  try
    FieldNames := TStringList.Create;

    if TSQLDataPump(Owner).SQLMetaDataDestination = nil then begin
      Application.MessageBox('Destination MetaData Name Missing.',
         'Error', mb_OK + MB_DefButton1 + MB_IconStop);
      Exit;
    end;
    if not(TSQLDataPump(Owner).DestinationTable > #32) then begin
      Application.MessageBox('Destination Table Name Missing.',
         'Error', mb_OK + MB_DefButton1 + MB_IconStop);
      Exit;
    end;

    TSQLDataPump(Owner).SQLMetaDataDestination.GetFieldNames(TSQLDataPump(Owner).DestinationTable, FieldNames);

    // Set Destination Fields
    Clear;
    for FieldNum := 0 to FieldNames.Count - 1 do begin
      FieldMetaData := TSQLDataPump(Owner).SQLMetaDataDestination.GetFieldMetaData(TSQLDataPump(Owner).DestinationTable, FieldNames[FieldNum]);
      FieldItem := Add;
      TDestinationFieldItem(FieldItem).FieldName := FieldMetaData.ColumnName;
      TDestinationFieldItem(FieldItem).SourceValueType := fvtColumn;
      TDestinationFieldItem(FieldItem).SourceColumn := FieldNames[FieldNum];
    end;

  finally
    FieldNames.Free;
  end;
end;

{ TSQLDataPump }

constructor TSQLDataPump.Create(AOwner: TComponent);
begin
  inherited;
  FAbortOnException := False;
  FAbout := 'Ver 0.9.0.1';
  FAsciiDataDef := TAsciiDataDef.Create(Self);
  FAsciiDateSeparator := '/';
  SetAsciiFileName('AsciiFile.txt');
  SetAsciiRecordFormat(ffFixedLength);
  FClearDestination := False;
  FCommitCount := 1;
  FConfirmClear := True;
  DataMoveMode := dmAppendUpdate;
  FDeleteCount := 0;
  FDestinationFields := TDestinationFields.Create(Self);
  FDestinationPrimaryKey := TStringList.Create;
  FExceptionFileAction := efaNone;
  FExceptionFileName := 'ExceptionLog.txt';
  FInsertCount := 0;
  FQueryRecordExist := TSQLQuery.Create(Self);
  FQueryRecordExist.NoMetadata := True;
  FQuerySource := TSQLQuery.Create(Self);
  FQuerySource.NoMetadata := True;
  FReadCount := 0;
  FShowRunningStatistic := dmsReadWrite;
  FShowSummaryStatistic := dmsReadWrite;
  SetSQLDataPumpMode(sdpTableToTable);
  FSQLDelete := TStringList.Create;
  FSQLInsert := TStringList.Create;
  FSQLSource := TStringList.Create;
  FSQLUpdate := TStringList.Create;
  FStatisticsCaption := 'Data Pump Statistics';
  FStatisticsInterval := 25;
  FUpdateCount := 0;
  FUseTransaction := True;
end;

destructor TSQLDataPump.Destroy;
begin
  FAsciiDataDef.Free;
  FDestinationFields.Free;
  FDestinationPrimaryKey.Free;
  FQueryRecordExist.Close;
  FQueryRecordExist.Free;
  FQuerySource.Close;
  FQuerySource.Free;
  FSQLDelete.Free;
  FSQLInsert.Free;
  FSQLSource.Free;
  FSQLUpdate.Free;
  inherited;
end;

procedure TSQLDataPump.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FSQLMetaDataDestination then
       FSQLMetaDataDestination := nil;
    if AComponent = FSQLMetaDataSource then
       FSQLMetaDataSource := nil;
  end;
end;

//  When AbortOnException = False and commit is not equal to one, it
//  makes it almost impossible to track exactly which record caused
//  the exception and therefor impossible to log it in the Exception File.
procedure TSQLDataPump.SetAbortOnException(Value: Boolean);
begin
  if FAbortOnException <> Value then begin
    FAbortOnException := Value;
    if FAbortOnException = False then SetCommitCount(1);
  end;
end;

procedure TSQLDataPump.SetAsciiDelimiter(Value: Char);
begin
  if FAsciiRecordFormat = ffCustom then FAsciiDelimiter := Value;
end;

procedure TSQLDataPump.SetAsciiFileName(Value: String);
begin
  FAsciiFileName := Value;
end;

procedure TSQLDataPump.SetAsciiRecordFormat(Value: TAsciiRecordFormat);
begin
  FAsciiRecordFormat := Value;
  if Value <> ffCustom then begin
    FAsciiDelimiter := #34;
    FAsciiSeparator := #44;
  end;
  if Value = ffCommaDoubleQuote then begin
    FAsciiDelimiter := #34;
    FAsciiSeparator := #44;
  end;
  if Value = ffTabDoubleQuote then begin
    FAsciiDelimiter := #34;
    FAsciiSeparator := #9;
  end;
end;

procedure TSQLDataPump.SetAsciiSeparator(Value: Char);
begin
  if FAsciiRecordFormat = ffCustom then FAsciiSeparator := Value;
end;

procedure TSQLDataPump.SetAsciiExportRefTable(Value: String);
begin
  if FAsciiExportRefTable <> Value then FAsciiExportRefTable := Value;
end;

//  See comments for SetAbortOnException
procedure TSQLDataPump.SetCommitCount(Value: Integer);
begin
  if FCommitCount <> Value then begin

    if FAbortOnException = False then FCommitCount := 1 else FCommitCount := Value;

    if (FAbortOnException = False) and (Value <> 1) and not(csLoading in ComponentState) then begin
      if (csDesigning in ComponentState) then begin
        Application.MessageBox('Commit Count must be 1 when AbortOnException is False.',
           'Commit Count Warning', MB_OK + MB_DEFBUTTON1 + MB_ICONINFORMATION);
        end
      else begin
        raise Exception.Create('Commit Count Error.' + #13#10 +
           'Commit Count must be 1 when AbortOnException is False.');
      end;
    end;
  end;
end;

procedure TSQLDataPump.SetDestinationDateTimeFormat(Value: String);
begin
  if (FDestinationDateTimeFormat = '') and not(csLoading in ComponentState) then begin
    FDestinationDateFormat := '';
    FDestinationDateTimeFormat := '';
    FDestinationTimeFormat := '';
    if SQLMetaDataDestination <> nil then begin
      if SQLMetaDataDestination.DriverName = 'DB2' then begin
        FDestinationDateFormat := 'yyyy-mm-dd';
        FDestinationDateTimeFormat := 'mm/dd/yyyy hh:mm:ss';
        FDestinationTimeFormat := 'hh:mm:ss';
      end;
      if SQLMetaDataDestination.DriverName = 'Interbase' then begin
        FDestinationDateFormat := 'mm/dd/yyyy';
        FDestinationDateTimeFormat := 'mm/dd/yyyy-hh:mm:ss';
        FDestinationTimeFormat := 'hh:mm:ss';
      end;
      {if SQLMetaDataDestination.DriverName = 'MYSQL' then begin
        FDestinationDateFormat := 'mm/dd/yyyy';
        FDestinationDateTimeFormat := 'mm/dd/yyyy hh:mm:ss';
        FDestinationTimeFormat := 'hh:mm:ss';
      end;}
      if SQLMetaDataDestination.DriverName = 'Oracle' then begin
        FDestinationDateFormat := '';
        FDestinationDateTimeFormat := 'dd-mmm-yyyy hh:mm:ss AM/PM';
        FDestinationTimeFormat := '';
      end;
    end;
    end
  else begin
    if Value <> FDestinationDateTimeFormat then begin
      FDestinationDateFormat := Copy(Value, 1, Pos(' ',Value) - 1);
      FDestinationDateTimeFormat := Value;
      FDestinationTimeFormat := Copy(Value, Pos(' ',Value) + 1, Length(Value) - Pos(' ',Value));
    end;
  end;
end;

function TSQLDataPump.GetSourceQuery: TSQLQuery;
begin
  Result := FQuerySource;
end;

procedure TSQLDataPump.SetAsciiDataDef(const Value: TAsciiDataDef);
begin
  FAsciiDataDef.Assign(Value);
end;

procedure TSQLDataPump.SetDestinationFields(const Value: TDestinationFields);
begin
  FDestinationFields.Assign(Value);
end;

procedure TSQLDataPump.SetDestinationTable(Value: String);
begin
  if FDestinationTable <> Value then begin
    FDestinationTable := Value;
    if not(csLoading in ComponentState) then DestinationFields.Clear;
  end;
end;

procedure TSQLDataPump.SetSQLDataPumpMode(Value: TSQLDataPumpMode);
begin
  //if Value <> sdpTableToTable then ShowMessage('ASCII Data Pumping is not yet supported.');
  //FSQLDataPumpMode := sdpTableToTable;
  if Value = sdpTableToAscii then begin
     //ShowMessage('ASCII Data Pumping is not yet supported.');
     //FSQLDataPumpMode := sdpTableToTable;
  end;
  FSQLDataPumpMode := Value;
end;

procedure TSQLDataPump.SetSQLMetaDataDestination(Value: TSQLMetaData);
begin
  if FSQLMetaDataDestination <> Value then begin
    FSQLMetaDataDestination := Value;
    if not(csLoading in ComponentState) then begin
      FDestinationDateTimeFormat := '';
      SetDestinationDateTimeFormat('');
    end;  
  end;
end;

procedure TSQLDataPump.SetSQLSource(Value: TStrings);
begin
  FSQLSource.Assign(Value);
end;

function TSQLDataPump.GetWriteCount: LongInt;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then begin
    Result := 0;
    end
  else begin
    Result := FInsertCount + FUpdateCount;
  end;
end;

function TSQLDataPump.GetCurrentSQLStatement: String;
begin
  Result := FCurrentSQLStatement;
end;

procedure TSQLDataPump.SetAsciiExport(TableName:String);
var
  FieldNames: TStrings;
  FieldNum: Integer;
begin
  if SQLMetaDataSource = nil then begin
    Application.MessageBox('Destination MetaData Name Missing.',
       'Error', mb_OK + MB_DefButton1 + MB_IconStop);
    Exit;
  end;

  FieldNames := nil;
  AsciiExportRefTable := TableName;
  AsciiDataDef.AddAll;

  try
    FieldNames := TStringList.Create;

    SQLMetaDataSource.GetFieldNames(AsciiExportRefTable, FieldNames);

    // Set Source SQL
    SQLSource.Clear;
    SQLSource.Add('SELECT');
    for FieldNum := 0 to FieldNames.Count - 1 do begin
      if FieldNum = FieldNames.Count - 1 then begin
        SQLSource.Add(FieldNames[FieldNum]);
        end
      else begin
        SQLSource.Add(FieldNames[FieldNum] + ' ,');
      end;
    end;
    SQLSource.Add('FROM');
    SQLSource.Add(DestinationTable);

  finally
    FieldNames.Free;
  end;
end;

procedure TSQLDataPump.SetAsciiImport(TableName:String);
var
  FieldNames: TStrings;
  FieldNum: Integer;
begin
  if SQLMetaDataDestination = nil then begin
    Application.MessageBox('Destination MetaData Name Missing.',
       'Error', mb_OK + MB_DefButton1 + MB_IconStop);
    Exit;
  end;

  FieldNames := nil;
  DestinationTable := TableName;
  DestinationFields.AddAll;

  try
    FieldNames := TStringList.Create;

    if SQLMetaDataSource = nil then begin
      Application.MessageBox('Source MetaData Name Missing.',
         'Error', mb_OK + MB_DefButton1 + MB_IconStop);
      Exit;
    end;

    AsciiExportRefTable := DestinationTable;
    AsciiDataDef.AddAll;
    SQLMetaDataDestination.GetFieldNames(DestinationTable, FieldNames);

    // Set Source SQL
    SQLSource.Clear;
    SQLSource.Add('SELECT');
    for FieldNum := 0 to FieldNames.Count - 1 do begin
      if FieldNum = FieldNames.Count - 1 then begin
        SQLSource.Add(FieldNames[FieldNum]);
        end
      else begin
        SQLSource.Add(FieldNames[FieldNum] + ' ,');
      end;
    end;
    SQLSource.Add('FROM');
    SQLSource.Add(DestinationTable);

  finally
    FieldNames.Free;
  end;
end;

procedure TSQLDataPump.SetDataPumpTable(TableName: String);
var
  FieldNames: TStrings;
  FieldNum: Integer;
begin
  if SQLMetaDataDestination = nil then begin
    Application.MessageBox('Destination MetaData Name Missing.',
       'Error', mb_OK + MB_DefButton1 + MB_IconStop);
    Exit;
  end;

  FieldNames := nil;
  DestinationTable := TableName;
  DestinationFields.AddAll;

  try
    FieldNames := TStringList.Create;

    if SQLMetaDataSource = nil then begin
      Application.MessageBox('Source MetaData Name Missing.',
         'Error', mb_OK + MB_DefButton1 + MB_IconStop);
      Exit;
    end;

    SQLMetaDataDestination.GetFieldNames(DestinationTable, FieldNames);

    // Set Source SQL
    SQLSource.Clear;
    SQLSource.Add('SELECT');
    for FieldNum := 0 to FieldNames.Count - 1 do begin
      if FieldNum = FieldNames.Count - 1 then begin
        SQLSource.Add(FieldNames[FieldNum]);
        end
      else begin
        SQLSource.Add(FieldNames[FieldNum] + ' ,');
      end;
    end;
    SQLSource.Add('FROM');
    SQLSource.Add(DestinationTable);

  finally
    FieldNames.Free;
  end;
end;

procedure TSQLDataPump.SetStatisticsPanel;
begin
  FStatisticsPanel.BevelInner := bvRaised;
  FStatisticsPanel.BevelOuter := bvRaised;
  FStatisticsPanel.BevelWidth := 2;
  FStatisticsPanel.Caption := FStatisticsCaption;
  FStatisticsPanel.Font.Size := 8;
  FStatisticsPanel.Font.Style := [fsBold];
  FStatisticsPanel.Height := 30;
  FStatisticsPanel.Width := 400;
  if FShowRunningStatistic = dmsReadUpdateInsert then FStatisticsPanel.Width := 550;

  FStatisticsPanel.Left := (FStatisticsPanel.Parent.Width - FStatisticsPanel.Width) div 2;
  if FStatisticsPanel.Left < 0 then FStatisticsPanel.Left := 0;
  FStatisticsPanel.Top := (FStatisticsPanel.Parent.Height - FStatisticsPanel.Height) div 2;
  if FStatisticsPanel.Top < 0 then FStatisticsPanel.Top := 0;
  FStatisticsPanel.Top := Trunc(0.77 * FStatisticsPanel.Top);

  FStatisticsPanel.BringToFront;
  FStatisticsPanel.Visible := True;
  FStatisticsPanel.Invalidate;
end;

function TSQLDataPump.SetSQLValue(Value: Variant; DataType: Integer): String;
begin
  if (VarType(Value) = varEmpty) or (VarType(Value) = varNull) then

    Result := 'NULL'

  else

    case DataType of
      0: // UnKnown
        begin
          Result := 'NULL';
        end;
      1: // String
        begin
          Result := QuotedStr(Value);
        end;
      5,6,7,8,12,13,14,18,19,25: // Number
        begin
          Result := Value;
        end;
      2: // Date
        begin
          Result := '''' + FormatDateTime(FDestinationDateFormat, Value) + '''';
        end;
      10: // Time
        begin
          Result := '''' + FormatDateTime(FDestinationTimeFormat, Value) + '''';
        end;
      11,24: // DateTime
        begin
          Result := '''' + FormatDateTime(FDestinationDateTimeFormat, Value) + '''';
          if SQLMetaDataDestination.DriverName = 'Oracle' then begin
            Result := 'TO_DATE(''' + FormatDateTime('dd-mmm-yyyy hh:mm:ss AM/PM', Value) +
               ''', ''DD-MON-YYYY HH:MI:SS AM'') ';
          end;
        end;
    else
      Result := 'NULL';
    end; //  End Case

  //fldUNKNOWN         = 0;
  //fldZSTRING         = 1;               { Null terminated string }
  //fldDATE            = 2;               { Date     (32 bit) }
  //fldBLOB            = 3;               { Blob }
  //fldBOOL            = 4;               { Boolean  (16 bit) }
  //fldINT16           = 5;               { 16 bit signed number }
  //fldINT32           = 6;               { 32 bit signed number }
  //fldFLOAT           = 7;               { 64 bit floating point }
  //fldBCD             = 8;               { BCD }
  //fldBYTES           = 9;               { Fixed number of bytes }
  //fldTIME            = 10;              { Time        (32 bit) }
  //fldTIMESTAMP       = 11;              { Time-stamp  (64 bit) }
  //fldUINT16          = 12;              { Unsigned 16 bit Integer }
  //fldUINT32          = 13;              { Unsigned 32 bit Integer }
  //fldFLOATIEEE       = 14;              { 80-bit IEEE float }
  //fldVARBYTES        = 15;              { Length prefixed var bytes }
  //fldLOCKINFO        = 16;              { Look for LOCKINFO typedef }
  //fldCURSOR          = 17;              { For Oracle Cursor type }
  //fldINT64           = 18;              { 64 bit signed number }
  //fldUINT64          = 19;              { Unsigned 64 bit Integer }
  //fldADT             = 20;              { Abstract datatype (structure) }
  //fldARRAY           = 21;              { Array field type }
  //fldREF             = 22;              { Reference to ADT }
  //fldTABLE           = 23;              { Nested table (reference) }
  //fldDATETIME        = 24;              { DateTime structure field }
  //fldFMTBCD          = 25;              { BCD Variant type: required by Midas, same as BCD for DBExpress}

  //MAXLOGFLDTYPES     = 26;              { Number of logical fieldtypes }

{ Sub Types (Logical) }

{ fldFLOAT subtype }

  //fldstMONEY         = 21;              { Money }

{ fldBLOB subtypes }

  //fldstMEMO          = 22;              { Text Memo }
  //fldstBINARY        = 23;              { Binary data }
  //fldstFMTMEMO       = 24;              { Formatted Text }
  //fldstOLEOBJ        = 25;              { OLE object (Paradox) }
  //fldstGRAPHIC       = 26;              { Graphics object }
  //fldstDBSOLEOBJ     = 27;              { dBASE OLE object }
  //fldstTYPEDBINARY   = 28;              { Typed Binary data }
  //fldstACCOLEOBJ     = 30;              { Access OLE object }
  //fldstHMEMO         = 33;              { CLOB }
  //fldstHBINARY       = 34;              { BLOB }
  //fldstBFILE         = 36;              { BFILE }

{ fldZSTRING subtype }

  //fldstPASSWORD      = 1;               { Password }
  //fldstFIXED         = 31;              { CHAR type }
  //fldstUNICODE       = 32;              { Unicode }

{ fldINT32 subtype }

  //fldstAUTOINC       = 29;

{ fldADT subtype }

  //fldstADTNestedTable = 35;             { ADT for nested table (has no name) }

{ fldDATE subtype }
  //fldstADTDATE       = 37;              { DATE (OCIDate) with in an ADT }

end;

function TSQLDataPump.DoRecordExist: Boolean;
var
  FieldCount: Integer;
begin
  Result := False;
  FQueryRecordExist.SQLConnection := TSQLConnection(SQLMetaDataDestination);
  FQueryRecordExist.SQL.Clear;
  FQueryRecordExist.SQL.Add('SELECT COUNT(*) FROM ' + DestinationTable + ' WHERE');

  for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
    FQueryRecordExist.SQL.Add(FDestinationPrimaryKeyInfo[FieldCount].FieldName + ' = ' +
       SetSQLValue(FQuerySource.FieldByName(FDestinationPrimaryKeyInfo[FieldCount].FieldName).Value,
       FDestinationPrimaryKeyInfo[FieldCount].FieldDataType));

    if FieldCount < FDestinationPrimaryKey.Count - 1 then
       FQueryRecordExist.SQL[FQueryRecordExist.SQL.Count - 1] :=
       FQueryRecordExist.SQL[FQueryRecordExist.SQL.Count - 1] + ' AND';
  end;

  FQueryRecordExist.Open;
  if FQueryRecordExist.Fields[0].AsInteger > 0 then Result := True;
  FQueryRecordExist.Close;

end;

function TSQLDataPump.Execute: LongInt;
begin
  Result := -1;
  if SQLDataPumpMode = sdpTableToTable then Result := ExecuteTableToTable;
  if SQLDataPumpMode = sdpTableToAscii then Result := ExecuteTableToAscii;
  if SQLDataPumpMode = sdpAsciiToTable then Result := ExecuteAsciiToTable;
end;

function TSQLDataPump.ExecuteTableToTable: LongInt;
var
  FieldCount, ItemCount, ConfirmDelete, ExceptionCount: Integer;
  IsTableEmpty, ClearDestTable, PrimaryKeyError: Boolean;
  FileStream: TFileStream;
  FileBuffer: PChar;
  WriteToFile: String;
  TransDesc: TTransactionDesc;
  SQLProcessMode: Integer; // 0=Skip, 1=Insert, 2=Update, 3=Delete, 4=AlwaysInsert
begin
  Result := -1;
  
  FReadCount := 0;
  FInsertCount := 0;
  FUpdateCount := 0;
  FDeleteCount := 0;
  ClearDestTable := False;
  ExceptionCount := 0;
  FileStream := nil;
  FStatisticsPanel := nil;

  try

    if FShowRunningStatistic <> dmsNone then begin
      FStatisticsPanel := TStatisticsPanel.Create(Self);
      if Self.Owner is TWinControl then FStatisticsPanel.Parent := TWinControl(Self.Owner);
      SetStatisticsPanel;
      FStatisticsPanel.Caption := 'Preparing Data Pump Process';
      FStatisticsPanel.Refresh;
    end;

    // Open Exception File
    if (ExceptionFileName > #32) and (FExceptionFileAction <> efaNone) then begin

      if FileExists(ExceptionFileName) = True then begin
        if FExceptionFileAction = efaCreate then begin
          FileStream := TFileStream.Create(ExceptionFileName, fmCreate);
          end
        else begin
          FileStream := TFileStream.Create(ExceptionFileName, fmOpenReadWrite);
        end;
        end
      else begin
        FileStream := TFileStream.Create(ExceptionFileName, fmCreate);
      end;

      WriteToFile := #13#10 + #13#10 + '*************** Start Log ' + FormatDateTime('dd-mmm-yyyy hh:mm:ss AM/PM', Now) +
         ' ***************' + #13#10;
      FileBuffer := PChar(WriteToFile);
      FileStream.Seek(0, soFromEnd);
      FileStream.Write(FileBuffer^, Length(WriteToFile));
    end;

    SQLProcessMode := 0;
    if SQLMetaDataSource = nil then
       raise Exception.Create('Source Metadata not set. Unable to continue.');
    if SQLMetaDataDestination = nil then
       raise Exception.Create('Destination Metadata not set. Unable to continue.');

    if SQLMetaDataSource.Connected = False then SQLMetaDataSource.Open;
    if SQLMetaDataDestination.Connected = False then SQLMetaDataDestination.Open;

    if DestinationTable = '' then
       raise Exception.Create('Destination Table not set. Unable to continue.');
    if SQLSource.Count = 0 then
       raise Exception.Create('Source SQL not set. Unable to continue.');
    if FDestinationPrimaryKey.Count > 16 then
       raise Exception.Create('System only supports up to 16 columns in a primary key. Unable to continue.');

    // Set Metadata Information for Destination Table
    DestinationFields.SetMetaData;

    // Set Primary Key Information for Destination Table
    SQLMetaDataDestination.GetPrimaryKeyFieldNames(DestinationTable, FDestinationPrimaryKey);
    if FDestinationPrimaryKey.Count = 0 then
       raise Exception.Create('Destination Table does not have a primary key. Unable to continue.');
    FDestinationPrimaryKeyCount := FDestinationPrimaryKey.Count;
    for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
      FDestinationPrimaryKeyInfo[FieldCount].FieldName := FDestinationPrimaryKey[FieldCount];
      FDestinationPrimaryKeyInfo[FieldCount].FieldDataType :=
         SQLMetaDataDestination.GetFieldMetaData(DestinationTable, FDestinationPrimaryKey[FieldCount]).ColumnDataType;
    end;


    // Set Source Query and Record Exist Query.
    FQuerySource.Close;
    FQuerySource.SQLConnection := TSQLConnection(SQLMetaDataSource);
    FQueryRecordExist.Close;
    FQueryRecordExist.SQLConnection := TSQLConnection(SQLMetaDataDestination);

    // Is Table Empty
    IsTableEmpty := False;
    FQueryRecordExist.SQL.Clear;
    FQueryRecordExist.SQL.Add('SELECT COUNT(*) FROM ' + DestinationTable);
    FQueryRecordExist.Open;
    if FQueryRecordExist.Fields[0].AsInteger = 0 then IsTableEmpty := True;
    FQueryRecordExist.Close;

    // Verify if table should be cleared
    if IsTableEmpty = False then begin
      ClearDestTable := False;
      if ClearDestination = True then begin
        if ConfirmClear = True then begin
          ConfirmDelete := Application.MessageBox
             ('Delete all data in the Destination Table?', 'Delete All Data',
             MB_YESNOCANCEL	 + mb_DefButton2 + mb_IconQuestion);
          if ConfirmDelete = 6 then ClearDestTable := True;
          if ConfirmDelete = 2 then Exit;  // Should this raise an exception?
          end
        else begin
          ClearDestTable := True;
        end;
      end;
      end
    else begin
      SQLProcessMode := 4;  // If Destination table is empty, always insert.
    end;

    if DataMoveMode = dmAlwaysInsert then SQLProcessMode := 4;
    if DataMoveMode = dmDelete  then SQLProcessMode := 3;

    // Set FSQLInsert
    FSQLInsert.Clear;
    FSQLInsert.Add('INSERT INTO ' + DestinationTable + ' (');
    for FieldCount := 0 to DestinationFields.Count - 1 do begin
      FSQLInsert.Add(DestinationFields.Items[FieldCount].FFieldName);
      if FieldCount < DestinationFields.Count - 1 then
         FSQLInsert[FSQLInsert.Count - 1] :=
         FSQLInsert[FSQLInsert.Count - 1] + ' ,';
    end;
    FSQLInsert.Add(') VALUES ( ');
    for FieldCount := 0 to DestinationFields.Count - 1 do begin
      FSQLInsert.Add('value_for_' + LowerCase(DestinationFields.Items[FieldCount].FFieldName));
      if FieldCount < DestinationFields.Count - 1 then
         FSQLInsert[FSQLInsert.Count - 1] :=
         FSQLInsert[FSQLInsert.Count - 1] + ' ,';
      if FieldCount = DestinationFields.Count - 1 then
         FSQLInsert[FSQLInsert.Count - 1] :=
         FSQLInsert[FSQLInsert.Count - 1] + ')';
    end;

    // Set FSQLUpdate
    FSQLUpdate.Clear;
    FSQLUpdate.Add('UPDATE ' + DestinationTable + ' SET');
    for FieldCount := 0 to DestinationFields.Count - 1 do begin
      FSQLUpdate.Add(DestinationFields.Items[FieldCount].FFieldName + ' = ' +
         'value_for_' + LowerCase(DestinationFields.Items[FieldCount].FFieldName));
      if FieldCount < DestinationFields.Count - 1 then
         FSQLUpdate[FSQLUpdate.Count - 1] :=
         FSQLUpdate[FSQLUpdate.Count - 1] + ' ,';
    end;
    FSQLUpdate.Add('WHERE');
    for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
      FSQLUpdate.Add(FDestinationPrimaryKeyInfo[FieldCount].FieldName +
         ' = value_for_pk');
      if FieldCount < FDestinationPrimaryKey.Count - 1 then
         FSQLUpdate[FSQLUpdate.Count - 1] :=
         FSQLUpdate[FSQLUpdate.Count - 1] + ' AND';
    end;

    TransDesc.GlobalID := 0;
    TransDesc.TransactionID := 1;
    TransDesc.IsolationLevel := xilREADCOMMITTED;

    try

      // Clear Destination Table
      if ClearDestTable = True then begin
        if SQLMetaDataDestination.DriverName = 'Oracle' then begin
          SQLMetaDataDestination.ExecuteDirect('TRUNCATE TABLE ' + DestinationTable);
          // Truncate table may only work if you own the table or have resource rights.
          // This is faster, but may cause more problems then it is worth.
          end
        else begin
          SQLMetaDataDestination.ExecuteDirect('DELETE FROM ' + DestinationTable);
        end;
        SQLProcessMode := 4;
      end;

      // Set Source TSQLQuery Object
      FQuerySource.SQL := SQLSource;
      if FQuerySource.SQLConnection.Connected = False then
         FQuerySource.SQLConnection.Open;
      FQuerySource.Open;

      // Check for Primary Key Fields
      for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
        PrimaryKeyError := True;
        for ItemCount := 0 to DestinationFields.Count -1 do begin
          if FDestinationPrimaryKey[FieldCount] = DestinationFields.Items[ItemCount].FFieldName then begin
             PrimaryKeyError := False;
             Break;
          end;
        end;
        if PrimaryKeyError = True then begin
          raise Exception.Create('Primary Key Fields must be part of the Destination fields. Unable to continue.');
        end;
      end;

      for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
        PrimaryKeyError := True;
        for ItemCount := 0 to FQuerySource.Fields.Count -1 do begin
          if FDestinationPrimaryKey[FieldCount] = FQuerySource.Fields[ItemCount].FieldName then begin
             PrimaryKeyError := False;
             Break;
          end;
        end;
        if PrimaryKeyError = True then begin
          raise Exception.Create('Primary Key Fields must be part of the Source fields. Unable to continue.');
        end;
      end;

      if FShowRunningStatistic <> dmsNone then begin
        FStatisticsPanel.Caption := 'Starting Data Pumping Process';
        FStatisticsPanel.Refresh;
      end;

      while not(FQuerySource.Eof) do begin
        if SQLMetaDataDestination.InTransaction = False then
           SQLMetaDataDestination.StartTransaction(TransDesc);

        // Set Destination Values
        for FieldCount := 0 to Self.DestinationFields.Count - 1 do begin
          if DestinationFields[FieldCount].SourceValueType = fvtLiteral then begin
            DestinationFields[FieldCount].SetValue(DestinationFields[FieldCount].SourceLiteral);
            end
          else begin
            DestinationFields[FieldCount].SetValue(
               FQuerySource.FieldByName(DestinationFields[FieldCount].SourceColumn).AsVariant);
          end;

          //Put Before Record Pump Code here
          DestinationFields[FieldCount].DoBeforeFieldPump(FieldCount, DestinationFields[FieldCount]);

        end;

        // Set SQL Process Mode
        if (SQLProcessMode <> 3) and (SQLProcessMode <> 4) then begin
          SQLProcessMode := 0;
          if DoRecordExist = True then begin
            if (DataMoveMode = dmUpdate) or (DataMoveMode = dmAppendUpdate) then
               SQLProcessMode := 2;
            end
          else begin
            if (DataMoveMode = dmAppend) or (DataMoveMode = dmAppendUpdate) then
               SQLProcessMode := 1;
          end;
        end;

        Inc(FReadCount);

        // 0=Skip, 1=Insert, 2=Update, 3=Delete, 4=AlwaysInsert
        case SQLProcessMode of
        1,4:  begin //DoInsert;
                for FieldCount := 0 to DestinationFields.Count - 1 do begin
                  FSQLInsert[FieldCount + DestinationFields.Count + 2] :=
                     SetSQLValue(DestinationFields.Items[FieldCount].Value,
                     DestinationFields.Items[FieldCount].FieldDataType);
                  if FieldCount < DestinationFields.Count - 1 then
                     FSQLInsert[FieldCount + DestinationFields.Count + 2] :=
                     FSQLInsert[FieldCount + DestinationFields.Count + 2] + ' ,';
                  if FieldCount = DestinationFields.Count - 1 then
                     FSQLInsert[FieldCount + DestinationFields.Count + 2] :=
                     FSQLInsert[FieldCount + DestinationFields.Count + 2] + ' )';
                end;
                FCurrentSQLStatement := FSQLInsert.Text;
                Inc(FInsertCount);
              end;
        2  :  begin //DoUpdate;
                for FieldCount := 0 to DestinationFields.Count - 1 do begin
                  FSQLUpdate[FieldCount + 1] :=
                     DestinationFields.Items[FieldCount].FieldName + ' = ' +
                     SetSQLValue(DestinationFields.Items[FieldCount].Value,
                     DestinationFields.Items[FieldCount].FieldDataType);
                  if FieldCount < DestinationFields.Count - 1 then
                     FSQLUpdate[FieldCount + 1] :=
                     FSQLUpdate[FieldCount + 1] + ' ,';
                end;
                // Set WHERE clause
                for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
                  FSQLUpdate[FieldCount + DestinationFields.Count + 2] :=
                     FDestinationPrimaryKeyInfo[FieldCount].FieldName + ' = ' +
                     SetSQLValue(FQuerySource.FieldByName(FDestinationPrimaryKeyInfo[FieldCount].FieldName).Value,
                     FDestinationPrimaryKeyInfo[FieldCount].FieldDataType);

                  if FieldCount < FDestinationPrimaryKey.Count - 1 then
                     FSQLUpdate[FieldCount + DestinationFields.Count + 2] :=
                     FSQLUpdate[FieldCount + DestinationFields.Count + 2] + ' AND';
                end;
                FCurrentSQLStatement := FSQLUpdate.Text;
                Inc(FUpdateCount);
              end;
        3  :  begin //DoDelete;
                FSQLDelete.Clear;
                FSQLDelete.Add('DELETE FROM ' + DestinationTable + ' WHERE');
                // Set WHERE clause
                for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
                  FSQLDelete.Add(FDestinationPrimaryKeyInfo[FieldCount].FieldName + ' = ' +
                     SetSQLValue(FQuerySource.FieldByName(FDestinationPrimaryKeyInfo[FieldCount].FieldName).Value,
                     FDestinationPrimaryKeyInfo[FieldCount].FieldDataType));
                  if FieldCount < FDestinationPrimaryKey.Count - 1 then
                     FSQLDelete[FSQLDelete.Count - 1] :=
                     FSQLDelete[FSQLDelete.Count - 1] + ' AND';
                end;
                FCurrentSQLStatement := FSQLDelete.Text;
                Inc(FDeleteCount);
              end;
        end; // End Case

        try  // For Abort On Exception and Write To Exception File Name
          if FCurrentSQLStatement > #32 then
             SQLMetaDataDestination.ExecuteDirect(FCurrentSQLStatement);
        except
          on E:EDatabaseError do begin
            ExceptionCount := ExceptionCount + 1;
            if SQLMetaDataDestination.InTransaction = True then
               SQLMetaDataDestination.Rollback(TransDesc);
            if FileStream <> nil then begin
              WriteToFile := #13#10 + E.Message + #13#10+ #13#10 +
                 GetCurrentSQLStatement + #13#10;
              FileBuffer := PChar(WriteToFile);
              FileStream.Seek(0, soFromEnd);
              FileStream.Write(FileBuffer^, Length(WriteToFile));
            end;
            if FAbortOnException = True then raise;
          end;
        else
          if SQLMetaDataDestination.InTransaction = True then
             SQLMetaDataDestination.Rollback(TransDesc);
          raise;
        end;
        FCurrentSQLStatement := '';
        FQuerySource.Next;

        // Do Commmit Count
        if FCommitCount > 0 then begin
          if GetWriteCount div FCommitCount = GetWriteCount/FCommitCount then begin
             SQLMetaDataDestination.Commit(TransDesc);
          end;
        end;

        //  Show Running Statistics
        if (FStatisticsPanel <> nil) and (FStatisticsInterval > 0) then begin
          if FReadCount div FStatisticsInterval = FReadCount/FStatisticsInterval then begin
            if FShowRunningStatistic = dmsReadWrite then begin
              FStatisticsPanel.Caption := 'Records Read: ' + IntToSTr(FReadCount) +
                 '   Records Written: ' + IntToStr(GetWriteCount);
            end;
            if FShowRunningStatistic = dmsReadUpdateInsert then begin
               FStatisticsPanel.Caption := 'Records Read: ' + IntToSTr(FReadCount) +
                 '   Records Updated: ' + IntToStr(FUpdateCount) + '   Records Inserted: ' + IntToStr(FInsertCount);
            end;
            FStatisticsPanel.Refresh;
          end;
        end;

      end;  // End While Loop

      if SQLMetaDataDestination.InTransaction = True then
         SQLMetaDataDestination.Commit(TransDesc);

      FQuerySource.Close;

      Result := GetWriteCount;

      if (FileStream <> nil) and (ExceptionCount = 0) then begin
        WriteToFile := #13#10 + '          **********  No Errors Logged ' +
         '**********     ' + #13#10;
        FileBuffer := PChar(WriteToFile);
        FileStream.Seek(0, soFromEnd);
        FileStream.Write(FileBuffer^, Length(WriteToFile));
      end;

      if FStatisticsPanel <> nil then begin
        FStatisticsPanel.Caption := '';
        FStatisticsPanel.Visible := False;
      end;

      //  Show Summary Statistics

      if FShowSummaryStatistic = dmsReadWrite then begin
        Application.MessageBox(PChar('Records Read:  ' + IntToStr(FReadCount) + '   ' + #13#10 +
                  'Records Written:  ' + IntToStr(GetWriteCount) + '   '), 'Summary Statistics',
             MB_OK + MB_DEFBUTTON1 + MB_ICONINFORMATION);;
      end;
      if FShowSummaryStatistic = dmsReadUpdateInsert then begin
         Application.MessageBox(PChar('Records Read: ' + IntToSTr(FReadCount) + '   ' + #13#10 +
             'Records Upate: ' + IntToStr(FUpdateCount) + '   ' + #13#10 +
             'Records Insert: ' + IntToStr(FInsertCount) + '   '), 'Summary Statistics',
             MB_OK + MB_DEFBUTTON1 + MB_ICONINFORMATION);
      end;

    except
      on E: EDatabaseError do begin
        if SQLMetaDataDestination.InTransaction = True then
           SQLMetaDataDestination.Rollback(TransDesc);
        FQuerySource.Close;
        if FileStream <> nil then begin
          WriteToFile := #13#10 + E.Message + #13#10+ #13#10 +
             FQuerySource.SQL.Text + #13#10;
          FileBuffer := PChar(WriteToFile);
          FileStream.Seek(0, soFromEnd);
          FileStream.Write(FileBuffer^, Length(WriteToFile));
        end;
        raise;
      end;
    else
      if SQLMetaDataDestination.InTransaction = True then
         SQLMetaDataDestination.Rollback(TransDesc);
      FQuerySource.Close;
      raise;
    end;
  finally
    FStatisticsPanel.Free;
    if FileStream <> nil then begin
      WriteToFile := #13#10 + '*************** End Log ' + FormatDateTime('dd-mmm-yyyy hh:mm:ss AM/PM', Now) +
         ' ***************' + #13#10;
      FileBuffer := PChar(WriteToFile);
      FileStream.Seek(0, soFromEnd);
      FileStream.Write(FileBuffer^, Length(WriteToFile));
      FileStream.Free;
    end;
  end;
end;

function TSQLDataPump.ExecuteTableToAscii: LongInt;
var
  {FieldCount, ItemCount,} ConfirmDelete, ExceptionCount: Integer;
  {IsTableEmpty,} ClearDestTable{, PrimaryKeyError}: Boolean;
  AsciiStream, FileStream: TFileStream;
  aBufferArray: array[0..$10000] of char;
  TempChar, AsciiBuffer, FileBuffer: PChar;
  TempPos, TempSize: integer;
  WriteToFile, aStr: String;
  TransDesc: TTransactionDesc;
  i: integer;
  //SQLProcessMode: Integer; // 0=Skip, 1=Insert, 2=Update, 3=Delete, 4=AlwaysInsert
  AsciiBufSize, AsciiBufPos: Int64;
begin
  Result := -1;
  Exit;

  FReadCount := 0;
  FInsertCount := 0;
  FUpdateCount := 0;
  FDeleteCount := 0;
  ClearDestTable := False;
  ExceptionCount := 0;
  FileStream := nil;
  FStatisticsPanel := nil;

  try

    if FShowRunningStatistic <> dmsNone then begin
      FStatisticsPanel := TStatisticsPanel.Create(Self);
      if Self.Owner is TWinControl then FStatisticsPanel.Parent := TWinControl(Self.Owner);
      SetStatisticsPanel;
      FStatisticsPanel.Caption := 'Preparing Data Pump Process';
      FStatisticsPanel.Refresh;
    end;

    // Open Exception File
    if (ExceptionFileName > #32) and (FExceptionFileAction <> efaNone) then begin

      if FileExists(ExceptionFileName) = True then begin
        if FExceptionFileAction = efaCreate then begin
          FileStream := TFileStream.Create(ExceptionFileName, fmCreate);
          end
        else begin
          FileStream := TFileStream.Create(ExceptionFileName, fmOpenReadWrite);
        end;
        end
      else begin
        FileStream := TFileStream.Create(ExceptionFileName, fmCreate);
      end;

      WriteToFile := #13#10 + #13#10 + '*************** Start Log ' + FormatDateTime('dd-mmm-yyyy hh:mm:ss AM/PM', Now) +
         ' ***************' + #13#10;
      FileBuffer := PChar(WriteToFile);
      FileStream.Seek(0, soFromEnd);
      FileStream.Write(FileBuffer^, Length(WriteToFile));
    end;

    //SQLProcessMode := 0;

    if SQLMetaDataSource = nil then
       raise Exception.Create('Source Metadata not set. Unable to continue.');
    //if SQLMetaDataDestination = nil then
    //   raise Exception.Create('Destination Metadata not set. Unable to continue.');

    if SQLMetaDataSource.Connected = False then SQLMetaDataSource.Open;
    //if SQLMetaDataDestination.Connected = False then SQLMetaDataDestination.Open;

   // if DestinationTable = '' then
   //    raise Exception.Create('Destination Table not set. Unable to continue.');
    if SQLSource.Count = 0 then
       raise Exception.Create('Source SQL not set. Unable to continue.');
   // if FDestinationPrimaryKey.Count > 16 then
   //    raise Exception.Create('System only supports up to 16 columns in a primary key. Unable to continue.');

    // Set Metadata Information for Destination Table
    //DestinationFields.SetMetaData;

    // Set Primary Key Information for Destination Table
    //SQLMetaDataDestination.GetPrimaryKeyFieldNames(DestinationTable, FDestinationPrimaryKey);
    //if FDestinationPrimaryKey.Count = 0 then
    //   raise Exception.Create('Destination Table does not have a primary key. Unable to continue.');
    //FDestinationPrimaryKeyCount := FDestinationPrimaryKey.Count;
    //for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
    //  FDestinationPrimaryKeyInfo[FieldCount].FieldName := FDestinationPrimaryKey[FieldCount];
    //  FDestinationPrimaryKeyInfo[FieldCount].FieldDataType :=
    //     SQLMetaDataDestination.GetFieldMetaData(DestinationTable, FDestinationPrimaryKey[FieldCount]).ColumnDataType;
    //end;


    // Set Source Query and Record Exist Query.
    FQuerySource.Close;
    FQuerySource.SQLConnection := TSQLConnection(SQLMetaDataSource);
    //FQueryRecordExist.Close;
    //FQueryRecordExist.SQLConnection := TSQLConnection(SQLMetaDataDestination);

    // Is Table Empty
    //IsTableEmpty := False;
    //FQueryRecordExist.SQL.Clear;
    //FQueryRecordExist.SQL.Add('SELECT COUNT(*) FROM ' + DestinationTable);
    //FQueryRecordExist.Open;
    //if FQueryRecordExist.Fields[0].AsInteger = 0 then IsTableEmpty := True;
    //FQueryRecordExist.Close;

    //  Does File Exit
    // **************************** Add code here  **********************************

    // Verify if Ascii Files should be recreated / deleted
    if FileExists(AsciiFileName) then begin
      if ClearDestination = True then begin
        ClearDestTable := False;
        if ConfirmClear = True then begin
          ConfirmDelete := Application.MessageBox
             ('Delete all data in the Destination File?', 'Delete All Data',
             MB_YESNOCANCEL	 + mb_DefButton2 + mb_IconQuestion);
          if ConfirmDelete = 6 then begin
            ClearDestTable := True;
            DeleteFile(AsciiFileName);
          end;
          if ConfirmDelete = 2 then Exit;  // Should this raise an exception?
          end
        else begin
          ClearDestTable := True;
          DeleteFile(AsciiFileName);
        end;
      end;
      end
    else begin
      ClearDestTable := True;  // If Destination File does not exits, always create.
    end;

    //if DataMoveMode = dmAlwaysInsert then SQLProcessMode := 4;
    //if DataMoveMode = dmDelete  then SQLProcessMode := 3;

   { // Set FSQLInsert
    FSQLInsert.Clear;
    FSQLInsert.Add('INSERT INTO ' + DestinationTable + ' (');
    for FieldCount := 0 to DestinationFields.Count - 1 do begin
      FSQLInsert.Add(DestinationFields.Items[FieldCount].FFieldName);
      if FieldCount < DestinationFields.Count - 1 then
         FSQLInsert[FSQLInsert.Count - 1] :=
         FSQLInsert[FSQLInsert.Count - 1] + ' ,';
    end;
    FSQLInsert.Add(') VALUES ( ');
    for FieldCount := 0 to DestinationFields.Count - 1 do begin
      FSQLInsert.Add('value_for_' + LowerCase(DestinationFields.Items[FieldCount].FFieldName));
      if FieldCount < DestinationFields.Count - 1 then
         FSQLInsert[FSQLInsert.Count - 1] :=
         FSQLInsert[FSQLInsert.Count - 1] + ' ,';
      if FieldCount = DestinationFields.Count - 1 then
         FSQLInsert[FSQLInsert.Count - 1] :=
         FSQLInsert[FSQLInsert.Count - 1] + ')';
    end;

    // Set FSQLUpdate
    FSQLUpdate.Clear;
    FSQLUpdate.Add('UPDATE ' + DestinationTable + ' SET');
    for FieldCount := 0 to DestinationFields.Count - 1 do begin
      FSQLUpdate.Add(DestinationFields.Items[FieldCount].FFieldName + ' = ' +
         'value_for_' + LowerCase(DestinationFields.Items[FieldCount].FFieldName));
      if FieldCount < DestinationFields.Count - 1 then
         FSQLUpdate[FSQLUpdate.Count - 1] :=
         FSQLUpdate[FSQLUpdate.Count - 1] + ' ,';
    end;
    FSQLUpdate.Add('WHERE');
    for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
      FSQLUpdate.Add(FDestinationPrimaryKeyInfo[FieldCount].FieldName +
         ' = value_for_pk');
      if FieldCount < FDestinationPrimaryKey.Count - 1 then
         FSQLUpdate[FSQLUpdate.Count - 1] :=
         FSQLUpdate[FSQLUpdate.Count - 1] + ' AND';
    end;

    TransDesc.GlobalID := 0;
    TransDesc.TransactionID := 1;
    TransDesc.IsolationLevel := xilREADCOMMITTED;
       }
    try
      // Set Source TSQLQuery Object
      FQuerySource.SQL := SQLSource;
      if FQuerySource.SQLConnection.Connected = False then
         FQuerySource.SQLConnection.Open;
      FQuerySource.Open;

      if FStatisticsPanel <> nil then begin
        FStatisticsPanel.Caption := 'Starting Data Pumping Process';
        FStatisticsPanel.Refresh;
      end;

      if not(AsciiDataDef.Count > 0) then begin
        AsciiDataDef.Clear;
        for i := 0 to FQuerySource.FieldCount - 1 do begin
           AsciiDataDef.Add;
           AsciiDataDef.Items[i].SetAsciiFieldName(FQuerySource.Fields[i].FieldName);
        end;
      end;
      for i := 0 to FQuerySource.FieldCount - 1 do
         AsciiDataDef.Items[i].SetExportDataType(FQuerySource.Fields[i].DataType);

      AsciiStream := nil;
      try

        try
          // Open Destination File
          if ClearDestTable = False then begin
            AsciiStream := TFileStream.Create(AsciiFileName , fmOpenReadWrite);
            AsciiStream.Seek(0, soFromEnd);
            end
          else begin
            AsciiStream := TFileStream.Create(AsciiFileName , fmCreate);
          end;

          AsciiBuffer := aBufferArray;
          AsciiBufSize := Length(aBufferArray);
          AsciiBufPos := 0;
          while not(FQuerySource.Eof) do begin
            for i := 0 to FQuerySource.Fields.Count -1 do begin

              if AsciiRecordFormat = ffCommaDoubleQuote then begin
                AsciiBuffer[AsciiBufPos] := FAsciiDelimiter;
                Inc(AsciiBufPos);
                if AsciiBufPos = AsciiBufSize then begin
                  AsciiStream.WriteBuffer(AsciiBuffer^, AsciiBufPos);
                  AsciiBufPos := 0;
                end;
              end;

              AsciiDataDef.Items[i].Value := FQuerySource.Fields[i].Value;
              if VarIsNull(AsciiDataDef.Items[i].Value) = False then begin
                aStr := VarAsType(AsciiDataDef.Items[i].Value, varString);
                end
              else begin
                aStr := '';
              end;
              TempSize := Length(aStr);
              TempChar := PChar(aStr);
              for TempPos := 0 to TempSize -1 do begin
                AsciiBuffer[AsciiBufPos] := TempChar[TempPos];
                Inc(AsciiBufPos);
                if AsciiBufPos = AsciiBufSize then begin
                  AsciiStream.WriteBuffer(AsciiBuffer^, AsciiBufPos);
                  AsciiBufPos := 0;
                end;
              end;
              if AsciiRecordFormat = ffCommaDoubleQuote then begin
                AsciiBuffer[AsciiBufPos] := FAsciiDelimiter;
                Inc(AsciiBufPos);
                if AsciiBufPos = AsciiBufSize then begin
                  AsciiStream.WriteBuffer(AsciiBuffer^, AsciiBufPos);
                  AsciiBufPos := 0;
                end;

                if i < FQuerySource.Fields.Count -1 then begin
                  AsciiBuffer[AsciiBufPos] := FAsciiSeparator;
                  Inc(AsciiBufPos);
                  if AsciiBufPos = AsciiBufSize then begin
                    AsciiStream.WriteBuffer(AsciiBuffer^, AsciiBufPos);
                    AsciiBufPos := 0;
                  end;
                end;
              end;

            end;

            {if SQLMetaDataDestination.InTransaction = False then
               SQLMetaDataDestination.StartTransaction(TransDesc);  }

             // Set Ascii Field Values


           { // Set Destination Values
            for FieldCount := 0 to Self.DestinationFields.Count - 1 do begin
              if DestinationFields[FieldCount].SourceValueType = fvtLiteral then begin
                DestinationFields[FieldCount].SetValue(DestinationFields[FieldCount].SourceLiteral);
                end
              else begin
                DestinationFields[FieldCount].SetValue(
                   FQuerySource.FieldByName(DestinationFields[FieldCount].SourceColumn).AsVariant);
              end;

              //Put Before Record Pump Code here
              DestinationFields[FieldCount].DoBeforeFieldPump(FieldCount, DestinationFields[FieldCount]);

            end;

            // Set SQL Process Mode
            if (SQLProcessMode <> 3) and (SQLProcessMode <> 4) then begin
              SQLProcessMode := 0;
              if DoRecordExist = True then begin
                if (DataMoveMode = dmUpdate) or (DataMoveMode = dmAppendUpdate) then
                   SQLProcessMode := 2;
                end
              else begin
                if (DataMoveMode = dmAppend) or (DataMoveMode = dmAppendUpdate) then
                   SQLProcessMode := 1;
              end;
            end;  }

            Inc(FReadCount);
            Inc(FInsertCount);
            {// 0=Skip, 1=Insert, 2=Update, 3=Delete, 4=AlwaysInsert
            case SQLProcessMode of
            1,4:  begin //DoInsert;
                    for FieldCount := 0 to DestinationFields.Count - 1 do begin
                      FSQLInsert[FieldCount + DestinationFields.Count + 2] :=
                         SetSQLValue(DestinationFields.Items[FieldCount].Value,
                         DestinationFields.Items[FieldCount].FieldDataType);
                      if FieldCount < DestinationFields.Count - 1 then
                         FSQLInsert[FieldCount + DestinationFields.Count + 2] :=
                         FSQLInsert[FieldCount + DestinationFields.Count + 2] + ' ,';
                      if FieldCount = DestinationFields.Count - 1 then
                         FSQLInsert[FieldCount + DestinationFields.Count + 2] :=
                         FSQLInsert[FieldCount + DestinationFields.Count + 2] + ' )';
                    end;
                    FCurrentSQLStatement := FSQLInsert.Text;
                    Inc(FInsertCount);
                  end;
            2  :  begin //DoUpdate;
                    for FieldCount := 0 to DestinationFields.Count - 1 do begin
                      FSQLUpdate[FieldCount + 1] :=
                         DestinationFields.Items[FieldCount].FieldName + ' = ' +
                         SetSQLValue(DestinationFields.Items[FieldCount].Value,
                         DestinationFields.Items[FieldCount].FieldDataType);
                      if FieldCount < DestinationFields.Count - 1 then
                         FSQLUpdate[FieldCount + 1] :=
                         FSQLUpdate[FieldCount + 1] + ' ,';
                    end;
                    // Set WHERE clause
                    for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
                      FSQLUpdate[FieldCount + DestinationFields.Count + 2] :=
                         FDestinationPrimaryKeyInfo[FieldCount].FieldName + ' = ' +
                         SetSQLValue(FQuerySource.FieldByName(FDestinationPrimaryKeyInfo[FieldCount].FieldName).Value,
                         FDestinationPrimaryKeyInfo[FieldCount].FieldDataType);

                      if FieldCount < FDestinationPrimaryKey.Count - 1 then
                         FSQLUpdate[FieldCount + DestinationFields.Count + 2] :=
                         FSQLUpdate[FieldCount + DestinationFields.Count + 2] + ' AND';
                    end;
                    FCurrentSQLStatement := FSQLUpdate.Text;
                    Inc(FUpdateCount);
                  end;
            3  :  begin //DoDelete;
                    FSQLDelete.Clear;
                    FSQLDelete.Add('DELETE FROM ' + DestinationTable + ' WHERE');
                    // Set WHERE clause
                    for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
                      FSQLDelete.Add(FDestinationPrimaryKeyInfo[FieldCount].FieldName + ' = ' +
                         SetSQLValue(FQuerySource.FieldByName(FDestinationPrimaryKeyInfo[FieldCount].FieldName).Value,
                         FDestinationPrimaryKeyInfo[FieldCount].FieldDataType));
                      if FieldCount < FDestinationPrimaryKey.Count - 1 then
                         FSQLDelete[FSQLDelete.Count - 1] :=
                         FSQLDelete[FSQLDelete.Count - 1] + ' AND';
                    end;
                    FCurrentSQLStatement := FSQLDelete.Text;
                    Inc(FDeleteCount);
                  end;
            end; // End Case}

            try  // For Abort On Exception and Write To Exception File Name
              if FCurrentSQLStatement > #32 then
                 SQLMetaDataDestination.ExecuteDirect(FCurrentSQLStatement);
            except
              on E:EDatabaseError do begin
                ExceptionCount := ExceptionCount + 1;
                if SQLMetaDataDestination.InTransaction = True then
                   SQLMetaDataDestination.Rollback(TransDesc);
                if FileStream <> nil then begin
                  WriteToFile := #13#10 + E.Message + #13#10+ #13#10 +
                     GetCurrentSQLStatement + #13#10;
                  FileBuffer := PChar(WriteToFile);
                  FileStream.Seek(0, soFromEnd);
                  FileStream.Write(FileBuffer^, Length(WriteToFile));
                end;
                if FAbortOnException = True then raise;
              end;
            else
              if SQLMetaDataDestination.InTransaction = True then
                 SQLMetaDataDestination.Rollback(TransDesc);
              raise;
            end;
            FCurrentSQLStatement := '';
            FQuerySource.Next;

            //End of Query
            if not(FQuerySource.Eof) then begin
              if AsciiRecordFormat = ffCommaDoubleQuote then begin
                AsciiBuffer[AsciiBufPos] := FAsciiSeparator;
                Inc(AsciiBufPos);
                if AsciiBufPos = AsciiBufSize then begin
                  AsciiStream.WriteBuffer(AsciiBuffer^, AsciiBufPos);
                  AsciiBufPos := 0;
                end;
              end;

              AsciiBuffer[AsciiBufPos] := #13;
              Inc(AsciiBufPos);
              if AsciiBufPos = AsciiBufSize then begin
                AsciiStream.WriteBuffer(AsciiBuffer^, AsciiBufPos);
                AsciiBufPos := 0;
              end;
              AsciiBuffer[AsciiBufPos] := #10;
              Inc(AsciiBufPos);
              if AsciiBufPos = AsciiBufSize then begin
                AsciiStream.WriteBuffer(AsciiBuffer^, AsciiBufPos);
                AsciiBufPos := 0;
              end;
            end;

            // Do Commmit Count
            {if FCommitCount > 0 then begin
              if GetWriteCount div FCommitCount = GetWriteCount/FCommitCount then begin
                 SQLMetaDataDestination.Commit(TransDesc);
              end;
            end;  }

            //  Show Running Statistics
            if (FStatisticsPanel <> nil) and (FStatisticsInterval > 0) then begin
              if FReadCount div FStatisticsInterval = FReadCount/FStatisticsInterval then begin
                if FShowRunningStatistic = dmsReadWrite then begin
                  FStatisticsPanel.Caption := 'Records Read: ' + IntToSTr(FReadCount) +
                     '   Records Written: ' + IntToStr(GetWriteCount);
                end;
                if FShowRunningStatistic = dmsReadUpdateInsert then begin
                   FStatisticsPanel.Caption := 'Records Read: ' + IntToSTr(FReadCount) +
                     '   Records Updated: ' + IntToStr(FUpdateCount) + '   Records Inserted: ' + IntToStr(FInsertCount);
                end;
                FStatisticsPanel.Refresh;
              end;
            end;

          end;  // End While Loop
          if AsciiBufPos > 0 then AsciiStream.WriteBuffer(AsciiBuffer^, AsciiBufPos);

          Result := GetWriteCount;

        except
          raise;
        end;
      finally
        AsciiStream.Free;
      end;

      {if ClearDestTable = True then begin
        if SQLMetaDataDestination.DriverName = 'Oracle' then begin
          SQLMetaDataDestination.ExecuteDirect('TRUNCATE TABLE ' + DestinationTable);
          // Truncate table may only work if you own the table or have resource rights.
          // This is faster, but may cause more problems then it is worth.
          end
        else begin
          SQLMetaDataDestination.ExecuteDirect('DELETE FROM ' + DestinationTable);
        end;
        SQLProcessMode := 4;
      end;}



      {// Check for Primary Key Fields
      for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
        PrimaryKeyError := True;
        for ItemCount := 0 to DestinationFields.Count -1 do begin
          if FDestinationPrimaryKey[FieldCount] = DestinationFields.Items[ItemCount].FFieldName then begin
             PrimaryKeyError := False;
             Break;
          end;
        end;
        if PrimaryKeyError = True then begin
          raise Exception.Create('Primary Key Fields must be part of the Destination fields. Unable to continue.');
        end;
      end;

      for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
        PrimaryKeyError := True;
        for ItemCount := 0 to FQuerySource.Fields.Count -1 do begin
          if FDestinationPrimaryKey[FieldCount] = FQuerySource.Fields[ItemCount].FieldName then begin
             PrimaryKeyError := False;
             Break;
          end;
        end;
        if PrimaryKeyError = True then begin
          raise Exception.Create('Primary Key Fields must be part of the Source fields. Unable to continue.');
        end;
      end; }


      {if SQLMetaDataDestination.InTransaction = True then
         SQLMetaDataDestination.Commit(TransDesc); }

      FQuerySource.Close;

      if (FileStream <> nil) and (ExceptionCount = 0) then begin
        WriteToFile := #13#10 + '          **********  No Errors Logged ' +
         '**********     ' + #13#10;
        FileBuffer := PChar(WriteToFile);
        FileStream.Seek(0, soFromEnd);
        FileStream.Write(FileBuffer^, Length(WriteToFile));
      end;

      if FStatisticsPanel <> nil then begin
        FStatisticsPanel.Caption := '';
        FStatisticsPanel.Visible := False;
      end;

      //  Show Summary Statistics

      if FShowSummaryStatistic = dmsReadWrite then begin
        Application.MessageBox(PChar('Records Read:  ' + IntToStr(FReadCount) + '   ' + #13#10 +
                  'Records Written:  ' + IntToStr(GetWriteCount) + '   '), 'Summary Statistics',
             MB_OK + MB_DEFBUTTON1 + MB_ICONINFORMATION);;
      end;
      if FShowSummaryStatistic = dmsReadUpdateInsert then begin
         Application.MessageBox(PChar('Records Read: ' + IntToSTr(FReadCount) + '   ' + #13#10 +
             'Records Upate: ' + IntToStr(FUpdateCount) + '   ' + #13#10 +
             'Records Insert: ' + IntToStr(FInsertCount) + '   '), 'Summary Statistics',
             MB_OK + MB_DEFBUTTON1 + MB_ICONINFORMATION);
      end;

    except
      {if SQLMetaDataDestination.InTransaction = True then
         SQLMetaDataDestination.Rollback(TransDesc);}
         FQuerySource.Close;
      raise;
    end;
  finally
    FStatisticsPanel.Free;
    if FileStream <> nil then begin
      WriteToFile := #13#10 + '*************** End Log ' + FormatDateTime('dd-mmm-yyyy hh:mm:ss AM/PM', Now) +
         ' ***************' + #13#10;
      FileBuffer := PChar(WriteToFile);
      FileStream.Seek(0, soFromEnd);
      FileStream.Write(FileBuffer^, Length(WriteToFile));
      FileStream.Free;
    end;
  end;
end;

function TSQLDataPump.ExecuteAsciiToTable: LongInt;
const
  BufferSize = Int64($10000);
var
  FieldCount, ItemCount, ConfirmDelete, ExceptionCount: Integer;
  IsTableEmpty, ClearDestTable, PrimaryKeyError, EndOfField: Boolean;
  AsciiStream, FileStream: TFileStream;
  AsciiBuffer, FileBuffer: PChar;
  TempBufSize, TempBufPos, RecordTail: Integer;
  TempCharArray, TestCharArray: array of char;
  WriteToFile: String;
  TransDesc: TTransactionDesc;
  SQLProcessMode: Integer; // 0=Skip, 1=Insert, 2=Update, 3=Delete, 4=AlwaysInsert
  AsciiSize, AsciiBufSize, AsciiBufPos: Int64;
begin
  Result := -1;

  FReadCount := 0;
  FInsertCount := 0;
  FUpdateCount := 0;
  FDeleteCount := 0;
  ClearDestTable := False;
  ExceptionCount := 0;
  FileStream := nil;
  FStatisticsPanel := nil;

  try

    if FShowRunningStatistic <> dmsNone then begin
      FStatisticsPanel := TStatisticsPanel.Create(Self);
      if Self.Owner is TWinControl then FStatisticsPanel.Parent := TWinControl(Self.Owner);
      SetStatisticsPanel;
      FStatisticsPanel.Caption := 'Preparing Data Pump Process';
      FStatisticsPanel.Refresh;
    end;

    // Open Exception File
    if (ExceptionFileName > #32) and (FExceptionFileAction <> efaNone) then begin

      if FileExists(ExceptionFileName) = True then begin
        if FExceptionFileAction = efaCreate then begin
          FileStream := TFileStream.Create(ExceptionFileName, fmCreate);
          end
        else begin
          FileStream := TFileStream.Create(ExceptionFileName, fmOpenReadWrite);
        end;
        end
      else begin
        FileStream := TFileStream.Create(ExceptionFileName, fmCreate);
      end;

      WriteToFile := #13#10 + #13#10 + '*************** Start Log ' + FormatDateTime('dd-mmm-yyyy hh:mm:ss AM/PM', Now) +
         ' ***************' + #13#10;
      FileBuffer := PChar(WriteToFile);
      FileStream.Seek(0, soFromEnd);
      FileStream.Write(FileBuffer^, Length(WriteToFile));
    end;

    SQLProcessMode := 0;
    if SQLMetaDataDestination = nil then
       raise Exception.Create('Destination Metadata not set. Unable to continue.');
    // NOT_NEEDED FOR ASCII IMPORT  if SQLMetaDataSource = nil then
    //   raise Exception.Create('Source Metadata not set. Unable to continue.');

    // NOT_NEEDED FOR ASCII IMPORT if SQLMetaDataSource.Connected = False then SQLMetaDataSource.Open;
    if SQLMetaDataDestination.Connected = False then SQLMetaDataDestination.Open;

    if DestinationTable = '' then
       raise Exception.Create('Destination Table not set. Unable to continue.');
    // NOT_NEEDED FOR ASCII IMPORT if SQLSource.Count = 0 then
        //raise Exception.Create('Source SQL not set. Unable to continue.');
    if FDestinationPrimaryKey.Count > 16 then
       raise Exception.Create('System only supports up to 16 columns in a primary key. Unable to continue.');

    // Set Metadata Information for Destination Table
    DestinationFields.SetMetaData;

    // Set Primary Key Information for Destination Table
    SQLMetaDataDestination.GetPrimaryKeyFieldNames(DestinationTable, FDestinationPrimaryKey);
    if FDestinationPrimaryKey.Count = 0 then
       raise Exception.Create('Destination Table does not have a primary key. Unable to continue.');
    FDestinationPrimaryKeyCount := FDestinationPrimaryKey.Count;
    for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
      FDestinationPrimaryKeyInfo[FieldCount].FieldName := FDestinationPrimaryKey[FieldCount];
      FDestinationPrimaryKeyInfo[FieldCount].FieldDataType :=
         SQLMetaDataDestination.GetFieldMetaData(DestinationTable, FDestinationPrimaryKey[FieldCount]).ColumnDataType;
    end;


    // Set Source Query and Record Exist Query.
    // NOT_NEEDED FOR ASCII IMPORT FQuerySource.Close;
    // NOT_NEEDED FOR ASCII IMPORT FQuerySource.SQLConnection := TSQLConnection(SQLMetaDataSource);
    FQueryRecordExist.Close;
    FQueryRecordExist.SQLConnection := TSQLConnection(SQLMetaDataDestination);

    // Is Table Empty
    IsTableEmpty := False;
    FQueryRecordExist.SQL.Clear;
    FQueryRecordExist.SQL.Add('SELECT COUNT(*) FROM ' + DestinationTable);
    FQueryRecordExist.Open;
    if FQueryRecordExist.Fields[0].AsInteger = 0 then IsTableEmpty := True;
    FQueryRecordExist.Close;

    // Verify if table should be cleared
    if IsTableEmpty = False then begin
      ClearDestTable := False;
      if ClearDestination = True then begin
        if ConfirmClear = True then begin
          ConfirmDelete := Application.MessageBox
             ('Delete all data in the Destination Table?', 'Delete All Data',
             MB_YESNOCANCEL	 + mb_DefButton2 + mb_IconQuestion);
          if ConfirmDelete = 6 then ClearDestTable := True;
          if ConfirmDelete = 2 then Exit;  // Should this raise an exception?
          end
        else begin
          ClearDestTable := True;
        end;
      end;
      end
    else begin
      SQLProcessMode := 4;  // If Destination table is empty, always insert.
    end;

    if DataMoveMode = dmAlwaysInsert then SQLProcessMode := 4;
    if DataMoveMode = dmDelete  then SQLProcessMode := 3;

    // Set FSQLInsert
    FSQLInsert.Clear;
    FSQLInsert.Add('INSERT INTO ' + DestinationTable + ' (');
    for FieldCount := 0 to DestinationFields.Count - 1 do begin
      FSQLInsert.Add(DestinationFields.Items[FieldCount].FFieldName);
      if FieldCount < DestinationFields.Count - 1 then
         FSQLInsert[FSQLInsert.Count - 1] :=
         FSQLInsert[FSQLInsert.Count - 1] + ' ,';
    end;
    FSQLInsert.Add(') VALUES ( ');
    for FieldCount := 0 to DestinationFields.Count - 1 do begin
      FSQLInsert.Add('value_for_' + LowerCase(DestinationFields.Items[FieldCount].FFieldName));
      if FieldCount < DestinationFields.Count - 1 then
         FSQLInsert[FSQLInsert.Count - 1] :=
         FSQLInsert[FSQLInsert.Count - 1] + ' ,';
      if FieldCount = DestinationFields.Count - 1 then
         FSQLInsert[FSQLInsert.Count - 1] :=
         FSQLInsert[FSQLInsert.Count - 1] + ')';
    end;

    // Set FSQLUpdate
    FSQLUpdate.Clear;
    FSQLUpdate.Add('UPDATE ' + DestinationTable + ' SET');
    for FieldCount := 0 to DestinationFields.Count - 1 do begin
      FSQLUpdate.Add(DestinationFields.Items[FieldCount].FFieldName + ' = ' +
         'value_for_' + LowerCase(DestinationFields.Items[FieldCount].FFieldName));
      if FieldCount < DestinationFields.Count - 1 then
         FSQLUpdate[FSQLUpdate.Count - 1] :=
         FSQLUpdate[FSQLUpdate.Count - 1] + ' ,';
    end;
    FSQLUpdate.Add('WHERE');
    for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
      FSQLUpdate.Add(FDestinationPrimaryKeyInfo[FieldCount].FieldName +
         ' = value_for_pk');
      if FieldCount < FDestinationPrimaryKey.Count - 1 then
         FSQLUpdate[FSQLUpdate.Count - 1] :=
         FSQLUpdate[FSQLUpdate.Count - 1] + ' AND';
    end;

    TransDesc.GlobalID := 0;
    TransDesc.TransactionID := 1;
    TransDesc.IsolationLevel := xilREADCOMMITTED;

    try

      // Clear Destination Table
      if ClearDestTable = True then begin
        if SQLMetaDataDestination.DriverName = 'Oracle' then begin
          SQLMetaDataDestination.ExecuteDirect('TRUNCATE TABLE ' + DestinationTable);
          // Truncate table may only work if you own the table or have resource rights.
          // This is faster, but may cause more problems then it is worth.
          end
        else begin
          SQLMetaDataDestination.ExecuteDirect('DELETE FROM ' + DestinationTable);
        end;
        SQLProcessMode := 4;
      end;

      // Set Source TSQLQuery Object
      // NOT_NEEDED FOR ASCII IMPORT FQuerySource.SQL := SQLSource;
      // NOT_NEEDED FOR ASCII IMPORT if FQuerySource.SQLConnection.Connected = False then
      // NOT_NEEDED FOR ASCII IMPORT   FQuerySource.SQLConnection.Open;
      // NOT_NEEDED FOR ASCII IMPORT FQuerySource.Open;

      // Check for Primary Key Fields
      for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
        PrimaryKeyError := True;
        for ItemCount := 0 to DestinationFields.Count -1 do begin
          if FDestinationPrimaryKey[FieldCount] = DestinationFields.Items[ItemCount].FFieldName then begin
             PrimaryKeyError := False;
             Break;
          end;
        end;
        if PrimaryKeyError = True then begin
          raise Exception.Create('Primary Key Fields must be part of the Destination fields. Unable to continue.');
        end;
      end;

      // NOT_NEEDED FOR ASCII IMPORT
      {for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
        PrimaryKeyError := True;
        for ItemCount := 0 to FQuerySource.Fields.Count -1 do begin
          if FDestinationPrimaryKey[FieldCount] = FQuerySource.Fields[ItemCount].FieldName then begin
             PrimaryKeyError := False;
             Break;
          end;
        end;
        if PrimaryKeyError = True then begin
          raise Exception.Create('Primary Key Fields must be part of the Source fields. Unable to continue.');
        end;
      end; }

      if FStatisticsPanel <> nil then begin
        FStatisticsPanel.Caption := 'Starting Data Pumping Process';
        FStatisticsPanel.Refresh;
      end;

      if not(AsciiDataDef.Count > 0) then AsciiDataDef.AddAll;

      // Start Reading from Ascii File
      AsciiStream := nil;
      ItemCount := 0;
      RecordTail := 0;
      EndOfField := False;
      try
        try
          AsciiStream := TFileStream.Create(AsciiFileName , fmOpenRead);
          AsciiSize := AsciiStream.Size - AsciiStream.Position;
          //Start reading from AsciiFile to AsciiBuffer
          while AsciiSize > 0 do begin
            if AsciiSize > BufferSize then begin
              AsciiBufSize := BufferSize;
              end
            else begin
              AsciiBufSize := AsciiSize;
            end;
            GetMem(AsciiBuffer, AsciiBufSize);
            try
              AsciiStream.Read(AsciiBuffer^, AsciiBufSize);
              AsciiBufPos := 0;
              TempBufSize := 0;
              TempBufPos := Length(TempCharArray);
              //Start reading field values from AsciiBuffer
              while AsciiBufPos < AsciiBufSize do begin
                //Set field values
                if AsciiRecordFormat = ffFixedLength then begin
                  while (AsciiBufSize >= AsciiBufPos + TempBufSize) and
                     (ItemCount < AsciiDataDef.Count) do begin
                    if RecordTail > 0 then begin
                      Inc(AsciiBufPos);
                      Dec(RecordTail);
                      end
                    else begin
                      Inc(TempBufSize);
                    end;
                    if TempBufSize = AsciiDataDef.Items[ItemCount].AsciiFieldSize then begin
                      SetLength(TempCharArray, AsciiDataDef.Items[ItemCount].AsciiFieldSize);
                      while TempBufPos < TempBufSize do begin
                        TempCharArray[TempBufPos] := AsciiBuffer[AsciiBufPos];
                        Inc(AsciiBufPos);
                        Inc(TempBufPos);
                      end;
                      TempBufPos := 0;
                      TempBufSize := 0;
                      if AsciiDataDef.Items[ItemCount].AsciiDataType = dtDate then
                         AsciiDataDef.Items[ItemCount].SetValue(StrToDate(string(TempCharArray)));
                      if AsciiDataDef.Items[ItemCount].AsciiDataType = dtDateTime then AsciiDataDef.Items[ItemCount].SetValue(StrToDateTime(string(TempCharArray)));
                      if AsciiDataDef.Items[ItemCount].AsciiDataType = dtNumber then AsciiDataDef.Items[ItemCount].SetValue(StrToFloat(string(TempCharArray)));
                      if AsciiDataDef.Items[ItemCount].AsciiDataType = dtString then AsciiDataDef.Items[ItemCount].SetValue(string(TempCharArray));
                      if AsciiDataDef.Items[ItemCount].AsciiDataType = dtTime then AsciiDataDef.Items[ItemCount].SetValue(StrToTime(string(TempCharArray)));
                      //if AsciiDataType = dtOther then AsciiDataDef.Items[ItemCount].Items[ItemCount].SetValue(string(TempCharArray));
                      Inc(ItemCount);
                    end;
                  end;
                  if TempBufSize > 0 then begin
                    if TempBufSize >= AsciiBufSize - AsciiBufPos then TempBufSize := AsciiBufSize - AsciiBufPos;
                    SetLength(TempCharArray, TempBufSize);
                    while TempBufPos < TempBufSize do begin
                      TempCharArray[TempBufPos] := AsciiBuffer[AsciiBufPos];
                      Inc(AsciiBufPos);
                      Inc(TempBufPos);
                    end;
                    TempBufPos := 0;
                  end;
                  TempBufSize := 0;
                end;

                if (AsciiRecordFormat <> ffFixedLength) and
                   (AsciiRecordFormat <> ffFieldPerLine) then begin
                  while (AsciiBufSize > AsciiBufPos + TempBufSize) and
                     (ItemCount < AsciiDataDef.Count) do begin
                    if RecordTail > 0 then begin
                      Inc(AsciiBufPos);
                      Dec(RecordTail);
                      end
                    else if (AsciiBuffer[AsciiBufPos + TempBufSize] = FAsciiSeparator) and
                       (EndOfField = True) then begin //End of field is confirmed by Separator
                      //Store Temp Buffer into TempCharArray and move forward AsciiBufPos
                      //SetLength(TempCharArray, Length(TempCharArray) + TempBufSize);
                      TempBufSize := TempBufSize + TempBufPos;
                      SetLength(TempCharArray, TempBufSize);
                      while TempBufPos < TempBufSize do begin
                        TempCharArray[TempBufPos] := AsciiBuffer[AsciiBufPos];
                        Inc(AsciiBufPos);
                        Inc(TempBufPos);
                      end;
                      TempBufPos := 0;
                      TempBufSize := 0;
                      //Remove leading and trailing Delimiters in TempCharArray
                      if Length(TempCharArray) > 2 then begin
                        TestCharArray := Copy(TempCharArray, 1, Length(TempCharArray) -2);
                        if AsciiDataDef.Items[ItemCount].AsciiDataType = dtDate then begin
                          AsciiDataDef.Items[ItemCount].SetValue(StrToDate(string(TestCharArray)));
                        end;
                        if AsciiDataDef.Items[ItemCount].AsciiDataType = dtDateTime then begin
                          AsciiDataDef.Items[ItemCount].SetValue(StrToDateTime(string(TestCharArray)));
                        end;
                        if AsciiDataDef.Items[ItemCount].AsciiDataType = dtNumber then begin
                          AsciiDataDef.Items[ItemCount].SetValue(StrToFloat(string(TestCharArray)));
                        end;
                        if AsciiDataDef.Items[ItemCount].AsciiDataType = dtString then begin
                          AsciiDataDef.Items[ItemCount].SetValue(string(TestCharArray));
                        end;
                        if AsciiDataDef.Items[ItemCount].AsciiDataType = dtTime then begin
                          AsciiDataDef.Items[ItemCount].SetValue(StrToTime(string(TestCharArray)));
                        end;
                        //if AsciiDataType = dtOther then
                           //AsciiDataDef.Items[ItemCount].Items[ItemCount].SetValue(string(Copy(TempCharArray, 1, Length(TempCharArray) -2))));
                        end
                      else begin
                        AsciiDataDef.Items[ItemCount].SetValue(Null);
                      end;

                      //AsciiDataDef.Items[ItemCount].SetValue(string(Copy(TempCharArray, 1, Length(TempCharArray) -2)));
                      Inc(ItemCount);
                      Inc(AsciiBufPos);            //Skip Separator
                      end
                    else begin                       //Current byte is not confirmation Separator
                      if AsciiBuffer[AsciiBufPos + TempBufSize] = FAsciiDelimiter then begin
                        //If previous byte is Delimiter then EndOfField = True
                        //EndOfField is switched to False to signal begin of field
                        //If previous byte is not Delimiter then EndOfField = False
                        //EndOfField is switched to True to signal possible end of field
                        //End of field must be confirmed by the next following Separator
                        EndOfField := True;
                        end
                      else begin
                        EndOfField := False;           //Make sure that end of field is false
                      end;
                      Inc(TempBufSize);              //Include current byte to Temp Buffer
                    end;

                  end;
                  if TempBufSize > 0 then begin
                    //Store Temp Buffer into TempCharArray and move AsciiBufPos
                    if TempBufSize > AsciiBufSize - AsciiBufPos then TempBufSize := AsciiBufSize - AsciiBufPos;
                    TempBufSize := TempBufSize + TempBufPos;
                    SetLength(TempCharArray, TempBufSize);
                    while TempBufPos < TempBufSize do begin
                      TempCharArray[TempBufPos] := AsciiBuffer[AsciiBufPos];
                      Inc(AsciiBufPos);
                      Inc(TempBufPos);
                    end;
                    TempBufPos := 0;
                    TempBufSize := 0;
                    //Last record does not have a separator
                    //This if statement prepares the the last record to be processed
                    if (AsciiBufSize = AsciiSize) and (AsciiBufSize = AsciiBufPos) then begin
                      while (TempCharArray[Length(TempCharArray) - TempBufPos -1] <> FAsciiDelimiter) and
                         (Length(TempCharArray) - TempBufPos > 1) do begin
                        Inc(TempBufPos);
                      end;
                      if (Length(TempCharArray) -2 - TempBufPos) > 0 then begin
                        TestCharArray := Copy(TempCharArray, 1, Length(TempCharArray) -2);
                        if AsciiDataDef.Items[ItemCount].AsciiDataType = dtDate then begin
                          AsciiDataDef.Items[ItemCount].SetValue(StrToDate(string(TestCharArray)));
                        end;
                        if AsciiDataDef.Items[ItemCount].AsciiDataType = dtDateTime then begin
                          AsciiDataDef.Items[ItemCount].SetValue(StrToDateTime(string(TestCharArray)));
                        end;
                        if AsciiDataDef.Items[ItemCount].AsciiDataType = dtNumber then begin
                          AsciiDataDef.Items[ItemCount].SetValue(StrToFloat(string(TestCharArray)));
                        end;
                        if AsciiDataDef.Items[ItemCount].AsciiDataType = dtString then begin
                          AsciiDataDef.Items[ItemCount].SetValue(string(TestCharArray));
                        end;
                        if AsciiDataDef.Items[ItemCount].AsciiDataType = dtTime then begin
                          AsciiDataDef.Items[ItemCount].SetValue(StrToTime(string(TestCharArray)));
                        end;
                        //if AsciiDataType = dtOther then
                           //AsciiDataDef.Items[ItemCount].Items[ItemCount].SetValue(string(Copy(TempCharArray, 1, Length(TempCharArray) -2))));
                        //AsciiDataDef.Items[ItemCount].SetValue(string(Copy(TempCharArray, 1, Length(TempCharArray) -2 - TempBufPos)));
                        end
                      else begin
                        AsciiDataDef.Items[ItemCount].SetValue(Null);
                      end;
                      TempBufPos := 0;
                      Inc(ItemCount);
                    end;
                  end;
                end;
                //End of Set field values

                if ItemCount > (AsciiDataDef.Count - 1) then begin
                //Process record.
                  if AsciiDataDef.Items[0].Value = '0000002711' then ShowMessage('Code: 0000002711');  
                  ItemCount := 0;
                  if AsciiRecordDelimiter = rdDOS then begin
                    RecordTail := 2;
                    end
                  else if AsciiRecordDelimiter = rdUnix then begin
                    RecordTail := 1;
                  end;

                  if SQLMetaDataDestination.InTransaction = False then
                     SQLMetaDataDestination.StartTransaction(TransDesc);

                  // Set Destination Values
                  for FieldCount := 0 to Self.DestinationFields.Count - 1 do begin
                    if DestinationFields[FieldCount].SourceValueType = fvtLiteral then begin
                      DestinationFields[FieldCount].SetValue(DestinationFields[FieldCount].SourceLiteral);
                      end
                    else begin
                      // Change for Ascii to read from stream and set  AsciiDataDef.Value Property
                      DestinationFields[FieldCount].SetValue(AsciiDataDef.Items[FieldCount].Value);
                    end;

                    //Put Before Record Pump Code here
                    DestinationFields[FieldCount].DoBeforeFieldPump(FieldCount, DestinationFields[FieldCount]);

                  end;

                  // Set SQL Process Mode
                  if (SQLProcessMode <> 3) and (SQLProcessMode <> 4) then begin
                    SQLProcessMode := 0;
                    if DoRecordExist = True then begin
                      if (DataMoveMode = dmUpdate) or (DataMoveMode = dmAppendUpdate) then
                         SQLProcessMode := 2;
                      end
                    else begin
                      if (DataMoveMode = dmAppend) or (DataMoveMode = dmAppendUpdate) then
                         SQLProcessMode := 1;
                    end;
                  end;

                  Inc(FReadCount);

                  // 0=Skip, 1=Insert, 2=Update, 3=Delete, 4=AlwaysInsert
                  case SQLProcessMode of
                  1,4:  begin //DoInsert;
                          for FieldCount := 0 to DestinationFields.Count - 1 do begin
                            FSQLInsert[FieldCount + DestinationFields.Count + 2] :=
                               SetSQLValue(DestinationFields.Items[FieldCount].Value,
                               DestinationFields.Items[FieldCount].FieldDataType);
                            if FieldCount < DestinationFields.Count - 1 then
                               FSQLInsert[FieldCount + DestinationFields.Count + 2] :=
                               FSQLInsert[FieldCount + DestinationFields.Count + 2] + ' ,';
                            if FieldCount = DestinationFields.Count - 1 then
                               FSQLInsert[FieldCount + DestinationFields.Count + 2] :=
                               FSQLInsert[FieldCount + DestinationFields.Count + 2] + ' )';
                          end;
                          FCurrentSQLStatement := FSQLInsert.Text;
                          Inc(FInsertCount);
                        end;
                  2  :  begin //DoUpdate;
                          for FieldCount := 0 to DestinationFields.Count - 1 do begin
                            FSQLUpdate[FieldCount + 1] :=
                               DestinationFields.Items[FieldCount].FieldName + ' = ' +
                               SetSQLValue(DestinationFields.Items[FieldCount].Value,
                               DestinationFields.Items[FieldCount].FieldDataType);
                            if FieldCount < DestinationFields.Count - 1 then
                               FSQLUpdate[FieldCount + 1] :=
                               FSQLUpdate[FieldCount + 1] + ' ,';
                          end;
                          // Set WHERE clause
                          for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
                            FSQLUpdate[FieldCount + DestinationFields.Count + 2] :=
                               FDestinationPrimaryKeyInfo[FieldCount].FieldName + ' = ' +
                               SetSQLValue(FQuerySource.FieldByName(FDestinationPrimaryKeyInfo[FieldCount].FieldName).Value,
                               FDestinationPrimaryKeyInfo[FieldCount].FieldDataType);

                            if FieldCount < FDestinationPrimaryKey.Count - 1 then
                               FSQLUpdate[FieldCount + DestinationFields.Count + 2] :=
                               FSQLUpdate[FieldCount + DestinationFields.Count + 2] + ' AND';
                          end;
                          FCurrentSQLStatement := FSQLUpdate.Text;
                          Inc(FUpdateCount);
                        end;
                  3  :  begin //DoDelete;
                          FSQLDelete.Clear;
                          FSQLDelete.Add('DELETE FROM ' + DestinationTable + ' WHERE');
                          // Set WHERE clause
                          for FieldCount := 0 to FDestinationPrimaryKey.Count - 1 do begin
                            FSQLDelete.Add(FDestinationPrimaryKeyInfo[FieldCount].FieldName + ' = ' +
                               SetSQLValue(FQuerySource.FieldByName(FDestinationPrimaryKeyInfo[FieldCount].FieldName).Value,
                               FDestinationPrimaryKeyInfo[FieldCount].FieldDataType));
                            if FieldCount < FDestinationPrimaryKey.Count - 1 then
                               FSQLDelete[FSQLDelete.Count - 1] :=
                               FSQLDelete[FSQLDelete.Count - 1] + ' AND';
                          end;
                          FCurrentSQLStatement := FSQLDelete.Text;
                          Inc(FDeleteCount);
                        end;
                  end; // End Case

                  try  // For Abort On Exception and Write To Exception File Name
                    if FCurrentSQLStatement > #32 then
                       SQLMetaDataDestination.ExecuteDirect(FCurrentSQLStatement);
                  except
                    on E:EDatabaseError do begin
                      ExceptionCount := ExceptionCount + 1;
                      if SQLMetaDataDestination.InTransaction = True then
                         SQLMetaDataDestination.Rollback(TransDesc);
                      if FileStream <> nil then begin
                        WriteToFile := #13#10 + E.Message + #13#10+ #13#10 +
                           GetCurrentSQLStatement + #13#10;
                        FileBuffer := PChar(WriteToFile);
                        FileStream.Seek(0, soFromEnd);
                        FileStream.Write(FileBuffer^, Length(WriteToFile));
                      end;
                      if FAbortOnException = True then raise;
                    end;
                  else
                    if SQLMetaDataDestination.InTransaction = True then
                       SQLMetaDataDestination.Rollback(TransDesc);
                    raise;
                  end;
                  FCurrentSQLStatement := '';
                  // NOT_NEEDED FOR ASCII IMPORT FQuerySource.Next;

                  // Do Commmit Count
                  if FCommitCount > 0 then begin
                    if GetWriteCount div FCommitCount = GetWriteCount/FCommitCount then begin
                       SQLMetaDataDestination.Commit(TransDesc);
                    end;
                  end;

                  //  Show Running Statistics
                  if (FStatisticsPanel <> nil) and (FStatisticsInterval > 0) then begin
                    if FReadCount div FStatisticsInterval = FReadCount/FStatisticsInterval then begin
                      if FShowRunningStatistic = dmsReadWrite then begin
                        FStatisticsPanel.Caption := 'Records Read: ' + IntToSTr(FReadCount) +
                           '   Records Written: ' + IntToStr(GetWriteCount);
                      end;
                      if FShowRunningStatistic = dmsReadUpdateInsert then begin
                         FStatisticsPanel.Caption := 'Records Read: ' + IntToSTr(FReadCount) +
                           '   Records Updated: ' + IntToStr(FUpdateCount) + '   Records Inserted: ' + IntToStr(FInsertCount);
                      end;
                      FStatisticsPanel.Refresh;
                    end;
                  end;

                end; //Process record

              end; //of while loop reading from Ascii Buffer

              Result := GetWriteCount;
            finally
              FreeMem(AsciiBuffer, AsciiBufSize);
            end;
            AsciiSize := AsciiStream.Size - AsciiStream.Position;
          end; //of while loop reading from ASCII File

        except
          raise;
        end;
      finally
        AsciiStream.Free;
      end;
      // End Reading from Ascii File

      if SQLMetaDataDestination.InTransaction = True then
         SQLMetaDataDestination.Commit(TransDesc);

      FQuerySource.Close;

      if (FileStream <> nil) and (ExceptionCount = 0) then begin
        WriteToFile := #13#10 + '          **********  No Errors Logged ' +
         '**********     ' + #13#10;
        FileBuffer := PChar(WriteToFile);
        FileStream.Seek(0, soFromEnd);
        FileStream.Write(FileBuffer^, Length(WriteToFile));
      end;

      if FStatisticsPanel <> nil then begin
        FStatisticsPanel.Caption := '';
        FStatisticsPanel.Visible := False;
      end;  

      //  Show Summary Statistics

      if FShowSummaryStatistic = dmsReadWrite then begin
        Application.MessageBox(PChar('Records Read:  ' + IntToStr(FReadCount) + '   ' + #13#10 +
                  'Records Written:  ' + IntToStr(GetWriteCount) + '   '), 'Summary Statistics',
             MB_OK + MB_DEFBUTTON1 + MB_ICONINFORMATION);;
      end;
      if FShowSummaryStatistic = dmsReadUpdateInsert then begin
         Application.MessageBox(PChar('Records Read: ' + IntToSTr(FReadCount) + '   ' + #13#10 +
             'Records Upate: ' + IntToStr(FUpdateCount) + '   ' + #13#10 +
             'Records Insert: ' + IntToStr(FInsertCount) + '   '), 'Summary Statistics',
             MB_OK + MB_DEFBUTTON1 + MB_ICONINFORMATION);
      end;

    except
      if SQLMetaDataDestination.InTransaction = True then
         SQLMetaDataDestination.Rollback(TransDesc);
         FQuerySource.Close;
      raise;
    end;
  finally
    FStatisticsPanel.Free;

    if FileStream <> nil then begin
      WriteToFile := #13#10 + '*************** End Log ' + FormatDateTime('dd-mmm-yyyy hh:mm:ss AM/PM', Now) +
         ' ***************' + #13#10;
      FileBuffer := PChar(WriteToFile);
      FileStream.Seek(0, soFromEnd);
      FileStream.Write(FileBuffer^, Length(WriteToFile));
      FileStream.Free;
    end;
  end;
end;

end.
