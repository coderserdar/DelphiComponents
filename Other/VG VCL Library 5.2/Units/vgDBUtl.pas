{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         vgDBUtl unit                                  }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L- }

unit vgDBUtl;

interface

uses Graphics, Classes, Controls, Forms, DB, vgSystem, vgDB {$IFNDEF _D3_}, DBTables{$ENDIF},
  DBGrids;

type
  TDataSetClass  = class of TDataSet;

  TDataSetProc   = procedure (DataSet: TDataSet);

  TEditProc      = procedure (DataSet: TDataSet;
    FormClass: TFormClass; const InsertCaption, EditCaption: string; IsNewRecord: Boolean);

  TEditEvent     = procedure (DataSet: TDataSet;
    FormClass: TFormClass; const InsertCaption, EditCaption: string; IsNewRecord: Boolean) of object;

  TDeleteProc    = procedure (DataSet: TDataSet; Confirm: Boolean);

  TDeleteEvent   = procedure (DataSet: TDataSet; Confirm: Boolean) of object;

  TCUEditProc    = procedure (DataSet, CUDataSet: TDataSet;
    FormClass: TFormClass; const InsertCaption, EditCaption: string; IsNewRecord: Boolean);

  TCUEditEvent   = procedure (DataSet, CUDataSet: TDataSet;
    FormClass: TFormClass; const InsertCaption, EditCaption: string; IsNewRecord: Boolean) of object;

  TCUDeleteProc  = procedure (DataSet, CUDataSet: TDataSet; Confirm: Boolean);

  TCUDeleteEvent = procedure (DataSet, CUDataSet: TDataSet; Confirm: Boolean) of object;

{ --- Edit dialogs }
function DataSetEdit(DataSet: TDataSet; EditClass: TFormClass;
  const InsertCaption, EditCaption: TCaption; InsertRecord: Boolean): Boolean;
{ Modally edits DataSet in form created from TFormClass       }
{ Setups form's 'dsEdit' datasource, if found and Caption  to }
{ InsertCaption or EditCaption accordinally to InsertRecord   }

{ --- Posting and deleting }
type
  TDataSetPostProc   = function (DataSet: TDataSet; Apply: Boolean): Boolean;
  TDataSetDeleteProc = function (DataSet: TDataSet; Confirm: Boolean): Boolean;

procedure RegisterPostProc(DataSetClass: TDataSetClass; Proc: TDataSetPostProc);
procedure RegisterDeleteProc(DataSetClass: TDataSetClass; Proc: TDataSetDeleteProc);
{ Registers functions for editing and deletion }

function DataSetPost(DataSet: TDataSet; Apply: Boolean): Boolean;
{ Posts changes in DataSet }

function DataSetDelete(DataSet: TDataSet; Confirm: Boolean): Boolean;
{ Deletes current record in DataSet}

type
  TFieldStringType    = (fsFieldName, fsDisplayLabel);
  TNumericFieldClass  = class of TNumericField;
  TDateTimeFieldClass = class of TDateTimeField;
  TFieldFormat        = (ffDisplayFormat, ffEditFormat);
  TFieldFormats       = Set of TFieldFormat;

{ --- Fields }
function FieldByNumber(DataSet: TDataSet; FieldNo: Integer): TField;
{ Finds a field based on a specified field number }

function FindFieldID(DataSet: TDataSet): TField;
{ Macro for DataSet.FindField(DefaultKeyFields)        }

function FieldID(DataSet: TDataSet): TField;
{ Macro for DataSet.FieldByName(DefaultKeyFields)      }

function ValueID(DataSet: TDataSet): Variant;
{ Macro for FieldID(DataSet).Value                     }

function NewValueID(DataSet: TDataSet): Variant;
{ Macro for FieldID(DataSet).NewValue                  }

function OldValueID(DataSet: TDataSet): Variant;
{ Macro for FieldID(DataSet).OldValue                  }

procedure ClearFields(DataSet: TDataSet);
{ Destroys all fields in DataSet                       }

procedure ClearRequired(DataSet: TDataSet);
{ Clears Required property for all fields in DataSet   }

procedure ClearReadOnly(DataSet: TDataSet);
{ Clears ReadOnly property for all fields in DataSet   }

procedure CreateFields(DataSet: TDataSet);
{ Creates fields for dataset from FieldDefs }

procedure CopyFields(Src, Dst: TDataSet);
{ Creates Dst fields the same to Src                   }
{ Destroys all existing Dst fields                     }

procedure CopyFieldDesc(Src, Dst: TDataSet);
{ Obsolete }
{ Macro for CopyFields procedure }

procedure AddFields(Dst, Src: TDataSet; const FieldNames: string);
{ Adds all fields from FieldName to DataSet Dst if     }
{ if they are exists in Src                            }

procedure AddFieldDesc(Dst, Src: TDataSet; const FieldNames: string);
{ Obsolete }
{ Macro for AddFields procedure                        }

{ --- Batch operations }
procedure DeleteRecords(DataSet: TDataSet);

{ --- Auto Sizer technology }
procedure InitFieldAutoSizer(DataSetClass: TDataSetClass);
procedure InitFieldAutoSizers(DataSetClasses: array of TDataSetClass);
{ Hooks VMT of DataSetClass(es) with auto-sizer mechanism  }

procedure DoneFieldAutoSizer(DataSetClass: TDataSetClass);
procedure DoneFieldAutoSizers(DataSetClasses: array of TDataSetClass);
{ Unhooks VMT auto-sizers for hooked classes                     }

{ Note that when package is about to unload it should Done       }
{ all initialized autosizers for classes from different packages }

{ --- SQL in run-time }
procedure AddFieldName(var Fields: string; const FieldName: string);
function GetFieldList(DataSet: TDataSet; List: TList; FieldNames: string; DataOnly: Boolean): string;
function GetFieldNames(DataSet: TDataSet): string;
function GetFieldNamesFromList(Fields: TList): string;
function GetFieldNamesFmt(DataSet: TDataSet; const NameFmt: string): string;
function GetFieldNamesListString(FieldNames: TStrings): string;
{ Reurns delimeted field names of DataSet }

procedure GetFieldNamesList(const FieldNames: string; List: TStrings);
{ Fills List with field names from FieldNames string }

procedure GetFieldStrings(Fields: TList; FieldStrings: TStrings; StringType: TFieldStringType);
{ Fills FieldStrings list with TFieldStringType information for fields from Fields list }

function IsSQLField(Field: TField): Boolean;
{ Returns true if Field can be used in SQL statements }

function IsBlobField(Field: TField): Boolean;
{ Returns true if Field is blob }

function IsNumeric(DataType: TFieldType): Boolean;
{ Returns true if DataType is numeric data type }

function IsTemporal(DataType: TFieldType): Boolean;
{ Returns true if DataType is numeric data type }

function QuoteSQLNameWith(const SQLName, QuoteChar: string): string;
{ Reutrns SQLName quotes accordinaly QuoteChar variable }

function QuoteSQLName(const SQLName: string): string;
{ Reutrns SQLName quotes accordinaly SQLQuote variable }

procedure AddSQLFieldName(var Fields: string; const FieldName: string);
{ Adds FieldName to Fields string with comma if needed }

function GetFieldNamesComma(FieldNames: TStrings): string;
function GetFieldNamesStrComma(const FieldNames: string): string;
{ Returns list of fields delimeted with comma }

procedure GetSQLFieldNames(DataSet: TDataSet; FieldNames: TStrings);
{ Extracts SQL fields from DataSet and fills FieldNames list }

function GetSQLFieldNamesStr(DataSet: TDataSet): string;
{ Extracts SQL fields from DataSet }

function GetSQLFieldNamesComma(DataSet: TDataSet; const NameFmt: string): string;
{ Extracts SQL fields from DataSet and formats each of them }
{ as shown in NameFmt format string }

function GetFieldListValues(Fields: TList): Variant;

type
  TFieldValuesKind = (fvValue, fvOldValue, fvNewValue, fvCurValue);

function GetFieldValue(Field: TField; FieldValue: TFieldValuesKind): Variant;
function GetFieldValuesEx(DataSet: TDataSet; const FieldName: string; Blobs: Boolean;
  FieldValue: TFieldValuesKind): Variant;
function GetFieldValues(DataSet: TDataSet; const FieldName: string; Blobs: Boolean): Variant;
function GetNewFieldValues(DataSet: TDataSet; const FieldName: string; Blobs: Boolean): Variant;
function GetCurFieldValues(DataSet: TDataSet; const FieldName: string; Blobs: Boolean): Variant;
function GetOldFieldValues(DataSet: TDataSet; const FieldName: string; Blobs: Boolean): Variant;

procedure SetFieldValues(DataSet: TDataSet; const FieldName: string; const Value: Variant);

function ForEachField(const FieldNames, Separator, StringMacro, Macro: string): string;
{ See ForEachString procedure }

{ --- SQL building }
function GetSelect(DataSet: TDataSet; const TableName, Condition: string): string;
{ Builds simple SELECT statement from TableName tablename and Condition condition }
{ Statement will select SQL fields from DataSet }

function GetUpdateSQL(const TableName: string; Fields, KeyFields: TStrings; UpdateKind: TUpdateKind): string;
function GetDataSetUpdateSQLEx(DataSet: TDataSet; const TableName,
  ExcludeFields: string; KeyFieldNames: string; UpdateKind: TUpdateKind): string;
function GetDataSetUpdateSQL(DataSet: TDataSet; const TableName,
  KeyFieldNames: string; UpdateKind: TUpdateKind): string;
{ Builds update SQL statement of specified UpdateKind kind }

{ --- Misc }
function FindEqualField(Fields: TList; const SearchStr: string;
  StringType: TFieldStringType): Integer;
procedure CheckFieldNames(DataSet: TDataSet; const FieldNames: string);
{ Makes sure that all fields from FieldNames are exists in DataSet }

procedure SetFieldFormat(DataSet: TDataSet; const DisplayFormat, EditFormat: string;
  FieldClass: TFieldClass; FieldFormats: TFieldFormats);
{ Setups formats all fields of FieldClass class in DataSet }

procedure SetDataSetsFieldFormat(AOwner: TComponent; Children: Boolean;
  const DisplayFormat, EditFormat: string; FieldClass: TFieldClass; FieldFormats: TFieldFormats);
{ Setups formats all fields of FieldClass class in children of AOwner }

{ --- NVL field functions }
function FieldNvlInteger(Field: TField): Integer;
function FieldNvlFloat(Field: TField): Double;
function FieldNvlDateTime(Field: TField): TDateTime;

{ --- Fields in OnUpdateRecord context }
function FieldUpdateValue(Field: TField): Variant;
function IsEqualOldValue(Field: TField): Boolean;
function IsEqualOldValues(DataSet: TDataSet; const FieldNames: string): Boolean;

{ --- Assign }
function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
{ Returns created blob stream }

procedure AssignField(Field: TField; const Value: Variant);
{ Assign Field.Value to Value if Value is not equal to Field.Value }

procedure AssignFieldValues(Dst, Src: TDataSet; FieldNames: string);
{ Assign field values to destination }

procedure AssignFields(Dst, Src: TList);
{ Assign field values to destination }

procedure CopyRecords(Dst, Src: TDataSet; const DstFieldNames, SrcFieldNames: string;
  Records: Integer; var RecsOut: Integer);
{ Copyies records from Src dataset to Dst }

{ --- Clipboard }
type
  TFieldLabels = (flNone, flFieldNames, flDisplayLabels);

procedure DataSetToStrings(DataSet: TDataSet; FieldNames, Delimeter: string;
  FieldLabels: TFieldLabels; Strings: TStrings; RowCount: Integer);
{ Copies DataSet to Strings as delimeted text }

procedure DataSetToClipboard(DataSet: TDataSet; FieldNames, Delimeter: string;
  FieldLabels: TFieldLabels);
{ Copies DataSet to clipboard as tab-delimeted text }

procedure GridToClipboard(Grid: TCustomDBGrid);
{ Copies Grid contents into clipoard }

{ --- Primary key bookmarks }
type
  TLocateProc        = function (DataSet: TDataSet; const KeyFields: string;
    const KeyValues: Variant; const Options: TLocateOptions): Boolean;

  TRefreshProc       = procedure (DataSet: TDataSet; const FieldsPK: string; const ValuesPK: Variant);

  TPKBookMark        = ^TPKBookMarkData;
  TPKBookMarkData    = record
    PKFields: string;
    PKValues: Variant;
  end;

  TLocateOrigin      = (loCurrent, loFirst, loNext, loPrior, loLast);
  TLocateDirection   = (ldForward, ldBackward);

{ --- Locating }
function GetPKBookMark(DataSet: TDataSet; FieldsPK: string): TPKBookMark;
{ Creates bookmark of type TPKBookmark on current DataSet record }

function SetToPKBookMark(DataSet: TDataSet; Bmk: TPKBookMark): Boolean;
{ Locate record in DataSet for bookmark created by GetPKBookMark function }

procedure FreePKBookMark(var Bmk: TPKBookMark);
{ Frees bookmark created by GetPKBookMark function }

function LocateEx(DataSet: TDataSet; FieldsPK: string; const ValuesPK: Variant): Boolean;
{ Locates for ValuesPK by calling LocateDataSet function }

function IsEqualFieldsList(Fields: TList; const Values: Variant): Boolean;
{ Compares values in Fields list to Values Variant array }

function IsEqualFields(DataSet: TDataSet; const FieldNames: string; const Values: Variant): Boolean;
{ Compares values in FieldNames string to Values Variant array }

function LocateFullSearch(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; const LocateOrigin: TLocateOrigin;
  const LocateDirection: TLocateDirection): Boolean;
{ Locates in DataSet for KeyValues by iterations }

function LocateFullSearchEx(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant): Boolean;
{ Macro for LocateFullSearch function }

{ --- Refreshing }
procedure RegisterRefreshProc(DataSetClass: TDataSetClass; Proc: TRefreshProc);
{ Register Refresh procedure for DataSetClass }
{ Inheritance is supported                    }

procedure ReopenDataSetEx(DataSet: TDataSet; const FieldsPK: string; const ValuesPK: Variant);
procedure ReopenDataSet(DataSet: TDataSet; FieldsPK: string);
procedure RefreshDataSet(DataSet: TDataSet);
{ Reopens DataSet and positions on old record }

procedure Resync(DataSet: TDataSet);
procedure UpdateRecord(DataSet: TDataSet);
{ Makes sure that all editing controls wrote changes into editing DataSet }

{ --- Filters }
procedure SetFilter(DataSet: TDataSet; const Filter: string; Filtered: Boolean);
{ Sets filter only when Filter or Filtered is changed from old }

function DatesFilter(const DateField: string; const StartDate, EndDate: TDateTime): string;
{ Creates a "between two dates" filter }

{ --- DBGrids }
type
  TColumnSettings = record
    FontColor: TColor;
    BkColor: TColor;
    FontStyle: TFontStyles;
  end;

  TSetupColumnsProc  = procedure (Columns: TDBGridColumns) of Object;
  TColumnCallBack    = procedure (Column: TColumn; Info: Pointer);

function FindColumn(Columns: TDBGridColumns; FieldName: string): TColumn;
procedure SetColumnSettings(Column: TColumn; Settings: TColumnSettings);
procedure UpdateColumns(Fields: TList; Columns: TDBGridColumns);
procedure UpdateColumnsForDataSet(DataSet: TDataSet; Columns: TDBGridColumns);
procedure ForEachColumn(Columns: TDBGridColumns; CallBack: TColumnCallBack; Info: Pointer);
procedure ColumnsAlignTitles(Columns: TDBGridColumns; Alignment: TAlignment);

function ConfirmDelete: Boolean;
{ Asks user for confirm Delete operation }

procedure CheckNotNullValue(Field: TField);
{ Checks that Field has a value and raises EWarningMessage if not }

procedure CheckNullValue(Field: TField);
{ Checks that Field has a null value and raises EWarningMessage if not }

procedure CheckLength(Field: TField; NeededLength: Integer);
{ Checks that Field has a string value at least NeededLength characters }

procedure CheckIntegerRange(Field: TIntegerField);
{ Checks that Field has an integer value between Field.MinValue and Field.MaxValue }

procedure CheckNotZeroValue(Field: TField);
{ Checks that Field.Value <> 0 }

procedure CheckCheckedAndNotNull(CheckField, DataField: TField);
{ Checks that CheckField.AsBoolean and if so, DateField has a value }

procedure CheckNotEqualMsg(Field1, Field2: TField; Msg: string);
{ Checks that Field1 and Field2 has not equal values }

procedure CheckNotEqual(Field1, Field2: TField);
{ Macro for CheckNotEqualMsg }

procedure CheckMoreThan(Field: TField; Value: Integer);
{ Checks that Field is more than Value }

procedure CheckMoreOrEqualThan(Field: TField; Value: Integer);
{ Checks that Field is more or equal than Value }

procedure CheckLessThan(Field: TField; Value: Integer);
{ Checks that Field is lower than Value }

procedure CheckEqualTo(Field: TField; Value: Integer);
{ Checks that Field is equal to Value }

procedure CheckRange(Field: TField; Value1, Value2: Double);
{ Checks that Field.Value is between Value1 and Value2 }

procedure CheckTwoNumeric(Field1, Field2: TField; CanEqual: Boolean);
{ Checks that both fields is not null and compares them accordinally to CanEqual }

procedure CheckTwoDates(Field1, Field2: TField; CanEqual: Boolean);
{ Analogically }

procedure CheckRowsAffected(RowsActual, RowsNeeded: Integer);
{ Raises exception if RowsActual <> RowsNeeded }

procedure RecordNotFound;
{ Raises EWarningMessage exception with "RecordNotFound" message}

procedure RecordNotFoundTbl(Msg: string);
{ Raises EWarningMessage exception with "RecordNotFound in table" message}

function IsEmpty(DataSet: TDataSet): Boolean;
{ Returns True if DataSet has no recrds }

procedure CheckNotEmpty(DataSet: TDataSet);
{ Checks that DataSet is not Empty }

procedure CheckNotEmptyTbl(DataSet: TDataSet; Msg: string);
{ Analogically }

procedure InsertRestrict;
{ Raises EWarningMessage exception with "Cannot insert" message }

procedure EditRestrict;
{ Raises EWarningMessage exception with "Cannot edit" message }

procedure DeleteRestrict;
{ Raises EWarningMessage exception with "Cannot delete" message }

function DefaultPostDataSet(DataSet: TDataSet; Apply: Boolean): Boolean;
function DefaultDeleteDataSet(DataSet: TDataSet; Confirm: Boolean): Boolean;
function DefaultLocateDataSet(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; const Options: TLocateOptions): Boolean;
procedure DefaultOpenDataSet(DataSet: TDataSet);
procedure DefaultFetchDataSet(DataSet: TDataSet);
procedure DefaultReopenDataSetEx(DataSet: TDataSet; FieldsPK: string; const ValuesPK: Variant);

procedure OpenDataSets(const DataSets: array of TDataSet);
{ Opens each of datasets }

procedure CloseDataSets(const DataSets: array of TDataSet);
{ Close each dataset }

procedure RegisterFieldClasses;
{ Streaming support }

function NameDelimiter(C: Char; const Delims: TCharSet): Boolean; 
function IsLiteral(C: Char): Boolean;
function StripLiterals(const Buffer: PChar): string;

{ FieldDefs }
function FieldDefsToVariant(FieldDefs: TFieldDefs): Variant;
procedure VariantToFieldDefs(const Value: Variant; FieldDefs: TFieldDefs);

{ IndexDefs }
function IndexDefsToVariant(IndexDefs: TIndexDefs): Variant;
procedure VariantToIndexDefs(const Value: Variant; IndexDefs: TIndexDefs);

const
  DefaultKeyFields: string   = 'ID';
  SQLQuote: string           = '';
  SQLBased: Boolean          = True;
  MacroChar: Char            = '%';
  FieldSeparator             = ', ';
  TrueSQL                    = '0=0';
  FalseSQL                   = '0=1';

  FieldNameChars: TCharSet = ['0'..'9', 'A'..'Z', 'a'..'z', '$', '_'];

  OpenDataSet: TDataSetProc  = DefaultOpenDataSet;
  FetchDataSet: TDataSetProc = DefaultFetchDataSet;
  LocateDataSet: TLocateProc = DefaultLocateDataSet;

implementation
uses SysUtils, vgUtils, vgVCLUtl, vgDBRes, vgClpbrd, Dialogs,
  {$IFDEF _D3_}BDEConst{$ELSE}DBConsts{$ENDIF};

type
  TDataSetHack = class(TDataSet)
{$IFDEF _D3_}
  protected
    procedure InternalInitFieldDefs; override;
{$ENDIF}
  public
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
  end;

procedure TDataSetHack.DataEvent(Event: TDataEvent; Info: Longint);
begin
  inherited;
end;

{$IFDEF _D3_}
procedure TDataSetHack.InternalInitFieldDefs;
begin
end;
{$ENDIF}

function FindClassListData(ClassList: TClassList; AClass: TClass): Pointer;
var
  I: Integer;
begin
  if Assigned(ClassList) then
  begin
    I := ClassList.IndexOfClass(AClass, True);
    if I >= 0 then
    begin
      Result := ClassList.Items[I].Data;
      Exit;
    end;
  end;
  Result := nil;
end;

{ Edit dialog }
function DataSetEdit(DataSet: TDataSet; EditClass: TFormClass;
  const InsertCaption, EditCaption: TCaption; InsertRecord: Boolean): Boolean;
var
  Form: TForm;
  EditSource: TDataSource;
begin
  AppSetCursor(crHourglass);
  try
    // Creating form
    Form := EditClass.Create(nil);
    // Setup properties
    with Form do
    try
      if InsertRecord then
      begin
        DataSet.Insert;
        Caption := InsertCaption;
      end else begin
        // Cached updates pre-fetch.
        // Rollback transaction may cause cursor destruction
        FetchDataSet(DataSet);
//        DataSet.Edit;
        Caption := EditCaption;
      end;
      // Reserved DataSource
      EditSource := TDataSource(FindComponent('dsEdit'));
      if Assigned(EditSource) then EditSource.DataSet := DataSet;
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
  finally
    if DataSet.State in dsEditModes then DataSet.Cancel;
    AppRestoreCursor;
  end;
end;

{ --- Posting and deleting }
var
  PostProcClassList: TClassList = nil;
  DeleteProcClassList: TClassList = nil;

procedure RegisterPostProc(DataSetClass: TDataSetClass; Proc: TDataSetPostProc);
begin
  if not Assigned(PostProcClassList) then PostProcClassList := TClassList.Create;
  PostProcClassList.RegisterClass(DataSetClass, @Proc, '', True);
end;

procedure RegisterDeleteProc(DataSetClass: TDataSetClass; Proc: TDataSetDeleteProc);
begin
  if not Assigned(DeleteProcClassList) then DeleteProcClassList := TClassList.Create;
  DeleteProcClassList.RegisterClass(DataSetClass, @Proc, '', True);
end;

function DefaultPostDataSet(DataSet: TDataSet; Apply: Boolean): Boolean;
begin
  if Apply then DataSet.Post else DataSet.Cancel;
  Result := True;
end;

function DataSetPost(DataSet: TDataSet; Apply: Boolean): Boolean;
var
  Proc: TDataSetPostProc;
begin
  Proc := FindClassListData(PostProcClassList, DataSet.ClassType);
  if not Assigned(Proc) then Proc := @DefaultPostDataSet;
  Result := Proc(DataSet, Apply);
end;

function DefaultDeleteDataSet(DataSet: TDataSet; Confirm: Boolean): Boolean;
begin
  Result := False;
  if not Confirm or ConfirmDelete then
  begin
    DataSet.Delete;
    Result := True;
  end;
end;

function DataSetDelete(DataSet: TDataSet; Confirm: Boolean): Boolean;
var
  Proc: TDataSetDeleteProc;
begin
  Proc := FindClassListData(DeleteProcClassList, DataSet.ClassType);
  if not Assigned(Proc) then Proc := @DefaultDeleteDataSet;
  Result := Proc(DataSet, Confirm);
end;

function FieldByNumber(DataSet: TDataSet; FieldNo: Integer): TField;
{$IFDEF _D3_}
begin
  Result := TDataSetHack(DataSet).FieldByNumber(FieldNo);
end;
{$ELSE}
var
  I: Integer;
begin
  for I := 0 to DataSet.FieldCount - 1 do
  begin
    Result := DataSet.Fields[I];
    if Result.FieldNo = FieldNo then Exit;
  end;
  Result := nil;
end;
{$ENDIF}

function FindFieldID(DataSet: TDataSet): TField;
begin
  Result := DataSet.FindField(DefaultKeyFields);
end;

function FieldID(DataSet: TDataSet): TField;
begin
  Result := DataSet.FieldByName(DefaultKeyFields);
end;

function ValueID(DataSet: TDataSet): Variant;
begin
  Result := FieldID(DataSet).Value;
end;

function NewValueID(DataSet: TDataSet): Variant;
begin
  Result := FieldID(DataSet).NewValue;
end;

function OldValueID(DataSet: TDataSet): Variant;
begin
  Result := FieldID(DataSet).OldValue;
end;

procedure ClearFields(DataSet: TDataSet);
begin
  while DataSet.FieldCount > 0 do DataSet.Fields[0].Free;
end;

procedure ClearRequired(DataSet: TDataSet);
var
  I: Integer;
begin
  for I := 0 to DataSet.FieldCount - 1 do
    DataSet.Fields[I].Required := False;
end;

procedure ClearReadOnly(DataSet: TDataSet);
var
  I: Integer;
begin
  for I := 0 to DataSet.FieldCount - 1 do
    DataSet.Fields[I].ReadOnly := False;
end;

procedure CreateFields(DataSet: TDataSet);
{$IFNDEF _D3_}
var
  I: Integer;
{$ENDIF}
begin
  DataSet.FieldDefs.Update;
{$IFNDEF _D3_}
  for I := 0 to DataSet.FieldDefs.Count - 1 do
    with DataSet.FieldDefs[I] do if DataType <> ftUnknown then CreateField(DataSet);
{$ELSE}
  TDataSetHack(DataSet).CreateFields;
{$ENDIF}
end;

procedure CopyFields(Src, Dst: TDataSet);
var
  I: Integer;
  Field: TField;
begin
  ClearFields(Dst);

  for I := 0 to Src.FieldCount - 1 do
  begin
    Field := TField(CreateClone(Src.Fields[I]));
    Field.DataSet := Dst;
  end;
end;

procedure CopyFieldDesc(Src, Dst: TDataSet);
begin
  CopyFields(Src, Dst);
end;

procedure AddFields(Dst, Src: TDataSet; const FieldNames: string);
var
  I: Integer;
  Field: TField;
  Fields: TList;
begin
  Fields := TList.Create;
  try
    Src.GetFieldList(Fields, FieldNames);
    for I := 0 to Fields.Count - 1 do
    begin
      Field := TField(CreateClone(Fields[I]));
      Field.DataSet := Dst;
    end;
  finally
    Fields.Free;
  end;
end;

procedure AddFieldDesc(Dst, Src: TDataSet; const FieldNames: string);
begin
  AddFields(Dst, Src, FieldNames);
end;

procedure DeleteRecords(DataSet: TDataSet);
begin
  with DataSet do
  begin
    First;
    while not EOF do Delete;
  end;
end;

{$IFDEF _D3_}
type
  PInternalInitFieldDefsData = ^TInternalInitFieldDefsData;
  TInternalInitFieldDefsData = record
    DataSetClass: TDataSetClass;
    Proc: Pointer;
  end;

var
  InternalInitFieldDefss: TList = nil;

function FindInternalInitFieldDefsData(DataSetClass: TDataSetClass): PInternalInitFieldDefsData;
var
  I: Integer;
begin
  if Assigned(InternalInitFieldDefss) then
  begin
    for I := 0 to InternalInitFieldDefss.Count - 1 do
    begin
      Result := InternalInitFieldDefss[I];
      if Result^.DataSetClass = DataSetClass then Exit;
    end;
  end;
  Result := nil;
end;

procedure FreeInternalInitFieldDefss;
var
  I: Integer;
  P: PInternalInitFieldDefsData;
begin
  if Assigned(InternalInitFieldDefss) then
  begin
    for I := InternalInitFieldDefss.Count - 1 downto 0  do
    begin
      P := InternalInitFieldDefss[I];
      DoneFieldAutoSizer(P^.DataSetClass);
    end;
  end;
end;

type
  TFieldAutoSizer = class(TObject)
  protected
    procedure InternalInitFieldDefs; virtual;
  end;

procedure TFieldAutoSizer.InternalInitFieldDefs;
var
  DataSet: TDataSet;
var
  DataSetClass: TDataSetClass;
  Data: PInternalInitFieldDefsData;
  Field: TField;
  FieldDef: TFieldDef;
  I: Integer;
begin
  asm
    mov    DataSet, eax
  end;

  DataSetClass :=TDataSetClass(Self.ClassType);
  Data := FindInternalInitFieldDefsData(DataSetClass);

  asm
    mov    ecx, Data
    mov    ecx, [ecx + 4]
    mov    eax, DataSet
    call   ecx
  end;

  with DataSet do
  begin
    if not DefaultFields then
    begin
      for I := 0 to FieldCount - 1 do
      begin
        Field := Fields[I];
        if not (Field.FieldKind in [fkCalculated, fkLookup]) then
        begin
          FieldDef := FieldDefs.Find(Field.FieldName);
          if (Field.Size <> FieldDef.Size) then
            Field.Size := FieldDef.Size;
        end;
      end;
    end;
  end;
end;
{$ENDIF}

procedure InitFieldAutoSizer(DataSetClass: TDataSetClass);
{$IFDEF _D3_}
var
  I, J: Integer;
  Addr: Pointer;
  Data: PInternalInitFieldDefsData;
{$ENDIF}
begin
  {$IFDEF _D3_}
  if FindInternalInitFieldDefsData(DataSetClass) <> nil then Exit;
  I := FindVirtualMethodIndex(TDataSetHack, @TDataSetHack.InternalInitFieldDefs);
  if not Assigned(InternalInitFieldDefss) then InternalInitFieldDefss := TList.Create;
  GetMem(Data, SizeOf(PInternalInitFieldDefsData));
  try
    Data^.DataSetClass := DataSetClass;
    Data^.Proc := GetVirtualMethodAddress(DataSetClass, I);
    J := FindVirtualMethodIndex(TFieldAutoSizer, @TFieldAutoSizer.InternalInitFieldDefs);
    Addr := GetVirtualMethodAddress(TFieldAutoSizer, J);
    SetVirtualMethodAddress(DataSetClass, I, Addr);
    InternalInitFieldDefss.Add(Data);
  except
    FreeMem(Data);
    raise;
  end;
  {$ENDIF}
end;

procedure DoneFieldAutoSizer(DataSetClass: TDataSetClass);
{$IFDEF _D3_}
var
  J: Integer;
  P: PInternalInitFieldDefsData;
{$ENDIF}
begin
{$IFDEF _D3_}
  P := FindInternalInitFieldDefsData(DataSetClass);
  if not Assigned(P) then Exit;
  J := FindVirtualMethodIndex(TDataSetHack, @TDataSetHack.InternalInitFieldDefs);
  SetVirtualMethodAddress(P^.DataSetClass, J, P^.Proc);
  InternalInitFieldDefss.Remove(P);
  if InternalInitFieldDefss.Count = 0 then
  begin
    InternalInitFieldDefss.Free;
    InternalInitFieldDefss := nil;
  end;
  FreeMem(P);
{$ENDIF}
end;

procedure InitFieldAutoSizers(DataSetClasses: array of TDataSetClass);
var
  I: Integer;
begin
  for I := Low(DataSetClasses) to High(DataSetClasses) do
    InitFieldAutoSizer(DataSetClasses[I]);
end;

procedure DoneFieldAutoSizers(DataSetClasses: array of TDataSetClass);
var
  I: Integer;
begin
  for I := Low(DataSetClasses) to High(DataSetClasses) do
    DoneFieldAutoSizer(DataSetClasses[I]);
end;

procedure AddFieldName(var Fields: string; const FieldName: string);
begin
  AddDelimeted(Fields, FieldName, ';');
end;

function GetFieldList(DataSet: TDataSet; List: TList; FieldNames: string; DataOnly: Boolean): string;
var
  I: Integer;
  Field: TField;
begin
  if FieldNames = '' then
    for I := 0 to DataSet.FieldCount - 1 do
    begin
      Field := DataSet.Fields[I];
      if not DataOnly or (Field.FieldKind in [fkData {$IFDEF _D3_}, fkInternalCalc{$ENDIF}]) then
      begin
        AddFieldName(FieldNames, Field.FieldName);
        if Assigned(List) then List.Add(Field);
      end;
    end else begin
      DataSet.GetFieldList(List, FieldNames);
    end;
  Result := FieldNames;
end;

function GetFieldNames(DataSet: TDataSet): string;
begin
  Result := GetFieldNamesFmt(DataSet, '%s');
end;

function GetFieldNamesFromList(Fields: TList): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Fields.Count - 1 do
    AddFieldName(Result, TField(Fields[I]).FieldName);
end;

function GetFieldNamesFmt(DataSet: TDataSet; const NameFmt: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to DataSet.FieldCount - 1 do
    AddFieldName(Result, Format(NameFmt, [DataSet.Fields[I].FieldName]));
end;

function GetFieldNamesListString(FieldNames: TStrings): string;
begin
  Result := GetListString('%s;', FieldNames);
end;

procedure GetFieldNamesList(const FieldNames: string; List: TStrings);
var
  Pos: Integer;
begin
  List.Clear;
  Pos := 1;
  while Pos <= Length(FieldNames) do
    List.Add(ExtractFieldName(FieldNames, Pos));
end;

procedure GetFieldStrings(Fields: TList;
  FieldStrings: TStrings; StringType: TFieldStringType);
var
  I: Integer;
  Field: TField;
begin
  FieldStrings.BeginUpdate;
  try
    FieldStrings.Clear;
    for I := 0 to Fields.Count - 1 do
    begin
      Field := TField(Fields[I]);
      case StringType of
        fsFieldName:
          FieldStrings.Add(Field.FieldName);
        fsDisplayLabel:
          FieldStrings.Add(Field.DisplayLabel);
      end;
    end;
  finally
    FieldStrings.EndUpdate;
  end;
end;

function IsSQLField(Field: TField): Boolean;
begin
  Result := Field.FieldKind = fkData;
end;

function IsBlobField(Field: TField): Boolean;
begin
{$IFDEF _D3_}
  Result := Field.IsBlob;
{$ELSE}
  Result := Field is TBlobField;
{$ENDIF};
end;

function IsNumeric(DataType: TFieldType): Boolean;
begin
  Result := DataType in [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency,
    ftBCD, ftAutoInc{$IFDEF _D4_}, ftLargeint{$ENDIF}];
end;

function IsTemporal(DataType: TFieldType): Boolean;
begin
  Result := DataType in [ftDate, ftTime, ftDateTime];
end;

function QuoteSQLNameWith(const SQLName, QuoteChar: string): string;
begin
  Result := QuoteChar + SQLName + QuoteChar;
end;

function QuoteSQLName(const SQLName: string): string;
begin
  Result := QuoteSQLNameWith(SQLName, SQLQuote);
end;

procedure AddSQLFieldName(var Fields: string; const FieldName: string);
begin
  AddDelimeted(Fields, FieldName, FieldSeparator);
end;

function GetFieldNamesComma(FieldNames: TStrings): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FieldNames.Count - 1 do
    AddSQLFieldName(Result, FieldNames[I]);
end;

function GetFieldNamesStrComma(const FieldNames: string): string;
var
  I: Integer;
  Fields: TStrings;
begin
  Fields := TStringList.Create;
  try
    GetFieldNamesList(FieldNames, Fields);
    Result := '';
    for I := 0 to Fields.Count - 1 do
      AddSQLFieldName(Result, Fields[I]);
  finally
    Fields.Free;
  end;
end;

procedure GetSQLFieldNames(DataSet: TDataSet; FieldNames: TStrings);
var
  I: Integer;
  Field: TField;
begin
  FieldNames.BeginUpdate;
  try
    FieldNames.Clear;
    for I := 0 to DataSet.FieldCount - 1 do
    begin
      Field := DataSet.Fields[I];
      if IsSQLField(Field) then FieldNames.Add(Field.FieldName);
    end;
  finally
    FieldNames.EndUpdate;
  end;
end;

function GetSQLFieldNamesStr(DataSet: TDataSet): string;
var
  I: Integer;
  Field: TField;
begin
  Result := '';       
  for I := 0 to DataSet.FieldCount - 1 do
  begin
    Field := DataSet.Fields[I];
    if IsSQLField(Field) then AddFieldName(Result, Field.FieldName);
  end;
end;

function GetSQLFieldNamesComma(DataSet: TDataSet; const NameFmt: string): string;
var
  I: Integer;
  Field: TField;
begin
  with DataSet do
  begin
    Result := '';
    for I := 0 to FieldCount - 1 do
    begin
      Field := Fields[I];
      if IsSQLField(Field) then
        AddSQLFieldName(Result, Format(NameFmt, [Field.FieldName]));
    end;
  end;
end;

function GetFieldListValues(Fields: TList): Variant;
var
  I: Integer;
begin
  if Fields.Count > 1 then
  begin
    Result := VarArrayCreate([0, Fields.Count - 1], varVariant);
    for I := 0 to Fields.Count - 1 do
      Result[I] := TField(Fields[I]).Value;
  end else if Fields.Count = 1 then
    Result := TField(Fields[0]).Value
  else
    Result := Unassigned;
end;

function GetFieldValue(Field: TField; FieldValue: TFieldValuesKind): Variant;
begin
  case FieldValue of
    fvValue:
      Result := Field.Value;
    fvOldValue:
      Result := Field.OldValue;
    fvNewValue:
      Result := Field.NewValue;
    fvCurValue:
{$IFDEF _D3_}
      Result := Field.CurValue;
{$ELSE}
      Result := Field.Value;
{$ENDIF}
  end;
end;

function GetFieldValuesEx(DataSet: TDataSet; const FieldName: string; Blobs: Boolean;
  FieldValue: TFieldValuesKind): Variant;
var
  I: Integer;
  Field: TField;
  Fields: TList;
begin
  if Pos(';', FieldName) <> 0 then
  begin
    Fields := TList.Create;
    try
      DataSet.GetFieldList(Fields, FieldName);
      Result := VarArrayCreate([0, Fields.Count - 1], varVariant);
      for I := 0 to Fields.Count - 1 do
      begin
        Field := TField(Fields[I]);
        if Blobs or not IsBlobField(Field) then
          Result[I] := GetFieldValue(Field, FieldValue);
      end;
    finally
      Fields.Free;
    end;
  end else begin
    Field := DataSet.FieldByName(FieldName);
    if Blobs or not IsBlobField(Field) then
      Result := GetFieldValue(Field, FieldValue);
   end;
end;

function GetFieldValues(DataSet: TDataSet; const FieldName: string; Blobs: Boolean): Variant;
begin
  Result := GetFieldValuesEx(DataSet, FieldName, Blobs, fvValue);
end;

function GetNewFieldValues(DataSet: TDataSet; const FieldName: string; Blobs: Boolean): Variant;
begin
  Result := GetFieldValuesEx(DataSet, FieldName, Blobs, fvNewValue);
end;

function GetCurFieldValues(DataSet: TDataSet; const FieldName: string; Blobs: Boolean): Variant;
begin
  Result := GetFieldValuesEx(DataSet, FieldName, Blobs, fvCurValue);
end;

function GetOldFieldValues(DataSet: TDataSet; const FieldName: string; Blobs: Boolean): Variant;
begin
  Result := GetFieldValuesEx(DataSet, FieldName, Blobs, fvOldValue);
end;

procedure SetFieldValues(DataSet: TDataSet; const FieldName: string; const Value: Variant);
var
  I: Integer;
  Fields: TList;
begin
  if Pos(';', FieldName) <> 0 then
  begin
    Fields := TList.Create;
    try
      DataSet.GetFieldList(Fields, FieldName);
      for I := 0 to Fields.Count - 1 do
        if not VarIsEmpty(Value[I]) then
          TField(Fields[I]).Value := Value[I];
    finally
      Fields.Free;
    end;
  end else
    if not VarIsEmpty(Value) then
      DataSet.FieldByName(FieldName).Value := Value;
end;

function ForEachField(const FieldNames, Separator, StringMacro, Macro: string): string;
var
  Fields: TStrings;
begin
  Fields := TStringList.Create;
  try
    GetFieldNamesList(FieldNames, Fields);
    Result := ForEachString(Fields, Separator, StringMacro, Macro);
  finally
    Fields.Free;
  end;
end;

function GetSelect(DataSet: TDataSet; const TableName, Condition: string): string;
const
  SelectSQL = 'select %s '#13#10'from %s';
var
  Fields: string;
begin
  if Assigned(DataSet) then
    Fields := GetSQLFieldNamesComma(DataSet, #13#10 + QuoteSQLName(TableName) + '.' + QuoteSQLName('%s')) else
    Fields := '*';

  FmtStr(Result, SelectSQL, [Fields, QuoteSQLName(TableName)]);
  if Condition <> '' then FmtStr(Result, '%s '#13#10'where %s', [Result, Condition]);
end;

function GetUpdateSQL(const TableName: string; Fields, KeyFields: TStrings; UpdateKind: TUpdateKind): string;
const
  UpdSQL: array [TUpdateKind] of string =
    ('update %s set%s'#13#10'where'#13#10'%s'#13#10,
     'insert into %s(%s)'#13#10'values(%s)',
     'delete from %s where'#13#10'%s');


var
  S1, S2: string;
  TableAlias: string;
begin
  if not SQLBased then
    TableAlias := QuoteSQLName(TableName) + '.' else
    TableAlias := '';

  case UpdateKind of
    ukModify:
      begin
        S1 := ForEachString(Fields, FieldSeparator, MacroChar, #13#10 + TableAlias + QuoteSQLName(MacroChar) + ' = :' + QuoteSQLName(MacroChar));
        S2 := ForEachString(KeyFields, #13#10'and ', MacroChar, TableAlias + QuoteSQLName(MacroChar) + ' = :' + QuoteSQLNameWith('OLD_' + MacroChar, '"'));
      end;
    ukInsert:
      begin
        S1 := ForEachString(Fields, FieldSeparator, MacroChar, #13#10 + TableAlias + QuoteSQLName(MacroChar));
        S2 := ForEachString(Fields, FieldSeparator, MacroChar, #13#10':' + QuoteSQLNameWith(MacroChar, '"'));
      end;
    ukDelete:
      begin
        S1 := ForEachString(KeyFields, #13#10' and ', MacroChar, TableAlias + QuoteSQLName(MacroChar) + ' = :' + QuoteSQLNameWith('OLD_' + MacroChar, '"'));
        S2 := '';
      end;
  end;
  Result := Format(UpdSQL[UpdateKind], [QuoteSQLName(TableName), S1, S2]);
end;

function GetDataSetUpdateSQLEx(DataSet: TDataSet; const TableName,
  ExcludeFields: string; KeyFieldNames: string; UpdateKind: TUpdateKind): string;
var
  Fields, KeyFields: TStrings;
  S: string;
  I, Pos: Integer;
begin
  Fields := TStringList.Create;
  KeyFields := TStringList.Create;
  try
    GetSQLFieldNames(DataSet, Fields);

    Pos := 1;
    while Pos <= Length(ExcludeFields) do
    begin
      S := ExtractFieldName(ExcludeFields, Pos);
      I := Fields.IndexOf(S);
      if I >= 0 then Fields.Delete(I);
    end;

    if KeyFieldNames = '' then KeyFieldNames := DefaultKeyFields;

    GetFieldNamesList(KeyFieldNames, KeyFields);

    Result := GetUpdateSQL(TableName, Fields, KeyFields, UpdateKind);
  finally
    Fields.Free;
    KeyFields.Free;
  end;
end;

function GetDataSetUpdateSQL(DataSet: TDataSet; const TableName,
  KeyFieldNames: string; UpdateKind: TUpdateKind): string;
begin
  Result := GetDataSetUpdateSQLEx(DataSet, TableName, '', KeyFieldNames, UpdateKind);
end;

function FindEqualField(Fields: TList; const SearchStr: string;
  StringType: TFieldStringType): Integer;
var
  Field: TField;
  CmpStr: string;
begin
  for Result := 0 to Fields.Count - 1 do
  begin
    Field := TField(Fields[Result]);
    case StringType of
      fsFieldName:
        CmpStr := Field.FieldName;
      fsDisplayLabel:
        CmpStr := Field.DisplayLabel;
    end;
    if AnsiCompareText(SearchStr, CmpStr) = 0 then Exit;
  end;
  Result := -1;
end;

procedure CheckFieldNames(DataSet: TDataSet; const FieldNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= Length(FieldNames) do
    DataSet.FieldByName(ExtractFieldName(FieldNames, Pos));
end;

procedure SetFieldFormat(DataSet: TDataSet; const DisplayFormat, EditFormat: string;
  FieldClass: TFieldClass; FieldFormats: TFieldFormats);
var
  I: Integer;
  Field: TField;
begin
  for I := 0 to DataSet.FieldCount - 1 do
  begin
    Field := DataSet.Fields[I];
    if (Field is FieldClass) then
    begin
      if Field is TNumericField then
      begin
        if (ffDisplayFormat in FieldFormats) then
          TNumericField(Field).DisplayFormat := DisplayFormat;

        if (ffEditFormat in FieldFormats) then
          TNumericField(Field).EditFormat := EditFormat;
      end else if Field is TDateTimeField then
      begin
        if (ffDisplayFormat in FieldFormats) then
          TDateTimeField(Field).DisplayFormat := DisplayFormat;

        if (ffEditFormat in FieldFormats) then
          TDateTimeField(Field).EditMask := EditFormat;
      end;
    end;
  end;
end;

procedure SetDataSetsFieldFormat(AOwner: TComponent; Children: Boolean;
  const DisplayFormat, EditFormat: string;
  FieldClass: TFieldClass; FieldFormats: TFieldFormats);

type
  PData = ^TData;
  TData = record
    DisplayFormat, EditFormat: string;
    FieldClass: TFieldClass;
    FieldFormats: TFieldFormats
  end;

 procedure SetFormat(Instance: TComponent; Data: Pointer);
 begin
   with PData(Data)^ do
     SetFieldFormat(TDataSet(Instance), DisplayFormat, EditFormat, FieldClass, FieldFormats);
 end;

var
  Data: TData;
begin
  Data.DisplayFormat := DisplayFormat;
  Data.EditFormat := EditFormat;
  Data.FieldClass := FieldClass;
  Data.FieldFormats := FieldFormats;

  ForEachComponent(AOwner, TDataSet, @SetFormat, @Data, Children);
end;

function FieldNvlInteger(Field: TField): Integer;
begin
  if not Field.IsNull then Result := Field.AsInteger else Result := 0;
end;

function FieldNvlFloat(Field: TField): Double;
begin
  if not Field.IsNull then Result := Field.AsFloat else Result := 0;
end;

function FieldNvlDateTime(Field: TField): TDateTime;
begin
  if not Field.IsNull then Result := Field.AsDateTime else Result := EncodeDate(1, 1, 1);
end;

function FieldUpdateValue(Field: TField): Variant;
begin
  Result := Field.NewValue;
  if VarIsEmpty(Result) then Result := Field.OldValue;
end;

function IsEqualOldValue(Field: TField): Boolean;
begin
  Result := VarIsEqual(Field.Value, Field.OldValue);
end;

function IsEqualOldValues(DataSet: TDataSet; const FieldNames: string): Boolean;
var
  Pos: Integer;
begin
  Result := True; Pos := 1;
  while Result and (Pos <= Length(FieldNames)) do
    Result := IsEqualOldValue(DataSet.FieldByName(ExtractFieldName(FieldNames, Pos)));
end;

function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
{$IFDEF _D3_}
  Result := Field.DataSet.CreateBlobStream(Field, bmRead);
{$ELSE}
  Result := TBlobStream.Create(Field as TBlobField, bmRead);
{$ENDIF}
end;

procedure AssignField(Field: TField; const Value: Variant);
begin
  if not VarIsEqual(Field.Value, Value) then Field.Value := Value;
end;

procedure AssignFieldValues(Dst, Src: TDataSet; FieldNames: string);
begin
  FieldNames := GetFieldList(Src, nil, FieldNames, True);
  Dst.FieldValues[FieldNames] := Src.FieldValues[FieldNames];
end;

procedure AssignFields(Dst, Src: TList);
var
  I: Integer;
begin
  for I := 0 to Dst.Count - 1 do
    TField(Dst[I]).Value := TField(Src[I]).Value;
end;

procedure CopyRecords(Dst, Src: TDataSet; const DstFieldNames, SrcFieldNames: string;
  Records: Integer; var RecsOut: Integer);
var
  DstFields, SrcFields: TList;
begin
  if Records <= 0 then
    Records := MaxInt else Src.First;
  RecsOut := 0;
  DstFields := TList.Create;
  try
    GetFieldList(Dst, DstFields, DstFieldNames, True);
    if DstFields.Count = 0 then Exit;
    SrcFields := TList.Create;
    try
      GetFieldList(Src, SrcFields, SrcFieldNames, True);
      Src.DisableControls;
      Dst.DisableControls;
      try
        while (RecsOut < Records) and not Src.EOF do
        begin
          Dst.Insert;
          try
            AssignFields(DstFields, SrcFields);
            Dst.Post;
          except
            Dst.Cancel;
            raise;
          end;
          Inc(RecsOut);
          Src.Next;
        end;
      finally
        Src.EnableControls;
        Dst.EnableControls;
      end;
    finally
       SrcFields.Free;
    end;
  finally
    DstFields.Free;
  end;
end;

procedure DataSetToStrings(DataSet: TDataSet; FieldNames, Delimeter: string;
  FieldLabels: TFieldLabels; Strings: TStrings; RowCount: Integer);
var
  I, Count: Integer;
  Txt: string;
  Bmk: TBookmark;
  Fields: TStrings;
begin
  if IsEmpty(DataSet) then Exit;
  if Delimeter = '' then Delimeter := #9;
  Fields := nil;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    Fields := TStringList.Create;
    if FieldNames <> '' then
      GetFieldNamesList(FieldNames, Fields) else
      DataSet.GetFieldNames(Fields);

    if Fields.Count = 0 then Exit;

    DataSet.DisableControls;
    try
      Bmk := DataSet.GetBookmark;
      try
        case FieldLabels of
          flFieldNames:
            begin
              Txt := Fields[0];
              for I := 1 to Fields.Count - 1 do
                Txt := Txt + Delimeter + Fields[I];
              Strings.Add(Txt);
            end;
          flDisplayLabels:
            begin
              Txt := DataSet.FieldByName(Fields[0]).DisplayLabel;
              for I := 1 to Fields.Count - 1 do
                Txt := Txt + Delimeter + DataSet.FieldByName(Fields[I]).DisplayLabel;
              Strings.Add(Txt);
            end;
        end;

        Count := 0;
        while not DataSet.EOF and ((RowCount < 0) or (Count < RowCount)) do
        begin
          Txt := DataSet.FieldByName(Fields[0]).DisplayText;
          for I := 1 to Fields.Count - 1 do
            Txt := Txt + Delimeter + DataSet.FieldByName(Fields[I]).DisplayText;
          Strings.Add(Txt);
          DataSet.Next;
          Inc(Count);
        end;
      finally
        try
          DataSet.GotoBookmark(Bmk);
        except end;
        DataSet.FreeBookmark(Bmk);
      end;
    finally
      DataSet.EnableControls;
    end;
  finally
    Strings.EndUpdate;
    Fields.Free;
  end;
end;

procedure DataSetToClipboard(DataSet: TDataSet; FieldNames, Delimeter: string;
  FieldLabels: TFieldLabels);
var
  Bmk: TBookmark;
  Strings: TStrings;
begin
  if IsEmpty(DataSet) then Exit;
  Strings := TStringList.Create;
  try
    AppSetCursor(crHourglass);
    try
      Bmk := DataSet.GetBookmark;
      try
        DataSet.First;
        DataSetToStrings(DataSet, FieldNames,
          Delimeter, FieldLabels, Strings, -1);
        DataSet.GotoBookmark(Bmk);
      finally
        DataSet.FreeBookmark(Bmk);
      end;
      ClipboardCopy(Strings.Text);
    finally
      AppRestoreCursor;
    end;
  finally
    Strings.Free;
  end;
end;

type
  TDBGridHack = class(TCustomDBGrid);

procedure GridToClipboard(Grid: TCustomDBGrid);
var
  I: Integer;
  Column: TColumn;
  FieldNames: string;
  DataSet: TDataSet;
begin
  if Assigned(TDBGridHack(Grid).DataSource) then
  begin
    DataSet := TDBGridHack(Grid).DataSource.DataSet;
    FieldNames := '';
    for I := 0 to TDBGridHack(Grid).Columns.Count - 1 do
    begin
      Column := TDBGridHack(Grid).Columns[I];
      if DataSet.FindField(Column.FieldName) <> nil then
        FieldNames := FieldNames + Column.FieldName + ';';
    end;
    if FieldNames <> '' then
      DataSetToClipboard(DataSet, FieldNames, #9, flDisplayLabels);
  end;
end;

function GetPKBookMark(DataSet: TDataSet; FieldsPK: string): TPKBookMark;
var
  V: Variant;
begin
  if FieldsPK = '' then FieldsPK := DefaultKeyFields;
  New(Result);
  try
    try
      with Result^ do
      begin
        PKFields := FieldsPK;
        V := DataSet.FieldValues[FieldsPK];
        PKValues := V;
      end;
    except
      Dispose(Result);
      raise;
    end;
  except
    Result := nil;
  end;
end;

function SetToPKBookMark(DataSet: TDataSet; Bmk: TPKBookMark): Boolean;
begin
  Result := Assigned(Bmk) and LocateEx(DataSet, Bmk^.PKFields, Bmk^.PKValues);
end;

procedure FreePKBookMark(var Bmk: TPKBookMark);
begin
  if Assigned(Bmk) then
  begin
    VarClear(Bmk^.PKValues);
    Dispose(Bmk);
    Bmk := nil;
  end;
end;

function DefaultLocateDataSet(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; const Options: TLocateOptions): Boolean;
begin
  Result := DataSet.Locate(KeyFields, KeyValues, Options);
end;

function LocateEx(DataSet: TDataSet; FieldsPK: string; const ValuesPK: Variant): Boolean;
begin
  AppSetCursor(crSQLWait);
  try
    with DataSet do
    begin
      DisableControls;
      try
        try
          if FieldsPK = '' then FieldsPK := DefaultKeyFields;
          Result := LocateDataSet(DataSet, FieldsPK, ValuesPK, []);
        except
          Result := False;
        end;
      finally
        EnableControls;
      end;
    end;
  finally
    AppRestoreCursor;
  end;
end;

function IsEqualFieldsList(Fields: TList; const Values: Variant): Boolean;
var
  I: Integer;
begin
  if Fields.Count = 0 then
    Result := False
  else if Fields.Count = 1 then
    Result := VarIsEqual(TField(Fields[0]).Value, Values)
  else begin
    for I := 0 to Fields.Count - 1 do
      if not VarIsEqual(TField(Fields[I]).Value, Values[I]) then
      begin
        Result := False;
        Exit;
      end;
    Result := True;
  end;
end;

function IsEqualFields(DataSet: TDataSet; const FieldNames: string; const Values: Variant): Boolean;
var
  Fields: TList;
begin
  Fields := TList.Create;
  try
    DataSet.GetFieldList(Fields, FieldNames);
    Result := IsEqualFieldsList(Fields, Values);
  finally
    Fields.Free;
  end;
end;

function LocateFullSearch(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; const LocateOrigin: TLocateOrigin;
  const LocateDirection: TLocateDirection): Boolean;
var
  Fields: TList;
  Bmk: TBookmark;
begin
  Result := False;
  Bmk := DataSet.GetBookmark;
  try
    Fields := TList.Create;
    try
      DataSet.GetFieldList(Fields, KeyFields);

      case LocateOrigin of
        loCurrent:
          ;
        loFirst:
          DataSet.First;
        loNext:
          DataSet.Next;
        loPrior:
          DataSet.Prior;
        loLast:
          DataSet.Last;
      end;

      while not Result and not (((LocateDirection = ldForward) and DataSet.EOF
        or (LocateDirection = ldBackward) and DataSet.BOF)) do
      begin
        Result := IsEqualFieldsList(Fields, KeyValues);
        if not Result then
          if LocateDirection = ldForward then
            DataSet.Next else DataSet.Prior;
      end;
    finally
      Fields.Free;
    end;
  finally
    try
      if not Result then DataSet.GotoBookmark(Bmk);
    except end;
    DataSet.FreeBookmark(Bmk);
  end;
end;

function LocateFullSearchEx(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant): Boolean;
begin
  Result := LocateFullSearch(DataSet, KeyFields, KeyValues, loFirst, ldForward);
end;


var
  RefreshClassList: TClassList = nil;

procedure RegisterRefreshProc(DataSetClass: TDataSetClass; Proc: TRefreshProc);
begin
  if not Assigned(RefreshClassList) then RefreshClassList := TClassList.Create;
  RefreshClassList.RegisterClass(DataSetClass, @Proc, '', True);
end;

procedure ReopenDataSetEx(DataSet: TDataSet; const FieldsPK: string; const ValuesPK: Variant);
var
  Proc: TRefreshProc;
begin
  Proc := FindClassListData(RefreshClassList, DataSet.ClassType);
  if not Assigned(Proc) then Proc := @DefaultReopenDataSetEx;
  Proc(DataSet, FieldsPK, ValuesPK);
end;

procedure ReopenDataSet(DataSet: TDataSet; FieldsPK: string);
begin
  if FieldsPK = '' then FieldsPK := DefaultKeyFields;
  ReopenDataSetEx(DataSet, FieldsPK, Unassigned);
end;

procedure RefreshDataSet(DataSet: TDataSet);
begin
  ReopenDataSet(DataSet, '');
end;

procedure Resync(DataSet: TDataSet);
begin
  with DataSet do if Active then
  begin
    UpdateCursorPos;
    Resync([]);
  end;
end;

procedure UpdateRecord(DataSet: TDataSet);
begin
  with DataSet do if (State in dsEditModes) then UpdateRecord;
end;

procedure SetFilter(DataSet: TDataSet; const Filter: string; Filtered: Boolean);
begin
  if (DataSet.Filtered <> Filtered) or (DataSet.Filter <> Filter) then
  begin
    DataSet.DisableControls;
    try
      DataSet.Filter := Filter;
      DataSet.Filtered := Filtered;
    finally
      DataSet.EnableControls;
    end;
  end;
end;

function DatesFilter(const DateField: string; const StartDate, EndDate: TDateTime): string;
begin
  Result := '';
  if (StartDate <> 0) then
    Result := Format('%s >= ''%s''', [QuoteSQLName(DateField), DateTimeToStr(StartDate)]) else
    Result := TrueSQL;
  if (EndDate <> 0) then
    Result := Format('%s and %s <= ''%s''', [Result, QuoteSQLName(DateField), DateTimeToStr(EndDate)]);
end;

function FindColumn(Columns: TDBGridColumns; FieldName: string): TColumn;
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do
  begin
    Result := Columns.Items[I];
    if AnsiCompareText(Result.FieldName, FieldName) = 0 then Exit;
  end;
  Result := nil;
end;

procedure SetColumnSettings(Column: TColumn; Settings: TColumnSettings);
begin
  if not Assigned(Column) then Exit;
  with Column, Settings do
  begin
    Color := BkColor;
    Font.Color := FontColor;
    Font.Style := FontStyle;
  end;
end;

procedure UpdateColumns(Fields: TList; Columns: TDBGridColumns);
var
  I, J: Integer;
  ColList: TList;
  Column: TColumn;
begin
  ColList := TList.Create;
  Columns.BeginUpdate;
  try
    Columns.State := csCustomized;
    for I := 0 to Fields.Count - 1 do
    begin
      for J := 0 to Columns.Count - 1 do
      begin
        Column := Columns[J];
        if (AnsiCompareText(Column.FieldName, TField(Fields[I]).FieldName) = 0) and
          (TField(Fields[I]).Visible) then
        begin
          ColList.Add(Column);
          Break;
        end;
      end;
    end;
    for I := Columns.Count - 1 downto 0 do
    begin
      Column := Columns[I];
      Column.Collection := nil;
      if ColList.IndexOf(Column) < 0 then Column.Free;
    end;

    for I := 0 to ColList.Count - 1 do
    begin
      Column := TColumn(ColList[I]);
      Column.Collection := Columns;
    end;
  finally
    ColList.Free;
    Columns.EndUpdate;
  end;
end;

procedure UpdateColumnsForDataSet(DataSet: TDataSet; Columns: TDBGridColumns);
var
  I: Integer;
  Fields: TList;
begin
  Fields := TList.Create;
  try
    for I := 0 to DataSet.FieldCount - 1 do
      Fields.Add(DataSet.Fields[I]);
    UpdateColumns(Fields, Columns);
  finally
    Fields.Free;
  end;
end;

procedure ForEachColumn(Columns: TDBGridColumns;
  CallBack: TColumnCallBack; Info: Pointer);
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do CallBack(Columns[I], Info);
end;

procedure ColumnsAlignTitles(Columns: TDBGridColumns; Alignment: TAlignment);
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do
    Columns[I].Title.Alignment := Alignment;
end;


function ConfirmDelete: Boolean;
begin
  AppSetCursor(crDefault);
  try
    Result := MessageDlg(LoadStr(SConfirmDelete), mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  finally
    AppRestoreCursor;
  end;
end;

procedure CheckNotNullValue(Field: TField);
begin
  if Field.IsNull then
    WarningMessage(Format(LoadStr(SNotNullReqired), [Field.DisplayLabel]));
end;

procedure CheckNullValue(Field: TField);
begin
  if not Field.IsNull then
    WarningMessage(Format(LoadStr(SNullReqired), [Field.DisplayLabel]));
end;

procedure CheckLength(Field: TField; NeededLength: Integer);
var
  Msg: string;
begin
  Field.AsString := Trim(Field.AsString);
  if Length(Field.AsString) < NeededLength then
  begin
    if NeededLength = 1 then Msg := LoadStr(SFieldRequired)
    else Msg := LoadStr(SLengthTooSmall);
    WarningMessage(Format(Msg, [Field.DisplayLabel, NeededLength]));
  end;
end;

procedure CheckIntegerRange(Field: TIntegerField);
begin
  if RangeCheck(Field.Value, Field.MinValue, Field.MaxValue) <> Field.Value then
    WarningMessage(Format(LoadStr(SIntFieldOutOfRange), [Field.DisplayLabel, Field.MinValue, Field.MaxValue]));
end;

procedure CheckNotZeroValue(Field: TField);
begin
  if Field.AsFloat = 0 then
    WarningMessage(Format(LoadStr(SNonZeroValueReq), [Field.DisplayLabel]));
end;

procedure CheckCheckedAndNotNull(CheckField, DataField: TField);
begin
  CheckNotNullValue(CheckField);
  if CheckField.AsBoolean then CheckNotNullValue(DataField);
end;

procedure CheckNotEqualMsg(Field1, Field2: TField; Msg: string);
begin
  CheckNotNullValue(Field1);
  CheckNotNullValue(Field2);
  if Field1.Value = Field2.Value then WarningMessage(Msg);
end;

procedure CheckNotEqual(Field1, Field2: TField);
begin
  CheckNotEqualMsg(Field1, Field2, Format(LoadStr(SNotEqualReq), [Field1.DisplayLabel, Field2.DisplayLabel]));
end;

procedure CheckMoreThan(Field: TField; Value: Integer);
begin
  CheckNotNullValue(Field);
  if Field.AsFloat <= Value then
    WarningMessage(Format(LoadStr(SMoreThanReq), [Field.DisplayLabel, Value]));
end;

procedure CheckMoreOrEqualThan(Field: TField; Value: Integer);
begin
  CheckNotNullValue(Field);
  if Field.AsInteger < Value then
    WarningMessage(Format(LoadStr(SMoreThanReq), [Field.DisplayLabel, Value]));
end;

procedure CheckLessThan(Field: TField; Value: Integer);
begin
  CheckNotNullValue(Field);
  if Field.AsInteger >= Value then
    WarningMessage(Format(LoadStr(SLessThanReq), [Field.DisplayLabel, Value]));
end;

procedure CheckEqualTo(Field: TField; Value: Integer);
begin
  CheckNotNullValue(Field);
  if Field.AsInteger <> Value then
    WarningMessage(Format(LoadStr(SEqualReq), [Field.DisplayLabel, Value]));
end;

procedure CheckRange(Field: TField; Value1, Value2: Double);
begin
  CheckNotNullValue(Field);
  if (Field.AsFloat < Value1) or (Field.AsFloat > Value2) then
    WarningMessage(Format(LoadStr(SValueRange), [Field.DisplayLabel, Value1, Value2]));
end;

procedure CheckRowsAffected(RowsActual, RowsNeeded: Integer);
begin
  if RowsActual <> RowsNeeded then DatabaseError(ResStr(SUpdateFailed));
end;

function Equals(Value: Boolean): string;
begin
  if Value then Result := LoadStr(SEqualStr) else Result := '';
end;

procedure CheckTwoNumeric(Field1, Field2: TField; CanEqual: Boolean);
begin
  CheckNotNullValue(Field1); CheckNotNullValue(Field2);
  if not CanEqual and (Field1.AsFloat = Field2.AsFloat) or (Field1.AsFloat > Field2.AsFloat) then
    WarningMessage(Format(LoadStr(SNumericSequenceReq), [Field2.DisplayLabel, Equals(CanEqual), Field1.DisplayLabel]));
end;

procedure CheckTwoDates(Field1, Field2: TField; CanEqual: Boolean);
begin
  CheckNotNullValue(Field1); CheckNotNullValue(Field2);
  if not CanEqual and (Field1.AsDateTime = Field2.AsDateTime) or (Field1.AsDateTime > Field2.AsDateTime) then
    WarningMessage(Format(LoadStr(SDateSequenceReq), [Field2.DisplayLabel, Equals(CanEqual), Field1.DisplayLabel]));
end;

procedure RecordNotFound;
begin
  WarningMessage(LoadStr(SRecordNotFound));
end;

procedure RecordNotFoundTbl(Msg: string);
begin
  WarningMessage(Format(LoadStr(SRecordNotFoundTbl), [Msg]));
end;

function IsEmpty(DataSet: TDataSet): Boolean;
begin
  with DataSet do Result := Assigned(DataSet) and ((not Active) or (Eof and Bof));
end;

procedure CheckNotEmpty(DataSet: TDataSet);
begin
  if IsEmpty(DataSet) then RecordNotFound;
end;

procedure CheckNotEmptyTbl(DataSet: TDataSet; Msg: string);
begin
  if IsEmpty(DataSet) then RecordNotFoundTbl(Msg);
end;

procedure InsertRestrict;
begin
  WarningMessage(LoadStr(SInsertNotAvail));
end;

procedure EditRestrict;
begin
  WarningMessage(LoadStr(SEditNotAvail));
end;

procedure DeleteRestrict;
begin
  WarningMessage(LoadStr(SDeleteNotAvail));
end;

procedure OpenDataSets(const DataSets: array of TDataSet);
var
  I: Integer;
begin
  if DataSets[0] = nil then Exit;
  AppSetCursor(crSQLWait);
  try
    for I := Low(DataSets) to High(DataSets) do OpenDataSet(DataSets[I]);
  finally
    AppRestoreCursor;
  end;
end;

procedure CloseDataSets(const DataSets: array of TDataSet);
var
  I: Integer;
begin
  if DataSets[0] = nil then Exit;

  AppSetCursor(crSQLWait);
  try
    for I := Low(DataSets) to High(DataSets) do DataSets[I].Close;
  finally
    AppRestoreCursor;
  end;
end;

procedure DefaultOpenDataSet(DataSet: TDataSet);
begin
  AppSetCursor(crSQLWait);
  try
    repeat
      try
        DataSet.Open;
        Break;
      except
        if IsMainThread then
        begin
          if (MessageDlg(Format(LoadStr(SOpenTableFailed), [Exception(ExceptObject).Message]),
             mtWarning, [mbYes, mbNo], 0) = mrNo) then SysUtils.Abort;
        end else
          Application.HandleException(DataSet);
      end;
    until False;
  finally
    AppRestoreCursor;
  end;
end;

procedure DefaultFetchDataSet(DataSet: TDataSet);
var
  Bmk: TBookMark;
begin
  AppSetCursor(crSQLWait);
  try
    with DataSet do
    begin
      DisableControls;
      try
        Bmk := DataSet.GetBookMark;
        try
          Last;
          GotoBookmark(Bmk);
        finally
          FreeBookmark(Bmk);
        end;
      finally
        EnableControls;
      end;
    end;
  finally
    AppRestoreCursor;
  end;
end;

procedure DefaultReopenDataSetEx(DataSet: TDataSet; FieldsPK: string; const ValuesPK: Variant);
var
  Bmk: TPKBookMark;
begin
  AppSetCursor(crSQLWait);
  try
    with DataSet do
    begin
      DisableControls;
      try
        if FieldsPK = '' then FieldsPK := DefaultKeyFields;
        Bmk := GetPKBookMark(DataSet, FieldsPK);
        try
          Close;
          OpenDataSet(DataSet);
          if VarIsEmpty(ValuesPK) or not LocateEx(DataSet, FieldsPK, ValuesPK) then
            SetToPKBookMark(DataSet, Bmk);
        finally
          FreePKBookMark(Bmk);
        end;
      finally
        EnableControls;
      end;
    end;
  finally
    AppRestoreCursor;
  end;
end;

procedure RegisterFieldClasses;
{$IFDEF _D3_}
var
  I: TFieldType;
{$ENDIF}
begin
{$IFDEF _D3_}
  for I := Low(DefaultFieldClasses) to High(DefaultFieldClasses) do
    if DefaultFieldClasses[I] <> nil then
      RegisterClass(DefaultFieldClasses[I]);
{$ELSE}
  RegisterClasses([
    TStringField,   
    TSmallintField, 
    TIntegerField,  
    TWordField,     
    TBooleanField,  
    TFloatField,    
    TCurrencyField,
    TBCDField,      
    TDateField,
    TTimeField,     
    TDateTimeField, 
    TBytesField,
    TVarBytesField,
    TAutoIncField,  
    TBlobField,     
    TMemoField,     
    TGraphicField,
    TBlobField
  ]);
{$ENDIF}
end;

function NameDelimiter(C: Char; const Delims: TCharSet): Boolean;
begin
  Result := (C in [' ', ',', ';', ')', #13, #10]) or (C in Delims);
end;

function IsLiteral(C: Char): Boolean;
const
  Literals = ['''', '"', '`'];
begin
  Result := C in Literals;
end;

function StripLiterals(const Buffer: PChar): string;
var
  Len: Word;
  TempBuf: PChar;

  procedure StripChar;
  begin
    if IsLiteral(TempBuf^) then
      StrMove(TempBuf, TempBuf + 1, Len - 1);
    if IsLiteral(TempBuf[StrLen(TempBuf) - 1]) then
      TempBuf[StrLen(TempBuf) - 1] := #0;
  end;

begin
  Len := StrLen(Buffer) + 1;
  TempBuf := AllocMem(Len);
  Result := '';
  try
    StrCopy(TempBuf, Buffer);
    StripChar;
    Result := StrPas(TempBuf);
  finally
    FreeMem(TempBuf, Len);
  end;
end;

function FieldDefsToVariant(FieldDefs: TFieldDefs): Variant;
var
  I: Integer;
begin
  if FieldDefs.Count = 0 then
    Result := Null
  else begin
    Result := VarArrayCreate([0, FieldDefs.Count - 1], varVariant);
    for I := 0 to FieldDefs.Count - 1 do
      with FieldDefs[I] do
        Result[I] := VarArrayOf([Name, Integer(DataType), Size, Required]);
  end;
end;

procedure VariantToFieldDefs(const Value: Variant; FieldDefs: TFieldDefs);
var
  I: Integer;
begin
  FieldDefs.Clear;
  if VarIsNull(Value) then Exit;
  for I := 0 to VarArrayHighBound(Value, 1) do
    FieldDefs.Add(Value[I][0], TFieldType(Value[I][1]), Value[I][2], Value[I][3]) ;
end;

function IndexDefsToVariant(IndexDefs: TIndexDefs): Variant;
var
  I: Integer;
begin
  if IndexDefs.Count = 0 then
    Result := Null
  else begin
    Result := VarArrayCreate([0, IndexDefs.Count - 1], varVariant);
    for I := 0 to IndexDefs.Count - 1 do
      with IndexDefs[I] do
        Result[I] := VarArrayOf([Name, Fields, Byte(Options), Source]);
  end;
end;

{$IFNDEF _D3_}
type
  TIndexDefHack = class
    FOwner: TIndexDefs;
    FSource: string;
  end;
{$ENDIF}

procedure VariantToIndexDefs(const Value: Variant; IndexDefs: TIndexDefs);
var
  I: Integer;
begin
  IndexDefs.Clear;
  if VarIsNull(Value) then Exit;
  for I := 0 to VarArrayHighBound(Value, 1) do
{$IFDEF _D3_}
  {$IFDEF CBuilder}
    with TIndexDef.Create(IndexDefs) do
    begin
      Name := string(Value[I][0]);
      Fields := string(Value[I][1]);
      Options := TIndexOptions(Byte(Value[I][2]));
    end;
 {$ELSE}
    with TIndexDef.Create(IndexDefs, string(Value[I][0]), string(Value[I][1]), TIndexOptions(Byte(Value[I][2]))) do
      Source := Value[I][3];
 {$ENDIF}
{$ELSE}
    with TIndexDefHack(TIndexDef.Create(IndexDefs, string(Value[I][0]), string(Value[I][1]), TIndexOptions(Byte(Value[I][2])))) do
      FSource := Value[I][3];
{$ENDIF}
end;

initialization
  RegisterRefreshProc(TDataSet, @DefaultReopenDataSetEx);

finalization
  RefreshClassList.Free;
  PostProcClassList.Free;
  DeleteProcClassList.Free;
{$IFDEF _D3_}
  FreeInternalInitFieldDefss;
{$ENDIF}

end.







