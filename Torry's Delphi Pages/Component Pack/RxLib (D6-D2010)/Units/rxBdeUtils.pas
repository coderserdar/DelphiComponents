{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

unit rxBdeUtils;

{$I RX.INC}
{$W-,R-,B-,N+,P+}

interface

uses
  SysUtils, Windows, Bde, Registry, RTLConsts, Classes, DB, DBTables,
  IniFiles, rxDBUtils;

type
{$IFNDEF RX_D3}
  TBDEDataSet = TDataSet;
{$ENDIF}

{$IFNDEF RX_D5}
  TDatabaseLoginEvent = TLoginEvent;
{$ENDIF}

  TDBLocate = class(TLocateObject)
  private
    function LocateCallback: Boolean;
    procedure RecordFilter(DataSet: TDataSet; var Accept: Boolean);
  protected
    function LocateFilter: Boolean; override;
    procedure CheckFieldType(Field: TField); override;
    function LocateKey: Boolean; override;
    function UseKey: Boolean; override;
    function FilterApplicable: Boolean; override;
  public
    destructor Destroy; override;
  end;

{ TCloneDataset }

  TCloneDataset = class(TBDEDataSet)
  private
    FSourceHandle: HDBICur;
    FReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure SetSourceHandle(ASourceHandle: HDBICur);
  protected
    function CreateHandle: HDBICur; override;
  public
    property SourceHandle: HDBICur read FSourceHandle write SetSourceHandle;
  published
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  end;

{ TCloneDbDataset }

  TCloneDbDataset = class(TDBDataSet)
  private
    FSourceHandle: HDBICur;
    FReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure SetSourceHandle(ASourceHandle: HDBICur);
  protected
    function CreateHandle: HDBICur; override;
  public
    procedure InitFromDataSet(Source: TDBDataSet; Reset: Boolean);
    property SourceHandle: HDBICur read FSourceHandle write SetSourceHandle;
  published
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  end;

{ TCloneTable }

  TCloneTable = class(TTable)
  private
    FSourceHandle: HDBICur;
    FReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure SetSourceHandle(ASourceHandle: HDBICur);
  protected
    function CreateHandle: HDBICur; override;
  public
    procedure InitFromTable(SourceTable: TTable; Reset: Boolean);
  published
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  end;

{ Utility routines }

function CreateDbLocate: TLocateObject;
procedure FetchAllRecords(DataSet: TBDEDataSet);
function TransActive(Database: TDatabase): Boolean;
function AsyncQrySupported(Database: TDatabase): Boolean;
function GetQuoteChar(Database: TDatabase): string;
procedure ExecuteQuery(const DbName, QueryText: string);
procedure ExecuteQueryEx(const SessName, DbName, QueryText: string);
procedure BdeTranslate(Locale: TLocale; Source, Dest: PAnsiChar; ToOem: Boolean);
function FieldLogicMap(FldType: TFieldType): Integer;
function FieldSubtypeMap(FldType: TFieldType): Integer;
procedure ConvertStringToLogicType(Locale: TLocale; FldLogicType: Integer;
  FldSize: Word; const FldName, Value: AnsiString; Buffer: Pointer);
function GetAliasPath(const AliasName: string): string;
function IsDirectory(const DatabaseName: string): Boolean;
function GetBdeDirectory: string;
function BdeErrorMsg(ErrorCode: DBIResult): string;
function LoginToDatabase(Database: TDatabase; OnLogin: TDatabaseLoginEvent): Boolean;
function DataSetFindValue(ADataSet: TBDEDataSet; const Value,
  FieldName: string): Boolean;
function DataSetFindLike(ADataSet: TBDEDataSet; const Value,
  FieldName: string): Boolean;
function DataSetRecNo(DataSet: TDataSet): Longint;
function DataSetRecordCount(DataSet: TDataSet): Longint;
function DataSetPositionStr(DataSet: TDataSet): string;
procedure DataSetShowDeleted(DataSet: TBDEDataSet; Show: Boolean);
function CurrentRecordDeleted(DataSet: TBDEDataSet): Boolean;
function IsFilterApplicable(DataSet: TDataSet): Boolean;
function IsBookmarkStable(DataSet: TBDEDataSet): Boolean;
function BookmarksCompare(DataSet: TBDEDataSet; Bookmark1,
  Bookmark2: TBookmark): Integer;
function SetToBookmark(ADataSet: TDataSet; ABookmark: TBookmark): Boolean;
procedure SetIndex(Table: TTable; const IndexFieldNames: string);
procedure RestoreIndex(Table: TTable);
procedure DeleteRange(Table: TTable; IndexFields: array of const;
  FieldValues: array of const);
procedure PackTable(Table: TTable);
procedure ReindexTable(Table: TTable);
procedure BdeFlushBuffers;
function GetNativeHandle(Database: TDatabase; Buffer: Pointer;
  BufSize: Integer): Pointer;
procedure ToggleDebugLayer(Active: Boolean; const DebugFile: string);
procedure DbNotSupported;

{ Export/import DataSet routines }

procedure ExportDataSet(Source: TBDEDataSet; DestTable: TTable;
  TableType: TTableType; const AsciiCharSet: string;
  AsciiDelimited: Boolean; MaxRecordCount: Longint);
procedure ExportDataSetEx(Source: TBDEDataSet; DestTable: TTable;
  TableType: TTableType; const AsciiCharSet: string;
  AsciiDelimited: Boolean; AsciiDelimiter, AsciiSeparator: Char;
  MaxRecordCount: Longint);
procedure ImportDataSet(Source: TBDEDataSet; DestTable: TTable;
  MaxRecordCount: Longint; Mappings: TStrings; Mode: TBatchMode);

{ ReportSmith initialization }

procedure InitRSRUN(Database: TDatabase; const ConName: string;
  ConType: Integer; const ConServer: string);

implementation

uses
  Forms, Controls, Dialogs, Consts, DBConsts, RXDConst, rxVCLUtils,
  rxFileUtil, rxAppUtils, rxStrUtils, rxMaxMin,
  {$IFDEF RX_D3} BDEConst, DBCommon, {$ENDIF} rxDateUtil;

{ Utility routines }

{$IFDEF RX_D5}
procedure DBError(Ident: Word);
begin
  DatabaseError(LoadStr(Ident));
end;
{$ENDIF}

function IsBookmarkStable(DataSet: TBDEDataSet): Boolean;
var
  Props: CURProps;
begin
  with DataSet do
    Result := Active and (DbiGetCursorProps(Handle, Props) = DBIERR_NONE) and
      Props.bBookMarkStable;
end;

function SetToBookmark(ADataSet: TDataSet; ABookmark: TBookmark): Boolean;
begin
  Result := False;
{$IFDEF RX_D3}
  with ADataSet do
    if Active and (ABookmark <> nil) and not (Bof and Eof) and
      BookmarkValid(ABookmark) then
    try
      ADataSet.GotoBookmark(ABookmark);
      Result := True;
    except
    end;
{$ELSE}
  with TBDEDataSet(ADataSet) do
    if Active and (ABookmark <> nil) and not (Bof and Eof) then
      if DbiSetToBookmark(Handle, ABookmark) = DBIERR_NONE then
      try
        Resync([rmExact, rmCenter]);
        Result := True;
      except
      end;
{$ENDIF}
end;

function BookmarksCompare(DataSet: TBDEDataSet; Bookmark1, Bookmark2: TBookmark): Integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt =
    ((2, CMPLess), (CMPGtr, CMPEql));
begin
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
  begin
    Check(DbiCompareBookmarks(DataSet.Handle, Bookmark1, Bookmark2, Result));
    if Result = CMPKeyEql then
      Result := CMPEql;
  end;
end;

function DBGetIntProp(const Handle: Pointer; PropName: Longint): Longint;
var
  Length: Word;
  Value: Longint;
begin
  Value := 0;
  Check(DbiGetProp(HDBIObj(Handle), PropName, @Value, SizeOf(Value), Length));
  Result := Value;
end;

function GetQuoteChar(Database: TDatabase): string;
{$IFNDEF RX_D3}
const
  dbQUOTECHAR = $0404000A;
{$ENDIF}
var
  Q: Char;
  Len: Word;
begin
  Result := '';
  if Database.IsSQLBased then
  begin
    Q := #0;
    DbiGetProp(HDBIObj(Database.Handle), dbQUOTECHAR, @Q, SizeOf(Q), Len);
    if Q <> #0 then
      Result := Q;
  end
  else
    Result := '"';
end;

function AsyncQrySupported(Database: TDatabase): Boolean;
begin
  Result := False;
  if Database.Connected then
    if Database.IsSQLBased then
      try
        Result := BOOL(DBGetIntProp(Database.Handle, dbASYNCSUPPORT));
      except
      end
    else
      Result := True;
end;

function FieldLogicMap(FldType: TFieldType): Integer;
{$IFNDEF RX_D3}
{$IFDEF VER80}
const
  FldTypeMap: array[TFieldType] of Integer = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL,
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES,
    fldVARBYTES, fldBLOB, fldBLOB, fldBLOB);
{$ELSE}
const
  FldTypeMap: array[TFieldType] of Integer = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL,
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES,
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB,
    fldBLOB, fldBLOB);
{$ENDIF}
{$ENDIF}
begin
  Result := FldTypeMap[FldType];
end;

function FieldSubtypeMap(FldType: TFieldType): Integer;
{$IFNDEF RX_D3}
{$IFDEF VER80}
const
  FldSubtypeMap: array[TFieldType] of Integer = (
    0, 0, 0, 0, 0, 0, 0, fldstMONEY, 0, 0, 0, 0, 0, 0, fldstBINARY,
    fldstMEMO, fldstGRAPHIC);
{$ELSE}
const
  FldSubtypeMap: array[TFieldType] of Integer = (
    0, 0, 0, 0, 0, 0, 0, fldstMONEY, 0, 0, 0, 0, 0, 0, fldstAUTOINC,
    fldstBINARY, fldstMEMO, fldstGRAPHIC, fldstFMTMEMO, fldstOLEOBJ,
    fldstDBSOLEOBJ, fldstTYPEDBINARY);
{$ENDIF}
{$ENDIF}
begin
  Result := FldSubtypeMap[FldType];
end;

{ Routine for convert string to IDAPI logical field type }

procedure ConvertStringToLogicType(Locale: TLocale; FldLogicType: Integer;
  FldSize: Word; const FldName, Value: AnsiString; Buffer: Pointer);
var
  Allocate: Boolean;
  BCD: FMTBcd;
  E: Integer;
  L: Longint;
  B: WordBool;
  DateTime: TDateTime;
  DtData: TDateTime;
  D: Double absolute DtData;
  Data: Longint absolute DtData;
  TimeStamp: TTimeStamp;
begin
  if Buffer = nil then
  begin
    Buffer := AllocMem(FldSize);
    Allocate := Buffer <> nil;
  end
  else
    Allocate := False;
  try
    case FldLogicType of
      fldZSTRING:
        AnsiToNative(Locale, Value, PAnsiChar(Buffer), FldSize);
      fldBYTES, fldVARBYTES:
        Move(Value[1], Buffer^, Min(Length(Value), FldSize));
      fldINT16, fldINT32, fldUINT16:
        begin
          if Value = '' then
            FillChar(Buffer^, FldSize, 0)
          else
          begin
            Val(string(Value), L, E);
            if E <> 0 then
{$IFDEF RX_D3}
              DatabaseErrorFmt(SInvalidIntegerValue, [Value, FldName]);
{$ELSE}
              DBErrorFmt(SInvalidIntegerValue, [Value, FldName]);
{$ENDIF}
            Move(L, Buffer^, FldSize);
          end;
        end;
      fldBOOL:
        begin
          L := Length(Value);
          if L = 0 then
            B := False
          else
          begin
            if Value[1] in ['Y', 'y', 'T', 't', '1'] then
              B := True
            else
              B := False;
          end;
          Move(B, Buffer^, SizeOf(WordBool));
        end;
      fldFLOAT, fldBCD:
        begin
          if Value = '' then
            FillChar(Buffer^, FldSize, 0)
          else
          begin
            D := StrToFloat(string(Value));
            if FldLogicType <> fldBCD then
              Move(D, Buffer^, SizeOf(Double))
            else
            begin
              DbiBcdFromFloat(D, 32, FldSize, BCD);
              Move(BCD, Buffer^, SizeOf(BCD));
            end;
          end;
        end;
      fldDATE, fldTIME, fldTIMESTAMP:
        begin
          if Value = '' then
            Data := Trunc(NullDate)
          else
          begin
            case FldLogicType of
              fldDATE:
                begin
                  DateTime := StrToDate(string(Value));
                  TimeStamp := DateTimeToTimeStamp(DateTime);
                  Data := TimeStamp.Date;
                end;
              fldTIME:
                begin
                  DateTime := StrToTime(string(Value));
                  TimeStamp := DateTimeToTimeStamp(DateTime);
                  Data := TimeStamp.Time;
                end;
              fldTIMESTAMP:
                begin
                  DateTime := StrToDateTime(string(Value));
                  TimeStamp := DateTimeToTimeStamp(DateTime);
                  D := TimeStampToMSecs(DateTimeToTimeStamp(DateTime));
                end;
            end;
          end;
          Move(D, Buffer^, FldSize);
        end;
      else
        DbiError(DBIERR_INVALIDFLDTYPE);
    end;
  finally
    if Allocate then
      FreeMem(Buffer, FldSize);
  end;
end;

{ Execute Query routine }

procedure ExecuteQueryEx(const SessName, DbName, QueryText: string);
begin
  with TQuery.Create(Application) do
  try
    DatabaseName := DbName;
    SessionName := SessName;
    SQL.Add(QueryText);
    ExecSQL;
  finally
    Free;
  end;
end;

procedure ExecuteQuery(const DbName, QueryText: string);
begin
  ExecuteQueryEx('', DbName, QueryText);
end;

{ Database Login routine }

function LoginToDatabase(Database: TDatabase; OnLogin: TDatabaseLoginEvent): Boolean;
var
  EndLogin: Boolean;
begin
  Result := Database.Connected;
  if Result then
    Exit;
  Database.OnLogin := OnLogin;
  EndLogin := True;
  repeat
    try
      Database.Connected := True;
      EndLogin := True;
    except
      on E: EDbEngineError do
        EndLogin := (MessageDlg(E.Message + '. ' + LoadStr(SRetryLogin),
          mtConfirmation, [mbYes, mbNo], 0) <> mrYes);
      on E: EDatabaseError do
        { User select "Cancel" in login dialog }
        MessageDlg(E.Message, mtError, [mbOk], 0);
      else
        raise;
    end;
  until EndLogin;
  Result := Database.Connected;
end;

{ ReportSmith runtime initialization routine }

procedure InitRSRUN(Database: TDatabase; const ConName: string;
  ConType: Integer; const ConServer: string);
const
  IniFileName = 'RPTSMITH.CON';
  scConNames = 'ConnectNamesSection';
  idConNames = 'ConnectNames';
  idType = 'Type';
  idServer = 'Server';
  idSQLDataFilePath = 'Database';
  idDataFilePath = 'DataFilePath';
  idSQLUserID = 'USERID';
var
  ParamList: TStringList;
  DBPath: string[127];
  TempStr, AppConName: string;
  UserName: string[30];
  ExeName: string;
  IniFile: TIniFile;
begin
  ParamList := TStringList.Create;
  try
    Database.Session.GetAliasParams(Database.AliasName, ParamList);
    if Database.IsSQLBased then
      DBPath := AnsiString(ParamList.Values['SERVER NAME'])
    else
      DBPath := AnsiString(ParamList.Values['PATH']);
    UserName := AnsiString(ParamList.Values['USER NAME']);
  finally
    ParamList.Free;
  end;
  AppConName := ConName;
  if AppConName = '' then
  begin
    ExeName := ExtractFileName(Application.ExeName);
    AppConName := Copy(ExeName, 1, Pos('.', ExeName) - 1);
  end;
  IniFile := TIniFile.Create(IniFileName);
  try
    TempStr := IniFile.ReadString(scConNames, idConNames, '');
    if Pos(AppConName, TempStr) = 0 then
    begin
      if TempStr <> '' then
        TempStr := TempStr + ',';
      IniFile.WriteString(scConNames, idConNames, TempStr + AppConName);
    end;
    IniFile.WriteInteger(AppConName, idType, ConType);
    IniFile.WriteString(AppConName, idServer, ConServer);
    if Database.IsSQLBased then
    begin
      IniFile.WriteString(AppConName, idSQLDataFilePath, string(DBPath));
      IniFile.WriteString(AppConName, idSQLUserID, string(UserName));
    end
    else
      IniFile.WriteString(AppConName, idDataFilePath, string(DBPath));
  finally
    IniFile.Free;
  end;
end;

{ BDE aliases routines }

function IsDirectory(const DatabaseName: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  if (DatabaseName = '') then
    Exit;
  I := 1;
  while I <= Length(DatabaseName) do
  begin
{$IFDEF RX_D3}
    if CharInSet(DatabaseName[I], LeadBytes) then
      Inc(I)
    else
{$ENDIF RX_D3}
    if CharInSet(DatabaseName[I], [':','\']) then
      Exit;
    Inc(I);
  end;
  Result := False;
end;

function GetAliasPath(const AliasName: string): string;
var
  SAlias: DBINAME;
  Desc: DBDesc;
  Params: TStrings;
begin
  Result := '';
  StrPLCopy(SAlias, AnsiString(AliasName), SizeOf(SAlias) - 1);
  AnsiToOem(SAlias, SAlias);
  Check(DbiGetDatabaseDesc(SAlias, @Desc));
  if StrIComp(Desc.szDbType, szCFGDBSTANDARD) = 0 then
  begin
    OemToAnsi(Desc.szPhyName, Desc.szPhyName);
    Result := string(StrPas(Desc.szPhyName));
  end
  else
  begin
    Params := TStringList.Create;
    try
      Session.Active := True;
      Session.GetAliasParams(AliasName, Params);
      Result := Params.Values['SERVER NAME'];
    finally
      Params.Free;
    end;
  end;
end;

{ TCloneDataset }

procedure TCloneDataset.SetSourceHandle(ASourceHandle: HDBICur);
begin
  if ASourceHandle <> FSourceHandle then
  begin
    Close;
    FSourceHandle := ASourceHandle;
    if FSourceHandle <> nil then
      Open;
  end;
end;

function TCloneDataset.CreateHandle: HDBICur;
begin
  Check(DbiCloneCursor(FSourceHandle, FReadOnly, False, Result));
end;

procedure TCloneDataset.SetReadOnly(Value: Boolean);
begin
  CheckInactive;
  FReadOnly := Value;
end;

{ TCloneDbDataset }

procedure TCloneDbDataset.InitFromDataSet(Source: TDBDataSet; Reset: Boolean);
begin
  with Source do
  begin
    Self.SessionName := SessionName;
    Self.DatabaseName := DatabaseName;
    SetSourceHandle(Handle);
    Self.Filter := Filter;
    Self.OnFilterRecord := OnFilterRecord;
    if not Reset then
      Self.Filtered := Filtered;
  end;
  if Reset then
  begin
    Filtered := False;
    First;
  end;
end;

procedure TCloneDbDataset.SetSourceHandle(ASourceHandle: HDBICur);
begin
  if ASourceHandle <> FSourceHandle then
  begin
    Close;
    FSourceHandle := ASourceHandle;
    if FSourceHandle <> nil then
      Open;
  end;
end;

function TCloneDbDataset.CreateHandle: HDBICur;
begin
  Check(DbiCloneCursor(FSourceHandle, FReadOnly, False, Result));
end;

procedure TCloneDbDataset.SetReadOnly(Value: Boolean);
begin
  CheckInactive;
  FReadOnly := Value;
end;

{ TCloneTable }

procedure TCloneTable.InitFromTable(SourceTable: TTable; Reset: Boolean);
begin
  with SourceTable do
  begin
    Self.TableType := TableType;
    Self.TableName := TableName;
    Self.SessionName := SessionName;
    Self.DatabaseName := DatabaseName;
    if not Reset then
    begin
      if IndexName <> '' then
        Self.IndexName := IndexName
      else
        if IndexFieldNames <> '' then
          Self.IndexFieldNames := IndexFieldNames;
    end;
    SetSourceHandle(Handle);
    Self.Filter := Filter;
    Self.OnFilterRecord := OnFilterRecord;
    if not Reset then
      Self.Filtered := Filtered;
  end;
  if Reset then
  begin
    Filtered := False;
    DbiResetRange(Handle);
    IndexName := '';
    IndexFieldNames := '';
    First;
  end;
end;

procedure TCloneTable.SetSourceHandle(ASourceHandle: HDBICur);
begin
  if ASourceHandle <> FSourceHandle then
  begin
    Close;
    FSourceHandle := ASourceHandle;
    if FSourceHandle <> nil then
      Open;
  end;
end;

procedure TCloneTable.SetReadOnly(Value: Boolean);
begin
  CheckInactive;
  FReadOnly := Value;
end;

function TCloneTable.CreateHandle: HDBICur;
begin
  Check(DbiCloneCursor(FSourceHandle, FReadOnly, False, Result));
end;

{ TDBLocate }

function CreateDbLocate: TLocateObject;
begin
  Result := TDBLocate.Create;
end;

destructor TDBLocate.Destroy;
begin
  inherited Destroy;
end;

procedure TDBLocate.CheckFieldType(Field: TField);
var
  Locale: TLocale;
begin
  if not (Field.DataType in [ftDate, ftTime, ftDateTime]) then
  begin
    if DataSet is TBDEDataSet then
      Locale := TBDEDataSet(DataSet).Locale
    else
      Locale := Session.Locale;
    ConvertStringToLogicType(Locale, FieldLogicMap(Field.DataType),
      Field.DataSize, AnsiString(Field.FieldName), AnsiString(LookupValue), nil);
  end;
end;

function TDBLocate.UseKey: Boolean;
var
  I: Integer;
begin
  Result := False;
  if DataSet is TTable then
    with DataSet as TTable do
    begin
      if (not Self.LookupField.IsIndexField) and (not IndexSwitch or
        (not CaseSensitive and Database.IsSQLBased)) then
        Exit;
      if (not LookupExact) and (Self.LookupField.DataType <> ftString) then
        Exit;
      IndexDefs.Update;
      for I := 0 to IndexDefs.Count - 1 do
        with IndexDefs[I] do
          if not (ixExpression in Options) and
            ((ixCaseInsensitive in Options) or CaseSensitive) then
            if AnsiCompareText(Fields, Self.LookupField.FieldName) = 0 then
            begin
              Result := True;
              Exit;
            end;
    end;
end;

function TDBLocate.LocateKey: Boolean;
var
  Clone: TCloneTable;

  function LocateIndex(Table: TTable): Boolean;
  begin
    with Table do
    begin
      SetKey;
      FieldByName(Self.LookupField.FieldName).AsString := LookupValue;
      if LookupExact then
        Result := GotoKey
      else
      begin
        GotoNearest;
        Result := MatchesLookup(FieldByName(Self.LookupField.FieldName));
      end;
    end;
  end;

begin
  try
    TTable(DataSet).CheckBrowseMode;
    if TTable(DataSet).IndexFieldNames = LookupField.FieldName then
      Result := LocateIndex(TTable(DataSet))
    else
    begin
      Clone := TCloneTable.Create(DataSet);
      with Clone do
      try
        ReadOnly := True;
        InitFromTable(TTable(DataSet), True);
        IndexFieldNames := Self.LookupField.FieldName;
        Result := LocateIndex(Clone);
        if Result then
        begin
          Check(DbiSetToCursor(TTable(DataSet).Handle, Handle));
          DataSet.Resync([rmExact, rmCenter]);
        end;
      finally
        Free;
      end;
    end;
  except
    Result := False;
  end;
end;

function TDBLocate.FilterApplicable: Boolean;
begin
  Result := IsFilterApplicable(DataSet);
end;

function TDBLocate.LocateCallback: Boolean;
var
  Clone: TCloneDbDataset;
begin
  Result := False;
  try
    TBDEDataSet(DataSet).CheckBrowseMode;
    Clone := TCloneDbDataset.Create(DataSet);
    with Clone do
    try
      ReadOnly := True;
      InitFromDataset(TDBDataSet(DataSet), True);
      OnFilterRecord := RecordFilter;
      Filtered := True;
      if not (BOF and EOF) then
      begin
        First;
        Result := True;
      end;
      if Result then
      begin
        Check(DbiSetToCursor(TBDEDataSet(DataSet).Handle, Handle));
        DataSet.Resync([rmExact, rmCenter]);
      end;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

procedure TDBLocate.RecordFilter(DataSet: TDataSet; var Accept: Boolean);
begin
  Accept := MatchesLookup(DataSet.FieldByName(LookupField.FieldName));
end;

function TDBLocate.LocateFilter: Boolean;
var
  SaveCursor: TCursor;
begin
  if LookupExact or (LookupField.DataType = ftString) or
    not (DataSet is TDBDataSet) then
    Result := inherited LocateFilter
  else
  begin
    SaveCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      Result := LocateCallback;
    finally
      Screen.Cursor := SaveCursor;
    end;
  end;
end;

{ DataSet locate routines }

function IsFilterApplicable(DataSet: TDataSet): Boolean;
var
  Status: DBIResult;
  Filter: hDBIFilter;
begin
  if DataSet is TBDEDataSet then
  begin
    Status := DbiAddFilter(TBDEDataSet(DataSet).Handle, 0, 0, False, nil, nil, Filter);
    Result := (Status = DBIERR_NONE) or (Status = DBIERR_INVALIDFILTER);
    if Result then
      DbiDropFilter(TBDEDataSet(DataSet).Handle, Filter);
  end
  else
    Result := True;
end;

function DataSetFindValue(ADataSet: TBDEDataSet;
  const Value, FieldName: string): Boolean;
begin
  with TDBLocate.Create do
  try
    DataSet := ADataSet;
    if ADataSet is TDBDataSet then
      IndexSwitch := not TDBDataSet(DataSet).Database.IsSQLBased;
    Result := Locate(FieldName, Value, True, False);
  finally
    Free;
  end;
end;

function DataSetFindLike(ADataSet: TBDEDataSet; const Value,
  FieldName: string): Boolean;
begin
  with TDBLocate.Create do
  try
    DataSet := ADataSet;
    if ADataSet is TDBDataSet then
      IndexSwitch := not TDBDataSet(DataSet).Database.IsSQLBased;
    Result := Locate(FieldName, Value, False, False);
  finally
    Free;
  end;
end;

const
  SaveIndexFieldNames: TStrings = nil;

procedure UsesSaveIndexies;
begin
  if SaveIndexFieldNames = nil then
    SaveIndexFieldNames := TStringList.Create;
end;

procedure ReleaseSaveIndexies; far;
begin
  if SaveIndexFieldNames <> nil then
  begin
    SaveIndexFieldNames.Free;
    SaveIndexFieldNames := nil;
  end;
end;

procedure SetIndex(Table: TTable; const IndexFieldNames: string);
var
  IndexToSave: string;
begin
  IndexToSave := Table.IndexFieldNames;
  Table.IndexFieldNames := IndexFieldNames;
  UsesSaveIndexies;
  SaveIndexFieldNames.AddObject(IndexToSave, Table.MasterSource);
end;

procedure RestoreIndex(Table: TTable);
begin
  if (SaveIndexFieldNames <> nil) and (SaveIndexFieldNames.Count > 0) then
  begin
    try
      Table.IndexFieldNames := SaveIndexFieldNames[SaveIndexFieldNames.Count - 1];
      Table.MasterSource :=
        TDataSource(SaveIndexFieldNames.Objects[SaveIndexFieldNames.Count - 1]);
    finally
      SaveIndexFieldNames.Delete(SaveIndexFieldNames.Count - 1);
      if SaveIndexFieldNames.Count = 0 then
        ReleaseSaveIndexies;
    end;
  end;
end;

procedure DeleteRange(Table: TTable; IndexFields: array of const;
  FieldValues: array of const);
var
  I: Integer;
  NewIndex: string;
begin
  NewIndex := '';
  for I := Low(IndexFields) to High(IndexFields) do
  begin
    NewIndex := NewIndex + string(TVarRec(IndexFields[I]).VString^);
    if I <> High(IndexFields) then
      NewIndex := NewIndex + ';';
  end;
  SetIndex(Table, NewIndex);
  try
    Table.SetRange(FieldValues, FieldValues);
    try
      while not Table.EOF do
        Table.Delete;
    finally
      Table.CancelRange;
    end;
  finally
    RestoreIndex(Table);
  end;
end;

procedure ReindexTable(Table: TTable);
var
  WasActive: Boolean;
  WasExclusive: Boolean;
begin
  with Table do
  begin
    WasActive := Active;
    WasExclusive := Exclusive;
    DisableControls;
    try
      if not (WasActive and WasExclusive) then
        Close;
      try
        Exclusive := True;
        Open;
        Check(dbiRegenIndexes(Handle));
      finally
        if not (WasActive and WasExclusive) then
        begin
          Close;
          Exclusive := WasExclusive;
          Active := WasActive;
        end;
      end;
    finally
      EnableControls;
    end;
  end;
end;

procedure PackTable(Table: TTable);
{ This routine copied and modified from demo unit TableEnh.pas
  from Borland Int. }
var
  { FCurProp holds information about the structure of the table }
  FCurProp: CurProps;
  { Specific information about the table structure, indexes, etc. }
  TblDesc: CRTblDesc;
  { Uses as a handle to the database }
  hDb: hDbiDB;
  { Path to the currently opened table }
  TablePath: array[0..dbiMaxPathLen] of AnsiChar;
  Exclusive: Boolean;
begin
  if not Table.Active then
    _DBError(SDataSetClosed);
  Check(DbiGetCursorProps(Table.Handle, FCurProp));
  if StrComp(FCurProp.szTableType, szParadox) = 0 then
  begin
    { Call DbiDoRestructure procedure if PARADOX table }
    hDb := nil;
    { Initialize the table descriptor }
    FillChar(TblDesc, SizeOf(CRTblDesc), 0);
    with TblDesc do
    begin
      { Place the table name in descriptor }
      StrPCopy(szTblName, AnsiString(Table.TableName));
      { Place the table type in descriptor }
      StrCopy(szTblType, FCurProp.szTableType);
      bPack := True;
      bProtected := FCurProp.bProtected;
    end;
    { Get the current table's directory. This is why the table MUST be
      opened until now }
    Check(DbiGetDirectory(Table.DBHandle, False, TablePath));
    { Close the table }
    Table.Close;
    try
      { NOW: since the DbiDoRestructure call needs a valid DB handle BUT the
        table cannot be opened, call DbiOpenDatabase to get a valid handle.
        Setting TTable.Active = False does not give you a valid handle }
      Check(DbiOpenDatabase(nil, szCFGDBSTANDARD, dbiReadWrite, dbiOpenExcl, nil,
        0, nil, nil, hDb));
      { Set the table's directory to the old directory }
      Check(DbiSetDirectory(hDb, TablePath));
      { Pack the PARADOX table }
      Check(DbiDoRestructure(hDb, 1, @TblDesc, nil, nil, nil, False));
      { Close the temporary database handle }
      Check(DbiCloseDatabase(hDb));
    finally
      { Re-Open the table }
      Table.Open;
    end;
  end
  else
  if StrComp(FCurProp.szTableType, szDBase) = 0 then
  begin
    { Call DbiPackTable procedure if dBase table }
    Exclusive := Table.Exclusive;
    Table.Close;
    try
      Table.Exclusive := True;
      Table.Open;
      try
        Check(DbiPackTable(Table.DBHandle, Table.Handle, nil, nil, True));
      finally
        Table.Close;
      end;
    finally
      Table.Exclusive := Exclusive;
      Table.Open;
    end;
  end
  else DbiError(DBIERR_WRONGDRVTYPE);
end;

procedure FetchAllRecords(DataSet: TBDEDataSet);
begin
  with DataSet do
    if not EOF then
    begin
      CheckBrowseMode;
      Check(DbiSetToEnd(Handle));
      Check(DbiGetPriorRecord(Handle, dbiNoLock, nil, nil));
      CursorPosChanged;
      UpdateCursorPos;
    end;
end;

procedure BdeFlushBuffers;
var
  I, L: Integer;
  Session: TSession;
  J: Integer;
begin
  for J := 0 to Sessions.Count - 1 do
  begin
    Session := Sessions[J];
    if not Session.Active then
      Continue;
    for I := 0 to Session.DatabaseCount - 1 do
      with Session.Databases[I] do
        if Connected and not IsSQLBased then
          for L := 0 to DataSetCount - 1 do
            if DataSets[L].Active then
              DbiSaveChanges(DataSets[L].Handle);
  end;
end;

function DataSetRecordCount(DataSet: TDataSet): Longint;
var
  IsCount: Boolean;
begin
{$IFDEF RX_D3}
  if DataSet is TBDEDataSet then
  begin
{$ENDIF}
    IsCount := (DbiGetExactRecordCount(TBDEDataSet(DataSet).Handle,
      Result) = DBIERR_NONE) or (DbiGetRecordCount(TBDEDataSet(DataSet).Handle,
      Result) = DBIERR_NONE);
{$IFDEF RX_D3}
  end
  else
    try
      Result := DataSet.RecordCount;
      IsCount := True;
    except
      IsCount := False;
    end;
{$ENDIF}
  if not IsCount then
    Result := -1;
end;

function DataSetRecNo(DataSet: TDataSet): Longint;
var
  FCurProp: CURProps;
  FRecProp: RECProps;
begin
  Result := -1;
  if (DataSet <> nil) and DataSet.Active and (DataSet.State in [dsBrowse,
    dsEdit]) then
  begin
{$IFDEF RX_D3}
    if not (DataSet is TBDEDataSet) then
    begin
      Result := DataSet.RecNo;
      Exit;
    end;
{$ENDIF}
    if DbiGetCursorProps(TBDEDataSet(DataSet).Handle, FCurProp) <> DBIERR_NONE then
      Exit;
    if (StrComp(FCurProp.szTableType, szParadox) = 0) or
      (FCurProp.iSeqNums = 1) then
    begin
      DataSet.GetCurrentRecord(nil);
      if DbiGetSeqNo(TBDEDataSet(DataSet).Handle, Result) <> DBIERR_NONE then
        Result := -1;
    end
    else
    if StrComp(FCurProp.szTableType, szDBase) = 0 then
    begin
      DataSet.GetCurrentRecord(nil);
      if DbiGetRecord(TBDEDataSet(DataSet).Handle, dbiNOLOCK, nil, @FRecProp) = DBIERR_NONE then
        Result := FRecProp.iPhyRecNum;
    end;
  end;
end;

function DataSetPositionStr(DataSet: TDataSet): string;
var
  RecNo, RecCount: Longint;
begin
  try
    RecNo := DataSetRecNo(DataSet);
  except
    RecNo := -1;
  end;
  if RecNo >= 0 then
  begin
    RecCount := DataSetRecordCount(DataSet);
    if RecCount >= 0 then
      Result := Format('%d:%d', [RecNo, RecCount])
    else
      Result := IntToStr(RecNo);
  end
  else
    Result := '';
end;

function TransActive(Database: TDatabase): Boolean;
var
  Info: XInfo;
  S: hDBISes;
begin
  Result := False;
  if DbiGetCurrSession(S) <> DBIERR_NONE then
    Exit;
  Result := (Database.Handle <> nil) and
    (DbiGetTranInfo(Database.Handle, nil, @Info) = DBIERR_NONE) and
    (Info.exState = xsActive);
  DbiSetCurrSession(S);
end;

function GetBdeDirectory: string;
const
  Ident = 'DLLPATH';
var
  Ini: TRegistry;
const
  BdeKey = 'SOFTWARE\Borland\Database Engine';
begin
  Result := '';
  Ini := TRegistry.Create;
  try
    Ini.RootKey := HKEY_LOCAL_MACHINE;
    if Ini.OpenKey(BdeKey, False) and Ini.ValueExists(Ident) then
      Result := Ini.ReadString(Ident);
  { Check for multiple directories, use only the first one }
  if Pos(';', Result) > 0 then
    Delete(Result, Pos(';', Result), MaxInt);
  if (Length(Result) > 2) and (Result[Length(Result)] <> '\') then
    Result := Result + '\';
  finally
    Ini.Free;
  end;
end;

procedure ExportDataSetEx(Source: TBDEDataSet; DestTable: TTable;
  TableType: TTableType; const AsciiCharSet: string;
  AsciiDelimited: Boolean; AsciiDelimiter, AsciiSeparator: Char;
  MaxRecordCount: Longint);

  function ExportAsciiField(Field: TField): Boolean;
  begin
    Result := Field.Visible and not (Field.Calculated
      or Field.Lookup) and not (Field.DataType in
      ftNonTextTypes + [ftUnknown]);
  end;

const
  TextExt = '.TXT';
  SchemaExt = '.SCH';
var
  I: Integer;
  S, Path: string;
  BatchMove: TBatchMove;
  TablePath: array[0..dbiMaxPathLen] of AnsiChar;
begin
  if Source = nil then
    _DBError(SDataSetEmpty);
  if DestTable.Active then
    DestTable.Close;
  if Source is TDBDataSet then
    DestTable.SessionName := TDBDataSet(Source).SessionName;
  if (TableType = ttDefault) then
  begin
    if DestTable.TableType <> ttDefault then
      TableType := DestTable.TableType
    else
      if (CompareText(ExtractFileExt(DestTable.TableName), TextExt) = 0) then
        TableType := ttASCII;
  end;
  BatchMove := TBatchMove.Create(Application);
  try
    StartWait;
    try
      BatchMove.Mode := batCopy;
      BatchMove.Source := Source;
      BatchMove.Destination := DestTable;
      DestTable.TableType := TableType;
      BatchMove.Mappings.Clear;
      if (DestTable.TableType = ttASCII) then
      begin
        if CompareText(ExtractFileExt(DestTable.TableName), SchemaExt) = 0 then
          DestTable.TableName := ChangeFileExt(DestTable.TableName, TextExt);
        with Source do
          for I := 0 to FieldCount - 1 do
            if ExportAsciiField(Fields[I]) then
              BatchMove.Mappings.Add(Format('%s=%0:s', [Fields[I].FieldName]));
        BatchMove.RecordCount := 1;
      end
      else
        BatchMove.RecordCount := MaxRecordCount;
      BatchMove.Execute;
      if (DestTable.TableType = ttASCII) then
      begin
        { ASCII table always created in "fixed" format with "ascii"
          character set }
        with BatchMove do
        begin
          Mode := batAppend;
          RecordCount := MaxRecordCount;
        end;
        S := ChangeFileExt(ExtractFileName(DestTable.TableName), '');
        Path := NormalDir(ExtractFilePath(DestTable.TableName));
        if Path = '' then
        begin
          DestTable.Open;
          try
            Check(DbiGetDirectory(DestTable.DBHandle, False, TablePath));
            Path := NormalDir(OemToAnsiStr(StrPas(TablePath)));
          finally
            DestTable.Close;
          end;
        end;
        with TIniFile.Create(ChangeFileExt(Path + S, SchemaExt)) do
        try
          if AsciiCharSet <> '' then
            WriteString(S, 'CharSet', AsciiCharSet)
          else
            WriteString(S, 'CharSet', 'ascii');
          if AsciiDelimited then
          begin { change ASCII-file format to CSV }
            WriteString(S, 'Filetype', 'VARYING');
            WriteString(S, 'Delimiter', AsciiDelimiter);
            WriteString(S, 'Separator', AsciiSeparator);
          end;
        finally
          Free;
        end;
        { clear previous output - overwrite existing file }
        S := Path + ExtractFileName(DestTable.TableName);
        if Length(ExtractFileExt(S)) < 2 then
          S := ChangeFileExt(S, TextExt);
        I := FileCreate(S);
        if I < 0 then
          raise EFCreateError.CreateFmt(ResStr(SFCreateError), [S]);
        FileClose(I);
        BatchMove.Execute;
      end;
    finally
      StopWait;
    end;
  finally
    BatchMove.Free;
  end;
end;

procedure ExportDataSet(Source: TBDEDataSet; DestTable: TTable;
  TableType: TTableType; const AsciiCharSet: string;
  AsciiDelimited: Boolean; MaxRecordCount: Longint);
begin
  ExportDataSetEx(Source, DestTable, TableType, AsciiCharSet,
    AsciiDelimited, '"', ',', MaxRecordCount);
end;

procedure ImportDataSet(Source: TBDEDataSet; DestTable: TTable;
  MaxRecordCount: Longint; Mappings: TStrings; Mode: TBatchMode);
var
  BatchMove: TBatchMove;
begin
  if Source = nil then
    _DBError(SDataSetEmpty);
  if (Source is TDBDataSet) and not Source.Active then
    TDBDataSet(Source).SessionName := DestTable.SessionName;
  BatchMove := TBatchMove.Create(Application);
  try
    StartWait;
    try
      BatchMove.Mode := Mode;
      BatchMove.Source := Source;
      BatchMove.Destination := DestTable;
      if Mappings.Count > 0 then
        BatchMove.Mappings.AddStrings(Mappings);
      BatchMove.RecordCount := MaxRecordCount;
      BatchMove.Execute;
    finally
      StopWait;
    end;
  finally
    BatchMove.Free;
  end;
end;

function GetNativeHandle(Database: TDatabase; Buffer: Pointer;
  BufSize: Integer): Pointer;
var
  Len: Word;
begin
  Result := nil;
  if Assigned(Database) and Database.Connected then
  begin
    if Database.IsSQLBased then
    begin
      Check(DbiGetProp(HDBIOBJ(Database.Handle), dbNATIVEHNDL,
        Buffer, BufSize, Len));
      Result := Buffer;
    end
    else
      DBError(SLocalDatabase);
  end
  else
    _DBError(SDatabaseClosed);
end;

procedure BdeTranslate(Locale: TLocale; Source, Dest: PAnsiChar; ToOem: Boolean);
var
  Len: Cardinal;
begin
  Len := StrLen(Source);
  if ToOem then
    AnsiToNativeBuf(Locale, Source, Dest, Len)
  else
    NativeToAnsiBuf(Locale, Source, Dest, Len);
  if Source <> Dest then
    Dest[Len] := #0;
end;

function TrimMessage(Msg: PAnsiChar): PAnsiChar;
var
  Blank: Boolean;
  Source, Dest: PAnsiChar;
begin
  Source := Msg;
  Dest := Msg;
  Blank := False;
  while Source^ <> #0 do
  begin
    if Source^ <= ' ' then
      Blank := True
    else
    begin
      if Blank then
      begin
        Dest^ := ' ';
        Inc(Dest);
        Blank := False;
      end;
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  if (Dest > Msg) and ((Dest - 1)^ = '.') then
    Dec(Dest);
  Dest^ := #0;
  Result := Msg;
end;

function BdeErrorMsg(ErrorCode: DBIResult): string;
var
  I: Integer;
  NativeError: Longint;
  Msg, LastMsg: DBIMSG;
begin
  I := 1;
  DbiGetErrorString(ErrorCode, Msg);
  TrimMessage(Msg);
  if Msg[0] = #0 then
    Result := Format(ResStr(SBDEError), [ErrorCode])
  else
    Result := string(StrPas(Msg));
  while True do
  begin
    StrCopy(LastMsg, Msg);
    ErrorCode := DbiGetErrorEntry(I, NativeError, Msg);
    if (ErrorCode = DBIERR_NONE) or (ErrorCode = DBIERR_NOTINITIALIZED) then
      Break;
    TrimMessage(Msg);
    if (Msg[0] <> #0) and (StrComp(Msg, LastMsg) <> 0) then
      Result := Format('%s. %s', [Result, Msg]);
    Inc(I);
  end;
  for I := 1 to Length(Result) do
    if Result[I] < ' ' then
      Result[I] := ' ';
end;

procedure DataSetShowDeleted(DataSet: TBDEDataSet; Show: Boolean);
begin
  with DataSet do
  begin
    CheckBrowseMode;
    Check(DbiValidateProp(hDBIObj(Handle), curSOFTDELETEON, True));
    DisableControls;
    try
      Check(DbiSetProp(hDBIObj(Handle), curSOFTDELETEON, Ord(Show)));
    finally
      EnableControls;
    end;
    if DataSet is TTable then
      TTable(DataSet).Refresh
    else
    begin
      CursorPosChanged;
      First;
    end;
  end;
end;

function CurrentRecordDeleted(DataSet: TBDEDataSet): Boolean;
var
  FRecProp: RECProps;
begin
  Result := False;
  if (DataSet <> nil) and DataSet.Active then
  begin
    DataSet.GetCurrentRecord(nil);
    if DbiGetRecord(DataSet.Handle, dbiNOLOCK, nil, @FRecProp) = DBIERR_NONE then
      Result := FRecProp.bDeleteFlag;
  end;
end;

procedure DbNotSupported;
begin
  DbiError(DBIERR_NOTSUPPORTED);
end;

procedure ToggleDebugLayer(Active: Boolean; const DebugFile: string);
const
  Options: array[Boolean] of Longint = (0, DEBUGON or OUTPUTTOFILE or
    APPENDTOLOG);
var
  FileName: DBIPATH;
begin
  Check(DbiDebugLayerOptions(Options[Active], StrPLCopy(FileName,
    AnsiString(DebugFile), SizeOf(DBIPATH) - 1)));
end;

initialization
  rxDbUtils.CreateLocateObject := CreateDbLocate;

finalization
  ReleaseSaveIndexies;

end.
