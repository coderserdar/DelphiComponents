{$I fb_define.inc}

unit fbcustomdataset;

interface

uses
  SysUtils, Classes, DB, mydbunit, jvuiblib, jvuib, fbmisc, fbparams, ExtCtrls
  , jvuibase,
{$IFDEF FB_USE_LCL}
  Forms, Controls
{$ENDIF}
  ;

type
  TFieldHeader = class
    FieldName:string;
    FieldNo:integer;
    FieldType:TFieldType;
    FieldSize:Cardinal;
    FieldPrecision:integer;
    FieldOffs:Cardinal;
    FieldRequired:boolean;
    FieldOrigin:string;
  end;

  TFieldsHeader = class(TList)
  private
    function GetFieldHeader(Index: Integer): TFieldHeader;
  public
    property FieldHeader[Index: Integer]:TFieldHeader read GetFieldHeader; default;
  end;

type
  TBlobCacheStream = class;

  { ета запись используется для выборки БЛОБА из буфера записи методами Get/SetFieldData}
  {this type use for retrieve BLOB with Get/SetField methods}
  PBLOBFieldData = ^TBLOBFieldData;
  TBLOBFieldData = record
      IscQuad : TIscQuad;
      Cache   : TBlobCacheStream;
  end;

  TRecordsBuffer = class;
  TFBCustomDataSet = class;
  TFBUpdateRecordTypes = set of TCachedUpdateStatus;

  TFBDataLink = class(TDetailDataLink)
  protected
    FFBCustomDataSet: TFBCustomDataSet;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    function GetDetailDataSet: TDataSet; override;
  public
    constructor Create(AFBCustomDataSet: TFBCustomDataSet);
    destructor Destroy; override;
  end;

  TAutoUpdateOptions = class(TPersistent)
  private
    FOwner:TFBCustomDataSet;
    FUpdateTableName: string; //Updated table
    FKeyField: string;
    FWhenGetGenID: TWhenGetGenID;
    FIncrementBy: integer;
    FGeneratorName: string; //Key field
    procedure SetKeyField(const AValue: string);
    procedure SetUpdateTableName(const AValue: string);
    procedure SetWhenGetGenID(const AValue: TWhenGetGenID);
    procedure SetGeneratorName(const AValue: string);
    procedure SetIncrementBy(const AValue: integer);
  public
    constructor Create(AOwner:TFBCustomDataSet);
    destructor Destroy; override;
    procedure ApplyGetGenID;
    procedure Assign(Source: TPersistent); override;
    function IsComplete:boolean;
  published
    property KeyField:string read FKeyField write SetKeyField;
    property UpdateTableName:string read FUpdateTableName write SetUpdateTableName;
    property WhenGetGenID:TWhenGetGenID read FWhenGetGenID write SetWhenGetGenID;
    property GeneratorName:string read FGeneratorName write SetGeneratorName;
    property IncrementBy:integer read FIncrementBy write SetIncrementBy;
  end;

(*
  TFBTimeField = class(TTimeField)
  protected
    procedure SetAsString(const AValue: string); override;
  end;
*)
  TFBAnsiMemoField = class(TMemoField)
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function GetIsNull: Boolean; override;
  end;

{$IFDEF FPC}
  TFBBlobField = class(TBlobField)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure SaveToStrings(Strings: TStrings);
  end;

{$ENDIF}

  TDSBLOBCacheList = TList;
  { TFBCustomDataSet }

  TFBCustomDataSet = class(TMyDBCustomDataSet)
  private
    FDataBase:TJvUIBDataBase;
    FMasterScrollBehavior: TMasterScrollBehavior;
    FOnFetchRecord: TNotifyEvent;
    FQuerySelect:TJvUIBQuery;
    FQueryRefresh:TJvUIBQuery;
    FQueryInsert:TJvUIBQuery;
    FQueryEdit:TJvUIBQuery;
    FQueryDelete:TJvUIBQuery;
    FFiledsHeader:TFieldsHeader;
    FRecordsBuffer:TRecordsBuffer;
    FRecordCount:integer;
    FOption: TFBDsOptions;
    FCachedUpdates: Boolean;
    FAllowedUpdateKinds: TUpdateKinds;
    FAutoUpdateOptions: TAutoUpdateOptions;
    FRefreshTransactionKind: TTransactionKind;
{$IFDEF FB_USE_LCL}
    FSQLScreenCursor: TCursor;
{$ENDIF}
    FUpdateRecordTypes: TFBUpdateRecordTypes;
    //Master-detail support
    FMasterFBDataLink:TFBDataLink;
    FDetailConditions: TDetailConditions;
    FDetailWaitTimer:TTimer;
    //Macro support - based on RXQuery from rxlib
    FSaveQueryChanged: TNotifyEvent;
    FMacros: TFBParams;
    FMacroChar: Char;
    FPatternChanged: Boolean;
    FSQLPattern: TStrings;
    FStreamPatternChanged: Boolean;
    FDisconnectExpected: Boolean;
    FAutoCommit: boolean;
    FDefaultFormats: TDefaultFormats;
    FInspectRecNo:integer;
    FBFCurrentOperationState:TBFCurrentOperationState;
    procedure DoFillParams(Qu:TJvUIBQuery; OnlyMaster:boolean);
    //Macro support - based on RXQuery from rxlib
    procedure RecreateMacros;
    procedure CreateMacros(List: TFBParams; const Value: PChar);
    procedure PatternChanged(Sender: TObject);
    procedure Expand(Query: TStrings);
    procedure QueryChanged(Sender: TObject);
    //Master-detail
    procedure MasterUpdate(MasterUpdateStatus:TMasterUpdateStatus);
    //Property metods
    procedure SetTransaction(const AValue: TJvUIBTransaction);
    procedure SetDataBase(const AValue: TJvUIBDataBase);
    function GetSQLRefresh: TStrings;
    function GetSQLSelect: TStrings;
    procedure SetSQLRefresh(const AValue: TStrings);
    procedure SetSQLSelect(const AValue: TStrings);
    function GetParams: TSQLParams;
    function GetSQLDelete: TStrings;
    function GetSQLEdit: TStrings;
    procedure SetSQLDelete(const AValue: TStrings);
    procedure SetSQLEdit(const AValue: TStrings);
    procedure SetOption(const AValue: TFBDsOptions);
    procedure SetCachedUpdates(const AValue: Boolean);
    procedure SetAllowedUpdateKinds(const AValue: TUpdateKinds);
    procedure SetDetailConditions(const AValue: TDetailConditions);
    procedure SetAutoUpdateOptions(const AValue: TAutoUpdateOptions);
    function GetSQLInsert: TStrings;
    procedure SetSQLInsert(const AValue: TStrings);
    function GetMacros: TFBParams;
    procedure SetMacroChar(const AValue: Char);
    procedure SetMacros(const AValue: TFBParams);
    function GetMacroCount: Word;
    procedure SetUpdateRecordTypes(const AValue: TFBUpdateRecordTypes);
    function StoreUpdateTransaction:boolean;
    function GetTransaction: TJvUIBTransaction;
    function GetUpdateTransaction: TJvUIBTransaction;
    procedure SetUpdateTransaction(const AValue: TJvUIBTransaction);
    function CheckUpdateKind(UpdateKind:TUpdateKind):boolean;
    procedure SetAutoCommit(const AValue: boolean);
    procedure UpdateStart;
    procedure UpdateCommit;
    function IsVisible(Buffer: PChar): Boolean;
    procedure SetDefaultFormats(const AValue: TDefaultFormats);
    procedure UpdateFieldsFormat;
    procedure QuerySelectOnClose(Sender: TObject);
  protected
    FMaxMEMOStringSize : cardinal;
    {use by BLOB caches to show that record data affected by caches}
    FBLOBCache : TDSBLOBCacheList;

    // overrided metods
    procedure DoBeforeDelete; override;
    procedure DoBeforeEdit; override;
    procedure DoBeforeInsert; override;

    procedure InternalInitFieldDefs; override;
    procedure InternalClose; override;
    procedure InternalOpen; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode;
        DoCheck: Boolean): TGetResult; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure SetFieldData2Record(Field: TField; Buffer: Pointer; Rec : PRecordBuffer);virtual;
    procedure SetBLOBCache(Field: TField; Buffer: PBLOBFieldData); virtual;
    function InternalRecordCount: Integer; override;
    procedure InternalAfterOpen; override;
    procedure InternalEdit; override;
    procedure InternalLast; override;
    procedure InternalRefresh; override;
    procedure InternalRefreshRow(UIBQuery:TJvUIBQuery);
    procedure InternalPost; override;
    procedure InternalDelete; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean);override;
    procedure SetFiltered(Value: Boolean); override;
    function GetDataSource: TDataSource; override;
    procedure DoOnNewRecord; override;
    procedure Loaded; override;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function IsEmptyEx: Boolean;override;
    // internal metods
    function BookmarkValid(ABookmark: TBookmark): Boolean; override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure InternalSaveRecord(Q:TJvUIBQuery; FBuff: PChar);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetFieldsFromParams;
    procedure ForceMasterRefresh;
    function GetAnyRecField(SrcRecNo:integer; AField:TField):variant;
    //
    property AllowedUpdateKinds:TUpdateKinds read FAllowedUpdateKinds write SetAllowedUpdateKinds default [ukModify, ukInsert, ukDelete];
    property AutoCommit:boolean read FAutoCommit write SetAutoCommit default False;
    property DataBase:TJvUIBDataBase read FDataBase write SetDataBase;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DefaultFormats:TDefaultFormats read FDefaultFormats write SetDefaultFormats;
    property DetailConditions:TDetailConditions read FDetailConditions write SetDetailConditions;
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates default False;
    property Transaction:TJvUIBTransaction read GetTransaction write SetTransaction;
    property SQLSelect:TStrings read GetSQLSelect write SetSQLSelect;
    property SQLRefresh:TStrings read GetSQLRefresh write SetSQLRefresh;
    property SQLEdit:TStrings read GetSQLEdit write SetSQLEdit;
    property SQLDelete:TStrings read GetSQLDelete write SetSQLDelete;
    property SQLInsert:TStrings read GetSQLInsert write SetSQLInsert;
    property QuerySelect:TJvUIBQuery read FQuerySelect;
    property QueryRefresh:TJvUIBQuery read FQueryRefresh;
    property QueryInsert:TJvUIBQuery read FQueryInsert;
    property QueryEdit:TJvUIBQuery read FQueryEdit;
    property QueryDelete:TJvUIBQuery read FQueryDelete;
    property Params: TSQLParams read GetParams;
    property Option:TFBDsOptions read FOption write SetOption;
    property AutoUpdateOptions:TAutoUpdateOptions read FAutoUpdateOptions write SetAutoUpdateOptions;
    property MacroChar: Char read FMacroChar write SetMacroChar default DefaultMacroChar;
    property Macros: TFBParams read GetMacros write SetMacros;
    property MasterScrollBehavior:TMasterScrollBehavior read FMasterScrollBehavior write FMasterScrollBehavior default msbCancel;
    property UpdateTransaction:TJvUIBTransaction read GetUpdateTransaction write SetUpdateTransaction {$IFNDEF FPC} stored StoreUpdateTransaction {$ENDIF FPC};
    property OnFetchRecord:TNotifyEvent read FOnFetchRecord write FOnFetchRecord;
    property UpdateRecordTypes: TFBUpdateRecordTypes read FUpdateRecordTypes
                                                      write SetUpdateRecordTypes;
    //create the valid cache cell 4 required mode if possible, true if cell is exist
    //создает ячейку кеша блоба по заданому режиму доступа, true - если кеш существует,
    // результат в BLOBRec
    function BlobCacheMaintain(Field: TField; Mode: TBlobStreamMode;
                            var BLOBRec : TBLOBFieldData
                          ) : boolean;
    function GetMemo(Field: TField) : AnsiString;
    procedure SetMemo(Field: TField; Value: AnsiString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    procedure ApplyUpdates;
    function  CachedUpdateStatus: TCachedUpdateStatus;
    procedure CancelUpdates;
    procedure CloseOpen(AFetchAll:boolean = false);
    procedure FetchAll;
    procedure FetchNext(FCount:integer);
    procedure SortOnField(FieldName:string; Asc:boolean);
    procedure SortOnFields(FieldNames:string; Asc: array of boolean);
    procedure ExpandMacros;
    function MacroByName(const AValue: string): TFBParam;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean;override;
    property MacroCount: Word read GetMacroCount;
    property FetchedRecordCount:integer read FRecordCount;
    //
    procedure CloneRecord(SrcRecord: integer; IgnoreFields: array of const);
    procedure CloneCurRecord(IgnoreFields: array of const);

    property MemoValue[Field: TField] : AnsiString read GetMemo write SetMemo;
    property RefreshTransactionKind:TTransactionKind read FRefreshTransactionKind write FRefreshTransactionKind;
{$IFDEF FB_USE_LCL}
    property SQLScreenCursor  :TCursor read FSQLScreenCursor write FSQLScreenCursor default crDefault;
{$ENDIF}
  published
    {if Memo size less than this value - it stores in memoty in native way - as string}
    {если размер Мемо меньше заданного значения - оно храница в виде строки иначе только в потоке}
    property MaxMEMOStringSize : cardinal read FMaxMEMOStringSize write FMaxMEMOStringSize default 16384;
  end;

  TRecBuf = class
  private
    FBufSize  : cardinal;
    FCurrent  : PChar;
    FOriginal : PChar;
    function GetOriginal: PChar;
  public
    constructor Create(ABufSize:cardinal);
    destructor Destroy; override;
    procedure Modify;
    property Current : PChar read FCurrent;
    property Original: PChar read GetOriginal;
  end;

  { TRecordsBuffer }

  TRecordsBuffer = class(TList)
  private
    FOwner:TFBCustomDataSet;
    function CompareField(Item1, Item2:PRecordBuffer; FieldNo:integer; Asc:Boolean):integer;
  public
    constructor Create(AOwner:TFBCustomDataSet);
    destructor Destroy; override;
    procedure ReadRecordFromQuery(RecNo:integer; Sourse:TJvUIBQuery);
    procedure RefreshRecordFromQuery(RecNo:integer; Sourse:TJvUIBQuery);
    procedure SaveToBuffer(RecNo:integer; Buffer:PChar);  //Ситаем из колекции в бувер датасета
    procedure SaveFromBuffer(RecNo:integer; Buffer:PChar); //Запомним в колекции - Post
    procedure SaveFromBufferI(RecNo:integer; Buffer:PChar); //Запомним в колекции - Insert
    procedure SaveFromBufferA(RecNo:integer; Buffer:PChar); //Запомним в колекции - Append
    procedure Clear;override;
    procedure SortOnField(FieldNo:integer; Asc:boolean);
    procedure SortOnFields(const SortArray:TFBInternalSortArray;const CountEl:integer);
    procedure DeleteRecord(RecNo:integer);
    procedure EditRecord(RecNo:integer; NewBuffer : PRecordBuffer);
  end;

  TFBDataSet = class(TFBCustomDataSet)
  public
    property Params;
    property QuerySelect;
    property QueryRefresh;
    property QueryInsert;
    property QueryEdit;
    property QueryDelete;
  published
    property AfterRefresh;
    property BeforeRefresh;
    property AllowedUpdateKinds;
    property AutoCommit;
    property AutoUpdateOptions;
    property DataSource;
    property DefaultFormats;
    property DetailConditions;
    property Filtered;
    property CachedUpdates;
    property DataBase;
    property Description;
    property MacroChar;
    property Macros;
    property MasterScrollBehavior;
    property Option;
    property RefreshTransactionKind;
    property Transaction;
    property UpdateTransaction;
    property UpdateRecordTypes;
    property SQLSelect;
    property SQLRefresh;
    property SQLEdit;
    property SQLDelete;
    property SQLInsert;
{$IFDEF FB_USE_LCL}
    property SQLScreenCursor;
{$ENDIF}
    //Events
    property OnUpdateRecord;
    property OnUpdateError;
    property OnFetchRecord;
    property MaxMEMOStringSize;
  end;
  
  TCacheState = (csNotReady, csFresh, csModified, csLoadBLOB, csStoreBLOB);

  TBlobCacheStream = class(TBLOBCache)
  protected
    FState : tCacheState;
    procedure DoWriteBlob;virtual;abstract;
    procedure DoReadBlob;virtual;abstract;
    procedure EnModifyValue;virtual;

    function GetSize: Int64; override;

    procedure SetText(const Src : AnsiString);virtual;
    function GetText : AnsiString;virtual;
  public
    DS       : TFBCustomDataSet;
    Isc      : TIscQuad;
    constructor Create(aDS : TFBCustomDataSet; aOriginISC : TIscQuad);
    destructor Destroy;override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SetSize(NewSize: Longint); override;
    function Modified : boolean;
    function New   : TBlobCacheStream;virtual;abstract;
    function Clone : TBlobCacheStream;virtual;
     { отсылает содержимое на сервер, если оно изменено, обновляет Isc}
    procedure Flush;
    { загружает содержимо БЛОБА с сервера}
    procedure Refresh;
    { делает кеш устаревшим}
    procedure OutDate;
    { ускоряет создание кеша - если текущий кеш неразделяем то переинициирует себя
      и себя же  возвращает, иначе создает новый кеш его и возвращает }
    function Change(const aIsc : tISCQuad) : TBLOBCache;
    {тоже только если текущий кеш разделеяем - то новый не создает а возвращает нил}
    function ChangeOrNil(const aIsc : tISCQuad) : TBLOBCache;

    property AnsiText : AnsiString read GetText write SetText;
  end;
  

  TFBBlobStream = class(TBlobCacheStream) {TBLOBCache}
  protected
    procedure DoWriteBlob;override;
    procedure DoReadBlob;override;
  public
    function New : TBlobCacheStream;override;
  end;

  TFBAnsiMemoStream = class(TFBBlobStream)
  protected
    FMemoText : AnsiString;
    FTextShared : boolean;
    {флаг устанавливается после чтения текста и сбрасывается после дублирования содержимого}

    procedure EnModifyValue;override;
    procedure SetText(const Src : AnsiString);override;
    function GetText : AnsiString;override;
    function Realloc(var NewCapacity: Longint): Pointer; override;
  public
    constructor Create(aDS : TFBCustomDataSet; aOriginISC : TIscQuad);
    destructor Destroy;override;
    function New : TBlobCacheStream;override;
    function Clone : TBlobCacheStream;override;
  end;

  TLostBLOBCacheEvent = procedure(const DSname : string; var List : TDSBLOBCacheList);

var
  DefFBDsOptions : TFBDsOptions = [poTrimCharFields, poRefreshAfterPost];
  OnLostBLOBCache : tLostBLOBCacheEvent  = nil;
  
implementation
  
uses Math,
{$IFDEF FPC}
  dbconst
{$ELSE}
  dbconsts
{$ENDIF}
  , sysConst
  ;

{$include FBRecord.inc}

type

  PMEMOString = AnsiString;
  { ета запись используется для хранения БЛОБА в буфере записи}
  { this record use for store BLOB in RecordBuffer}
  PBLOBRecordData = ^TBLOBRecordData;
  TBLOBRecordData = record
      IscQuad : TIscQuad;
      ListIdx : cardinal;
  end;

{*********************************************************************
                       TBlobWrapStream
this stream used to access to BLOB cache
етот поток используется как посредник с кешем БЛОБа
*********************************************************************}
  TBlobWrapStream = class(TStream)
  protected
    FField: TField;
    FBlobStream: TBlobCacheStream;
  protected
    function GetSize: Int64; override;
  public
    Mode: TBlobStreamMode;
    constructor Create(AField: TField; ABlobStream: TBlobCacheStream;
                      aMode: TBlobStreamMode);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

constructor TBlobWrapStream.Create(AField: TField; ABlobStream: TBlobCacheStream;
                                    aMode: TBlobStreamMode);
begin
  FField := AField;
  FBlobStream := ABlobStream;
  Mode := aMode;
  if (Mode = bmWrite) then
    FBlobStream.SetSize(0)
  else
    FBlobStream.Position := 0;
end;

function TBlobWrapStream.Read(var Buffer; Count: Longint): Longint;
begin
  result := FBlobStream.Read(Buffer, Count);
end;

function TBlobWrapStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  result := FBlobStream.Seek(Offset, Origin);
end;

function TBlobWrapStream.GetSize: Int64;
begin
  result := FBlobStream.Size;
end;

procedure TBlobWrapStream.SetSize(NewSize: Longint);
begin
  if not (Mode in [bmWrite, bmReadWrite]) then
    FBErrorStr(fbeBlobCannotBeWritten);
  FBlobStream.SetSize(NewSize);
{  TFBCustomDataSet(FField.DataSet).DataEvent(deFieldChange, Longint(FField));}
end;

function TBlobWrapStream.Write(const Buffer; Count: Longint): Longint;
begin
  if not (Mode in [bmWrite, bmReadWrite]) then
    FBErrorStr(fbeBlobCannotBeWritten);
  result := FBlobStream.Write(Buffer, Count);
  {TFBDataSet(FField.DataSet).RecordModified(True);}
{ TFBCustomDataSet(FField.DataSet).DataEvent(deFieldChange, Longint(FField));}
end;

{*********************************************************************
this stream used to wrap access to null BLOB
етот поток используется как пустой БЛОБ, использую для избежания
лишней работы по управлению кешем для пустых полей
*********************************************************************}
type
  TNullBlobWrapStream = class(TStream)
    protected
      function GetSize: Int64; override;
    public
      function Read(var Buffer; Count: Longint): Longint; override;
      function Seek(Offset: Longint; Origin: Word): Longint; override;
      procedure SetSize(NewSize: Longint); override;
      function Write(const Buffer; Count: Longint): Longint; override;
  end;

function TNullBLOBWrapStream.GetSize: Int64;
begin
  result := 0;
end;

function TNullBLOBWrapStream.Read(var Buffer; Count: Longint): Longint;
begin
  result := 0;
end;

function TNullBLOBWrapStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  result := 0;
end;

procedure TNullBLOBWrapStream.SetSize(NewSize: Longint);
begin
  FBErrorStr(fbeBlobCannotBeWritten);
end;

function TNullBLOBWrapStream.Write(const Buffer; Count: Longint): Longint;
begin
  FBErrorStr(fbeBlobCannotBeWritten);
  Write := 0;
end;

const
  parPrefixNew = 'NEW_';
  parPrefixOLD = 'OLD_';

{ TFieldsHeader }
  
function TFieldsHeader.GetFieldHeader(Index: Integer): TFieldHeader;
begin
  Result:=TFieldHeader(Items[Index])
end;

{ TFBCustomDataSet }

procedure TFBCustomDataSet.ApplyUpdates;
var
  CurBookmark: string;
  FUpdateAction: TUpdateAction;
  UpdateKind: TUpdateKind;
  bRecordsSkipped: Boolean;
  i, rc: integer;
  RecBuff:PMyDBInfo;
  CurUpdateTypes:TFBUpdateRecordTypes;
begin
  if not FCachedUpdates then
    FBError(fbeNotCachedUpdates, [Name]);
  if State in [dsEdit, dsInsert] then Post;
  if FRecordCount = 0 then Exit;

  DisableControls;
  CurBookmark := Bookmark;
  CurUpdateTypes := FUpdateRecordTypes;
  FUpdateRecordTypes := [cusModified, cusInserted, cusDeleted];
  try
    UpdateStart;
    First;
    bRecordsSkipped := False or Eof;
    i := 1;
    rc := FRecordCount;
    while (i <= rc) and not Eof do
    begin
      Inc(i);
      RecBuff:=@(PRecordBuffer(GetActiveBuf)^);
      case RecBuff^.CachedUpdateStatus of
        cusModified: UpdateKind := ukModify;
        cusInserted: UpdateKind := ukInsert;
      else
        UpdateKind := ukDelete;
      end;

      //if assigned manual updater - try it
      if (Assigned(FOnUpdateRecord)) then
      begin
        FUpdateAction := uaFail;
        FOnUpdateRecord(Self, UpdateKind, FUpdateAction);
      end
      else
        FUpdateAction := uaRetry;

      case FUpdateAction of
        uaFail: FBError(fbeUserAbort, []);
        uaAbort: SysUtils.Abort;
        uaApplied:
          begin
            RecBuff^.CachedUpdateStatus := cusUnmodified;
            FRecordsBuffer.SaveFromBuffer(RecBuff^.Bookmark, GetActiveBuf);
          end;
        uaSkip:
          bRecordsSkipped := True;
      end;

      while (FUpdateAction in [uaRetry]) do
      begin
        try
          case RecBuff^.CachedUpdateStatus  of
            cusModified: InternalSaveRecord(QueryEdit, GetActiveBuf);
            cusInserted: InternalSaveRecord(QueryInsert, GetActiveBuf);
            cusDeleted: InternalSaveRecord(QueryDelete, GetActiveBuf);
          end;
          FUpdateAction := uaApplied;
        except
          on E: EFBError do
          begin
            FUpdateAction := uaFail;
            if Assigned(FOnUpdateError) then
              FOnUpdateError(Self, E, UpdateKind, FUpdateAction);
            case FUpdateAction of
              uaFail: raise;
              uaAbort: raise EAbort(E.Message);
              uaSkip: bRecordsSkipped := True;
            end;
          end;
        end;
      end;
      Next;
    end;
    if not bRecordsSkipped then  UpdateCommit;
  finally
    FUpdateRecordTypes := CurUpdateTypes;
    Bookmark := CurBookmark;
    EnableControls;
  end;
end;

function TFBCustomDataSet.CachedUpdateStatus: TCachedUpdateStatus;
begin
  Result:=PMyDBInfo(Pointer(Cardinal(ActiveBuffer)+FRecordSize))^.CachedUpdateStatus;
end;

procedure TFBCustomDataSet.CancelUpdates;
begin

end;

function TFBCustomDataSet.CheckUpdateKind(
  UpdateKind: TUpdateKind): boolean;
begin
  case UpdateKind of
    ukModify:Result:=QueryEdit.SQL.Text<>'';
    ukInsert:Result:=QueryInsert.SQL.Text<>'';
    ukDelete:Result:=QueryDelete.SQL.Text<>'';
  else
    Result:=false;
  end;
  if Result then
    Result:=UpdateKind in FAllowedUpdateKinds;
end;

procedure TFBCustomDataSet.CloseOpen(AFetchAll: boolean);
{$IFDEF FB_USE_LCL}
var
  tmpCursor: Integer;
{$ENDIF}
begin
{$IFDEF FB_USE_LCL}
  if FSQLScreenCursor <> crDefault then
  begin
    tmpCursor := Screen.Cursor;
    Screen.Cursor := FSQLScreenCursor;
  end;
{$ENDIF}

  DisableControls;
  try
    if Active then
      Close;
    Open;
    if AFetchAll then
      FetchAll;
  finally
    EnableControls;
{$IFDEF FB_USE_LCL}
    if FSQLScreenCursor <> crDefault then
      Screen.Cursor := tmpCursor;
{$ENDIF}
  end;
end;

constructor TFBCustomDataSet.Create(AOwner: TComponent);

function DoCreateQuery(qName:string):TJvUIBQuery;
begin
//  Result:=TJvUIBQuery.Create(Self);
  Result:=TJvUIBQuery.Create(nil);
  Result.CachedFetch:=false;
  Result.Name:=Name+'_'+qName;
end;

begin
  inherited Create(AOwner);
  FBFCurrentOperationState:=cosNone;
  FInspectRecNo:=-1;
  FAutoUpdateOptions:=TAutoUpdateOptions.Create(Self);
  FDefaultFormats:=TDefaultFormats.Create;

{$IFDEF FB_USE_LCL}
  FSQLScreenCursor:=crDefault;
{$ENDIF}
  
  FFiledsHeader:=TFieldsHeader.Create;

  FBLOBCache := TDSBLOBCacheList.Create;

  FQuerySelect:=DoCreateQuery('QuerySelect');
  FQueryRefresh:=DoCreateQuery('QueryRefresh');
  FQueryEdit:=DoCreateQuery('QueryEdit');
  FQueryDelete:=DoCreateQuery('QueryDelete');
  FQueryInsert:=DoCreateQuery('QueryInsert');

  FRecordsBuffer:=TRecordsBuffer.Create(Self);
  FMasterFBDataLink:=TFBDataLink.Create(Self);
  //Macros suppert - based on RxQuery from rxlib
  FMacros := TFBParams.Create(Self);
  FSQLPattern := TStringList.Create;
  FMacroChar := DefaultMacroChar;
  FSaveQueryChanged := TStringList(FQuerySelect.SQL).OnChange;
  TStringList(FQuerySelect.SQL).OnChange := QueryChanged;
  TStringList(FSQLPattern).OnChange := PatternChanged;
  //
  FOption:=DefFBDsOptions;
  FMasterScrollBehavior:=msbCancel;
  FCachedUpdates:=false;
  FAllowedUpdateKinds := [ukModify, ukInsert, ukDelete];
  FUpdateRecordTypes := [cusUnmodified, cusModified, cusInserted];
  FRefreshTransactionKind:=tkDefault;
end;

procedure TFBCustomDataSet.CreateMacros(List: TFBParams; const Value: PChar);
begin
  CreateQueryParams(List, Value, True, MacroChar, ['.']);
end;

destructor TFBCustomDataSet.Destroy;
var
  tmp : TDSBlobCacheList;
  aName : string;
begin
  tmp := FBLOBCache;
  aName := Name;
  Active:=false;
  FFiledsHeader.Clear;
  FreeAndNil(FFiledsHeader);
  FreeAndNil(FRecordsBuffer);
  FreeAndNil(FMasterFBDataLink);
  FreeAndNil(FAutoUpdateOptions);
  FreeAndNil(FMacros);
  FreeAndNil(FSQLPattern);
  FreeAndNil(FDefaultFormats);
  FreeAndNil(FQuerySelect);
  FreeAndNil(FQueryRefresh);
  FreeAndNil(FQueryEdit);
  FreeAndNil(FQueryDelete);
  FreeAndNil(FQueryInsert);
  inherited Destroy;
  if tmp.Count > 0 then
    if Assigned(OnLostBLOBCache) then
       OnLostBLOBCache(aName, tmp)
    else
      raise Exception.Create('Остались потеряные кеши');
  tmp.Destroy;
end;

procedure TFBCustomDataSet.DoBeforeDelete;
begin
  if not CheckUpdateKind(ukDelete) then abort
  else inherited DoBeforeDelete;
end;

procedure TFBCustomDataSet.DoBeforeEdit;
begin
  if not CheckUpdateKind(ukModify) then abort
  else inherited DoBeforeEdit;
end;

procedure TFBCustomDataSet.DoBeforeInsert;
begin
  if not CheckUpdateKind(ukInsert) then abort
  else inherited DoBeforeInsert;
end;

procedure TFBCustomDataSet.DoFillParams(Qu: TJvUIBQuery; OnlyMaster:boolean);
var
  I:integer;
  S:string;
  F:TField;
begin
  if Trim(Qu.SQL.Text)<>'' then
  begin
    for i:=0 to Qu.Params.FieldCount-1 do
    begin
      F:=nil;
      S:=Qu.Params.FieldName[i];
      if Assigned(FMasterFBDataLink.DataSource) and Assigned(FMasterFBDataLink.DataSource.DataSet) then
        F:=FMasterFBDataLink.DataSource.DataSet.FindField(S)
      else
      if not OnlyMaster then
        F:=FindField(S);
      if F<>nil then
      begin
        if F.IsNull then
          Qu.Params.IsNull[i]:=true
        else
        case F.DataType of
          ftFloat:Qu.Params.AsDouble[i]:=F.AsFloat;
          ftString:Qu.Params.AsString[i]:=F.AsString;
          ftSmallint:Qu.Params.AsSmallint[i]:=F.AsInteger;
          ftInteger:Qu.Params.AsInteger[i]:=F.AsInteger;
          ftDateTime:Qu.Params.AsDateTime[i]:=F.AsDateTime;
//          ftDate:Qu.Params.AsDate[I]:=F.AsDate;
//          ftTime:Qu.Params.AsTime[i]:=F.Asti;
          ftBoolean:Qu.Params.AsBoolean[i]:=F.AsBoolean;
        end;
      end;
    end;
  end;
end;

procedure TFBCustomDataSet.DoOnNewRecord;
begin
  //Fill auto generation values
  if FAutoUpdateOptions.FWhenGetGenID = wgOnNewRecord then
    FAutoUpdateOptions.ApplyGetGenID;

  //Copy field values from maser table
  if poAutoParamsToFields in FOption then
    if Assigned(FMasterFBDataLink.DataSource) and Assigned(FMasterFBDataLink.DataSource.DataSet) then
      SetFieldsFromParams;
  inherited DoOnNewRecord;
end;

procedure TFBCustomDataSet.Expand(Query: TStrings);

  function ReplaceString(const S: string): string;
  var
    I, J, P, LiteralChars: Integer;
    Param: TFBParam;
    Found: Boolean;
  begin
    Result := S;
    for I := Macros.Count - 1 downto 0 do begin
      Param := Macros[I];
      if Param.Name = '' then Continue;
      repeat
        P := Pos(MacroChar + Param.Name, Result);
        Found := (P > 0) and ((Length(Result) = P + Length(Param.Name)) or
          NameDelimiter(Result[P + Length(Param.Name) + 1], ['.']));
        if Found then begin
          LiteralChars := 0;
          for J := 1 to P - 1 do
            if IsLiteral(Result[J]) then Inc(LiteralChars);
          Found := LiteralChars mod 2 = 0;
          if Found then
          begin
            Result := Copy(Result, 1, P - 1) + Param.Value + Copy(Result,
              P + Length(Param.Name) + 1, MaxInt);
          end;
        end;
      until not Found;
    end;
  end;

var
  I: Integer;
begin
  for I := 0 to FSQLPattern.Count - 1 do
    Query.Add(ReplaceString(FSQLPattern[I]));
end;

procedure TFBCustomDataSet.ExpandMacros;
var
  ExpandedSQL: TStringList;
begin
  if not FPatternChanged and not FStreamPatternChanged and
    (MacroCount = 0) then Exit;
  ExpandedSQL := TStringList.Create;
  try
    Expand(ExpandedSQL);
    FDisconnectExpected := True;
    try
      FQuerySelect.SQL := ExpandedSQL;
    finally
      FDisconnectExpected := False;
    end;
  finally
    ExpandedSQL.Free;
  end;
end;

procedure TFBCustomDataSet.FetchAll;
var
  P:TBookmark;
begin
  if QuerySelect.Eof {or not (QuerySelect.CurrentState <> qsExecute)} then exit;
  DisableControls;
  P:=GetBookmark;
  try
    Last;
  finally
    GotoBookmark(P);
    FreeBookmark(P);
    EnableControls;
  end;
end;

procedure TFBCustomDataSet.FetchNext(FCount: integer);
var
  P:TBookmark;
begin
  DisableControls;
  P:=GetBookmark;
  try
    while (not Eof) or (FCount>0) do
    begin
      Next;
      Dec(FCount);
    end;
  finally
    GotoBookmark(P);
    FreeBookmark(P);
    EnableControls;
  end;
end;

procedure TFBCustomDataSet.ForceMasterRefresh;
begin
  if Assigned(FMasterFBDataLink.DataSource) and Assigned(FMasterFBDataLink.DataSource.DataSet) then
    FMasterFBDataLink.DataSource.DataSet.Refresh;
end;

function TFBCustomDataSet.GetAnyRecField(SrcRecNo: integer; AField: TField
  ): variant;
begin
  FInspectRecNo:=SrcRecNo;
  try
    Result:=AField.Value;
  finally
    FInspectRecNo:=-1;
  end;
end;

function TFBCustomDataSet.GetDataSource: TDataSource;
begin
  if FMasterFBDataLink = nil then Result := nil
  else Result := FMasterFBDataLink.DataSource;
end;

function TFBCustomDataSet.GetFieldData(Field: TField;
  Buffer: Pointer): Boolean;
var
  FieldOffset:integer;
  RecBuf :PRecordBuffer;
  FieldDataPtr : pointer;
begin
  Result:=false;
  if FInspectRecNo > -1 then
    RecBuf:= FRecordsBuffer.Items[FInspectRecNo]
  else
    RecBuf:= PRecordBuffer(GetActiveBuf);
  if Assigned(RecBuf) then
  begin
    if {(not IsEmptyEx) and }(Field.FieldNo>0) then
    begin
      Result:=not GetRecordNulls(Self, RecBuf)^[Field.FieldNo-1];
      FieldOffset:=TFieldHeader(FFiledsHeader[Field.FieldNo-1]).FieldOffs;
      FieldDataPtr := @(RecBuf^.Data[FieldOffset]);
    end
    else
    begin
      FieldDataPtr :=@(PBytes(GetRecordCalcs(Self, RecBuf))^[Field.Offset]);
      Result := not PBoolean(FieldDataPtr)^;
      FieldDataPtr := @(PBytes(FieldDataPtr)^[SizeOf(Boolean)]);
    end;
    if Result and assigned(Buffer) then begin
      if Field.DataType in [ftBlob, ftMemo] then begin
          with PBLOBRecordData(FieldDataPtr)^ do begin
            if ListIdx = 0 then
              PBLOBFieldData(Buffer)^.Cache := nil
            else begin
              PBLOBFieldData(Buffer)^.Cache :=
                TBLOBCacheStream(RecBuf^.BlobList.Item(ListIdx));
                  {have to avoid a property use due to bugs of delphi}
            end;
            if not assigned(PBLOBFieldData(Buffer)^.Cache) then
              PBLOBFieldData(Buffer)^.IscQuad := IscQuad
            else
              PBLOBFieldData(Buffer)^.IscQuad := PBLOBFieldData(Buffer)^.Cache.Isc;
          end;{with}
      end
      else begin
          Move(FieldDataPtr^, Buffer^, Field.DataSize);
      end;
    end;{if Result}
  end;
end;{GetFieldData}

function TFBCustomDataSet.GetMacroCount: Word;
begin
  Result := FMacros.Count;
end;

procedure TFBCustomDataSet.SetUpdateRecordTypes(
  const AValue: TFBUpdateRecordTypes);
begin
  if FUpdateRecordTypes = AValue then exit;
  FUpdateRecordTypes := AValue;
  if Active then
    First;
end;

function TFBCustomDataSet.GetMacros: TFBParams;
begin
  if FStreamPatternChanged then
  begin
    FStreamPatternChanged := False;
    PatternChanged(nil);
  end;
  Result := FMacros;
end;

function TFBCustomDataSet.GetParams: TSQLParams;
begin
  Result := FQuerySelect.Params;
end;

function TFBCustomDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;

function DoGetRecord: TGetResult;
begin
  Result:=grOk;
  case GetMode of
    gmCurrent:
      begin
        if (FCurrentRecord >= 0) then
        begin
          if FCurrentRecord < FRecordCount then
            FRecordsBuffer.SaveToBuffer(FCurrentRecord, Buffer)
          else
          begin
            while (not FQuerySelect.Eof) and (FCurrentRecord >= FRecordCount) do
            begin
              FQuerySelect.Next;
              FRecordsBuffer.ReadRecordFromQuery(FCurrentRecord, FQuerySelect);
              //FetchCurrentRecordToBuffer(FQSelect, FRecordCount, Buffer);
              Inc(FRecordCount);
            end;
            FCurrentRecord := FRecordCount - 1;
            if (FCurrentRecord >= 0) then
             FRecordsBuffer.SaveToBuffer(FCurrentRecord, Buffer)
          end;
          Result := grOk;
        end
        else
          Result := grBOF;
      end;
    gmNext:
      begin
        Result := grOk;
        if FCurrentRecord = FRecordCount then Result := grEOF
        else
        if FCurrentRecord = FRecordCount - 1 then
        begin
          if (not FQuerySelect.Eof) then
          begin
            FQuerySelect.Next;
            Inc(FCurrentRecord);
          end;
          if (FQuerySelect.Eof) then Result := grEOF
          else
          begin
            FRecordsBuffer.ReadRecordFromQuery(FCurrentRecord, FQuerySelect);
            FRecordsBuffer.SaveToBuffer(FCurrentRecord, Buffer);
            Inc(FRecordCount);
            if Assigned(FOnFetchRecord) then
              FOnFetchRecord(Self);
          end;
        end
        else
        if (FCurrentRecord < FRecordCount) then
        begin
          Inc(FCurrentRecord);
          FRecordsBuffer.SaveToBuffer(FCurrentRecord, Buffer)
        end;
      end;
    gmPrior:
      begin
        if FCurrentRecord <= 0 then
        begin
          Result := grBOF;
          FCurrentRecord:=-1;
        end
        else
        begin
          dec(FCurrentRecord);
          FRecordsBuffer.SaveToBuffer(FCurrentRecord, Buffer)
        end;
      end;
  end;
end;

begin
  if Assigned(Buffer) then
  begin
    repeat
      Result:=DoGetRecord;
      if not IsVisible(Buffer) and (GetMode = gmCurrent) then
          GetMode := gmPrior;
    until IsVisible(Buffer) or (Result <> grOK);
  end
  else
    Result:=DoGetRecord;
end;

function TFBCustomDataSet.GetSQLDelete: TStrings;
begin
  Result:=FQueryDelete.SQL;
end;

function TFBCustomDataSet.GetSQLEdit: TStrings;
begin
  Result:=FQueryEdit.SQL;
end;

function TFBCustomDataSet.GetSQLInsert: TStrings;
begin
  Result:=FQueryInsert.SQL;
end;

function TFBCustomDataSet.GetSQLRefresh: TStrings;
begin
  Result:=FQueryRefresh.SQL;
end;

function TFBCustomDataSet.GetSQLSelect: TStrings;
begin
//  Result:=FQuerySelect.SQL;
  Result:=FSQLPattern;
end;

function TFBCustomDataSet.GetTransaction: TJvUIBTransaction;
begin
  Result:=QuerySelect.Transaction;
end;

function TFBCustomDataSet.GetUpdateTransaction: TJvUIBTransaction;
begin
  Result:=QueryEdit.Transaction;
end;

procedure TFBCustomDataSet.InternalAfterOpen;
begin
  ExpandMacros;
  if Assigned(FMasterFBDataLink.DataSource) and Assigned(FMasterFBDataLink.DataSource.DataSet) then
    DoFillParams(FQuerySelect, true);
  FQuerySelect.Execute;
  FQuerySelect.OnClose:=QuerySelectOnClose;
  FRecordCount:=0;
  UpdateFieldsFormat;
end;

procedure TFBCustomDataSet.InternalClose;
begin
  FQuerySelect.Close;
  FRecordsBuffer.Clear;
  FRecordCount:=0;
  inherited InternalClose;
end;

procedure TFBCustomDataSet.InternalOpen;
begin
  if (csDesigning in ComponentState) and (not DataBase.Connected) then
    exit;
  inherited InternalOpen;
  if poFetchAll in Option then
    InternalLast;
  EofCrack := InternalRecordCount;
  FCurrentRecord := BofCrack;
end;

procedure TFBCustomDataSet.InternalEdit;
var
  Buffer : PRecordBuffer;
begin
  if (not FCachedUpdates) and (poRefreshBeforeEdit in FOption) then
    InternalRefresh;
  Buffer := PRecordBuffer(GetActiveBuf);
  FRecordsBuffer.EditRecord(FCurrentRecord, Buffer);
end;

procedure TFBCustomDataSet.InternalDelete;
var
  Buf:PChar;
begin
  Buf:=GetActiveBuf;
  if CachedUpdates then
  begin
    with PRecordBuffer(Buf)^ do
      if CachedUpdateStatus = cusInserted then CachedUpdateStatus := cusUninserted
      else CachedUpdateStatus := cusDeleted;
    FRecordsBuffer.SaveFromBuffer(FCurrentRecord, Buf);
  end
  else
  begin
    UpdateStart;
    InternalSaveRecord(QueryDelete, Buf);
    FRecordsBuffer.DeleteRecord(FCurrentRecord);
    Dec(FRecordCount);
    UpdateCommit;
    if dcForceMasterRefresh in DetailConditions then
      ForceMasterRefresh;
  end;
end;

procedure TFBCustomDataSet.InternalInitFieldDefs;
var
  i:integer;
  FOfs:Cardinal;
  FieldHeader:TFieldHeader;
  Suffix: Integer;
begin
  FFiledsHeader.Clear;
  ExpandMacros;
  FieldDefs.BeginUpdate;
  FieldDefs.Clear;
  FOfs:=0;
  try
    FQuerySelect.Prepare;
    for i := 0 to FQuerySelect.Fields.FieldCount - 1 do
    begin
      FieldHeader:=TFieldHeader.Create;
      FFiledsHeader.Add(FieldHeader);
      FieldHeader.FieldName:=FQuerySelect.Fields.AliasName[i];
      if FieldDefs.IndexOf(FieldHeader.FieldName) >= 0 then
      begin
        Suffix := 0;
        repeat
          Inc(Suffix);
          FieldHeader.FieldName := Format('%s_%d', [FQuerySelect.Fields.AliasName[i], Suffix]);
        until FieldDefs.IndexOf(FieldHeader.FieldName) < 0;
      end;
      FieldHeader.FieldNo:=i;
      FieldHeader.FieldRequired:=not FQuerySelect.Fields.IsNullable[i];
      FieldHeader.FieldPrecision:=-1;
      FieldHeader.FieldOffs:=FOfs;
      FieldHeader.FieldSize:=0;
      FieldHeader.FieldOrigin:=FQuerySelect.Fields.RelName[i] + '.' + FQuerySelect.Fields.SqlName[i];
      //block from  jvuibdataset - за основу взято из jvuibdataset
      case FQuerySelect.Fields.FieldType[i] of
        uftNumeric:
          begin
            {.$IFDEF FPC}
            FieldHeader.FieldType:=ftFloat;
            FOfs:=FOfs+SizeOf(Double);
            {.$ELSE}
(*            case FQuerySelect.Fields.SQLType[i] of
              SQL_SHORT:
                begin
                  FieldHeader.FieldType:=ftBCD;
                  FieldHeader.FieldSize:=-FQuerySelect.Fields.Data.sqlvar[i].SqlScale;
                  if FieldHeader.FieldSize = 4 then FieldHeader.FieldPrecision := 5
                  else FieldHeader.FieldPrecision := 4;
                end;
              SQL_LONG:
                begin
                  FieldHeader.FieldSize := -FQuerySelect.Fields.Data.sqlvar[i].SqlScale;
                  if FieldHeader.FieldSize = 9 then FieldHeader.FieldPrecision := 10
                  else FieldHeader.FieldPrecision := 9;
                  {$IFDEF COMPILER6_UP}
                  if size > 4 then FieldHeader.FieldType:=ftFMTBcd
                  else
                  {$ENDIF}
                  FieldHeader.FieldType:=ftBCD;
                end;
              SQL_INT64,
              SQL_QUAD:
                begin
                  FieldHeader.FieldType := ftBCD;
                  FieldHeader.FieldSize := -FQuerySelect.Fields.Data.sqlvar[i].SqlScale;
                  if FieldHeader.FieldSize = 18 then FieldHeader.FieldPrecision := 19
                  else FieldHeader.FieldPrecision := 18;
                  {$IFDEF COMPILER6_UP}
                  if FieldHeader.FieldSize > 4 then FieldHeader.FieldType:=ftFMTBcd else
                  {$ENDIF}
                  FieldHeader.FieldType:=ftBCD;
                end;
              SQL_DOUBLE:FieldHeader.FieldType:=ftFloat; // possible
            else
              //raise
            end;
            {$ENDIF} *)
          end;
        uftChar,
        uftCstring,
        uftVarchar:
          begin
            FieldHeader.FieldType:=ftString;
            FieldHeader.FieldSize := FQuerySelect.Fields.SQLLen[i];
            FOfs:=FOfs+FieldHeader.FieldSize+1;
          end;
        uftSmallint:
          begin
            FieldHeader.FieldType:=ftSmallint;
            FOfs:=FOfs+SizeOf(SmallInt);
          end;
        uftInteger :
          begin
            FieldHeader.FieldType:=ftInteger;
            FOfs:=FOfs+SizeOf(Integer);
          end;
        uftFloat,
        uftDoublePrecision:
          begin
            FieldHeader.FieldType:=ftFloat;
            FOfs:=FOfs+SizeOf(Double);
          end;
        uftTimestamp:
          begin
            FieldHeader.FieldType:=ftDateTime;
            FOfs:=FOfs+SizeOf(TDateTime);
          end;
        uftBlob :
          begin
            if FQuerySelect.Fields.Data.sqlvar[i].SqlSubType = 1 then
              FieldHeader.FieldType:=ftMemo
            else
              FieldHeader.FieldType:=ftBlob;
            FieldHeader.FieldSize := SizeOf(TBLOBRecordData);
            FOfs:=FOfs+FieldHeader.FieldSize;
          end;
        uftDate :
          begin
            FieldHeader.FieldType:=ftDate;
            {$IFDEF FPC}
            FOfs:=FOfs+SizeOf(TDateTime);
            {$ELSE}
            FOfs:=FOfs+SizeOf(Integer);
            {$ENDIF}
          end;
        uftTime :
          begin
            FieldHeader.FieldType:=ftTime;
            {$IFDEF FPC}
            FOfs:=FOfs+SizeOf(TDateTime);
            {$ELSE}
            FOfs:=FOfs+SizeOf(Integer);
            {$ENDIF}
          end;
        uftInt64:
          begin
            FieldHeader.FieldType:=ftLargeint;
            FOfs:=FOfs+SizeOf(Largeint);
          end;
        {$IFDEF IB7_UP}
        uftBoolean:
          begin
            FieldHeader.FieldType:=ftBoolean;
            FOfs:=FOfs+SizeOf(Boolean);
          end;
        {$ENDIF}
      else
        FieldHeader.FieldType:=ftUnknown;
      end;
      FieldDefs.Add(FieldHeader.FieldName, FieldHeader.FieldType, FieldHeader.FieldSize, FieldHeader.FieldRequired);
      if FieldHeader.FieldPrecision<>-1 then
        FieldDefs.Items[FieldHeader.FieldNo].Precision:=FieldHeader.FieldPrecision;
      if FieldHeader.FieldType = ftString then
            FieldHeader.FieldSize := FieldHeader.FieldSize + 1;
    end;
    FRecordSize:=FOfs;
  finally
    FieldDefs.EndUpdate;
  end;
end;

procedure TFBCustomDataSet.InternalLast;
{$IFDEF FB_USE_LCL}
var
  tmpCursor: Integer;
{$ENDIF}
begin
  if (FQuerySelect.Eof) then
    FCurrentRecord := FRecordCount
  else
  begin
{$IFDEF FB_USE_LCL}
    if FSQLScreenCursor <> crDefault then
    begin
      tmpCursor := Screen.Cursor;
      Screen.Cursor := FSQLScreenCursor;
    end;
{$ENDIF}
    try
      try
        while not FQuerySelect.Eof do
        begin
          FQuerySelect.Next;
          if not FQuerySelect.Eof then
          begin
            FRecordsBuffer.ReadRecordFromQuery(FRecordCount, FQuerySelect);
            Inc(FRecordCount);
          end;
        end;
      except
      end;
      FCurrentRecord := FRecordCount;
    finally
{$IFDEF FB_USE_LCL}
    if FSQLScreenCursor <> crDefault then
      Screen.Cursor := tmpCursor;
{$ENDIF}
    end;
  end;
  if Assigned(FOnFetchRecord) then
    FOnFetchRecord(Self);
end;

procedure TFBCustomDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  if CheckUpdateKind(ukInsert) then
  begin
    if Append then
      InternalLast;
    with PRecordBuffer(ActiveBuffer)^ do
    begin
      if Append then
      begin
        Bookmark:=FRecordCount;
        FCurrentRecord:=FRecordCount;
      end
      else
        Bookmark:=FCurrentRecord;
      CachedUpdateStatus:=cusInserted;
    end;
    InternalPost;
    end
  else
    FBError(fbeCannotInsert, [Name]);
end;

procedure TFBCustomDataSet.InternalPost;

procedure DoRefresh;
begin
  if poRefreshAfterPost in Option then
    InternalRefresh;
  if dcForceMasterRefresh in DetailConditions then
    ForceMasterRefresh;
end;

begin
  CheckActive;
  FBFCurrentOperationState:=cosInPost;
  //Fill auto generation values
  if FAutoUpdateOptions.FWhenGetGenID = wgBeforePost then
    FAutoUpdateOptions.ApplyGetGenID;
  UpdateStart;

  if not FCachedUpdates then
  begin
    if State=dsEdit then
      InternalSaveRecord(QueryEdit, ActiveBuffer)
    else
      InternalSaveRecord(QueryInsert, ActiveBuffer);
  end;

  if State=dsEdit then
  begin
    with PRecordBuffer(ActiveBuffer)^ do
    if CachedUpdateStatus <> cusInserted then
      CachedUpdateStatus:=cusModified;
    FRecordsBuffer.SaveFromBuffer(FCurrentRecord, ActiveBuffer);
  end
  else
  if State=dsInsert then
  begin
    with PRecordBuffer(ActiveBuffer)^ do
      CachedUpdateStatus:=cusInserted;
    if (FCurrentRecord < FRecordCount) and (FCurrentRecord>=0) then
      FRecordsBuffer.SaveFromBufferI(FCurrentRecord, ActiveBuffer) //Запомним в колекции - Insert
    else
    begin
      FRecordsBuffer.SaveFromBufferA(FRecordCount, ActiveBuffer); //Запомним в колекции - Append
      FCurrentRecord:=FRecordCount;
    end;
    Inc(FRecordCount)
  end;
  if not FCachedUpdates then
  begin
  
    if RefreshTransactionKind in [tkDefault, tkUpdateTransaction] then
      DoRefresh;
    UpdateCommit;
    if RefreshTransactionKind in [tkReadTransaction] then
      DoRefresh;
  end;
  FBFCurrentOperationState:=cosNone;
end;

function TFBCustomDataSet.InternalRecordCount: Integer;
begin
  Result:=FRecordCount;
end;

procedure TFBCustomDataSet.InternalRefresh;
begin
  if FRecordCount>0 then
  begin
    {$IFNDEF FPC}
    DoBeforeRefresh;
    {$ENDIF FPC}
    InternalRefreshRow(FQueryRefresh);
    FRecordsBuffer.SaveToBuffer(FCurrentRecord, GetActiveBuf);
    {$IFNDEF FPC}
    DoAfterRefresh;
    {$ENDIF FPC}
  end;
end;

procedure TFBCustomDataSet.InternalRefreshRow(UIBQuery:TJvUIBQuery);
var
  i:integer;
  F:TField;
  S, S1:string;
begin
  if Trim(UIBQuery.SQL.Text)<>'' then
  begin
    for i:=0 to UIBQuery.Params.FieldCount-1 do
    begin
      S:=UIBQuery.Params.FieldName[i];
      S1:=AnsiUpperCase(Copy(S, 1, Length(parPrefixNew)));
      if (S1 = parPrefixNew) or (S1 = parPrefixOLD) then
        System.Delete(S, 1, Length(parPrefixNew));
      F:=FindField(S);
      if F<>nil then
      begin
        case F.DataType of
          ftFloat:UIBQuery.Params.AsDouble[i]:=F.AsFloat;
          ftString:UIBQuery.Params.AsString[i]:=F.AsString;
          ftSmallint:UIBQuery.Params.AsSmallint[i]:=F.AsInteger;
          ftInteger:UIBQuery.Params.AsInteger[i]:=F.AsInteger;
          ftDateTime,
          ftTime,
          ftDate:UIBQuery.Params.AsDateTime[i]:=F.AsDateTime;
          ftBoolean:UIBQuery.Params.AsBoolean[i]:=F.AsBoolean;
        end;
      end;
    end;

    if UpdateTransaction <> Transaction then
    begin
      case FRefreshTransactionKind of
        tkReadTransaction:UIBQuery.Transaction:=Transaction;
        tkUpdateTransaction:UIBQuery.Transaction:=UpdateTransaction;
      else;
        if FBFCurrentOperationState = cosInPost then
          UIBQuery.Transaction:=UpdateTransaction
        else
          UIBQuery.Transaction:=Transaction;
      end;
    end;
    
    try
      UIBQuery.Execute;
      UIBQuery.Next;
      FRecordsBuffer.RefreshRecordFromQuery(FCurrentRecord, UIBQuery);
    finally
      UIBQuery.Close;
    end;
  end;
end;

procedure TFBCustomDataSet.InternalSaveRecord(Q:TJvUIBQuery; FBuff: PChar);
var
  i, l:integer;
  F:TField;
  BLOBRec : TBLOBFieldData;
  S, S1:string;
  SourceRec : PRecordBuffer absolute FBuff;
begin
  if Trim(Q.SQL.Text)='' then
    FBError(fbeEmptySQLEdit, [Self.Name+'.'+Q.Name]);
  try
    Q.Prepare;
    for i:=0 to Q.Params.FieldCount-1 do
    begin

      S:=Q.Params.FieldName[i];
      S1:=AnsiUpperCase(Copy(S, 1, Length(parPrefixNew)));
      if (S1 = parPrefixNew) or (S1 = parPrefixOLD) then
        System.Delete(S, 1, Length(parPrefixNew));

      F:=FindField(S);
      if F<>nil then
      begin
        if F.IsNull then
          Q.Params.IsNull[i]:=true
        else
        case F.DataType of
          ftFloat:Q.Params.AsDouble[i]:=F.AsFloat;
          ftString:Q.Params.AsString[i]:=F.AsString;
          ftSmallint:Q.Params.AsSmallint[i]:=F.AsInteger;
          ftInteger:Q.Params.AsInteger[i]:=F.AsInteger;
          ftDate,
          ftDateTime, ftTime:
          begin
            Q.Params.AsDateTime[i]:=F.AsDateTime;
          end;
          ftBoolean:Q.Params.AsBoolean[i]:=F.AsBoolean;
          ftBlob,
          ftMemo:begin
                   if GetFieldData(F, @BLOBRec) then begin
                      if assigned(BLOBRec.Cache) and (BLOBRec.Cache.Modified) then begin
                        BLOBRec.Cache.Flush;
                        BLOBRec.IscQuad :=BLOBRec.Cache.Isc;
                        SetBLOBCache(F, @BLOBRec);
                      end;
                      Q.Params.AsQuad[i] := BLOBRec.IscQuad;
                   end;
                 end;
          ftLargeint:Q.Params.AsInt64[i]:=Trunc(F.AsFloat);
        end;
      end
      else
      begin
        if poFillEmptyEPFromParams in FOption then
        begin
          l:=FQuerySelect.Params.GetFieldIndex(S);
          case FQuerySelect.Params.FieldType[l] of
            uftNumeric,
            uftQuad,
            uftFloat,
            uftDoublePrecision :Q.Params.AsDouble[i]:=FQuerySelect.Params.AsDouble[l];

            uftChar,
            uftVarchar,
            uftCstring         :Q.Params.AsString[i]:=FQuerySelect.Params.AsString[l];

            uftSmallint,
            uftInteger         :Q.Params.AsInteger[i]:=FQuerySelect.Params.AsInteger[l];

            uftTimestamp       :Q.Params.AsDateTime[i]:=FQuerySelect.Params.AsDateTime[l];
            uftDate            :Q.Params.AsDate[i]:=FQuerySelect.Params.AsDate[l];
            uftTime            :Q.Params.AsTime[i]:=FQuerySelect.Params.AsTime[l];
            uftInt64           :Q.Params.AsInt64[i]:=FQuerySelect.Params.AsInt64[l];
          end;
        end
      end;
    end;
    Q.Execute;
    ReleaseOldBuffer(SourceRec^);
  except
    on E:Exception do
      FBError(fbeErrorExecuteQ, [Q.Name, E.Message]);
  end;
  Q.Close;
end;

function TFBCustomDataSet.IsVisible(Buffer: PChar): Boolean;
var
  SaveState: TDataSetState;
begin
  if not (State = dsOldValue) then
  begin
    Result:=true;
    if Filtered and Assigned(OnFilterRecord) then
    begin
      SaveState := SetTempState(dsFilter);
      FFilterBuffer := Buffer;
      OnFilterRecord(Self, Result);
      RestoreState(SaveState);
    end;
    if Result then
      Result := PRecordBuffer(Buffer)^.CachedUpdateStatus in
          FUpdateRecordTypes;
  end
  else
    Result := True;
end;

procedure TFBCustomDataSet.Loaded;
begin
  inherited Loaded;
  GetMacros; {!! trying this way}
end;

function TFBCustomDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  case FieldType of
    ftMemo:Result := TFBAnsiMemoField;
    {$IFDEF FPC}
    ftBLOB:Result := TFBBlobField;
    {$ENDIF}
  else
    Result := inherited GetFieldClass(FieldType);
  end;
end;

function TFBCustomDataSet.MacroByName(const AValue: string): TFBParam;
begin
  Result := FMacros.ParamByName(AValue);
end;

procedure TFBCustomDataSet.MasterUpdate(MasterUpdateStatus:TMasterUpdateStatus);
begin
  if (State  in [dsEdit, dsInsert]) then
  begin
    case FMasterScrollBehavior of
      msbCancel:Cancel;
      msbPost:Post;
    else
      exit;
    end;
  end;
  
  if (MasterUpdateStatus=muFieldChange) then
    CloseOpen(false)
  else
  if (MasterUpdateStatus=muClose) and Active and not (dcIgnoreMasterClose in DetailConditions) then
    Close
  else
  if (MasterUpdateStatus=muOpen) and (dcForceOpen in DetailConditions) and not Active then
    Open;
end;

procedure TFBCustomDataSet.PatternChanged(Sender: TObject);
begin
  if (csLoading in ComponentState) then
  begin
    FStreamPatternChanged := True;
    Exit;
  end;
  Close;
  RecreateMacros;
  FPatternChanged := True;
  try
    ExpandMacros;
  finally
    FPatternChanged := False;
  end;
end;

procedure TFBCustomDataSet.QueryChanged(Sender: TObject);
begin
  FSaveQueryChanged(Sender);
  if not FDisconnectExpected then
  begin
    FSQLPattern := FQuerySelect.SQL;
  end;
end;

procedure TFBCustomDataSet.RecreateMacros;
var
  List: TFBParams;
begin
  if not (csReading in ComponentState) then
  begin
    List := TFBParams.Create(Self);
    try
      CreateMacros(List, PChar(FSQLPattern.Text));
      List.AssignValues(FMacros);
      FMacros.Clear;
      FMacros.Assign(List);
    finally
      List.Free;
    end;
  end
  else
  begin
    FMacros.Clear;
    CreateMacros(FMacros, PChar(FSQLPattern.Text));
  end;
end;

procedure TFBCustomDataSet.SetAllowedUpdateKinds(
  const AValue: TUpdateKinds);
begin
  FAllowedUpdateKinds := AValue;
end;

procedure TFBCustomDataSet.SetAutoCommit(const AValue: boolean);
begin
  FAutoCommit := AValue;
end;

procedure TFBCustomDataSet.SetAutoUpdateOptions(
  const AValue: TAutoUpdateOptions);
begin
  FAutoUpdateOptions.Assign(AValue);
end;

procedure TFBCustomDataSet.SetCachedUpdates(const AValue: Boolean);
begin
  if not AValue and FCachedUpdates and Active then
    CancelUpdates;
  FCachedUpdates := AValue;
end;

procedure TFBCustomDataSet.SetDataBase(const AValue: TJvUIBDataBase);
begin
  if FDataBase <> AValue then
  begin
    FDataBase := AValue;
    FQuerySelect.Close;
    FQueryRefresh.Close;
    FQueryEdit.Close;
    FQueryDelete.Close;
    if (not Assigned(FQuerySelect.Transaction)) or (FQuerySelect.Transaction.DataBase<>AValue) then
    begin
      FQuerySelect.Transaction:=nil;
      FQueryRefresh.Transaction:=nil;
    end;

    if (not Assigned(FQueryEdit.Transaction)) or (FQueryEdit.Transaction.DataBase<>AValue) then
    begin
      FQueryEdit.Transaction:=nil;
      FQueryDelete.Transaction:=nil;
      FQueryInsert.Transaction:=nil;
    end;

    FQuerySelect.DataBase:=AValue;
    FQueryRefresh.DataBase:=AValue;
    FQueryEdit.DataBase:=AValue;
    FQueryDelete.DataBase:=AValue;
    FQueryInsert.DataBase:=AValue;
  end;
end;

procedure TFBCustomDataSet.SetDataSource(AValue: TDataSource);
begin
  {$IFNDEF FPC}
  if IsLinkedTo(AValue) then
    FBError(fbeCircularReference, [Name])
  else
  {$ENDIF}
  if Assigned(FMasterFBDataLink) then
    FMasterFBDataLink.DataSource:=AValue;
end;

procedure TFBCustomDataSet.SetDetailConditions(
  const AValue: TDetailConditions);
begin
  FDetailConditions := AValue;
end;

procedure TFBCustomDataSet.SetBLOBCache(Field: TField; Buffer: PBLOBFieldData);
var
  SaveState : tDataSetState;
  SaveModified : boolean;
  Rec : PRecordBuffer;
begin
  Rec := PRecordBuffer(GetActiveBuf);
  SetFieldData2Record(Field,Buffer,Rec);
end;

procedure TFBCustomDataSet.SetFieldData2Record(Field: TField; Buffer: Pointer; Rec : PRecordBuffer);
var
  FSize:integer;
  Ptr:PByte;
  FNull:PBoolean;
  RecBuf : PRecordBuffer absolute Rec;
  FieldHeader:TFieldHeader;
  BLOBRecord : PBLOBRecordData;
  BLOBField  : PBLOBFieldData;
begin
  if Rec <> nil then
  begin
    if Field.FieldNo >= 0 then
    begin
      Field.Validate(Buffer);
      FieldHeader:=FFiledsHeader[Field.FieldNo-1];
      FNull := @(PBooleans(GetRecordNulls(Self, RecBuf))^[Field.FieldNo-1]);
      Ptr := @(RecBuf^.Data[FieldHeader.FieldOffs]);
      if FieldHeader.FieldSize<>0 then
        FSize:=FieldHeader.FieldSize
      else
        FSize:=Field.DataSize;
    end
    else
    begin
      FNull:=@(PBytes(GetRecordCalcs(Self, Rec))^[Field.Offset]);
      Ptr := @(PBytes(FNull)^[SizeOf(Boolean)]);
      FSize:=Field.DataSize;
    end;

    if (Buffer=nil) or ((Field.DataType in [ftString]) and (PChar(Buffer)[0] = #0)) then
    begin
      FNull^:=true;
      if (Field.DataType in [ftBlob, ftMemo]) then
      begin
        if  PBLOBRecordData(Ptr)^.ListIdx <> 0 then
        begin
           RecBuf^.BlobList.SetItem(PBLOBRecordData(Ptr)^.ListIdx ,nil);
        end;
      end
    end
    else
    begin
      FNull^:=false;
      if not (Field.DataType in [ftBlob, ftMemo]) then
        Move(Buffer^, Ptr^, FSize)
      else begin
        BLOBRecord := PBLOBRecordData(Ptr);
        BLOBField := PBLOBFieldData(Buffer);
        BLOBRecord^.IscQuad := BLOBField^.IscQuad;
        if BLOBRecord^.ListIdx <> 0 then
        begin
          RecBuf^.BlobList.SetItem(BLOBRecord^.ListIdx, BLOBField^.Cache);
        end
        else
        if assigned(PBLOBFieldData(Buffer)^.Cache) then
        begin
          PBLOBRecordData(Ptr)^.ListIdx := RecBuf^.BlobList.Add(PBLOBFieldData(Buffer)^.Cache);
        end;
      end;
    end;
  end;{if assigned(rec)}
end;

procedure TFBCustomDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  Rec : PRecordBuffer;
begin
  Rec := PRecordBuffer(GetActiveBuf);
  if assigned(rec) then begin
    SetFieldData2Record(Field, Buffer, Rec);
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then begin
        DataEvent(deFieldChange, Longint(Field));
    end;
  end;
end;{SetFieldData}

procedure TFBCustomDataSet.SetFieldsFromParams;
var
  i:integer;
  FMaster,FSelf:TField;
begin
  if Assigned(FMasterFBDataLink.DataSource) and Assigned(FMasterFBDataLink.DataSource.DataSet) then
  for i:=0 to FQuerySelect.Params.ParamCount-1 do
  begin
     FMaster:=FMasterFBDataLink.DataSource.DataSet.FindField(FQuerySelect.Params.FieldName[i]);
    FSelf:=FindField(FQuerySelect.Params.FieldName[i]);
    if Assigned(FMaster) and Assigned(FSelf) then
      FSelf.Assign(FMaster);
    DataEvent(deLayoutChange, FCurrentRecord);
  end;
end;

procedure TFBCustomDataSet.SetFiltered(Value: Boolean);
begin
  inherited SetFiltered(Value);
  if Active then
    First;
end;

procedure TFBCustomDataSet.SetMacroChar(const AValue: Char);
begin
  if AValue <> FMacroChar then
  begin
    FMacroChar := AValue;
    RecreateMacros;
  end;
end;

procedure TFBCustomDataSet.SetMacros(const AValue: TFBParams);
begin
  FMacros.AssignValues(AValue);
end;

procedure TFBCustomDataSet.SetOption(const AValue: TFBDsOptions);
begin
  FOption := AValue;
end;

procedure TFBCustomDataSet.SetSQLDelete(const AValue: TStrings);
begin
  FQueryDelete.SQL:=AValue;
end;

procedure TFBCustomDataSet.SetSQLEdit(const AValue: TStrings);
begin
  FQueryEdit.SQL:=AValue;
end;

procedure TFBCustomDataSet.SetSQLInsert(const AValue: TStrings);
begin
  FQueryInsert.SQL:=AValue;
end;

procedure TFBCustomDataSet.SetSQLRefresh(const AValue: TStrings);
begin
  FQueryRefresh.SQL:=AValue;
end;

procedure TFBCustomDataSet.SetSQLSelect(const AValue: TStrings);
begin
  Active:=false;
  TStringList(FSQLPattern).OnChange := nil;
  FSQLPattern.Assign(AValue);
  TStringList(FSQLPattern).OnChange := PatternChanged;
  PatternChanged(nil);
end;

procedure TFBCustomDataSet.SetTransaction(const AValue: TJvUIBTransaction);
begin
  if FQuerySelect.Transaction <> AValue then
  begin
    FQuerySelect.Close;
    FQueryRefresh.Close;
    if FQueryEdit.Transaction = FQuerySelect.Transaction then
    begin
      FQueryEdit.Close;
      FQueryDelete.Close;
      FQueryInsert.Close;
      FQueryEdit.Transaction:=AValue;
      FQueryDelete.Transaction:=AValue;
      FQueryInsert.Transaction:=AValue;
    end;
    FQuerySelect.Transaction:=AValue;
    FQueryRefresh.Transaction:=AValue;
  end;
end;

procedure TFBCustomDataSet.SetUpdateTransaction(
  const AValue: TJvUIBTransaction);
begin
  QueryInsert.Transaction:=AValue;
  QueryEdit.Transaction:=AValue;
  QueryDelete.Transaction:=AValue;
end;

procedure TFBCustomDataSet.SortOnField(FieldName: string; Asc: boolean);
begin
  //Metod for local sorting
  DisableControls;
  try
    FetchAll;
    FRecordsBuffer.SortOnField(FieldByName(FieldName).FieldNo-1, Asc);
  finally
    Resync([]);
    EnableControls;
  end;
end;

procedure TFBCustomDataSet.SortOnFields(FieldNames: string;
  Asc: array of boolean);
var
  SortArray:TFBInternalSortArray;
  CntEl, C:integer;
  S:string;
begin
  FieldNames:=Trim(FieldNames);
  if FieldNames = '' then exit;
  CntEl:=0;
  FillChar(SortArray, SizeOf(TFBInternalSortArray), 0);

  C:=Pos(',', FieldNames);
  while (C>0) and (CntEl < MaxSortField-1) do
  begin
    S:=Copy(FieldNames, 1, C-1);
    System.Delete(FieldNames, 1, C);
    SortArray[CntEl].FieldNo:=FieldByName(S).FieldNo-1;
    if High(Asc)>=CntEl then
      SortArray[CntEl].Asc:=Asc[CntEl]
    else
      SortArray[CntEl].Asc:=true;
    Inc(CntEl);
    C:=Pos(',', FieldNames);
  end;

  if (FieldNames<>'') and (CntEl < MaxSortField-1) then
  begin
    SortArray[CntEl].FieldNo:=FieldByName(FieldNames).FieldNo-1;
    if High(Asc)>=CntEl then
      SortArray[CntEl].Asc:=Asc[CntEl]
    else
      SortArray[CntEl].Asc:=true;
    Inc(CntEl);
  end;
  
  if CntEl = 0 then exit;
  //Metod for local sorting
  DisableControls;
  try
    FetchAll;
    FRecordsBuffer.SortOnFields(SortArray, CntEl);
  finally
    Resync([]);
    EnableControls;
  end;
end;


function TFBCustomDataSet.StoreUpdateTransaction: boolean;
begin
  Result:=QuerySelect.Transaction<>QueryEdit.Transaction;
end;

procedure TFBCustomDataSet.UpdateCommit;
begin
  if FAutoCommit and (UpdateTransaction<>nil) then
    if UpdateTransaction<>Transaction then
      UpdateTransaction.Commit
    else
      UpdateTransaction.CommitRetaining;
end;

procedure TFBCustomDataSet.UpdateStart;
begin
  if UpdateTransaction<>nil then
    if not UpdateTransaction.InTransaction then
      UpdateTransaction.StartTransaction;
end;

procedure TFBCustomDataSet.SetDefaultFormats(const AValue: TDefaultFormats);
begin
  FDefaultFormats.Assign(AValue);
end;

procedure TFBCustomDataSet.UpdateFieldsFormat;
var
  i:integer;
begin
  for i:=0 to Fields.Count-1 do
  begin
    case Fields[i].DataType of
      ftDateTime:if ((Fields[i] as TDateTimeField).DisplayFormat = '') then
                    (Fields[i] as TDateTimeField).DisplayFormat:=DefaultFormats.DisplayFormatDateTime;
      ftDate:if ((Fields[i] as TDateTimeField).DisplayFormat = '') then
                    (Fields[i] as TDateTimeField).DisplayFormat:=DefaultFormats.DisplayFormatDate;
      ftTime:if ((Fields[i] as TDateTimeField).DisplayFormat = '') then
                    (Fields[i] as TDateTimeField).DisplayFormat:=DefaultFormats.DisplayFormatTime;
      ftFloat:begin
                if ((Fields[i] as TNumericField).DisplayFormat = '') then
                    (Fields[i] as TNumericField).DisplayFormat:=DefaultFormats.DisplayFormatNumeric;
                if ((Fields[i] as TNumericField).EditFormat = '') then
                    (Fields[i] as TNumericField).EditFormat:=DefaultFormats.EditFormatNumeric;
              end;
      ftInteger:begin
                if ((Fields[i] as TNumericField).DisplayFormat = '') then
                    (Fields[i] as TNumericField).DisplayFormat:=DefaultFormats.DisplayFormatInteger;
                if ((Fields[i] as TNumericField).EditFormat = '') then
                    (Fields[i] as TNumericField).EditFormat:=DefaultFormats.EditFormatInteger;
              end;
    end;
  end;
end;

procedure TFBCustomDataSet.QuerySelectOnClose(Sender: TObject);
begin
  FQuerySelect.OnClose:=nil;
  Active:=false;
end;

function TFBCustomDataSet.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  O, N:integer;
begin
  o:=FCurrentRecord;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then
  begin
    N:=FCurrentRecord;
    FCurrentRecord:=O;
    DoBeforeScroll;
    FCurrentRecord:=N;
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

function FieldInArray(Field: TField; Arr: array of const): boolean;
var
  i: integer;
  CI: boolean;
begin
  Result := False;
  for i := Low(Arr) to High(Arr) do
  begin
    with Arr[i] do
    begin
      case VType of
        vtInteger: Result := Field.Index = VInteger;
        vtPChar:
          Result :=
            AnsiUpperCase(Field.FieldName) = AnsiUpperCase(vPChar);
        vtAnsiString:
          Result :=AnsiUpperCase(Field.FieldName) = AnsiUpperCase(string(VAnsiString));
//            EquelNames(CI, Field.FieldName, string(VAnsiString));
      else
//        Result :=
      end
    end;
    if Result then
      exit;
  end;
end;

procedure TFBCustomDataSet.CloneRecord(SrcRecord: integer;
  IgnoreFields: array of const);
var
  i:integer;
begin
  if State <> dsInsert then
    Append;
  for i := 0 to FieldCount - 1 do
  begin
    if (Fields[i].FieldKind in [fkData]) and (not Fields[i].IsBlob)
      and (not FieldInArray(Fields[i], IgnoreFields)) then
    begin
      Fields[i].Value := GetAnyRecField(SrcRecord - 1, Fields[i]);
    end;
  end;
end;

procedure TFBCustomDataSet.CloneCurRecord(IgnoreFields: array of const);
begin
  CloneRecord(RecNo, IgnoreFields);
end;

function TFBCustomDataSet.BlobCacheMaintain(Field: TField; Mode: TBlobStreamMode;
  var BLOBRec : TBLOBFieldData) : boolean;
var
  BlobIsNull : boolean;
begin
  BlobIsNull := not GetFieldData(Field, @BLOBRec);
  Result := false;
  if ((Mode = bmWrite) and (BLOBIsNull or not assigned(BLOBrec.Cache) or not BLOBrec.Cache.Alone))
    or ((Mode = bmRead) and (Not BlobIsNull and not assigned(BLOBrec.Cache)))
    or ((Mode = bmReadWrite) and (BlobIsNull or not assigned(BLOBrec.Cache))) then
  begin
    if (Field.DataType = ftMemo) or (Field.DataType = ftFmtMemo) then
      BLOBrec.Cache := TFBAnsiMemoStream.Create(Self, BLOBrec.IscQuad)
    else
      BLOBrec.Cache := TFBBlobStream.Create(Self, BLOBrec.IscQuad);
    SetBLOBCache(field,@BLOBRec);
    BlobIsNull := false;
  end
  else
  if ((Mode = bmReadWrite) and (Not BlobIsNull and assigned(BLOBrec.Cache) and not BLOBrec.Cache.Alone)) then
  begin
    BLOBrec.Cache := BLOBrec.Cache.Clone;
    SetBLOBCache(field,@BLOBRec);
    BlobIsNull := false;
  end;
  Result := (not BlobIsNull) and assigned(BLOBrec.Cache);
end;

function TFBCustomDataSet.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
var
  BLOBRec : TBLOBFieldData;
begin
  if BlobCacheMaintain(Field, Mode, BLOBRec) then
    Result := TBlobWrapStream.Create(Field, BLOBRec.Cache, Mode)
  else
    Result := TNullBlobWrapStream.Create;
end;

function TFBCustomDataSet.GetMemo(Field: TField) : AnsiString;
var
  BLOBRec : TBLOBFieldData;
begin
  if BlobCacheMaintain(Field, bmRead, BLOBRec) then
    Result := BLOBRec.Cache.AnsiText
  else
    Result := '';
end;

procedure TFBCustomDataSet.SetMemo(Field: TField; Value: AnsiString);
var
  BLOBRec : TBLOBFieldData;
begin
  if BlobCacheMaintain(Field, bmWrite, BLOBRec) then
    BLOBRec.Cache.AnsiText := Value;
end;

procedure TFBCustomDataSet.InternalGotoBookmark(ABookmark: Pointer);
var
  ReqBookmark: Integer;
  Buffer: PChar;
begin
  ReqBookmark := PInteger (ABookmark)^;
  Buffer:=nil;
  if (ReqBookmark >= 0) and (ReqBookmark < InternalRecordCount) then
    FCurrentRecord := ReqBookmark
  else
  if ReqBookmark >= 0 then
  begin
    while FCurrentRecord < ReqBookmark do
    begin
      if GetRecord(Buffer, gmNext, false) <> grOk then
        break;
    end;
  end
  else
    raise EMdDataSetError.Create ('Bookmark ' +
      IntToStr (ReqBookmark) + ' not found');
end;

function TFBCustomDataSet.IsEmptyEx: Boolean;
begin
  IsEmptyEx:=(FRecordCount<=0){ or ((BofCrack<0) and (EofCrack))};
end;

function TFBCustomDataSet.BookmarkValid(ABookmark: TBookmark): Boolean;
begin
  Result := False;
  if Assigned(ABookmark) then
    Result := (PMyDBInfo(ABookmark)^.Bookmark>=0) and (PMyDBInfo(ABookmark)^.Bookmark<FRecordsBuffer.Count);
end;

{ TRecordsBuffer }

procedure TRecordsBuffer.Clear;
var
  i:integer;
  Buffer : PChar;
begin
  for i:=0 to Count-1 do
  begin
    Buffer := PChar(Items[I]);
    FOwner.FreeRecordBuffer(Buffer);
    Items[I] := Buffer;
  end;
  inherited Clear;
end;

function TRecordsBuffer.CompareField(Item1, Item2: PRecordBuffer; FieldNo: integer; Asc:Boolean
  ): integer;
var
  Fi1Int:PInteger absolute Item1;
  Fi2Int:PInteger absolute Item2;
  Fi1SInt:PSmallInt absolute Item1;
  Fi2SInt:PSmallInt absolute Item2;
  Fi1Card:PCardinal absolute Item1;
  Fi2Card:PCardinal absolute Item2;
  Fi1D:PDouble absolute Item1;
  Fi2D:PDouble absolute Item2;
  Fi1Bool:PInteger absolute Item1;
  Fi2Bool:PInteger absolute Item2;
  S1,S2:string;
  Nulls1 : PBooleans;
  Nulls2 : PBooleans;
begin

  Nulls1 := GetRecordNulls(FOwner, Item1);
  Nulls2 := GetRecordNulls(FOwner, Item2);
  if Nulls1^[FieldNo] and Nulls2^[FieldNo] then
  begin
    Result:=0;
    exit;
  end
  else
  if Nulls1^[FieldNo] then
  begin
    if Asc then
      Result:=-1
    else
      Result:=1;
    exit;
  end
  else
  if Nulls2^[FieldNo] then
  begin
    if Asc then
      Result:=1
    else
      Result:=-1;
    exit;
  end;

  Item1:=@(Item1^.Data[FOwner.FFiledsHeader[FieldNo].FieldOffs]);
  Item2:=@(Item2^.Data[FOwner.FFiledsHeader[FieldNo].FieldOffs]);

  case FOwner.FFiledsHeader[FieldNo].FieldType of
    ftDate,
    ftInteger:Result:=Sign(Fi1Int^-Fi2Int^);
    ftDateTime,
    ftFloat:Result:=Sign(Fi1D^ - Fi2D^);
    ftString:
      begin
        SetLength(S1, FOwner.FFiledsHeader[FieldNo].FieldSize);
        SetLength(S2, FOwner.FFiledsHeader[FieldNo].FieldSize);
        System.Move(Item1^, S1[1], FOwner.FFiledsHeader[FieldNo].FieldSize);
        System.Move(Item2^, S2[1], FOwner.FFiledsHeader[FieldNo].FieldSize);
        S1:=Trim(s1);
        S2:=Trim(s2);
        Result:=CompareText(S1, S2);
      end;
    ftSmallint:Result:=Sign(Fi1SInt^ - Fi2SInt^);
    ftTime:
      begin
        if Fi1Card^ > Fi2Card^ then Result:=1
        else
        if Fi1Card^ < Fi2Card^ then Result:=-1
        else Result:=0;
      end;
    ftBoolean:
      begin
        if Fi1Bool^ > Fi1Bool^ then Result:=1
        else
        if Fi1Bool^ < Fi1Bool^ then Result:=-1
        else Result:=0;
      end;
  else
    Result:=0;
  end;
  if not Asc then Result:=Result * -1;
end;

constructor TRecordsBuffer.Create(AOwner: TFBCustomDataSet);
begin
  inherited Create;
  FOwner:=AOwner;
end;

procedure TRecordsBuffer.DeleteRecord(RecNo: integer);
var
  i:integer;
  Buffer : PChar;
begin
  Buffer := PChar(Items[RecNo]);
  FOwner.FreeRecordBuffer(Buffer);
  Delete(RecNo);
  for i:=0 to Count-1 do
    with PRecordBuffer(List^[i])^ do
      Bookmark:=i;
end;

procedure TRecordsBuffer.EditRecord(RecNo:integer; NewBuffer : PRecordBuffer);
begin
  if not assigned(NewBuffer^.OldBuffer) then begin
    AssignOldBuffer(FOwner, NewBuffer^, Items[RecNo]);
  end;
end;

destructor TRecordsBuffer.Destroy;
begin
  Clear;
end;

procedure TRecordsBuffer.ReadRecordFromQuery(RecNo:integer; Sourse:TJvUIBQuery);
var
  i:integer;
  P : PRecordBuffer;
  FiPtr:Pointer;
  FieldHeader:TFieldHeader;
  Nulls : PBooleans;
  S:string;
begin
  P := PRecordBuffer(FOwner.AllocRecordBuffer);
  {FOwner.InternalInitRecord(P);}
  Add(P);
  Nulls := GetRecordNulls(FOwner, P);
  with P^ do begin
    Bookmark:=RecNo;
    BookmarkFlag:=bfCurrent;
  end;
  for i := 0 to Sourse.Fields.FieldCount - 1 do
  begin
    FieldHeader:=FOwner.FFiledsHeader[i];
    if Sourse.Fields.IsNull[i] then
    begin
      Nulls^[i]:=true;
    end
    else begin
      Nulls^[i]:=false;
      FiPtr:=@(P^.Data[FieldHeader.FieldOffs]);
      case FieldHeader.FieldType of
        ftFloat:PDouble(FiPtr)^:=Sourse.Fields.AsDouble[i]; //Расширить !!!
        ftString:begin
                   S:=Sourse.Fields.AsString[i];
                   if poTrimCharFields in FOwner.FOption then S:=TrimRight(S);
                   System.Move(S[1], FiPtr^, Min(FieldHeader.FieldSize, Length(S)));
                 end;
        ftSmallint:PSmallInt(FiPtr)^:=Sourse.Fields.AsSmallint[i];
        ftInteger:PInteger(FiPtr)^:=Sourse.Fields.AsInteger[i];
        ftDateTime:
          begin
            {.$IFDEF LINUX}
//            DecodeTimeStamp(PIscTimeStamp(Sourse.Fields.Data.sqlvar[i].sqldata),  Double(FiPtr^));
            {.$ELSE}
            DecodeTimeStamp(PIscTimeStamp(Sourse.Fields.Data.sqlvar[i].sqldata),  TTimeStamp(FiPtr^));
            Double(FiPtr^) := TimeStampToMSecs(TTimeStamp(FiPtr^));
            {.$ENDIF}
          end;
        ftDate:
          begin
            PInteger(FiPtr)^ := PInteger(Sourse.Fields.Data.sqlvar[i].sqldata)^ - DateOffset + 693594;
          end;
        ftTime:
          begin
            PInteger(FiPtr)^:=PCardinal(Sourse.Fields.Data.sqlvar[i].sqldata)^ div 10;
          end;
        ftBlob,
        ftMemo: with PBLOBRecordData(FiPtr)^ do begin
          ISCQuad:=Sourse.Fields.AsQuad[i];
          if ListIdx <> 0 then
            P^.BlobList.SetItem(
              ListIdx,
              TFBBlobStream(P^.BlobList.Item(ListIdx)).ChangeOrNil(ISCQuad)
            );
        end;
        ftLargeint:PInt64(FiPtr)^:=Sourse.Fields.AsInt64[i];
        ftBoolean:PBoolean(FiPtr)^:=Sourse.Fields.AsBoolean[i];
      else
      end
    end;
  end;
  FOwner.GetCalcFields(PChar(P));
end;

procedure TRecordsBuffer.RefreshRecordFromQuery(RecNo: integer;
  Sourse: TJvUIBQuery);
var
  j,k, ii:integer;
  P : PRecordBuffer;
  FiPtr:Pointer;
  FieldHeader:TFieldHeader;
  S:string;
  Nulls : PBooleans;
begin
  P := FOwner.FreshRecordBuffer(PRecordBuffer(Items[RecNo]));
  if (P <> PRecordBuffer(Items[RecNo])) then
    Items[RecNo] := P;
  Nulls := GetRecordNulls(FOwner, P);
  for j := 0 to FOwner.FFiledsHeader.Count-1 do
  begin
    FieldHeader:=FOwner.FFiledsHeader[j];
    K:=-1;
    for ii:=0 to Sourse.Fields.FieldCount-1 do
    begin
      if Sourse.Fields.AliasName[ii]=FieldHeader.FieldName then
      begin
        K:=ii;
        break;
      end
    end;
    if K<>-1 then
    begin
      if Sourse.Fields.IsNull[K] then
      begin
        Nulls^[K] :=true;
      end
      else begin
        Nulls^[K] :=false;
        FiPtr:=@(P^.Data[FieldHeader.FieldOffs]);
        case FieldHeader.FieldType of
          ftFloat:PDouble(FiPtr)^:=Sourse.Fields.AsDouble[k]; //Расширить !!!
          ftString:begin
                     S:=Sourse.Fields.AsString[k];
                     if poTrimCharFields in FOwner.FOption then
                       S:=TrimRight(S);
                     FillChar(FiPtr^, FieldHeader.FieldSize, 0);
                     System.Move(S[1], FiPtr^, Min(FieldHeader.FieldSize, Length(S)));
                   end;
          ftSmallint:PSmallInt(FiPtr)^:=Sourse.Fields.AsSmallint[k];
          ftInteger:PInteger(FiPtr)^:=Sourse.Fields.AsInteger[k];
          ftDateTime:
            begin
              {.$IFDEF  LINUX}
              //DecodeTimeStamp(PIscTimeStamp(Sourse.Fields.Data.sqlvar[k].sqldata),  Double(FiPtr^));
              {.$ELSE}
              DecodeTimeStamp(PIscTimeStamp(Sourse.Fields.Data.sqlvar[k].sqldata),  TTimeStamp(FiPtr^));
              Double(FiPtr^) := TimeStampToMSecs(TTimeStamp(FiPtr^));
              {.$ENDIF}
            end;
          ftDate:
            begin
              PInteger(FiPtr)^:=PInteger(Sourse.Fields.Data.sqlvar[k].sqldata)^ - DateOffset + 693594;
            end;
          ftTime:
            begin
              PInteger(FiPtr)^:=PCardinal(Sourse.Fields.Data.sqlvar[K].sqldata)^ div 10;
            end;


          ftBlob,
          ftMemo: with PBLOBRecordData(FiPtr)^ do begin
            ISCQuad:=Sourse.Fields.AsQuad[K];
            if  ListIdx <> 0 then
              P^.BlobList.SetItem(
                ListIdx,
                TFBBlobStream(P^.BlobList.Item(ListIdx)).ChangeOrNil(ISCQuad)
              );
          end;{ftBlob,ftMemo}
          ftLargeint:PInt64(FiPtr)^:=Sourse.Fields.AsInt64[k];
          ftBoolean:PBoolean(FiPtr)^:=Sourse.Fields.AsBoolean[k];
        else
        end{case FieldHeader.FieldType}
      end;{else if Sourse.Fields.IsNull[K]}
    end;{if K<>-1}
  end;
  FOwner.GetCalcFields(PChar(P));
end;

procedure TRecordsBuffer.SaveFromBuffer(RecNo: integer; Buffer: PChar);
var
  Target : PRecordBuffer;
  NewCopy :PRecordBuffer;
begin
  FOwner.GetCalcFields(Buffer);
  Target := Items[RecNo];
  NewCopy := FOwner.CopyRecordBuffer(PRecordBuffer(Buffer), Target);
  if NewCopy <> Target then
    Items[Recno] := NewCopy;
end;

procedure TRecordsBuffer.SaveFromBufferA(RecNo: integer; Buffer: PChar);
var
  P : PRecordBuffer;
begin
  FOwner.GetCalcFields(Buffer);
  P := FOwner.CopyRecordBuffer(PRecordBuffer(Buffer), nil);
  Add(P);
  with P^ do
  begin
    References := 0;
    Bookmark:=RecNo;
    BookmarkFlag:=bfCurrent;
  end;
end;

procedure TRecordsBuffer.SaveFromBufferI(RecNo: integer; Buffer: PChar);
var
  P : PRecordBuffer;
  i:integer;
  BufferInfo : PMyDBInfo;
begin
  FOwner.GetCalcFields(Buffer);
  P := FOwner.CopyRecordBuffer(PRecordBuffer(Buffer), nil);
  Insert(RecNo, P);
  with P^ do
  begin
    References := 0;
    Bookmark:=RecNo;
    BookmarkFlag:=bfCurrent;
  end;
  for i:=RecNo+1 to Count-1 do
    with PRecordBuffer(List^[i])^ do
      Bookmark:=i;
end;
 

procedure TRecordsBuffer.SaveToBuffer(RecNo: integer; Buffer: PChar);
var
  Source : PRecordBuffer;
  NewCopy :PRecordBuffer;
begin
  if Assigned(Buffer) and (count > 0) then
  begin
    Source := Items[RecNo];
    NewCopy := FOwner.CopyRecordBuffer(Source, PRecordBuffer(Buffer));
    if NewCopy <> PRecordBuffer(Buffer) then
       raise EReferencedObjLoose.Create(EReferencedDatSetRecordBufferLooseMsg);
  end;
end;
 
procedure TRecordsBuffer.SortOnField(FieldNo: integer; Asc: boolean);
procedure DoSort(L,R:Integer);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := List^[(L + R) shr 1];
    repeat
      while CompareField(List^[I], P, FieldNo, Asc) < 0 do
        Inc(I);
      while CompareField(List^[J], P, FieldNo, Asc) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := List^[I];
        List^[I] := List^[J];
        List^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then DoSort(L, J);
    L := I;
  until I >= R;
end;

var
  i:Cardinal;
begin
  if Count>0 then
  begin
    DoSort(0, Count - 1);
    for i:=0 to Count-1 do
      with  PRecordBuffer(List^[i])^ do
        Bookmark:=i;
  end
end;

procedure TRecordsBuffer.SortOnFields(const SortArray:TFBInternalSortArray;const CountEl:integer);

function DoCompare(Item1, Item2:Pointer):integer;
var
  i:integer;
begin
  Result:=0;
  for i:=0 to CountEl-1 do
  begin
    Result:=CompareField(Item1, Item2, SortArray[i].FieldNo, SortArray[i].Asc);
    if Result<>0 then exit;
  end;
end;

procedure DoSort(L,R:Integer);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := List^[(L + R) shr 1];
    repeat
      while DoCompare(List^[I], P) < 0 do
        Inc(I);
      while DoCompare(List^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := List^[I];
        List^[I] := List^[J];
        List^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then DoSort(L, J);
    L := I;
  until I >= R;
end;

var
  i:Cardinal;
begin
  if Count>0 then
  begin
    DoSort(0, Count - 1);
    for i:=0 to Count-1 do
      with  PRecordBuffer(List^[i])^ do
        Bookmark:=i;
  end
end;

{ TRecBuf }

constructor TRecBuf.Create(ABufSize: cardinal);
begin
  FBufSize:=ABufSize;
  GetMem(FCurrent, FBufSize);
  FillChar(FCurrent^, SizeOf(FBufSize), 0);
end;

destructor TRecBuf.Destroy;
begin
  FreeMem(FCurrent, FBufSize);
  if FOriginal<>nil then
    FreeMem(FOriginal, FBufSize);
  inherited;
end;

function TRecBuf.GetOriginal: PChar;
begin
  if FOriginal=nil then Result:=FCurrent
  else Result:=FOriginal
end;


procedure TRecBuf.Modify;
begin
  if FOriginal=nil then
  begin
    GetMem(FOriginal, FBufSize);
    Move(FCurrent^, FOriginal^, FBufSize)
  end;
end;

{ TFBDataLink }

procedure TFBDataLink.ActiveChanged;
begin
  if Active then
    FFBCustomDataSet.MasterUpdate(muOpen)
  else
    FFBCustomDataSet.MasterUpdate(muClose)
end;

constructor TFBDataLink.Create(AFBCustomDataSet: TFBCustomDataSet);
begin
  inherited Create;
  FFBCustomDataSet:=AFBCustomDataSet;
end;

destructor TFBDataLink.Destroy;
begin
  FFBCustomDataSet.FMasterFBDataLink:=nil;
  inherited;
end;

function TFBDataLink.GetDetailDataSet: TDataSet;
begin
  Result:=FFBCustomDataSet;
end;

procedure TFBDataLink.RecordChanged(Field: TField);
begin
  if not (FFBCustomDataSet.FMasterFBDataLink.DataSet.State in [dsEdit, dsInsert]) then
    FFBCustomDataSet.MasterUpdate(muFieldChange);
end;

{ TAutoUpdateOptions }
procedure TAutoUpdateOptions.ApplyGetGenID;
const
  SGENSQL = 'SELECT GEN_ID(%s, %d) FROM RDB$DATABASE';  {do not localize}
var
  sqlGen : TJvUIBQuery;
begin
  if IsComplete and (FOwner.FieldByName(FKeyField).IsNull) then
  begin
    sqlGen := TJvUIBQuery.Create(nil);
    sqlGen.DataBase:=FOwner.DataBase;
    sqlGen.Transaction := FOwner.Transaction;
    try
      sqlGen.SQL.Text := Format(SGENSQL, [QuoteIdentifier(FOwner.Database.SQLDialect, FGeneratorName), FIncrementBy]);
      sqlGen.Execute;
      sqlGen.Next;
      if sqlGen.Fields.FieldType[0] = uftInt64 then
        FOwner.FieldByName(FKeyField).AsInteger := sqlGen.Fields.AsInt64[0]
      else FOwner.FieldByName(FKeyField).AsInteger := sqlGen.Fields.AsInteger[0];
      sqlGen.Close;
    finally
      sqlGen.Free;
    end;
  end;
end;

procedure TAutoUpdateOptions.Assign(Source: TPersistent);
var
  STemp : TAutoUpdateOptions absolute Source;
begin
  if Source is TAutoUpdateOptions then
  begin
    FUpdateTableName:=STemp.FUpdateTableName;
    FKeyField:=STemp.FKeyField;
    FWhenGetGenID:=STemp.FWhenGetGenID;
    FIncrementBy:=STemp.FIncrementBy;
    FGeneratorName:=STemp.FGeneratorName;
  end
  else
    inherited Assign(Source);
end;

constructor TAutoUpdateOptions.Create(AOwner: TFBCustomDataSet);
begin
  inherited Create;
  FOwner:=AOwner;
  FWhenGetGenID:=wgNever;
  FIncrementBy:=1;
end;

destructor TAutoUpdateOptions.Destroy;
begin

  inherited;
end;

function TAutoUpdateOptions.IsComplete: boolean;
begin
  Result:=(FKeyField<>'') and (FGeneratorName<>'') {and (FUpdateTableName<>'')};
end;

procedure TAutoUpdateOptions.SetGeneratorName(const AValue: string);
begin
  FGeneratorName := AValue;
end;

procedure TAutoUpdateOptions.SetIncrementBy(const AValue: integer);
begin
  FIncrementBy := AValue;
end;

procedure TAutoUpdateOptions.SetKeyField(const AValue: string);
begin
  FKeyField := AValue;
end;

procedure TAutoUpdateOptions.SetUpdateTableName(const AValue: string);
begin
  FUpdateTableName := AValue;
end;

procedure TAutoUpdateOptions.SetWhenGetGenID(const AValue: TWhenGetGenID);
begin
  FWhenGetGenID := AValue;
end;

{ TFBTimeField }
(*
procedure TFBTimeField.SetAsString(const AValue: string);
var
  R : TDateTime;
begin
  R:=StrToTime(AVAlue);
  SetData(@R);
end;
*)
{$IFDEF FPC}
(*
{ TFBStringField }

function TFBStringField.GetAsString: string;
var
  Buf : TStringFieldBuffer;
begin
  FillChar(Buf, SizeOf(TStringFieldBuffer), 0);
  if GetData(@Buf) then
    Result:=Buf
  else
    Result:='';
end;

procedure TFBStringField.SetAsString(const AValue: string);
Const NullByte : char = #0;

begin
  IF Length(AValue)=0 then
    SetData(@NullByte)
  else
    SetData(@AValue[1]);
end;

function TFBStringField.IsValidChar(InputChar: Char): Boolean;
begin
  Result:=true;
end;
*)
{$ENDIF}


{******************************************************************************
                             TBlobCacheStream
******************************************************************************}
procedure TBlobCacheStream.EnModifyValue;
begin
  if not (FState in [csStoreBLOB, csLoadBLOB]) then
  begin
    if references > 1 then
      raise Exception.Create('TFBBlobStream: try o modify an multiple used cache value');
    FState := csModified;
    DS.SetModified(True);
  end;
end;

function TBlobCacheStream.Modified : boolean;
begin
  result := (FState = csModified);
end;

procedure TBlobCacheStream.Flush;
begin
  if Modified then
    DoWriteBlob;
end;

procedure TBlobCacheStream.OutDate;
begin
  FState := csNotReady;
  Clear;{Save mem space}
end;

function TBlobCacheStream.ChangeOrNil(const aIsc : tISCQuad) : TBLOBCache;
begin
  result := Self;
  if not (FState in [csStoreBLOB, csLoadBLOB]) then
  begin
    if Alone then
    begin
      ISC := aIsc;
      OutDate;
    end
    else
      Result := nil;
  end;
end;

function TBlobCacheStream.Change(const aIsc : tISCQuad) : TBLOBCache;
begin
  Result := ChangeOrNil(aIsc);
  if not Assigned(Result) then
    Result := TFBBlobStream.Create(DS,aISC);
end;

procedure TBlobCacheStream.Refresh;
begin
  DoReadBlob;
end;

function TBlobCacheStream.Write(const Buffer; Count: Integer): Longint;
begin
  EnModifyValue;
  Result := inherited Write(Buffer, Count);
end;

function TBlobCacheStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (FState = csNotReady) then
    Refresh;
  Result := inherited Read(Buffer, Count);
end;

function TBlobCacheStream.GetSize: Int64;
begin
  if (FState = csNotReady) then
    Refresh;
  Result := inherited GetSize;
end;

procedure TBlobCacheStream.SetSize(NewSize: Longint);
begin
  EnModifyValue;
  inherited SetSize(NewSize);
end;

function TBlobCacheStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if (FState = csNotReady) then
    Refresh;
  Result := inherited Seek(Offset, Origin);
end;

constructor TBlobCacheStream.Create(aDS : TFBCustomDataSet; aOriginISC : tISCQuad);
begin
  inherited Create;
  DS:=aDS;
  ISC := aOriginISC;
  OutDate;
  aDS.FBLOBCache.Add(Self);
end;

destructor TBlobCacheStream.Destroy;
begin
  DS.FBLOBCache.Remove(Self);
  inherited Destroy;
end;

function TBlobCacheStream.Clone : TBlobCacheStream;
begin
  result := New;
  result.LoadFromStream(Self);
  result.FState := FState;
end;

function TBlobCacheStream.GetText : AnsiString;
begin
  SetLength(result, Size);
  Position := 0;
  Read(result[1],Size);
end;

procedure TBlobCacheStream.SetText(const Src : AnsiString);
begin
  SetSize(Length(Src));
  Position := 0;
  Write(Src[1],Length(Src));
end;

{******************************************************************************
                           TFBBlobStream
******************************************************************************}

procedure TFBBlobStream.DoReadBlob;
var
  BlobHandle: IscBlobHandle;
  HDB:IscDbHandle;
  HTR:IscTrHandle;
begin
    HDB:=DS.FDataBase.DbHandle;
    if not DS.UpdateTransaction.InTransaction then
      HTR:=DS.Transaction.TrHandle
    else
      HTR:=DS.UpdateTransaction.TrHandle;
    with DS.FDataBase.Lib do
    begin
      BlobHandle := nil;
      BlobOpen(HDB, HTR, BlobHandle, Isc);
      try
        FState := csLoadBLOB;
        BlobSaveToStream(BlobHandle, Self);
        BlobClose(BlobHandle);
        Seek(0, soFromBeginning);
        FState := csFresh;
      except
        BlobClose(BlobHandle);
        FState := csNotReady;
      end;
    end;
end;

procedure TFBBlobStream.DoWriteBlob;
var
  BLOBRec : TBLOBFieldData;
  BlobHandle: IscBlobHandle;
  HDB:IscDbHandle;
  HTR:IscTrHandle;
begin
  HDB:=DS.FDataBase.DbHandle;
  if not DS.UpdateTransaction.InTransaction then
    DS.UpdateTransaction.StartTransaction;
  HTR:=DS.UpdateTransaction.TrHandle;
  with DS.FDataBase.Lib do
  begin
    BlobHandle := nil;
    BLOBRec.IscQuad := BlobCreate(HDB, HTR, BlobHandle);
    try
      FState := csStoreBLOB;
      BlobWriteStream(BlobHandle, Self);
      BlobClose(BlobHandle);
      Isc := BLOBRec.IscQuad;
      FState := csFresh;
    except
      BlobClose(BlobHandle);
      FState := csModified;
    end;
  end;
end;

function TFBBlobStream.New : TBlobCacheStream;
begin
  Result := TFBBlobStream.Create(DS,ISC);
end;

{*******************************************************************************
                               TFBAnsiMemoStream
*******************************************************************************}
constructor TFBAnsiMemoStream.Create(aDS : TFBCustomDataSet; aOriginISC : TIscQuad);
begin
  inherited Create(aDS,aOriginISC);
  FTextShared := false;
  FMemoText := '';
end;

destructor TFBAnsiMemoStream.Destroy;
begin
  Ansitext := '';
  inherited destroy;
end;

function TFBAnsiMemoStream.New : TBlobCacheStream;
begin
  Result := TFBAnsiMemoStream.Create(DS,ISC);
end;

function TFBAnsiMemoStream.Clone : TBlobCacheStream;
var
  tmp : TFBAnsiMemoStream;
begin
   tmp := TFBAnsiMemoStream.create(DS,ISC);
   tmp.FState := FState;
   tmp.AnsiText :=AnsiText;
   Result := tmp;
end;

function TFBAnsiMemoStream.GetText : AnsiString;
begin
  if (FState = csNotReady) then
    Refresh;
  Result := FMemoText;
  FTextShared := true;
end;

procedure TFBAnsiMemoStream.SetText(const Src : AnsiString);
begin
  FTextShared := false;
  EnModifyValue;
  FMemoText := Src;
  SetPointer(@FMemoText[1],length(FMemoText));
  Capacity := length(FMemoText);
  FTextShared := true;
end;

function TFBAnsiMemoStream.Realloc(var NewCapacity: Longint): Pointer;
begin
  if FTextShared or (Length(FMemoText) <> NewCapacity) then
    SetLength(FMemoText, NewCapacity);
  result := @FMemoText[1];
end;

procedure TFBAnsiMemoStream.EnModifyValue;
begin
  inherited EnModifyValue;
  Capacity := Size;
end;

{*****************************************************************************
                                TFBAnsiMemoField
******************************************************************************}

function TFBAnsiMemoField.GetAsString: string;
begin
  if (DataSet is TFBCustomDataSet) then
    result := (DataSet as TFBCustomDataSet).MemoValue[Self]
  else
    result := inherited GetAsString;
end;

procedure TFBAnsiMemoField.SetAsString(const Value: string);
begin
  if (DataSet is TFBCustomDataSet) then
    (DataSet as TFBCustomDataSet).MemoValue[Self] := Value
  else
    inherited SetAsString(Value);
end;

function TFBAnsiMemoField.GetIsNull: Boolean;
begin
  result := not DataSet.GetFieldData(Self,nil);
end;


{$IFDEF FPC}

{ TFBBlobField }

procedure TFBBlobField.AssignTo(Dest: TPersistent);
begin
  if Dest is TStrings then
  begin
    SaveToStrings(TStrings(Dest));
    Exit;
  end;
  inherited AssignTo(Dest);
end;

procedure TFBBlobField.SaveToStrings(Strings: TStrings);
var
  BlobStream: TStream;
begin
  BlobStream := DataSet.CreateBlobStream(Self, bmRead);
  try
    Strings.LoadFromStream(BlobStream);
  finally
    BlobStream.Free;
  end;
end;

{$ENDIF FPC}


end.
