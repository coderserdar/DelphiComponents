{$I fb_define.inc}
unit mydbunit;

interface

uses
  Classes, SysUtils, DB, fbmisc;

type
  EMdDataSetError = class(Exception);
  EReferencedObjLoose = class(Exception);

type
  TCachedUpdateStatus =
   (cusUnmodified, cusModified, cusInserted, cusDeleted, cusUninserted,
    cusDeletedApplied);

  { record buffer strcture                                                      }
  { 0.. SizeOf(TMyDBInfo)-1 - TMyDBInfo (info of record number and bookmark's)  }
  { SizeOf(TMyDBInfo) .. RecordSize+SizeOf(TMyDBInfo)  - place for record data  }
  { RecordSize+SizeOf(TMyDBInfo) .. RecordSize+SizeOf(TMyDBInfo) + SizeOf(Boolean) * FieldDefs.Count}
  {               - place for null flags of all fields                          }
  { RecordSize+SizeOf(TMyDBInfo) + SizeOf(Boolean) * FieldDefs.Count ...        }
  {               - place for data of calc fields's                             }

  PBytes = ^ Bytes;
  Bytes = packed array[Word] of byte;
  PRecordBuffer = ^tRecordBuffer;
  PBooleans = ^Booleans;
  Booleans = array[Word] of boolean;

  PStream = TStream;
  TBLOBCache = class(TMemoryStream)
  protected
    references : cardinal;
  public
    {true - if not referenced}
    function Dereference : boolean;
    procedure IncReference;overload;
    procedure IncReference(count : cardinal);overload;
    function Alone : boolean;

    constructor Create;
    destructor Destroy; override;
  end;

  PBLOBCache = TBLOBCache;

  TBLOBIndex = byte;

  {a simple wrapper of TList with enforce of Item index starts from 1}
  {упрощенный список с принудительной нумерацией значащих значений с 1
   и ссылочным контролем использования - как в AnsiString }

  TBLOBListItems = array of TBLOBCache;
  TBLOBList = {$IFNDEF nomix_class_obj} object {$ELSE} class {$ENDIF}
  protected
    FItems : TBLOBListItems;
    function references : cardinal;
  public
    {ксожалению 7й борланд глючно делает свойства объектов}
    {casualty Delphi7 implemnt propertie 4 objects with bugs}
    {warning - if old item was nil - then new assigned item will be shared
     on all referenced lists}
    {товарисч помни! - если новое значение затирает пустое - то оно будет
     разделено всеми связаными списками}
    procedure SetItem(Idx : tBLOBIndex; const aItem : PBLOBCache);
    function Item(Idx : tBLOBIndex) : PBLOBCache;
    function Add( aItem : PBLOBCache) : tBLOBIndex;
    function Count : tBLOBIndex;

    {true - if not referenced}
    function Dereference : boolean;
    procedure IncReference;
    {расщепляет\копирует текущий список - делает его неразделяемым}
    {make a copy of current LIST so that one is unshared}
    procedure Clone;
    constructor Init;
    procedure Free;

    constructor Create;
    destructor Destroy;
  end;

  PMyDBInfo = ^TMyDBInfo;
{$IFNDEF nomix_class_obj}

  TMyDBInfo = object
    Bookmark:Longint;
    BookmarkFlag:TBookmarkFlag;
    CachedUpdateStatus:TCachedUpdateStatus;
    BlobList : TBLOBList;
    OldBuffer : PRecordBuffer;
    References : cardinal; {account of additional references to a record}
  end;

  tRecordBuffer = object(TMyDBInfo)
    Data   : Bytes;
  end;

{$ELSE}

  TMyDBInfo = record
    Bookmark:Longint;
    BookmarkFlag:TBookmarkFlag;
    CachedUpdateStatus:TCachedUpdateStatus;
    BlobList : TBLOBList;
    OldBuffer : PRecordBuffer;
    References : cardinal; {account of additional references to a record}
  end;

  tRecordBuffer = record
    Bookmark:Longint;
    BookmarkFlag:TBookmarkFlag;
    CachedUpdateStatus:TCachedUpdateStatus;
    BlobList : TBLOBList;
    OldBuffer : PRecordBuffer;
    References : cardinal; {account of additional references to a record}
    Data   : Bytes;
  end;

{$ENDIF}

{максимальный размер непрерывного пустого блока в TFieldMapList,
 пустоты такого размера автоматически уплотняются методом Pack}
{this is a size of free nodes block 4 wich automaticly called Pack}
const
  MaxDeletedFragment = 8;

type
  {Fields Map - is a list of variables :TField associated with field Name,
    that automaticaly assigned/updated when DataSet fields change
    TFBCustomDataSet.RegisterMap, ForgetMap - works with this list
  }
  {карта полей датасета - задает список переменных TField связанных с именами своих полей
    которые автоматически  обновляются при смене раскладки ДатаСета  ,
    очередность следования\раскладка  определяется юзером,
    вызовом RegisterMap карта регистрируется в датасете и с тех  пор обновляется
      датасетом при каждом изменении раскладки его полей
   вызов ForgetMap - отключает обновление карты.
  }
  PField = ^TField;
  PFields = array of PField;
  TFieldName = string;
  TFieldsMap = array[0 .. 0] of TField;
  PFieldsMap = ^TFieldsMap;

  TFieldMapNode = class(TObject)
    FreeIdx : integer;
    Map    : PFields;

    function Add(var AMap : TField): Integer;
    procedure Delete(Idx : Integer);overload;
    function Delete(var AMap : TField) : boolean;overload;
    procedure RefreshWith( Field : TField);
    function Pack(from : Integer): Integer;
    constructor Create;
    destructor Destroy;override;
  end;

  TFieldMapList = class(TStringList)
    function Add(const Name : TFieldName; var Map : TField): Integer;overload;
    {удаляет фрагмент с пустыми записями на который указывает from,
     возвращает новый индекс записей следущих за удаленным фрагментом}
    procedure Delete(var Map : TField);overload;
    constructor Create;
  end;

type

  TMyDBCustomDataSet = class(TDataSet)
  private
    FDescription: string;
  protected
    // status
    FIsTableOpen: Boolean;
    // record data
  public
    FRecordSize,                 // the size of the actual data
    FRecordBufferSize : Integer; // data + housekeeping (TRecInfo)
    property CalcFieldsSize;
  protected
    FCurrentRecord, // current record (0 to FRecordCount - 1)
    BofCrack, // before the first record (crack)
    EofCrack: Integer; // after the last record (crack)
    FFilterBuffer: PChar;
    FOnUpdateRecord: TUpdateRecordEvent;
    FOnUpdateError: TUpdateErrorEvent;

    FBeforeInit : TDataSetNotifyEvent;
    FAfterInit : TDataSetNotifyEvent;

    FieldMaps : TFieldMapList;
    procedure RefreshMaps;

       procedure Loaded;override;

    // create, close, and so on
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function IsCursorOpen: Boolean; override;
    function GetActiveBuf: PChar;
    function IsEmptyEx: Boolean;virtual;abstract;

    // custom functions
    function InternalRecordCount: Integer; virtual; abstract;
    procedure InternalPreOpen; virtual;
    procedure InternalAfterOpen; virtual;
    procedure InternalLoadCurrentRecord(Buffer: PChar); virtual; abstract;

    // memory management
    function AllocRecordBuffer: PChar; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    {warning do not use if dest=source}
    function CopyRecordBuffer(const Source : PRecordBuffer; Dest : PRecordBuffer) : PRecordBuffer;
    {this is unreference record, or make a new record if source cant be referenced
     properly}
    function FreshRecordBuffer(const Source : PRecordBuffer) : PRecordBuffer;
    function GetRecordSize: Word; override;
  public
    procedure AssignOldBuffer(var Target : tRecordBuffer; const OldBuffer : PRecordBuffer);
    procedure ReleaseOldBuffer(var Target : tRecordBuffer);
    procedure ReferenceOldBuffer(var Target : tRecordBuffer);
  protected
    // movement and optional navigation (used by grids)
    function GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    function GetRecNo: Longint; override;
    function GetRecordCount: Longint; override;
    procedure SetRecNo(Value: Integer); override;

    // bookmarks
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;

    // editing (dummy vesions)
    procedure InternalDelete; override;
    procedure InternalAddRecord(Buffer: Pointer; FAppend: Boolean); override;
    procedure InternalPost; override;

    // other
    procedure InternalHandleException; override;
    property Description: string read FDescription write FDescription;
    property OnUpdateRecord:TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
    property OnUpdateError:TUpdateErrorEvent read FOnUpdateError write FOnUpdateError;

  protected
    UseOldValue : boolean;
    function GetStateFieldValue(State: TDataSetState; Field: TField): Variant; {$IFNDEF FPC} override; {$ENDIF}
    procedure SetStateFieldValue(State: TDataSetState; Field: TField; const Value: Variant); {$IFNDEF FPC} override; {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;

    {регистрирует переменную поля Map : TField для имени FieldName - для автоматического
     обновления при изменении раскладки ДатаСета}
    procedure RegisterMap(const FieldName : TFieldName; var Map : TField);overload;
    {регистрирует массив переменных полей на имена имени FieldName - для автоматического
     обновления при изменении раскладки ДатаСета
     Map1st - первая переменная из массива,
              количество регистрируемых полей определяется количеством имен}
    procedure RegisterMap(const FieldNames : array of TFieldName; var Map1st : TField);overload;
    procedure ForgetMap(var Map : TField);

  published
    // redeclared data set properties
    property Active;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    {this one executed after loading component}
    property BeforeInit : TDataSetNotifyEvent read FBeforeInit write FBeforeInit;
    property AfterInit : TDataSetNotifyEvent read FAfterInit write FAfterInit;
  end;

implementation

{$include FBRecord.inc}

{******************************************************************************
                        TFieldMapNode
*******************************************************************************}
constructor TFieldMapNode.Create;
begin
  map := nil;
  FreeIdx := -1;
end;

destructor TFieldMapNode.Destroy;
begin
  map := nil;
end;

function TFieldMapNode.Add(var AMap : TField): Integer;
var
  idx : integer;
begin
  if FreeIdx < 0 then
  begin
    SetLength(Map, length(Map) + 1);
    idx := High(Map);
  end
  else
  begin
    idx := FreeIdx;
    inc(FreeIdx);
    if (FreeIdx > High(Map)) or assigned(Map[FreeIdx]) then
      FreeIdx := -1;
  end;
  Map[idx] := @AMap;
  result := idx;
end;

procedure TFieldMapNode.Delete(Idx : Integer);
begin
  map[Idx] := nil;
  if FreeIdx < 0 then
    FreeIdx := idx;
end;

function TFieldMapNode.Delete(var AMap : TField) : boolean;
var
  idx : integer;
  FreeCount : integer;
begin
  idx := low(map);
  FreeCount := 0;
  Result := false;
  while (idx < length(Map)) and not Result do
  begin
    if assigned(map[idx]) then
    begin
      FreeCount := 0;
      if map[idx] = @Map then
      begin
        Delete(Idx);
        Result := true;
      end
      else
      begin
        inc(idx);
        continue;
      end;
    end;

    Inc(FreeCount);
    if FreeCount >= MaxDeletedFragment then
    begin
      idx := Pack(idx);
      FreeCount := 0;
    end
    else
      Inc(idx);
  end;
end;

procedure TFieldMapNode.RefreshWith( Field : TField);
var
  idx : integer;
  FreeCount : integer;
begin
  FreeCount := 0;
  idx := low(map);
  while idx <= High(Map) do
  begin
    if assigned(map[Idx]) then
    begin
      FreeCount := 0;
      Map[Idx]^ := Field;
      inc(idx);
    end
    else
    begin
      inc(FreeCount);
      if FreeCount >= MaxDeletedFragment then
      begin
        idx := Pack(idx);
        FreeCount := 0;
      end;
    end;
  end;
end;

function TFieldMapNode.Pack(from : Integer): Integer;
var
  HeadIdx, TailIdx : Integer;
begin
  result := from;
  if assigned(map[from]) then exit;
  HeadIdx := from;
  while (HeadIdx > low(Map) ) do
    if not assigned(map[HeadIdx -1]) then
      dec(HeadIdx);

  result := HeadIdx;
  TailIdx := from + 1;

  while (TailIdx < High(map)) do
    if not assigned(map[tailIdx]) then
      inc(TailIdx);

  if TailIdx < High(map) then
  begin
    move(map[TailIdx], map[HeadIdx], SizeOf(pField) * (TailIdx - HeadIdx));
    SetLength(map, length(map) - (TailIdx - HeadIdx));
  end
  else
    SetLength(map, HeadIdx);

  if (FreeIdx >= HeadIdx) and (FreeIdx < TailIdx) then
    FreeIdx := -1;
end;
{******************************************************************************
                        TFieldMapList
*******************************************************************************}
constructor TFieldMapList.Create;
begin
  inherited Create;
  CaseSensitive := false;
end;

function TFieldMapList.Add(const Name : TFieldName; var Map : TField): Integer;
var
  idx : integer;
begin
  idx := IndexOf(Name);
  if idx < 0 then
    idx := AddObject(name, TFieldMapNode.Create);

  TFieldmapNode(Objects[idx]).Add(Map);
  result := idx;
end;

procedure TFieldMapList.Delete(var Map : TField);
var
  idx : integer;
begin
  for idx := 0 to Count-1 do
  begin
    if TFieldmapNode(Objects[idx]).Delete(Map) then
      break;
  end;
  if length(TFieldmapNode(Objects[idx]).Map) = 0 then
    Delete(idx);
end;

{***************************************************************************
                               TBLOBCache
****************************************************************************}
function TBLOBCache.Dereference : boolean;
begin
  if assigned(self) then
  begin
    if references > 0 then
      dec(references);
    result := (references = 0);
  end
  else
    result := false;
end;

procedure TBLOBCache.IncReference;
begin
  if assigned(Self) then
    inc(references);
end;

procedure TBLOBCache.IncReference(count : cardinal);
begin
  if assigned(Self) then
    inc(references, count+1);
end;

constructor TBLOBCache.Create;
begin
  inherited;
  references := 0
end;

function TBLOBCache.Alone : boolean;
begin
  Alone := (references = 0);
end;

destructor TBLOBCache.Destroy;
begin
  dereference;
  if not Alone then
    raise EReferencedObjLoose.Create(EReferencedBLOBCacheLooseMsg);
  inherited;
end;

{***************************************************************************
                               TBLOBList
****************************************************************************}
function TBLOBList.references : cardinal;
begin
   if Length(FItems) = 0 then
      result := 0
   else
      result := cardinal(FItems[0]);
end;

function TBLOBList.Item(Idx : tBLOBIndex) : PBLOBCache;
begin
  if idx >= Length(FItems) then
    raise Exception.Create('TBLOBList: попытка доступа к несуществующему кешу');
  result := FItems[Idx];
end;

function TBLOBList.Add( aItem : PBLOBCache) : tBLOBIndex;
begin
  if references > 0 then
    Clone;
  if Length(FItems) = 0 then
  begin
    SetLength(FItems,2);
    FItems[0] := nil;
    FItems[1] := aItem;
  end
  else
  begin
    SetLength(FItems,Length(FItems)+1);
    FItems[High(FItems)] := aItem;
  end;
  aItem.IncReference;
  result := High(FItems);
end;

procedure TBLOBList.SetItem(Idx : tBLOBIndex; const aItem : PBLOBCache);
begin
  if idx > High(FItems) then
    raise Exception.Create('TBLOBList: попытка доступа к несуществующему кешу');
  if assigned(FItems[idx]) then
  begin
    if references > 0 then
    {allow to share assigned value, if old value is nil - possible bug source}
    {если старое значение было пустым по пусть новое значение будет разделяемым
     между списками - возможно ето будет причиной багов}
      if (aItem <> FItems[idx]) then
        Clone;
    aItem.Increference;
    if FItems[idx].Dereference then
      FItems[idx].Free;
  end
  else
    aItem.Increference(cardinal(FItems[0]));
  FItems[idx] := aItem;
end;

function TBLOBList.Count : tBLOBIndex;
begin
  result := Length(FItems);
end;

constructor TBLOBList.Init;
begin
  FItems := nil;
end;

procedure TBLOBList.Free;
begin
  Dereference;
end;

function TBLOBList.Dereference : boolean;
var
  idx : tBLOBIndex;
begin
  if Length(FItems) > 0 then
  begin
    for idx := 1 to High(FItems) do
      if FItems[idx].Dereference then
      begin
        FItems[idx].Free;
        FItems[idx] := nil;
      end;

    if FItems[0] = nil then
      SetLength(FItems,0)
    else
      FItems[0] := PBLOBCache(integer(FItems[0])-1);
  end;

  Result := (references = 0);
end;

procedure TBLOBList.IncReference;
var
  idx : tBLOBIndex;
begin
  if Length(FItems) > 0 then
  begin
    FItems[0] := PBLOBCache(integer(FItems[0])+1);
    for idx := 1 to High(FItems) do
      FItems[idx].IncReference;
  end;
end;

procedure TBLOBList.Clone;
var
  ShadowItems : tBLOBListItems;
  PFItems : ^tBLOBListItems;
  tmp : pointer;
type
  PPtr = ^pointer;
begin
  if references > 0 then
  begin
    ShadowItems := copy(FItems, 0, high(Integer));
    FItems[0] := PBLOBCache(integer(FItems[0])-1);
    ShadowItems[0] := nil;
    PFItems := @FItems;
    tmp := FItems;
    pptr(PFItems)^ := nil;
    FItems := ShadowItems;
  end;
end;

constructor TBLOBList.Create;
begin
  Init;
end;

destructor TBLOBList.Destroy;
begin
  Free;
end;

{ TMyDBCustomDataSet }

constructor TMyDBCustomDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FieldMaps := TFieldMapList.Create;
end;
 
destructor  TMyDBCustomDataSet.Destroy;
begin
  FreeAndNil(FieldMaps);
  inherited Destroy;
end;

procedure TMyDBCustomDataSet.InternalOpen;
begin
  InternalPreOpen; // custom method for subclasses
  // initialize the field definitions (another virtual abstract method of TDataSet)
  InternalInitFieldDefs;
  // if there are no persistent field objects, create the fields dynamically
  if DefaultFields then
    CreateFields;
  // connect the TField objects with the actual fields
       
       RefreshMaps;
  
  BindFields (True);

  InternalAfterOpen; // custom method for subclasses
  // sets cracks and record position and size
  BofCrack := -1;
  EofCrack := InternalRecordCount;
  FCurrentRecord := BofCrack;

  FRecordBufferSize := FRecordSize + sizeof (TMyDBInfo) + SizeOf(Boolean) * FieldDefs.Count + CalcFieldsSize;
  BookmarkSize := sizeOf (Integer);

  // everything OK: table is now open
  FIsTableOpen := True;

end;

procedure TMyDBCustomDataSet.Loaded;
begin
  if Assigned(BeforeInit) then BeforeInit(Self);
  inherited Loaded;
  if assigned(AfterInit) then AfterInit(Self);
end;

procedure TMyDBCustomDataSet.InternalClose;
begin
  // disconnet field objects
  BindFields (False);
  // destroy field object (if not persistent)
  if DefaultFields then
    DestroyFields;
  RefreshMaps;

  // close the file
  FIsTableOpen := False;
end;

function TMyDBCustomDataSet.IsCursorOpen: Boolean;
begin
  Result := FIsTableOpen;
end;

procedure TMyDBCustomDataSet.InternalPreOpen;
begin
  // nothing to do: subclasses can hook in here
end;

procedure TMyDBCustomDataSet.InternalAfterOpen;
begin
  // nothing to do: subclasses can hook in here
end;

function TMyDBCustomDataSet.AllocRecordBuffer: PChar;
begin
  GetMem (Result, FRecordBufferSize);
  FillChar(Result^, FRecordBufferSize, 0);
{$IFDEF nomix_class_obj}
  PRecordBuffer(Result)^.BlobList := TBLOBList.Create;
{$ENDIF}
end;

procedure TMyDBCustomDataSet.ReleaseOldBuffer(var Target : tRecordBuffer);
var
  OldDBInfo : PMyDBInfo;
begin
  if assigned(Target.OldBuffer) then begin
    OldDBInfo := @(Target.OldBuffer^);
    if OldDBInfo^.References = 0 then
      FreeRecordBuffer(PChar(Target.OldBuffer))
    else
      dec(OldDBInfo^.References);
    Target.OldBuffer := nil;
  end;
end;

procedure TMyDBCustomDataSet.ReferenceOldBuffer(var Target : tRecordBuffer);
begin
  if assigned(Target.OldBuffer) then
  begin
    inc(Target.OldBuffer^.References);
  end;
end;

procedure TMyDBCustomDataSet.AssignOldBuffer(var Target : tRecordBuffer; const OldBuffer : PRecordBuffer);
begin
  if assigned(Target.OldBuffer) then
    ReleaseOldBuffer(Target);
  inc(OldBuffer^.References);
  Target.OldBuffer := OldBuffer;
end;

procedure TMyDBCustomDataSet.InternalInitRecord(Buffer: PChar);
var
  i:integer;
  BoolPtr:PBooleans;
  RecBuf : PRecordBuffer absolute Buffer;
begin
  ReleaseOldBuffer(RecBuf^);
  RecBuf^.BlobList.Free;
  FillChar(Buffer^, FRecordBufferSize, 0);
{$IFDEF nomix_class_obj}
  RecBuf^.BlobList := TBLOBList.Create;
{$ENDIF}
  BoolPtr:=PBooleans(@(RecBuf^.Data[FRecordSize]) );
  for i:=0 to FieldDefs.Count-1 do
  begin
    BoolPtr^[i]:=true;
  end;
end;

function TMyDBCustomDataSet.CopyRecordBuffer(const Source : PRecordBuffer; Dest : PRecordBuffer) : PRecordBuffer;
begin
  Source^.BlobList.IncReference;
  if assigned(Source^.OldBuffer) then ReferenceOldBuffer(Source^);

  if not assigned(Dest) then
     Dest := PRecordBuffer(AllocRecordBuffer)
  else if Dest^.References = 0 then begin
    Dest^.BlobList.Dereference;
    if assigned(Dest^.OldBuffer) then ReleaseOldBuffer(Dest^);
  end
  else begin
    dec(Dest^.References);
    Dest := PRecordBuffer(AllocRecordBuffer);
  end;
       result := Dest;
  System.Move(Source^, Dest^, FRecordBufferSize);
  Dest^.References := 0;
end;

function TMyDBCustomDataSet.FreshRecordBuffer(const Source : PRecordBuffer) : PRecordBuffer;
begin
  result := Source;
  if result^.References = 0 then
    ReleaseOldBuffer(result^)
  else begin
    dec(result^.References);
    result := PRecordBuffer(AllocRecordBuffer);
    with result^ do begin
      Bookmark:=Source^.Bookmark;
      BookmarkFlag:=Source^.BookmarkFlag;
      CachedUpdateStatus := Source^.CachedUpdateStatus;
    end;
  end;
end;

procedure TMyDBCustomDataSet.FreeRecordBuffer(var Buffer: PChar);
var
  BlobList:^TBLOBList;
  RecBuf : PRecordBuffer absolute Buffer;
begin
  if Assigned(Buffer) then
  begin
    if RecBuf^.References > 0 then
{$IFDEF FB_USE_FREESTYLE_REFERENCED_RECORDS}
      begin
        dec(RecBuf^.References);
        exit;
      end;
{$ELSE}
      raise EReferencedObjLoose.Create(EReferencedRecordBufferLooseMsg);
{$ENDIF}
    ReleaseOldBuffer(RecBuf^);
    RecBuf^.BlobList.Free;
    {FreeAndNil(RecBuf^.BlobList);}
  end;
  FreeMem (Buffer);
end;

function TMyDBCustomDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize; // data only
end;

function TMyDBCustomDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  Result := grOK; // default
  case GetMode of
    gmNext: // move on
      if FCurrentRecord < InternalRecordCount - 1 then
        Inc (FCurrentRecord)
      else
        Result := grEOF; // end of file
    gmPrior: // move back
      if FCurrentRecord > 0 then
        Dec (FCurrentRecord)
      else
        Result := grBOF; // begin of file
    gmCurrent: // check if empty
      if FCurrentRecord >= InternalRecordCount then
        Result := grError;
  end;
  // load the data
  if Result = grOK then
    InternalLoadCurrentRecord (Buffer)
  else
    if (Result = grError) and DoCheck then
      raise EMdDataSetError.Create (sfbeGetRecordInvalidRec);
end;

procedure TMyDBCustomDataSet.InternalFirst;
begin
  FCurrentRecord := BofCrack;
end;

procedure TMyDBCustomDataSet.InternalLast;
begin
  EofCrack := InternalRecordCount;
  FCurrentRecord := EofCrack;
end;

function TMyDBCustomDataSet.GetRecNo: Longint;
begin
  UpdateCursorPos;
  if FCurrentRecord < 0 then
    Result := 1
  else
    Result := FCurrentRecord + 1;
end;

function TMyDBCustomDataSet.GetRecordCount: Longint;
begin
  if (State <> dsInactive) then
    Result:=InternalRecordCount
  else
    Result:=0;
end;

procedure TMyDBCustomDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value >= 1) and (Value <= InternalRecordCount) then
  begin
    FCurrentRecord := Value - 1;
    Resync([]);
  end;
end;

procedure TMyDBCustomDataSet.InternalGotoBookmark(ABookmark: Pointer);
var
  ReqBookmark: Integer;
begin
  ReqBookmark := PInteger (ABookmark)^;
  if (ReqBookmark >= 0) and (ReqBookmark < InternalRecordCount) then
    FCurrentRecord := ReqBookmark
  else
    raise EMdDataSetError.Create ('Bookmark ' +    IntToStr (ReqBookmark) + ' not found');
end;

procedure TMyDBCustomDataSet.InternalSetToRecord(Buffer: PChar);
var
  ReqBookmark: Integer;
begin
  ReqBookmark := PRecordBuffer(Buffer)^.Bookmark;
  InternalGotoBookmark (@ReqBookmark);
end;

procedure TMyDBCustomDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PRecordBuffer(Buffer)^.Bookmark := PInteger(Data)^;
end;

procedure TMyDBCustomDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^ := PRecordBuffer(Buffer)^.Bookmark;
end;

procedure TMyDBCustomDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag
  );
begin
  PRecordBuffer(Buffer)^.BookmarkFlag := Value;
end;

function TMyDBCustomDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PRecordBuffer(Buffer)^.BookmarkFlag;
end;

function TMyDBCustomDataSet.CompareBookmarks(Bookmark1,
  Bookmark2: TBookmark): Integer;
begin
  if (Bookmark1 = nil) and (Bookmark2 = nil) then Result := 0
  else
  if (Bookmark1 <> nil) and (Bookmark2 = nil) then Result := 1
  else
  if (Bookmark1 = nil) and (Bookmark2 <> nil) then Result := -1
  else
  if PLongint(Bookmark1)^ > PLongint(Bookmark2)^ then Result := 1
  else
  if PLongint(Bookmark1)^ < PLongint(Bookmark2)^ then Result := -1
  else Result := 0;
end;

procedure TMyDBCustomDataSet.InternalDelete;
begin
  // not supported in this generic version
  raise EMdDataSetError.Create ('Delete: Operation not supported');
end;

procedure TMyDBCustomDataSet.InternalAddRecord(Buffer: Pointer; FAppend: Boolean
  );
begin
  // not supported in this generic version
  raise EMdDataSetError.Create ('AddRecord: Operation not supported');
end;

procedure TMyDBCustomDataSet.InternalPost;
begin
  // not supported in this generic version
  raise EMdDataSetError.Create ('Post: Operation not supported');
end;

procedure TMyDBCustomDataSet.InternalHandleException;
begin
  // special purpose exception handling
  // do nothing
end;

function TMyDBCustomDataSet.GetActiveBuf: PChar;
var
  myDBInfo : PMyDBInfo;
begin
  case State of
    dsBrowse:
//      if (Bof and Eof) then
      if IsEmptyEx then
        result := nil
      else
        result := ActiveBuffer;
    dsCalcFields  : Result := CalcBuffer;
    dsFilter      : Result := FFilterBuffer;
    dsInactive    : Result := nil;
{    dsEdit,
    dsInsert      : Result := ActiveBuffer;
    dsNewValue    : Result := ActiveBuffer;
    dsOldValue    : begin
      Result := ActiveBuffer;
      myDBInfo := PMyDBInfo(@(PBytes(Result)^[FRecordSize]));
      if assigned(myDBInfo^.OldBuffer) then
        result := PChar(myDBInfo^.OldBuffer);
    end;
}
    else{case}
{      if not FOpen then
        result := nil
      else}
        result := ActiveBuffer;
  end;{case}
  if UseOldValue then begin
      myDBInfo := @(PRecordBuffer(Result)^);
      if assigned(myDBInfo^.OldBuffer) then
        result := PChar(myDBInfo^.OldBuffer);
  end;
end;

function TMyDBCustomDataSet.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
var
  SaveUseOldValue : boolean;
{$IFDEF FPC}
function DoGetStateFieldValue(AState: TDataSetState; Field: TField): Variant;
var
  SaveState: TDataSetState;
begin
  if Field.FieldKind in [fkData, fkInternalCalc] then
  begin
    SaveState := State;
    State := AState;
    try
      Result := Field.AsVariant;
    finally
      State := SaveState;
    end;
  end
  else
    Result := NULL;
end;
{$ENDIF}
begin
  if State in [dsOldvalue, dsNewValue, dsCurValue] then
  begin
    SaveUseOldValue := UseOldValue;
    UseOldValue := (State = dsOldValue);
    try
      result := Field.AsVariant;
    finally
      UseOldValue := SaveUseOldValue;
    end;
  end
  else
  {$IFNDEF FPC}
    Result := inherited GetStateFieldValue(State, Field);
  {$ELSE}
    Result := DoGetStateFieldValue(State, Field);
  {$ENDIF}
end;

procedure TMyDBCustomDataSet.SetStateFieldValue(State: TDataSetState; Field: TField; const Value: Variant);
var
  SaveUseOldValue : boolean;
{$IFDEF FPC}
procedure DoSetStateFieldValue(AState: TDataSetState; Field: TField; const Value: Variant);
var
  SaveState: TDataSetState;
begin
  if Field.FieldKind <> fkData then Exit;
  SaveState := State;
  State := AState;
  try
    Field.AsVariant := Value;
  finally
    State := SaveState;
  end;
end;
{$ENDIF}

begin
  if State in [dsOldvalue, dsNewValue, dsCurValue] then
  begin
    SaveUseOldValue := UseOldValue;
    UseOldValue := (State = dsOldValue);
    try
      Field.AsVariant := Value;
    finally
      UseOldValue := SaveUseOldValue;
    end;
  end
  else
  {$IFNDEF FPC}
    inherited SetStateFieldValue(State, Field, Value);
  {$ELSE}
    DoSetStateFieldValue(State, Field, Value);
  {$ENDIF}
end;

procedure TMyDBCustomDataSet.RegisterMap(const FieldName : TFieldName; var Map : TField);
begin
  if FieldName <> '' then begin
    FieldMaps.Add(FieldName, Map);
  end
  else
    Map := nil;
end;

procedure TMyDBCustomDataSet.RegisterMap(const FieldNames : array of TFieldName; var Map1st : TField);
var
  Map : TFieldsMap absolute Map1st;
  idx : cardinal;
begin
  for idx := low(FieldNames) to high(FieldNames) do
    Registermap(FieldNames[idx], Map[idx]);
end;

procedure TMyDBCustomDataSet.ForgetMap(var Map : TField);
begin
  FieldMaps.Delete(Map);
end;

procedure TMyDBCustomDataSet.RefreshMaps;
var
  MapIdx : integer;
begin
  for MapIdx := 0 to FieldMaps.count-1 do
    TFieldMapNode(FieldMaps.Objects[mapIdx]).RefreshWith( Self.Fields.FindField(FieldMaps.Strings[MapIdx]));
end;


end.




