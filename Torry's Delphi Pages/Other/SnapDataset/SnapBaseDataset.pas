unit SnapBaseDataset;

interface

uses
  Classes,
  TypInfo,
  DB,
  DBConsts;

type
  { TRecInfo }

{   This structure is used to access additional information stored in
  each record buffer which follows the actual record data.

    Buffer: PChar;
   ||
   \/
    --------------------------------------------
    |  Record Data  | Bookmark | Bookmark Flag |
    --------------------------------------------
                    ^-- PRecInfo = Buffer + FRecInfoOfs

  Keep in mind that this is just an example of how the record buffer
  can be used to store additional information besides the actual record
  data.  There is no requirement that TDataSet implementations do it this
  way.

  For the purposes of this demo, the bookmark format used is just an integer
  value.  For an actual implementation the bookmark would most likely be
  a native bookmark type (as with BDE), or a fabricated bookmark for
  data providers which do not natively support bookmarks (this might be
  a variant array of key values for instance).

  The BookmarkFlag is used to determine if the record buffer contains a
  valid bookmark and has special values for when the dataset is positioned
  on the "cracks" at BOF and EOF. }

  TSnapCustomDataset = class;

  TSnapBookmark = pointer;

  PRecordInfo = ^TRecordInfo;
  TRecordInfo = record
    RecordID: Pointer;
    Bookmark: TSnapBookmark;
    BookMarkFlag: TBookmarkFlag;
  end;

  TSnapMasterDataLink = class(TMasterDataLink)
  protected
    procedure ActiveChanged; override;
  end;

  TSnapCustomDataset = class(TDataset)
  private
    FisOpen: Boolean;
    FStartCalculated: Integer;
    FBufferMap: TStringList;
    procedure FillBufferMap;
    function _GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult;
    function RecordFilter: Boolean;
    function GetMasterSource: TDataSource;
    procedure SetMasterSource(const Value: TDataSource);
  protected
    FMasterDataLink: TSnapMasterDataLink;
    // simplified methods to override
    function DoOpen: Boolean; virtual;
    procedure DoClose; virtual; abstract;
    procedure DoDeleteRecord; virtual;
    procedure DoInsertRecord; virtual;
    procedure DoCancelRecord; virtual;
    procedure DoPostRecord; virtual;
    procedure DoCreateFieldDefs; virtual;
    procedure DoSetMasterField(Field: TField); virtual; abstract;
    function GetFieldValue(Field: TField): Variant; virtual; abstract;
    procedure SetFieldValue(Field: TField; Value: Variant); virtual; abstract;
    procedure GetBlobField(Field: TBlobField; Stream: TStream); virtual; abstract;
    procedure SetBlobField(Field: TBlobField; Stream: TStream); virtual; abstract;
    //Handle buffer ID
    function AllocateRecordID: Pointer; virtual; abstract;
    procedure DisposeRecordID(Value: Pointer); virtual; abstract;
    procedure GotoRecordID(Value: Pointer); virtual; abstract;
    //BookMark functions
    function GetBookMarkSize: Integer; virtual;
    procedure DoGotoBookmark(Bookmark: Pointer); virtual; abstract;
    procedure AllocateBookMark(RecordID: Pointer; Bookmark: Pointer); virtual; abstract;
    //Navigation methods
    procedure DoFirst; virtual; abstract;
    procedure DoLast; virtual; abstract;
    function Navigate(GetMode: TGetMode): TGetResult; virtual; abstract;
    procedure SetFiltered(Value: Boolean); override;
    //Internal isOpen property
    property IsOpen: Boolean read FisOpen;
    // DataLink for Master/Detail
    procedure MasterChanged(Sender: TObject); virtual;
    procedure MasterDisabled(Sender: TObject); virtual;
    procedure SetDataSetField(const Value: TDataSetField); override;
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    procedure SetMasterAttribute(const Value: string);
    function GetMasterAttribute: string;
  protected
    // Internal functions that can be overriden if needed
    procedure AllocateBLOBPointers(Buffer: PChar); virtual;
    procedure FreeBlobPointers(Buffer: PChar); virtual;
    procedure FreeRecordPointers(Buffer: PChar); virtual;
    function GetDataSize: Integer; virtual;
    function GetFieldOffset(Field: TField): Integer; virtual;
    procedure SetRecordId(ABuffer: PChar; ABookmarkFlag: TBookmarkFlag); virtual;
    procedure BufferToRecord(Buffer: PChar); virtual;
    procedure RecordToBuffer(Buffer: PChar); virtual;
  protected
    // Dataset abstract methods (required)
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalInsert; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalEdit; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalCancel; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    function IsCursorOpen: Boolean; override;
    function GetCanModify: Boolean; override;
    procedure ClearCalcFields(Buffer: PChar); override;
    function GetActiveRecordBuffer: PChar; virtual;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function SupportedFieldType(AType: TFieldType): Boolean; virtual;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
    property MasterAttribute: string read GetMasterAttribute write SetMasterAttribute;
  end;

  TSnapBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TSnapCustomDataset;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    FOpened: Boolean;
    procedure LoadBlobData;
    procedure SaveBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TSnapCustonIndexedDataset = class(TSnapCustomDataset)
  private
    fAbout: string;
    function GetIndex: integer;
    procedure SetIndex(Value: Integer);
  protected
    FCurrent: Integer;

    function DoOpen: Boolean; override;
    procedure DoClose; override;
    procedure DoFirst; override;
    procedure DoLast; override;
    procedure DoGotoBookmark(Bookmark: Pointer); override;
    procedure GotoRecordID(Value: Pointer); override;
    function Navigate(GetMode: TGetMode): TGetResult; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;

    function AllocateRecordID: Pointer; override;
    procedure DisposeRecordID(Value: Pointer); override;

    property ActiveIndex: integer read GetIndex write SetIndex;
  published
    property About: string read fAbout write fAbout;
  end;

implementation

uses
  SysUtils,
  Windows,
  forms,
  graphics;

resourcestring
  SIndexOutOfRange = 'Index out of range';


{ TSnapCustomDataset }

procedure TSnapCustomDataset.FillBufferMap;
var
  Index: Integer;
begin
  FBufferMap.Clear;
  for Index := 0 to FieldCount - 1 do
    FBufferMap.Add(Fields[Index].FieldName);
end;

procedure TSnapCustomDataset.InternalOpen;
begin
  if DoOpen then
  begin
    BookmarkSize := GetBookMarkSize; //Bookmarks not supported
    InternalInitFieldDefs;
    if DefaultFields then
      CreateFields;
    BindFields(True);
    FisOpen := True;
    FillBufferMap;
  end;
end;

function TSnapCustomDataset.AllocRecordBuffer: PChar;
begin
  GetMem(Result, GetRecordSize);
  FillChar(Result^, GetRecordSize, 0);
  AllocateBlobPointers(Result);
end;

procedure TSnapCustomDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeRecordPointers(Buffer);
  FreeMem(Buffer, GetRecordSize);
end;

procedure TSnapCustomDataset.FreeRecordPointers(Buffer: PChar);
begin
  FreeBlobPointers(Buffer);
  DisposeRecordID(PRecordInfo(Buffer + GetDataSize).RecordID);
  if PRecordInfo(Buffer + GetDataSize)^.BookMark <> nil then
    begin
      FreeMem(PRecordInfo(Buffer + GetDataSize)^.BookMark);
      PRecordInfo(Buffer + GetDataSize)^.BookMark := nil;
    end;
end;

procedure TSnapCustomDataset.AllocateBLOBPointers(Buffer: PChar);
var
  Index: Integer;
  Offset: Integer;
  Stream: TMemoryStream;
begin
  for Index := 0 to FieldCount - 1 do
    if Fields[Index].DataType in [ftMemo, ftGraphic] then
      begin
        Offset := GetFieldOffset(Fields[Index]);
        Stream := TMemoryStream.Create;
        Move(Pointer(Stream), (Buffer + Offset)^, sizeof(Pointer));
      end;
end;

procedure TSnapCustomDataset.FreeBlobPointers(Buffer: PChar);
var
  Index: Integer;
  Offset: Integer;
  FreeObject: TObject;
begin
  for Index := 0 to FieldCount - 1 do
    if Fields[Index].DataType in [ftMemo, ftGraphic] then
      begin
        Offset := GetFieldOffset(Fields[Index]);
        Move((Buffer + Offset)^, Pointer(FreeObject), sizeof(Pointer));
        if FreeObject <> nil then
          FreeObject.Free;
        FreeObject := nil;
        Move(Pointer(FreeObject), (Buffer + Offset)^, sizeof(Pointer));
      end;
end;

procedure TSnapCustomDataset.InternalInitFieldDefs;
begin
  DoCreateFieldDefs;
end;

procedure TSnapCustomDataset.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[FStartCalculated], CalcFieldsSize, 0);
end;

function TSnapCustomDataset.GetActiveRecordBuffer: PChar;
begin
  case State of
    dsBrowse: if isEmpty then
        Result := nil
      else
        Result := ActiveBuffer;
    dsCalcFields: Result := CalcBuffer;
    dsFilter: Result := TempBuffer;
    dsEdit, dsInsert: Result := ActiveBuffer;
  else
    Result := nil;
  end;
end;

function TSnapCustomDataset.GetCanModify: Boolean;
begin
  Result := False;
end;

function TSnapCustomDataset.RecordFilter: Boolean;
var
  SaveState: TDataSetState;
begin
  Result := True;
  if Assigned(OnFilterRecord) then
  begin
    SaveState := SetTempState(dsFilter);
    try
      SetRecordId(TempBuffer, bfCurrent);
      RecordToBuffer(TempBuffer);
      OnFilterRecord(Self, Result);
    except
      Application.HandleException(Self);
    end;
    RestoreState(SaveState);
  end;
end;

function TSnapCustomDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  localAccept : boolean;
begin
  localAccept := True;
  repeat
    Result := Navigate(GetMode);
    if (Result = grOk) then
    begin
      if Filtered then
        localAccept := RecordFilter;

      if localAccept then
      begin
        SetRecordId(Buffer, bfCurrent);
        RecordToBuffer(Buffer);
        ClearCalcFields(Buffer);
        GetCalcFields(Buffer);
      end;
    end
    else if (Result = grError) and DoCheck then
      DatabaseError('No Records');
  until localAccept or (Result in [grEOF, grBOF]);
end;

function TSnapCustomDataset._GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := Navigate(GetMode);
  if (Result = grOk) then
    begin
      SetRecordId(Buffer, bfCurrent);
      RecordToBuffer(Buffer);
      ClearCalcFields(Buffer);
      GetCalcFields(Buffer);
    end
  else if (Result = grError) and DoCheck then
    DatabaseError('No Records');
end;

function TSnapCustomDataset.GetRecordSize: Word;
begin
  Result := GetDataSize + sizeof(TRecordInfo) + CalcFieldsSize;
  FStartCalculated := GetDataSize + sizeof(TRecordInfo);
end;

function TSnapCustomDataset.GetDataSize: Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to FieldCount - 1 do
    case Fields[Index].DataType of
      ftString: Result := Result + Fields[Index].Size + 1; //Leave space for terminating null
      ftInteger: Result := Result + sizeof(Integer);
      ftSmallInt: Result := Result + sizeof(Smallint);
      ftWord: Result := Result + sizeof(word);
      ftBoolean: Result := Result + sizeof(WordBool);
      ftFloat,
      ftCurrency: Result := Result + sizeof(Double);
      ftLargeInt: Result := Result + sizeof(int64);
      ftBCD,
      ftDateTime,
      ftDate,
      ftTime,
      ftTimeStamp: Result := Result + sizeof(double);
      ftMemo,
      ftGraphic,
      ftDataset: Result := Result + sizeof(Pointer);
    end;
end;

procedure TSnapCustomDataset.InternalClose;
begin
  BindFields(False);
  if DefaultFields then
    DestroyFields;
  DoClose;
  FisOpen := False;
end;

procedure TSnapCustomDataset.InternalDelete;
begin
  DoDeleteRecord;
end;

procedure TSnapCustomDataset.InternalEdit;
begin
  if GetActiveRecordBuffer <> nil then
    InternalSetToRecord(GetActiveRecordBuffer);
end;

procedure TSnapCustomDataset.InternalFirst;
begin
  DoFirst;
end;

procedure TSnapCustomDataset.InternalHandleException;
begin
  Application.HandleException(Self);
end;

{This is called by the TDataset to initialize an already existing buffer.
We cannot just fill the buffer with 0s since that would overwrite our BLOB pointers.
Therefore we free the blob pointers first, then fill the buffer with zeros, then
reallocate the blob pointers}

procedure TSnapCustomDataset.InternalInitRecord(Buffer: PChar);
begin
  FreeRecordPointers(Buffer);
  FillChar(Buffer^, GetRecordSize, 0);
  AllocateBlobPointers(Buffer);
end;

procedure TSnapCustomDataset.InternalInsert;
begin
  UpdateCursorPos;

  DoInsertRecord;

  SetRecordId(ActiveBuffer, bfInserted);
  RecordToBuffer(ActiveBuffer);
  ClearCalcFields(ActiveBuffer);
  GetCalcFields(ActiveBuffer);
end;

procedure TSnapCustomDataset.InternalLast;
begin
  DoLast;
end;

procedure TSnapCustomDataset.InternalPost;
begin
  if FisOpen then
  begin
    //DoBeforeSetFieldValue(State = dsInsert);
    BufferToRecord(GetActiveRecordBuffer);
    DoPostRecord;
    //DoAfterSetFieldValue(State = dsInsert);
  end;
end;

procedure TSnapCustomDataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  if Append then
    InternalLast;
  //DoBeforeSetFieldValue(True);
  BufferToRecord(Buffer);
  //DoAfterSetFieldValue(True);
end;

procedure TSnapCustomDataset.InternalSetToRecord(Buffer: PChar);
begin
  if GetBookmarkFlag(Buffer) in [bfCurrent, bfInserted] then
    GotoRecordID(PRecordInfo(Buffer + GetDataSize).RecordID);
end;

function TSnapCustomDataset.IsCursorOpen: Boolean;
begin
  Result := FisOpen;
end;

function TSnapCustomDataset.GetFieldOffset(Field: TField): Integer;
var
  Index, FPos: Integer;
begin
  Result := 0;
  FPos := FBufferMap.Indexof(Field.FieldName);
  for Index := 0 to FPos - 1 do
    begin
      case FieldbyName(FBufferMap[Index]).DataType of
        ftString: inc(Result, FieldbyName(FBufferMap[Index]).Size + 1);
        ftInteger: inc(Result, sizeof(Integer));
        ftSmallInt: inc(Result, sizeof(Smallint));
        ftWord: inc(Result, sizeof(Word));
        ftFloat,
        ftCurrency: inc(Result, sizeof(Double));
        ftLargeInt: inc(Result, sizeof(int64));
        ftBCD,
        ftDate,
        ftTime,
        ftDateTime,
        ftTimeStamp: inc(Result, sizeof(double));
        ftBoolean: inc(Result, sizeof(WordBool));
        ftGraphic,
        ftMemo,
        ftDataset: inc(Result, sizeof(Pointer));
      end;
    end;
end;

procedure TSnapCustomDataset.BufferToRecord(Buffer: PChar);
var
  TempStr: string;
  TempInt: Integer;
  TempDouble: Double;
  TempBool: WordBool;
  TempSmallInt: smallint;
  TempSmallWord: word;
  TempLargeInt: int64;
  TempObject: TObject;
  Offset: Integer;
  Index: Integer;
  Stream: TStream;
  V: variant;
begin
  for Index := 0 to FieldCount - 1 do
    begin
      Offset := GetFieldOffset(Fields[Index]);
      case Fields[Index].DataType of
        ftString:
          begin
            TempStr := PChar(Buffer + Offset);
            SetFieldValue(Fields[Index], TempStr);
          end;
        ftInteger:
          begin
            Move((Buffer + Offset)^, TempInt, sizeof(Integer));
            SetFieldValue(Fields[Index], TempInt);
          end;
        ftSmallInt:
          begin
            Move((Buffer + Offset)^, TempSmallint, sizeof(smallint));
            SetFieldValue(Fields[Index], TempSmallint);
          end;
        ftWord:
          begin
            Move((Buffer + Offset)^, TempSmallWord, sizeof(TempSmallWord));
            SetFieldValue(Fields[Index], TempSmallWord);
          end;
        ftBoolean:
          begin
            Move((Buffer + Offset)^, TempBool, sizeof(WordBool));
            SetFieldValue(Fields[Index], TempBool);
          end;
        ftFloat,
        ftCurrency:
          begin
            Move((Buffer + Offset)^, TempDouble, sizeof(Double));
            SetFieldValue(Fields[Index], TempDouble);
          end;
        ftLargeInt:
          begin
            Move((Buffer + Offset)^, TempLargeInt, sizeof(TempLargeInt));
            SetFieldValue(Fields[Index], TempLargeInt);
          end;
        ftBCD,
        ftDateTime,
        ftDate,
        ftTime,
        ftTimeStamp:
          begin
            DataConvert(Fields[Index], (Buffer + Offset), @TempDouble, false);
            SetFieldValue(Fields[Index], TempDouble);
          end;
        ftGraphic,
        ftMemo:
          begin
            Move((Buffer + Offset)^, Pointer(Stream), Sizeof(Pointer));
            Stream.Position := 0;
            SetBlobField(Fields[Index] as TBlobField, Stream);
          end;
        ftDataset:
          begin
            Move((Buffer + Offset)^, TempObject, Sizeof(TObject));
            TVarData(V).VType := varByRef;
            TVArData(V).VPointer := TempObject;
            SetFieldValue(Fields[Index], V);
          end;
      end;
    end;
end;

procedure TSnapCustomDataset.RecordToBuffer(Buffer: PChar);
var
  Value: Variant;
  TempStr: string;
  TempInt: Integer;
  TempDouble: Double;
  TempBool: WordBool;
  TempSmallInt: Smallint;
  TempSmallWord: word;
  TempLargeInt: int64;
  TempObject: TObject;
  Offset: Integer;
  Index: Integer;
  Stream: TStream;
begin
(*
  with PRecordInfo(Buffer + GetDataSize)^ do
  begin
    BookmarkFlag := bfInserted;
    //BookmarkFlag := bfCurrent;
    RecordID := AllocateRecordID;
    if GetBookMarkSize > 0 then
    begin
      if BookMark = nil then
        GetMem(BookMark, GetBookMarkSize);
      AllocateBookMark(RecordID, BookMark);
    end
    else
      BookMark := nil;
  end;
*)
  //DoBeforeGetFieldValue;
  for Index := 0 to FieldCount - 1 do
    begin
      if not (Fields[Index].DataType in [ftMemo, ftGraphic]) then
        Value := GetFieldValue(Fields[Index]);
      Offset := GetFieldOffset(Fields[Index]);
      case Fields[Index].DataType of
        ftString:
          begin
            TempStr := Value;
            if length(TempStr) > Fields[Index].Size then
              System.Delete(TempStr, Fields[Index].Size, length(TempStr) - Fields[Index].Size);
            StrLCopy(PChar(Buffer + Offset), PChar(TempStr), length(TempStr));
          end;
        ftInteger:
          begin
            TempInt := Value;
            Move(TempInt, (Buffer + Offset)^, sizeof(TempInt));
          end;
        ftSmallInt:
          begin
            TempSmallint := Value;
            Move(TempSmallint, (Buffer + Offset)^, sizeof(TempSmallint));
          end;
        ftWord:
          begin
            TempSmallWord := Value;
            Move(TempSmallWord, (Buffer + Offset)^, sizeof(TempSmallWord));
          end;
        ftLargeInt:
          begin
            TempLargeint := Value;
            Move(TempLargeint, (Buffer + Offset)^, sizeof(TempLargeint));
          end;
        ftBoolean:
          begin
            TempBool := Value;
            Move(TempBool, (Buffer + Offset)^, sizeof(TempBool));
          end;
        ftFloat,
        ftCurrency:
          begin
            TempDouble := Value;
            Move(TempDouble, (Buffer + Offset)^, sizeof(TempDouble));
          end;
        ftBCD,
        ftDateTime,
        ftTime,
        ftDate,
        ftTimeStamp:
          begin
            TempDouble := Value;
            DataConvert(Fields[Index], @TempDouble, (Buffer + Offset), True);
            //Move(TempDouble, (Buffer + Offset)^, sizeof(TempDouble));
          end;
        ftMemo,
        ftGraphic:
          begin
            Move((Buffer + Offset)^, Pointer(Stream), sizeof(Pointer));
            Stream.Size := 0;
            Stream.Position := 0;
            GetBlobField(Fields[Index] as TBlobField, Stream);
          end;
        ftDataset:
          begin
            TempObject := TVarData(Value).VPointer;
            Move(TempObject, (Buffer + Offset)^, sizeof(TObject));
          end;
      end;
    end;
  //DoAfterGetFieldValue;
end;

procedure TSnapCustomDataset.DoDeleteRecord;
begin
  //Nothing in base class
end;

function TSnapCustomDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuffer: PChar;
  pSrc, pDst: PChar;
  Offset: Integer;
  TempDouble: Double;
  Data: TDateTimeRec;
  TimeStamp: TTimeStamp;
  TempBool: WordBool;
begin
  Result := false;
  if not FisOpen then
    exit;
  RecBuffer := GetActiveRecordBuffer;
  if RecBuffer = nil then
    exit;
  if Buffer = nil then
    begin
    //Dataset checks if field is null by passing a nil buffer
    //Tell it is not null by passing back a result of True
      Result := True;
      exit;
    end;
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
    begin
      inc(RecBuffer, FStartCalculated + Field.Offset);
      if (RecBuffer[0] = #0) or (Buffer = nil) then
        exit
      else
        CopyMemory(Buffer, @RecBuffer[1], Field.DataSize);
    end
  else
  begin
    Offset := GetFieldOffset(Field);
    (*
    case Field.DataType of
      ftInteger, ftTime, ftDate: Move((RecBuffer + Offset)^, Integer(Buffer^), sizeof(Integer));
      ftBoolean:
        begin
          Move((RecBuffer + Offset)^, TempBool, sizeof(WordBool));
          Move(TempBool, WordBool(Buffer^), sizeof(WordBool));
        end;
      ftString: StrLCopy(Buffer, PChar(RecBuffer + Offset), StrLen(PChar(RecBuffer + Offset)));
      ftCurrency, ftFloat: Move((RecBuffer + Offset)^, Double(Buffer^), sizeof(Double));
      ftDateTime:
        begin
          Move((RecBuffer + Offset)^, TempDouble, sizeof(Double));
          TimeStamp := DateTimeToTimeStamp(TempDouble);
          Data.DateTime := TimeStampToMSecs(TimeStamp);
          Move(Data, Buffer^, sizeof(TDateTimeRec));
        end;
    end;
    *)
    pDst := pchar(Buffer);
    pSrc := (RecBuffer + Offset);
    case Field.DataType of
      ftDataset: Move(pSrc^, pDst^, Sizeof(TObject));
    else
      Move(pSrc^, pDst^, Field.DataSize);
    end;
    Result := True;

  end;
  Result := True;
end;

procedure TSnapCustomDataset.SetFieldData(Field: TField; Buffer: Pointer);
var
  Offset: Integer;
  pSrc, pDst: PChar;
  RecBuffer: Pchar;
  TempDouble: Double;
  Data: TDateTimeRec;
  TimeStamp: TTimeStamp;
  TempBool: WordBool;
begin
  if not Active then
    exit;
  RecBuffer := GetActiveRecordBuffer;
  if RecBuffer = nil then
    exit;
  if Buffer = nil then
    exit;
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
    begin
      Inc(RecBuffer, FStartCalculated + Field.Offset);
      Boolean(RecBuffer[0]) := (Buffer <> nil);
      if Boolean(RecBuffer[0]) then
        CopyMemory(@RecBuffer[1], Buffer, Field.DataSize);
    end
  else
    begin
      Offset := GetFieldOffset(Field);
      (*
      case Field.DataType of
        ftInteger, ftDate, ftTime: Move(Integer(Buffer^), (RecBuffer + Offset)^, sizeof(Integer));
        ftBoolean:
          begin
            Move(WordBool(Buffer^), TempBool, sizeof(WordBool));
            Move(TempBool, (RecBuffer + Offset)^, sizeof(WordBool));
          end;
        ftString: StrLCopy(PChar(RecBuffer + Offset), Buffer, StrLen(PChar(Buffer)));
        ftDateTime:
          begin
            Data := TDateTimeRec(Buffer^);
            TimeStamp := MSecsToTimeStamp(Data.DateTime);
            TempDouble := TimeStampToDateTime(TimeStamp);
            Move(TempDouble, (RecBuffer + Offset)^, sizeof(TempDouble));
          end;
        ftFloat, ftCurrency: Move(Double(Buffer^), (RecBuffer + Offset)^, sizeof(Double));
      end;
      *)
      pSrc := pchar(Buffer);
      pDst := (RecBuffer + Offset);
      case Field.DataType of
        ftDataset: Move(pSrc^, pDst^, Sizeof(TObject));
      else
        Move(pSrc^, pDst^, Field.Datasize);
      end;

    end;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));
end;

function TSnapCustomDataset.GetBookMarkSize: Integer;
begin
  Result := 0;
end;

procedure TSnapCustomDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if BookMarkSize > 0 then
    AllocateBookMark(PRecordInfo(Buffer + GetDataSize).RecordID, Data);
end;

function TSnapCustomDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PRecordInfo(Buffer + GetDataSize).BookMarkFlag;
end;

procedure TSnapCustomDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if PRecordInfo(Buffer + GetDataSize)^.BookMark = nil then
    GetMem(PRecordInfo(Buffer + GetDataSize)^.BookMark, GetBookMarkSize);
  Move(PRecordInfo(Buffer + GetDataSize).BookMark^, Data, GetBookMarkSize);
end;

procedure TSnapCustomDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecordInfo(Buffer + GetDataSize).BookMarkFlag := Value;
end;

procedure TSnapCustomDataset.InternalGotoBookmark(Bookmark: Pointer);
begin
  DoGotoBookMark(BookMark);
end;

function TSnapCustomDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TSnapBlobStream.Create(Field as TBlobField, Mode);
end;

//************************** TOBlobStream ***************************************

constructor TSnapBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  inherited Create;
  FField := Field;
  FMode := Mode;
  FDataSet := FField.DataSet as TSnapCustomDataset;
  if Mode <> bmWrite then
    LoadBlobData;
end;

destructor TSnapBlobStream.Destroy;
begin
  if FModified then
    SaveBlobData;
  inherited Destroy;
end;

function TSnapBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := inherited Read(Buffer, Count);
  FOpened := True;
end;

function TSnapBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

procedure TSnapBlobStream.LoadBlobData;
var
  Stream: TMemoryStream;
  Offset: Integer;
  RecBuffer: PChar;
begin
  Self.Size := 0;
  RecBuffer := FDataset.GetActiveRecordBuffer;
  if RecBuffer <> nil then
    begin
      Offset := FDataset.GetFieldOffset(FField);
      Move((RecBuffer + Offset)^, Pointer(Stream), sizeof(Pointer));
      Self.CopyFrom(Stream, 0);
    end;
  Position := 0;
end;

procedure TSnapBlobStream.SaveBlobData;
var
  Stream: TMemoryStream;
  Offset: Integer;
  RecBuffer: Pchar;
begin
  RecBuffer := FDataset.GetActiveRecordBuffer;
  if RecBuffer <> nil then
    begin
      Offset := FDataset.GetFieldOffset(FField);
      Move((RecBuffer + Offset)^, Pointer(Stream), sizeof(Pointer));
      Stream.Size := 0;
      Stream.CopyFrom(Self, 0);
      Stream.Position := 0;
    end;
  FModified := False;
end;

procedure TSnapCustomDataset.SetFiltered(Value: Boolean);
begin
  inherited;
  First;
end;

procedure TSnapCustomDataset.InternalCancel;
begin
  DoCancelRecord;
end;

procedure TSnapCustomDataset.DoInsertRecord;
begin
  //Nothing in base class
end;

procedure TSnapCustomDataset.DoCancelRecord;
begin
  //Nothing in base class
end;

function TSnapCustomDataset.GetMasterSource: TDataSource;
begin
  Result := fMasterDataLink.DataSource;
end;

procedure TSnapCustomDataset.SetMasterSource(
  const Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCircularDataLink, Self);
  fMasterDataLink.DataSource := Value;
end;

procedure TSnapCustomDataset.MasterChanged(Sender: TObject);
var
  f: TField;
begin
  f := MasterSource.DataSet.FindField(FMasterDataLink.FieldNames);
  if f<>nil then
    DoSetMasterField(f);
  if Active then
    Resync([]);
end;

procedure TSnapCustomDataset.MasterDisabled(Sender: TObject);
begin
  if Active then
    Resync([]);
end;

procedure TSnapCustomDataset.SetRecordId(ABuffer: PChar; ABookmarkFlag: TBookmarkFlag);
begin
  with PRecordInfo(ABuffer + GetDataSize)^ do
  begin
    BookmarkFlag := ABookmarkFlag;
    RecordID := AllocateRecordID;
    if GetBookMarkSize > 0 then
    begin
      if BookMark = nil then
        GetMem(BookMark, GetBookMarkSize);
      AllocateBookMark(RecordID, BookMark);
    end
    else
      BookMark := nil;
  end;
end;

procedure TSnapCustomDataset.AfterConstruction;
begin
  inherited;
  ObjectView := True;
  FBufferMap := TStringList.Create;
  fMasterDataLink := TSnapMasterDataLink.Create(Self);
  fMasterDataLink.OnMasterChange := MasterChanged;
  fMasterDataLink.OnMasterDisable := MasterDisabled;
end;

procedure TSnapCustomDataset.BeforeDestruction;
begin
  if Active then
    Close;
  FBufferMap.Free;
  fMasterDataLink.Free;
  inherited;
end;

function TSnapCustomDataset.DoOpen: Boolean;
var
  f: TField;
begin
  Result := True;
  if (MasterSource<>nil) and (MasterSource.DataSet<>nil) and (FMasterDataLink.FieldNames<>'') then
  begin
    if (MasterSource.DataSet.Active) then
    begin
      f := MasterSource.DataSet.FindField(FMasterDataLink.FieldNames);
      if f<>nil then
        DoSetMasterField(f)
      else
        DatabaseError(SFieldNotFound);
    end
    else
      DatabaseError(SDataSetClosed, MasterSource.DataSet);
  end;
end;

procedure TSnapCustomDataset.SetDataSetField(
  const Value: TDataSetField);
var
  ds: TSnapCustomDataset;
begin
  if Assigned(Value) then
    DoSetMasterField(Value);
  inherited SetDataSetField(Value);
end;

procedure TSnapCustomDataset.DataEvent(Event: TDataEvent;
  Info: Integer);
begin
  if Event = deParentScroll then
  begin
    if Assigned(DatasetField) then
    begin
      DoSetMasterField(DatasetField);
      Resync([]);
    end;
  end;

  inherited DataEvent(Event, Info);
end;

procedure TSnapCustomDataset.DoCreateFieldDefs;
var
  FieldDef: TFieldDef;

  procedure InitFieldDefsFromFields(Fields: TFields; FieldDefs: TFieldDefs);
  var
    i: integer;
    F: TField;
  begin
    for I := 0 to Fields.Count - 1 do
    begin
      F := Fields[I];
      with F do
        if FieldDefs.IndexOf(FieldName) = -1 then
        begin
          FieldDef := FieldDefs.AddFieldDef;
          FieldDef.Name := FieldName;
          FieldDef.DataType := DataType;
          FieldDef.Size := Size;
          if Required then
            FieldDef.Attributes := [faRequired];
          if ReadOnly then
            FieldDef.Attributes := FieldDef.Attributes + [db.faReadonly];
          if (DataType = ftBCD) and (F is TBCDField) then
            FieldDef.Precision := TBCDField(F).Precision;
          if F is TObjectField then
            InitFieldDefsFromFields(TObjectField(F).Fields, FieldDef.ChildDefs);
        end;
    end;
  end;

begin
  FieldDefs.Clear;
  InitFieldDefsFromFields(Fields, FieldDefs);
end;

function TSnapCustomDataset.GetMasterAttribute: string;
begin
  Result := fMasterDataLink.FieldNames;
end;

procedure TSnapCustomDataset.SetMasterAttribute(const Value: string);
begin
  fMasterDataLink.FieldNames := Value;
end;

procedure TSnapCustomDataset.DoPostRecord;
begin
  //Nothing in base class
end;

function TSnapCustomDataset.SupportedFieldType(AType: TFieldType): Boolean;
begin
  Result := True;
end;

{ TSnapMasterDataLink }

procedure TSnapMasterDataLink.ActiveChanged;
begin
  if Dataset = nil then
    Exit;

  // Fake a field.
  if Fields.Count = 0 then
    Fields.Add(TField.Create(Dataset));

  if DataSet.Active and not (csDestroying in DataSet.ComponentState) then
    if Active then
    begin
      if Assigned(OnMasterChange) then
        OnMasterChange(Self);
    end
    else
    begin
      if Assigned(OnMasterDisable) then
        OnMasterDisable(Self);
    end;
end;

{ TSnapCustonIndexedDataset }

function TSnapCustonIndexedDataset.AllocateRecordID: Pointer;
begin
  Result := Pointer(FCurrent);
end;

procedure TSnapCustonIndexedDataset.DisposeRecordID(Value: Pointer);
begin
  //
end;

procedure TSnapCustonIndexedDataset.DoFirst;
begin
  FCurrent := -1;
end;

procedure TSnapCustonIndexedDataset.DoLast;
begin
  FCurrent := RecordCount;
end;

function TSnapCustonIndexedDataset.DoOpen: Boolean;
begin
  Result := inherited DoOpen;
  FCurrent := -1;
end;

procedure TSnapCustonIndexedDataset.GotoRecordID(Value: Pointer);
begin
  FCurrent := Integer(Value);
end;

function TSnapCustonIndexedDataset.Navigate(GetMode: TGetMode): TGetResult;
begin
  if RecordCount<1 then
    Result := grEOF
  else
  begin
    Result := grOK;
    case GetMode of
      gmNext:
        begin
          if FCurrent>=RecordCount-1 then
            Result:=grEOF
          else
            Inc(FCurrent);
        end;
      gmPrior:
        begin
          if FCurrent<=0 then
          begin
            Result := grBOF;
            FCurrent := -1;
          end
        else
          Dec(FCurrent);
        end;
      gmCurrent:
        if (FCurrent < 0) or (FCurrent >= RecordCount) then
          Result := grError;
    end;
  end;
end;

procedure TSnapCustonIndexedDataset.SetRecNo(Value: Integer);
begin
  if Value<1 then
    Value := 1
  else if Value>=RecordCount then
    Value := RecordCount;
  //Value := min(max(Value, 1), RecordCount);
  if RecNo <> Value then
  begin
    DoBeforeScroll;
    FCurrent := Value-1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

function TSnapCustonIndexedDataset.GetRecNo: Integer;
var
  RecBuf: PChar;
begin
  //CheckActive;
  Result := -1;

  RecBuf := ActiveBuffer;
  if GetBookmarkFlag(RecBuf) = bfCurrent then
    Result := integer(PRecordInfo(RecBuf + GetDataSize).RecordID) + 1;
end;

procedure TSnapCustonIndexedDataset.DoClose;
begin
  FCurrent := -1;
end;

function TSnapCustonIndexedDataset.GetIndex: integer;
begin
  Result := RecNo;
  if Result > -1 then
    dec(Result);
end;

procedure TSnapCustonIndexedDataset.SetIndex(Value: Integer);
begin
  if (Value < 0) or (Value >= RecordCount) then
    DatabaseError(SIndexOutOfRange, Self);
  RecNo := Value+1;
end;


procedure TSnapCustonIndexedDataset.DoGotoBookmark(Bookmark: Pointer);
begin
  FCurrent := PInteger(Bookmark)^;
end;

end.
