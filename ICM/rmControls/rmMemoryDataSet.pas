{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmMemoryDataSet
Purpose  : To allow dataset like functionality with out an actual DB being
           attached.
Date     : 04-24-2000
Author   : Ryan J. Mills
Version  : 1.90
Notes    : This unit is originally based upon the work of Patrick O'Keeffe.
           It was at his request that I took the component over and rm'ified it.
================================================================================}

unit rmMemoryDataSet;

interface

{$I CompilerDefines.INC}

{$ifdef D6_or_higher}
uses
   SysUtils, Windows, DB, Classes, Forms, Variants;
{$else}
uses
  SysUtils, Windows, DB, Classes, Forms;
{$endif}

type
  TSortType = (stAscending, stDescending, stAlternate);
  TSortArray = array of boolean;

  PIntArray = ^TIntArray;
  TIntArray = array[0..1000000] of Integer;

  PByteArray = ^TByteArray;
  TByteArray = array[0..1000000000] of Byte;

  TRecordBlob = packed record
    BlobData : PByteArray;
    BlobSize : LongInt;
    FieldNum : LongInt;
  end;

  TBlobArray = array[0..10] of TRecordBlob;

  TRecordData = packed record
    Bookmark : Integer;
    BookmarkFlag : TBookmarkFlag;
    ArraySize : Integer;
    Blobs : TBlobArray;
    Bytes : TByteArray;
  end;
  PRecordData = ^TRecordData;

  TFieldDefType = (fdtString,
    fdtInteger,
    fdtFloat,
    fdtDateTime,
    fdtBoolean,
    fdtMemo);

  TFieldDefItem = class(TCollectionItem)
  private
    FSize : Integer;
    FName : string;
    FFieldType : TFieldDefType;
  protected

  public
    procedure Assign(Source: TPersistent); override;
  published
    property Name : string read FName write FName;
    property Size : Integer read FSize write FSize;
    property FieldType : TFieldDefType read FFieldType write FFieldType;
  end;

  TFieldDefList = class(TCollection)
  private
    FInternalOwner : TComponent;
    function GetItem(Index : Integer) : TFieldDefItem;
    procedure SetItem(Index : Integer; Value : TFieldDefItem);
  public
    function Add : TFieldDefItem;
    constructor Create(AOwner : TComponent);
    property Items[Index : Integer] : TFieldDefItem read GetItem write SetItem; default;
    property InternalOwner : TComponent read FInternalOwner;
  end;

  // Collection item object for records
  TTextRecord = class(TCollectionItem)
  public
    destructor Destroy; override;
  protected
    Data : PRecordData;
  end;

  TrmLongStringField = class(TStringField)
  public
    class procedure CheckTypeSize(Value : Integer); override;
    function GetAsString : string; override;
    function GetAsVariant : Variant; override;
    function GetValue(var Value : string) : Boolean;
    procedure SetAsString(const Value : string); override;
  end;

{ TrmMemoryDataSet }

  TrmMemoryDataSet = class(TDataSet)
  private
    Records : TCollection;
    FieldOffSets : PIntArray;
    FFieldRoster : TFieldDefList;
    FRecBufSize : Integer;
    FCurRec : Integer;
    FLastBookmark : Integer;
    MaxFieldNo : Integer;
    FRecordSize : Integer;
    FFilterBuffer : PRecordData;
    FSortOrder : TSortArray;
    function GetRecBufSize : Integer;
    procedure QueryRecords;
    function FieldOffset(Field : TField) : Integer;
    function GetActiveRecBuf(var RecBuf : PRecordData) : Boolean;
    procedure InternalUpdate;
    procedure SaveRecordData(Buffer : PRecordData; Index : Integer);
  protected
    { Overriden abstract methods (required) }
    function AllocRecordBuffer : PChar; override;
    procedure FreeRecordBuffer(var Buffer : PChar); override;
    procedure GetBookmarkData(Buffer : PChar; Data : Pointer); override;
    function GetBookmarkFlag(Buffer : PChar) : TBookmarkFlag; override;
    function GetFieldClass(FieldType : TFieldType) : TFieldClass; override;
    function GetRecord(Buffer : PChar; GetMode : TGetMode; DoCheck : Boolean) : TGetResult; override;
    function GetRecordSize : Word; override;
    procedure InternalAddRecord(Buffer : Pointer; Append : Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark : Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer : PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer : PChar); override;
    function IsCursorOpen : Boolean; override;
    procedure SetBookmarkFlag(Buffer : PChar; Value : TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer : PChar; Data : Pointer); override;
    procedure SetFieldData(Field : TField; Buffer : Pointer); override;

    procedure CloseBlob(Field : TField); override;
  protected
    { Additional overrides (optional) }
    function GetRecordCount : Integer; override;
    function GetRecNo : Integer; override;
    procedure SetRecNo(Value : Integer); override;
  public
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field : TField; Mode : TBlobStreamMode) : TStream; override;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field : TField; Buffer : Pointer) : Boolean; override;
    procedure Sort(field : TField; direction : TSortType);
    function Lookup(const KeyFields : string; const KeyValues : Variant;
      const ResultFields : string) : Variant; override;
    function Locate(const KeyFields : string; const KeyValues : Variant;
      Options : TLocateOptions) : Boolean; override;
    property SortOrder : TSortArray read fSortOrder;
  published
    property FieldRoster : TFieldDefList read FFieldRoster write FFieldRoster;
    property Active;
    property OnNewRecord;
    property OnCalcFields;
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
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnPostError;
  end;

const
  RecInfoSize = SizeOf(TRecordData) - SizeOf(TByteArray);

const
  DefaultFieldClasses : array[ftUnknown..ftTypedBinary] of TFieldClass = (
    nil, (* ftUnknown *)
    TrmLongStringField, (* ftString *)
    TSmallintField, (* ftSmallint *)
    TIntegerField, (* ftInteger *)
    TWordField, (* ftWord *)
    TBooleanField, (* ftBoolean *)
    TFloatField, (* ftFloat *)
    TCurrencyField, (* ftCurrency *)
    TBCDField, (* ftBCD *)
    TDateField, (* ftDate *)
    TTimeField, (* ftTime *)
    TDateTimeField, (* ftDateTime *)
    TBytesField, (* ftBytes *)
    TVarBytesField, (* ftVarBytes *)
    TAutoIncField, (* ftAutoInc *)
    TBlobField, (* ftBlob *)
    TMemoField, (* ftMemo *)
    TGraphicField, (* ftGraphic *)
    TBlobField, (* ftFmtMemo *)
    TBlobField, (* ftParadoxOle *)
    TBlobField, (* ftDBaseOle *)
    TBlobField); (* ftTypedBinary *)

implementation

type
  // TResultSetBlobStream
  TResultSetBlobStream = class(TStream)
  private
    FField : TBlobField;
    FBlobIdx : LongInt;
    FDataSet : TrmMemoryDataSet;
    FBuffer : PRecordData;
    FMode : TBlobStreamMode;
    FOpened : Boolean;
    FModified : Boolean;
    FPosition : Longint;
    function GetBlobSize : Longint;
    function GetBlobOffset(FieldNum : LongInt) : LongInt;
  public
    constructor Create(Field : TBlobField; Mode : TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count : Longint) : Longint; override;
    function Write(const Buffer; Count : Longint) : Longint; override;
    function Seek(Offset : Longint; Origin : Word) : Longint; override;
    procedure Truncate;
  end;


procedure FreeBlob(Buffer : PRecordData);
var
  Idx : LongInt;
begin
  for Idx := 0 to 10 do
  begin
    with Buffer^.Blobs[Idx] do
    begin
      if (BlobData <> nil) and (BlobSize > 0) then
        FreeMem(BlobData, BlobSize);
      BlobData := nil;
      BlobSize := 0;
      FieldNum := 0;
    end;
  end;
end;

// TResultSetBlobStream
// This stream is used to communicate between the Blob fields and the
// underlying record collection

constructor TResultSetBlobStream.Create(Field : TBlobField; Mode : TBlobStreamMode);
begin
  FMode := Mode;
  FField := Field;
  FDataSet := FField.DataSet as TrmMemoryDataSet;
  if not FDataSet.GetActiveRecBuf(FBuffer) then
    Exit;
  FBlobIdx := GetBlobOffset(FField.FieldNo);
  if not FField.Modified then
  begin
    if Mode <> bmRead then
      if not (FDataSet.State in [dsEdit, dsInsert]) then
        DatabaseError('Not in Insert or Edit Mode');
  end;
  FOpened := True;
  if Mode = bmWrite then
  begin
    Truncate;
  end;
end;

destructor TResultSetBlobStream.Destroy;
begin
  if FOpened then
  begin
    if FModified then FField.Modified := True;
  end;
  if FModified then
  begin
    try
      FDataSet.DataEvent(deFieldChange, Longint(FField));
    except
      Application.HandleException(Self);
    end;
  end;
end;

function TResultSetBlobStream.Read(var Buffer; Count : Longint) : Longint;
begin
  Result := 0;
  if FOpened then
  begin
    if Count > Size - FPosition then
    begin
      Result := Size - FPosition
    end
    else
    begin
      Result := Count;
    end;
    if Result > 0 then
    begin
      Move(FBuffer^.Blobs[FBlobIdx].BlobData^[FPosition], Buffer, Result);
      Inc(FPosition, Result);
    end;
  end;
end;

function TResultSetBlobStream.Write(const Buffer; Count : Longint) : Longint;
var
  Temp : Pointer;
  NewSize : LongInt;
begin
  Result := 0;
  if FOpened then
  begin
    NewSize := FPosition + Count;
    if NewSize < FBuffer^.Blobs[FBlobIdx].BlobSize then
    begin
      NewSize := FBuffer^.Blobs[FBlobIdx].BlobSize;
    end;

    if (NewSize > FBuffer^.Blobs[FBlobIdx].BlobSize) or
      not (FModified or FField.Modified) then
    begin
      GetMem(Temp, NewSize);
      if (FBuffer^.Blobs[FBlobIdx].BlobData <> nil) and
        (FBuffer^.Blobs[FBlobIdx].BlobSize > 0) then
      begin
        Move(FBuffer^.Blobs[FBlobIdx].BlobData^, Temp^, FBuffer^.Blobs[FBlobIdx].BlobSize);
        if (FModified or FField.Modified) then
        begin
          FreeBlob(FBuffer);
        end;
      end;
      FBuffer^.Blobs[FBlobIdx].BlobData := Temp;
    end;
    Move(Buffer, FBuffer^.Blobs[FBlobIdx].BlobData^[FPosition], Count);
    Inc(FPosition, Count);
    if FPosition > FBuffer^.Blobs[FBlobIdx].BlobSize then
    begin
      FBuffer^.Blobs[FBlobIdx].BlobSize := FPosition;
    end;
    Result := Count;
    FModified := True;
  end;
end;

function TResultSetBlobStream.Seek(Offset : Longint; Origin : Word) : Longint;
begin
  case Origin of
    soFromBeginning : FPosition := Offset;
    soFromCurrent : Inc(FPosition, Offset);
    soFromEnd : FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

procedure TResultSetBlobStream.Truncate;
begin
  if FOpened then
  begin
    FPosition := 0;
    if FField.Modified then
    begin
      FreeBlob(FBuffer);
    end;
    FBuffer^.Blobs[FBlobIdx].BlobData := nil;
    FBuffer^.Blobs[FBlobIdx].BlobSize := 0;
    FModified := True;
  end;
end;

function TResultSetBlobStream.GetBlobSize : Longint;
begin
  Result := 0;
  if FOpened then
  begin
    Result := FBuffer^.Blobs[FBlobIdx].BlobSize;
  end;
end;

function TResultSetBlobStream.GetBlobOffset(FieldNum : LongInt) : LongInt;
var
  Idx : LongInt;
begin
  Result := -1;
  for Idx := 0 to 10 do
  begin
    if FBuffer^.Blobs[Idx].FieldNum = FieldNum then
    begin
      Result := Idx;
      Break;
    end;
  end;

  if result < 0 then
    for idx := 0 to 10 do
      if FBuffer^.Blobs[Idx].FieldNum = -1 then
      begin
        FBuffer^.Blobs[Idx].FieldNum := FieldNum;
        result := idx;
        break;
      end;

  if result < 0 then
    DatabaseError('Too many blobs', self.FDataSet);
end;

(*
 * TrmLongStringField - implementation
 *)

class procedure TrmLongStringField.CheckTypeSize(Value : Integer);
begin
  (*
   * Just don't check. Any string size is valid.
   *)
end;



function TrmLongStringField.GetAsString : string;
begin
  if not GetValue(Result) then Result := '';
end;

function TrmLongStringField.GetAsVariant : Variant;
var
  S : string;
begin
  if GetValue(S) then
    Result := S
  else
    Result := Null;
end;

function TrmLongStringField.GetValue(var Value : string) : Boolean;
var
  Buffer : PChar;
begin
  GetMem(Buffer, Size + 1);
  try
    Result := GetData(Buffer);
    if Result then
    begin
      Value := Trim(string(Buffer));
      if Transliterate and (Value <> '') then
        DataSet.Translate(PChar(Value), PChar(Value), False);
    end
  finally
    FreeMem(Buffer);
  end;
end;

procedure TrmLongStringField.SetAsString(const Value : string);
var
  Buffer : PChar;
begin
  GetMem(Buffer, Size + 1);
  try
    StrLCopy(Buffer, PChar(Value), Size);
    if Transliterate then
      DataSet.Translate(Buffer, Buffer, True);
    SetData(Buffer);
  finally
    FreeMem(Buffer);
  end;
end;


destructor TTextRecord.Destroy;
begin
  if Data <> nil then
  begin
    FreeMem(Data, Data^.ArraySize + RecInfoSize);
  end;
  inherited;
end;

{ TrmMemoryDataSet }

{ This method is called by TDataSet.Open and also when FieldDefs need to
  be updated (usually by the DataSet designer).  Everything which is
  allocated or initialized in this method should also be freed or
  uninitialized in the InternalClose method. }

constructor TrmMemoryDataSet.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FFieldRoster := TFieldDefList.Create(Self);
end;

destructor TrmMemoryDataSet.Destroy;
begin
  FFieldRoster.Free;
  inherited Destroy;
end;


function TrmMemoryDataSet.GetFieldClass(FieldType : TFieldType) : TFieldClass;
begin
  Result := DefaultFieldClasses[FieldType];
end;

// Calculate Buffer Size. Can only be called after BindFields

function TrmMemoryDataSet.GetRecBufSize : Integer;
var
  i : Integer;

begin
  MaxFieldNo := 0;
  for i := 0 to FieldCount - 1 do
    with Fields[i] do
      if FieldNo > MaxFieldNo then
        MaxFieldNo := FieldNo;
  Inc(MaxFieldNo);

  GetMem(FieldOffsets, MaxFieldNo * SizeOf(Integer));

  Result := 0;
  FRecordSize := 0;
  for i := 0 to FieldCount - 1 do
    with Fields[i] do
    begin
      if FieldNo >= 0 then
        FieldOffsets^[FieldNo] := FRecordSize;
      Inc(Result, DataSize + 1);
      Inc(FRecordSize, DataSize + 1);
    end;
  Inc(Result, RecInfoSize);
end;


procedure TrmMemoryDataSet.QueryRecords;
begin
  // Clear the record collection
  Records.Clear;
end;

function TrmMemoryDataSet.FieldOffset(Field : TField) : Integer;
begin
  Result := FieldOffsets[Field.FieldNo];
end;

procedure TrmMemoryDataSet.InternalOpen;
begin
  FieldOffsets := nil;
  // Initialize our internal position.
  // We use -1 to indicate the "crack" before the first record.
  FCurRec := -1;
  // Tell TDataSet how big our Bookmarks are
  BookmarkSize := SizeOf(Integer);

  InternalInitFieldDefs;
  // Create TField components when no persistent fields have been created
  if DefaultFields then CreateFields;
  // Bind the TField components to the physical fields
  BindFields(True);
  // Create collection for records
  Records := TCollection.Create(TTextRecord);

  // Calculate the size of the record buffers.
  // Note: This is NOT the same as the RecordSize property which
  // only gets the size of the data in the record buffer
  FRecBufSize := GetRecBufSize;
  // Query records to fill collection
  QueryRecords;
  SetLength(fSortOrder, fields.Count);
end;

procedure TrmMemoryDataSet.InternalClose;
begin
  // Free the record collection
  Records.Free;
  Records := nil;
  // Destroy the TField components if no persistent fields
  if DefaultFields then
    DestroyFields;
  // Reset these internal flags
  FLastBookmark := 0;
  FCurRec := -1;
  // Free memory for Field offset array
  if FieldOffsets <> nil then
    FreeMem(FieldOffsets, MaxFieldNo * SizeOf(Integer));
  FieldOffsets := nil;
end;

function TrmMemoryDataSet.IsCursorOpen : Boolean;
begin
  Result := Assigned(Records);
end;

procedure TrmMemoryDataSet.InternalInitFieldDefs;
var
  i : Integer;
  FieldName : string;
  FieldRequired : boolean;
  FieldSize : Integer;
  FieldType : TFieldType;
  FieldNo : Integer;

begin
  FieldDefs.Clear;

  // Create a field in the dataset for each field in the query
  if FFieldRoster.Count = 0 then
    raise Exception.Create('There are no fields in the Field Roster');

  FieldNo := 1;

  for i := 0 to FFieldRoster.Count - 1 do
  begin
    FieldName := FFieldRoster.Items[i].Name;
    FieldRequired := True;
    FieldSize := FFieldRoster.Items[i].Size;
    case FFieldRoster.Items[i].FieldType of
      fdtString : FieldType := ftString;
      fdtInteger : FieldType := ftInteger;
      fdtFloat : FieldType := ftFloat;
      fdtDateTime : FieldType := ftDateTime;
      fdtBoolean : FieldType := ftBoolean;
      fdtMemo : FieldType := ftMemo;
    else
      FieldType := ftString;
    end;

    if not (FieldType in [ftString]) then
      FieldSize := 0;

    TFieldDef.Create(FieldDefs, FieldName, FieldType, FieldSize, FieldRequired, FieldNo);
    Inc(FieldNo);
  end;
end;

procedure TrmMemoryDataSet.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TrmMemoryDataSet.InternalGotoBookmark(Bookmark : Pointer);
var
  i, b : Integer;

begin
  b := PInteger(Bookmark)^;
  if (b - 1 > 0) and (b - 1 < Records.Count) then
  begin
    if b = TTextRecord(Records.Items[b - 1]).Data^.Bookmark then
    begin
      FCurRec := b - 1;
      Exit;
    end;
  end;
  for i := 0 to Records.Count - 1 do
  begin
    if PInteger(Bookmark)^ = TTextRecord(Records.Items[i]).Data^.Bookmark then
    begin
      FCurRec := i;
      Exit;
    end;
  end;
  DatabaseError('Bookmark not found');
end;

procedure TrmMemoryDataSet.InternalSetToRecord(Buffer : PChar);
begin
  InternalGotoBookmark(@PRecordData(Buffer).Bookmark);
end;

function TrmMemoryDataSet.GetBookmarkFlag(Buffer : PChar) : TBookmarkFlag;
begin
  Result := PRecordData(Buffer).BookmarkFlag;
end;

procedure TrmMemoryDataSet.SetBookmarkFlag(Buffer : PChar; Value : TBookmarkFlag);
begin
  PRecordData(Buffer).BookmarkFlag := Value;
end;

procedure TrmMemoryDataSet.GetBookmarkData(Buffer : PChar; Data : Pointer);
begin
  PInteger(Data)^ := PRecordData(Buffer).Bookmark;
end;

procedure TrmMemoryDataSet.SetBookmarkData(Buffer : PChar; Data : Pointer);
begin
  PRecordData(Buffer).Bookmark := PInteger(Data)^;
end;

function TrmMemoryDataSet.GetRecordSize : Word;
begin
  Result := FRecordSize;
end;

function TrmMemoryDataSet.AllocRecordBuffer : PChar;
var
  b : PRecordData;
  Idx : Integer;

begin
  GetMem(Result, FRecBufSize);
  b := PRecordData(Result);
  b^.ArraySize := FRecBufSize - RecInfoSize;
  for Idx := 0 to 10 do
  begin
    b^.Blobs[Idx].BlobData := nil;
    b^.Blobs[Idx].BlobSize := 0;
    b^.Blobs[Idx].FieldNum := 0;
  end;
end;

procedure TrmMemoryDataSet.FreeRecordBuffer(var Buffer : PChar);
begin
  FreeMem(Buffer, FRecBufSize);
end;

function TrmMemoryDataSet.GetRecord(Buffer : PChar; GetMode : TGetMode;
  DoCheck : Boolean) : TGetResult;
begin
  Result := grOK;
  case GetMode of
    gmNext :
      if FCurRec < Records.Count - 1 then
        Inc(FCurRec)
      else
        Result := grEOF;

    gmPrior :
      if FCurRec <= 0 then
        Result := grBOF
      else
        Dec(FCurRec);

    gmCurrent :
      if (FCurRec < 0) or (FCurRec >= Records.Count) then
      begin
        Result := grError
      end;
  end;
  if Result = grOK then
  begin
    Move(TTextRecord(Records.Items[FCurRec]).Data^, Buffer^, FRecBufSize);
    with PRecordData(Buffer)^ do
      BookmarkFlag := bfCurrent;
  end
  else
    if (Result = grError) and DoCheck then
      DatabaseError('No Records');
end;

procedure TrmMemoryDataSet.InternalInitRecord(Buffer : PChar);
var
  Idx : Integer;
begin
  FillChar(PRecordData(Buffer)^.Bytes[0], FRecordSize + CalcFieldsSize, 1);
  for Idx := 0 to 10 do
  begin
    PRecordData(Buffer)^.Blobs[Idx].BlobData := nil;
    PRecordData(Buffer)^.Blobs[Idx].BlobSize := 0;
    PRecordData(Buffer)^.Blobs[Idx].FieldNum := -1;
  end;
end;

function TrmMemoryDataSet.GetActiveRecBuf(var RecBuf : PRecordData) : Boolean;
var
  i : Integer;

begin
  case State of
    dsBrowse :
      if IsEmpty then
        RecBuf := nil
      else
        RecBuf := PRecordData(ActiveBuffer);

    dsEdit, dsInsert :
      RecBuf := PRecordData(ActiveBuffer);

    dsNewValue,
      dsCurValue :
      RecBuf := PRecordData(ActiveBuffer);

    dsFilter :
      RecBuf := PRecordData(FFilterBuffer);

    dsOldValue :
      begin
        i := FCurRec;
        if i < 0 then
          i := 0;
        if i < Records.Count then
          RecBuf := TTextRecord(Records.Items[i]).Data
        else
          RecBuf := nil;
      end;
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

function TrmMemoryDataSet.GetFieldData(Field : TField; Buffer : Pointer) : Boolean;
var
  b : PRecordData;
begin
  Result := False;
  if not GetActiveRecBuf(b) then Exit;
  if b^.Bytes[FieldOffset(Field)] = 0 then
  begin
    if Buffer <> nil then
      Move(b^.Bytes[FieldOffset(Field) + 1], Buffer^, Field.DataSize);
    Result := True;
  end;
end;

procedure TrmMemoryDataSet.SetFieldData(Field : TField; Buffer : Pointer);
var
  b : PRecordData;
begin
  if not GetActiveRecBuf(b) then
    Exit;
  if State in [dsEdit, dsInsert] then
    Field.Validate(Buffer);

  if Buffer = nil then
    b^.Bytes[FieldOffset(Field)] := 1
  else
  begin
    b^.Bytes[FieldOffset(Field)] := 0;

    Move(Buffer^, b^.Bytes[FieldOffset(Field) + 1], Field.DataSize);
  end;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));
end;

procedure TrmMemoryDataSet.InternalFirst;
begin
  FCurRec := -1;
end;

procedure TrmMemoryDataSet.InternalLast;
begin
  FCurRec := Records.Count;
end;


procedure TrmMemoryDataSet.SaveRecordData(Buffer : PRecordData; Index : Integer);
var
  b : PRecordData;
  Idx : LongInt;
  blobStore : TBlobArray;

begin
  b := TTextRecord(Records.Items[Index]).Data;
  begin
    blobStore := b^.Blobs;

    Move(Buffer^, b^, FRecBufSize);
    for Idx := 0 to 10 do
      if Buffer^.Blobs[Idx].BlobData <> blobStore[Idx].BlobData then
      begin
        if blobStore[Idx].BlobData <> nil then
          freeMem(blobStore[Idx].BlobData);

        GetMem(b^.Blobs[Idx].BlobData, Buffer^.Blobs[Idx].BlobSize);
        Move(Buffer^.Blobs[Idx].BlobData^, b^.Blobs[Idx].BlobData^, Buffer^.Blobs[Idx].BlobSize);
        b^.Blobs[Idx].BlobSize := Buffer^.Blobs[Idx].BlobSize;
        b^.Blobs[Idx].FieldNum := Buffer^.Blobs[Idx].FieldNum;
      end
  end;
end;

procedure TrmMemoryDataSet.InternalUpdate;
var
  b : PRecordData;

begin
  if not GetActiveRecBuf(b) then
    Exit;
  // Update the record in the collection
  SaveRecordData(b, FCurRec);
end;

procedure TrmMemoryDataSet.InternalPost;
var
  b : PRecordData;

begin
  if State = dsEdit then
    InternalUpdate
  else
  begin
    GetActiveRecBuf(b);
    InternalAddRecord(b, False);
  end;
end;

procedure TrmMemoryDataSet.InternalAddRecord(Buffer : Pointer; Append : Boolean);
var
  b : PRecordData;
  r : TTextRecord;

begin
  if not GetActiveRecBuf(b) then
    Exit;
  if b <> Buffer then
    raise Exception.Create('InternalAddRecord: b <> buffer');

  if Append then
    InternalLast;

  r := TTextRecord.Create(Records);
  if FCurRec >= 0 then
    r.Index := FCurRec;

  r.Data := PRecordData(AllocRecordBuffer);
  SaveRecordData(b, r.Index);
  Inc(FLastBookmark);
  r.Data^.Bookmark := FLastBookmark;
  r.Data^.BookmarkFlag := bfCurrent;
end;

procedure TrmMemoryDataSet.InternalDelete;
var
  b : PRecordData;

begin
  if not GetActiveRecBuf(b) then
    Exit;
  Records.Items[FCurRec].Free;
end;

function TrmMemoryDataSet.GetRecordCount : Longint;
begin
  Result := Records.Count;
end;

function TrmMemoryDataSet.GetRecNo : Longint;
begin
  UpdateCursorPos;
  if (FCurRec = -1) and (RecordCount > 0) then
    Result := 1
  else
    Result := FCurRec + 1;
end;

procedure TrmMemoryDataSet.SetRecNo(Value : Integer);
begin
  if (Value >= 0) and (Value < Records.Count) then
  begin
    FCurRec := Value - 1;
    Resync([]);
  end;
end;

function TrmMemoryDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  if (PInteger(Bookmark1)^ = PInteger(Bookmark2)^) then
     Result := 0
  else if (PInteger(Bookmark1)^ > PInteger(Bookmark2)^) then
     Result := 1
  else
     Result := -1
end;


procedure TrmMemoryDataSet.Sort(field : TField; direction : TSortType);
var
  buffer : pointer;
  dir : boolean;

  function GetRecordValue(index : integer) : variant;
  var
    SaveState : TDataSetState;
    oldFilter : pointer;

  begin
    fCurRec := index;
    GetRecord(buffer, gmCurrent, FALSE); // Get the actual record

    oldFilter := FFilterBuffer; // Save off the old filter buffer
    SaveState := SetTempState(dsFilter); // Tell the dataset we are filtering
    try
      FFilterBuffer := pointer(buffer); // Point the filter buf to the supplied buf
      result := field.Value;
    finally
      RestoreState(SaveState); // Put the state back the way it was
      FFilterBuffer := oldFilter; // Put the old filter buffer back
    end;
  end;

  function CompareCell(c : integer; s2 : variant) : double;
  begin
    case field.DataType of
      ftString : result := CompareStr(GetRecordValue(c), s2);
    else
      result := GetRecordValue(c) - s2;
    end;

    if dir then result := -result;
  end;

  procedure QuickSort(iLo, iHi : Integer);
  var
    Lo, Hi : Integer;
    mid : variant;
    tempRec : PRecordData;

  begin
    Lo := iLo;
    Hi := iHi;
    Mid := GetRecordValue((Lo + Hi) div 2);

    repeat

      while (lo < RecordCount) and (CompareCell(Lo, Mid) < 0) do
        Inc(Lo);
      while (hi >= 0) and (CompareCell(Hi, Mid) > 0) do
        Dec(Hi);

      if Lo <= Hi then
      begin
        if lo <> hi then
        begin
          tempRec := TTextRecord(Records.Items[lo]).Data;
          TTextRecord(records.Items[lo]).Data := TTextRecord(records.Items[hi]).Data;
          TTextRecord(records.Items[hi]).Data := tempRec;
        end;

        inc(lo);
        dec(hi);
      end;

    until Lo > Hi;

    if Hi > iLo then QuickSort(iLo, Hi);
    if Lo < iHi then QuickSort(Lo, iHi);
  end;

begin
  if (field.IsBlob) then
    raise Exception.Create('Sorting not supported on blob fields (Field: ' + field.name + ').');

  case direction of
    stAscending : dir := FALSE;
    stDescending : dir := TRUE;
    stAlternate :
      begin
        dir := fSortOrder[field.Index];
        fSortOrder[field.Index] := not fSortOrder[field.Index];
      end;
  end;

  DisableControls;
  try
    GetMem(buffer, GetRecBufSize);
    try
      QuickSort(0, RecordCount - 1);
    except
    end;
  finally
    FreeMem(buffer);

    EnableControls;
    First;
  end;
end;

{ TFieldDefList }

function TFieldDefList.Add : TFieldDefItem;
begin
  Result := TFieldDefItem(inherited Add);
end;

constructor TFieldDefList.Create(AOwner : TComponent);
begin
  inherited Create(TFieldDefItem);
  FInternalOwner := AOwner;
end;

function TFieldDefList.GetItem(Index : Integer) : TFieldDefItem;
begin
  Result := TFieldDefItem(inherited GetItem(Index));
end;

procedure TFieldDefList.SetItem(Index : Integer; Value : TFieldDefItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TrmMemoryDataSet.CloseBlob(Field : TField);
begin
  FreeBlob(PRecordData(ActiveBuffer));
end;

function TrmMemoryDataSet.CreateBlobStream(Field : TField;
  Mode : TBlobStreamMode) : TStream;
begin
  Result := TResultSetBlobStream.Create(Field as TBlobField, Mode);
end;

function VarEquals(const V1, V2 : Variant) : Boolean;
begin
  Result := False;
  try
    Result := V1 = V2;
  except
  end;
end;

function TrmMemoryDataSet.Lookup(const KeyFields : string;
  const KeyValues : Variant; const ResultFields : string) : Variant;
var
  b : TBookMark;
  keyf : TField;

begin
  result := FALSE;

  b := GetBookmark;
  DisableControls;
  try
    keyf := fieldByName(KeyFields);
    first;
    while not eof do
    begin
      if VarEquals(keyf.value, keyValues) then
      begin
        result := fieldByName(resultFields).Value;
        break;
      end;

      moveBy(1);
    end;
  finally
    GotoBookmark(b);
    EnableControls;
  end;
end;

function TrmMemoryDataSet.Locate(const KeyFields : string;
  const KeyValues : Variant; Options : TLocateOptions) : Boolean;
var
  b : TBookMark;
  keyf : TField;

begin
  result := FALSE;

  b := GetBookmark;
  DisableControls;
  try
    keyf := fieldByName(KeyFields);
    first;
    while not eof do
    begin
      if VarEquals(keyf.value, KeyValues) then
      begin
        result := TRUE;
        b := nil;
        break;
      end;

      MoveBy(1);
    end;
  finally
    if b <> nil then
      GotoBookmark(b);
    EnableControls;
  end;
end;

{ TFieldDefItem }

procedure TFieldDefItem.Assign(Source: TPersistent);
begin
  if (Source is TFieldDefItem) then
  begin
    FName := TFieldDefItem(Source).Name;
    FSize := TFieldDefItem(Source).Size;
    FFieldType := TFieldDefItem(Source).FieldType;
  end
  else
    inherited Assign(Source);
end;

initialization

  RegisterClass(TrmLongStringField);

end.

