
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit Storage;

interface

{$I STD.INC}

uses
  Classes, ComObj, SysUtils, ActiveX, Windows;

{ EStorageError exception }

type
  EStorageError = class(Exception);

{ TStructuredStorage class }

  TStructureKind = (skFolder, skStream, skLockBytes, skProperty);
  TWideName = array[0..MAX_PATH] of WideChar;

  TFolderStructure = class;
  TStreamStructure = class;

  TStructuredStorage = class(TPersistent)
  private
    FParent: TFolderStructure;
    FData: Pointer;
    FKind: TStructureKind;
    FName: TWideName;
    FUnknown: IUnknown;
    function GetLevel: Integer;
    function GetOpened: Boolean;
    procedure SetOpened(Value: Boolean);
    function GetName: string;
    procedure SetName(const Value: string);
  protected
    function MoveElement(ElementName: string; Folder: TFolderStructure;
      Copy: Boolean): string;
    function GetWideName: PWideChar;
    procedure SetWideName(const Value: string);
    property Unknown: IUnknown read FUnknown write FUnknown;
  public
    destructor Destroy; override;
    procedure Close; virtual;
    function CopyTo(Folder: TFolderStructure;
      const NewName: string = ''): TStructuredStorage;
    procedure Delete;
    procedure MoveTo(Folder: TFolderStructure; const NewName: string = '');
    procedure Open; virtual;
    property Data: Pointer read FData write FData;
    property Opened: Boolean read GetOpened write SetOpened;
    property Parent: TFolderStructure read FParent;
    property Kind: TStructureKind read FKind;
    property Level: Integer read GetLevel;
    property Name: string read GetName write SetName;
  end;

{ TFolderStructure class }

  TFolderStructure = class(TStructuredStorage)
  private
    FList: TList;
    FUpdateRef: Integer;
    function GetStructureCount: Integer;
    function GetStructure(Index: Integer): TStructuredStorage;
  protected
    procedure Clear;
    function InternalAdd(const S: string;
      Kind: TStructureKind): TStructuredStorage;
    procedure InternalRemove(Structure: TStructuredStorage);
    procedure Update;
  public
    constructor Create(const FileName: string = '');
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function Add(const S: string; Kind: TStructureKind): TStructuredStorage;
    function AddFolder(const S: string): TFolderStructure;
    function AddStream(const S: string): TStreamStructure;
    procedure Close; override;
    function GetUniqueName(const S: string = ''): string;
    function Find(const S: string; out Structure: TStructuredStorage): Boolean;
    function FindFolder(const S: string): TFolderStructure;
    function FindStream(const S: string): TStreamStructure;
    function IndexOf(const S: string): Integer; overload;
    function IndexOf(Structure: TStructuredStorage): Integer; overload;
    procedure Open; override;
    function OpenFolder(const S: string; CanCreate: Boolean = True): TFolderStructure;
    function OpenStream(const S: string; CanCreate: Boolean = True): TStreamStructure;
    property StructureCount: Integer read GetStructureCount;
    property Structure[Index: Integer]: TStructuredStorage read GetStructure; default;
  end;

{ TStreamStructure class }

  TConversionMode = (cmReset, cmCurrent);

  TStreamStructure = class(TStructuredStorage)
  private
    FMode: TConversionMode;
    FRefCount: Integer;
    FStream: TStream;
    procedure CheckMode;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(Value: Boolean);
    function GetAsFloat: Double;
    procedure SetAsFloat(Value: Double);
    function GetAsInteger: Integer;
    procedure SetAsInteger(Value: Integer);
    function GetAsStream: TStream;
    procedure SetAsStream(Value: TStream);
    function GetAsString: string;
    procedure SetAsString(const S: string);
    function GetInternalStream: IStream;
    function GetSize: Integer;
    procedure SetSize(Value: Integer);
  protected
    procedure SafeRead(Data: Pointer; Count: Integer);
    procedure SafeWrite(Data: Pointer; Count: Integer);
    property InternalStream: IStream read GetInternalStream;
  public
    destructor Destroy; override;
    procedure Close; override;
    procedure Open; override;
    procedure Append;
    function Write(var Data; Count: Integer): Integer;
    function Read(var Data; Count: Integer): Integer;
    procedure Reset;
    procedure Rewrite;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsStream: TStream read GetAsStream write SetAsStream;
    property AsString: string read GetAsString write SetAsString;
    property Mode: TConversionMode read FMode write FMode;
    property Size: Integer read GetSize write SetSize;
  end;

{ TReverseStreamAdapter class }

  TReverseStreamAdapter = class(TStream)
  private
    FStream: IStream;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(Stream: IStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

implementation

uses
  StrConst;

{ TStructuredStorage }

const
  STGM_DEFAULTMODE = STGM_READWRITE or STGM_SHARE_EXCLUSIVE;

destructor TStructuredStorage.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TStructuredStorage.Close;
begin
  FUnknown := nil;
end;

function TStructuredStorage.CopyTo(Folder: TFolderStructure;
  const NewName: string = ''): TStructuredStorage;
begin
  Result := Folder.InternalAdd(MoveElement(NewName, Folder, True), Kind);
end;

procedure TStructuredStorage.Delete;
begin
  if FParent = nil then
    raise EStorageError.Create(SNoParentStructure);
  FParent.InternalRemove(Self);
  Destroy;
end;

function TStructuredStorage.GetWideName: PWideChar;
begin
  Result := @FName;
end;

function TStructuredStorage.MoveElement(ElementName: string;
  Folder: TFolderStructure; Copy: Boolean): string;
const
  MoveOperation: array[Boolean] of Integer = (STGMOVE_MOVE, STGMOVE_COPY);
var
  WideName: TWideName;
  SourceStorage: IStorage;
  DestStorage: IStorage;
begin
  if FParent = nil then
    raise EStorageError.Create(SNoParentStructure);
  if ElementName = '' then
    Result := ElementName
  else
    Result := Name;
  Close;
  StringToWideChar(Result, WideName, SizeOf(TWideName) div SizeOf(WideChar));
  SourceStorage := FParent.Unknown as IStorage;
  Folder.Open;
  DestStorage := Folder.Unknown as IStorage;
  if not (SourceStorage.MoveElementTo(GetWideName, DestStorage,
    WideName, MoveOperation[Copy]) = S_OK) then
    raise EStorageError.Create(SCannotPerformOperation);
end;

procedure TStructuredStorage.MoveTo(Folder: TFolderStructure;
  const NewName: string = '');
var
  NarrowName: string;
begin
  NarrowName := MoveElement(NewName, Folder, True);
  FParent.InternalRemove(Self);
  FParent := Folder;
  FParent.FList.Add(Self);
  SetWideName(NarrowName);
  FParent.Update;
end;

procedure TStructuredStorage.Open;
begin
  raise EStorageError.Create(SAbstractStructure);
end;

procedure TStructuredStorage.SetWideName(const Value: string);
begin
  StringToWideChar(Value, FName, SizeOf(FName) div 2);
end;

function TStructuredStorage.GetOpened: Boolean;
begin
  Result := FUnknown <> nil;
end;

procedure TStructuredStorage.SetOpened(Value: Boolean);
begin
  if Value <> Opened then
    if Value then
      Open
    else
      Close;
end;

function TStructuredStorage.GetLevel: Integer;
begin
  Result := 0;
  if FParent <> nil then
    Result := FParent.Level + 1;
end;

function TStructuredStorage.GetName: string;
begin
  Result := WideCharToString(FName);
end;

procedure TStructuredStorage.SetName(const Value: string);
var
  WideBuffer: array[0..MAX_PATH] of WideChar;
  Storage: IStorage;
begin
  if FParent = nil then
    raise EStorageError.Create(SNoParentStructure);
  Storage := FParent.Unknown as IStorage;
  if not (Storage.RenameElement(FName, StringToWideChar(Value, WideBuffer,
    MAX_PATH)) = S_OK) then
    raise EStorageError.CreateFmt(SInvalidStructureName, [Value]);
  SetWideName(Value);
end;

{ TFolderStructure }

constructor TFolderStructure.Create(const FileName: string = '');
begin
  inherited Create;
  FList := TList.Create;
  if FileName <> '' then
  begin
    SetWideName(FileName);
    Open;
  end;
end;

destructor TFolderStructure.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TFolderStructure.Add(const S: string; Kind: TStructureKind): TStructuredStorage;
begin
  Open;
  if IndexOf(S) > -1 then
    raise EStorageError.Create(SDuplicateName);
  Result := InternalAdd(S, Kind);
  try
    Result.Open;
    Result.Close;
  except
    FList.Remove(Result);
    FreeAndNil(Result);
    raise;
  end;
end;

function TFolderStructure.AddFolder(const S: string): TFolderStructure;
begin
  Result := Add(S, skFolder) as TFolderStructure;
end;

function TFolderStructure.AddStream(const S: string): TStreamStructure;
begin
  Result := Add(S, skStream) as TStreamStructure;
end;

function TFolderStructure.Find(const S: string; out Structure: TStructuredStorage): Boolean;
var
  I: Integer;
begin
  Open;
  I := IndexOf(S);
  Result := I > -1;
  if Result then
    Structure := Self.Structure[I]
  else
    Structure := nil;
end;

function TFolderStructure.FindFolder(const S: string): TFolderStructure;
var
  Structure: TStructuredStorage;
begin
  if Find(S, Structure) and (Structure is TFolderStructure) then
    Result := Structure as TFolderStructure
  else
    Result := nil;
end;

function TFolderStructure.FindStream(const S: string): TStreamStructure;
var
  Structure: TStructuredStorage;
begin
  if Find(S, Structure) and (Structure is TStreamStructure) then
    Result := Structure as TStreamStructure
  else
    Result := nil;
end;

procedure TFolderStructure.BeginUpdate;
begin
  Inc(FUpdateRef);
end;

procedure TFolderStructure.Clear;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    TObject(FList.Items[I]).Free;
  FList.Clear;
end;

procedure TFolderStructure.Close;
begin
  if FList <> nil then
    Clear;
  inherited Close;
end;

procedure TFolderStructure.EndUpdate;
begin
  if FUpdateRef > 0 then
    Dec(FUpdateRef);
  Update;
end;

function TFolderStructure.GetUniqueName(const S: string = ''): string;
var
  I: Integer;
begin
  Open;
  Result := S;
  if Result = '' then
    Result := 'Item';
  if IndexOf(Result) > 0 then
  begin
    I := 0;
    repeat
      Inc(I);
    until IndexOf(Result + IntToStr(I)) > 0;
    Result := Result + IntToStr(I);
  end;
end;

function TFolderStructure.IndexOf(const S: string): Integer;
var
  ItemName: string;
  I: Integer;
begin
  Result := -1;
  ItemName := UpperCase(S);
  for I := 0 to FList.Count - 1 do
    if ItemName = UpperCase(TStructuredStorage(FList[I]).Name) then
    begin
      Result := I;
      Break;
    end;
end;

function TFolderStructure.IndexOf(Structure: TStructuredStorage): Integer;
begin
  Result := FList.IndexOf(Structure);
end;

function TFolderStructure.InternalAdd(const S: string;
  Kind: TStructureKind): TStructuredStorage;
begin
  Result := nil;
  case Kind of
    skFolder: Result := TFolderStructure.Create;
    skStream: Result := TStreamStructure.Create;
  end;
  if Result <> nil then
  begin
    Result.FKind := Kind;
    Result.FParent := Self;
    Result.SetWideName(S);
    FList.Add(Result);
    Update;
  end
  else
    raise EStorageError.Create(SAbstractStructure);
end;

procedure TFolderStructure.Open;
var
  ParentStorage: IStorage;
  Storage: IStorage;
  EnumStatStg: IEnumStatStg;
  StatStg: TStatStg;
  Dummy: Integer;
  S: string;
begin
  if FUnknown = nil then
  try
    BeginUpdate;
    if Level = 0 then
    begin
      if FileExists(Name) then
        if not (StgOpenStorage(GetWideName, nil, STGM_DEFAULTMODE, nil,
          0, Storage) = S_OK) then
           raise EStorageError.Create(SNoOpenStructure)
         else
      else if not (StgCreateDocfile(GetWideName, STGM_CREATE or
        STGM_DEFAULTMODE, 0, Storage) = S_OK) then
        raise EStorageError.Create(SNoOpenStructure)
    end
    else
    begin
      ParentStorage := FParent.Unknown as IStorage;
      if not (ParentStorage.OpenStorage(GetWideName, nil, STGM_DEFAULTMODE,
        nil, 0, Storage) = S_OK) then
        if not (ParentStorage.CreateStorage(GetWideName, STGM_CREATE or
          STGM_DEFAULTMODE, 0, 0, Storage) = S_OK) then
          raise EStorageError.Create(SNoOpenStructure);
    end;
    OleCheck(Storage.EnumElements(0, nil, 0, EnumStatStg));
    while EnumStatStg.Next(1, StatStg, @Dummy) = S_OK do
    try
      if StatStg.pwcsName = nil then
        Continue;
      S := WideCharToString(StatStg.pwcsName);
      case StatStg.dwType of
        STGTY_STORAGE: InternalAdd(S, skFolder);
        STGTY_STREAM: InternalAdd(S, skStream);        STGTY_LOCKBYTES: InternalAdd(S, skLockBytes);        STGTY_PROPERTY: InternalAdd(S, skProperty);      end;
    finally
      CoTaskMemFree(StatStg.pwcsName);
    end;
    Unknown := Storage as IUnknown;
  finally
    EndUpdate;
  end;
end;

procedure TFolderStructure.InternalRemove(Structure: TStructuredStorage);
var
  Storage: IStorage;
  I: Integer;
begin
  if Unknown <> nil then
  begin
    Storage := Unknown as IStorage;
    I := FList.IndexOf(Structure);
    if I > -1 then
    begin
      OleCheck(Storage.DestroyElement(Structure.GetWideName));
      FList.Remove(Structure);
    end;
  end;
end;

function TFolderStructure.OpenFolder(const S: string;
  CanCreate: Boolean = True): TFolderStructure;
begin
  Result :=  FindFolder(S);
  if Result = nil then
    if CanCreate then
      Result := AddFolder(S)
    else
      raise EStorageError.Create(SCannotOpenStructure);
end;

function TFolderStructure.OpenStream(const S: string;
  CanCreate: Boolean = True): TStreamStructure;
begin
  Result := FindStream(S);
  if Result = nil then
    if CanCreate then
      Result := AddStream(S)
    else
      raise EStorageError.Create(SCannotOpenStructure);
end;

function StructureSort(Item1, Item2: Pointer): Integer;
var
  LeftStruct: TStructuredStorage absolute Item1;
  RightStruct: TStructuredStorage absolute Item2;
begin
  if LeftStruct.Kind <> RightStruct.Kind then
    Result := Ord(LeftStruct.Kind) - Ord(RightStruct.Kind)
  else if UpperCase(LeftStruct.Name) > UpperCase(RightStruct.Name) then
    Result := 1
  else if UpperCase(LeftStruct.Name) < UpperCase(RightStruct.Name) then
    Result := -1
  else
    Result := 0;
end;

procedure TFolderStructure.Update;
begin
  if FUpdateRef = 0 then
    if FList <> nil then
      FList.Sort(StructureSort);
end;

function TFolderStructure.GetStructureCount: Integer;
begin
  Result := FList.Count;
end;

function TFolderStructure.GetStructure(Index: Integer): TStructuredStorage;
begin
  Result := FList[Index];
end;

{ TStreamStructureAdapter }

type
  TStreamStructureAdapter = class(TReverseStreamAdapter)
  private
    FStructure: TStreamStructure;
  public
    constructor Create(Structure: TStreamStructure);
    destructor Destroy; override;
  end;

{ TStreamStructure }

var
  Dummy: Largeint;

destructor TStreamStructure.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TStreamStructure.Close;
begin
  Dec(FRefCount);
  if FRefCount = 0 then
    inherited Close;
end;

procedure TStreamStructure.Append;
var
  Stream: IStream;
  Move: Largeint;
begin
  Stream := InternalStream;
  Move := 0;
  OleCheck(Stream.Seek(Move, STREAM_SEEK_END, Dummy));
end;

procedure TStreamStructure.CheckMode;
begin
  if Mode = cmReset then
    Size := 0;
end;

procedure TStreamStructure.Open;
var
  ParentStorage: IStorage;
  Stream: IStream;
begin
  if FRefCount < 1 then
  begin
    FRefCount := 0;
    ParentStorage := FParent.Unknown as IStorage;
    if not (ParentStorage.OpenStream(GetWideName, nil,
      STGM_DEFAULTMODE, 0, Stream) = S_OK) then
      if not (ParentStorage.CreateStream(GetWideName, STGM_CREATE or
        STGM_DEFAULTMODE, 0, 0, Stream) = S_OK) then
        raise EStorageError.Create(SNoOpenStructure);
    Unknown := Stream as IUnknown;
  end;
  Inc(FRefCount);
end;

function TStreamStructure.Read(var Data; Count: Integer): Integer;
var
  Stream: IStream;
begin
  Stream := InternalStream;
  OleCheck(Stream.Read(@Data, Count, @Result));
end;

procedure TStreamStructure.SafeRead(Data: Pointer; Count: Integer);
var
  Stream: IStream;
  Bytes: Integer;
begin
  Stream := InternalStream;
  OleCheck(Stream.Read(Data, Count, @Bytes));
  if Bytes <> Count then
    raise EStorageError.Create(SSafeReadWriteFail);
end;

procedure TStreamStructure.Rewrite;
var
  Stream: IStream;
  Move: Largeint;
  NewSize: Largeint;
begin
  Stream := InternalStream;
  Move := 0;
  OleCheck(Stream.Seek(Move, STREAM_SEEK_SET, Dummy));
  NewSize  := 0;
  OleCheck(Stream.SetSize(NewSize));
end;

procedure TStreamStructure.Reset;
var
  Stream: IStream;
  Move: Largeint;
begin
  Stream := InternalStream;
  Move := 0;
  Stream.Seek(Move, STREAM_SEEK_SET, Dummy);
end;

function TStreamStructure.Write(var Data; Count: Integer): Integer;
var
  Stream: IStream;
begin
  Stream := InternalStream;
  OleCheck(Stream.Write(@Data, Count, @Result));
end;

procedure TStreamStructure.SafeWrite(Data: Pointer; Count: Integer);
var
  Stream: IStream;
  Bytes: Integer;
begin
  Stream := InternalStream;
  OleCheck(Stream.Write(Data, Count, @Bytes));
  if Bytes <> Count then
    raise EStorageError.Create(SSafeReadWriteFail);
end;

function TStreamStructure.GetAsBoolean: Boolean;
begin
  Open;
  try
    SafeRead(@Result, SizeOf(Result));
  finally
    Close;
  end;
end;

procedure TStreamStructure.SetAsBoolean(Value: Boolean);
begin
  Open;
  try
    CheckMode;
    SafeWrite(@Value, SizeOf(Value));
  finally
    Close;
  end;
end;

function TStreamStructure.GetAsFloat: Double;
begin
  Open;
  try
    SafeRead(@Result, SizeOf(Result));
  finally
    Close;
  end;
end;

procedure TStreamStructure.SetAsFloat(Value: Double);
begin
  Open;
  try
    CheckMode;
    SafeWrite(@Value, SizeOf(Value));
  finally
    Close;
  end;
end;

function TStreamStructure.GetAsInteger: Integer;
begin
  Open;
  try
    SafeRead(@Result, SizeOf(Result));
  finally
    Close;
  end;
end;

procedure TStreamStructure.SetAsInteger(Value: Integer);
begin
  Open;
  try
    CheckMode;
    SafeWrite(@Value, SizeOf(Value));
  finally
    Close;
  end;
end;

function TStreamStructure.GetAsStream: TStream;
begin
  if FStream = nil then
    FStream := TStreamStructureAdapter.Create(Self);
  Result := FStream;
end;

procedure TStreamStructure.SetAsStream(Value: TStream);
var
  Stream: TStream;
  WasActive: Boolean;
begin
  Stream := FStream;
  WasActive := Stream <> nil;
  if not WasActive then
    Stream := TStreamStructureAdapter.Create(Self);
  Stream.Size := 0;
  Stream.CopyFrom(Value, 0);
  if WasActive then
    Stream.Free;
end;

function TStreamStructure.GetAsString: string;
var
  I: Integer;
begin
  Open;
  try
    SafeRead(@I, SizeOf(Integer));
    if I > 0 then
    begin
      SetLength(Result, I);
      SafeRead(Pointer(Result), I);
    end
    else
      Result := '';
  finally
    Close;
  end;
end;

procedure TStreamStructure.SetAsString(const S: string);
var
  I: Integer;
begin
  Open;
  try
    CheckMode;
    I := Length(S);
    SafeWrite(@I, SizeOf(Integer));
    if I > 0 then
      SafeWrite(Pointer(S), I);
  finally
    Close;
  end;
end;

function TStreamStructure.GetInternalStream: IStream;
begin
  if Unknown = nil then
    raise EStorageError.Create(SNoOpenStructure);
  Result := Unknown as IStream;
end;

function TStreamStructure.GetSize: Integer;
var
  Stream: IStream;
  Move: Largeint;
  Position: Largeint;
  LargeResult: Largeint;
begin
  Stream := InternalStream;
  Move := 0;
  OleCheck(Stream.Seek(Move, STREAM_SEEK_CUR, Position));
  OleCheck(Stream.Seek(Move, STREAM_SEEK_END, LargeResult));
  OleCheck(Stream.Seek(Position, STREAM_SEEK_SET, Dummy));
  Result := LargeResult;
end;

procedure TStreamStructure.SetSize(Value: Integer);
var
  Stream: IStream;
  NewSize: Largeint;
begin
  Stream := InternalStream;
  NewSize := Value;
  OleCheck(Stream.SetSize(NewSize));
end;

{ TReverseStreamAdapter }

constructor TReverseStreamAdapter.Create(Stream: IStream);
begin
  FStream := Stream;
  inherited Create;
end;

destructor TReverseStreamAdapter.Destroy;
begin
  FStream := nil;
  inherited Destroy;
end;

function TReverseStreamAdapter.Read(var Buffer; Count: Longint): Longint;
begin
  OleCheck(FStream.Read(@Buffer, Count, @Result));
end;

function TReverseStreamAdapter.Write(const Buffer; Count: Longint): Longint;
begin
  OleCheck(FStream.Write(@Buffer, Count, @Result));
end;

function TReverseStreamAdapter.Seek(Offset: Longint; Origin: Word): Longint;
var
  NewPosition: TLargeInteger;
begin
  OleCheck(FStream.Seek(Offset, Origin, NewPosition));
  Result := NewPosition;
end;

procedure TReverseStreamAdapter.SetSize(NewSize: Longint);
begin
  OleCheck(FStream.SetSize(NewSize));
end;

{ TStreamStructureAdapter }

constructor TStreamStructureAdapter.Create(Structure: TStreamStructure);
begin
  FStructure := Structure;
  FStructure.Open;
  inherited Create(FStructure.InternalStream);
end;

destructor TStreamStructureAdapter.Destroy;
begin
  FStructure.Close;
  FStructure.FStream := nil;
  inherited Destroy;
end;

end.
