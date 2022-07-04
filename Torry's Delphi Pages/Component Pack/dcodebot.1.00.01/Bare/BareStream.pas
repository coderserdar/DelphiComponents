
(********************************************************)
(*                                                      *)
(*  Bare Object Library @ www.codebot.org/delphi        *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BareStream;

interface

{$I BARE.INC}

uses
  {$IFDEF BARE}BareUtils,{$ELSE} SysUtils, Classes, ComObj, StrTools,{$ENDIF}
  ActiveX, Windows;


{ TReadMemoryStream }

type
  EStreamFormatError = class(Exception);
  
  TReadMemoryStream = class(TCustomMemoryStream)
  public
    constructor Create(Reference: Pointer; Size: Integer);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

{ IStreamable }

  IStreamable = interface(IUnknown)
    function GetStream: TStream;
    property Stream: TStream read GetStream;
  end;

{ IStreamReader interface }

  IStreamReader = interface(IStreamable)
    function ReadInteger: Integer;
    function ReadString: string;
    function ReadBoolean: Boolean;
    function ReadFloat: Double;
    function ReadVariant: OleVariant;
    procedure ReadInstance(Instance: TObject);
  end;

{ IStreamWriter interface }

  IStreamWriter = interface(IStreamable)
    procedure WriteInteger(I: Integer);
    procedure WriteString(const S: string);
    procedure WriteBoolean(Flag: Boolean);
    procedure WriteVariant(const V: OleVariant);
    procedure WriteInstance(Instance: TObject);
  end;

{ TStreamReaderWriter class }

  TStreamReaderWriter = class(TInterfacedObject, IStreamReader, IStreamWriter)
  private
    FStream: TStream;
    FOwnedStream: Boolean;
  protected
    { IStreamable }
    function GetStream: TStream;
    { IStreamReader }
    function ReadInteger: Integer;
    function ReadString: string;
    function ReadBoolean: Boolean;
    function ReadFloat: Double;
    function ReadVariant: OleVariant;
    procedure ReadInstance(Instance: TObject);
    { IStreamWriter }
    procedure WriteInteger(I: Integer);
    procedure WriteString(const S: string);
    procedure WriteBoolean(Flag: Boolean);
    procedure WriteFloat(const Value: Double);
    procedure WriteVariant(const V: OleVariant);
    procedure WriteInstance(Instance: TObject);
  public
    constructor Create(Stream: TStream); overload;
    constructor Create(Stream: IStream); overload;
    destructor Destroy; override;
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

{ EStorageError class }

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
    function GetFolderCount: Integer;
    function GetStructureCount: Integer;
    function GetStructure(Index: Integer): TStructuredStorage;
  protected
    function InternalAdd(const S: string;
      Kind: TStructureKind): TStructuredStorage;
    procedure InternalRemove(Structure: TStructuredStorage);
    procedure Clear;
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
    property FolderCount: Integer read GetFolderCount;
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
    procedure LoadFromFile(const FileName: string);
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

function StreamFromPath(Folder: TFolderStructure; const Path: string): TStream;

implementation

resourcestring
  SNoParentStructure = 'No parent structure available';
  SCannotPerformOperation = 'Cannot perform operation';
  SAbstractStructure = 'Abstract structure error';
  SInvalidStructureName = '"%s" is not a valid structure name';
  SDuplicateName = 'Duplicate names not allowed';
  SNoOpenStructure = 'Invalid file format';
  SCannotOpenStructure = 'Cannot open structure object';
  SSafeReadWriteFail = 'Safe stream read/write operation failed.';
  SInvalidStructureType = 'Invalid structure type.';
  SInvalidToken = 'Invalid token encountered at row %d column %d';
  SCantWriteStream = 'Can''t write to stream';
  SCantReadStream = 'Can''t read from stream';
  SInvalidStreamFormat = 'Invalid stream resource';

const
  varByteArray = varByte or varArray;

{ TReadMemoryStream }

constructor TReadMemoryStream.Create(Reference: Pointer; Size: Integer);
begin
  inherited Create;
  SetPointer(Reference, Size);
end;

function TReadMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EStreamFormatError.Create(SCantWriteStream);
end;

{ TStreamReaderWriter }

constructor TStreamReaderWriter.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
end;

constructor TStreamReaderWriter.Create(Stream: IStream);
begin
  inherited Create;
  FStream := TReverseStreamAdapter.Create(Stream);
  FOwnedStream := True;
end;

destructor TStreamReaderWriter.Destroy;
begin
  if FOwnedStream then
    FStream.Free;
  inherited Destroy;
end;

{ TStreamReaderWriter.IStreamable }

function TStreamReaderWriter.GetStream: TStream;
begin
  Result := FStream;
end;

{ TStreamReaderWriter.IStreamReader }

function TStreamReaderWriter.ReadInteger: Integer;
begin
  FStream.Read(Result, SizeOf(Integer));
end;

function TStreamReaderWriter.ReadString: string;
var
  I: Integer;
begin
  FStream.Read(I, SizeOf(Integer));
  SetLength(Result, I);
  FStream.Read(Result, I);
end;

function TStreamReaderWriter.ReadBoolean: Boolean;
begin
  FStream.Read(Result, SizeOf(Boolean));
end;

function TStreamReaderWriter.ReadFloat: Double;
begin
  FStream.Read(Result, SizeOf(Double));
end;

function TStreamReaderWriter.ReadVariant: OleVariant;
var
  P: Pointer;
  I: Integer;
begin
  I := ReadInteger;
  if I > 0 then
  begin
    Result := VarArrayCreate([0, I - 1], varByte);
    P := VarArrayLock(Result);
    try
      FStream.Read(P^, I);
    finally
      VarArrayUnlock(Result);
    end;
  end
  else
    Result := Unassigned;
end;

procedure TStreamReaderWriter.ReadInstance(Instance: TObject);
begin
  { TODO: Read }
end;

{ TStreamReaderWriter.IStreamWriter }

procedure TStreamReaderWriter.WriteInteger(I: Integer);
begin
  FStream.Write(I, SizeOf(Integer));
end;

procedure TStreamReaderWriter.WriteString(const S: string);
var
  I: Integer;
begin
  I := Length(S);
  FStream.Write(I, SizeOf(Integer));
  FStream.Write(PChar(S)^, I);
end;

procedure TStreamReaderWriter.WriteBoolean(Flag: Boolean);
begin
  FStream.Write(Flag, SizeOf(Boolean));
end;

procedure TStreamReaderWriter.WriteFloat(const Value: Double);
begin
  FStream.Write(Value, SizeOf(Double));
end;

procedure TStreamReaderWriter.WriteVariant(const V: OleVariant);
var
  P: Pointer;
  I: Integer;
begin
  if (TVarData(V).VType and varByteArray = varByteArray) then
  begin
    I := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;
    P := VarArrayLock(V);
    try
      WriteInteger(I);
      FStream.Write(P^, I);
    finally
      VarArrayUnlock(V);
    end;
  end
  else
    WriteInteger(0);
end;

procedure TStreamReaderWriter.WriteInstance(Instance: TObject);
begin
  { TODO: Write }
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

function GetErrorName(Code: HRESULT): string;
begin
  case Code of
    STG_E_INVALIDFUNCTION: Result := 'STG_E_INVALIDFUNCTION';
    STG_E_FILENOTFOUND: Result := 'STG_E_FILENOTFOUND';
    STG_E_PATHNOTFOUND: Result := 'STG_E_PATHNOTFOUND';
    STG_E_TOOMANYOPENFILES: Result := 'STG_E_TOOMANYOPENFILES';
    STG_E_ACCESSDENIED: Result := 'STG_E_ACCESSDENIED';
    STG_E_INVALIDHANDLE: Result := 'STG_E_INVALIDHANDLE';
    STG_E_INSUFFICIENTMEMORY: Result := 'STG_E_INSUFFICIENTMEMORY';
    STG_E_INVALIDPOINTER: Result := 'STG_E_INVALIDPOINTER';
    STG_E_NOMOREFILES: Result := 'STG_E_NOMOREFILES';
    STG_E_DISKISWRITEPROTECTED: Result := 'STG_E_DISKISWRITEPROTECTED';
    STG_E_SEEKERROR: Result := 'STG_E_SEEKERROR';
    STG_E_WRITEFAULT: Result := 'STG_E_WRITEFAULT';
    STG_E_READFAULT: Result := 'STG_E_READFAULT';
    STG_E_SHAREVIOLATION: Result := 'STG_E_SHAREVIOLATION';
    STG_E_LOCKVIOLATION: Result := 'STG_E_LOCKVIOLATION';
    STG_E_FILEALREADYEXISTS: Result := 'STG_E_FILEALREADYEXISTS';
    STG_E_INVALIDPARAMETER: Result := 'STG_E_INVALIDPARAMETER';
    STG_E_MEDIUMFULL: Result := 'STG_E_MEDIUMFULL';
    STG_E_PROPSETMISMATCHED: Result := 'STG_E_PROPSETMISMATCHED';
    STG_E_ABNORMALAPIEXIT: Result := 'STG_E_ABNORMALAPIEXIT';
    STG_E_INVALIDHEADER: Result := 'STG_E_INVALIDHEADER';
    STG_E_INVALIDNAME: Result := 'STG_E_INVALIDNAME';
    STG_E_UNKNOWN: Result := 'STG_E_UNKNOWN';
    STG_E_UNIMPLEMENTEDFUNCTION: Result := 'STG_E_UNIMPLEMENTEDFUNCTION';
    STG_E_INVALIDFLAG: Result := 'STG_E_INVALIDFLAG';
    STG_E_INUSE: Result := 'STG_E_INUSE';
    STG_E_NOTCURRENT: Result := 'STG_E_NOTCURRENT';
    STG_E_REVERTED: Result := 'STG_E_REVERTED';
    STG_E_CANTSAVE: Result := 'STG_E_CANTSAVE';
    STG_E_OLDFORMAT: Result := 'STG_E_OLDFORMAT';
    STG_E_OLDDLL: Result := 'STG_E_OLDDLL';
    STG_E_SHAREREQUIRED: Result := 'STG_E_SHAREREQUIRED';
    STG_E_NOTFILEBASEDSTORAGE: Result := 'STG_E_NOTFILEBASEDSTORAGE';
    STG_E_EXTANTMARSHALLINGS: Result := 'STG_E_EXTANTMARSHALLINGS';
    STG_E_DOCFILECORRUPT: Result := 'STG_E_DOCFILECORRUPT';
    STG_E_BADBASEADDRESS: Result := 'STG_E_BADBASEADDRESS';
    STG_E_INCOMPLETE: Result := 'STG_E_INCOMPLETE';
    STG_E_TERMINATED: Result := 'STG_E_TERMINATED';
    STG_S_CONVERTED : Result := 'STG_S_CONVERTED ';
    STG_S_BLOCK : Result := 'STG_S_BLOCK ';
    STG_S_RETRYNOW : Result := 'STG_S_RETRYNOW ';
    STG_S_MONITORING: Result := 'STG_S_MONITORING';
  else
    Result := 'no code fail';
  end
end;

procedure TStructuredStorage.SetName(const Value: string);
var
  WideBuffer: TWideName;
  Storage: IStorage;
  I: Integer;
begin
  if FParent = nil then
    raise EStorageError.Create(SNoParentStructure);
  for I := 0 to FParent.StructureCount - 1 do
    FParent[I].Close;
  Storage := FParent.Unknown as IStorage;
  FillChar(WideBuffer, SizeOf(WideBuffer), #0);
  StringToWideChar(Value, WideBuffer, MAX_PATH);
  if Storage.RenameElement(@FName, @WideBuffer) = S_OK then
    SetWideName(Value)
  else
    raise EStorageError.CreateFmt(SInvalidStructureName, [Value]);
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
  FList.Free;
  FList := nil;
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
    Result.Free;
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
var
  Storage: IStorage;
begin
  if FList <> nil then
    Clear;
  if Unknown <> nil then
  begin
    Storage := Unknown as IStorage;
    OleCheck(Storage.Commit(STGC_OVERWRITE));
  end;
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
  if IndexOf(Result) > -1 then
  begin
    I := 0;
    repeat
      Inc(I);
    until IndexOf(Result + IntToStr(I)) = -1;
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
      if not (ParentStorage.OpenStorage(GetWideName, nil, STGM_DEFAULTMODE, nil, 0, Storage) = S_OK) then
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

function TFolderStructure.GetFolderCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    if TObject(FList[I]) is TFolderStructure then
      Inc(Result);
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
  if FRefCount > 0 then
  begin
    Dec(FRefCount);
    if FRefCount = 0 then
      inherited Close;
  end;
end;

procedure TStreamStructure.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    AsStream := Stream;
  finally
    Stream.Free;
  end;
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
      STGM_DEFAULTMODE or STGM_DIRECT, 0, Stream) = S_OK) then
      if not (ParentStorage.CreateStream(GetWideName, STGM_CREATE or
        STGM_DEFAULTMODE or STGM_DIRECT, 0, 0, Stream) = S_OK) then
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
  if not WasActive then
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

function StreamFromPath(Folder: TFolderStructure; const Path: string): TStream;
var
  Count: Integer;
  I: Integer;
begin
  Result := nil;
  Count := FieldCount(Path, '\');
  for I := 0 to Count - 1 do
    if I = Count - 1 then
      Result := Folder.FindStream(FieldValue(Path, '\', I)).AsStream
    else
      Folder := Folder.FindFolder(FieldValue(Path, '\', I));
end;

end.
