{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Special system classes                        }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{         TPropInfoList, TPropsFiler (TPropsStorage)    }
{         Copyright (c) 1995, 1997 RX Library           }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgSystem;

interface
uses Windows, SysUtils, TypInfo, Classes, IniFiles{$IFDEF _D3_}, ActiveX{$ENDIF}, Contnrs;

resourcestring
  SListIndexError = 'List index out of bounds (%d)';
  SListCapacityError = 'List capacity out of bounds (%d)';
  SListCountError = 'List count out of bounds (%d)';
  SFCreateError = 'Cannot create file %s';
  SFOpenError = 'Cannot open file %s';
  SClassNotFound = 'Class %s not found';
  SUnknownProperty = 'Property does not exist';

type
  TDesignerSelectionList=TComponentList;
{$IFNDEF _D4_}
  PLongWord = ^LongWord;
  LongWord = Longint;

  POleVariant = ^OleVariant;
  {$IFNDEF _D3_}
  OleVariant = Variant;
  {$ENDIF}

  TSysCharSet = set of Char;
{$ENDIF}

  TCharSet = TSysCharSet;

  TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

  TDayTable = array[1..12] of Word;
  PDayTable = ^TDayTable;

  PIntArray = ^TIntArray;
  TIntArray = array[0..0] of Integer;

  PVariantArray = ^TVariantArray;
  TVariantArray = array[0..0] of Variant;

  TMaxPath = array[0..MAX_PATH - 1] of Char;

  PInstance = ^HINST;

const
  MaxDispArgs = 32;

type
  PNamesArray = ^TNamesArray;
  TNamesArray = array[0..MaxDispArgs - 1] of PWideChar;
  TNames      = array [0..1023] of Char;

  PDispIDs    = ^TDispIDs;
  TDispID     = Longint;
  TDispIDs    = array [0..MaxDispArgs - 1] of TDispID;

  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;

{ TTlsBuffer }
  TTlsBuffer = class
  private
    FBlocks: TList;
    FSize: Integer;
    FTlsIndex: Integer;
    function GetMemory: Pointer;
    function GetTlsValue: Pointer;
    class procedure AddTlsBuffer(ATlsBuffer: TTlsBuffer);
    class procedure RemoveTlsBuffer(ATlsBuffer: TTlsBuffer);
  protected
    function AllocMemory(ASize: Integer): Pointer; virtual;
    procedure DoDetachThreadInternal;
    procedure FreeMemory(P: Pointer); virtual;
  public
    constructor Create(ASize: Integer);
    destructor Destroy; override;
    class procedure DoDetachThread;
    property Memory: Pointer read GetMemory;
    property Size: Integer read FSize;
    property TlsIndex: Integer read FTlsIndex;
  end;

{ TCustomThread }
  TCustomThread = class(TThread)
  protected
    procedure DoExecute; virtual;
    procedure DoHandleException(Sender: TObject); virtual;
    procedure Execute; override;
  end;

{ TCustomWaitThread }
  TCustomWaitThread = class(TCustomThread)
  private
    FEvent: THandle;
    FTimeout: DWord;
  protected
    procedure DoReset; virtual;
    procedure DoTimeout; virtual;
    procedure DoExecute; override;
  public
    constructor Create(CreateSuspended: Boolean; ATimeout: DWord);
    destructor Destroy; override;
    procedure Reset(TerminateThread: Boolean);
    property Timeout: DWord read FTimeout write FTimeout;
  end;

{ TWaitThread }
  TWaitThread = class(TCustomWaitThread)
  private
    FOnReset, FOnTimeout: TNotifyEvent;
  protected
    procedure DoReset; override;
    procedure DoTimeout; override;
  public
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
    property OnTimeout: TNotifyEvent read FOnTimeout write FOnTimeout;
  end;

{ TThreadEx }
  TThreadEx = class(TCustomThread)
  private
    FOnExecute, FOnException: TNotifyEvent;
  protected
    procedure DoExecute; override;
    procedure DoHandleException(Sender: TObject); override;
  public
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnException: TNotifyEvent read FOnException write FOnException;
  end;

{ TCustomMessageThread }
  TCustomMessageThread = class(TCustomThread)
  protected
    procedure DoAfterMessage(const Msg: TMsg; const RetValue: Integer); virtual;
    procedure DoBeforeMessage(var Msg: TMsg; var Handled: Boolean); virtual;
    procedure DoExecute; override;
  public
    destructor Destroy; override;
    procedure PostMessage(Msg: UINT; wParam: WPARAM; lParam: LPARAM);
    procedure PostQuitMessage;
    procedure WaitForQuit;
  end;

{ TMessageThread }
  TAfterMessageEvent = procedure (const  Msg: TMsg; const RetValue: Integer) of object;
  TBeforeMessageEvent = procedure (var Msg: TMsg; var Handled: Boolean) of object;

  TMessageThread = class(TCustomMessageThread)
  private
    FOnAfterMessage: TAfterMessageEvent;
    FOnBeforeMessage: TBeforeMessageEvent;
  protected
    procedure DoAfterMessage(const Msg: TMsg; const RetValue: Integer); override;
    procedure DoBeforeMessage(var Msg: TMsg; var Handled: Boolean); override;
  public
    property OnAfterMessage: TAfterMessageEvent read FOnAfterMessage write FOnAfterMessage;
    property OnBeforeMessage: TBeforeMessageEvent read FOnBeforeMessage write FOnBeforeMessage;
  end;

{ TPropInfoList }
  TPropInfoList = class(TObject)
  private
    FList: PPropList;
    FCount: Integer;
    FSize: Integer;
    function Get(Index: Integer): PPropInfo;
  public
    constructor Create(AObject: TObject; Filter: TTypeKinds);
    destructor Destroy; override;
    function Find(const AName: string): PPropInfo;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
    property Items[Index: Integer]: PPropInfo read Get; default;
  end;

{$IFNDEF _D4_}
  TCustomIniFile = TIniFile;
{$ENDIF}

{$IFDEF _D4_}
{ TPropsFiler }
  TPropsFiler = class(TObject)
  private
    FIniFile: TCustomIniFile;
    FObject: TObject;
    FOwner: TComponent;
    FPrefix: string;
    FSection: string;
    function StoreIntegerProperty(PropInfo: PPropInfo): string;
    function StoreCharProperty(PropInfo: PPropInfo): string;
    function StoreEnumProperty(PropInfo: PPropInfo): string;
    function StoreFloatProperty(PropInfo: PPropInfo): string;
    function StoreStringProperty(PropInfo: PPropInfo): string;
    function StoreSetProperty(PropInfo: PPropInfo): string;
    function StoreClassProperty(PropInfo: PPropInfo): string;
    function StoreStringsProperty(PropInfo: PPropInfo): string;
    function StoreComponentProperty(PropInfo: PPropInfo): string;
    function StoreLStringProperty(PropInfo: PPropInfo): string;
    function StoreWCharProperty(PropInfo: PPropInfo): string;
    function StoreVariantProperty(PropInfo: PPropInfo): string;
    procedure LoadLStringProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadWCharProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadVariantProperty(const S: string; PropInfo: PPropInfo);
    function StoreInt64Property(PropInfo: PPropInfo): string;
    procedure LoadInt64Property(const S: string; PropInfo: PPropInfo);
    procedure LoadIntegerProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadCharProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadEnumProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadFloatProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadStringProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadSetProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadClassProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadStringsProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadComponentProperty(const S: string; PropInfo: PPropInfo);
    function CreateInfoList(AComponent: TComponent; StoredList: TStrings): TStrings;
    procedure FreeInfoLists(Info: TStrings);
  protected
    function ReadString(const ASection, Item, Default: string): string; virtual;
    procedure WriteString(const ASection, Item, Value: string); virtual;
    procedure EraseSection(const ASection: string); virtual;
    function GetItemName(const APropName: string): string; virtual;
    function CreateStorage: TPropsFiler; virtual;
  public
    constructor Create(AIniFile: TCustomIniFile; const ASection: string);
    procedure StoreAnyProperty(PropInfo: PPropInfo);
    procedure LoadAnyProperty(PropInfo: PPropInfo);
    procedure StoreProperties(PropList: TStrings);
    procedure LoadProperties(PropList: TStrings);
    procedure LoadObjectsProps(AComponent: TComponent; StoredList: TStrings);
    procedure StoreObjectsProps(AComponent: TComponent; StoredList: TStrings);
    property AObject: TObject read FObject write FObject;
    property Prefix: string read FPrefix write FPrefix;
    property Section: string read FSection;
  end;
{$ENDIF}

{ TvgThreadList }
  TvgThreadList = class
  private
{$IFDEF _D4_}
    FLock: TMultiReadExclusiveWriteSynchronizer;
{$ELSE}
    FLock: TRTLCriticalSection;
{$ENDIF}
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginRead;
    procedure EndRead;
    procedure BeginWrite;
    procedure EndWrite;
    procedure Lock;
    procedure Unlock;
    function IndexOf(Item: Pointer): Integer;
    function Add(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    procedure Remove(Item: Pointer);
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: Pointer read GetItem; default;
  end;

  TCustomPoolManager = class;

{ TCustomPoolInstance }
  TCustomPoolInstance = class(TObject)
  private
    FInUse: Boolean;
    FPoolManager: TCustomPoolManager;
  public
    property PoolManager: TCustomPoolManager read FPoolManager;
    property InUse: Boolean read FInUse;
  end;

{ TCustomPoolManager }
  TCustomPoolManager = class(TObject)
  private
    FItems: TvgThreadList;
    FMaxCount: Integer;
    FTimeout: DWord;
    FSemaphore: THandle;
    function GetCount: Integer;
    function GetItem(Index: Integer): TCustomPoolInstance;
  protected
    function InternalCreateNewInstance: TCustomPoolInstance; virtual; abstract;
    function CreateNewInstance: TCustomPoolInstance;
    function GetLock(Instance: TCustomPoolInstance): Boolean;
    procedure LockedInstance(Instance: TCustomPoolInstance; Value: Boolean); virtual;
    procedure CheckLocked(Instance: TCustomPoolInstance; var InUse: Boolean); virtual;
  public
    constructor Create(AMaxCount: Integer; ATimeout: DWord);
    destructor Destroy; override;
    procedure Clear;
    procedure ClearUnused;
    procedure Lock;
    procedure Unlock;
    function LockInstance: TCustomPoolInstance;
    procedure UnlockInstance(Instance: TCustomPoolInstance);
    property Items[Index: Integer]: TCustomPoolInstance read GetItem;
    property Count: Integer read GetCount;
    property Timeout: DWord read FTimeout;
    property MaxCount: Integer read FMaxCount;
  end;

{ TComponentPoolInstance }
  TComponentPoolInstance = class(TCustomPoolInstance)
  private
    FComponent: TComponent;
  public
    destructor Destroy; override;
    property Component: TComponent read FComponent write FComponent;
  end;

{ TComponentPoolManager }
  TComponentPoolManager = class(TCustomPoolManager)
  private
    FComponentClass: TComponentClass;
  protected
    function CreateComponent(Instance: TCustomPoolInstance): TComponent; virtual;
    function InternalCreateNewInstance: TCustomPoolInstance; override;
    procedure CheckLocked(Instance: TCustomPoolInstance; var InUse: Boolean); override;
  public
    constructor Create(AComponentClass: TComponentClass;
      AMaxCount: Integer; ATimeout: DWord);
    property ComponentClass: TComponentClass read FComponentClass;
  end;

{$IFDEF _D3_}
{ TIntfPoolInstance }
  TIntfPoolInstance = class(TCustomPoolInstance)
  private
    FUnk: IUnknown;
    function GetDispatch: IDispatch;
    function GetVariant: OleVariant;
  public
    destructor Destroy; override;
    property AsDispatch: IDispatch read GetDispatch;
    property AsUnknown: IUnknown read FUnk write FUnk;
    property AsVaraint: OleVariant read GetVariant;
  end;

{ TIntfPoolManager }
  TIntfPoolManager = class(TCustomPoolManager)
  protected
    function InternalCreateNewInstance: TCustomPoolInstance; override;
    function CreateUnknown(Instance: TCustomPoolInstance): IUnknown; virtual; abstract;
  public
    function LockInstance: TIntfPoolInstance;
  end;
{$ENDIF}

  TSignature = array [0..3] of Char;

{ TCompressor }
  TCompressor = class
  private
    FBuff, FData: Pointer;
    FBuffSize: Integer;
    FStream: TStream;
  protected
    property Buff: Pointer read FBuff;
    property BuffSize: Integer read FBuffSize;
    property Data: Pointer read FData;
    property Stream: TStream read FStream;
  public
    constructor Create; virtual;
    procedure Compress(AStream: TStream; const ABuff; ACount: Integer; AData: Pointer); virtual;
    procedure UnCompress(AStream: TStream; const ABuff; ACount: Integer; AData: Pointer); virtual;
    class function Sign: TSignature; virtual;
  end;

{ TBlockCompressor }
  TBlockCompressor = class(TCompressor)
  private
    FSourcePos: Integer;
  protected
    procedure GetBlock(var Buffer; Count: Integer; var ActualCount: Integer);
    procedure PutBlock(var Buffer; Count: Integer; var ActualCount: Integer);
    property SourcePos: Integer read FSourcePos;
  end;

  TCompressorClass = class of TCompressor;

{ TCompressorList }
  TCompressorList = class
  private
    FItems: TList;
    function GetCompressor(Index: Integer): TCompressorClass;
    function GetCount: Integer;
  public
    destructor Destroy; override;
    function CreateCompressor(Sign: TSignature): TCompressor;
    function FindCompressor(Sign: TSignature): TCompressorClass;
    procedure RegisterCompressor(CompressorClass: TCompressorClass);
    procedure UnRegisterCompressor(CompressorClass: TCompressorClass);
    property Compressors[Index: Integer]: TCompressorClass read GetCompressor;
    property Count: Integer read GetCount;
  end;

  { TReadMemoryStream }
  TReadMemoryStream = class (TCustomMemoryStream)
  public
    procedure SetPointer(Ptr: Pointer; Size: Longint);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TFileAccessModes   = (famWrite, famRead);
  TFileAccessMode    = set of TFileAccessModes;

  TFileShareModes    = (fsmRead, fsmWrite, fsmDelete);
  TFileShareMode     = set of TFileShareModes;

  TFileCreationMode  = (fcmCreateNew, fcmCreateAlways, fcmOpenExisting,
    fcmOpenAlways, fcmTruncateExisting);

{ TWinFileStream }
  TWinFileStream = class(THandleStream)
  public
    constructor Create(const FileName: TFileName; Access: TFileAccessMode;
      Share: TFileShareMode; Creation: TFileCreationMode; FileAttrsAndFlags: DWord;
      lpSecurity: PSecurityAttributes; TemplateHandle: Integer);
    destructor Destroy; override;
  end;

{ TClassItem }
  TClassItem = class
  private
    FClassType: TClass;
    FData: Pointer;
    FInfo: string;
  public
    constructor Create(AClassType: TClass; AData: Pointer; AInfo: string);
    property GetClassType: TClass read FClassType;
    property Data: Pointer read FData write FData;
    property Info: string read FInfo write FInfo;
  end;

{ TClassList }
  TClassName = type ShortString;

  TClassList = class
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TClassItem;
  protected
    function InternalRegister(AClass: TClass; const AData: Pointer; const AInfo: string; Inheritance: Boolean): TClassItem; virtual;
    procedure InternalUnRegister(AClass: TClass; Index: Integer); virtual;
  public
    destructor Destroy; override;
    procedure Clear;
    function ClassItemByName(const AClassName: TClassName): TClassItem;
    function FindClassItem(const AClassName: TClassName): TClassItem;
    function IndexOf(const AClassName: TClassName): Integer;
    function IndexOfClass(AClass: TClass; Inheritance: Boolean): Integer;
    procedure RegisterClass(AClass: TClass; const AData: Pointer; const AInfo: string; Inheritance: Boolean);
    procedure UnregisterClass(AClass: TClass);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TClassItem read GetItem; default;
  end;

{$IFDEF _D3_}
{ TEnumeratorObject }
  TEnumeratorObject = class(TInterfacedObject, IEnumVariant)
  private
    FFetched: LongWord;
  protected
    { IEnumVariant }
    {$IFDEF _D5_}
    function Next(celt: LongWord; var rgvar : OleVariant;
      out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    {$ELSE}
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    {$ENDIF}
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;
  public
    function Fetch(Index: LongWord; var VarResult: OleVariant): HResult; virtual;
    function CreateEnumerator: TEnumeratorObject; virtual;
    function GetCount: LongWord; virtual;
    property Fetched: LongWord read FFetched;
  end;

  TEnumeratorObjectClass = class of TEnumeratorObject;
{$ENDIF}

{ TPropInfoList }
procedure UpdateStoredList(AComponent: TComponent; AStoredList: TStrings; FromForm: Boolean);

{ TPropFiler }
function CreateStoredItem(const CompName, PropName: string): string;
function ParseStoredItem(Root: TComponent; const Item: string; var CompName, PropName: string): Boolean;
function ReplaceComponentName(Root: TComponent; const Item, CompName: string): string;
function GetEnumName(TypeInfo: PTypeInfo; Value: Integer): string;

function CompressorList: TCompressorList;

{ --- Buffer compression }
procedure Compress(Sign: TSignature; Stream: TStream; const Buff; Count: Integer; Data: Pointer);
procedure UnCompress(Sign: TSignature; Stream: TStream; const Buff; Count: Integer; Data: Pointer);

const
  famAccessAll = [famRead, famWrite];

  ThreadPriorities: array [TThreadPriority] of Integer =
   (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL);

  CreationMode: array [TFileCreationMode] of Integer = (
    CREATE_NEW, CREATE_ALWAYS, OPEN_EXISTING, OPEN_ALWAYS, TRUNCATE_EXISTING);

{ TPropFiler }
const
  sCount                     = 'Count';
  sItem                      = 'Item%d';
  sNull                      = '(null)';
  sPropNameDelimiter         = '_';

implementation
uses Messages, Consts, vgVCLRes, vgUtils;

var
  FTlsBuffers: TList = nil;
  FTlsLock: TRTLCriticalSection;

function CreateStoredItem(const CompName, PropName: string): string;
begin
  Result := '';
  if (CompName <> '') and (PropName <> '') then
    Result := CompName + '.' + PropName;
end;

function ParseStoredItem(Root: TComponent; const Item: string; var CompName, PropName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length(Item) = 0 then Exit;
  I := Pos('.', Item);
  if I > 0 then
  begin
    CompName := Trim(Copy(Item, 1, I - 1));
    PropName := Trim(Copy(Item, I + 1, MaxInt));
  end else if Assigned(Root) then
  begin
    CompName := Root.Name;
    PropName := Item
  end else begin
    CompName := '';
    PropName := '';
  end;
  Result := (Length(CompName) > 0) and (Length(PropName) > 0);
end;

function ReplaceComponentName(Root: TComponent; const Item, CompName: string): string;
var
  ACompName, APropName: string;
begin
  Result := '';
  if ParseStoredItem(Root, Item, ACompName, APropName) then
    Result := CreateStoredItem(CompName, APropName);
end;

function GetEnumName(TypeInfo: PTypeInfo; Value: Integer): string;
begin
  Result := TypInfo.GetEnumName(TypeInfo, Value);
end;

procedure UpdateStoredList(AComponent: TComponent; AStoredList: TStrings; FromForm: Boolean);
var
  I: Integer;
  Component: TComponent;
  CompName, PropName: string;
begin
  if not Assigned(AStoredList) or not Assigned(AComponent) then Exit;
  AStoredList.BeginUpdate;
  try
    for I := AStoredList.Count - 1 downto 0 do
    begin
      if ParseStoredItem(AComponent, AStoredList[I], CompName, PropName) then
      begin
        if FromForm then
        begin
          Component := AComponent.FindComponent(CompName);
          if not Assigned(Component) and (CompareText(AComponent.Name, CompName) = 0) then
            Component := AComponent;
          if Assigned(Component) then
            AStoredList.Objects[I] := Component else
            AStoredList.Delete(I);
        end else begin
          Component := TComponent(AStoredList.Objects[I]);
          if Assigned(Component) then
            AStoredList[I] := ReplaceComponentName(AComponent, AStoredList[I], Component.Name) else
            AStoredList.Delete(I);
        end;
      end else
        AStoredList.Delete(I);
    end;
  finally
    AStoredList.EndUpdate;
  end;
end;

{ TTlsBuffer }
constructor TTlsBuffer.Create(ASize: Integer);
begin
  AddTlsBuffer(Self);
  FSize := ASize;
  FTlsIndex := TlsAlloc;
  if FTlsIndex < 0 then
    raise Exception.Create(LoadStr(STlsCannotAlloc));
end;

destructor TTlsBuffer.Destroy;
begin
  while Assigned(FBlocks) do FreeMemory(FBlocks[0]);
  if (FTlsIndex >= 0) then TlsFree(FTlsIndex);
  RemoveTlsBuffer(Self);
  inherited;
end;

procedure TTlsBuffer.DoDetachThreadInternal;
begin
  FreeMemory(GetTlsValue);
end;

class procedure TTlsBuffer.DoDetachThread;
var
  I: Integer;
begin
  if Assigned(FTlsBuffers) then
  begin
    EnterCriticalSection(FTlsLock);
    try
      for I := 0 to FTlsBuffers.Count - 1 do
        TTlsBuffer(FTlsBuffers.Items[I]).DoDetachThreadInternal;
    finally
      LeaveCriticalSection(FTlsLock);
    end;
  end;
end;

function TTlsBuffer.GetMemory: Pointer;
begin
  Result := GetTlsValue;
  if not Assigned(Result) then
  begin
    EnterCriticalSection(FTlsLock);
    try
      Result := AllocMemory(FSize);
      try
        ListAdd(FBlocks, Result);
      except
        FreeMemory(Result);
        raise;
      end;
    finally
      LeaveCriticalSection(FTlsLock);
    end;
    TlsSetValue(FTlsIndex, Result);
  end;
end;

function TTlsBuffer.GetTlsValue: Pointer;
begin
  Result := TlsGetValue(FTlsIndex);
end;

function TTlsBuffer.AllocMemory(ASize: Integer): Pointer;
begin
  Result := AllocMem(ASize);
end;

procedure TTlsBuffer.FreeMemory(P: Pointer);
begin
  if Assigned(P) then
  begin
    EnterCriticalSection(FTlsLock);
    try
      ListRemove(FBlocks, P);
      FreeMem(P);
    finally
      LeaveCriticalSection(FTlsLock);
    end;
  end;
end;

class procedure TTlsBuffer.AddTlsBuffer(ATlsBuffer: TTlsBuffer);
begin
  ListAdd(FTlsBuffers, ATlsBuffer);
  if FTlsBuffers.Count = 1 then
    InitializeCriticalSection(FTlsLock);
end;

class procedure TTlsBuffer.RemoveTlsBuffer(ATlsBuffer: TTlsBuffer);
begin
  ListRemove(FTlsBuffers, ATlsBuffer);
  if not Assigned(FTlsBuffers) then
    DeleteCriticalSection(FTlsLock);
end;

{ TCustomThread }
procedure TCustomThread.DoExecute;
begin
end;

procedure TCustomThread.DoHandleException(Sender: TObject);
begin
end;

procedure TCustomThread.Execute;
begin
  try
    DoExecute;
  except
    DoHandleException(Self);
  end;
end;

{ TCustomWaitThread }
constructor TCustomWaitThread.Create(CreateSuspended: Boolean; ATimeout: DWord);
begin
  inherited Create(CreateSuspended);
  FTimeout := ATimeout;
  FEvent := CreateEvent(nil, False, False, nil);
end;

destructor TCustomWaitThread.Destroy;
begin
  Reset(True);
  CloseHandle(FEvent);
  inherited
end;

procedure TCustomWaitThread.DoReset;
begin
end;

procedure TCustomWaitThread.DoTimeout;
begin
end;

procedure TCustomWaitThread.DoExecute;
begin
  while not Terminated do
    case WaitForSingleObject(FEvent, FTimeOut) of
      WAIT_OBJECT_0:
      try
        DoReset;
      except
        DoHandleException(Self);
      end;
      WAIT_TIMEOUT:
      try
        DoTimeout;
      except
        DoHandleException(Self);
      end;
    end;
end;

procedure TCustomWaitThread.Reset(TerminateThread: Boolean);
begin
  if TerminateThread then Terminate;
  if Suspended then Resume;
  SetEvent(FEvent);
end;

{ TWaitThread }
procedure TWaitThread.DoReset;
begin
  if Assigned(FOnReset) then FOnReset(Self);
end;

procedure TWaitThread.DoTimeout;
begin
  if Assigned(FOnTimeout) then FOnTimeout(Self);
end;

{ TThreadEx }
procedure TThreadEx.DoExecute;
begin
  if Assigned(FOnExecute) then FOnExecute(Self);
end;

procedure TThreadEx.DoHandleException(Sender: TObject);
begin
  if Assigned(FOnException) then FOnException(Self);
end;

{ TCustomMessageThread }
destructor TCustomMessageThread.Destroy;
begin
  WaitForQuit;
  inherited;
end;

procedure TCustomMessageThread.DoAfterMessage(const Msg: TMsg; const RetValue: Integer);
begin
end;

procedure TCustomMessageThread.DoBeforeMessage(var Msg: TMsg; var Handled: Boolean);
begin
end;

procedure TCustomMessageThread.DoExecute;
var
  Msg: TMsg;
  Handled: Boolean;
begin
  while GetMessage(Msg, 0, 0, 0) do
  begin
    DoBeforeMessage(Msg, Handled);
    if not Handled then
      DoAfterMessage(Msg, DispatchMessage(Msg));
  end;
end;

procedure TCustomMessageThread.PostMessage(Msg: UINT; wParam: WPARAM; lParam: LPARAM);
begin
  PostThreadMessage(ThreadID, Msg, wParam, lParam);
end;

procedure TCustomMessageThread.PostQuitMessage;
begin
  Self.PostMessage(WM_QUIT, 0, 0);
end;

procedure TCustomMessageThread.WaitForQuit;
begin
  if not (Terminated or Suspended) then
  begin
    PostQuitMessage;
    Terminate;
    WaitFor;
  end;
end;

{ TMessageThread }
procedure TMessageThread.DoAfterMessage(const Msg: TMsg; const RetValue: Integer);
begin
  if Assigned(FOnAfterMessage) then FOnAfterMessage(Msg, RetValue);
end;

procedure TMessageThread.DoBeforeMessage(var Msg: TMsg; var Handled: Boolean);
begin
  Handled := False;
  if Assigned(FOnBeforeMessage) then FOnBeforeMessage(Msg, Handled);
end;

{ TPropInfoList }
constructor TPropInfoList.Create(AObject: TObject; Filter: TTypeKinds);
begin
  if AObject <> nil then
  begin
    FCount := GetPropList(AObject.ClassInfo, Filter, nil);
    FSize := FCount * SizeOf(Pointer);
    GetMem(FList, FSize);
    GetPropList(AObject.ClassInfo, Filter, FList);
  end else begin
    FCount := 0;
    FList := nil;
  end;
end;

destructor TPropInfoList.Destroy;
begin
  if FList <> nil then FreeMem(FList, FSize);
end;

function TPropInfoList.Find(const AName: string): PPropInfo;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    with FList^[I]^ do
      if (CompareText(Name, AName) = 0) then
      begin
        Result := FList^[I];
        Exit;
      end;
  Result := nil;
end;

procedure TPropInfoList.Delete(Index: Integer);
begin
  Dec(FCount);
  if Index < FCount then Move(FList^[Index + 1], FList^[Index],
    (FCount - Index) * SizeOf(Pointer));
end;

function TPropInfoList.Get(Index: Integer): PPropInfo;
begin
  Result := FList^[Index];
end;

{$IFDEF _D4_}
{ TPropsFiler }
constructor TPropsFiler.Create(AIniFile: TCustomIniFile; const ASection: string);
begin
  FIniFile := AIniFile;
  FSection := ASection;
end;

function TPropsFiler.GetItemName(const APropName: string): string;
begin
  Result := Prefix + APropName;
end;

procedure TPropsFiler.LoadAnyProperty(PropInfo: PPropInfo);
var
  S, Def: string;
begin
  try
    if PropInfo <> nil then
    begin
      case PropInfo^.PropType^.Kind of
        tkInteger: Def := StoreIntegerProperty(PropInfo);
        tkChar: Def := StoreCharProperty(PropInfo);
        tkEnumeration: Def := StoreEnumProperty(PropInfo);
        tkFloat: Def := StoreFloatProperty(PropInfo);
        tkWChar: Def := StoreWCharProperty(PropInfo);
        tkLString: Def := StoreLStringProperty(PropInfo);
        tkVariant: Def := StoreVariantProperty(PropInfo);
        tkInt64: Def := StoreInt64Property(PropInfo);
        tkString: Def := StoreStringProperty(PropInfo);
        tkSet: Def := StoreSetProperty(PropInfo);
        tkClass: Def := '';
        else Exit;
      end;
      if (Def <> '') or (PropInfo^.PropType^.Kind in [tkString, tkClass])
        or (PropInfo^.PropType^.Kind in [tkLString,  tkWChar])
      then
        S := Trim(ReadString(Section, GetItemName(PropInfo^.Name), Def))
      else S := '';
      case PropInfo^.PropType^.Kind of
        tkInteger: LoadIntegerProperty(S, PropInfo);
        tkChar: LoadCharProperty(S, PropInfo);
        tkEnumeration: LoadEnumProperty(S, PropInfo);
        tkFloat: LoadFloatProperty(S, PropInfo);
        tkWChar: LoadWCharProperty(S, PropInfo);
        tkLString: LoadLStringProperty(S, PropInfo);
        tkVariant: LoadVariantProperty(S, PropInfo);
        tkInt64: LoadInt64Property(S, PropInfo);
        tkString: LoadStringProperty(S, PropInfo);
        tkSet: LoadSetProperty(S, PropInfo);
        tkClass: LoadClassProperty(S, PropInfo);
      end;
    end;
  except end;
end;

procedure TPropsFiler.StoreAnyProperty(PropInfo: PPropInfo);
var
  S: string;
begin
  if PropInfo <> nil then
  begin
    case PropInfo^.PropType^.Kind of
      tkInteger: S := StoreIntegerProperty(PropInfo);
      tkChar: S := StoreCharProperty(PropInfo);
      tkEnumeration: S := StoreEnumProperty(PropInfo);
      tkFloat: S := StoreFloatProperty(PropInfo);
      tkLString: S := StoreLStringProperty(PropInfo);
      tkWChar: S := StoreWCharProperty(PropInfo);
      tkVariant: S := StoreVariantProperty(PropInfo);
      tkInt64: S := StoreInt64Property(PropInfo);
      tkString: S := StoreStringProperty(PropInfo);
      tkSet: S := StoreSetProperty(PropInfo);
      tkClass: S := StoreClassProperty(PropInfo);
      else Exit;
    end;
    if (S <> '') or (PropInfo^.PropType^.Kind in [tkString
      , tkLString, tkWChar]) then
      WriteString(Section, GetItemName(PropInfo^.Name), Trim(S));
  end;
end;

function TPropsFiler.StoreIntegerProperty(PropInfo: PPropInfo): string;
begin
  Result := IntToStr(GetOrdProp(FObject, PropInfo));
end;

function TPropsFiler.StoreCharProperty(PropInfo: PPropInfo): string;
begin
  Result := Char(GetOrdProp(FObject, PropInfo));
end;

function TPropsFiler.StoreEnumProperty(PropInfo: PPropInfo): string;
begin
  Result := GetEnumName(GetPropType(PropInfo), GetOrdProp(FObject, PropInfo));
end;

function TPropsFiler.StoreFloatProperty(PropInfo: PPropInfo): string;
const
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 19);
begin
  Result := ReplaceStr(FloatToStrF(GetFloatProp(FObject, PropInfo), ffGeneral,
    Precisions[GetTypeData(GetPropType(PropInfo))^.FloatType], 0), 
    DecimalSeparator, '.');
end;

function TPropsFiler.StoreStringProperty(PropInfo: PPropInfo): string;
begin
  Result := GetStrProp(FObject, PropInfo);
end;

function TPropsFiler.StoreLStringProperty(PropInfo: PPropInfo): string;
begin
  Result := GetStrProp(FObject, PropInfo);
end;

function TPropsFiler.StoreWCharProperty(PropInfo: PPropInfo): string;
begin
  Result := Char(GetOrdProp(FObject, PropInfo));
end;

function TPropsFiler.StoreVariantProperty(PropInfo: PPropInfo): string;
begin
  Result := GetVariantProp(FObject, PropInfo);
end;

function TPropsFiler.StoreInt64Property(PropInfo: PPropInfo): string;
begin
  Result := IntToStr(GetInt64Prop(FObject, PropInfo));
end;

function TPropsFiler.StoreSetProperty(PropInfo: PPropInfo): string;
var
  TypeInfo: PTypeInfo;
  W: Cardinal;
  I: Integer;
begin
  Result := '[';
  W := GetOrdProp(FObject, PropInfo);
  TypeInfo := GetTypeData(GetPropType(PropInfo))^.CompType^;
  for I := 0 to SizeOf(TCardinalSet) * 8 - 1 do
    if I in TCardinalSet(W) then
    begin
      if Length(Result) <> 1 then Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  Result := Result + ']';
end;

function TPropsFiler.StoreStringsProperty(PropInfo: PPropInfo): string;
var
  List: TObject;
  I: Integer;
  SectName: string;
begin
  Result := '';
  List := TObject(GetOrdProp(Self.FObject, PropInfo));
  SectName := Format('%s.%s', [Section, GetItemName(PropInfo^.Name)]);
  EraseSection(SectName);
  if (List is TStrings) and (TStrings(List).Count > 0) then
  begin
    WriteString(SectName, sCount, IntToStr(TStrings(List).Count));
    for I := 0 to TStrings(List).Count - 1 do
      WriteString(SectName, Format(sItem, [I]), TStrings(List)[I]);
  end;
end;

function TPropsFiler.StoreComponentProperty(PropInfo: PPropInfo): string;
var
  Comp: TComponent;
  RootName: string;
begin
  Comp := TComponent(GetOrdProp(FObject, PropInfo));
  if Comp <> nil then
  begin
    Result := Comp.Name;
    if (Comp.Owner <> nil) and (Comp.Owner <> FOwner) then
    begin
      RootName := Comp.Owner.Name;
      if RootName = '' then
      begin
        RootName := Comp.Owner.ClassName;
        if (RootName <> '') and (UpCase(RootName[1]) = 'T') then
          Delete(RootName, 1, 1);
      end;
      Result := Format('%s.%s', [RootName, Result]);
    end;
  end
  else Result := sNull;
end;

function TPropsFiler.StoreClassProperty(PropInfo: PPropInfo): string;
var
  Saver: TPropsFiler;
  I: Integer;
  Obj: TObject;

  procedure StoreObjectProps(Obj: TObject; const APrefix, ASection: string);
  var
    I: Integer;
    Props: TPropInfoList;
  begin
    with Saver do
    begin
      AObject := Obj;
      Prefix := APrefix;
      Props := TPropInfoList.Create(AObject, tkProperties);
      try
        for I := 0 to Props.Count - 1 do StoreAnyProperty(Props.Items[I]);
      finally
        Props.Free;
      end;
    end;
  end;

begin
  Result := '';
  Obj := TObject(GetOrdProp(Self.FObject, PropInfo));
  if (Obj <> nil) then
  begin
    if Obj is TStrings then
      StoreStringsProperty(PropInfo)
    else if Obj is TCollection then
    begin
      EraseSection(Format('%s.%s', [Section, Prefix + PropInfo^.Name]));
      Saver := CreateStorage;
      try
        WriteString(Section, Format('%s.%s', [Prefix + PropInfo^.Name, sCount]),
          IntToStr(TCollection(Obj).Count));
        for I := 0 to TCollection(Obj).Count - 1 do
        begin
          StoreObjectProps(TCollection(Obj).Items[I],
            Format(sItem, [I]) + sPropNameDelimiter,
            Format('%s.%s', [Section, Prefix + PropInfo^.Name]));
        end;
      finally
        Saver.Free;
      end;
    end else if Obj is TComponent then
    begin
      Result := StoreComponentProperty(PropInfo);
      Exit;
    end;
  end;
  Saver := CreateStorage;
  try
    with Saver do
      StoreObjectProps(Obj, Self.Prefix + PropInfo^.Name, Self.Section);
  finally
    Saver.Free;
  end;
end;

procedure TPropsFiler.LoadIntegerProperty(const S: string; PropInfo: PPropInfo);
begin
  SetOrdProp(FObject, PropInfo, StrToIntDef(S, 0));
end;

procedure TPropsFiler.LoadCharProperty(const S: string; PropInfo: PPropInfo);
begin
  SetOrdProp(FObject, PropInfo, Integer(S[1]));
end;

procedure TPropsFiler.LoadEnumProperty(const S: string; PropInfo: PPropInfo);
var
  I: Integer;
  EnumType: PTypeInfo;
begin
  EnumType := GetPropType(PropInfo);
  with GetTypeData(EnumType)^ do
    for I := MinValue to MaxValue do
      if CompareText(GetEnumName(EnumType, I), S) = 0 then
      begin
        SetOrdProp(FObject, PropInfo, I);
        Exit;
      end;
end;

procedure TPropsFiler.LoadFloatProperty(const S: string; PropInfo: PPropInfo);
begin
  SetFloatProp(FObject, PropInfo, StrToFloat(ReplaceStr(S, '.',
    DecimalSeparator)));
end;

procedure TPropsFiler.LoadInt64Property(const S: string; PropInfo: PPropInfo);
begin
  SetInt64Prop(FObject, PropInfo, StrToInt64Def(S, 0));
end;

procedure TPropsFiler.LoadLStringProperty(const S: string; PropInfo: PPropInfo);
begin
  SetStrProp(FObject, PropInfo, S);
end;

procedure TPropsFiler.LoadWCharProperty(const S: string; PropInfo: PPropInfo);
begin
  SetOrdProp(FObject, PropInfo, Longint(S[1]));
end;

procedure TPropsFiler.LoadVariantProperty(const S: string; PropInfo: PPropInfo);
begin
  SetVariantProp(FObject, PropInfo, S);
end;

procedure TPropsFiler.LoadStringProperty(const S: string; PropInfo: PPropInfo);
begin
  SetStrProp(FObject, PropInfo, S);
end;

procedure TPropsFiler.LoadSetProperty(const S: string; PropInfo: PPropInfo);
const
  Delims = [' ', ',', '[', ']'];
var
  TypeInfo: PTypeInfo;
  W: Cardinal;
  I, N: Integer;
  Count: Integer;
  EnumName: string;
begin
  W := 0;
  TypeInfo := GetTypeData(GetPropType(PropInfo))^.CompType^;
  Count := WordCount(S, Delims);
  for N := 1 to Count do
  begin
    EnumName := ExtractWord(N, S, Delims);
    try
      I := GetEnumValue(TypeInfo, EnumName);
      if I >= 0 then Include(TCardinalSet(W), I);
    except end;
  end;
  SetOrdProp(FObject, PropInfo, W);
end;

procedure TPropsFiler.LoadStringsProperty(const S: string; PropInfo: PPropInfo);
var
  List: TObject;
  Temp: TStrings;
  I, Cnt: Integer;
  SectName: string;
begin
  List := TObject(GetOrdProp(Self.FObject, PropInfo));
  if (List is TStrings) then
  begin
    SectName := Format('%s.%s', [Section, GetItemName(PropInfo^.Name)]);
    Cnt := StrToIntDef(Trim(ReadString(SectName, sCount, '0')), 0);
    if Cnt > 0 then
    begin
      Temp := TStringList.Create;
      try
        for I := 0 to Cnt - 1 do
          Temp.Add(ReadString(SectName, Format(sItem, [I]), ''));
        TStrings(List).Assign(Temp);
      finally
        Temp.Free;
      end;
    end;
  end;
end;

procedure TPropsFiler.LoadComponentProperty(const S: string; PropInfo: PPropInfo);
var
  RootName, Name: string;
  Root: TComponent;
  P: Integer;
begin
  if Trim(S) = '' then Exit;
  if CompareText(SNull, Trim(S)) = 0 then
  begin
    SetOrdProp(FObject, PropInfo, Longint(nil));
    Exit;
  end;
  P := Pos('.', S);
  if P > 0 then
  begin
    RootName := Trim(Copy(S, 1, P - 1));
    Name := Trim(Copy(S, P + 1, MaxInt));
  end else begin
    RootName := '';
    Name := Trim(S);
  end;
  if RootName <> '' then
    Root := FindGlobalComponent(RootName)
  else Root := FOwner;
  if (Root <> nil) then
    SetOrdProp(FObject, PropInfo, Longint(Root.FindComponent(Name)));
end;

procedure TPropsFiler.LoadClassProperty(const S: string; PropInfo: PPropInfo);
var
  Loader: TPropsFiler;
  I: Integer;
  Cnt: Integer;
  Recreate: Boolean;
  Obj: TObject;

  procedure LoadObjectProps(Obj: TObject; const APrefix, ASection: string);
  var
    I: Integer;
    Props: TPropInfoList;
  begin
    with Loader do
    begin
      AObject := Obj;
      Prefix := APrefix;
      Props := TPropInfoList.Create(AObject, tkProperties);
      try
        for I := 0 to Props.Count - 1 do LoadAnyProperty(Props.Items[I]);
      finally
        Props.Free;
      end;
    end;
  end;

begin
  Obj := TObject(GetOrdProp(Self.FObject, PropInfo));
  if (Obj <> nil) then
  begin
    if Obj is TStrings then
      LoadStringsProperty(S, PropInfo)
    else if Obj is TCollection then
    begin
      Loader := CreateStorage;
      try
        Cnt := TCollection(Obj).Count;
        Cnt := StrToIntDef(ReadString(Section, Format('%s.%s',
          [Prefix + PropInfo^.Name, sCount]), IntToStr(Cnt)), Cnt);
        Recreate := TCollection(Obj).Count <> Cnt;
        TCollection(Obj).BeginUpdate;
        try
          if Recreate then TCollection(Obj).Clear;
          for I := 0 to Cnt - 1 do
          begin
            if Recreate then TCollection(Obj).Add;
            LoadObjectProps(TCollection(Obj).Items[I],
              Format(sItem, [I]) + sPropNameDelimiter,
              Format('%s.%s', [Section, Prefix + PropInfo^.Name]));
          end;
        finally
          TCollection(Obj).EndUpdate;
        end;
      finally
        Loader.Free;
      end;
    end else if Obj is TComponent then
    begin
      LoadComponentProperty(S, PropInfo);
      Exit;
    end;
  end;
  Loader := CreateStorage;
  try
    LoadObjectProps(Obj, Self.Prefix + PropInfo^.Name, Self.Section);
  finally
    Loader.Free;
  end;
end;

procedure TPropsFiler.StoreProperties(PropList: TStrings);
var
  I: Integer;
  Props: TPropInfoList;
begin
  Props := TPropInfoList.Create(AObject, tkProperties);
  try
    for I := 0 to PropList.Count - 1 do
      StoreAnyProperty(Props.Find(PropList[I]));
  finally
    Props.Free;
  end;
end;

procedure TPropsFiler.LoadProperties(PropList: TStrings);
var
  I: Integer;
  Props: TPropInfoList;
begin
  Props := TPropInfoList.Create(AObject, tkProperties);
  try
    for I := 0 to PropList.Count - 1 do
      LoadAnyProperty(Props.Find(PropList[I]));
  finally
    Props.Free;
  end;
end;

function TPropsFiler.CreateInfoList(AComponent: TComponent; StoredList: TStrings): TStrings;
var
  I: Integer;
  Obj: TComponent;
  Props: TPropInfoList;
begin
  UpdateStoredList(AComponent, StoredList, False);
  Result := TStringList.Create;
  try
    TStringList(Result).Sorted := True;
    for I := 0 to StoredList.Count - 1 do
    begin
      Obj := TComponent(StoredList.Objects[I]);
      if Result.IndexOf(Obj.Name) < 0 then
      begin
        Props := TPropInfoList.Create(Obj, tkProperties);
        try
          Result.AddObject(Obj.Name, Props);
        except
          Props.Free;
          raise;
        end;
      end;
    end;
  except
    Result.Free;
    Result := nil;
  end;
end;

procedure TPropsFiler.FreeInfoLists(Info: TStrings);
var
  I: Integer;
begin
  for I := Info.Count - 1 downto 0 do
    Info.Objects[I].Free;
  Info.Free;
end;

procedure TPropsFiler.LoadObjectsProps(AComponent: TComponent; StoredList: TStrings);
var
  Info: TStrings;
  I, Idx: Integer;
  Props: TPropInfoList;
  CompName, PropName: string;
begin
  Info := CreateInfoList(AComponent, StoredList);
  if Info <> nil then
  try
    FOwner := AComponent;
    for I := 0 to StoredList.Count - 1 do
    begin
      if ParseStoredItem(AComponent, StoredList[I], CompName, PropName) then
      begin
        AObject := StoredList.Objects[I];
        Prefix := TComponent(AObject).Name;
        Idx := Info.IndexOf(Prefix);
        if Idx >= 0 then
        begin
          Prefix := Prefix + sPropNameDelimiter;
          Props := TPropInfoList(Info.Objects[Idx]);
          if Props <> nil then LoadAnyProperty(Props.Find(PropName));
        end;
      end;
    end;
  finally
    FOwner := nil;
    FreeInfoLists(Info);
  end;
end;

procedure TPropsFiler.StoreObjectsProps(AComponent: TComponent; StoredList: TStrings);
var
  Info: TStrings;
  I, Idx: Integer;
  Props: TPropInfoList;
  CompName, PropName: string;
begin
  Info := CreateInfoList(AComponent, StoredList);
  if Info <> nil then
  try
    FOwner := AComponent;
    for I := 0 to StoredList.Count - 1 do
    begin
      if ParseStoredItem(AComponent, StoredList[I], CompName, PropName) then
      begin
        AObject := StoredList.Objects[I];
        Prefix := TComponent(AObject).Name;
        Idx := Info.IndexOf(Prefix);
        if Idx >= 0 then
        begin
          Prefix := Prefix + sPropNameDelimiter;
          Props := TPropInfoList(Info.Objects[Idx]);
          if Props <> nil then StoreAnyProperty(Props.Find(PropName));
        end;
      end;
    end;
  finally
    FOwner := nil;
    FreeInfoLists(Info);
  end;
end;

function TPropsFiler.CreateStorage: TPropsFiler;
begin
  Result := TPropsFiler.Create(FIniFile, FSection);
end;

function TPropsFiler.ReadString(const ASection, Item, Default: string): string;
begin
  Result := FIniFile.ReadString(ASection, Item, Default);
end;

procedure TPropsFiler.WriteString(const ASection, Item, Value: string);
begin
  FIniFile.WriteString(ASection, Item, Value);
end;

procedure TPropsFiler.EraseSection(const ASection: string);
begin
  FIniFile.EraseSection(ASection);
end;
{$ENDIF}

{ TvgThreadList }
constructor TvgThreadList.Create;
begin
{$IFDEF _D4_}
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
{$ELSE}
  InitializeCriticalSection(FLock);
{$ENDIF}
end;

destructor TvgThreadList.Destroy;
begin
  Clear;
{$IFDEF _D4_}
  FLock.Free;
{$ELSE}
  DeleteCriticalSection(FLock);
{$ENDIF}
  inherited;
end;

function TvgThreadList.GetCount: Integer;
begin
  BeginRead;
  try
    Result := ListCount(FItems);
  finally
    EndRead;
  end;
end;

function TvgThreadList.GetItem(Index: Integer): Pointer;
begin
  BeginRead;
  try
    Result := ListItem(FItems, Index);
  finally
    EndRead;
  end;
end;

function TvgThreadList.IndexOf(Item: Pointer): Integer;
begin
  BeginRead;
  try
    Result := ListIndexOf(FItems, Item);
  finally
    EndRead;
  end;
end;

procedure TvgThreadList.BeginRead;
begin
{$IFDEF _D4_}
  FLock.BeginRead;
{$ELSE}
  EnterCriticalSection(FLock);
{$ENDIF}
end;

procedure TvgThreadList.EndRead;
begin
{$IFDEF _D4_}
  FLock.EndRead;
{$ELSE}
  LeaveCriticalSection(FLock);
{$ENDIF}
end;

procedure TvgThreadList.BeginWrite;
begin
{$IFDEF _D4_}
  FLock.BeginWrite;
{$ELSE}
  EnterCriticalSection(FLock);
{$ENDIF}
end;

procedure TvgThreadList.EndWrite;
begin
{$IFDEF _D4_}
  FLock.EndWrite;
{$ELSE}
  LeaveCriticalSection(FLock);
{$ENDIF}
end;

procedure TvgThreadList.Lock;
begin
  BeginWrite;
end;

procedure TvgThreadList.Unlock;
begin
  EndWrite;
end;

function TvgThreadList.Add(Item: Pointer): Integer;
begin
  BeginWrite;
  try
    Result := Count;
    Insert(Result, Item);
  finally
    EndWrite;
  end;
end;

procedure TvgThreadList.Insert(Index: Integer; Item: Pointer);
begin
  BeginWrite;
  try
    ListInsert(FItems, Index, Item);
  finally
    EndWrite;
  end;
end;

procedure TvgThreadList.Clear;
begin
  BeginWrite;
  try
    ListClear(FItems);
  finally
    EndWrite;
  end;
end;

procedure TvgThreadList.Remove(Item: Pointer);
begin
  BeginWrite;
  try
    if ListIndexOf(FItems, Item) >= 0 then
      ListRemove(FItems, Item);
  finally
    EndWrite;
  end;
end;

var
  FCompressorList: TObject = nil;

function CompressorList: TCompressorList;
begin
  if not Assigned(FCompressorList) then FCompressorList := TCompressorList.Create;
  Result := TCompressorList(FCompressorList);
end;

procedure Compress(Sign: TSignature; Stream: TStream; const Buff; Count: Integer; Data: Pointer);
var
  Compressor: TCompressor;
begin
  Compressor := CompressorList.CreateCompressor(Sign);
  try
    Compressor.Compress(Stream, Buff, Count, Data);
  finally
    Compressor.Free;
  end;
end;

procedure UnCompress(Sign: TSignature; Stream: TStream; const Buff; Count: Integer; Data: Pointer);
var
  Compressor: TCompressor;
begin
  Compressor := CompressorList.CreateCompressor(Sign);
  try
    Compressor.UnCompress(Stream, Buff, Count, Data);
  finally
    Compressor.Free;
  end;
end;

{ TCompressor }
constructor TCompressor.Create;
begin
end;

class function TCompressor.Sign: TSignature;
begin
  Result := (#0#0#0#0);
end;

procedure TCompressor.Compress(AStream: TStream; const ABuff; ACount: Integer; AData: Pointer);
begin
  FStream := AStream;
  FBuff := @ABuff;
  FBuffSize := ACount;
  FData := AData;
end;

procedure TCompressor.UnCompress(AStream: TStream; const ABuff; ACount: Integer; AData: Pointer);
begin
  FStream := AStream;
  FBuff := @ABuff;
  FBuffSize := ACount;
  FData := AData;
end;

{ TBlockCompressor }
procedure TBlockCompressor.GetBlock(var Buffer; Count: Integer; var ActualCount: Integer);
begin
  ActualCount := Min(Count, BuffSize - SourcePos);
  Move(Pointer(Integer(Buff) + SourcePos)^, Buffer, ActualCount);
  FSourcePos := FSourcePos + ActualCount;
end;

procedure TBlockCompressor.PutBlock(var Buffer; Count: Integer; var ActualCount: Integer);
begin
  ActualCount := Stream.Write(Buffer, Count);
end;

{ TCompressorList}
destructor TCompressorList.Destroy;
begin
  ListClear(FItems);
  inherited;
end;

function TCompressorList.GetCompressor(Index: Integer): TCompressorClass;
begin
  Result := ListItem(FItems, Index);
end;

function TCompressorList.GetCount: Integer;
begin
  Result := ListCount(FItems);
end;

function TCompressorList.CreateCompressor(Sign: TSignature): TCompressor;
var
  C: TCompressorClass;
begin
  C := FindCompressor(Sign);
  if not Assigned(C) then
    raise EInvalidOp.Create(Format(LoadStr(SUknownCompressorSign), [Sign]));
  Result := C.Create;
end;

function TCompressorList.FindCompressor(Sign: TSignature): TCompressorClass;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Compressors[I];
    if Result.Sign = Sign then Exit;
  end;
  Result := nil;
end;

procedure TCompressorList.RegisterCompressor(CompressorClass: TCompressorClass);
begin
  if FindCompressor(CompressorClass.Sign) = nil then
    ListAdd(FItems, CompressorClass);
end;

procedure TCompressorList.UnRegisterCompressor(CompressorClass: TCompressorClass);
begin
  ListRemove(FItems, CompressorClass);
end;

{ TReadMemoryStream }
procedure TReadMemoryStream.SetPointer(Ptr: Pointer; Size: Longint);
begin
  inherited SetPointer(Ptr, Size);
end;

function TReadMemoryStream.Write(const Buffer; Count: Longint): Longint;
asm
        XOR     EAX,EAX
end;

{ TWinFileStream }
constructor TWinFileStream.Create(const FileName: TFileName; Access: TFileAccessMode;
  Share: TFileShareMode; Creation: TFileCreationMode; FileAttrsAndFlags: DWord;
  lpSecurity: PSecurityAttributes; TemplateHandle: Integer);
var
  AHandle: Integer;
begin
  AHandle := CreateFile(PChar(FileName), Byte(Access) and 3 shl 30,
    Byte(Share), lpSecurity, CreationMode[Creation],
    FileAttrsAndFlags, TemplateHandle);

  if AHandle < 0 then
  begin
    if Creation in [fcmCreateNew, fcmCreateAlways] then
      raise EFCreateError.CreateFmt(SFCreateError, [FileName]) else
      raise EFOpenError.CreateFmt(SFOpenError, [FileName]);
  end;
  inherited Create(AHandle);
end;

destructor TWinFileStream.Destroy;
begin
  if Handle >= 0 then FileClose(Handle);
  inherited;
end;

{ TClassItem }
constructor TClassItem.Create(AClassType: TClass; AData: Pointer; AInfo: string);
begin
  FClassType := AClassType;
  FData := AData;
  FInfo := AInfo;
end;

{ TClassList }
destructor TClassList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TClassList.Clear;
begin
  ListDestroy(FItems);
end;

function TClassList.ClassItemByName(const AClassName: TClassName): TClassItem;
begin
  Result := FindClassItem(AClassName);
  if not Assigned(Result) then
    raise EClassNotFound.Create(Format(SClassNotFound, [AClassName]));
end;

function TClassList.FindClassItem(const AClassName: TClassName): TClassItem;
var
  I: Integer;
begin
  I := IndexOf(AClassName);
  if I >= 0 then
    Result := Items[I] else Result := nil;
end;

function TClassList.GetCount: Integer;
begin
  Result := ListCount(FItems);
end;

function TClassList.GetItem(Index: Integer): TClassItem;
begin
  Result := ListItem(FItems, Index);
end;

function TClassList.IndexOf(const AClassName: TClassName): Integer;
var
  AClass: TClass;
begin
  for Result := 0 to Count - 1 do
  begin
    AClass := Items[Result].GetClassType;
    if AnsiCompareText(AClass.ClassName, AClassName) = 0 then Exit;
  end;
  Result := -1;
end;

function TClassList.IndexOfClass(AClass: TClass; Inheritance: Boolean): Integer;
begin
  if Inheritance then
  begin
    for Result := Count - 1 downto 0 do
      if IsClass(AClass, Items[Result].GetClassType) then Exit;
  end else begin
    for Result := 0 to Count - 1 do
      if Items[Result].GetClassType = AClass then Exit;
  end;
  Result := -1;
end;

function TClassList.InternalRegister(AClass: TClass; const AData: Pointer; const AInfo: string; Inheritance: Boolean): TClassItem;
var
  I, J: Integer;
  Item: TClassItem;
begin
  Result := TClassItem.Create(AClass, AData, AInfo);
  try
    if Inheritance then
    begin
      J := 0;
      for I := Count - 1 downto 0 do
      begin
        Item := Items[I];
        if IsClass(AClass, Item.GetClassType) then
        begin
          J := I + 1;
          Break;
        end;
      end;
    end else
      J := Count;
    ListInsert(FItems, J, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TClassList.InternalUnRegister(AClass: TClass; Index: Integer);
begin
  ListDelete(FItems, Index);
end;

procedure TClassList.RegisterClass(AClass: TClass; const AData: Pointer; const AInfo: string; Inheritance: Boolean);
begin
  if IndexOfClass(AClass, False) < 0 then
    InternalRegister(AClass, AData, AInfo, Inheritance);
end;

procedure TClassList.UnregisterClass(AClass: TClass);
var
  I: Integer;
begin
  I := IndexOfClass(AClass, False);
  if I >= 0 then InternalUnRegister(AClass, I);
end;

{ TCustomPoolInstance }

{ TCustomPoolManager }
constructor TCustomPoolManager.Create(AMaxCount: Integer; ATimeout: DWord);
begin
  FItems := TvgThreadList.Create;
  FTimeout := ATimeout;
  FMaxCount := AMaxCount;
  FSemaphore := CreateSemaphore(nil, FMaxCount, FMaxCount, nil);
end;

destructor TCustomPoolManager.Destroy;
begin
  FItems.Free;
  CloseHandle(FSemaphore);
  inherited;
end;

procedure TCustomPoolManager.Clear;
var
  I: Integer;
begin
  Lock;
  try
    for I := 0 to FItems.Count - 1 do
      Items[I].Free;
    FItems.Clear;
  finally
    Unlock;
  end;
end;

procedure TCustomPoolManager.ClearUnused;
var
  I: Integer;
  Item: TCustomPoolInstance;
begin
  Lock;
  try
    for I := FItems.Count - 1 downto 0 do
    begin
      Item := Items[I];
      if not Item.InUse then
      begin
        Item.Free;
        FItems.Remove(Item);
      end;
    end;
  finally
    Unlock;
  end;
end;


procedure TCustomPoolManager.Lock;
begin
  FItems.Lock;
end;

procedure TCustomPoolManager.Unlock;
begin
  FItems.Unlock;
end;

function TCustomPoolManager.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCustomPoolManager.GetItem(Index: Integer): TCustomPoolInstance;
begin
  Result := FItems[Index];
end;

function TCustomPoolManager.LockInstance: TCustomPoolInstance;

  procedure RaiseError;
  begin
  {$IFDEF _D3_}
    raise EOutOfResources.Create(SOutOfResources);
  {$ELSE}
    raise EInvalidOp.Create(SOutOfResources);
  {$ENDIF}
  end;

var
  I: Integer;
  Instance: TCustomPoolInstance;
begin
  Result := nil;
  if WaitForSingleObject(FSemaphore, Timeout) = WAIT_FAILED then
    RaiseError;
  Lock;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      Instance := FItems[I];
      if GetLock(Instance) then
      begin
        Result := Instance;
        Exit;
      end;
    end;
    if FItems.Count < MaxCount then
      Result := CreateNewInstance
    else
      RaiseError;
  finally
    Unlock;
  end;
end;

procedure TCustomPoolManager.UnlockInstance(Instance: TCustomPoolInstance);
begin
  Lock;
  try
    LockedInstance(Instance, False);
    Instance.FInUse := False;
    ReleaseSemaphore(FSemaphore, 1, nil);
  finally
    Unlock;
  end;
end;

procedure TCustomPoolManager.LockedInstance(Instance: TCustomPoolInstance; Value: Boolean);
begin
end;

procedure TCustomPoolManager.CheckLocked(Instance: TCustomPoolInstance; var InUse: Boolean);
begin
end;

function TCustomPoolManager.GetLock(Instance: TCustomPoolInstance): Boolean;
begin
  Lock;
  try
    CheckLocked(Instance, Instance.FInUse);
    Result := not Instance.InUse;
    if Result then
      Instance.FInUse := True;
    LockedInstance(Instance, True);
  finally
    Unlock;
  end;
end;

function TCustomPoolManager.CreateNewInstance: TCustomPoolInstance;
begin
  Lock;
  try
    Result := InternalCreateNewInstance;
    if Assigned(Result) then
    try
      Result.FInUse := True;
      Result.FPoolManager := Self;
      FItems.Add(Result);
      LockedInstance(Result, True);
    except
      Result.Free;
      raise;
    end;
  finally
    Unlock;
  end;
end;

{ TComponentPoolInstance }
destructor TComponentPoolInstance.Destroy;
begin
  FComponent.Free;
  inherited;
end;

{ TComponentPoolManager }
constructor TComponentPoolManager.Create(AComponentClass: TComponentClass;
  AMaxCount: Integer; ATimeout: DWord);
begin
  inherited Create(AMaxCount, ATimeout);
  FComponentClass := AComponentClass;
end;

function TComponentPoolManager.CreateComponent(Instance: TCustomPoolInstance): TComponent;
begin
  Result := FComponentClass.Create(nil);
end;

function TComponentPoolManager.InternalCreateNewInstance: TCustomPoolInstance;
begin
  Result := TComponentPoolInstance.Create;
  try
    TComponentPoolInstance(Result).FComponent := CreateComponent(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TComponentPoolManager.CheckLocked(Instance: TCustomPoolInstance;
  var InUse: Boolean);
begin
  if TComponentPoolInstance(Instance).Component = nil then
  begin
    TComponentPoolInstance(Instance).FComponent := CreateComponent(Instance);
    InUse := False;
  end;
end;

{$IFDEF _D3_}
{ TIntfPoolInstance }
function TIntfPoolInstance.GetDispatch: IDispatch;
begin
  FUnk.QueryInterface(IDispatch, Result);
end;

function TIntfPoolInstance.GetVariant: OleVariant;
begin
  Result := AsDispatch;
end;

destructor TIntfPoolInstance.Destroy;
begin
  FUnk := nil;
  inherited;
end;

{ TIntfPoolManager }
function TIntfPoolManager.InternalCreateNewInstance: TCustomPoolInstance;
begin
  Result := TIntfPoolInstance.Create;
  try
    TIntfPoolInstance(Result).AsUnknown := CreateUnknown(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TIntfPoolManager.LockInstance: TIntfPoolInstance;
begin
   Result := TIntfPoolInstance(inherited LockInstance);
end;

{ TEnumeratorObject }
function TEnumeratorObject.CreateEnumerator: TEnumeratorObject;
begin
  Result := nil;
end;

function TEnumeratorObject.Fetch(Index: LongWord; var VarResult: OleVariant): HResult;
begin
  Result := S_FALSE;
end;

function TEnumeratorObject.GetCount: LongWord;
begin
  Result := 0;
end;

{ TEnumeratorObject.IEnumVariant }
{$IFDEF _D5_}
function TEnumeratorObject.Next(celt: LongWord; var rgvar : OleVariant;
  out pceltFetched: LongWord): HResult;
{$ELSE}
function TEnumeratorObject.Next(celt: Longint; out elt;
  pceltFetched: PLongint): HResult; stdcall;
{$ENDIF}
var
{$IFDEF _D5_}
  I: LongWord;
{$ELSE}
  I: Longint;
{$ENDIF}
  Tmp: OleVariant;
begin
  I := 0;
  while (I < celt) and (FFetched < GetCount) and (Fetch(FFetched, Tmp) = S_OK) do
  begin
{$IFDEF _D5_}
    TVariantArray(rgvar)[I] := Tmp;
{$ELSE}
    TVariantArray(elt)[I] := Tmp;
{$ENDIF}
    Inc(FFetched);
    Inc(I);
  end;

  if I = celt then
  begin
{$IFDEF _D5_}
    if Assigned(@pceltFetched) then
      pceltFetched := I;
{$ELSE}
    if Assigned(pceltFetched) then
      pceltFetched^ := I;
{$ENDIF}
    Result := S_OK;
  end else
    Result := S_FALSE;
end;

{$IFDEF _D5_}
function TEnumeratorObject.Skip(celt: LongWord): HResult;
{$ELSE}
function TEnumeratorObject.Skip(celt: Longint): HResult;
{$ENDIF}
begin
{$IFDEF _D5_}
  if FFetched + celt <= GetCount then
{$ELSE}
  if FFetched + LongWord(celt) <= GetCount then
{$ENDIF}
  begin
    Inc(FFetched, celt);
    Result := S_OK;
  end else
    Result := S_FALSE;
end;

function TEnumeratorObject.Reset: HResult;
begin
  FFetched := 0;
  Result := S_OK;
end;

function TEnumeratorObject.Clone(out Enum: IEnumVariant): HResult;
var
  Enumerator: TEnumeratorObject;
begin
  Enumerator := CreateEnumerator;
  if Assigned(Enumerator) then
  begin
    Enumerator.FFetched := FFetched;
    Enum := Enumerator as IEnumVariant;
    Result := S_OK;
  end else
    Result := E_NOTIMPL;
end;
{$ENDIF}

initialization

finalization
   FCompressorList.Free;

end.

