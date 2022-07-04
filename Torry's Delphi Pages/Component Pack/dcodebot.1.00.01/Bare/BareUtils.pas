
(********************************************************)
(*                                                      *)
(*  Bare Object Library @ www.codebot.org/delphi        *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BareUtils;

interface

{$I BARE.INC}

uses
  Windows, Messages, MSXML{$IFNDEF BARE}, Classes, SysUtils, ComObj{$ENDIF};

{$IFDEF BARE}

{ File mode magic numbers }

const
  fmClosed = $D7B0;
  fmInput  = $D7B1;
  fmOutput = $D7B2;
  fmInOut  = $D7B3;

  fmOpenRead       = $0000;
  fmOpenWrite      = $0001;
  fmOpenReadWrite  = $0002;
  fmShareCompat    = $0000;
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead  = $0030;
  fmShareDenyNone  = $0040;

type
  WordRec = packed record
    Lo, Hi: Byte;
  end;

  LongRec = packed record
    Lo, Hi: Word;
  end;

  TMethod = record
    Code, Data: Pointer;
  end;

{ Standard events }

type
  TNotifyEvent = procedure(Sender: TObject) of object;

{ Exception }

  Exception = class(TObject)
  private
    FMessage: string;
  public
    constructor Create(const Message: string);
    constructor CreateFmt(const Message: string; const Args: array of const);
    constructor CreateResFmt(Ident: Integer; const Args: array of const);
    property Message: string read FMessage;
  end;

  ExceptClass = class of Exception;

{ EWin32Error }

  EWin32Error = class(Exception)
  private
    FErrorCode: Integer;
  public
    property ErrorCode: Integer read FErrorCode write FErrorCOde;
  end;

{ EOleError }

  EOleError = class(EWin32Error)
  public
    constructor Create(const Message: string; ErrorCode: Integer);
  end;

{ Other error types }

  EConvertError = class(Exception);
  EFormatError = class(Exception);
  EStreamError = class(Exception);
  EFCreateError = class(EStreamError);
  EFOpenError = class(EStreamError);
  EFilerError = class(EStreamError);
  EReadError = class(EFilerError);
  EWriteError = class(EFilerError);
  EResNotFound = class(Exception);
  EListError = class(Exception);
  EStringListError = class(Exception);

{ Maximum TList size }

const
  MaxListSize = Maxint div 16;

{ TStream seek origins }

  soFromBeginning = 0;
  soFromCurrent = 1;
  soFromEnd = 2;

{ TFileStream create mode }

  fmCreate = $FFFF;
  faReadOnly  = $00000001;
  faHidden    = $00000002;
  faSysFile   = $00000004;
  faVolumeID  = $00000008;
  faDirectory = $00000010;
  faArchive   = $00000020;
  faAnyFile   = $0000003F;

{ Forward class declarations }

{$IFDEF FREEPASCAL}
type
  PString = ^string;

  PResStringRec = ^TResStringRec;
  TResStringRec = packed record
    Module: ^Longint;
    Identifier: Integer;
  end;
{$ENDIF}

type
  TStream = class;

{ TList class }

  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize - 1] of Pointer;
  TListSortCompare = function (Item1, Item2: Pointer): Integer;
  TListNotification = (lnAdded, lnExtracted, lnDeleted);

  TList = class(TObject)
  private
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Message: string; Data: Integer); overload; virtual;
    class procedure Error(Message: PResStringRec; Data: Integer); overload;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TList;
    function Extract(Item: Pointer): Pointer;
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PPointerList read FList;
  end;

{ TPersistent abstract class }

  TPersistent = class(TObject)
  private
    procedure AssignError(Source: TPersistent);
  protected
    procedure AssignTo(Dest: TPersistent); virtual;
  public
    procedure Assign(Source: TPersistent); virtual;
  end;

{ TPersistent class reference type }

  TPersistentClass = class of TPersistent;

  TStrings = class(TPersistent)
  private
    FUpdateCount: Integer;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
  protected
    procedure Error(const Message: string; Data: Integer); overload;
    procedure Error(Message: PResStringRec; Data: Integer); overload;
    function Get(Index: Integer): string; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: string; virtual;
    procedure Put(Index: Integer; const S: string); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: string); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
  public
    function Add(const S: string): Integer; virtual;
    function AddObject(const S: string; AObject: TObject): Integer; virtual;
    procedure Append(const S: string);
    procedure AddStrings(Strings: TStrings); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function IndexOf(const S: string): Integer; virtual;
    function IndexOfName(const Name: string): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: string); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: string;
      AObject: TObject);
    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
  end;

{ TStringList class }

  TStringList = class;

  PStringItem = ^TStringItem;
  TStringItem = record
    FString: string;
    FObject: TObject;
  end;

  PStringItemList = ^TStringItemList;
  TStringItemList = array[0..MaxListSize] of TStringItem;
  TStringListSortCompare = function(List: TStringList; Index1, Index2: Integer): Integer;

{ Duplicate management }

  TDuplicates = (dupIgnore, dupAccept, dupError);

  TStringList = class(TStrings)
  private
    FList: PStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
    procedure InsertItem(Index: Integer; const S: string);
    procedure SetSorted(Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: string; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

{ TStream abstract class }

  TStream = class(TObject)
  private
    function GetPosition: Longint;
    procedure SetPosition(Pos: Longint);
    function GetSize: Longint;
  protected
    procedure SetSize(NewSize: Longint); virtual;
  public
    function Read(var Buffer; Count: Longint): Longint; virtual; abstract;
    function Write(const Buffer; Count: Longint): Longint; virtual; abstract;
    function Seek(Offset: Longint; Origin: Word): Longint; virtual; abstract;
    procedure ReadBuffer(var Buffer; Count: Longint);
    procedure WriteBuffer(const Buffer; Count: Longint);
    function CopyFrom(Source: TStream; Count: Longint): Longint;
    property Position: Longint read GetPosition write SetPosition;
    property Size: Longint read GetSize write SetSize;
  end;

{ THandleStream class }

  THandleStream = class(TStream)
  private
    FHandle: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(AHandle: Integer);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property Handle: Integer read FHandle;
  end;

{ TFileStream class }

  TFileStream = class(THandleStream)
  public
    constructor Create(const FileName: string; Mode: Word);
    destructor Destroy; override;
  end;

{ TCustomMemoryStream abstract class }

  TCustomMemoryStream = class(TStream)
  private
    FMemory: Pointer;
    FSize, FPosition: Longint;
  protected
    procedure SetPointer(Ptr: Pointer; ASize: Longint);
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    property Memory: Pointer read FMemory;
  end;

{ TMemoryStream }

  TMemoryStream = class(TCustomMemoryStream)
  private
    FCapacity: Longint;
    procedure SetCapacity(NewCapacity: Longint);
  protected
    function Realloc(var NewCapacity: Longint): Pointer; virtual;
    property Capacity: Longint read FCapacity write SetCapacity;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

{ TStringStream }

  TStringStream = class(TStream)
  private
    FDataString: string;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const AString: string);
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadString(Count: Longint): string;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteString(const AString: string);
    property DataString: string read FDataString;
  end;

{ TResourceStream }

  TResourceStream = class(TCustomMemoryStream)
  private
    HResInfo: HRSRC;
    HGlobal: THandle;
    procedure Initialize(Instance: THandle; Name, ResType: PChar);
  public
    constructor Create(Instance: THandle; const ResName: string; ResType: PChar);
    constructor CreateFromID(Instance: THandle; ResID: Integer; ResType: PChar);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

{$ENDIF}

type
  TDocParser = class
  private
    FList: TList;
    FDocument: IXMLDOMDocument;
    FNode: IXMLDOMNode;
    FSelection: IXMLDOMNodeList;
    function GetEmpty: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    procedure Clear;
    function Push(const Section: string): Boolean;
    procedure Pop;
    function Next: Boolean;
    function ReadInt(const Name: string; Default: Integer = 0): Integer;
    function ReadString(const Name: string; Default: string = ''): string;
    function ReadFloat(const Name: string; Default: Double = 0): Double;
    function ReadBool(const Name: string; Default: Boolean = False): Boolean;
    procedure WriteInt(const Name: string; Value: Integer);
    procedure WriteString(const Name, Value: string);
    procedure WriteFloat(const Name: string; Value: Double);
    procedure WriteBool(const Name: string; Value: Boolean);
    property Empty: Boolean read GetEmpty;
  end;

  TNodeCache = class
  public
    Node: IXMLDOMNode;
    Selection: IXMLDOMNodeList;
    constructor Create(Node: IXMLDOMNode; Selection: IXMLDOMNodeList);
  end;

{ TPuckButton }

type
	TPuckButton = (pbLeft, pbMiddle, pbRight);

{ TUtilityWindow }

  TUtilityWindow = class(TObject)
  private
    FOwner: TObject;
    FHandle: HWND;
  public
    constructor Create(AOwner: TObject; WindowClass: string = '');
    destructor Destroy; override;
    property Handle: HWND read FHandle;
  end;

{$IFDEF BARE} 

{ TThread }

  EThread = class(Exception);

  TThreadMethod = procedure of object;
  TThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest,
    tpTimeCritical);

  TThread = class
  private
    FHandle: THandle;
    FThreadID: THandle;
    FTerminated: Boolean;
    FSuspended: Boolean;
    FFreeOnTerminate: Boolean;
    FFinished: Boolean;
    FReturnValue: Integer;
    FOnTerminate: TNotifyEvent;
    FMethod: TThreadMethod;
    FSynchronizeException: TObject;
    procedure CallOnTerminate;
    function GetPriority: TThreadPriority;
    procedure SetPriority(Value: TThreadPriority);
    procedure SetSuspended(Value: Boolean);
  protected
    procedure DoTerminate; virtual;
    procedure Execute; virtual; abstract;
    procedure Synchronize(Method: TThreadMethod);
    property ReturnValue: Integer read FReturnValue write FReturnValue;
    property Terminated: Boolean read FTerminated;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Resume;
    procedure Suspend;
    procedure Terminate;
    function WaitFor: LongWord;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Handle: THandle read FHandle;
    property Priority: TThreadPriority read GetPriority write SetPriority;
    property Suspended: Boolean read FSuspended write SetSuspended;
    property ThreadID: THandle read FThreadID;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

function AllocMem(Size: Cardinal): Pointer;
function CompareMem(P1, P2: pByte; const Size: Integer): Boolean;

{$ENDIF}

function MRandom: Double; overload;
function MRandom(Low, High: Integer): Integer; overload;
procedure MRandSeed(Seed: Integer);

{$IFDEF BARE}

var
  EmptyStr: string;
  NullStr: PString;

function NewStr(const S: string): PString;
procedure DisposeStr(P: PString);
function StrPas(const Str: PChar): string;

function UpperCase(const S: string): string;
function LowerCase(const S: string): string;
function CompareText(const S1, S2: string): Integer;
function AnsiCompareText(const S1, S2: string): Integer;
function Trim(const S: string): string;

{$ENDIF}

function FindCmdSwitch(const Switch: string; out Param: string): Boolean; overload;
function FindCmdSwitch(const Switch: string; out Param: Integer): Boolean; overload;
function FindCmdSwitch(const Switch: string): Boolean; overload;

{$IFDEF BARE}

type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

function StringReplace(const S, OldPattern, NewPattern: string): string; overload;
function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string; overload;

function LoadStr(Ident: Integer): string;

function LastDelimiter(const Delimiters, S: string): Integer;

function StrScan(const Str: PChar; Chr: Char): PChar;
function StrSearch(Str: PChar; Buffer: PChar): PChar;
function StrLen(const Str: PChar): Cardinal;

function DateToStr(const D: TDateTime): string;
function FloatToStr(const Value: Double): string;
function IntToStr(Value: Integer): string;
function IntToHex(Value: Integer): string;
function StrToDate(const S: string): TDateTime;
function StrToFloat(const S: string): Double;
function StrToInt(const S: string): Integer;
function StrToIntDef(const S: string; Default: Integer): Integer;

function TimeStamp: string;

function Format(const S: string; const Args: array of const): string;

{$ENDIF}

function SlashedPath(const S: string): string;

type
  TStringArray = array of string;

function LoadStringArray(const FileList: TStringArray): TStringArray; overload;
function LoadStringArray(const FileList: TStrings): TStringArray; overload;

procedure GetFileList(const Directory: string; Strings: TStrings;
  Wildcards: string = '*.*');

procedure WriteLog(const FileName: string; const S: string);

function FileReadString(const FileName: string; const Default: string = ''): string;
function RegReadString(const Key: string; const Default: string = ''): string;
procedure FileWriteString(const FileName, Value: string);
procedure RegWriteString(const Key, Value: string);


function ReadInt(Stream: TStream): Integer;
function ReadStr(Stream: TStream): string;
procedure WriteInt(Stream: TStream; Value: Integer);
procedure WriteStr(Stream: TStream; const Value: string);

function Split(const Source: string; Terminator: Char): TStringArray;
function FieldCount(const Source: string; Terminator: Char): Integer;
function FieldValue(const Source: string; Terminator: Char; Index: Integer): string;
function FieldSearch(const Key, Source: string; Terminator: Char): Boolean;

{$IFDEF BARE}

type
  TFieldPath = record
    Folder: string;
    Name: string;
  end;
  TProcedure = procedure;

  TFileName = type string;

  TSearchRec = record
    Time: Integer;
    Size: Integer;
    Attr: Integer;
    Name: TFileName;
    ExcludeAttr: Integer;
    FindHandle: THandle;
    FindData: TWin32FindData;
  end;


  TFileRec = packed record (* must match the size the compiler generates: 332 bytes *)
    Handle: Integer;
    Mode: Integer;
    RecSize: Cardinal;
    Private: array[1..28] of Byte;
    UserData: array[1..32] of Byte;
    Name: array[0..259] of Char;
  end;

  PTextBuf = ^TTextBuf;
  TTextBuf = array[0..127] of Char;
  TTextRec = packed record (* must match the size the compiler generates: 460 bytes *)
    Handle: Integer;
    Mode: Integer;
    BufSize: Cardinal;
    BufPos: Cardinal;
    BufEnd: Cardinal;
    BufPtr: PChar;
    OpenFunc: Pointer;
    InOutFunc: Pointer;
    FlushFunc: Pointer;
    CloseFunc: Pointer;
    UserData: array[1..32] of Byte;
    Name: array[0..259] of Char;
    Buffer: TTextBuf;
  end;

function FindMatchingFile(var F: TSearchRec): Integer;
function FindFirst(const Path: string; Attr: Integer;
  var F: TSearchRec): Integer;
function FindNext(var F: TSearchRec): Integer;
procedure FindClose(var F: TSearchRec);

function ExtractFieldPath(const S: string): TFieldPath;

function FileOpen(const FileName: string; Mode: LongWord): Integer;
function FileCreate(const FileName: string): Integer;
function FileRead(Handle: Integer; var Buffer; Count: LongWord): Integer;
function FileWrite(Handle: Integer; const Buffer; Count: LongWord): Integer;
function FileSeek(Handle, Offset, Origin: Integer): Integer; overload;
procedure FileClose(Handle: Integer);
function FileAge(const FileName: string): Integer;
function FileExists(const FileName: string): Boolean;
function FileTempName(const Path: string = ''): string;

function ExtractFileName(const FileName: string): string;
function ExtractFileExt(const FileName: string): string;
function ExtractFilePath(const FileName: string): string;

function DeleteFile(const FileName: string): Boolean;
function RenameFile(const OldName, NewName: string): Boolean;

type
  TModalResult = Integer;

procedure MessageNotify(const Msg: string);

type
  TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation);

  TMsgDlgButtons = (mbAbortRetryIgnore, mbOk, mbOkCancel, mbRetryCancel,
    mbYesNo, mbYesNoCancel);

function MessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TModalResult; overload;

function MessageDialog(const Msg: string; const Caption: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TModalResult; overload;

procedure ShowException(Msg: string);

function SysErrorMessage(ErrorCode: Integer): string;
procedure RaiseLastWin32Error;
function Win32Check(RetVal: BOOL): BOOL;
procedure OleError(ErrorCode: HResult);
procedure OleCheck(Result: HResult);

function ProgIDToClassID(const ProgID: string): TGUID;
function CreateOleObject(const ClassName: string): IDispatch;

{$ENDIF}

implementation

const
  ole32    = 'ole32.dll';

type
  POleStr = PWideChar;
  TCLSID = TGUID;
  TIID = TGUID;

const
  CLSCTX_INPROC_SERVER     = 1;
  CLSCTX_LOCAL_SERVER      = 4;

function CoInitialize(pvReserved: Pointer): HResult; stdcall; external ole32 name 'CoInitialize';
procedure CoUninitialize; stdcall; external ole32 name 'CoInitialize';
function CLSIDFromProgID(pszProgID: POleStr; out clsid: TCLSID): HResult; stdcall; external ole32 name 'CLSIDFromProgID';
function CoCreateInstance(const clsid: TCLSID; unkOuter: IUnknown;
  dwClsContext: Longint; const iid: TIID; out pv): HResult; stdcall; external ole32 name 'CoCreateInstance';


{$IFDEF BARE}

resourcestring
  SWin32Error = 'Win32 Error. Code: %d.'#10'%s';
  SUnkWin32Error = 'A Win32 API function failed';
  SOleError = 'OLE error %x';
  SExceptionMessage = 'Exception raised at %x';
  SInvalidFormat = 'Format ''%s'' invalid or incompatible with argument';
  SArgumentMissing = 'No argument for format "%s"';
  SAssignError = 'Cannot assign a %s to a %s';
  SFCreateError = 'Cannot create file %s';
  SFOpenError = 'Cannot open file %s';
  SReadError = 'Stream read error';
  SWriteError = 'Stream write error';
  SMemoryStreamError = 'Out of memory while expanding memory stream';
  SCantWriteResourceStreamError = 'Can''t write to a read-only resource stream';
  SResNotFound = 'Resource %s not found';
  SListIndexError = 'List index out of bounds (%d)';
  SListCapacityError = 'List capacity out of bounds (%d)';
  SListCountError = 'List count out of bounds (%d)';
  SSortedListError = 'Operation not allowed on sorted string list';
  SDuplicateString = 'String list does not allow duplicates';

function AllocMem(Size: Cardinal): Pointer;
begin
  GetMem(Result, Size);
  FillChar(Result^, Size, 0);
end;

function CompareMem(P1, P2: pByte; const Size: Integer): Boolean;
var i: Integer;
begin
  Result := True;
  for i := 1 to Size do
  begin
    if P1^ <> P2^ then Result := False;
    inc(P1); inc(P2);
  end {for i}
end;

{$ENDIF}

{ Random number generator }

var
  M0: Integer = 0;
  M1: Integer = 0;
  M2: Integer = 0;
  M3: Integer = 0;
  MC: Integer = 0;
  MF0: Integer = 5115;
  MF1: Integer = 1776;
  MF2: Integer = 1492;
  MF3: Integer = 2111111111;
  F2M32: Integer = $2F800000;
  EXTEND: Comp = 0;

function MRandom: Double;
asm
        PUSH    EDI;
        MOV     EAX, MF3;
        MUL     M3;
        MOV     ECX, EAX;
        MOV     EAX, M2;
        MOV     EDI, EDX;
        MOV     M3, EAX;
        MUL     MF2;
        ADD     ECX, EAX;
        MOV     EAX, M1;
        ADC     EDI, EDX;
        MOV     M2, EAX;
        MUL     MF1;
        ADD     ECX, EAX;
        MOV     EAX, M0;
        ADC     EDI, EDX;
        MOV     M1, EAX;
        MUL     MF0;
        ADD     EAX, ECX;
        ADC     EDX, EDI;
        ADD     EAX, MC;
        ADC     EDX, 0;
        MOV     M0, EAX;
        MOV     MC, EDX;
        LEA     EDI, EXTEND;
        MOV     [EDI], EAX;
        FILD    EXTEND;
        POP     EDI;
        FMUL    F2M32;
end;

function MRandom(Low, High: Integer): Integer;
begin
  Result := Low + Trunc(MRandom * (High - Low));
end;

procedure MRandSeed(Seed: Integer);
asm
        PUSH    EDI;
        CMP     EAX, 1;
        SBB     EAX, 0;
        XOR     ECX, ECX;
@R80:   MOV     EDX, EAX;
        SHL     EAX, 13;
        XOR     EDX, EAX;
        MOV     EAX, EDX;
        SHR     EDX, 17;
        XOR     EAX, EDX;
        MOV     EDX, EAX;
        SHL     EDX, 5;
        XOR     EAX, EDX;
        MOV     M0[ECX * 4], EAX;
        INC     ECX;
        CMP     ECX, 5;
        JB      @R80;
        MOV     EDI, 19;
@R90:   CALL    MRandom;
        FSTP    ST(0);
        DEC     EDI;
        JNZ     @R90;
        POP     EDI;
end;

{$IFDEF BARE}

function NewStr(const S: string): PString;
begin
  if S = '' then Result := NullStr else
  begin
    New(Result);
    Result^ := S;
  end;
end;

procedure DisposeStr(P: PString);
begin
  if (P <> nil) and (P^ <> '') then Dispose(P);
end;

function StrPas(const Str: PChar): string;
begin
  Result := Str;
end;

function UpperCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function LowerCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'A') and (Ch <= 'Z') then Inc(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

{$ENDIF}

function CompareText(const S1, S2: string): Integer; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
        OR      EAX,EAX
        JE      @@0
        MOV     EAX,[EAX-4]
@@0:    OR      EDX,EDX
        JE      @@1
        MOV     EDX,[EDX-4]
@@1:    MOV     ECX,EAX
        CMP     ECX,EDX
        JBE     @@2
        MOV     ECX,EDX
@@2:    CMP     ECX,ECX
@@3:    REPE    CMPSB
        JE      @@6
        MOV     BL,BYTE PTR [ESI-1]
        CMP     BL,'a'
        JB      @@4
        CMP     BL,'z'
        JA      @@4
        SUB     BL,20H
@@4:    MOV     BH,BYTE PTR [EDI-1]
        CMP     BH,'a'
        JB      @@5
        CMP     BH,'z'
        JA      @@5
        SUB     BH,20H
@@5:    CMP     BL,BH
        JE      @@3
        MOVZX   EAX,BL
        MOVZX   EDX,BH
@@6:    SUB     EAX,EDX
        POP     EBX
        POP     EDI
        POP     ESI
end;

function FindCmdSwitch(const Switch: string; out Param: string): Boolean; overload;
var
  I: Integer;
  S: string;
begin
  Param := '';
  Result := False;
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if S[1] in ['-', '/'] then
      if CompareText(Copy(S, 2, Length(Switch)), Switch) = 0 then
      begin
        if S[Length(Switch) + 1] = ':' then
        begin
          Param := Copy(S, Length(Switch) + 2, Maxint);
	        Result := Param <> '';
        end
        else
	        Result := True;
        Break;
      end;
  end;
end;

function FindCmdSwitch(const Switch: string; out Param: Integer): Boolean; overload;
var
  S: string;
begin
	Param := 0;
  Result := FindCmdSwitch(Switch, S);
  if Result then
  begin
  	Param := StrToIntDef(S, MaxInt);
    Result := Param < MaxInt;
  end;
end;

function FindCmdSwitch(const Switch: string): Boolean; overload;
var
  S: string;
begin
  Result := FindCmdSwitch(Switch, S);
end;

function AnsiCompareText(const S1, S2: string): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar(S1),
    Length(S1), PChar(S2), Length(S2)) - 2;
end;

{$IFDEF BARE}

function Trim(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
var
  SearchStr, Patt, NewStr: string;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := UpperCase(S);
    Patt := UpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function StringReplace(const S, OldPattern, NewPattern: string): string;
begin
  Result := StringReplace(S, OldPattern, NewPattern, [rfReplaceAll, rfIgnoreCase]);
end;

type
  PStrData = ^TStrData;
  TStrData = record
    Ident: Integer;
    Buffer: PChar;
    BufSize: Integer;
    nChars: Integer;
  end;

function EnumStringModules(Instance: Longint; Data: Pointer): Boolean;
begin
  with PStrData(Data)^ do
  begin
    nChars := LoadString(Instance, Ident, Buffer, BufSize);
    Result := nChars = 0;
  end;
end;

function FindStringResource(Ident: Integer; Buffer: PChar; BufSize: Integer): Integer;
var
  StrData: TStrData;
begin
  StrData.Ident := Ident;
  StrData.Buffer := Buffer;
  StrData.BufSize := BufSize;
  StrData.nChars := 0;
  EnumResourceModules(EnumStringModules, @StrData);
  Result := StrData.nChars;
end;

function LoadStr(Ident: Integer): string;
var
  Buffer: array[0..1023] of Char;
begin
  SetString(Result, Buffer, FindStringResource(Ident, Buffer, SizeOf(Buffer)));
end;

function LastDelimiter(const Delimiters, S: string): Integer;
var
  P: PChar;
begin
  Result := Length(S);
  P := PChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (StrScan(P, S[Result]) <> nil) then Exit;
    Dec(Result);
  end;
end;

function StrScan(const Str: PChar; Chr: Char): PChar; assembler;
asm
        PUSH    EDI
        PUSH    EAX
        MOV     EDI,Str
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        NOT     ECX
        POP     EDI
        MOV     AL,Chr
        REPNE   SCASB
        MOV     EAX,0
        JNE     @@1
        MOV     EAX,EDI
        DEC     EAX
@@1:    POP     EDI
end;

function StrSearch(Str: PChar; Buffer: PChar): PChar;
asm
        PUSH    EBX
        MOV     ECX, EAX
        MOV     EAX, EDX
        MOV     EDX, ECX
        TEST    EAX,EAX
        JZ      @6
        TEST    EDX,EDX
        JZ      @6
        XOR     ECX,ECX
        JMP     @3
@1:     INC     ECX
@2:     INC     EDX
@3:     MOV     BL,[EAX+ECX]
        OR      BL,BL
        JZ      @7
@4:     MOV     BH,[EDX]
        OR      BH,BH
        JZ      @6
        CMP     BH,'a'
        JB      @5
        CMP     BH,'z'
        JA      @5
        SUB     BH,'a'-'A'
@5:     CMP     BL,BH
        JE      @1
        OR      ECX,ECX
        JZ      @2
        SUB     EDX,ECX
        INC     EDX
        XOR     ECX,ECX
        JMP     @3
@6:     XOR     EDX,EDX
@7:     MOV     EAX,EDX
        POP     EBX;
end;

function StrLen(const Str: PChar): Cardinal;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        MOV     EAX,0FFFFFFFEH
        SUB     EAX,ECX
        MOV     EDI,EDX
end;

function DateToStr(const D: TDateTime): string;
var
  V: Variant;
begin
  V := D;
  Result := V;
end;

function FloatToStr(const Value: Double): string;
var
  X: Extended;
  Width, Decimals: Integer;
begin
  X := Value;
  Width := 0;
  repeat
    X := X / 10;
    Inc(Width);
  until X < 1;
  Decimals := 3;
  Str(Value:Width:Decimals, Result);
end;

function IntToStr(Value: Integer): string;
begin
  Str(Value, Result);
end;

function IntToHex(Value: Integer): string;
const
  HexDigits: array[0..15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' );
var
  Digit: Integer;
  I: Integer;
begin
  for I := 0 to 7 do
  begin
    Digit := Value and $F;
    Result := HexDigits[Digit] + Result;
    Value := Value shr 4;
  end;
  Result := '$' + Result;
end;

function StrToInt(const S: string): Integer;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code <> 0 then raise EConvertError.CreateFmt('''%s'' is not a valid integer', [S]);;
end;

function StrToIntDef(const S: string; Default: Integer): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := Default;
end;

function StrToDate(const S: string): TDateTime;
var
  V: Variant;
begin
  V := S;
  Result := V;
end;

function StrToFloat(const S: string): Double;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code <> 0 then raise EConvertError.CreateFmt('''%s'' is not a floating point number', [S]);;
end;

function TimeStamp: string;
const
  MonthNames: array[1..12] of string = (
    'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August',
    'September', 'October', 'November', 'December');
var
  Time: TSystemTime;
  M, S, Meridian: string;
begin
  GetLocalTime(Time);
  with Time do
  begin
    M := IntToStr(wMinute);
    if Length(M) = 1 then M := '0' + M;
    S := IntToStr(wSecond);
    if wHour div 12 > 1 then
      Meridian := ' PM'
    else
      Meridian := 'AM';
    wHour := wHour mod 12;
    if wHour = 0 then wHour := 12;
    if Length(S) = 1 then S := '0' + S;
    Result := Format('%s %d %d %d:%s:%s%s', [MonthNames[wMonth], wDay, wYear,
      wHour, M, S, Meridian]);
  end;
end;

function Format(const S: string; const Args: array of const): string;
var
  Argument, Section: string;
  P, Token: PChar;
  I: Integer;
begin
  Result := '';
  if S = '' then Exit;
  P := PChar(S);
  I := 0;
  repeat
    Argument := '';
    Token := StrSearch(P, '%');
    if (Token <> nil) and (I <= High(Args)) then
      with Args[I] do
        case Token[0] of
          'D', 'd':
            case VType of
              vtInteger:
                Argument := IntToStr(VInteger);
              vtInt64:
                Argument := IntToStr(VInt64^);
            else
              raise EFormatError.CreateFmt(SInvalidFormat, ['%d']);
            end;
          'X', 'x':
            case VType of
              vtInteger:
                Argument := IntToHex(VInteger);
              vtInt64:
                Argument := IntToHex(VInt64^);
              vtPointer:
                Argument := IntToHex(Integer(VPointer));
            else
              raise EFormatError.CreateFmt(SInvalidFormat, ['%d']);
            end;
          'F', 'f':
            case VType of
              vtInteger:
                Argument := IntToStr(VInteger);
              vtInt64:
                Argument := IntToStr(VInt64^);
              vtExtended:
                Argument := FloatToStr(VExtended^);
            else
              raise EFormatError.CreateFmt(SInvalidFormat, ['%f']);
            end;
          'S', 's':
            case VType of
              vtString:
                begin
                  Argument := VString^;
                  if Argument = '' then Argument := ' ';
                end;
              vtAnsiString:
                begin
                  Argument := string(VAnsiString);
                  if Argument = '' then Argument := ' ';
                end;
              vtPChar:
                begin
                  Argument := VPChar;
                  if Argument = '' then Argument := ' ';
               end;
            else
              raise EFormatError.CreateFmt(SInvalidFormat, ['%s']);
            end;
        end;
    if Argument <> '' then
    begin
      SetString(Section, P, Integer(Token) - Integer(P) - 1);
      Result := Result + Section + Argument;
      Inc(Token);
      Inc(I);
    end
    else
    begin
      if Token = nil then
      begin
        Token := P;
        while Token^ <> #0 do Inc(Token);
      end;
      SetString(Section, P, Integer(Token) - Integer(P));
      Result := Result + Section + Argument;
    end;
    P := Token;
  until Token^ = #0;
end;

{$ENDIF}

function SlashedPath(const S: string): string;
begin
  Result := S;
  if Result <> '' then
    if Result[Length(Result)] <> '\' then
      Result := Result + '\';
end;

function LoadStringArray(const FileList: TStringArray): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, Length(FileList));
  for I := Low(Result) to High(Result) do
    Result[I] := FileReadString(FileList[I]);
end;

function LoadStringArray(const FileList: TStrings): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, FileList.Count);
  for I := Low(Result) to High(Result) do
    Result[I] := FileReadString(FileList[I]);
end;


procedure GetFileList(const Directory: string; Strings: TStrings;
  Wildcards: string = '*.*');
var
  PriorSorted: Boolean;
  SearchRec: TSearchRec;
  SearchResult: Integer;
  S: string;
begin
  Strings.Clear;
  S := Directory;
  if S = '' then Exit;
  if S[Length(S)] <> '\' then
    S := S + '\';
  SearchResult := FindFirst(S + Wildcards, faAnyFile and
    (not faDirectory), SearchRec);
  while SearchResult = 0 do
  begin
    Strings.AddObject(S + SearchRec.Name, TObject(SearchRec.Size));
    SearchResult := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  if Strings is TStringList then
    with Strings as TStringList do
    begin
      PriorSorted := Sorted;
      Sorted := True;
      Sorted := PriorSorted;
    end;
end;

procedure WriteLog(const FileName: string; const S: string);
var
  LogFile: TextFile;
begin
  AssignFile(LogFile, FileName);
  if FileExists(FileName) then
    Append(LogFile)
  else
    ReWrite(LogFile);
  try
    WriteLn(LogFile, S);
  finally
    CloseFile(LogFile)
  end;
end;

function FileReadString(const FileName: string; const Default: string = ''): string;
var
  F: THandle;
  Bytes: Cardinal;
begin
  Result := '';
  F := CreateFile(PChar(FileName), GENERIC_READ, 0, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  if F <> INVALID_HANDLE_VALUE then
  try
    Bytes := GetFileSize(F, nil);
    if Bytes > 0 then
    begin
      SetLength(Result, Bytes);
      ReadFile(F, PChar(Result)^, Bytes, Bytes, nil);
    end;
  finally
    CloseHandle(F);
  end;
end;

function RegReadString(const Key: string; const Default: string = ''): string;
var
  RegKey: HKEY;
  Size: Cardinal;
  S: string;
  I: Integer;
begin
  Result := Default;
  S := UpperCase(FieldValue(Key, '\', 0));
  if S = 'HKEY_CLASSES_ROOT' then
    RegKey := HKEY_CLASSES_ROOT
  else if S = 'HKEY_CURRENT_USER' then    RegKey := HKEY_CURRENT_USER  else if S = 'HKEY_LOCAL_MACHINE' then    RegKey := HKEY_LOCAL_MACHINE  else if S = 'HKEY_USERS' then
    RegKey := HKEY_USERS
  else
    Exit;
  S := '';
  for I := 1 to FieldCount(Key, '\') - 2 do
    S := S + FieldValue(Key, '\', I) + '\';
  if S = '' then
    Exit;
  SetLength(S, Length(S) - 1);
  if RegOpenKeyEx(RegKey, PChar(S), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  try
    Size := 0;
    S := FieldValue(Key, '\', FieldCount(Key, '\') - 1);
    if S <> '' then
      case RegQueryValueEx(RegKey, PChar(S), nil, nil, nil, @Size) of
        ERROR_SUCCESS, ERROR_MORE_DATA:
          begin
            SetLength(Result, Size);
            RegQueryValueEx(RegKey, PChar(S), nil, nil, Pointer(Result), @Size);
            SetLength(Result, StrLen(PChar(Result)));
          end;
      end;
  finally
    RegCloseKey(RegKey);
  end;
end;

procedure FileWriteString(const FileName, Value: string);
var
  F: THandle;
  Bytes: Cardinal;
begin
  F := CreateFile(PChar(FileName), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
    FILE_ATTRIBUTE_NORMAL, 0);
  if F <> INVALID_HANDLE_VALUE then
  try
    if Value <> '' then
      WriteFile(F, PChar(Value)^, Length(Value), Bytes, nil);
  finally
    CloseHandle(F);
  end;
end;

procedure RegWriteString(const Key, Value: string);
const
  EmptyKey: Char = #0;
var
  RegKey: HKEY;
  Disposition: Cardinal;
  S: string;
  I: Integer;
begin
  S := UpperCase(FieldValue(Key, '\', 0));
  if S = 'HKEY_CLASSES_ROOT' then
    RegKey := HKEY_CLASSES_ROOT
  else if S = 'HKEY_CURRENT_USER' then    RegKey := HKEY_CURRENT_USER  else if S = 'HKEY_LOCAL_MACHINE' then    RegKey := HKEY_LOCAL_MACHINE  else if S = 'HKEY_USERS' then
    RegKey := HKEY_USERS
  else
    Exit;
  S := '';
  for I := 1 to FieldCount(Key, '\') - 2 do
    S := S + FieldValue(Key, '\', I) + '\';
  if S = '' then
    Exit;
  SetLength(S, Length(S) - 1);
  if RegCreateKeyEx(RegKey, PChar(S), 0, nil,  REG_OPTION_NON_VOLATILE,
    KEY_WRITE, nil, RegKey, @Disposition) = ERROR_SUCCESS then
  try
    S := FieldValue(Key, '\', FieldCount(Key, '\') - 1);
    if (S <> '') and (Value <> '') then
      RegSetValueEx(RegKey, PChar(S), 0, REG_SZ, Pointer(Value), Length(Value) + 1)
    else
      RegSetValueEx(RegKey, PChar(S), 0, REG_SZ, @EmptyKey, 1);
  finally
    RegCloseKey(RegKey);
  end;
end;

function ReadInt(Stream: TStream): Integer;
begin
	Stream.Read(Result, SizeOf(Result));
end;

function ReadStr(Stream: TStream): string;
var
	I: Integer;
begin
	I := ReadInt(Stream);
  if I > 0 then
  begin
  	SetLength(Result, I);
    Stream.Read(PChar(Result)^, I);
  end
  else
  	Result := '';
end;

procedure WriteInt(Stream: TStream; Value: Integer);
begin
	Stream.Write(Value, SizeOf(Value));
end;

procedure WriteStr(Stream: TStream; const Value: string);
var
  P: PChar;
	I: Integer;
begin
	I := Length(Value);
  if I > 0 then
  begin
    P := PChar(Value);
    Dec(P, SizeOf(Integer));
  	Stream.Write(P^, I + SizeOf(Integer));
  end;
end;

function Split(const Source: string; Terminator: Char): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, FieldCount(Source, Terminator));
  for I := Low(Result) to High(Result) do
    Result[I] := FieldValue(Source, Terminator, I);
end;

function FieldCount(const Source: string; Terminator: Char): Integer;
var
  P: PChar;
begin
  Result := 0;
  if Source <> '' then
  begin
    P := PChar(Source);
    repeat
      if P^ = Terminator then
        Inc(Result);
      Inc(P);
    until P^ = #0;
    if (Result > 0) and (P[-1] <> Terminator) then
      Inc(Result);
  end;
  if Result = 0 then Result := 1;
end;

function FieldValue(const Source: string; Terminator: Char; Index: Integer): string;
var
  Start, P: PChar;
  I: Integer;
begin
  Result := '';
  if Source <> '' then
  begin
    Start := PChar(Source);
    P := Start;
    I := 0;
    while P^ > #0 do
    begin
      if P^ = Terminator then
      begin
        if I = Index then
        begin
          SetString(Result, Start, Integer(P - Start));
          Exit;
        end;
        Start := P;
        Inc(Start);
        Inc(I);
      end;
      Inc(P);
    end;
    if I = Index then
      SetString(Result, Start, Integer(P - Start));
  end;
end;

function FieldSearch(const Key, Source: string; Terminator: Char): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FieldCount(Source, Terminator) -1 do
    if Key = FieldValue(Source, Terminator, I) then
    begin
      Result := True;
      Break;
    end;
end;

{$IFDEF BARE}

function FindMatchingFile(var F: TSearchRec): Integer;
var
  LocalFileTime: TFileTime;
begin
  with F do
  begin
    while FindData.dwFileAttributes and ExcludeAttr <> 0 do
      if not FindNextFile(FindHandle, FindData) then
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi,
      LongRec(Time).Lo);
    Size := FindData.nFileSizeLow;
    Attr := FindData.dwFileAttributes;
    Name := FindData.cFileName;
  end;
  Result := 0;
end;

function FindFirst(const Path: string; Attr: Integer;
  var F: TSearchRec): Integer;
const
  faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;
begin
  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := FindFirstFile(PChar(Path), F.FindData);
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := FindMatchingFile(F);
    if Result <> 0 then FindClose(F);
  end else
    Result := GetLastError;
end;

function FindNext(var F: TSearchRec): Integer;
begin
  if FindNextFile(F.FindHandle, F.FindData) then
    Result := FindMatchingFile(F) else
    Result := GetLastError;
end;

procedure FindClose(var F: TSearchRec);
begin
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
end;

function ExtractFieldPath(const S: string): TFieldPath;
var
  Count: Integer;
  I: Integer;
begin
  Result.Folder := '';
  Result.Name := '';
  Count := FieldCount(S, '\');
  if Count = 0 then Exit;
  for I := 0 to Count - 2 do
    Result.Folder := Result.Folder + FieldValue(S, '\', I) + '\';
  if Result.Folder <> '' then
    SetLength(Result.Folder, Length(Result.Folder) - 1);
  if Count > 1 then
    Result.Name :=  FieldValue(S, '\', Count - 1);
end;

function FileOpen(const FileName: string; Mode: LongWord): Integer;
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := Integer(CreateFile(PChar(FileName), AccessMode[Mode and 3],
    ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0));
end;

function FileCreate(const FileName: string): Integer;
begin
  Result := Integer(CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
    0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
end;

function FileRead(Handle: Integer; var Buffer; Count: LongWord): Integer;
begin
  if not ReadFile(THandle(Handle), Buffer, Count, LongWord(Result), nil) then
    Result := -1;
end;

function FileWrite(Handle: Integer; const Buffer; Count: LongWord): Integer;
begin
  if not WriteFile(THandle(Handle), Buffer, Count, LongWord(Result), nil) then
    Result := -1;
end;

function FileSeek(Handle, Offset, Origin: Integer): Integer;
begin
  Result := SetFilePointer(THandle(Handle), Offset, nil, Origin);
end;

procedure FileClose(Handle: Integer);
begin
  CloseHandle(THandle(Handle));
end;

function FileAge(const FileName: string): Integer;
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
        LongRec(Result).Lo) then Exit;
    end;
  end;
  Result := -1;
end;

function FileExists(const FileName: string): Boolean;
begin
  Result := FileAge(FileName) <> -1;
end;

function FileTempName(const Path: string = ''): string;
var
  TempPath: string;
begin
  TempPath := Path;
  if TempPath = '' then
  begin
    SetLength(TempPath, MAX_PATH);
    GetTempPath(MAX_PATH, PChar(TempPath));
    SetLength(TempPath, StrLen(PChar(TempPath)));
  end;
  if TempPath[Length(TempPath)] <> '\' then
    TempPath := TempPath + '\';
  SetLength(Result, MAX_PATH);
  GetTempFileName(PChar(TempPath), '~TM', 0, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
  DeleteFile(Result);
end;

function ExtractFileName(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('\:', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractFileExt(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.\:', FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

function ExtractFilePath(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('\:', FileName);
  Result := Copy(FileName, 1, I);
end;

function DeleteFile(const FileName: string): Boolean;
begin
  Result := Windows.DeleteFile(PChar(FileName));
end;

function RenameFile(const OldName, NewName: string): Boolean;
begin
  Result := MoveFile(PChar(OldName), PChar(NewName));
end;

procedure MessageNotify(const Msg: string);
begin
  MessageDialog(Msg, mtInformation, mbOk);
end;

function MessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TModalResult;
const
  Captions: array[TMsgDlgType] of string = (
    'Warning', 'Error', 'Information', 'Confirmation');
begin
  Result := MessageDialog(Msg, Captions[DlgType], DlgType, Buttons);
end;

function MessageDialog(const Msg: string; const Caption: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): TModalResult;
const
  Flags: array[TMsgDlgButtons] of LongWord = (
    MB_ABORTRETRYIGNORE, MB_OK, MB_OKCANCEL, MB_RETRYCANCEL,  MB_YESNO,
    MB_YESNOCANCEL);
  Icons: array[TMsgDlgType] of LongWord = (
    MB_ICONWARNING, MB_ICONERROR, MB_ICONINFORMATION, MB_ICONQUESTION);
begin
  Result := MessageBox(0, PChar(Msg), PChar(Caption), Flags[Buttons] or
    Icons[DlgType] or MB_TASKMODAL or MB_SETFOREGROUND or MB_TOPMOST);
end;

procedure ShowException(Msg: string);
begin
  MessageDialog(Msg, mtError, mbOk);
end;

function SysErrorMessage(ErrorCode: Integer): string;
var
  Len: Integer;
  Buffer: array[0..255] of Char;
begin
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, ErrorCode, 0, Buffer,
    SizeOf(Buffer), nil);
  while (Len > 0) and (Buffer[Len - 1] in [#0..#32, '.']) do Dec(Len);
  SetString(Result, Buffer, Len);
end;

procedure RaiseLastWin32Error;
var
  LastError: DWORD;
  Error: EWin32Error;
begin
  LastError := GetLastError;
  if LastError <> ERROR_SUCCESS then
    Error := EWin32Error.CreateFmt(SWin32Error, [LastError,
      SysErrorMessage(LastError)])
  else
    Error := EWin32Error.Create(SUnkWin32Error);
  Error.ErrorCode := LastError;
  raise Error;
end;

function Win32Check(RetVal: BOOL): BOOL;
begin
  if not RetVal then RaiseLastWin32Error;
  Result := RetVal;
end;

procedure OleError(ErrorCode: HResult);
begin
  raise EOleError.Create('', ErrorCode);
end;

procedure OleCheck(Result: HResult);
begin
  if not Succeeded(Result) then OleError(Result);
end;

function ProgIDToClassID(const ProgID: string): TGUID;
begin
  OleCheck(CLSIDFromProgID(PWideChar(WideString(ProgID)), Result));
end;

function CreateOleObject(const ClassName: string): IDispatch;
var
  ClassID: TCLSID;
begin
  ClassID := ProgIDToClassID(ClassName);
  OleCheck(CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or
    CLSCTX_LOCAL_SERVER, IDispatch, Result));
end;

{ Exception }

constructor Exception.Create(const Message: string);
begin
  inherited Create;
  FMessage := Message;
end;

constructor Exception.CreateFmt(const Message: string; const Args: array of const);
begin
  inherited Create;
  FMessage := Format(Message, Args);
end;

constructor Exception.CreateResFmt(Ident: Integer; const Args: array of const);
begin
  inherited Create;
  FMessage := Format(LoadStr(Ident), Args);
end;

{ EOleError }

constructor EOleError.Create(const Message: string; ErrorCode: Integer);
var
  S: string;
begin
  S := Message;
  if S = '' then
  begin
    S := SysErrorMessage(ErrorCode);
    if S = '' then
      S := Format(SOleError, [ErrorCode]);
  end;
  inherited Create(S);
  FErrorCode := ErrorCode;
end;

{ TList }

destructor TList.Destroy;
begin
  Clear;
end;

function TList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

procedure TList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TList.Delete(Index: Integer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Temp := Items[Index];
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Pointer));
  if Temp <> nil then
    Notify(Temp, lnDeleted);
end;

class procedure TList.Error(const Message: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EListError.CreateFmt(Message, [Data]) at ReturnAddr;
end;

class procedure TList.Error(Message: PResStringRec; Data: Integer);
begin
  TList.Error(LoadResString(Message), Data);
end;

procedure TList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SListIndexError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TList.Expand: TList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TList.First: Pointer;
begin
  Result := Get(0);
end;

function TList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Result := FList^[Index];
end;

procedure TList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then
    Error(@SListIndexError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FCount);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

function TList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(@SListIndexError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList^[NewIndex] := Item;
  end;
end;

procedure TList.Put(Index: Integer; Item: Pointer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Temp := FList^[Index];
  FList^[Index] := Item;
  if Temp <> nil then
    Notify(Temp, lnDeleted);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

function TList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TList.Pack;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if Items[I] = nil then
      Delete(I);
end;

procedure TList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure TList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(@SListCountError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure QuickSort(SortList: PPointerList; L, R: Integer;
  SCompare: TListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do
        Inc(I);
      while SCompare(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TList.Sort(Compare: TListSortCompare);
begin
  if (FList <> nil) and (Count > 0) then
    QuickSort(FList, 0, Count - 1, Compare);
end;

function TList.Extract(Item: Pointer): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(Item);
  if I >= 0 then
  begin
    Result := Item;
    FList^[I] := nil;
    Delete(I);
    Notify(Result, lnExtracted);
  end;
end;

procedure TList.Notify(Ptr: Pointer; Action: TListNotification);
begin
end;

{ TPersistent }

procedure TPersistent.Assign(Source: TPersistent);
begin
  if Source <> nil then Source.AssignTo(Self) else AssignError(nil);
end;

procedure TPersistent.AssignError(Source: TPersistent);
var
  SourceName: string;
begin
  if Source <> nil then
    SourceName := Source.ClassName else
    SourceName := 'nil';
  raise EConvertError.CreateFmt(SAssignError, [SourceName, ClassName]);
end;

procedure TPersistent.AssignTo(Dest: TPersistent);
begin
  Dest.AssignError(Self);
end;

{ TStrings }

function TStrings.Add(const S: string): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TStrings.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TStrings.Append(const S: string);
begin
  Add(S);
end;

procedure TStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TStrings.Assign(Source: TPersistent);
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TStrings(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

function TStrings.Equals(Strings: TStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

procedure TStrings.Error(const Message: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EStringListError.CreateFmt(Message, [Data]) at ReturnAddr;
end;

procedure TStrings.Error(Message: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Message), Data);
end;

procedure TStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TStrings.GetCapacity: Integer;
begin  // descendants may optionally override/replace this default implementation
  Result := Count;
end;

function TStrings.GetName(Index: Integer): string;
var
  P: Integer;
begin
  Result := Get(Index);
  P := Pos('=', Result);
  if P <> 0 then
    SetLength(Result, P-1) else
    SetLength(Result, 0);
end;

function TStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TStrings.GetTextStr: string;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S: string;
begin
  Count := GetCount;
  Size := 0;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + 2);
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    P^ := #13;
    Inc(P);
    P^ := #10;
    Inc(P);
  end;
end;

function TStrings.GetValue(const Name: string): string;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TStrings.IndexOf(const S: string): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareText(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

function TStrings.IndexOfName(const Name: string): Integer;
var
  P: Integer;
  S: string;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos('=', S);
    if (P <> 0) and (CompareText(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TStrings.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TStrings.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TStrings.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: string;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);
    SetTextStr(S);
  finally
    EndUpdate;
  end;
end;

procedure TStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TStrings.Put(Index: Integer; const S: string);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

procedure TStrings.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TStrings.SaveToStream(Stream: TStream);
var
  S: string;
begin
  S := GetTextStr;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

procedure TStrings.SetTextStr(const Value: string);
var
  P, Start: PChar;
  S: string;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not (P^ in [#0, #10, #13]) do Inc(P);
        SetString(S, Start, P - Start);
        Add(S);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  finally
    EndUpdate;
  end;
end;

procedure TStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TStrings.SetValue(const Name, Value: string);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + '=' + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

{ TStringList }

destructor TStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TStringList.Add(const S: string): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  InsertItem(Result, S);
end;

procedure TStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TStringItem));
  Changed;
end;

procedure TStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

function TStringList.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareText(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TStringList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FString;
end;

function TStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TStringList.IndexOf(const S: string): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

procedure TStringList.Insert(Index: Integer; const S: string);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S);
end;

procedure TStringList.InsertItem(Index: Integer; const S: string);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TStringList.Put(Index: Integer; const S: string);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FString := S;
  Changed;
end;

procedure TStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FObject := AObject;
  Changed;
end;

procedure TStringList.QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TStringItem));
  FCapacity := NewCapacity;
end;

procedure TStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function StringListCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareText(List.FList^[Index1].FString,
                            List.FList^[Index2].FString);
end;

procedure TStringList.Sort;
begin
  CustomSort(StringListCompare);
end;

procedure TStringList.CustomSort(Compare: TStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

{ TStream }

function TStream.GetPosition: Longint;
begin
  Result := Seek(0, 1);
end;

procedure TStream.SetPosition(Pos: Longint);
begin
  Seek(Pos, 0);
end;

function TStream.GetSize: Longint;
var
  Pos: Longint;
begin
  Pos := Seek(0, 1);
  Result := Seek(0, 2);
  Seek(Pos, 0);
end;

procedure TStream.SetSize(NewSize: Longint);
begin
  // default = do nothing  (read-only streams, etc)
end;

procedure TStream.ReadBuffer(var Buffer; Count: Longint);
begin
  if (Count <> 0) and (Read(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.WriteBuffer(const Buffer; Count: Longint);
begin
  if (Count <> 0) and (Write(Buffer, Count) <> Count) then
    raise EWriteError.Create(SWriteError);
end;

function TStream.CopyFrom(Source: TStream; Count: Longint): Longint;
const
  MaxBufSize = $F000;
var
  BufSize, N: Integer;
  Buffer: PChar;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;
  Result := Count;
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  GetMem(Buffer, BufSize);
  try
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.ReadBuffer(Buffer^, N);
      WriteBuffer(Buffer^, N);
      Dec(Count, N);
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

{ THandleStream }

constructor THandleStream.Create(AHandle: Integer);
begin
  FHandle := AHandle;
end;

function THandleStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FileRead(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;
end;

function THandleStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FileWrite(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;
end;

function THandleStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := FileSeek(FHandle, Offset, Origin);
end;

procedure THandleStream.SetSize(NewSize: Longint);
begin
  Seek(NewSize, soFromBeginning);
  Win32Check(SetEndOfFile(FHandle));
end;

{ TFileStream }

constructor TFileStream.Create(const FileName: string; Mode: Word);
begin
  if Mode = fmCreate then
  begin
    FHandle := FileCreate(FileName);
    if FHandle < 0 then
      raise EFCreateError.CreateFmt(SFCreateError, [FileName]);
  end else
  begin
    FHandle := FileOpen(FileName, Mode);
    if FHandle < 0 then
      raise EFOpenError.CreateFmt(SFOpenError, [FileName]);
  end;
end;

destructor TFileStream.Destroy;
begin
  if FHandle >= 0 then FileClose(FHandle);
end;

{ TCustomMemoryStream }

procedure TCustomMemoryStream.SetPointer(Ptr: Pointer; ASize: Longint);
begin
  FMemory := Ptr;
  FSize := ASize;
end;

function TCustomMemoryStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Result := FSize - FPosition;
    if Result > 0 then
    begin
      if Result > Count then Result := Count;
      Move(Pointer(Longint(FMemory) + FPosition)^, Buffer, Result);
      Inc(FPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TCustomMemoryStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TCustomMemoryStream.SaveToStream(Stream: TStream);
begin
  if FSize <> 0 then Stream.WriteBuffer(FMemory^, FSize);
end;

procedure TCustomMemoryStream.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ TMemoryStream }

const
  MemoryDelta = $2000; { Must be a power of 2 }

destructor TMemoryStream.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMemoryStream.Clear;
begin
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
end;

procedure TMemoryStream.LoadFromStream(Stream: TStream);
var
  Count: Longint;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetSize(Count);
  if Count <> 0 then Stream.ReadBuffer(FMemory^, Count);
end;

procedure TMemoryStream.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMemoryStream.SetCapacity(NewCapacity: Longint);
begin
  SetPointer(Realloc(NewCapacity), FSize);
  FCapacity := NewCapacity;
end;

procedure TMemoryStream.SetSize(NewSize: Longint);
var
  OldPosition: Longint;
begin
  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then Seek(0, soFromEnd);
end;

function TMemoryStream.Realloc(var NewCapacity: Longint): Pointer;
begin
  if NewCapacity > 0 then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  Result := Memory;
  if NewCapacity <> FCapacity then
  begin
    if NewCapacity = 0 then
    begin
      GlobalFreePtr(Memory);
      Result := nil;
    end else
    begin
      if Capacity = 0 then
        Result := GlobalAllocPtr(HeapAllocFlags, NewCapacity)
      else
        Result := GlobalReallocPtr(Memory, NewCapacity, HeapAllocFlags);
      if Result = nil then raise EStreamError.Create(SMemoryStreamError);
    end;
  end;
end;

function TMemoryStream.Write(const Buffer; Count: Longint): Longint;
var
  Pos: Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      if Pos > FSize then
      begin
        if Pos > FCapacity then
          SetCapacity(Pos);
        FSize := Pos;
      end;
      System.Move(Buffer, Pointer(Longint(FMemory) + FPosition)^, Count);
      FPosition := Pos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;

{ TStringStream }

constructor TStringStream.Create(const AString: string);
begin
  inherited Create;
  FDataString := AString;
end;

function TStringStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Length(FDataString) - FPosition;
  if Result > Count then Result := Count;
  Move(PChar(@FDataString[FPosition + 1])^, Buffer, Result);
  Inc(FPosition, Result);
end;

function TStringStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  SetLength(FDataString, (FPosition + Result));
  Move(Buffer, PChar(@FDataString[FPosition + 1])^, Result);
  Inc(FPosition, Result);
end;

function TStringStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := Length(FDataString) - Offset;
  end;
  if FPosition > Length(FDataString) then
    FPosition := Length(FDataString)
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

function TStringStream.ReadString(Count: Longint): string;
var
  Len: Integer;
begin
  Len := Length(FDataString) - FPosition;
  if Len > Count then Len := Count;
  SetString(Result, PChar(@FDataString[FPosition + 1]), Len);
  Inc(FPosition, Len);
end;

procedure TStringStream.WriteString(const AString: string);
begin
  Write(PChar(AString)^, Length(AString));
end;

procedure TStringStream.SetSize(NewSize: Longint);
begin
  SetLength(FDataString, NewSize);
  if FPosition > NewSize then FPosition := NewSize;
end;

{ TResourceStream }

constructor TResourceStream.Create(Instance: THandle; const ResName: string;
  ResType: PChar);
begin
  inherited Create;
  Initialize(Instance, PChar(ResName), ResType);
end;

constructor TResourceStream.CreateFromID(Instance: THandle; ResID: Integer;
  ResType: PChar);
begin
  inherited Create;
  Initialize(Instance, PChar(ResID), ResType);
end;

procedure TResourceStream.Initialize(Instance: THandle; Name, ResType: PChar);

  procedure Error;
  begin
    raise EResNotFound.CreateFmt(SResNotFound, [Name]);
  end;

begin
  HResInfo := FindResource(Instance, Name, ResType);
  if HResInfo = 0 then Error;
  HGlobal := LoadResource(Instance, HResInfo);
  if HGlobal = 0 then Error;
  SetPointer(LockResource(HGlobal), SizeOfResource(Instance, HResInfo));
end;

destructor TResourceStream.Destroy;
begin
  UnlockResource(HGlobal);
  FreeResource(HGlobal);
  inherited Destroy;
end;

function TResourceStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EStreamError.Create(SCantWriteResourceStreamError);
end;

{$ENDIF}

constructor TNodeCache.Create(Node: IXMLDOMNode; Selection: IXMLDOMNodeList);
begin
  Self.Node := Node;
  Self.Selection := Selection;
end;

constructor TDocParser.Create;
begin
  inherited Create;
  FDocument := CreateOleObject('Microsoft.XMLDOM') as IXMLDOMDocument;
  FDocument.async := False;
  FNode := FDocument;
  FList := TList.Create;
  Clear;
end;

destructor TDocParser.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TDocParser.Clear;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    TObject(FList[I]).Free;
  FList.Clear;
  FNode := FDocument;
  FSelection := nil;
end;

procedure TDocParser.SaveToFile(const FileName: string);
begin
  FDocument.save(FileName);
  Clear;
end;

procedure TDocParser.LoadFromFile(const FileName: string);
begin
  FDocument.load(FileName);
  Clear;
end;

function TDocParser.Next: Boolean;
begin
  if FSelection <> nil then
    FNode := FSelection.nextNode
  else
    FNode := nil;
  Result := FNode <> nil;
end;

function TDocParser.Push(const Section: string): Boolean;
begin
  FList.Add(TNodeCache.Create(FNode, FSelection));
  if FNode <> nil then
    FSelection := FNode.selectNodes(Section);
  Result := Next;
end;

procedure TDocParser.Pop;
var
  C: TNodeCache;
  I: Integer;
begin
  I := FList.Count - 1;
  if I > -1 then
  begin
    C := TNodeCache(FList[I]);
    FNode := C.Node;
    FSelection := C.Selection;
    C.Free;
    FList.Delete(I);
  end;
end;

function TDocParser.ReadInt(const Name: string; Default: Integer = 0): Integer;
begin
  Result := StrToIntDef(ReadString(Name), Default);
end;

function TDocParser.ReadString(const Name: string; Default: string = ''): string;
var
  N: IXMLDOMNode;
begin
  if FNode = nil then
  begin
    Result := Default;
    Exit;
  end;
  N := FNode.selectSingleNode(Name);
  if N = nil then
  begin
    Result := Default;
    Exit;
  end;
  Result := Trim(N.text);
  if Result = '' then Result := Default;
end;

function StrToFloatDef(const S: string; Default: Double): Double;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := Default;
end;

function TDocParser.ReadFloat(const Name: string; Default: Double = 0): Double;
begin
  Result := StrToFloatDef(ReadString(Name), Default);
end;

function TDocParser.ReadBool(const Name: string; Default: Boolean = False): Boolean;
var
  S: string;
begin
  S := UpperCase(ReadString(Name));
  if S = 'Y' then
    Result := True
  else if S = 'YES' then
    Result := True
  else if S = '1' then
    Result := True
  else if S = 'TRUE' then
    Result := True
  else if S = 'N' then
    Result := True
  else if S = 'NO' then
    Result := True
  else if S = '0' then
    Result := True
  else if S = 'FALSE' then
    Result := True
  else
    Result := Default;
end;

procedure TDocParser.WriteInt(const Name: string; Value: Integer);
begin
  WriteString(Name, IntToStr(Value));
end;

procedure TDocParser.WriteString(const Name, Value: string);
var
  S: TStringArray;
  R, N: IXMLDOMNode;
  I: Integer;
begin
  if FNode = nil then
    Exit;
  S := nil;
  N := FNode.selectSingleNode(Name);
  if N = nil then
  begin
    R := FNode;
    S := Split(Name, '/');
    for I := 0 to Length(S) - 1 do
    begin
      if Length(S[I]) = 0 then Exit;
      N := R.selectSingleNode(S[I]);
      if N = nil then
        if S[I][1] = '@' then
        begin
          N := FDocument.createAttribute(StrPas(@S[I][2]));
          R.attributes.setNamedItem(N);
          Break;
        end
        else
        begin
          N := FDocument.createElement(S[I]);
          R.appendChild(N);
        end;
      R := N;
    end;
  end;
  N.text := Trim(Value);
end;

procedure TDocParser.WriteFloat(const Name: string; Value: Double);
begin
  WriteString(Name, FloatToStr(Value));
end;

procedure TDocParser.WriteBool(const Name: string; Value: Boolean);
const
  BoolIdents: array[Boolean] of string = ('false', 'true');
begin
  WriteString(Name, BoolIdents[Value])
end;

function TDocParser.GetEmpty: Boolean;
begin
  Result := (FNode = nil) or (FSelection = nil);
end;

{ TUtilityWindow }

threadvar
  CreationWindow: TUtilityWindow;

function UtilityProc(Wnd: HWND; uMsg: Cardinal; wParam: LongInt; lParam: LongInt): Integer; stdcall;
var
  UtilityWindow: TUtilityWindow;
  Msg: TMessage;
begin
  if CreationWindow <> nil then
  begin
    UtilityWindow := CreationWindow;
    UtilityWindow.FHandle := Wnd;
    CreationWindow := nil;
    SetWindowLong(Wnd, GWL_USERDATA, Integer(UtilityWindow));
  end
  else
    UtilityWindow := TUtilityWindow(GetWindowLong(Wnd, GWL_USERDATA));
  Result := DefWindowProc(Wnd, uMsg, wParam, lParam);
  if UtilityWindow <> nil then
  try
    Msg.Msg := uMsg;
    Msg.wParam := wParam;
    Msg.lParam := lParam;
    Msg.Result := Result;
    UtilityWindow.FOwner.Dispatch(Msg);
    Result := Msg.Result;
    if Msg.Msg = WM_DESTROY then
      UtilityWindow.FHandle := 0;
  except
    on E: Exception do
      MessageBox(0, PChar(E.ClassName + ': ' + E.Message), 'Error',
        MB_ICONERROR or MB_OK or MB_TASKMODAL);
  end;
end;

constructor TUtilityWindow.Create(AOwner: TObject; WindowClass: string = '');
var
  WindowName: string;
  WndClass: TWndClass;
begin
  inherited Create;
  FOwner := AOwner;
  if WindowClass = '' then
    WindowClass := ClassName;
  WindowName := FOwner.ClassName;
  if not GetClassInfo(HInstance, PChar(WindowClass), WndClass) then
    with WndClass do
    begin
      FillChar(WndClass, SizeOf(TWndClass), #0);
      lpfnWndProc := @UtilityProc;
      lpszClassName := PChar(WindowClass);
      hInstance := MainInstance;
      if Windows.RegisterClass(WndClass) = 0 then
        RaiseLastWin32Error;
    end;
  CreationWindow := Self;
  try
    CreateWindow(PChar(WindowClass), PChar(WindowName), 0, 0, 0, 0, 0, 0, 0, 0, nil);
  except
    CreationWindow := nil;
  end;
  if FHandle = 0 then
    RaiseLastWin32Error
  else
  	ShowWindow(FHandle, SW_HIDE);  
end;

destructor TUtilityWindow.Destroy;
var
  WindowClass: string;
begin
  if FHandle <> 0 then
    DestroyWindow(FHandle);
  WindowClass := ClassName;
  if FindWindow(PChar(WindowClass), nil) = 0 then
    Windows.UnregisterClass(PChar(WindowClass), MainInstance);
end;

{$IFDEF BARE}

{ TThread }

const
  CM_EXECPROC = $8FFF;
  CM_DESTROYWINDOW = $8FFE;

type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;

var
  ThreadLock: TRTLCriticalSection;
  ThreadWindow: HWND;
  ThreadCount: Integer;

procedure FreeThreadWindow;
begin
  if ThreadWindow <> 0 then
  begin
    DestroyWindow(ThreadWindow);
    ThreadWindow := 0;
  end;
end;

function ThreadWndProc(Window: HWND; Message, wParam, lParam: Longint): Longint; stdcall;
begin
  case Message of
    CM_EXECPROC:
      with TThread(lParam) do
      begin
        Result := 0;
        try
          FSynchronizeException := nil;
          FMethod;
        except
          if RaiseList <> nil then
          begin
            FSynchronizeException := PRaiseFrame(RaiseList)^.ExceptObject;
            PRaiseFrame(RaiseList)^.ExceptObject := nil;
          end;
        end;
      end;
    CM_DESTROYWINDOW:
      begin
        EnterCriticalSection(ThreadLock);
        try
          Dec(ThreadCount);
          if ThreadCount = 0 then
            FreeThreadWindow;
        finally
          LeaveCriticalSection(ThreadLock);
        end;
        Result := 0;
      end;
  else
    Result := DefWindowProc(Window, Message, wParam, lParam);
  end;
end;

var
  ThreadWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @ThreadWndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TThreadWindow');

procedure AddThread;

  function AllocateWindow: HWND;
  var
    TempClass: TWndClass;
    ClassRegistered: Boolean;
  begin
    ThreadWindowClass.hInstance := HInstance;
    ClassRegistered := GetClassInfo(HInstance, ThreadWindowClass.lpszClassName,
      TempClass);
    if not ClassRegistered or (TempClass.lpfnWndProc <> @ThreadWndProc) then
    begin
      if ClassRegistered then
        Windows.UnregisterClass(ThreadWindowClass.lpszClassName, HInstance);
      Windows.RegisterClass(ThreadWindowClass);
    end;
    Result := CreateWindow(ThreadWindowClass.lpszClassName, '', 0,
      0, 0, 0, 0, 0, 0, HInstance, nil);
  end;

begin
  EnterCriticalSection(ThreadLock);
  try
    if ThreadCount = 0 then
      ThreadWindow := AllocateWindow;
    Inc(ThreadCount);
  finally
    LeaveCriticalSection(ThreadLock);
  end;
end;

procedure RemoveThread;
begin
  EnterCriticalSection(ThreadLock);
  try
    if ThreadCount = 1 then
      PostMessage(ThreadWindow, CM_DESTROYWINDOW, 0, 0);
  finally
    LeaveCriticalSection(ThreadLock);
  end;
end;

{ TThread }

function ThreadProc(Thread: TThread): Integer;
var
  FreeThread: Boolean;
begin
  try
    Thread.Execute;
  finally
    FreeThread := Thread.FFreeOnTerminate;
    Result := Thread.FReturnValue;
    Thread.FFinished := True;
    Thread.DoTerminate;
    if FreeThread then Thread.Free;
    EndThread(Result);
  end;
end;

constructor TThread.Create(CreateSuspended: Boolean);
var
  Flags: DWORD;
begin
  inherited Create;
  AddThread;
  FSuspended := CreateSuspended;
  Flags := 0;
  if CreateSuspended then Flags := CREATE_SUSPENDED;
  FHandle := BeginThread(nil, 0, @ThreadProc, Pointer(Self), Flags, FThreadID);
end;

destructor TThread.Destroy;
begin
  if not FFinished and not Suspended then
  begin
    Terminate;
    WaitFor;
  end;
  if FHandle <> 0 then CloseHandle(FHandle);
  inherited Destroy;
  RemoveThread;
end;

procedure TThread.CallOnTerminate;
begin
  if Assigned(FOnTerminate) then FOnTerminate(Self);
end;

procedure TThread.DoTerminate;
begin
  if Assigned(FOnTerminate) then Synchronize(CallOnTerminate);
end;

const
  Priorities: array [TThreadPriority] of Integer =
   (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL);

function TThread.GetPriority: TThreadPriority;
var
  P: Integer;
  I: TThreadPriority;
begin
  P := GetThreadPriority(FHandle);
  Result := tpNormal;
  for I := Low(TThreadPriority) to High(TThreadPriority) do
    if Priorities[I] = P then Result := I;
end;

procedure TThread.SetPriority(Value: TThreadPriority);
begin
  SetThreadPriority(FHandle, Priorities[Value]);
end;

procedure TThread.Synchronize(Method: TThreadMethod);
begin
  FSynchronizeException := nil;
  FMethod := Method;
  SendMessage(ThreadWindow, CM_EXECPROC, 0, Longint(Self));
  if Assigned(FSynchronizeException) then raise FSynchronizeException;
end;

procedure TThread.SetSuspended(Value: Boolean);
begin
  if Value <> FSuspended then
    if Value then
      Suspend else
      Resume;
end;

procedure TThread.Suspend;
begin
  FSuspended := True;
  SuspendThread(FHandle);
end;

procedure TThread.Resume;
begin
  if ResumeThread(FHandle) = 1 then FSuspended := False;
end;

procedure TThread.Terminate;
begin
  FTerminated := True;
end;

function TThread.WaitFor: LongWord;
var
  Msg: TMsg;
  H: THandle;
begin
  H := FHandle;
  if GetCurrentThreadID = MainThreadID then
    while MsgWaitForMultipleObjects(1, H, False, INFINITE,
      QS_SENDMESSAGE) = WAIT_OBJECT_0 + 1 do PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE)
  else WaitForSingleObject(H, INFINITE);
  GetExitCodeThread(H, Result);
end;

procedure ExceptHandler(ExceptObject: TObject; ExceptAddr: Pointer); far;
begin
  if ExceptObject is Exception then
    ShowException((ExceptObject as Exception).Message)
  else
    ShowException(Format(SExceptionMessage, [ExceptAddr]));
end;

initialization
  EmptyStr := '';
  NullStr := @EmptyStr;
  InitializeCriticalSection(ThreadLock);
  CoInitialize(nil);
  ErrorProc := @ExceptHandler;
  ExceptProc := @ExceptHandler;
  ExceptionClass := Exception;
finalization
  DeleteCriticalSection(ThreadLock);
{$ELSE}
initialization
  CoInitialize(nil);
{$ENDIF}
end.
