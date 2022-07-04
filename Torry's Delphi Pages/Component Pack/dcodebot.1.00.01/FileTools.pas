
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit FileTools;

interface

{$I STD.INC}

uses
  Windows, ShellAPI, SysUtils, Classes, ComObj, ActiveX, ShlObj, Registry;

{ CreateAssociation procedure }

procedure CreateAssociation(const Name, Extension: string; Icon: Integer = 0);

{ GetStandardApplication function }

const
  saNotepad = 0;
  saPaint = 1;
  saCalculator = 2;
  saCDPlayer = 3;
  saMediaPlayer = 4;

function GetStandardApplication(Application: Integer): string;

{ GetTargetFileName }

function GetTargetFileName: string;

{ GetLocalFileName }

function GetLocalFileName(const FileName: string): string;

{ GetTempFileName function }

function GetTempFileName(const Path: string = ''): string;

function FileTempName(const Path: string = ''): string;

{ ChangeFileName function }

function ChangeFileName(const FilePath: string; FileName: string): string;

{ GetShortFileName function }

function GetShortFileName(const FileName: string): string;

{ GetSystemPath function }

function GetSystemPath: string;

{ GetWindowsPath function }

function GetWindowsPath: string;

{ GetConsolePath function }

function GetConsolePath: string;

{ GetFileList procedure }

procedure GetFileList(const Directory: string; Strings: TStrings;
  Wildcards: string = '*.*');

{ SlashedPath procedure }

function SlashedPath(const S: string): string;

{ FileOperation function }

const
  foCopy = FO_COPY;
  foDelete = FO_DELETE;
  foMove = FO_MOVE;
  foRename = FO_RENAME;

type
  TOperationOption = (ooAllowUndo, ooConfirmMouse, ooFileOnly, ooMultiDestFile,
    ooNoConfirmation, ooNoConfirmMkDir, ooRenameCollision, ooSilent,
    ooSimpleProgress);
  TOperationOptions = set of TOperationOption;

function FileOperation(const Source, Dest: string; Operation: Integer;
  Options: TOperationOptions = [ooSimpleProgress]): Boolean;

{ ExecuteVerb procedure }

procedure ExecuteVerb(Handle: HWND; ItemList: PItemIDList; const Verb: string); overload;
procedure ExecuteVerb(Handle: HWND; const DisplayName: string; const Verb: string); overload;

{ TDrive  }

type
  TDriveKind = (dkUnknown, dkRemovable, dkFixed, dkRemote, dkCDRom, dkRAMDisk);

  TDrive = class
  private
    FName: string;
    FVolume: string;
    FSerialNumber: Cardinal;
    FFileSystem: string;
    FFreeSpace: Cardinal;
    FSize: Cardinal;
    FKind: TDriveKind;
    FEjected: Boolean;
  public
    property Name: string read FName;
    property Volume: string read FVolume;
    property SerialNumber: Cardinal read FSerialNumber;
    property FileSystem: string read FFileSystem;
    property FreeSpace: Cardinal read FFreeSpace;
    property Size: Cardinal read FSize;
    property Kind: TDriveKind read FKind;
    property Ejected: Boolean read FEjected;
  end;

{ TDrives }

  TDrives = class
  private
    FDrives: TList;
    FRootDrive: TDrive;
    function GetDrive(Index: Integer): TDrive;
    function GetDriveCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Drive[Index: Integer]: TDrive read GetDrive; default;
    property DriveCount: Integer read GetDriveCount;
    property RootDrive: TDrive read FRootDrive;
  end;

function GetDriveSerialNumber: Cardinal;

{ TSearchThread }

type
  TSearchThread = class(TThread)
  private
    FAborted: Boolean;
    FData: Pointer;
    FEvent: TThreadMethod;
    FFieldAddress: Pointer;
    procedure CallTerminate;
    procedure CheckFieldAddress;
  protected
    procedure DoTerminate; override;
    procedure SyncEvent(const Event: TThreadMethod);
  public
    constructor Create(FieldAddress: Pointer = nil);
    procedure Abort;
    procedure Search;
    property Data: Pointer read FData write FData;
    property Aborted: Boolean read FAborted;
  end;

{ TFileSearch }

  TFindFileEvent = procedure(Sender: TObject; const FileName: string; Size: Integer) of object;
  TFindDirectoryEvent = procedure(Sender: TObject; const Directory: string) of object;

  TFileSearch = class(TSearchThread)
  private
    FFileName: string;
    FFileSize: Integer;
    FDirectory: string;
    FPath: string;
    FRecurse: Boolean;
    FWildcards: string;
    FOnFindDirectory: TFindDirectoryEvent;
    FOnFindFile: TFindFileEvent;
    procedure SetPath(const Value: string);
  protected
    procedure CallFindDirectory;
    procedure CallFindFile;
    procedure Execute; override;
    procedure FindDirectory; virtual;
    procedure FindFile; virtual;
    property Directory: string read FDirectory;
    property FileName: string read FFileName;
  public
    property Path: string read FPath write SetPath;
    property Recurse: Boolean read FRecurse write FRecurse;
    property Wildcards: string read FWildcards write FWildcards;
    property OnFindDirectory: TFindDirectoryEvent read FOnFindDirectory
      write FOnFindDirectory;
    property OnFindFile: TFindFileEvent read FOnFindFile write FOnFindFile;
  end;

{ TModuleItem }

  TModuleResource = class;

  TModuleItem = class(TPersistent)
  private
    FOwner: TModuleResource;
    FHandle: THandle;
    FID: Integer;
    FName: string;
    function GetHandle: THandle;
  protected
    function CreateHandle: THandle; virtual; abstract;
    procedure DestroyHandle; virtual; abstract;
    function GetResName: PChar;
    class function GetResType: PChar; virtual; abstract;
    function LoadData(var Data: Pointer): Integer;
  public
    constructor Create(AOwner: TModuleResource; ResName: PChar);
    destructor Destroy; override;
    procedure SaveToFile(const Filename: string);
    procedure SaveToStream(Stream: TStream);
    property Handle: THandle read GetHandle;
    property ID: Integer read FID write FID;
    property Name: string read FName;
    property Owner: TModuleResource read FOwner;
  end;

  TModuleItemClass = class of TModuleItem;

{ TBitmapModuleItem }

  TBitmapModuleItem = class(TModuleItem)
  protected
    function CreateHandle: THandle; override;
    procedure DestroyHandle; override;
    class function GetResType: PChar; override;
  end;

{ TIconModuleItem }

  TIconModuleItem = class(TModuleItem)
  protected
    function CreateHandle: THandle; override;
    procedure DestroyHandle; override;
    class function GetResType: PChar; override;
  end;

{ TModuleResource }

  TModuleResource = class(TPersistent)
  private
    FItemClass: TModuleItemClass;
    FList: TList;
    FHandle: THandle;
    function GetCount: Integer;
    function GetItem(Index: Integer): TModuleItem;
    function AddItem(ResName: PChar): TModuleItem;
  public
    constructor Create(const Module: string; ItemClass: TModuleItemClass); overload;
    constructor Create(const Module: HMODULE; ItemClass: TModuleItemClass); overload;
    destructor Destroy; override;
    procedure Realize;
    property Count: Integer read GetCount;
    property Handle: THandle read FHandle;
    property Item[index: Integer]: TModuleItem read GetItem; default;
  end;

(* TVersionInformation class

   Because these DLLs are shared components, they reside in the Windows "system"
   directory. That is, they should be installed into the directory indicated by
   the return value from GetSystemDirectory. Installing in this directory, like
   other shared system components, must be done carefully to avoid overwriting
   newer versions of the DLL that other applications previously installed may be
   relying upon. To retrieve the version information for the installed files, use
   the GetFileVersionInfo APIs provided by the Windows version API. You should
   only install a new DLL if its version information, give by the dwFileVersionMS
   and dwFileVersionLS fields of the VS_FIXEDFILEINFO structure are larger than
   the same version information of the already installed DLL.

 The VS_VERSION_INFO structure is the root structure that contains all other
 file-version information structures.

 VS_VERSION_INFO {
     WORD  wLength;
     WORD  wValueLength;
     WORD  wType;
     WCHAR szKey[];
     WORD  Padding1[];
     VS_FIXEDFILEINFO Value;
     WORD  Padding2[];
     WORD  Children[];
 }; *)

  TLangAndCodePage = record
    wLanguage: WORD;
    wCodePage: WORD;
  end;
  PLangAndCodePage = ^TLangAndCodePage;

  TFixedFileInfo = record
    dwSignature: DWORD;
    dwStrucVersion: DWORD;
    dwFileVersionMS: DWORD;
    dwFileVersionLS: DWORD;
    dwProductVersionMS: DWORD;
    dwProductVersionLS: DWORD;
    dwFileFlagsMask: DWORD;
    dwFileFlags: DWORD;
    dwFileOS: DWORD;
    dwFileType: DWORD;
    dwFileSubtype: DWORD;
    dwFileDateMS: DWORD;
    dwFileDateLS: DWORD;
  end;
  PFixedFileInfo = ^TFixedFileInfo;

  TVersionInfo = packed record
    wLength: WORD;
    wValueLength: WORD;
    wType: WORD;
    szKey: array [0..13] of Char;
    Value: TFixedFileInfo;
  end;
  PVersionInfo = ^TVersionInfo;

  TVersionInformation = class(TObject)
  private
    FFileName: string;
    FVersionInfoSize: Integer;
    FVersionInfo: Pointer;
    FUseSysDir: Boolean;
    procedure SetFileName(const Value: string);
    function GetMajorVersion: Integer;
    function GetMinorVersion: Integer;
    function GetVersionItem(Index: Integer): string;
  protected
    procedure Clear;
    property VerionInfo: Pointer read FVersionInfo;
  public
    destructor Destroy; override;
    property UseSysDir: Boolean read FUseSysDir write FUseSysDir;
    property MajorVersion: Integer read GetMajorVersion;
    property MinorVersion: Integer read GetMinorVersion;
    property FileName: string read FFileName write SetFileName;
    property CompanyName: string index 0 read GetVersionItem;
    property FileDescription: string index 1 read GetVersionItem;
    property FileVersion: string index 2 read GetVersionItem;
    property InternalName: string index 3 read GetVersionItem;
    property LegalCopyright: string index 4 read GetVersionItem;
    property OriginalFilename: string index 5 read GetVersionItem;
    property ProductName: string index 6 read GetVersionItem;
    property ProductVersion: string index 7 read GetVersionItem;
  end;

{ Undocumented shell folder CoClass IDs }

const
  CLSID_NetworkPlaces: TGUID = (
    D1:$208D2C60; D2:$3AEA; D3:$1069; D4:($A2,$D7,$08,$00,$2B,$30,$30,$9D));
  CLSID_NetworkDomain: TGUID = (
    D1:$46E06680; D2:$4BF0; D3:$11D1; D4:($83,$EE,$00,$A0,$C9,$0D,$C8,$49));
  CLSID_NetworkServer: TGUID = (
    D1:$C0542A90; D2:$4BF0; D3:$11D1; D4:($83,$EE,$00,$A0,$C9,$0D,$C8,$49));
  CLSID_NetworkShare: TGUID = (
    D1:$54A754C0; D2:$4BF0; D3:$11D1; D4:($83,$EE,$00,$A0,$C9,$0D,$C8,$49));
  CLSID_MyComputer: TGUID = (
    D1:$20D04FE0; D2:$3AEA; D3:$1069; D4:($A2,$D8,$08,$00,$2B,$30,$30,$9D));
  CLSID_Internet: TGUID = (
    D1:$871C5380; D2:$42A0; D3:$1069; D4:($A2,$EA,$08,$00,$2B,$30,$30,$9D));
  CLSID_ShellFSFolder: TGUID = (
    D1:$F3364BA0; D2:$65B9; D3:$11CE; D4:($A9,$BA,$00,$AA,$00,$4A,$E8,$37));
  CLSID_RecycleBin: TGUID = (
    D1:$645FF040; D2:$5081; D3:$101B; D4:($9F,$08,$00,$AA,$00,$2F,$95,$4E));
  CLSID_ControlPanel: TGUID = (
    D1:$21EC2020; D2:$3AEA; D3:$1069; D4:($A2,$DD,$08,$00,$2B,$30,$30,$9D));
  CLSID_Printers: TGUID = (
    D1:$2227A280; D2:$3AEA; D3:$1069; D4:($A2,$DE,$08,$00,$2B,$30,$30,$9D));
  CLSID_MyDocuments: TGUID = (
    D1:$450D8FBA; D2:$AD25; D3:$11D0; D4:($98,$A8,$08,$00,$36,$1B,$11,$03));
  CLSID_PrintersAndFaxes: TGUID = (
    D1:$2227A280; D2:$3AEA; D3:$1069; D4:($A2,$DE,$08,$00,$2B,$30,$30,$9D));

//HKEY_CLASSES_ROOT\CLSID\{2227A280-3AEA-1069-A2DE-08002B30309D}

{ Item list manipulation routines }

function ILCreateFromPath(const Path: string): PItemIDList;
function ILAppendID(pidl: PItemIDList; const ItemID: TSHItemID;
  AddToEnd: Boolean): PItemIDList;
function ILClone(pidl: PItemIDList): PItemIDList;
function ILCloneFirst(pidl: PItemIDList): PItemIDList;
function ILCombine(pidl1, pidl2: PItemIDList): PItemIDList;
function ILFindChild(pidlParent: PItemIDList;
  pidlChild: PItemIDList): PItemIDList;
function ILFindLastID(pidl: PItemIDList): PItemIDList;
procedure ILFree(pidl: PItemIDList);
function ILGetCount(pidl: PItemIDList): Integer;
function ILGetNext(pidl: PItemIDList): PItemIDList;
function ILGetSize(pidl: PItemIDList): Integer;
function ILIsChild(pidlParent, pidlChild: PItemIDList): Boolean;
function ILIsEqual(pidl1, pidl2: PItemIDList): Boolean;
function ILIsParent(pidlParent, pidlChild: PItemIDList;
  Immediate: Boolean): Boolean;
function ILIsRoot(pidl: PItemIDList): Boolean;
function ILRemoveLastID(pidl: PItemIDList): Boolean;

{ Shell helper functions }

function ExtractStrRet(const StrRet: TStrRet; pidl: PItemIDList): string;

{ TShellNode }

type
  TSpecialFolder = (
    sfDesktop, sfInternet, sfPrograms, sfControls, sfPrinters, sfPersonal,
    sfFavorites, sfStartup, sfRecent, sfSendto, sfBitBucket, sfStartmenu,
    sfDesktopDirectory, sfDrives, sfNetwork, sfNethood, sfFonts,
    sfTemplates, sfCommonStartMenu, sfCommonPrograms, sfCommonStartup,
    sfCommonDesktopDirectory, sfAppData, sfPrintHood, sfAltStartup,
    sfCommonAltStartup, sfCommonFavorites, sfInternetCache, sfCookies,
    sfHistory);

  TShellNode = class;

  TShellNodeArray = array of TShellNode;

  TShellNode = class(TPersistent)
  private
    FAbsoluteList: PItemIDList;
    FItem: TShellNodeArray;
    FParent: TShellNode;
    FRelativeList: PItemIDList;
    FShellFolder: IShellFolder;
    function GetCount: Integer;
    function GetHasChildren: Boolean;
    function GetItem(Index: Integer): TShellNode;
    function GetName: string;
    function GetPath: string;
  protected
    procedure Initialize; virtual;
  public
    constructor Create(Parent: TShellNode; ItemList: PItemIDList);
    constructor CreateFromList(ItemList: PItemIDList);
    constructor CreateFromFolder(SpecialFolder: TSpecialFolder);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function Clone: TShellNode;
		function  Execute(Wnd: HWND; const Verb: string): Boolean;
    function GetAttributes(Flags: UINT): UINT;
    function IsEqual(Node: TShellNode): Boolean;
    property AbsoluteList: PItemIDList read FAbsoluteList;
    property HasChildren: Boolean read GetHasChildren;
    property RelativeList: PItemIDList read FRelativeList;
    property ShellFolder: IShellFolder read FShellFolder;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TShellNode read GetItem; default;
    property Parent: TShellNode read FParent;
    property Name: string read GetName;
    property Path: string read GetPath;
  end;

  TShellNodeClass = class of TShellNode;

function MRandom: Double; overload;
function MRandom(Low, High: Integer): Integer; overload;
procedure MRandSeed(Seed: Integer);

implementation

uses
  StrConst;

var
  Malloc: IMalloc;

procedure CreateAssociation(const Name, Extension: string; Icon: Integer = 0);
var
  Reg: TRegistry;
  S: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey('.' + Extension, True);
    Reg.WriteString('', Extension + '_autofile');
    Reg.CloseKey;
    Reg.OpenKey(Extension + '_autofile', True);
    Reg.WriteString('', Name);
    Reg.CloseKey;
    Reg.OpenKey(Extension + '_autofile\shell\open\command', True);
    S := ParamStr(0);
    Reg.WriteString('', S + ' "%1"');
    Reg.CloseKey;
    if Icon <> 0 then
    begin
      Reg.OpenKey(Extension + '_autofile\DefaultIcon', True);
      Reg.WriteString('', S + ',' + IntToStr(Icon));
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function GetStandardApplication(Application: Integer): string;
const
  Applications: array[saNotepad..saMediaPlayer] of PChar =
    ('NOTEPAD.EXE', 'PBRUSH.EXE', 'CALC.EXE', 'CDPLAYER.EXE', 'MPLAYER.EXE');
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  Result := '';
  if (Application >= saNotepad) or (Application <= saMediaPlayer) then
  begin
    GetWindowsDirectory(Buffer, MAX_PATH);
    Result := Format('%s\%s', [StrPas(Buffer),
      StrPas(Applications[Application])]);
    if not FileExists(Result) then
      Result := '';
  end;
end;

function GetTargetFileName: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if IsLibrary then
  begin
    GetModuleFileName(HInstance, Buffer, SizeOf(Buffer));
    Result := Buffer;
  end
  else
    Result := ParamStr(0);
end;

function GetLocalFileName(const FileName: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := SlashedPath(Result);
  Result := Result + FileName;
end;

function GetTempFileName(const Path: string = ''): string;
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
  TempPath := SlashedPath(TempPath);
  SetLength(Result, MAX_PATH);
  Windows.GetTempFileName(PChar(TempPath), '~TM', 0, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
  DeleteFile(Result);
end;

function FileTempName(const Path: string = ''): string;
begin
  Result := GetTempFileName;
end;

function ChangeFileName(const FilePath: string; FileName: string): string;
var
  Path: string;
  S: string;
begin
  Path := FilePath;
  S := ExtractFileName(Path);
  SetLength(Path, Length(Path) - Length(S));
  Result := Path + FileName;
end;

function GetShortFileName(const FileName: string): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if GetShortPathName(PChar(FileName), Buffer, MAX_PATH) > 0 then
    Result := Buffer
  else
    RaiseLastWin32Error;
end;

function GetSystemPath: string;
begin
  SetLength(Result, MAX_PATH);
  GetSystemDirectory(PChar(Result), MAX_PATH);
  SetLength(Result, StrLen(PChar(Result)));
  Result := SlashedPath(Result);
end;

function GetWindowsPath: string;
begin
  SetLength(Result, MAX_PATH);
  GetWindowsDirectory(PChar(Result), MAX_PATH);
  SetLength(Result, StrLen(PChar(Result)));
  Result := SlashedPath(Result);
end;

function GetConsolePath: string;
const
  Consoles: array[Boolean] of string = ('command.com', 'cmd.exe');
var
  Info: TOSVersionInfo;
  S: string;
begin
  Info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(Info);
  S := Consoles[Info.dwPlatformId = VER_PLATFORM_WIN32_NT];
  if FileExists(GetSystemPath + S) then
    Result := GetSystemPath + S
  else if FileExists(GetWindowsPath + S) then
    Result := GetWindowsPath + S
  else
    Result := '';
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
  S := SlashedPath(S);
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

function SlashedPath(const S: string): string;
begin
  Result := S;
  if Result <> '' then
    if Result[Length(Result)] <> '\' then
      Result := Result + '\';
end;

function FileOperation(const Source, Dest: string; Operation: Integer;
  Options: TOperationOptions = [ooSimpleProgress]): Boolean;
const
  Flags: array[TOperationOption] of Integer = (FOF_ALLOWUNDO, FOF_CONFIRMMOUSE,
    FOF_FILESONLY, FOF_MULTIDESTFILES, FOF_NOCONFIRMATION, FOF_NOCONFIRMMKDIR,
    FOF_RENAMEONCOLLISION, FOF_SILENT, FOF_SIMPLEPROGRESS);
var
  FileOpStruct: TSHFileOpStruct;
  LastMode: Integer;
  I: TOperationOption;
begin
  Result := False;
  LastMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  with FileOpStruct do
  try
    Wnd := 0;
    wFunc := Operation;
    pFrom := PChar(Source);
    pTo := PChar(Dest);
    fFlags := 0;
    for I := Low(TOperationOption) to High(TOperationOption) do
      if I in Options then
        fFlags := fFlags or Flags[I];
    fAnyOperationsAborted := True;
    hNameMappings := nil;
    lpszProgressTitle := nil;
    if SHFileOperation(FileOpStruct) = 0 then
      Result := not fAnyOperationsAborted;
  finally
    SetErrorMode(LastMode);
  end
end;

{ ShowProperties }

procedure ExecuteVerb(Handle: HWND; ItemList: PItemIDList; const Verb: string);
var
  Desktop: IShellFolder;
  Folder: IShellFolder;
  ParentList: PItemIDList;
  RelativeList: PItemIDList;
  ContextMenu: IContextMenu;
  CommandInfo: TCMInvokeCommandInfo;
begin
  ParentList := ILClone(ItemList);
  if ParentList <> nil then
  try
    ILRemoveLastID(ParentList);
    OleCheck(SHGetDesktopFolder(Desktop));
    OleCheck(Desktop.BindToObject(ParentList, nil, IID_IShellFolder, Folder));
    RelativeList := ILFindChild(ParentList, ItemList);
    OleCheck(Folder.GetUIObjectOf(Handle, 1, RelativeList, IID_IContextMenu,
      nil, ContextMenu));
    FillChar(CommandInfo, SizeOf(TCMInvokeCommandInfo), #0);
    with CommandInfo do
    begin
      cbSize := SizeOf(TCMInvokeCommandInfo);
      hwnd := Handle;
      lpVerb := PChar(Verb);
      nShow := SW_SHOW;
    end;
    OleCheck(ContextMenu.InvokeCommand(CommandInfo));
  finally
    ILFree(ParentList);
  end;
end;

procedure ExecuteVerb(Handle: HWND; const DisplayName, Verb: string);
var
  ItemList: PItemIDList;
begin
  ItemList := ILCreateFromPath(PChar(DisplayName));
  try
    ExecuteVerb(Handle, ItemList, Verb)
  finally
    ILFree(ItemList);
  end;
end;

{ TDrives }

constructor TDrives.Create;

  procedure GetDriveInfo(Drive: PChar; var FreeSpace, Size: Cardinal);
  var
    BytesPerSector: Cardinal;
    SectorsPerCluster: Cardinal;
    NumberOfFreeClusters: Cardinal;
    TotalNumberOfClusters: Cardinal;
  begin
    FreeSpace := 0;
    Size := 0;
    if GetDiskFreeSpace(Drive, SectorsPerCluster, BytesPerSector,
      NumberOfFreeClusters, TotalNumberOfClusters) then
    begin
      FreeSpace := BytesPerSector * SectorsPerCluster * NumberOfFreeClusters;
      Size := BytesPerSector * SectorsPerCluster * TotalNumberOfClusters;
    end;
  end;

const
  MAX_DRIVESTRINGS = MAX_PATH * 24;
var
  LastMode: Cardinal;
  Drive: TDrive;
  Dummy: Cardinal;
  S: string;
  P, Pos: PChar;
begin
  FDrives := TList.Create;
  LastMode := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
  try
    Setlength(S, MAX_DRIVESTRINGS);
    GetLogicalDriveStrings(MAX_DRIVESTRINGS, PChar(S));
    P := PChar(S);
    Pos := P;
    repeat
      while Pos^ <> #0 do
        Inc(Pos);
      Drive := TDrive.Create;
      with Drive do
      begin
        FName := P;
        SetLength(FVolume, MAX_PATH);
        SetLength(FFileSystem, MAX_PATH);
        GetVolumeInformation(P, PChar(FVolume), MAX_PATH, @FSerialNumber, Dummy,
          Dummy, PChar(FFileSystem), MAX_PATH);
        SetLength(FVolume, StrLen(PChar(FVolume)));
        SetLength(FFileSystem,StrLen(PChar(FFileSystem)));
        case GetDriveType(P) of
          DRIVE_REMOVABLE: FKind := dkRemovable;
          DRIVE_FIXED: FKind := dkFixed;
          DRIVE_REMOTE: FKind := dkRemote;
          DRIVE_CDROM: FKind := dkCDRom;
          DRIVE_RAMDISK: FKind := dkRAMDisk;
        else
          FKind := dkUnknown;
        end;
        GetDriveInfo(P, FFreeSpace, FSize);
        if UpperCase(FName) = Copy(GetWindowsPath, 1, Length(FName)) then
          FRootDrive := Drive;
      end;
      FDrives.Add(Drive);
      Inc(Pos);
      P := Pos;
    until P^ = #0;
  finally
    SetErrorMode(LastMode);
  end;
end;

destructor TDrives.Destroy;
var
  I: Integer;
begin
  for I := 0 to FDrives.Count - 1 do
    TObject(FDrives[I]).Free;
  FDrives.Free;
  inherited Destroy;
end;

function TDrives.GetDrive(Index: Integer): TDrive;
begin
  Result := TDrive(FDrives[Index]);
end;

function TDrives.GetDriveCount: Integer;
begin
  Result := FDrives.Count;
end;

function GetDriveSerialNumber: Cardinal;
begin
  with TDrives.Create do
  try
    Result := RootDrive.SerialNumber;
  finally
    Free;
  end;
end;

{ TSearchThread }

constructor TSearchThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

procedure TSearchThread.Abort;
begin
  FAborted := True;
  Terminate;
  if (FFieldAddress <> nil) and (TObject(FFieldAddress^) = Self) then
    TObject(FFieldAddress^) := nil;
end;

procedure TSearchThread.CallTerminate;
begin
  OnTerminate(Self);
  if FFieldAddress <> nil then
    TObject(FFieldAddress^) := nil;
end;

procedure TSearchThread.CheckFieldAddress;
begin
  if (FFieldAddress = nil) or (TObject(FFieldAddress^) = Self) then
    FEvent
  else
    Abort;
end;

procedure TSearchThread.DoTerminate;
begin
  if Assigned(OnTerminate) then
    SyncEvent(CallTerminate);
end;

procedure TSearchThread.Search;
begin
  Resume;
end;

procedure TSearchThread.SyncEvent(const Event: TThreadMethod);
begin
  FEvent := Event;
  Synchronize(CheckFieldAddress);
  FEvent := nil;
end;

{ TFileSearch }

procedure TFileSearch.CallFindDirectory;
begin
  FOnFindDirectory(Self, FDirectory);
end;

procedure TFileSearch.CallFindFile;
begin
  FOnFindFile(Self, FFileName, FFileSize);
end;

procedure TFileSearch.FindDirectory;
begin
  if (not Terminated) and Assigned(FOnFindDirectory) then
    SyncEvent(CallFindDirectory);
end;

procedure TFileSearch.FindFile;
begin
  if (not Terminated) and Assigned(FOnFindFile) then
    SyncEvent(CallFindFile);
end;

procedure TFileSearch.Execute;
var
  WildcardStrings: TStrings;

  procedure Search(const Dir: string);
  var
    SearchRec: TSearchRec;
    SearchResult: Integer;
    Directories: TStrings;
    I: Integer;
  begin
    Directories := TStringList.Create;
    try
      FDirectory := Dir;
      SyncEvent(FindDirectory);
      for I := 0 to WildcardStrings.Count - 1 do
        if not Terminated then
        begin
          SearchResult := FindFirst(Dir + WildcardStrings[I], faAnyFile and (not faDirectory), SearchRec);
          while (not Terminated) and (SearchResult = 0) do
          begin
            FFileName := Dir + SearchRec.Name;
            FFileSize := SearchRec.Size;
            SyncEvent(FindFile);
            SearchResult := FindNext(SearchRec);
          end;
          FindClose(SearchRec);
        end;
      if FRecurse then
      begin
        SearchResult := FindFirst(Dir + '*.*', faDirectory, SearchRec);
        while SearchResult = 0 do
        begin
          if SearchRec.Name[Length(SearchRec.Name)] <> '.' then
            Directories.Add(Dir + SearchRec.Name + '\');
          SearchResult := FindNext(SearchRec);
        end;
        FindClose(SearchRec);
      end;
      for I := 0 to Directories.Count - 1 do
        if not Terminated then
          Search(Directories[I]);
    finally
      Directories.Free;
    end;
  end;

var
  Wildcard: string;
  StartPos: PChar;
  P: PChar;
begin
  WildcardStrings := TStringList.Create;
  try
    if FWildcards <> '' then
    begin
      P := PChar(FWildcards);
      while P^ <> #0 do
      begin
        StartPos := P;
        while (P^ <> #0) and (P^ <> ';') do
          Inc(P);
        SetString(Wildcard, StartPos, P - StartPos);
        WildcardStrings.Add(Wildcard);
        if P^ = ';' then
          Inc(P);
      end;
    end
    else
      WildCardStrings.Add('*.*');
    if not Terminated then
      Search(FPath);
  finally
    WildcardStrings.Free;
  end;
end;

procedure TFileSearch.SetPath(const Value: string);
begin
  FPath := Value;
  if FPath <> '' then
    FPath := SlashedPath(FPath);
end;

{ TModuleItem }

constructor TModuleItem.Create(AOwner: TModuleResource; ResName: PChar);
begin
  inherited Create;
  FOwner := AOwner;
  if LongRec(ResName).Hi = 0 then
    FID := Integer(ResName)
  else
    FName := ResName;
end;

destructor TModuleItem.Destroy;
begin
  if FHandle <> 0 then
    DestroyHandle;
  inherited Destroy;
end;

function TModuleItem.GetHandle: THandle;
begin
  if FHandle = 0 then
    FHandle := CreateHandle;
  Result := FHandle;
end;

function TModuleItem.GetResName: PChar;
begin
  if FName <> '' then
    Result := PChar(FName)
  else
    Result := MakeIntResource(FID);
end;

function TModuleItem.LoadData(var Data: Pointer): Integer;
var
  Module: THandle;
  ResInfo: THandle;
begin
  Module := FOWner.Handle;
  ResInfo := FindResource(Module, GetResName, GetResType);
  Data := LockResource(ResInfo);
  Result := SizeofResource(Module, ResInfo);
end;

procedure TModuleItem.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TModuleItem.SaveToStream(Stream: TStream);
var
  Data: Pointer;
  DataSize: Integer;
begin
  DataSize := LoadData(Data);
  Stream.Write(PChar(Data)^, DataSize);
end;

{ TBitmapModuleItem }

function TBitmapModuleItem.CreateHandle: THandle;
begin
  Result := LoadBitmap(Owner.Handle, GetResName);
end;

procedure TBitmapModuleItem.DestroyHandle;
begin
  DeleteObject(Handle);
end;

class function TBitmapModuleItem.GetResType: PChar;
begin
  Result := RT_BITMAP;
end;

{ TIconModuleItem }

function TIconModuleItem.CreateHandle: THandle;
begin
  Result := LoadIcon(Owner.Handle, GetResName);
end;

procedure TIconModuleItem.DestroyHandle;
begin
  DeleteObject(Handle);
end;

class function TIconModuleItem.GetResType: PChar;
begin
  Result := RT_ICON;
end;

{ TModuleResource }

constructor TModuleResource.Create(const Module: HMODULE; ItemClass: TModuleItemClass);
begin
  inherited Create;
  FHandle := Module;
  if FHandle = 0 then
    raise Exception.Create('invalid module');
  FItemClass := ItemClass;
end;

constructor TModuleResource.Create(const Module: string; ItemClass: TModuleItemClass);
begin
  Create(LoadLibrary(PChar(Module)), ItemClass)
end;

destructor TModuleResource.Destroy;
var
  I: Integer;
begin
  if FList <> nil then
  begin
    for I := FList.Count - 1 downto 0 do
      TObject(FList[I]).Free;
    FList.Free;
  end;
  if FHandle <> 0 then
    FreeLibrary(FHandle); // change to detect creation type
  inherited Destroy;
end;

function TModuleResource.AddItem(ResName: PChar): TModuleItem;
begin
  Result := FItemClass.Create(Self, ResName);
  with Result do
    if Handle <> 0 then
    begin
      DestroyHandle;
      FHandle := 0;
      FList.Add(Result);
    end
    else
    begin
      Free;
      Result := nil;
    end;
end;

function EnumResourceProc(Module: THandle; ResType: Integer; ResName: PChar;
  ModuleResource: TModuleResource): Boolean; stdcall;
{var
  ModuleItem: TModuleItem;}
begin
  ModuleResource.AddItem(ResName);
  Result := True;
end;

procedure TModuleResource.Realize;
begin
  if FList = nil then
  begin
    FList := TList.Create;
    EnumResourceNames(FHandle, FItemClass.GetResType, @EnumResourceProc, Integer(Self));
  end;
end;

function EnumCountProc(Module: THandle; ResType: Integer; ResName: PChar;
  Count: PInteger): Boolean; stdcall;
begin
  Inc(Count^);
  Result := True;
end;

function TModuleResource.GetCount: Integer;
var
  I: Integer;
begin
  if FList <> nil then
    Result := FList.Count
  else
  begin
    I := 0;
    EnumResourceNames(FHandle, FItemClass.GetResType, @EnumCountProc, Integer(@I));
    Result := I;
  end;
end;

function TModuleResource.GetItem(Index: Integer): TModuleItem;
begin
  Realize;
  Result := TModuleItem(FList[Index]);
end;

{ TVersionInformation }

const
  VER_STRINGCOUNT = 8;
  VersionNames: array [0..VER_STRINGCOUNT - 1] of string =
    ('CompanyName',
     'FileDescription',
     'FileVersion',
     'InternalName',
     'LegalCopyright',
     'OriginalFilename',
     'ProductName',
     'ProductVersion');

destructor TVersionInformation.Destroy;
begin
  Clear;
end;

procedure TVersionInformation.Clear;
begin
  if Assigned(FVersionInfo) then
  begin
    FreeMem(FVersionInfo);
    FVersionInfo := nil;
    FVersionInfoSize := 0;
  end;
end;

procedure TVersionInformation.SetFileName(const Value: string);
var
  Handle: THandle;
  SysDir: PChar;
begin
  Clear;
  SysDir := StrAlloc(MAX_PATH);
  GetSystemDirectory(SysDir, MAX_PATH - 1);
  if FUseSysDir then
    FFileName := SysDir + '\' + Value
  else
    FFileName := Value;
  FVersionInfoSize := GetFileVersionInfoSize(PChar(FFileName), Handle);
  if FVersionInfoSize <> 0 then
  begin
    GetMem(FVersionInfo, FVersionInfoSize);
    GetFileVersionInfo(PChar(FFileName), Handle, FVersionInfoSize, FVersionInfo);
  end;
  StrDispose(SysDir);
end;

function TVersionInformation.GetMajorVersion: Integer;
begin
  Result := 0;
  if (FVersionInfo <> nil) and (PVersionInfo(FVersionInfo).wValueLength <> 0) then
    Result := LongRec(PVersionInfo(FVersionInfo).Value.dwFileVersionMS).Hi;
end;

function TVersionInformation.GetMinorVersion: Integer;
begin
  Result := 0;
  if (FVersionInfo <> nil) and (PVersionInfo(FVersionInfo).wValueLength <> 0) then
    Result := LongRec(PVersionInfo(FVersionInfo).Value.dwFileVersionMS).Lo;
end;

function TVersionInformation.GetVersionItem(Index: Integer): string;
var
  Buffer: Pointer;
  BufferSize: Cardinal;
  CodePage: PLangAndCodePage absolute Buffer;
  S: string;
begin
  Result := '';
  if (FVersionInfo <> nil) and (VerQueryValue(FVersionInfo,
    '\VarFileInfo\Translation', Buffer, BufferSize)) then
  begin
    S := Format('\StringFileInfo\%.4x%.4x\%s', [CodePage.wLanguage,
      CodePage.wCodePage, VersionNames[Index]]);
    if VerQueryValue(FVersionInfo, PChar(S), Buffer, BufferSize) then
      Result := PChar(Buffer);
  end;
end;

{ Item list manipulation routines }

var
  pidlDesktop: PItemIDList;

function ILCreateFromPath(const Path: string): PItemIDList;
var
  Desktop: IShellFolder;
  WideName: array[0..MAX_PATH] of WideChar;
  Dummy: Cardinal;
	S: string;
begin
	Result := nil;
	S := UpperCase(Trim(Path));
  if S = '' then Exit;
  if S = 'DESKTOP' then
    Result := ILClone(pidlDesktop)
  else if S = 'MY NETWORK PLACES' then
    SHGetSpecialFolderLocation(0, CSIDL_NETWORK, Result)
  else if S = 'MY DOCUMENTS' then
    SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, Result)
  else if S = 'MY COMPUTER' then
    SHGetSpecialFolderLocation(0, CSIDL_DRIVES, Result)
  else if S = 'RECYCLE BIN' then
    SHGetSpecialFolderLocation(0, CSIDL_BITBUCKET, Result)
  else if S = 'CONTROL PANEL' then
    SHGetSpecialFolderLocation(0, CSIDL_CONTROLS, Result)
  else if S = 'FONTS' then
    SHGetSpecialFolderLocation(0, CSIDL_FONTS, Result)
  else
  begin
    OleCheck(SHGetDesktopFolder(Desktop));
    StringToWideChar(S, WideName, MAX_PATH);
    Dummy := 0;
    if Desktop.ParseDisplayName(0, nil, WideName, Dummy,
      Result, Dummy) <> S_OK then Result := nil;
  end;
end;

function ILAppendID(pidl: PItemIDList; const ItemID: TSHItemID;
  AddToEnd: Boolean): PItemIDList;
var
  ListSize: LongWord;
  P: PByte;
begin
  ListSize := ILGetSize(pidl);
  Result := Malloc.Alloc(ListSize + ItemID.cb + 2);
  P := Pointer(Result);
  FillMemory(Result, ListSize + ItemID.cb+ 2, 0);
  if AddToEnd then
  begin
    CopyMemory(P, pidl, ListSize);
    Inc(P, ListSize);
    CopyMemory(P, @ItemID, ItemID.cb);
  end
  else
  begin
    CopyMemory(P, @ItemID, ItemID.cb);
    Inc(P, ItemID.cb);
    CopyMemory(P, pidl, ListSize);
  end;
  ILFree(pidl);
end;

function ILCombine(pidl1, pidl2: PItemIDList): PItemIDList;
var
  Size1: LongWord;
  Size2: LongWord;
  P: PByte;
begin
  Result := nil;
  Size1 := ILGetSize(pidl1);
  Size2 :=  ILGetSize(pidl2);
  if Size1 + Size2 = 0 then
    Exit;
  Result := Malloc.Alloc(Size2 + Size1 + 2);
  P := Pointer(Result);
  FillMemory(Result, Size1 + Size2 + 2, 0);
  if Size1 > 0 then
  begin
    CopyMemory(P, pidl1, Size1);
    Inc(P, Size1);
  end;
  if Size2 > 0 then
    CopyMemory(P, pidl2, Size2);
end;

function ILClone(pidl: PItemIDList): PItemIDList;
var
  I: Integer;
begin
  Result := nil;
  I := ILGetSize(pidl);
  if I > 0 then
  begin
    Result := Malloc.Alloc(I + 2);
    FillMemory(Result, I + 2, 0);
    CopyMemory(Result, pidl, I);
  end;
end;

function ILCloneFirst(pidl: PItemIDList): PItemIDList;
begin
  Result := nil;
  if pidl <> nil then
  begin
    Result := Malloc.Alloc(pidl.mkid.cb + 2);
    FillMemory(Result, pidl.mkid.cb + 2, 0);
    CopyMemory(Result, pidl, pidl.mkid.cb);
  end;
end;

function ILFindChild(pidlParent: PItemIDList;
  pidlChild: PItemIDList): PItemIDList;
var
  Size: Integer;
begin
  Result := nil;
  Size := ILGetSize(pidlParent);
  if (Size < ILGetSize(pidlChild)) and CompareMem(pidlParent,  pidlChild,
    Size) then
  begin
    Result := pidlChild;
    Inc(PByte(Result), Size);
  end;
end;

function ILFindLastID(pidl: PItemIDList): PItemIDList;
begin
  Result := nil;
  if pidl <> nil then
    repeat
      Result := pidl;
      Inc(PByte(pidl), pidl.mkid.cb);
    until pidl.mkid.cb = 0;
end;

procedure ILFree(pidl: PItemIDList);
begin
  if pidl<> nil then
    Malloc.Free(pidl);
end;

function ILGetCount(pidl: PItemIDList): Integer;
begin
  Result := 0;
  if pidl <> nil then
    repeat
      Inc(Result);
      Inc(PByte(pidl), pidl.mkid.cb);
    until pidl.mkid.cb = 0;
end;

function ILGetNext(pidl: PItemIDList): PItemIDList;
begin
  Result := nil;
  if pidl <> nil then
  begin
    Result := pidl;
    Inc(PByte(Result), Result.mkid.cb);
    if Result.mkid.cb = 0 then
      Result := nil;
  end;
end;

function ILGetSize(pidl: PItemIDList): Integer;
begin
  Result := 0;
  if pidl <> nil then
    repeat
      Inc(Result, pidl.mkid.cb);
      Inc(PByte(pidl), pidl.mkid.cb);
    until pidl.mkid.cb = 0;
end;

function ILIsChild(pidlParent, pidlChild: PItemIDList): Boolean;
var
  Size: Integer;
begin
	Result := False;
	if ILGetCount(pidlParent) = 1 then
  	Result := ILIsRoot(pidlParent) and (not (ILIsRoot(pidlChild)));
  if not Result then
  begin
	  Size := ILGetSize(pidlParent);
  	if Size < ILGetSize(pidlChild) then
  		Result := CompareMem(pidlParent, pidlChild, Size);
	end;
end;

function UndocumentedILIsEqual(pidl1, pidl2: PItemIDList): Boolean; stdcall; external 'shell32.dll' index 21;

function ILIsEqual(pidl1, pidl2: PItemIDList): Boolean;
{var
  Size: Integer;
begin
  Size := ILGetSize(pidl1);
  Result := (Size = ILGetSize(pidl2)) and CompareMem(pidl1, pidl2, Size);
end;

5/6/2006 as per Gustavo comments at http://www.codebot.org/delphi/#477 }
begin
  Result := UndocumentedILIsEqual(pidl1, pidl2);
end;

function ILIsParent(pidlParent, pidlChild: PItemIDList;
  Immediate: Boolean): Boolean;
begin
  Result := ILIsRoot(pidlParent);
  if Result then
    if Immediate then
      Result := ILGetCount(pidlChild) = 1
  else
  begin
    Result := ILFindChild(pidlParent, pidlChild) <> nil;
    if Result and Immediate then
      Result := ILGetCount(pidlParent) = ILGetCount(pidlChild) - 1;
   end;
end;

function ILIsRoot(pidl: PItemIDList): Boolean;
begin
  Result := ILIsEqual(pidl, pidlDesktop);
end;

function ILRemoveLastID(pidl: PItemIDList): Boolean;
var
  Item: PItemIDList;
begin
  Result := False;
  if pidl <> nil then
  begin
    Inc(PByte(pidl), pidl.mkid.cb);
    Result := pidl.mkid.cb <> 0;
    if Result then
    begin
      repeat
        Item := pidl;
        Inc(PByte(pidl), pidl.mkid.cb);
      until pidl.mkid.cb = 0;
      Item.mkid.cb := 0;
    end;
  end;
end;

{ Shell helper functions }

function ExtractStrRet(const StrRet: TStrRet; pidl: PItemIDList): string;
begin
  Result := '';
  case StrRet.uType of
    STRRET_WSTR:
      begin
        Result := WideCharToString(StrRet.pOleStr);
        Malloc.Free(StrRet.pOleStr);
      end;
    STRRET_OFFSET: Result := PChar(LongWord(pidl) + StrRet.uOffset);
    STRRET_CSTR: Result := StrRet.cStr;
  end;
end;

{ TShellNode }

const
  SpecialFolderMap: array[TSpecialFolder] of LongWord = (
    CSIDL_DESKTOP, CSIDL_INTERNET, CSIDL_PROGRAMS, CSIDL_CONTROLS,
    CSIDL_PRINTERS, CSIDL_PERSONAL, CSIDL_FAVORITES, CSIDL_STARTUP,
    CSIDL_RECENT, CSIDL_SENDTO, CSIDL_BITBUCKET, CSIDL_STARTMENU,
    CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_NETWORK, CSIDL_NETHOOD,
    CSIDL_FONTS, CSIDL_TEMPLATES, CSIDL_COMMON_STARTMENU,
    CSIDL_COMMON_PROGRAMS, CSIDL_COMMON_STARTUP,
    CSIDL_COMMON_DESKTOPDIRECTORY, CSIDL_APPDATA, CSIDL_PRINTHOOD,
    CSIDL_ALTSTARTUP, CSIDL_COMMON_ALTSTARTUP, CSIDL_COMMON_FAVORITES,
    CSIDL_INTERNET_CACHE, CSIDL_COOKIES, CSIDL_HISTORY);

constructor TShellNode.Create(Parent: TShellNode; ItemList: PItemIDList);
begin
  inherited Create;
  FParent := Parent;
  FRelativeList := ItemList;
  if FParent = nil then
  begin
    SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, FAbsoluteList);
    SHGetDesktopFolder(FShellFolder);
  end
  else
  begin
    FAbsoluteList := ILCombine(FParent.FAbsoluteList, FRelativeList);
    FParent.ShellFolder.BindToObject(FRelativeList, nil, IID_IShellFolder,
      FShellFolder);
  end;
  Initialize;
end;

constructor TShellNode.CreateFromList(ItemList: PItemIDList);
begin
  if (ItemList = nil) or ILIsRoot(ItemList) then
    Create(nil, nil)
  else
    Create(TShellNodeClass(ClassType).Create(nil, nil), ItemList);
end;

constructor TShellNode.CreateFromFolder(SpecialFolder: TSpecialFolder);
var
  ItemList: PItemIDList;
begin
  if SpecialFolder = sfDesktop then
    Create(nil, nil)
  else
  begin
    SHGetSpecialFolderLocation(0, SpecialFolderMap[SpecialFolder], ItemList);
    CreateFromList(ItemList);
  end;
end;

destructor TShellNode.Destroy;
begin
  Clear;
  FShellFolder := nil;
  ILFree(FAbsoluteList);
  ILFree(FRelativeList);
  if (FParent <> nil) and (FParent.FItem = nil) then
    FParent.Free;
  inherited Destroy;
end;

procedure TShellNode.Assign(Source: TPersistent);
var
  SourceNode: TShellNode;
begin
  if Source is TShellNode then
  begin
    SourceNode := Source as TShellNode;
    Clear;
    ILFree(FAbsoluteList);
    ILFree(FRelativeList);
    FShellFolder := SourceNode.ShellFolder;
    FAbsoluteList := ILClone(SourceNode.AbsoluteList);
  end
  else
    inherited Assign(Source);
end;

procedure TShellNode.Clear;
var
  I: Integer;
begin
  for I := 0 to Length(FItem) - 1 do
    FItem[I].Free;
  FItem := nil;
end;

function TShellNode.Clone: TShellNode;
begin
  Result := TShellNodeClass(ClassType).Create(nil, nil);
  Result.Assign(Self);
end;

function TShellNode.Execute(Wnd: HWND; const Verb: string): Boolean;
var
  ContextMenu: IContextMenu;
  CommandInfo: TCMInvokeCommandInfo;
begin
	if FParent = nil then
  begin
		Result := False;
  	Exit;
	end;
  OleCheck(FParent.ShellFolder.GetUIObjectOf(Wnd, 1,
    FRelativeList, IID_IContextMenu, nil, ContextMenu));
  FillChar(CommandInfo, SizeOf(TCMInvokeCommandInfo), #0);
  with CommandInfo do
  begin
    cbSize := SizeOf(TCMInvokeCommandInfo);
    hwnd := Wnd;
    lpVerb := PChar(Verb);
    nShow := SW_SHOWNORMAL;
  end;
  Result := ContextMenu.InvokeCommand(CommandInfo) = S_OK;
end;

procedure TShellNode.Initialize;
begin
end;

function TShellNode.GetCount: Integer;

  function Compare(Left, Right: TShellNode): Integer;
  begin
    Result :=  SmallInt(FShellFolder.CompareIDs(0, Left.FRelativeList,
      Right.FRelativeList));
  end;

  procedure Sort(L, R: Integer);
  var
    I, J: Integer;
    P, T: Pointer;
  begin
    repeat
      I := L;
      J := R;
      P := FItem[(L + R) shr 1];
      repeat
        while Compare(FItem[I], P) < 0 do
          Inc(I);
        while Compare(FItem[J], P) > 0 do
          Dec(J);
        if I <= J then
        begin
          T := FItem[I];
          FItem[I] := FItem[J];
          FItem[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        Sort(L, J);
      L := I;
    until I >= R;
  end;

var
  Cursor: HCURSOR;
  EnumList: IEnumIDList;
  NewItem: PItemIDList;
  Dummy: LongWord;
begin
  Result := Length(FItem);
  if (Result = 0) and HasChildren then
  begin
    Cursor := SetCursor(LoadCursor(0, IDC_WAIT));
    try
      if FShellFolder.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_INCLUDEHIDDEN, EnumList) = S_OK then
        while EnumList.Next(1, NewItem, Dummy) = S_OK do
        begin
          Inc(Result);
          SetLength(FItem, Result);
          FItem[Result - 1] := TShellNodeClass(ClassType).Create(Self, NewItem);
        end;
      if Length(FItem) > 0 then
        Sort(0, Length(FItem) - 1);
    finally
      Cursor := SetCursor(Cursor);
      DestroyCursor(Cursor);
    end;
  end;
end;

function TShellNode.GetAttributes(Flags: UINT): UINT;
begin
  if FParent <> nil then
  begin
    FParent.ShellFolder.GetAttributesOf(1, FRelativeList, Flags);
    Result := Flags;
  end
  else
    Result := 0;
end;

function TShellNode.IsEqual(Node: TShellNode): Boolean;
begin
  if Node = nil then
    Result := False
  else
    Result := ILIsEqual(FAbsoluteList, Node.AbsoluteList);    
end;

function TShellNode.GetHasChildren: Boolean;
begin
  if FParent <> nil then
		Result := GetAttributes(SFGAO_HASSUBFOLDER) and SFGAO_HASSUBFOLDER = SFGAO_HASSUBFOLDER
  else
  	Result := True;
end;

function TShellNode.GetItem(Index: Integer): TShellNode;
begin
  Result := nil;
  if (Index < 0) or (Index > GetCount - 1) then
    Exit;
  Result := FItem[Index];
end;

function GetDisplayName(pidl: PitemIDList): string;
const
  Flags = SHGFI_DISPLAYNAME or SHGFI_PIDL;
var
  SHFileInfo: TSHFileInfo;
begin
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  SHGetFileInfo(PChar(pidl), 0, SHFileInfo, SizeOf(TSHFileInfo), Flags);
  Result := StrPas(SHFileInfo.szDisplayName);
end;

function TShellNode.GetName: string;
var
  StrRet: TStrRet;
begin
  if ILIsRoot(FAbsoluteList) then
  begin
    ShellFolder.GetDisplayNameOf(nil, SHGDN_INFOLDER, StrRet);
    Result := ExtractStrRet(StrRet, nil);
  end
  else
    Result := GetDisplayName(FAbsoluteList);
    {Result := GetDisplayName(FAbsoluteList);
    //Parent.FShellFolder.GetDisplayNameOf(FRelativeList, SHGDN_INFOLDER,
      //StrRet)
  else
    ;
  Result := ExtractStrRet(StrRet, FRelativeList);
  if Result = '' then
  if ILIsRoot()
    Result := GetDisplayName(FAbsoluteList);}
end;

function TShellNode.GetPath: string;
var
  ItemPath: array[0..MAX_PATH] of Char;
begin
  if SHGetPathFromIDList(FAbsoluteList, ItemPath) then
    Result := ItemPath
  else
    Result := '';
end;

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

initialization
  CoGetMalloc(1, Malloc);
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, pidlDesktop);
finalization
  ILFree(pidlDesktop);
  Malloc := nil;
end.

