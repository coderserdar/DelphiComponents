unit API_files;

{$WARN SYMBOL_PLATFORM OFF}

//------------------------------------------------------------------------------
// file handling routines and other file related things
//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
//  * browsefolderdialog moved to just external function
//
// r1.32, 12092009, ari pikivirta
//  * added PathCombine function
//  * added function CombineFiles
//
// r1.31, 15082009, ari pikivirta
//  * added some prepared get special folder functions, like TemplatesFolder: String
//
// r1.30, 11082009, ari pikivirta
//  * added ParseListDetails to parse details on findfiles function result
//  * added function UpdateFiles to update folder contents with source
//  * copy files or directory doesn't rename on collision by default
//  * find files allow passing relatepath as parameter
//
// r1.29, 07082009, ari pikivirta
//  * removed prefilled CSIDL functions from the component
//  * added features to AppDataFolder function 
//
// r1.28, 30062009, ari pikivirta
//  * exported file operations to be able to use them outside component
//  * added checking of fAnyOperationsAborted in file operations
//  * added StartAssociatedExe function
//  * updated GetFileType function
//  * added IsASCIIFile(Filename) function
//
// r1.27, 24062009, ari pikivirta
//  * fixed bug on get CSIDL folders that didn't remove spaces on the result
//  * fixed bug on AppDataFolder function
//
// r1.26, 15062009, ari pikivirta
//  * function AppDataFolder( ApplicationName ) added
//
// r1.25, 12062009, ari pikivirta
//  * added function IsEmptyFolder(dirname)
//  * minor cleaning on functions
//
// r1.24, 02062009, ari pikivirta
//  * added functions to get and change file extension (FileExtension)
//  * listed all CSIDLs to the source below exported functions
//  * fixed bug on GetTempFile and added new function GetTempFilename
//
// r1.23, 02012009, ari pikivirta
//  * added functions FileTimeToTDateTime and TDateTimeToFileTime
//
// r1.22, 03102008, ari pikivirta
//  * added possibility to add details to find files
//
// r1.21, 17092008, ari pikivirta
//  * findfiles changed to ignore case
//
// r1.20, 18062008, ari pikivirta
//  * rewrote findfiles and findfolders functions
//
// r1.19, 16062008, ari pikivirta
//  * added function GetFileType
//  * added function SetFileDateTime
//  * moved more functions and procedures to available outside class
//  * changed FileDTM name to FileDateTime
//
// r1.18, 14022008, ari pikivirta
//  * added allowundo parameter to delete dir and delete file functions
//
// r1.17/17082007, ari pikivirta
//  * added extracticon function (exported)
//  * added get file owner function (exported)
//  * added get file time(s) function
//
// r1.16/14082007, ari pikivirta
//  * added string and file crc32 calculation routines (exported)
//
// r1.15/18022007, ari pikivirta
//  * added FileDTM function to return date as tdatetime directly
//  * added FileSize function to return file size correctly (>2G)
//
// r1.14/17022007, ari pikivirta
//  * added FileInUse function
//  * added GetFileInfo function (returns data shown on file properties)
//  * added CalcCRC32 function
//  * fixed warning with pwidechar casting
//
// r1.13/04022007, ari pikivirta
//  * added copyfilestoclipboard and pastefilesfromclipboard functions
//  * added BytesToStr function
//
// r1.12/19112006, ari pikivirta
//  * added getsystemfolder and getwindowzfolder functions (CSIDL ver5.0)
//
// r1.11/03112006, ari pikivirta
//  * fixed findfolders function
//
// r1.10/29102006, ari pikivirta
//  * fixed problem with find files function to return also "." and ".." files
//    also function itself is now simplified
//
// r1.09/12102006, ari pikivirta
//  * added two findfiles function overloads
//  * added create directory and showfiles parameter to browsefolderdialog function
//  * added explorefolder function (just runs shellexecute "explore" on param)
//  * added GetUniversalName function, alias UNC
//  * added CalcCheckSum function
//  * added CompareFiles function, returns true if totally identical
//
// r1.08, ari pikivirta
// * added get special folder function (was lost for some reason..)
//
// r1.07, ari pikivirta
// * added showprogress property (for file operations)
// * added confirmation property (for file operations)
//
// r1.06, ari pikivirta
// * fixed find file procedure and added event on found files
// * added browse folders dialog function
//
// r1.05, ari pikivirta
// * added get revision information
// * fixed find files procedure (now results list of found files)
//
// r1.04, ari pikivirta
// * added get temporary filename (in temp folder)
//
// r1.03, ari pikivirta
// * added associate file procedure
// * added delete file function
// * added rename file function
// * added find file execution procedure

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Graphics, API_base, shlobj;

const
  CSIDL_DESKTOP = shlobj.CSIDL_DESKTOP;
  CSIDL_INTERNET = shlobj.CSIDL_INTERNET;
  CSIDL_PROGRAMS = shlobj.CSIDL_PROGRAMS;
  CSIDL_CONTROLS = shlobj.CSIDL_CONTROLS;
  CSIDL_PRINTERS = shlobj.CSIDL_PRINTERS;
  CSIDL_PERSONAL = shlobj.CSIDL_PERSONAL;
  CSIDL_FAVORITES = shlobj.CSIDL_FAVORITES;
  CSIDL_STARTUP = shlobj.CSIDL_STARTUP;
  CSIDL_RECENT = shlobj.CSIDL_RECENT;
  CSIDL_SENDTO = shlobj.CSIDL_SENDTO;
  CSIDL_BITBUCKET = shlobj.CSIDL_BITBUCKET;
  CSIDL_STARTMENU = shlobj.CSIDL_STARTMENU;
  CSIDL_MYDOCUMENTS = shlobj.CSIDL_MYDOCUMENTS;
  CSIDL_MYMUSIC = shlobj.CSIDL_MYMUSIC;
  CSIDL_MYVIDEO = shlobj.CSIDL_MYVIDEO;
  CSIDL_DESKTOPDIRECTORY = shlobj.CSIDL_DESKTOPDIRECTORY;
  CSIDL_DRIVES = shlobj.CSIDL_DRIVES;
  CSIDL_NETWORK = shlobj.CSIDL_NETWORK;
  CSIDL_NETHOOD = shlobj.CSIDL_NETHOOD;
  CSIDL_FONTS = shlobj.CSIDL_FONTS;
  CSIDL_TEMPLATES = shlobj.CSIDL_TEMPLATES;
  CSIDL_COMMON_STARTMENU = shlobj.CSIDL_COMMON_STARTMENU;
  CSIDL_COMMON_PROGRAMS = shlobj.CSIDL_COMMON_PROGRAMS; // = 23
  CSIDL_COMMON_STARTUP = shlobj.CSIDL_COMMON_STARTUP;
  CSIDL_COMMON_DESKTOPDIRECTORY = shlobj.CSIDL_COMMON_DESKTOPDIRECTORY;
  CSIDL_APPDATA = shlobj.CSIDL_APPDATA; // = 26
  CSIDL_PRINTHOOD = shlobj.CSIDL_PRINTHOOD;
  CSIDL_LOCAL_APPDATA = shlobj.CSIDL_LOCAL_APPDATA;
  CSIDL_ALTSTARTUP = shlobj.CSIDL_ALTSTARTUP;
  CSIDL_COMMON_ALTSTARTUP = shlobj.CSIDL_COMMON_ALTSTARTUP;
  CSIDL_COMMON_FAVORITES = shlobj.CSIDL_COMMON_FAVORITES;
  CSIDL_INTERNET_CACHE = shlobj.CSIDL_INTERNET_CACHE; // = 32
  CSIDL_COOKIES = shlobj.CSIDL_COOKIES; // = 33
  CSIDL_HISTORY = shlobj.CSIDL_HISTORY;
  CSIDL_COMMON_APPDATA = shlobj.CSIDL_COMMON_APPDATA; // = 35
  CSIDL_WINDOWS = shlobj.CSIDL_WINDOWS;
  CSIDL_SYSTEM = shlobj.CSIDL_SYSTEM; // = 37
  CSIDL_PROGRAM_FILES = shlobj.CSIDL_PROGRAM_FILES;
  CSIDL_MYPICTURES = shlobj.CSIDL_MYPICTURES;
  CSIDL_PROFILE = shlobj.CSIDL_PROFILE; // = 40
  CSIDL_SYSTEMX86 = shlobj.CSIDL_SYSTEMX86; // = 41
  CSIDL_PROGRAM_FILESX86 = shlobj.CSIDL_PROGRAM_FILESX86;
  CSIDL_PROGRAM_FILES_COMMON = shlobj.CSIDL_PROGRAM_FILES_COMMON;
  CSIDL_PROGRAM_FILES_COMMONX86 = shlobj.CSIDL_PROGRAM_FILES_COMMONX86;
  CSIDL_COMMON_TEMPLATES = shlobj.CSIDL_COMMON_TEMPLATES;
  CSIDL_COMMON_DOCUMENTS = shlobj.CSIDL_COMMON_DOCUMENTS;
  CSIDL_COMMON_ADMINTOOLS = shlobj.CSIDL_COMMON_ADMINTOOLS;
  CSIDL_ADMINTOOLS = shlobj.CSIDL_ADMINTOOLS;
  CSIDL_CONNECTIONS = shlobj.CSIDL_CONNECTIONS; // = 49
  CSIDL_COMMON_MUSIC = shlobj.CSIDL_COMMON_MUSIC; // = 53
  CSIDL_COMMON_PICTURES = shlobj.CSIDL_COMMON_PICTURES; // = 54
  CSIDL_COMMON_VIDEO = shlobj.CSIDL_COMMON_VIDEO;
  CSIDL_RESOURCES = shlobj.CSIDL_RESOURCES;
  CSIDL_RESOURCES_LOCALIZED = shlobj.CSIDL_RESOURCES_LOCALIZED;
  CSIDL_COMMON_OEM_LINKS = shlobj.CSIDL_COMMON_OEM_LINKS;
  CSIDL_CDBURN_AREA = shlobj.CSIDL_CDBURN_AREA; // = 59
  CSIDL_COMPUTERSNEARME = shlobj.CSIDL_COMPUTERSNEARME;
  CSIDL_PROFILES = shlobj.CSIDL_PROFILES; // = 62

type
  TAPIOnFileFoundEvent = procedure (Sender: TObject; const Found: string) of object;
  TAPIOnFileSearchReady = procedure (sender: tobject; const count: integer) of object;

  TAPI_files = class(TAPI_Custom_Component)
  private
    fhdserial: string;
    fonlocaldrive: boolean;
    fonfilefound: tapionfilefoundevent;
    fonfindfileready: TAPIOnFileSearchReady;
    fshowprogress: boolean;
    fconfirmation: boolean;

    procedure dummys(s:string);
    procedure dummyb(b:boolean);

  protected
  public
    constructor Create (aowner:tcomponent); override;
    destructor Destroy; override;

    // miscellous
    function CreateLink(filename, Description, ShortcutTo, Parameters, WorkingDir, IconFilename: String; iconIndex, ShowCmd: Integer): Boolean;
    Procedure AssociateFile(Ext, Filetype, Description, Appname: string);
    function GetTempFile(extension: string): string;
    procedure ExploreFolder(Folder: string);

    // searching
    // note, these create stringlist as return value, so remember
    // free the resulting stringlists!
    function  FindFiles(List: Tstrings; Folder: string; Filter: string = '*.*'; IncludeSubs: boolean = True; ReadOnly: Boolean = False; Hidden: boolean = False; SeparatorForDetails: char = #0): int64; overload;
    function  FindFiles(Folder: string; Filter: string = '*.*'; IncludeSubs: boolean = True; ReadOnly: Boolean = False; Hidden: boolean = False): tstringlist; overload;
    function  LastChanged(Folder: string; Filter: String = '*.*'; IncludeSubs: Boolean = TRUE): tdatetime;

    // files & directories
    function CopyDir(const fromDir, toDir: string): boolean;
    function MoveDir(const fromDir, toDir: string; Const AllowUndo: boolean = TRUE): boolean;
    function DeleteDir(const dir: string; Const AllowUndo: boolean = TRUE): boolean;
    function RelativePath(const Path, Root: string ): string;
    function IsOnLocalDrive(const fname: string): boolean;
    function GetFileDtm(Const FileName: String; Var CreationTime: TDateTime; Var LastAccessTime: TDateTime; Var LastWriteTime: TDateTime) : Boolean;
    function GetRevision(Const fname: String; var major, minor, release, build: dword): boolean;
    function CopyFile(Const source, dest: string): Boolean;
    function MoveFile(Const source, dest: string; Const AllowUndo: boolean = TRUE): Boolean;
    function DeleteFile(Const filename: string; Const AllowUndo: boolean = TRUE): boolean;
    function RenameFile(Const source, dest: string; Const AllowUndo: boolean = TRUE): boolean;
    function CalcCheckSum(FileName: string): DWORD;
    procedure GetFileInfo(filename: string; List: tstrings);
    function FileInUse(filename: string): boolean;
    function CRC32(filename: string): string;
    function FileSize(filename: string): int64;
    function GetFileType(filename: string): string;

    // clipboard
    procedure PasteFilenamesFromClipboard(Filenames: TStrings);
    function CopyFilenamesToClipboard(Filenames: TStrings): Boolean;

    // special folders
    function GetSpecialFolder (const fCLSID: integer): string;

  published
    property DiskSerial: string read fhdserial write dummys stored false;
    property ShowProgress: boolean read fshowprogress write fshowprogress;
    property Confirmation: boolean read fconfirmation write fconfirmation;
    property OnlocalDrive: boolean read fonlocaldrive write dummyb stored false;
    property OnFileFound: tapionfilefoundevent read fonfilefound write fonfilefound;
    property OnFindFileReady: TAPIOnFileSearchReady read fonfindfileready write fonfindfileready;

  end;

procedure Register;

function AddBackSlash(const Path: string): string;
function DeleteBackSlash(const Path: string): string;
function PathAppend(const Path, DirToAdd: string): string;
function GetUniversalName(Const Filename: string): string;
function FileExtension(Const Filename: String): string; overload;
function FileExtension(Const Filename, NewExtension: String): String; overload;
function PathCombine(Const Folder1, Folder2: AnsiString): AnsiString;

function FileInUse(filename: string): boolean;
function GetFileSize(Const Filename: string): int64;
function IsOnLocalDrive(fname: string): boolean;
function GetFileType(const strFilename: string): string;
function GetRevision(fname: String; var major, minor, release, build: dword): boolean;
function IsASCIIFile(Const Filename: string; Const CheckOnlyBeginning: Boolean = TRUE): Boolean;
function FileTimeToTDateTime(const AFileTime: TFileTime): TDateTime;
function TDateTimeToFileTime(const ADateTime: TDateTime): TFileTime;
function FileDateTime(Const FileName: string; NewDateTime: TDateTime): Boolean; overload;
function FileDateTime(Const filename: string): tdatetime; overload;
function GetFileTimes(const FileName: string; var Created: TDateTime; var Accessed: TDateTime; var Modified: TDateTime): Boolean;
Function AssociatedIcon(FName : String; Idx : Word; var Icon : TIcon) : Boolean;
Function IconFromFile(FName : String; idx : Word; var Icon : TIcon) : Boolean;
procedure AssociateFile(Const Ext, Filetype, Description, Appname: string);
function ExtractIcon(Const Filename: string): ticon;
function GetFileOwner(Const FileName: string; var Domain, Username: string): Boolean;
function StartAssociatedExe(Const FileName: string; var ErrorCode: Cardinal): Boolean;
function DriveSerial(Const Drive: Char): String;

function ParseListDetails(Const ListItem: String; Var AFileName: String; Var AFileTime: TDateTime; Var AFileSize: int64; Var AAttributes: integer; Const ASeparatorForDetails: char = ';'): Boolean;
function LastChanged(Const Folder: string; Const Filter: String = '*.*'; Const IncludeSubs: Boolean = TRUE): tdatetime;
function FindFiles(
  List: Tstrings;
  Const AFolder: String;
  Const AFilter: String = '*.*';
  Const ARelativeRoot: String = '';
  Const AIncludeSubs: Boolean = True;
  Const AReadOnly: Boolean = False;
  Const AHidden: Boolean = False;
  Const ASeparatorForDetails: char = #0; // = no details
  Const ABreakOnFirstFileFound: Boolean = FALSE
  ): int64;
function GetSpecialFolder (const fCLSID: integer): string; // see CLSID list below the exported functions
function IsEmptyFolder(Const Folder: String): boolean;
procedure FindFolders(List: Tstrings; Const Folder: string; Const ReadOnly: Boolean = False; Const Hidden: Boolean = False);
function AppDataFolder(Const ApplicationName, CompanyName: String; Const Personal: Boolean = FALSE; Const CreateDirectory: Boolean = TRUE): String;

function BrowseFolderDialog(Const Caption, startfolder: string; CreateFolders: boolean = FALSE): string;
function CopyDir(Const fromDir, toDir: string; Const AShowProgress: Boolean = FALSE; Const AConfirm: Boolean = FALSE; Const AAllowUndo: Boolean = TRUE; Const ARenameOnCollision: Boolean = FALSE): boolean;
function MoveDir(Const fromDir, toDir: string; Const AShowProgress: Boolean = FALSE; Const AConfirm: Boolean = FALSE; Const AAllowUndo: Boolean = TRUE): boolean;
function DeleteDir(Const dir: string; Const AShowProgress: Boolean = FALSE; Const AConfirm: Boolean = FALSE; Const AAllowUndo: Boolean = TRUE): boolean;
function CopyFile(Const Source, Dest: String; Const AShowProgress: Boolean = FALSE; Const AConfirm: Boolean = FALSE; Const AAllowUndo: Boolean = TRUE; Const ARenameOnCollision: Boolean = FALSE): Boolean;
function MoveFile(Const Source, Dest: String; Const AShowProgress: Boolean = FALSE; Const AConfirm: Boolean = FALSE; Const AAllowUndo: Boolean = TRUE): Boolean;
function DeleteFile(Const Filename: string; Const AShowProgress: Boolean = FALSE; Const AConfirm: Boolean = FALSE; Const AAllowUndo: Boolean = TRUE ): boolean;
function RenameFile(Const source, dest: string; Const AShowProgress: Boolean = FALSE; Const AConfirm: Boolean = FALSE; Const AAllowUndo: Boolean = TRUE ): boolean;
function UpdateFiles(Const FromFolder, ToFolder: String; Const AIncludeSubs: Boolean = TRUE; Const AIncludeHiddenFiles: Boolean = TRUE; Const AShowProgress: Boolean = FALSE; Const AConfirm: Boolean = FALSE; Const ACreateFolders: Boolean = TRUE): boolean;
function CombineFiles(Const ATargetFilename: String; AFileList: TStrings): Boolean;

function GetTempFilename: String;
function GetTempFile(Const Extension: string): string;
function CreateLink(Const filename, Description, ShortcutTo, Parameters, WorkingDir, IconFilename: String; iconIndex, ShowCmd: Integer): Boolean;
function CRC32(p: pointer; ByteCount: dword): dword; overload;
function CRC32(Filename: string): dword; overload;
procedure PasteFilenamesFromClipboard(Filenames: TStrings);
function CopyFilenamesToClipboard(Filenames: TStrings): Boolean;
function CompareFiles(const File1, File2: TFileName): Boolean;

function GetDiskFreeSpace(sDrv: string; var cFree, cSize: int64): Boolean;

implementation

{$include '..\API_source\inc\CompilerVersions.INC'}
{$R *.RES}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

uses
  comobj, activex, registry, dialogs, Clipbrd, shellapi,
  api_strings, filectrl;

const
  versioninfo = 'r1.32/ari.pikivirta]at[kolumbus.fi';

type
  FreePIDLProc  =  procedure (PIDL: PItemIDList); stdcall;

var
  FreePIDL : FreePIDLProc = nil;

//------------------------------------------------------------------------------
function CombineFiles(Const ATargetFilename: String; AFileList: TStrings): Boolean;
var
  tfs, sfs: tfilestream;
  i, bread: integer;
  buf: array[0..1024*8] of byte; // 8k buffered
begin
  result:= FALSE;
  //
  tfs:= tfilestream.Create(ATargetFilename, fmCreate or FmOpenWrite);
  try
    tfs.Seek(0, soFromBeginning);
    for i:=0 to AFileList.Count-1 do
      if fileexists(AFilelist[i]) then
      begin
        sfs:= Tfilestream.Create(AFilelist[i], fmOpenRead);
        try
          sfs.Seek(0, SoFromBeginning);
          while (sfs.Position<sfs.Size) do
          begin
            bread:= sfs.Read(buf, sizeof(buf));
            tfs.Write(buf, bread);
          end;
        finally
          freeandnil(sfs);
        end;
      end else
        exit;
  finally
    freeandnil(tfs);
  end;
  //
  result:= TRUE;
end;

//------------------------------------------------------------------------------
function PathCombine(Const Folder1, Folder2: AnsiString): AnsiString;
var
  p: integer;
  tmpS: AnsiString;
begin
  result:= '';
  // seek common
  p:= 1;
  while (p<length(ansistring(folder2))) do
  begin
    tmpS:= copy(ansistring(folder1), p, length(ansistring(folder1)));
    if (pos(tmpS, ansistring(folder2), TRUE)=1) then
    begin
      result:= copy(ansistring(folder1), 1, p-1) + Folder2;
      exit;
    end else
      p:= p + 1;
  end;
  // nothing to combine was found
  result:= folder1 + folder2;
end;

//------------------------------------------------------------------------------
function DesktopFolder: String;
begin
  result:= getspecialfolder(CSIDL_DESKTOP);
end;

function StartupFolder: String;
begin
  result:= getspecialfolder(CSIDL_STARTUP);
end;

function RecentFolder: String;
begin
  result:= getspecialfolder(CSIDL_RECENT);
end;

function BitBucket: String;
begin
  result:= getspecialfolder(CSIDL_BITBUCKET);
end;

function MyDocumentsFolder: String;
begin
  result:= getspecialfolder(CSIDL_MYDOCUMENTS);
end;

function TemplatesFolder: String;
begin
  result:= getspecialfolder(CSIDL_TEMPLATES);
end;

function InternetCacheFolder: String;
begin
  result:= getspecialfolder(CSIDL_INTERNET_CACHE);
end;

function WindowsFolder: String;
begin
  result:= getspecialfolder(CSIDL_WINDOWS);
end;

function SystemFolder: String;
begin
  result:= getspecialfolder(CSIDL_SYSTEM);
end;

function CommonAdminToolsFolder: String;
begin
  result:= getspecialfolder(CSIDL_COMMON_ADMINTOOLS);
end;

//------------------------------------------------------------------------------
function GetDiskFreeSpace(sDrv: string; var cFree, cSize: int64): Boolean;
type
  LargeInt = Int64;
  pLargeInt = ^Int64;
var
  GetDiskFreeSpaceEx: function(RootName: PChar; var FreeForCaller, TotNoOfBytes: LargeInt; TotNoOfFreeBytes: PLargeInt): Bool; stdcall;
  cFree2, cSize2: int64;
  hndLib: THandle;
begin
  Result := False;
  cFree2 := -1;
  cSize2 := -1;
  hndLib := GetModuleHandle('Kernel32'); { Get the handle for kernel32.dll }
  if hndLib <> 0 then
  begin
    @GetDiskFreeSpaceEx := GetProcAddress(hndLib, 'GetDiskFreeSpaceExA');
    if (@GetDiskFreeSpaceEx <> nil) and GetDiskFreeSpaceEx(PChar(sDrv), cFree2, cSize2, nil) then
    begin
      cFree := cFree2;
      cSize := cSize2;
      Result := True;
    end;
    FreeLibrary(hndLib);
  end;
end;

//------------------------------------------------------------------------------
function IsASCIIFile(Const Filename: string; Const CheckOnlyBeginning: Boolean = TRUE): Boolean;
const
  Buffersize = 2048;
var
  c: array[0..Buffersize] of Byte;
  i: Integer;
  BytesRead, TotSize, IncSize: Int64;
  fs: tfilestream;
begin
  Result:= FALSE;
  if FileExists(Filename) then
  begin
    fs:= tfilestream.Create(Filename, fmOpenRead);
    try
      Result:= TRUE;
      Totsize:= fs.size;
      if (CheckOnlyBeginning) and (TotSize>BufferSize) then TotSize:= BufferSize;
      IncSize:= 0;
      BytesRead:= 1;
      while (Result) and (IncSize<TotSize) and (BytesRead>0) do
      begin
        BytesRead:= fs.Read(c, sizeof(c));
        for i:= 0 to BytesRead-1 do if (c[i] < 32) and (not (c[i] in [9, 10, 13, 26])) then Result:= FALSE;
        IncSize:= IncSize + BytesRead;
      end; { while }
    finally
      fs.free;
    end;
  end;
end;

//------------------------------------------------------------------------------
function GetFileType(const strFilename: string): string;
var
  FileInfo: TSHFileInfo;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  SHGetFileInfo(PChar(strFilename), 0, FileInfo, SizeOf(FileInfo), SHGFI_TYPENAME);
  Result := strpas(FileInfo.szTypeName);
end;

//------------------------------------------------------------------------------
function StartAssociatedExe(Const FileName: string; var ErrorCode: Cardinal): Boolean;
var
  Prg: string;
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
  SetLength(Prg, MAX_PATH);
  Result := False;
  ErrorCode := FindExecutable(PChar(FileName), nil, PChar(Prg));
  if ErrorCode >= 32 then
  begin
    SetLength(Prg, StrLen(PChar(Prg)));
    FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
    with StartupInfo do
    begin
      cb := SizeOf(TStartupInfo);
      wShowWindow := SW_SHOW;
    end;
    if CreateProcess(PChar(Prg), PChar(Format('%s %s', [Prg, FileName])), nil, nil, False, NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
    begin
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, ErrorCode);
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
      Result := True;
    end else
      ErrorCode := GetLastError;
  end;
end;

//------------------------------------------------------------------------------
function AppDataFolder(Const ApplicationName, CompanyName: String;
  Const Personal: Boolean = FALSE; Const CreateDirectory: Boolean = TRUE): String;
begin
  // retrieve personal or common appdata folder..
  // in case result is empty, we will use documents
  // folder instead -> that is another one that should
  // be possible to store data on both vista and 7
  result:= '';
  if (Personal) then
  begin
    result:= getspecialFolder(CSIDL_APPDATA); // << roaming.. CSIDL_LOCAL_APPDATA ??
    if (result='') then result:= getspecialfolder(CSIDL_PERSONAL); // << my documents
  end else
  begin
    result:= getspecialFolder(CSIDL_COMMON_APPDATA);
    if (result='') then // what should we use if this happens ?
  end;
  // check if application name was defined
  if (CompanyName<>'') then result:= addbackslash(result)+CompanyName;
  if (ApplicationName<>'') then result:= addbackslash(result)+FileExtension(ExtractFilename(ApplicationName),'');
  // crate directory if requested and does
  // not already exist
  if (CreateDirectory) and (not directoryexists(result)) then
    ForceDirectories(Result);
end;

//------------------------------------------------------------------------------
function FileExtension(Const Filename: String): string;
// Descr.: Function returns extension of given filename.
var
  p: integer;
begin
  p:= posfromend('.', ansistring(Filename));
  if p>0 then result:= string(copy(Filename, p+1, length(ansistring(Filename))))
    else result:= '';
end;

function FileExtension(Const Filename, NewExtension: String): String;
// Descr.: Function replaces or adds given newExtension to given filename as result
var
  p: integer;
begin
  p:= posfromend('.', ansistring(Filename));
  if p>0 then result:= string(copy(Filename, 1, p-1))
    else result:= Filename;
  if (Newextension<>'') and (Newextension[1]<>'.') then
    result:= result+'.'+NewExtension
    else result:= result + NewExtension;
end;

//------------------------------------------------------------------------------
function FileTimeToTDateTime(const AFileTime: TFileTime): TDateTime;
var
  LDosTime : LongInt;
begin
  Result := 0;
  if Windows.FileTimeToDosDateTime(AFileTime, LongRec(LDosTime).Hi, LongRec(LDosTime).Lo) then
    Result := SysUtils.FileDateToDateTime(LDosTime)
    else SysUtils.RaiseLastOSError;
end;

//------------------------------------------------------------------------------
function TDateTimeToFileTime(const ADateTime: TDateTime): TFileTime;
var
  LDosTime : LongInt;
begin
  Result.dwLowDateTime := 0;
  Result.dwHighDateTime := 0;
  LDosTime := SysUtils.DateTimeToFileDate(ADateTime);
  DosDateTimeToFileTime(LongRec(LDosTime).Hi, LongRec(LDosTime).Lo, Result);
end;

//------------------------------------------------------------------------------
function FileDateTime(Const FileName: string; NewDateTime: TDateTime): Boolean;
var
  FileHandle: Integer;
  FileTime: TFileTime;
  LFT: TFileTime;
  LST: TSystemTime;
begin
  Result:= False;
  DecodeDate(NewDateTime, LST.wYear, LST.wMonth, LST.wDay);
  DecodeTime(NewDateTime, LST.wHour, LST.wMinute, LST.wSecond, LST.wMilliSeconds);
  if SystemTimeToFileTime(LST, LFT) then
    if LocalFileTimeToFileTime(LFT, FileTime) then
    begin
      FileHandle:= FileOpen(FileName, fmOpenReadWrite or fmShareExclusive);
      try
        if SetFileTime(FileHandle, nil, nil, @FileTime) then Result:= True;
      finally
        fileclose(FileHandle);
      end;
    end;
end;

//------------------------------------------------------------------------------
function GetFileTimes(const FileName: string; var Created: TDateTime;
  var Accessed: TDateTime; var Modified: TDateTime): Boolean;
var
  h: THandle;
  Info1, Info2, Info3: TFileTime;
  SysTimeStruct: SYSTEMTIME;
  TimeZoneInfo: TTimeZoneInformation;
  Bias: Double;
begin
  Result := False;
  Bias   := 0;
  h      := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if h > 0 then
    try
      if GetTimeZoneInformation(TimeZoneInfo) <> $FFFFFFFF then
        Bias := TimeZoneInfo.Bias / 1440; // 60x24
      GetFileTime(h, @Info1, @Info2, @Info3);
      if FileTimeToSystemTime(Info1, SysTimeStruct) then
        Created := SystemTimeToDateTime(SysTimeStruct) - Bias;
      if FileTimeToSystemTime(Info2, SysTimeStruct) then
        Accessed := SystemTimeToDateTime(SysTimeStruct) - Bias;
      if FileTimeToSystemTime(Info3, SysTimeStruct) then
        Modified := SystemTimeToDateTime(SysTimeStruct) - Bias;
      Result := True;
    finally
      FileClose(h);
    end;
end;

//------------------------------------------------------------------------------
// 16062008, ari pikivirta
//  * free OwnerSID added
function GetFileOwner(Const FileName: string; var Domain, Username: string): Boolean;
var
  SecDescr: PSecurityDescriptor;
  SizeNeeded, SizeNeeded2: DWORD;
  OwnerSID: PSID;
  OwnerDefault: BOOL;
  OwnerName, DomainName: PChar;
  OwnerType: SID_NAME_USE;
begin
  GetFileOwner := False;

  GetMem(SecDescr, 1024);
  GetMem(OwnerSID, SizeOf(PSID));
  GetMem(OwnerName, 1024);
  GetMem(DomainName, 1024);
  try
    if not GetFileSecurity(PChar(FileName),
      OWNER_SECURITY_INFORMATION,
      SecDescr, 1024, SizeNeeded) then
      Exit;
    if not GetSecurityDescriptorOwner(SecDescr,
      OwnerSID, OwnerDefault) then
      Exit;
    SizeNeeded  := 1024;
    SizeNeeded2 := 1024;
    if not LookupAccountSID(nil, OwnerSID, OwnerName,
      SizeNeeded, DomainName, SizeNeeded2, OwnerType) then
      Exit;
    Domain   := DomainName;
    Username := OwnerName;
  finally
    FreeMem(SecDescr);
    Freemem(OwnerSID);
    FreeMem(OwnerName);
    FreeMem(DomainName);
  end;
  GetFileOwner := True;
end;

//------------------------------------------------------------------------------
Function AssociatedIcon(FName : String; Idx : Word; var Icon : TIcon) : Boolean;
begin
  Icon.Handle := shellapi.ExtractAssociatedIcon(HInstance, PChar(FName), Idx);
  if Icon.Handle <> 0 then
    Result := True
  else
    Result := False;
end;

//------------------------------------------------------------------------------
Function IconFromFile(FName : String; idx : Word; var Icon : TIcon) : Boolean;
begin
  Icon.Handle := shellapi.ExtractIcon(hInStance, PChar(FName), idx);
  if Icon.Handle <= 1 then
    Result := False
  else
    Result := True;
end;

//------------------------------------------------------------------------------
function ExtractIcon(Const Filename: string): ticon;
var
  {$IFDEF DELPHI2009UP}
  FileInfo: _SHFILEINFOW; // d2009
  {$ELSE}
  FileInfo: _SHFILEINFOA; // previous versions
  {$ENDIF}
  Attrib: DWord;
  Flags: Cardinal;
begin
  Flags:=SHGFI_ICON;
  if directoryexists(filename) then
    Attrib:=FILE_ATTRIBUTE_DIRECTORY
    else attrib:= 0;
  SHGetFileInfo(pchar(FileName), Attrib, FileInfo, SizeOf(FileInfo), Flags);
  Result:= TIcon.Create;
  Result.Handle:= FileInfo.hIcon;
end;

//------------------------------------------------------------------------------
function CRC32(p: pointer; ByteCount: dword): dword;
  // The constants here are for the CRC-32 generator
  // polynomial, as defined in the Microsoft
  // Systems Journal, March 1995, pp. 107-108
  const table32: ARRAY[0..255] OF DWORD =
 ($00000000, $77073096, $EE0E612C, $990951BA,
  $076DC419, $706AF48F, $E963A535, $9E6495A3,
  $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
  $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
  $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
  $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
  $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
  $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
  $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
  $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
  $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
  $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
  $26D930AC, $51DE003A, $C8D75180, $BFD06116,
  $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
  $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
  $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

  $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
  $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
  $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
  $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
  $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
  $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
  $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
  $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
  $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
  $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
  $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
  $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
  $5005713C, $270241AA, $BE0B1010, $C90C2086,
  $5768B525, $206F85B3, $B966D409, $CE61E49F,
  $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
  $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

  $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
  $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
  $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
  $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
  $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
  $F762575D, $806567CB, $196C3671, $6E6B06E7,
  $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
  $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
  $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
  $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
  $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
  $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
  $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
  $CC0C7795, $BB0B4703, $220216B9, $5505262F,
  $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
  $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

  $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
  $9C0906A9, $EB0E363F, $72076785, $05005713,
  $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
  $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
  $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
  $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
  $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
  $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
  $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
  $A7672661, $D06016F7, $4969474D, $3E6E77DB,
  $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
  $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
  $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
  $BAD03605, $CDD70693, $54DE5729, $23D967BF,
  $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
  $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

  // The following is a little cryptic (but executes very quickly).
  // The algorithm is as follows:
  // 1. exclusive-or the input byte with the low-order byte of
  // the CRC register to get an INDEX
  // 2. shift the CRC register eight bits to the right
  // 3. exclusive-or the CRC register with the contents of Table[INDEX]
  // 4. repeat steps 1 through 3 for all bytes
var
  i: DWORD;
  q: ^BYTE;
  crcvalue: DWORD;
begin
  crcvalue:= $ffffffff;
  q:= p;
  for i:= 0 to ByteCount-1 do
  begin
    crcvalue:= (crcvalue SHR 8) XOR
    table32[ q^ XOR (crcvalue AND $000000FF) ];
    inc(q)
  end;
  crcvalue:= NOT crcvalue;
  result:= crcvalue;
end;

//------------------------------------------------------------------------------
function CRC32 (Filename: string): dword;
var
  Stream: TMemoryStream;
begin
  result:= 0;
  if not fileexists(filename) then exit;
  begin
    Stream:= TMemoryStream.Create;
    try
      try
        stream.LoadFromFile(filename);
        if stream.size>0 then
          result:= CRC32(Stream.Memory, stream.size);
      except
        // failed!
      end;
    finally
      Stream.free;
    end;
  end;
end;

//------------------------------------------------------------------------------
function AddBackSlash(const Path: string): string;
begin
  result:= path;
  if result<>'' then
    if result[length(result)]<>'\' then
      result:= result + '\';
end;

//------------------------------------------------------------------------------
function DeleteBackSlash(const Path: string): string;
begin
  result:= path;
  if path<>'' then
    if result[length(result)]='\' then
      delete(result,length(result),1);
end;

//------------------------------------------------------------------------------
function PathAppend(const Path, DirToAdd: string): string;
begin
  if dirtoadd<>'' then
  begin
    if length(path)<1 then
    begin
      result:= dirtoadd;
    end else
    begin
      result:= addbackslash(path)+dirtoadd;
    end;
  end else
    result:= path;
end;

//------------------------------------------------------------------------------
function FileInUse(filename: string): boolean;
var
  h: thandle;
begin
  h:= createfile(pchar(filename), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  result:= (h=INVALID_HANDLE_VALUE);
  if not result then closehandle(h);
end;

//------------------------------------------------------------------------------
function GetFileSize(Const filename: string): int64;
var
  SearchRec: TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, SearchRec)=0 then
  try
    {$ifdef WIN32}
    Result:= Int64(SearchRec.FindData.nFileSizeHigh) shl Int64(32) + Int64(SearchREc.FindData.nFileSizeLow);
    {$else}
    Result:= Int64(SearchRec.Size);
    {$endif}
  finally
    FindClose(SearchRec);
  end else
    result:= -1;
end;

//------------------------------------------------------------------------------
function FileDateTime(Const filename: string): tdatetime;
var
  SearchRec: TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, SearchRec )=0 then
  begin
    Result:= filedatetodatetime(searchrec.Time);
    FindClose(SearchRec);
  end else
    result:= -1;
end;

function CopyFilenamesToClipboard(Filenames: TStrings): Boolean;
var
  sFilenames: String;
  iIndex: Integer;
  hBuffer: HGLOBAL;
  pBuffer: PDropFiles;
begin
  // check entry conditions
  Result := (Filenames <> nil) and (Filenames.Count > 0);
  if (not Result) then Exit;
  // bring the filenames in a form,
  // separated by #0 and ending with a double #0#0
  sFilenames := '';
  for iIndex := 0 to Filenames.Count - 1 do
    sFilenames := sFilenames +
      ExcludeTrailingPathDelimiter(Filenames.Strings[iIndex]) + #0;
  sFilenames := sFilenames + #0;
  // allocate memory with the size of the "DropFiles" structure plus the
  // length of the filename buffer.
  hBuffer := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT,
    SizeOf(DROPFILES) + Length(sFilenames));
  try
  Result := (hBuffer <> 0);
  if (Result) then
  begin
    pBuffer := GlobalLock(hBuffer);
    try
    // prepare the "DROPFILES" structure
    pBuffer^.pFiles := SizeOf(DROPFILES);
    // behind the "DROPFILES" structure we place the filenames
    pBuffer := Pointer(Integer(pBuffer) + SizeOf(DROPFILES));
    CopyMemory(pBuffer, PChar(sFilenames), Length(sFilenames));
    finally
      GlobalUnlock(hBuffer);
    end;
    // copy buffer to the clipboard
    Clipboard.Open;
    try
    Clipboard.SetAsHandle(CF_HDROP, hBuffer);
    finally
      Clipboard.Close;
    end;
  end;
  except
    Result := False;
    // free only if handle could not be passed to the clipboard
    GlobalFree(hBuffer);
  end;
end;

procedure PasteFilenamesFromClipboard(Filenames: TStrings);
var
  hDropHandle: HDROP;
  szBuffer: PChar;
  iCount, iIndex: Integer;
  iLength: Integer;
begin
  // check entry conditions
  if (Filenames = nil) then Exit;
  Filenames.Clear;
  // lock clipboard
  Clipboard.Open;
  try
  // does clipboard contain filenames?
  if (Clipboard.HasFormat(CF_HDROP)) then
  begin
    // get drop handle from the clipboard
    hDropHandle := Clipboard.GetAsHandle(CF_HDROP);
    // enumerate filenames
    iCount := DragQueryFile(hDropHandle, $FFFFFFFF, nil, 0);
    for iIndex := 0 to iCount - 1 do
    begin
      // get length of filename
      iLength := DragQueryFile(hDropHandle, iIndex, nil, 0);
      // allocate the memory, the #0 is not included in "iLength"
      szBuffer := StrAlloc(iLength + 1);
      try
      // get filename
      DragQueryFile(hDropHandle, iIndex, szBuffer, iLength + 1);
      Filenames.Add(szBuffer);
      finally // free the memory
        StrDispose(szBuffer);
      end;
    end;
  end;
  finally
    // unlock clipboard
    Clipboard.Close;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_files.dummys(s:string); begin end;
procedure tAPI_files.dummyb(b:boolean); begin end;

//------------------------------------------------------------------------------
constructor tAPI_files.create(aowner:tcomponent);
begin
  inherited create(aowner);
  fonlocaldrive:= isonlocaldrive(paramstr(0));
  fhdserial:= driveserial('C');
  version:= versioninfo;
  fshowprogress:= TRUE;
  fconfirmation:= FALSE;
end;

//------------------------------------------------------------------------------
destructor tAPI_files.destroy;
begin
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_files.GetFileType(filename: string): string;
begin
  result:= api_files.GetFileType(filename);
end;

//------------------------------------------------------------------------------
function TAPI_files.FileSize(filename: string): int64;
begin
  result:= GetFileSize(filename);
end;

//------------------------------------------------------------------------------
function TAPI_files.FileInUse(filename: string): boolean;
begin
  result:= api_files.FileInUse(filename);
end;

//------------------------------------------------------------------------------
procedure TAPI_files.GetFileInfo(filename: string; List: tstrings);
var
  buffer: String;
  info: Pointer;
  idata: pointer;
  isize: dword;
  ilen: dword;
  dlen: dword;
begin
  list.clear;

  if not fileexists(filename) then exit;

  dlen:= 255;
  filename:= filename + #0;

  isize:= getfileversioninfosize(@filename[1], ilen);
  if isize>0 then
  begin
    getmem(info, isize);
    try
      if getfileversioninfo(@filename[1], ilen, isize, info) then
      begin
        buffer:= 'companyname'+#0;
        if verqueryvalue(info, @buffer[1], idata, dlen) then
          list.add('Company Name='+strpas(pchar(idata)));
        buffer:= 'filedescription'+#0;
        if verqueryvalue(info, @buffer[1], idata, dlen) then
          list.add('File Description='+strpas(pchar(idata)));
        buffer:= 'fileversion'+#0;
        if verqueryvalue(info, @buffer[1], idata, dlen) then
          list.add('File Version='+strpas(pchar(idata)));
        buffer:= 'internalname'+#0;
        if verqueryvalue(info, @buffer[1], idata, dlen) then
          list.add('Internal Name='+strpas(pchar(idata)));
        buffer:= 'legalcopyright'+#0;
        if verqueryvalue(info, @buffer[1], idata, dlen) then
          list.add('Legal Copyright='+strpas(pchar(idata)));
        buffer:= 'originalfilename'+#0;
        if verqueryvalue(info, @buffer[1], idata, dlen) then
          list.add('Original Filename='+strpas(pchar(idata)));
        buffer:= 'productname'+#0;
        if verqueryvalue(info, @buffer[1], idata, dlen) then
          list.add('Product Name='+strpas(pchar(idata)));
        buffer:= 'productversion'+#0;
        if verqueryvalue(info, @buffer[1], idata, dlen) then
          list.add('Product Version='+strpas(pchar(idata)));
        buffer:= 'specialbuild'+#0;
        if verqueryvalue(info, @buffer[1], idata, dlen) then
          list.add('Special Build='+strpas(pchar(idata)));
      end;
    finally
      freemem(info);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_files.PasteFilenamesFromClipboard(Filenames: TStrings);
begin
  api_files.PasteFilenamesFromClipboard(filenames);
end;

//------------------------------------------------------------------------------
function tapi_files.CopyFilenamesToClipboard(Filenames: TStrings): Boolean;
begin
  result:= api_files.CopyFilenamesToClipboard(filenames);
end;

//------------------------------------------------------------------------------
function TAPI_files.RelativePath( const Path, Root: string ): string;
begin
  result:= ExtractRelativePath(root, path);
end;

//------------------------------------------------------------------------------
procedure TAPI_files.ExploreFolder(Folder: string);
begin
  ShellExecute(0, PChar('explore'), pchar(Folder), nil, nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------
(*
function GetUNCName(const LocalPath: string): string;
var
  BufferSize: DWord;
  DummyBuffer: Byte;
  Buffer: Pointer;
  Error: DWord;
begin
  BufferSize := 1;
  WNetGetUniversalName(PChar(LocalPath), UNIVERSAL_NAME_INFO_LEVEL, @DummyBuffer, BufferSize);
  Buffer := AllocMem(BufferSize);
  try
    Error := WNetGetUniversalName(PChar(LocalPath), UNIVERSAL_NAME_INFO_LEVEL, Buffer, BufferSize);
    if Error <> NO_ERROR then
      begin
        SetLastError(Error);
        RaiseLastWin32Error;
      end;
    Result := PUniversalNameInfo(Buffer)^.lpUniversalName
  finally
    FreeMem(Buffer);
  end;
end;
*)

function GetUniversalName(Const Filename: string): string;
var
  BufferSize: DWord;
  DummyBuffer: Byte;
  Buffer: Pointer;
  Error: DWord;
begin
  BufferSize:= 1;
  WNetGetUniversalName(PChar(Filename), UNIVERSAL_NAME_INFO_LEVEL, @DummyBuffer, BufferSize);
  Buffer:= AllocMem(BufferSize);
  try
    Error:= WNetGetUniversalName(PChar(Filename), UNIVERSAL_NAME_INFO_LEVEL, Buffer, BufferSize);
    if Error<>NO_ERROR then
    begin
      SetLastError(Error);
      result:= Filename;
      //RaiseLastWin32Error;
      exit;
    end;
    Result:= PUniversalNameInfo(Buffer)^.lpUniversalName
  finally
    FreeMem(Buffer);
  end;
end;

//------------------------------------------------------------------------------
function GetSpecialFolder (const fCLSID: integer): string;
(*
    desktop       CSIDL_DESKTOP
    programs      CSIDL_PROGRAMS
    recent        CSIDL_RECENT
    bitbucket     CSIDL_BITBUCKET
    templates     CSIDL_TEMPLATES
    windows       $0024
    system        $0025
    .. and more is found on the shlobj unit
*)
var
  idlist: PItemIDList;
  pstring: array[0..255] of char;
begin
  shlobj.SHGetSpecialFolderLocation(0, fCLSID, idlist);
  if idlist<>nil then
  begin
    shlobj.SHGetPathFromIDList(idlist, pstring);
    result:= strpas(pstring);
  end else
    result:='';
end;

function TAPI_files.GetSpecialFolder (const fCLSID: integer): string;
begin
  result:= api_files.getspecialfolder(fCLSID);
end;

//------------------------------------------------------------------------------

function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam: LPARAM; lpData: LPARAM): Integer; stdcall;
begin
  if (uMsg = BFFM_INITIALIZED) then
    SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
  BrowseCallbackProc := 0;
end;

function BrowseFolderDialog(Const Caption, startfolder: string; CreateFolders: boolean = FALSE): string;
(*
const
  BIF_STATUSTEXT           = $0004;
  BIF_NEWDIALOGSTYLE       = $0040;
  BIF_RETURNONLYFSDIRS     = $0080;
  BIF_SHAREABLE            = $0100;
  BIF_USENEWUI             = BIF_EDITBOX or BIF_NEWDIALOGSTYLE;
  *)
var
  dir: String;
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  JtemIDList: PItemIDList;
  Path: PChar;
begin
  Result := ''; // as nothing selected!
  //
  dir:= startfolder;
  Path := StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(0, CSIDL_DRIVES, JtemIDList);
  with BrowseInfo do
  begin
    hwndOwner := GetActiveWindow;
    pidlRoot := JtemIDList;
    SHGetSpecialFolderLocation(hwndOwner, CSIDL_DRIVES, JtemIDList);
    pszDisplayName := StrAlloc(MAX_PATH);
    lpszTitle := PChar(Caption);//'Select the folder';
    lpfn := @BrowseCallbackProc;
    lParam := LongInt(PChar(dir));
    //
    ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE;
    if not CreateFolders then
      ulFlags := ulFlags or BIF_NONEWFOLDERBUTTON;
  end;
  //
  ItemIDList := SHBrowseForFolder(BrowseInfo);
  if (ItemIDList <> nil) then
  begin
    if SHGetPathFromIDList(ItemIDList, Path) then
    begin
      Result := path
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_files.CRC32(filename: string): string;
begin
  // make checksum32 to 8 char long string
  result:= inttohex(api_files.crc32(filename),8);
end;

//------------------------------------------------------------------------------
function TAPI_files.CalcCheckSum(FileName: string): DWORD;
var
  F: file of DWORD;
  P: Pointer;
  Fsize: DWORD;
  Buffer: array [0..500] of DWORD;
begin
  FileMode:= 0;
  AssignFile(F, FileName);
  Reset(F);
  Seek(F, system.FileSize(F) div 2);
  Fsize:= system.FileSize(F)-1-FilePos(F);
  if Fsize>500 then Fsize:= 500;
  BlockRead(F, Buffer, Fsize);
  Close(F);
  P := @Buffer;
  asm
     xor eax, eax
     xor ecx, ecx
     mov edi , p
     @again:
       add eax, [edi + 4*ecx]
       inc ecx
       cmp ecx, fsize
     jl @again
     mov @result, eax
  end;
end;

//------------------------------------------------------------------------------
function GetRevision(fname: String; var major, minor, release, build: dword): boolean;

type
  PFixedFileInfo = ^TFixedFileInfo;
  TFixedFileInfo =
  record
     dwSignature       : DWORD;
     dwStrucVersion    : DWORD;
     wFileVersionMS    : WORD;      // Minor Version
     wFileVersionLS    : WORD;        // Major Version
     wProductVersionMS : WORD;      // Build Number
     wProductVersionLS : WORD;        // Release Version
     dwFileFlagsMask   : DWORD;
     dwFileFlags       : DWORD;
     dwFileOS          : DWORD;
     dwFileType        : DWORD;
     dwFileSubtype     : DWORD;
     dwFileDateMS      : DWORD;
     dwFileDateLS      : DWORD;
  end;

var
  dwHandle, dwVersionSize: DWORD;
  strSubBlock: String;
  pTemp: Pointer;
  pData: Pointer;
  info: tfixedfileinfo;

begin
  result:=false;
  strSubBlock := '\';
  dwVersionSize := GetFileVersionInfoSize( PChar(fname),dwHandle );
  if dwVersionSize <> 0 then
  begin
    GetMem( pTemp, dwVersionSize );
    try
      if GetFileVersionInfo( PChar(fname),dwHandle,dwVersionSize,pTemp ) then
        if VerQueryValue( pTemp,PChar( strSubBlock ),pData,dwVersionSize ) then
        begin
          info := PFixedFileInfo( pData )^;
          minor:=info.wFileVersionMS;
          major:=info.wFileVersionLS;
          build:=info.wProductVersionMS;
          release:=info.wProductVersionLS;
          result:=true;
        end;
    finally
      FreeMem( pTemp );
    end;
  end;
end;

function TAPI_files.GetRevision(Const fname: String; var major, minor, release, build: dword): boolean;
begin
  result:= api_files.getrevision(fname, major, minor, release, build);
end;

//------------------------------------------------------------------------------
function GetTempFilename: String;
var
  Buffer: array[0..MAX_PATH] OF Char;
begin
  windows.GetTempPath(Sizeof(Buffer)-1,Buffer);
  windows.GetTempFileName(Buffer,'~',0,Buffer);
  result:= StrPas(Buffer);
end;

function GetTempFile(Const Extension: string): string;
begin
  result:= GetTempFilename + Extension;
end;

function TAPI_files.GetTempFile(extension: string): string;
begin
  result:= api_files.gettempfile(extension);
end;

//------------------------------------------------------------------------------
function ParseListDetails(Const ListItem: String;
  Var AFileName: String;
  Var AFileTime: TDateTime;
  Var AFileSize: int64;
  Var AAttributes: integer;
  Const ASeparatorForDetails: char = ';'): Boolean;
var
  p, s: integer;
  t: string;
begin
  // get file name
  p:= pos(ansistring(ASeparatorForDetails), ansistring(ListItem), FALSE, 1);
  if p<1 then
    raise exception.create('Internal error:'+#13+'Detail item file name position was not found.');
  AFilename:= copy(ListItem, 1, p-1);
  // get datetime
  s:= p+1;
  p:= pos(ansistring(ASeparatorForDetails), ansistring(ListItem), FALSE, s);
  if p<1 then
    raise exception.create('Internal error:'+#13+'Detail item time position was not found.');
  t:= copy(ListItem, s, (p-s));
  if not trystrtofloat(t, double(AFileTime)) then
    raise exception.create('Internal error:'+#13+t+#13+'is not valid floating point value.');
  // get file size
  s:= p+1;
  p:= pos(ansistring(ASeparatorForDetails), ansistring(ListItem), FALSE, s);
  if p<1 then
    raise exception.create('Internal error:'+#13+'Detail item size position was not found.');
  t:= copy(ListItem, s, (p-s));
  if not trystrtoint64(t, AFileSize) then
    raise exception.create('Internal error:'+#13+t+#13+'is not valid integer value.');
  // get file attributes
  s:= p+1;
  p:= pos(ansistring(ASeparatorForDetails), ansistring(ListItem), FALSE, s);
  if p<1 then
    raise exception.create('Internal error:'+#13+'Detail item attr was not found.');
  t:= copy(ListItem, s, (p-s));
  if not trystrtoint(t, AAttributes) then
    raise exception.create('Internal error:'+#13+t+#13+'is not valid integer value.');
  // all fields were parsed..
  result:= TRUE;
end;

//------------------------------------------------------------------------------
// 01072009api; fixed bug on breakonfirstfilefound
function FindFiles(
  List: Tstrings;
  Const AFolder: String;
  Const AFilter: String = '*.*';
  Const ARelativeRoot: String = '';
  Const AIncludeSubs: Boolean = True;
  Const AReadOnly: Boolean = False;
  Const AHidden: Boolean = False;
  Const ASeparatorForDetails: char = #0; // = no details
  Const ABreakOnFirstFileFound: Boolean = FALSE
  ): int64;
var
  sr: tsearchrec;
  dir, tempdir: string;
  attr: integer;
begin
  dir:= addbackslash(Afolder);
  result:= 0;

  // relative root
  if ARelativeRoot<>'' then
  begin
    tempdir:= addbackslash( ExtractRelativePath( addbackslash(ARelativeRoot), dir ));
  end else
    tempdir:= dir;

  // find according to the filter
  attr:= faAnyFile;
  if (not Aincludesubs) then attr:= attr - faDirectory;
  if (not Areadonly) then attr:= attr - faReadonly;
  if (not Ahidden) then attr:= attr - faHidden;

  // search files
  if sysutils.FindFirst(dir+'*.*', attr, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory) = faDirectory then
      begin
        if (sr.name[1]<>'.') then
          result:= result + findfiles(List, dir+sr.name, Afilter, ARelativeRoot, Aincludesubs, Areadonly, Ahidden, ASeparatorForDetails, ABreakOnFirstFileFound);
      end else
      begin
        if api_strings.Match(lowercase(ansistring(Afilter)), lowercase(ansistring(sr.name))) then // apply filter
        begin
          result:= result + sr.size;
          if Aseparatorfordetails<>#0 then
          begin // add with details
            List.Add(
              tempdir + sr.Name + ASeparatorForDetails +
              floattostr(FileDateToDateTime(sr.Time)) + ASeparatorForDetails +
              inttostr(Int64(sr.FindData.nFileSizeHigh) shl Int64(32) + Int64(sr.FindData.nFileSizeLow)) + ASeparatorForDetails +
              inttostr(sr.Attr) + ASeparatorForDetails
              )
          end else
          begin // normal add (without details)
            List.Add(tempdir + sr.Name);
          end;
          if ABreakOnFirstFileFound then break; // not directory you damn ass!
        end;
      end;
    until findnext(sr)<>0;
  finally
    findclose(sr);
  end;
end;

function TAPI_files.FindFiles(List: Tstrings; Folder: string; Filter: string = '*.*'; IncludeSubs: boolean = True; ReadOnly: Boolean = False; Hidden: boolean = False; SeparatorForDetails: char = #0): int64;
begin
  result:= api_files.FindFiles(list, folder, filter, '', includesubs, readonly, hidden, separatorfordetails);
end;

function TAPI_Files.FindFiles(Folder, Filter: string; IncludeSubs, ReadOnly, Hidden: boolean): tstringlist;
begin
  (*
      note that stringlist is created as result,
      so you've to free that after use!
  *)
  result:= tstringlist.create;
  result.clear;
  findfiles(result, folder, filter, includesubs, readonly, hidden);
end;

//------------------------------------------------------------------------------
function LastChanged(Const Folder: string; Const Filter: String = '*.*'; Const IncludeSubs: Boolean = TRUE): tdatetime;
var
  sl: tstringlist;
  i, p: integer;
  tmpS: ansistring;
  d: tdatetime;
begin
  sl:= tstringlist.create;
  try
    result:= 0;
    sl.clear;
    findfiles(sl, folder, filter, '', includesubs, FALSE, FALSE, '|'); // get listing with details
    for i:=0 to sl.count-1 do
    begin
      tmpS:= ansistring(sl[i]);
      // name
      p:= pos('|', tmpS);
      delete(tmpS, 1, p);
      // date
      p:= pos('|', tmpS);
      //showmessage(copy(tmpS, 1, p-1));
      try
        d:= strtofloat(string(copy(tmpS, 1, p-1)));
        if d>result then result:= d;
      except
        // just ignore
      end;
      // size
    end;
  finally
    FreeAndNil(sl);
  end;
end;

function TAPI_Files.LastChanged(Folder: string; Filter: String = '*.*'; IncludeSubs: Boolean = TRUE): tdatetime;
begin
  result:= api_files.lastchanged(folder, filter, includesubs);
end;

//------------------------------------------------------------------------------
function IsEmptyFolder(Const Folder: String): boolean;
var
  templist: tstringlist;
begin
  templist:= tstringlist.create;
  try
    templist.clear;
    api_files.FindFiles(templist, folder, '*.*', '', true, true, true, #0, TRUE);
    result:= (templist.count<1); // TRUE if any files were found
  finally
    freeandnil(templist);
  end;
end;

//------------------------------------------------------------------------------
procedure FindFolders(List: Tstrings; Const Folder: string; Const ReadOnly: Boolean = False; Const Hidden: Boolean = False);
var
  sr: tsearchrec;
  dir: string;
  attr: integer;
begin
  dir:= addbackslash(folder);

  // find according to the filter
  attr:= faAnyFile;
  if (not readonly) then attr:= attr - faReadonly;
  if (not hidden) then attr:= attr - faHidden;

  // search files
  if findfirst(dir+'*.*', attr, sr) = 0 then
  try
    repeat
      if (sr.Attr and faDirectory) = faDirectory then
        if (sr.name[1]<>'.') then
        begin
          list.add(dir+sr.name);
          findfolders(list, dir+sr.name, readonly, hidden);
        end;
    until findnext(sr)<>0;
  finally
    findclose(sr);
  end;
end;

//------------------------------------------------------------------------------
function CompareFiles(const File1, File2: TFileName): Boolean;
const
  BlockSize = 65536;
var
  fs1, fs2: TFileStream;
  L1, L2: Integer;
  B1, B2: array[1..BlockSize] of Byte;
begin
  result:= false;
  fs1:= TFileStream.Create(File1, fmOpenRead or fmShareDenyWrite);
  try
    fs2:= TFileStream.Create(File2, fmOpenRead or fmShareDenyWrite);
    try
      if fs1.Size=fs2.Size then
      begin
        while fs1.Position<fs1.Size do
        begin
          L1:= fs1.Read(B1[1], BlockSize);
          L2:= fs2.Read(B2[1], BlockSize);
          if L1<>L2 then exit;
          if not CompareMem(@B1[1], @B2[1], L1) then Exit;
        end;
        result:= true;
      end;
    finally
      fs2.Free;
    end;
  finally
    fs1.Free;
  end;
end;

//------------------------------------------------------------------------------
function CreateLink(Const filename, Description, ShortcutTo, Parameters,
  WorkingDir, IconFilename: String; iconIndex, ShowCmd: Integer): Boolean;
var
  Obj: IUnknown;
  SL: IShellLink;
  PF: IPersistFile;
  WideFilename: WideString;
begin
  Obj := CreateComObject(CLSID_ShellLink);
  SL := Obj as IShellLink;
  SL.SetPath(PChar(ShortcutTo));
  SL.SetArguments(PChar(Parameters));
  if WorkingDir <> '' then SL.SetWorkingDirectory(PChar(WorkingDir));
  if IconFilename <> '' then SL.SetIconLocation(PChar(IconFilename), IconIndex);
  SL.SetShowCmd(ShowCmd);
  if Description <> '' then SL.SetDescription(PChar(Description));
  PF := Obj as IPersistFile;
  WideFilename := Filename;
  Result := SUCCEEDED(PF.Save(PWideChar(WideFilename), True));
end;

function tAPI_files.CreateLink(filename, Description, ShortcutTo, Parameters,
  WorkingDir, IconFilename: String; iconIndex, ShowCmd: Integer): Boolean;
begin
  result:= api_files.createlink(filename, description, shortcutto, parameters, workingdir, iconfilename, iconindex, showcmd);
end;

//------------------------------------------------------------------------------
procedure AssociateFile(Const Ext, Filetype, Description, Appname: String);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    // create class entry
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey(ext, True);
    Reg.WriteString('', filetype);
    Reg.CloseKey;

    // create type entry
    Reg.OpenKey(filetype, True);
    Reg.WriteString('', description);
    Reg.CloseKey;

    // assign default icon (applications icon)
    Reg.OpenKey(filetype + '\DefaultIcon', True);
    Reg.WriteString('', appname + ',0');
    Reg.CloseKey;

    // write open action in explorer
    Reg.OpenKey(filetype + '\Shell\Open', True);
    Reg.WriteString('', '&Open');
    Reg.CloseKey;

    // Write application to open it with
    Reg.OpenKey(filetype + '\Shell\Open\Command', True);
    Reg.WriteString('', '"' + appname + '" "%1"');
    Reg.CloseKey;

    // notify explorer
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  finally
    Reg.Free;
  end;
end;

procedure TAPI_files.AssociateFile(ext,filetype,description,appname: string);
begin
  api_files.associatefile(ext, filetype, description, appname);
end;

//------------------------------------------------------------------------------
function tAPI_files.getfiledtm(Const FileName: String; Var CreationTime: TDateTime;
  Var LastAccessTime: TDateTime; Var LastWriteTime: TDateTime) : Boolean;
Var
  S: Array[0..256] Of Char;
  H: THandle;
  CT,LA,LW: TFileTime;
Begin
  Result := False;
  StrPCopy(S, FileName);

  H := CreateFile(
    S,
    GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE,
    Nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0);

  If H<>INVALID_HANDLE_VALUE Then
  Begin
    Result         := Windows.GetFileTime(H, @CT, @LA, @LW);
    CreationTime   := FileTimeToTDateTime(CT);
    LastAccessTime := FileTimeToTDateTime(LA);
    LastWriteTime  := FileTimeToTDateTime(LW);
    CloseHandle(H);
  End;
End;

//------------------------------------------------------------------------------
// FILE OPERATIONS
//------------------------------------------------------------------------------
function CopyDir(
  Const fromDir, toDir: string;
  Const AShowProgress: Boolean = FALSE;
  Const AConfirm: Boolean = FALSE;
  Const AAllowUndo: Boolean = TRUE;
  Const ARenameOnCollision: Boolean = FALSE
  ): boolean;
var
  fos: TSHFileOpStruct;
begin
  if not directoryexists(fromdir) then
  begin
    result:= FALSE;
    exit;
  end;
  ZeroMemory(@fos, SizeOf(fos));
  with fos do
  begin
    Wnd := 0;
    //lpszProgressTitle := '';
    wFunc := FO_COPY;
    //fFlags := FOF_FILESONLY;
    fFlags:= 0; //FOF_SIMPLEPROGRESS;
    if not AConfirm then fflags:= fflags or FOF_NOCONFIRMMKDIR; // do not cofirm make directory
    if ARenameOnCollision then fflags:= fflags or FOF_RENAMEONCOLLISION; // automatically rename on collision to (2)
    if not AConfirm then fflags:= fflags or FOF_NOCONFIRMATION;
    if not AShowProgress then fflags:= fflags or FOF_SILENT;
    if AAllowUndo then fflags:= fflags or FOF_ALLOWUNDO;
    pFrom := PChar(fromDir+#0);
    pTo := PChar(toDir+#0);
  end;
  Result := (0 = ShFileOperation(fos)) and (not fos.fAnyOperationsAborted);
end;

function tAPI_files.copydir(Const fromDir, toDir: string): boolean;
begin
  result:= api_files.CopyDir(fromdir, todir, fshowprogress, fconfirmation, FALSE, TRUE);
end;

//------------------------------------------------------------------------------
function MoveDir(
  Const fromDir, toDir: string;
  Const AShowProgress: Boolean = FALSE;
  Const AConfirm: Boolean = FALSE;
  Const AAllowUndo: Boolean = TRUE
  ): boolean;
var
  fos: TSHFileOpStruct;
begin
  if not directoryexists(fromdir) then
  begin
    result:= FALSE;
    exit;
  end;
  ZeroMemory(@fos, SizeOf(fos));
  with fos do
  begin
    Wnd := 0; //application.Handle;
    wFunc := FO_MOVE;
    //lpszProgressTitle := '';
    //fFlags := FOF_FILESONLY;
    fFlags:= 0; //FOF_SIMPLEPROGRESS;
    if not AConfirm then fflags:= fflags or FOF_NOCONFIRMATION;
    if not AShowProgress then fflags:= fflags or FOF_SILENT;
    if AAllowUndo then fflags:= fflags or FOF_ALLOWUNDO;
    pFrom:= PChar(fromDir+#0);
    pTo:= PChar(toDir+#0);
  end;
  Result:= (0 = ShFileOperation(fos)) and (not fos.fAnyOperationsAborted);
end;

function tAPI_files.movedir(Const fromDir, toDir: string; Const AllowUndo: boolean = TRUE): boolean;
begin
  result:= api_files.MoveDir(fromdir, todir, self.fshowprogress, self.fconfirmation, AllowUndo);
end;

//------------------------------------------------------------------------------
function DeleteDir(
  Const dir: string;
  Const AShowProgress: Boolean = FALSE;
  Const AConfirm: Boolean = FALSE;
  Const AAllowUndo: Boolean = TRUE
  ): boolean;
var
  fos: TSHFileOpStruct;
begin
  if not directoryexists(dir) then
  begin
    result:= FALSE;
    exit;
  end;
  ZeroMemory(@fos, SizeOf(fos));
  with fos do
  begin
    Wnd:= 0; //application.Handle;
    wFunc:= FO_DELETE;
    fFlags:= 0; //FOF_SIMPLEPROGRESS;
    if not AConfirm then fflags:= fflags or FOF_NOCONFIRMATION;
    if not AShowProgress then fflags:= fflags or FOF_SILENT;
    if AAllowUndo then fflags:= fflags or FOF_ALLOWUNDO;
    pFrom := PChar(dir+#0);
  end;
  Result := (0 = ShFileOperation(fos)) and (not fos.fAnyOperationsAborted);
end;

function tAPI_files.deletedir(Const dir: string; Const AllowUndo: boolean = TRUE): boolean;
begin
  result:= api_files.DeleteDir(dir, fshowprogress, fconfirmation, allowundo);
end;

//------------------------------------------------------------------------------
function CopyFile(
  Const Source, Dest: String;
  Const AShowProgress: Boolean = FALSE;
  Const AConfirm: Boolean = FALSE;
  Const AAllowUndo: Boolean = TRUE;
  Const ARenameOnCollision: Boolean = FALSE
  ): Boolean;
var
  Struct: TSHFileOpStruct;
begin
  if not fileexists(source) then
  begin
    result:= FALSE;
    exit;
  end;
  ZeroMemory(@Struct, SizeOf(Struct));
  with struct do
  begin
    Wnd := 0; //application.Handle;
    //lpszProgressTitle := '';
    wFunc:= FO_COPY;
    pFrom:= PChar(Source+#0);
    pTo:= PChar(Dest+#0);
    fFlags:= 0; //FOF_SIMPLEPROGRESS;
    if ARenameOnCollision then fflags:= fflags or FOF_RENAMEONCOLLISION; // automatically rename on collision to (2)
    if not AConfirm then fflags:= fflags or FOF_NOCONFIRMATION;
    if not AShowProgress then fflags:= fflags or FOF_SILENT;
    if AAllowUndo then fflags:= fflags or FOF_ALLOWUNDO;
  end;
  Result:= (ShFileOperation(Struct) = 0) and (not Struct.fAnyOperationsAborted);
end;

function tAPI_files.copyfile(Const source, dest: string): Boolean;
begin
  result:= api_files.CopyFile(source, dest, fshowprogress, fconfirmation, FALSE);
end;

//------------------------------------------------------------------------------
function MoveFile(
  Const Source, Dest: String;
  Const AShowProgress: Boolean = FALSE;
  Const AConfirm: Boolean = FALSE;
  Const AAllowUndo: Boolean = TRUE
  ): Boolean;
var
  Struct : TSHFileOpStruct;
begin
  ZeroMemory(@Struct, SizeOf(Struct));
  with struct do
  begin
    Wnd := 0; //application.Handle;
    //lpszProgressTitle := '';
    wFunc := FO_MOVE;
    pFrom := PChar((Source)+#0);
    pTo := PChar((Dest)+#0);
    fFlags:= 0; //FOF_SIMPLEPROGRESS;
    if not AConfirm then fflags:= fflags or FOF_NOCONFIRMATION;
    if not AShowProgress then fflags:= fflags or FOF_SILENT;
    if AAllowUndo then fflags:= fflags or FOF_ALLOWUNDO;
  end;
  Result := (ShFileOperation(Struct)=0) and (not Struct.fAnyOperationsAborted);
end;

function tAPI_files.movefile(Const source, dest: string; Const AllowUndo: boolean = TRUE): Boolean;
begin
  result:= api_files.MoveFile(source, dest, fshowprogress, fconfirmation, AllowUndo);
end;

//------------------------------------------------------------------------------
function DeleteFile(
  Const Filename: string;
  Const AShowProgress: Boolean = FALSE;
  Const AConfirm: Boolean = FALSE;
  Const AAllowUndo: Boolean = TRUE
  ): boolean;
var
  Sh: TSHFileOpStruct;
begin
  if not fileexists(filename) then
  begin
    result:= FALSE;
    exit;
  end;
  ZeroMemory(@Sh, SizeOf(Sh));
  with Sh do
  begin
    Wnd := 0; //application.Handle;
    //lpszProgressTitle := '';
    wFunc := FO_DELETE;
    pFrom := PChar((FileName)+#0);
    fFlags:= 0; //FOF_SIMPLEPROGRESS;
    if not AConfirm then fflags:= fflags or FOF_NOCONFIRMATION;
    if not AShowProgress then fflags:= fflags or FOF_SILENT;
    if AAllowUndo then fflags:= fflags or FOF_ALLOWUNDO;
  end;
  Result:= (SHFileOperation(Sh) = 0) and (not Sh.fAnyOperationsAborted);
end;

function TAPI_files.deletefile(Const filename: string; Const AllowUndo: boolean = TRUE): boolean;
begin
  result:= api_files.DeleteFile(filename, fshowprogress, fconfirmation, AllowUndo);
end;

//------------------------------------------------------------------------------
function RenameFile(
  Const source, dest: string;
  Const AShowProgress: Boolean = FALSE;
  Const AConfirm: Boolean = FALSE;
  Const AAllowUndo: Boolean = TRUE
  ): boolean;
var
  Sh: TSHFileOpStruct;
begin
  if not FileExists(source) then
  begin
    result:= FALSE;
    exit;
  end;
  ZeroMemory(@Sh, SizeOf(Sh));
  with Sh do
  begin
    Wnd := 0; //application.Handle;
    //lpszProgressTitle := '';
    wFunc := FO_RENAME;
    pFrom := PChar((source)+#0);
    pTo := PChar((dest)+#0);
    fFlags :=  FOF_SIMPLEPROGRESS;
    if not AConfirm then fflags:= fflags or FOF_NOCONFIRMATION;
    if not AShowProgress then fflags:= fflags or FOF_SILENT;
    if AAllowUndo then fflags:= fflags or FOF_ALLOWUNDO;
  end;
  Result := (SHFileOperation(Sh) = 0) and (not Sh.fAnyOperationsAborted);
end;

function TAPI_files.RenameFile(Const source, dest: string; Const AllowUndo: boolean = TRUE): boolean;
begin
  result:= api_files.RenameFile(source, dest, fshowprogress, fconfirmation, AllowUndo)
end;

//------------------------------------------------------------------------------
function UpdateFiles(Const FromFolder, ToFolder: String;
  Const AIncludeSubs: Boolean = TRUE;
  Const AIncludeHiddenFiles: Boolean = TRUE;
  Const AShowProgress: Boolean = FALSE;
  Const AConfirm: Boolean = FALSE;
  Const ACreateFolders: Boolean = TRUE): boolean;
var
  sourcelist, targetlist: tstringlist;
  sourcepos, targetpos, p: integer;
  Filename1, Filename2: String;
  Filesize1, Filesize2: int64;
  Filetime1, filetime2: TDatetime;
  Fileattr1, Fileattr2: integer;
begin
  result:= FALSE;
  sourcelist:= Tstringlist.create;
  targetlist:= tstringlist.create;
  try
    // create folders in case they does not exist
    if ACreateFolders then
    begin
      if not directoryexists(fromfolder) then forcedirectories(fromfolder);
      if not directoryexists(tofolder) then forcedirectories(tofolder);
    end;
    // generate lists of both folders
    findfiles(sourcelist, fromfolder, '*.*', fromfolder, AIncludeSubs, TRUE, AIncludeHiddenFiles, '|', FALSE);
    findfiles(targetlist, tofolder, '*.*', tofolder, AIncludeSubs, TRUE, AIncludeHiddenFiles, '|', FALSE);
    // go trough listing
    for sourcepos:=0 to sourcelist.count-1 do
    begin
      if not ParseListDetails(sourcelist[sourcepos], Filename1, Filetime1, Filesize1, Fileattr1, '|') then
        raise exception.Create('Internal error:'+#13+sourcelist[sourcepos]+#13+'couldn''t be parsed.');
      // check if source exists on target with same name and older time stamp
      // we don't care about the size now
      p:= -1;
      for targetpos:=0 to targetlist.count-1 do
      begin
        if not ParseListDetails(targetlist[targetpos], Filename2, Filetime2, Filesize2, Fileattr2, '|') then
          raise exception.Create('Internal error:'+#13+targetlist[targetpos]+#13+'couldn''t be parsed.');
        if (FileName2=Filename1) and (Filetime2>=Filetime1) then
        begin
          p:= targetpos;
          break;
        end;
      end;
      // overwrite or create new if was not found
      if p<0 then
      begin
        filename2:= addbackslash(tofolder)+filename1; // target name
        filename1:= addbackslash(fromfolder)+filename1; // source name
        if not directoryexists(extractfiledir(filename2)) then forcedirectories(extractfiledir(filename2)); // create directory
        //showmessage(filename1+#13+filename2);
        api_files.CopyFile(filename1, filename2, AShowProgress, AConfirm, FALSE, FALSE); // copy file
      end;
    end;
  finally
    freeandnil(sourcelist);
    freeandnil(targetlist);
  end;
end;

//------------------------------------------------------------------------------
function Driveserial(Const drive: Char): string;
var
  SerialNumber:DWORD;
  OldErrorMode: Integer;
  NotUsed,VolFlags: DWORD;
  Buf: array [0..MAX_PATH] of Char;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Buf[0] := #$00;
    if GetVolumeInformation(PChar(Drive + ':\'),Buf,DWORD(sizeof(Buf)),@SerialNumber,NotUsed,VolFlags,nil,0)
      then Result := inttohex(SerialNumber,8)
      else Result := '';
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

//------------------------------------------------------------------------------
function IsOnLocalDrive(fname: string): boolean;
var
  adrive: string;
begin
  adrive := ExtractFileDrive(fname);
  if (GetDriveType(PChar(aDrive)) = DRIVE_REMOVABLE) or (GetDriveType(PChar(aDrive)) = DRIVE_FIXED) then
    Result := True
    else result := False;
end;

function tAPI_files.isonlocaldrive(Const fname: string): boolean;
begin
  result:= api_files.isonlocaldrive(fname);
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_files]);
end;

end.
