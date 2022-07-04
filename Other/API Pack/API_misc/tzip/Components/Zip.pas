//------------------------------------------------------------------------------
// Component         TZip Component                                            .
// Version:          1.4                                                       .
// Date:             6 April 2003                                              .
// Compilers:        Delphi 3 - Delphi 7.                                      .
// Author:           Angus Johnson - angusj-AT-myrealbox-DOT-com               .
// Copyright:        © 2001-2003 Angus Johnson                                 .
//                                                                             .
// Description:      Delphi interface to the ZipDll.dll & UnzDll.dll libraries .
//                   created by Eric W. Engler and Chris Vleghert. (SFX        .
//                   support is provided but not as part of this component.)   .
//                                                                             .
// Acknowledgements: Based on the TZipMstr component which was created by      .
//                   Eric W. Engler and Chris Vleghert.                        .
// -----------------------------------------------------------------------------

//Updated 6 April 2003;
// Added Delphi 7 compiler options.

//Updated 13 June 2002;
// *Added IsMultiDiskZip boolean property.

//Updated 5 June 2002;
//  * Bugfix: The Password (if any) wasn't being assigned to each
//  individual fFileSpec when adding files.
//  * Bugfix: MergeArchiveTo() method was broken.

//Updated 22 April 2002:
//  * Bugfix: ChDir() added back into the Extract and Add operations as it still
//  seems to be required inspite of changes to the ver160p dll's.

//Updated 21 March 2002:
//  * Bugfix: The Progressbar previously raised an error with filesizes > 65535
//  whenever the OS was using COMCTL32.DLL version less than 4.70.
//  * Moved several unit scope variables into TZip to improve encapsulation.
//  * ExtractErrorCnt property added.
//  * NOW REQUIRES DLL's VER 160p OR LATER because :
//    - ExtractDir functionality has changed
//    - AddDir functionality has changed
//  * A zip archive is now automatically backed-up before adding files in case
//  the op is cancelled which would otherwise result in a corrupted archive.

//Updated 23 February 2002:
//  Bugfix: If ShowProgressDialog is enabled, and an Add or Extract operation
//  was called prior to creating application.mainform then an exception
//  was raised. (spotted by Colin Kemp)
//  Bugfix: The AddOptions flag - aoWithFullPath - was being ignored if
//  relative paths were used in FileSpecList. (spotted by Colin Kemp)

//Updated 22 December 2001:
//  Bugfix: GetVolumeName() was limiting the length of the returned string to
//  11 chars which caused problems with NTFS drives. (spotted by Andreas Bormann)

//Updated 18 December 2001:
//  VER140_PLUS compiler conditional added
//  Minor improvements to password prompts

//Updated 29 October 2001:
//  Resourcestring s_unable_to_load was missing a '%s' in its definition

//Updated 5 Sept 2001:
//  The TZip.AddPath property is now unassigned after each Add method call.
//  If the TZip.AddPath property is assigned, TZip will change the current
//  directory to that path (ChDir) just before Adding.

(*
Current Dll issues (ver 1.6p):
  1. when adding Foldernames to FileSpecList, a trailing slash is required
     to indicate they are folders.
  2. ??bug - if filenames are stored in OEM format the dll will extract but
     not delete them.
  3. cancelling an add/delete operation corrupts the zip file. A workaround
     requires saving a copy of the file before these operations.
  4. When extracting files using many FileSpecs (eg > 1000), the dll takes a
     very long time to process these fileSpecs resulting in the application
     appearing to "hang". Partial workaround: use wildcard FileSpecs whenever
     possible (see the demo).
*)

unit Zip;

{$IFDEF VER120} //delphi 4
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER125} //bcb 4
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER130} //delphi 5
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER135} //bcb 5
  {$DEFINE VER120_PLUS}
{$ENDIF}
{$IFDEF VER140} //delphi 6
  {$DEFINE VER120_PLUS}
  {$DEFINE VER140_PLUS}
{$ENDIF}
{$IFDEF VER150} //delphi 7
  {$DEFINE VER120_PLUS}
  {$DEFINE VER140_PLUS}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ZipDlls, StdCtrls, ComCtrls {$IFNDEF VER140_PLUS}, FileCtrl {$ENDIF};

type

{$WARNINGS OFF} //hides 'Unsafe Type' compiler warnings

  ZipException = class(Exception);

  TAddOption = (aoFreshen, aoUpdate, aoRecursive, aoWithFullPath,
    aoFolderEntries, aoZipTime, aoHiddenFiles, aoForceDOS);
  TAddOptions = set of TAddOption;

  //TExtractOptions -
  //nb: newer files will be overwritten automatically if eoUpdate or eoFreshen
  //set. eoAskOverwrite will be ignored with eoUpdate or eoFreshen options.
  TExtractOption = (eoUpdate, eoFreshen, eoTest, eoAskOverwrite, eoWithPaths);
  TExtractOptions = set of TExtractOption;

  //Zip/Extract/Delete (ZED) event methods...
  TZEDBeginFileEvent  = procedure(Sender: TObject; const Filename: string;
    FileSize, BatchCount, TotalBytesInBatch: longint) of object;
  TZEDEndBatchEvent  = procedure(Sender: TObject;
    SkippedCount, BatchCount, TotalBytesInBatch: longint) of object;

  //All purpose progress event...
  TProgressEvent = procedure(Sender: TObject; const Filename: string;
    BytesDone, TotalBytes: longint) of object;

  //A record for each file within the Zip archive ...
  pFileInfo = ^TFileInfo;
  TFileInfo = packed record
    cfh           : TCentralFileHeader;
    PasswordUsed  : Boolean;
    Filename      : ansistring;   //ANSI format
    ExtraField    : ansistring;
    FileComment   : ansistring;   //ANSI format
    Reserved1     : Cardinal; //2 fields used internally by TZip when
    Reserved2     : Cardinal; //creating spanned archives.
  end;

  TEOCHInfo = packed record
    eoch        : TEndOfCentralHeader;
    ZipComment  : ansistring;     //ANSI format
    EocOffset   : integer;
  end;

  TZip = class(TComponent)
  private
    fFilename: string;
    fPassword: string;
    fDllPath: string;
    fTempPath: string;
    fRequiresOemAnsiConversion: boolean;
    //fFileList contains a list of pointers to TFileInfo records ...
    fFileList: TList;
    fFileSpecList: TStrings;
    fEocInfo: TEOCHInfo;
    fCancel: boolean;
    fExtractErrors: integer;
    fShowProgress: boolean;
    fTrueSfxSize: integer;
    fOnContentsChanged: TNotifyEvent;
    fProgressRatio: single;
    fProgressForm: TCustomForm;
    fSaveTraceLog: boolean;
    traceLogStream: TStream;

    BatchFilename: string;
    BatchCount,
    BatchBytesTotal,
    BatchBytes,
    BatchSkipped: longint;


    fAddPath: string;
    fAddOptions: TAddOptions;

    fExtractPath: string;
    fExtractOptions: TExtractOptions;

    fZEDBeginFileEvent: TZEDBeginFileEvent;
    fZEDProgressEvent: TProgressEvent;
    fZEDEndBatchEvent: TZEDEndBatchEvent;
    fMergeProgressEvent: TProgressEvent;
    fSpanProgressEvent: TProgressEvent;
    procedure SetFilename(const filename: string);
    procedure SetFileSpecList(strings: TStrings);
    procedure ClearFileList;
    procedure LoadFileList;
    function GetFileInfo(index: integer): TFileInfo;
    function IsMultiDiskArchive: boolean;

    function GetCount: integer;
    function LoadZipDll: boolean;
    function LoadUnZipDll: boolean;
    procedure UnLoadDlls;
    procedure SetPassword(const password: string);
    function DoAddDelete(IsAdd: boolean): integer;

    // get/set general zip file comment...
    function GetZipComment: string;
    procedure SetZipComment(comment: string);
    // get/set individual file comments...
    function GetFileComment(index: integer): string;
    procedure SetFileComment(index: integer; comment: string);

    function OemStrToAnsiStr(const s: string): ansistring;
    function AnsiStrToOemStr(const s: ansistring): string;

  protected
    { Protected declarations }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function Extract: integer; //returns No. files extracted
    function Add: integer;     //returns No. files added
    function Delete: integer;  //returns No. files deleted

    procedure Cancel;

    //merge a multi disk Zip file into one file (on hard disk)
    function MergeArchiveTo(const Target: string): boolean;
    //create a multi disk Zip file from one file (on hard disk)
    function SpanArchiveTo(const Target: string;
      MaxBytesPerDisk, BytesToReserveDisk1: integer): boolean;

    property Count: integer read GetCount;

    //No. of files not extracted due to errors...
    property ExtractErrorCnt: integer read fExtractErrors;
    property Cancelled: boolean read fCancel;
    
    property FileInfos[index: integer]: TFileInfo read GeTFileInfo;
    //Name of Zip archive (will auto create a new file if it doesn't exist)...
    property Filename: string read fFilename write SetFilename;
    //List of files to add/extract/delete (with or without wildcards *.*) ...
    property FileSpecList: TStrings read fFileSpecList write SetFileSpecList;
    //Optional path to Dlls if they're not stored in exe's folder or shell's path ...
    property DllPath: string read fDllPath write fDllPath;
    //Base path to which files will be extracted
    //(otherwise files will be extracted into same folder as Zip archive)
    property ExtractPath: string read fExtractPath write fExtractPath;

    property AddPath: string read fAddPath write fAddPath;
    property AddOptions: TAddOptions read fAddOptions write fAddOptions;

    property ExtractOptions: TExtractOptions read fExtractOptions write fExtractOptions;

    property FileComments[index: integer]: string read GetFileComment write SetFileComment;
    property ZipComment: string read GetZipComment write SetZipComment;
    property Password: string read fPassword write SetPassword;
    //SaveTraceLog is only useful when debugging the DLLs
    property SaveTraceLog: boolean read fSaveTraceLog write fSaveTraceLog;
    property SfxSize: integer read fTrueSfxSize;
    property IsMultiDiskZip: boolean read IsMultiDiskArchive;
  published
    property ShowProgressDialog: boolean read fShowProgress write fShowProgress;
    //Zip/Extract/Delete (ZED) events...
    property OnZEDBeginFileEvent: TZEDBeginFileEvent
      read fZEDBeginFileEvent write fZEDBeginFileEvent;
    property OnZEDProgressEvent: TProgressEvent
      read fZEDProgressEvent write fZEDProgressEvent;
    property OnZEDEndBatchEvent: TZEDEndBatchEvent
      read fZEDEndBatchEvent write fZEDEndBatchEvent;
    //Merge and Span progress events...
    property OnMergeProgress: TProgressEvent
      read fMergeProgressEvent write fMergeProgressEvent;
    property OnSpanProgress: TProgressEvent
      read fSpanProgressEvent write fSpanProgressEvent;
    //When a new zip is opened, or files added to or deleted from zip ...
    property OnContentsChanged: TNotifyEvent read fOnContentsChanged write fOnContentsChanged;
  end;

//Declared in the interface section as also needed by SfxUtils.pas
function FindEOCHeaderOffset(stream: TStream): longint;

//wrapper functions for WinAPI functions...
function GetVolumeName(DrivePath: string; out VolumeName: string): boolean;
function GetDiskType(const filename: string): integer;
function GetTempPath: string;
function GetTempFilename: string;

function AppendSlash(const str: string): string;

procedure Register;

const
  ERROR_VALUE = -1;
  MAX_PASSWORD_RETRIES = 3;
  CRLF = #13#10;
  
//add in all the resource strings ...
//(An include file is used to simplify translations.)

{$include '..\..\..\API_source\inc\CompilerVersions.INC'}
{$I zip_str_english.txt}

implementation

type

  TProgressForm = class(TCustomForm)
  private
    OwnerZip: TZip;
    TextLabel: TLabel;
    CancelButton: TButton;
    ProgressBar: TProgressBar;
    procedure CancelClicked(Sender: TObject);
    procedure SetProgressMax(max: integer);
    procedure SetProgressPosition(position: integer);
    procedure SetText(text: string);
  public
    property ProgressMax: integer write SetProgressMax;
    property ProgressPosition: integer write SetProgressPosition;
    property Text: string write SetText;
    constructor CreateNew(Owner: TComponent
      {$IFDEF VER120_PLUS};Dummy: Integer = 0); override;{$ELSE});{$ENDIF}
  end;

//---------------------------------------------------------------------
// Miscellaneous functions...
//---------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('Samples', [TZip]);
end;
//---------------------------------------------------------------------

function min(a,b: integer): integer;
begin
  if a < b then result := a else result := b;
end;
//---------------------------------------------------------------------

procedure CenterWindowInDesktop(Window: HWnd);
var
  r: TRect;
  l,t,w,h: integer;
begin
  //get the window dimensions...
  GetWindowRect(Window,r);
  w := r.Right-r.Left;
  h := r.Bottom-r.Top;
  //get the desktop workarea (excluding the taskbar)...
  SystemParametersInfo(SPI_GETWORKAREA,0,@r,0);
  //center the window...
  l := r.Left + (r.Right-r.Left - w) div 2;
  t := r.Top + (r.Bottom -r.Top - h) div 2;
  SetWindowPos(Window,0,l,t,0,0,SWP_NOREDRAW or SWP_NOSIZE or SWP_NOZORDER);
end;
//---------------------------------------------------------------------

function SwapSlashes(const str: string): string;
var
  i: integer;
begin
  result := str;
  for i := 1 to length(result) do
    if result[i] = '/' then result[i] := '\';
end;
//---------------------------------------------------------------------

function AppendSlash(const str: string): string;
begin
  if (str <> '') and (str[length(str)] <> '\') then
    result := str +'\' else
    result := str;
end;
//---------------------------------------------------------------------

function GetDiskType(const filename: string): integer;
var
  drive: string;
begin
  if filename = '' then result := DRIVE_NO_ROOT_DIR else
  begin
    drive := AppendSlash(ExtractFileDrive(filename));
    result := GetDriveType(pchar(drive));
  end;
end;
//---------------------------------------------------------------------

function GetTempPath: string;
begin
  //get the OS's temppath...
  Setlength(result,MAX_PATH+1);
  Setlength(result,windows.GetTempPath(MAX_PATH, pchar(result)));
  if result = '' then result := extractfilepath(paramstr(0));
end;
//---------------------------------------------------------------------

function GetTempFilename: string;
begin
  setlength(result,MAX_PATH);
  windows.GetTempFilename(pchar(GetTempPath),'zzz',0,pchar(result));
  setlength(result,strlen(pchar(result)));
end;
//---------------------------------------------------------------------

//nb: GetDiskFree() is used internally only when creating
//multi-disk archives on floppies so I've chosen to ignore free space > 2gig
function GetDiskFree(const Filename: string): cardinal;
var
   SectorsPCluster, BytesPSector, FreeClusters, TotalClusters: DWORD;
   Drive: string;
begin
   Result   := 0;
   if (length(Filename) < 3) or (pos(':\',Filename) <> 2) then exit;
   Drive := copy(Filename,1,3);
   if GetDiskFreeSpace( pChar( Drive ),
     SectorsPCluster, BytesPSector, FreeClusters, TotalClusters ) then
        result := FreeClusters * SectorsPCluster * BytesPSector;
end;
//---------------------------------------------------------------------

function GetVolumeName(DrivePath: string; out VolumeName: string): boolean;
var
  OldErrMode: dword;
  VolName: array[0..255] of char; //nb: can be > 11 chars with NTFS drives
  dummy1: Cardinal;
  dummy2: dword;
begin
  DrivePath := AppendSlash(DrivePath);
  OldErrMode := SetErrorMode( SEM_FAILCRITICALERRORS );
  result := GetVolumeInformation(pchar(DrivePath), VolName, sizeof(VolName), nil, dummy1, dummy2, nil, 0 );
  if result then VolumeName := VolName;
  SetErrorMode( OldErrMode );
end;
//---------------------------------------------------------------------

function GetSpannedZipDiskNum(const Filename: string): integer;
var
  Volume: string;
begin
  result := 0;
  //nb: cannot detect floppy disks if across a network
  if not FileExists(Filename) or (GetDiskType(Filename) <> DRIVE_REMOVABLE) or
    not GetVolumeName(ExtractFileDrive(Filename),Volume) then exit;
  if (Pos( 'PKBACK# ', Volume) = 1) then
      result := StrToIntDef(Copy(Volume,9,3), 0);
end;
//---------------------------------------------------------------------

function PromptMultiDiskNum(const filename: string; Num: integer): boolean;
var
  i: integer;
begin
  result := false;
  i := GetSpannedZipDiskNum(filename);
  while i <> num do
  begin
    if application.messagebox(pchar(format(s_multidisk_prompt, [Num])),
      pchar(application.title), mb_okcancel or mb_iconinformation) <> IDOK then
        exit; //cancelled
    i := GetSpannedZipDiskNum(filename);
  end;
  result := true;
end;
//---------------------------------------------------------------------

function PrependPath(const path, filename: string): string;
begin
  if (length(filename) > 2) and
    ((filename[1] = '\') or (filename[2] = ':')) then
    result := filename else
    result := appendSlash(path)+filename;
end;

//---------------------------------------------------------------------
// TProgressForm methods ...
//---------------------------------------------------------------------

const
  PROGRESS_CLIENTWIDTH = 300;

constructor TProgressForm.CreateNew(Owner: TComponent
  {$IFDEF VER120_PLUS};Dummy: Integer);{$ELSE});{$ENDIF}
begin
  inherited CreateNew(Owner);
  OwnerZip := Owner as TZip;
  BorderStyle := bsDialog;
  Caption := application.title;
  ClientWidth := PROGRESS_CLIENTWIDTH;
  ClientHeight := 100;
  FormStyle := fsStayOnTop;

  //center the progressForm over the middle of the mainform...
  if not assigned(application.mainform) then
    CenterWindowInDesktop(handle)
  else
    with application.mainform do
      self.SetBounds(left + (width-self.width) div 2,
        top + (height-self.height) div 2, self.width, self.height);

  //create controls...
  TextLabel := TLabel.Create(self);
  with TextLabel do
  begin
    Parent := self;
    AutoSize := False;
    Top := 15;
    caption := s_wait_caption;
    width := PROGRESS_CLIENTWIDTH -20;
    Left := (PROGRESS_CLIENTWIDTH - width) div 2;
  end;
  ProgressBar := TProgressBar.create(self);
  with ProgressBar do
  begin
    Parent := self;
    width := PROGRESS_CLIENTWIDTH -20;
    Left := (PROGRESS_CLIENTWIDTH - width) div 2;
    Top := 35;
  end;
  CancelButton := TButton.Create(self);
  with CancelButton do
  begin
    Parent := self;
    Caption := s_cancel_caption;
    Default := True;
    Top := 60;
    Left := (PROGRESS_CLIENTWIDTH-width) div 2;
    OnClick := CancelClicked;
  end;
end;
//---------------------------------------------------------------------

procedure TProgressForm.CancelClicked(Sender: TObject);
begin
  OwnerZip.fCancel := true;
  hide;
end;
//---------------------------------------------------------------------

procedure TProgressForm.SetProgressMax(max: integer);
begin
  ProgressBar.Max := max;
end;
//---------------------------------------------------------------------

procedure TProgressForm.SetProgressPosition(position: integer);
begin
  ProgressBar.Position := position;
end;
//---------------------------------------------------------------------

procedure TProgressForm.SetText(text: string);
begin
  TextLabel.Caption := text;
  TextLabel.Left := (PROGRESS_CLIENTWIDTH - TextLabel.width) div 2;
end;

//---------------------------------------------------------------------
// TZip methods...
//---------------------------------------------------------------------

constructor TZip.create(aOwner: TComponent);
begin
  inherited;
  fFileList := TList.create;
  fFileSpecList := TStringList.create;
  fShowProgress := true;
  fAddOptions := [aoUpdate];      //default to update
  fExtractOptions := [eoUpdate];
  fTempPath := GetTempPath;
end;
//---------------------------------------------------------------------

destructor TZip.destroy;
begin
  fFileSpecList.free;
  ClearFileList;
  fFileList.free;
  UnLoadDlls; //unloads dlls if loaded
  inherited destroy;
end;
//---------------------------------------------------------------------

function TZip.LoadZipDll: boolean;
var
  fullname: string;
  OldErrMode: dword;
begin
  result := true;
  if ZipDllHandle <> 0 then exit; //returns non-zero if already loaded
  if fDllPath = '' then
    fullname := 'ZipDll.dll' else
    fullname := AppendSlash(fDllPath)+ 'ZipDll.dll';

  OldErrMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  try
    ZipDllHandle := LoadLibrary(pChar(fullname));
    if ZipDllHandle = 0 then exit; //error!
    @ZipDllExec := GetProcAddress( ZipDllHandle, 'ZipDllExec' );
    @GetZipDllVersion := GetProcAddress( ZipDllHandle, 'GetZipDllVersion' );
    if (@ZipDllExec = nil) or (@GetZipDllVersion = nil) then //error
    begin
      FreeLibrary( ZipDllHandle );
      ZipDllHandle := 0;
    end
    else if (GetZipDllVersion < 160) then
    begin
      FreeLibrary( ZipDllHandle );
      ZipDllHandle := 0;
      ZipException.create(s_invalid_dll_version);
    end;
  finally
    SetErrorMode(OldErrMode);
    result := (ZipDllHandle <> 0);
  end;
end;
//---------------------------------------------------------------------

function TZip.LoadUnZipDll: boolean;
var
  fullname: string;
  OldErrMode: dword;
begin
  result := true;
  if UnZipDllHandle <> 0 then exit; //returns non-zero if already loaded
  if fDllPath = '' then
    fullname := 'UnzDll.dll' else
    fullname := AppendSlash(fDllPath)+ 'UnzDll.dll';
  OldErrMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  try
    UnZipDllHandle := LoadLibrary(pChar(fullname));
    if UnZipDllHandle = 0 then exit; //error!
    @UnZipDllExec := GetProcAddress( UnZipDllHandle, 'UnzDllExec' );
    @GetUnZipDllVersion := GetProcAddress( UnZipDllHandle, 'GetUnzDllVersion' );
    if (@UnZipDllExec = nil) or (@GetUnZipDllVersion = nil) then //error
    begin
      FreeLibrary( UnZipDllHandle );
      UnZipDllHandle := 0;
    end
    else if (GetUnZipDllVersion < 160) then
    begin
      FreeLibrary( UnZipDllHandle );
      UnZipDllHandle := 0;
      ZipException.create(s_invalid_dll_version);
    end;
  finally
    SetErrorMode(OldErrMode);
    result := (UnZipDllHandle <> 0);
  end;
end;
//---------------------------------------------------------------------

procedure TZip.UnLoadDlls;
begin
   if ZipDllHandle <> 0 then
   begin
     FreeLibrary( ZipDllHandle );
     ZipDllHandle := 0;
   end;
   if UnZipDllHandle <> 0 then
   begin
     FreeLibrary( UnZipDllHandle );
     UnZipDllHandle := 0;
   end;
end;
//---------------------------------------------------------------------

procedure TZip.SetFileSpecList(strings: TStrings);
begin
  fFileSpecList.assign(strings);
end;
//---------------------------------------------------------------------

procedure TZip.ClearFileList;
var
  i: integer;
begin
  for i := 0 to fFileList.count-1 do dispose(pFileInfo(fFileList[i]));
  fFileList.clear;
end;
//---------------------------------------------------------------------

function TZip.GetCount: integer;
begin
  result := fFileList.count;
end;
//---------------------------------------------------------------------

function TZip.GetFileInfo(index: integer): TFileInfo;
begin
  if (index < 0) or (index >= fFileList.count) then
    raise ZipException.create(s_index_range_error)
  else
    result := pFileInfo(fFileList[index])^;
end;
//---------------------------------------------------------------------

procedure TZip.SetPassword(const password: string);
begin
  if length(password) <= PASSWORD_MAXLEN then
    fPassword := password else
    raise ZipException.create(s_password_too_long);
end;
//---------------------------------------------------------------------

procedure TZip.SetFilename(const filename: string);
var
  vol: string;
  Stream: TfileStream;
begin
  //allow assigning the same filename to force reinitializing...

  //reinitialize values...
  fPassword := '';
  fRequiresOemAnsiConversion := false;
  FillChar(fEocInfo.eoch, sizeof(TEndOfCentralHeader),0);
  fEocInfo.ZipComment := '';
  fEocInfo.EocOffset := ERROR_VALUE;

  fEocInfo.eoch.HeaderSig := EOC_HEADERSIG;
  fTrueSfxSize := 0;
  ClearFileList;
  fFilename := filename;
  if filename <> '' then
  begin
    //first check there is a disk in the selected drive...
    if not GetVolumeName(ExtractFileDrive(filename), vol) then
        raise ZipException.create(s_unable_to_access_drive);

    //if filename doesn't exist, create a new empty Zip file...
    if not fileexists(filename) then
    begin
      try
        Stream := TfileStream.create(filename,fmCreate);
        try
          //write just the fixed length fields...
          Stream.write(fEocInfo.eoch , SizeOf(TEndOfCentralHeader));
        finally
          Stream.free;
        end;
      except
        raise ZipException.createfmt(s_unable_to_create,[filename]);
      end;
    end;
    
    LoadFileList;
  end;
  if assigned(fOnContentsChanged) then fOnContentsChanged(self);
end;
//---------------------------------------------------------------------

//returns stream position (or -1 if not found)...
function FindEOCHeaderOffset(stream: TStream): longint;
var
  buffSize: integer;
  Buffer, buffptr: pchar;
  Eoc: TEndOfCentralHeader;
begin
  result := ERROR_VALUE;
  if Stream.size < sizeof(TEndOfCentralHeader) then exit; //can't be a zip file

  //Note: A zip file comment may exist at the end of the archive just
  //after the EndOfCentralHeader record. As its length is unknown (but <= 65k),
  //we may have to seek for the EOCHeader record.

  Stream.seek(-sizeof(TEndOfCentralHeader), soFromEnd);
  if Stream.read(Eoc, sizeof(TEndOfCentralHeader)) <> sizeof(TEndOfCentralHeader) then
    exit; //error

  if Eoc.HeaderSig = EOC_HEADERSIG then //found
  begin
    Stream.seek(-sizeof(TEndOfCentralHeader),soFromCurrent);
    result := stream.Position;
    exit;
  end;

  buffSize := min(Stream.size,MAXWORD); //ie: buffSize <= 65k
  buffer := AllocMem(buffSize);
  try
    Stream.seek(-buffSize,soFromEnd);
    //read the block into the buffer...
    if Stream.read(Buffer^,buffSize) <> buffSize then exit;

    //perform a backwards search for the EOC_HEADERSIG ('PK'#5#6)...
    buffptr := buffer + buffSize - sizeof(TEndOfCentralHeader);
    while buffptr >= buffer do
    begin
      if pdword(buffptr)^ = EOC_HEADERSIG then //FOUND!!
      begin
        //move the stream position to the beginning of the EOCHeader...
        Stream.seek(Stream.position - buffSize + (buffptr-buffer), soFromBeginning);
        result := stream.Position;
        exit;
      end;
      dec(buffptr);
    end;
  finally
    FreeMem(Buffer);
  end;
end;
//---------------------------------------------------------------------

procedure TZip.LoadFileList;
var
  i: integer;
  stream: TStream;

  //-------------------------------------------------------------------

  function GetCentralHeaderInfo: boolean;
  var
    i,j: integer;
    cfhdr: TCentralFileHeader;
    FileInfo: pFileInfo;
  begin
    result := false;
    if fEocInfo.eoch.ThisDiskNo <> 0 then //multi disk zip
      Stream.Position := fEocInfo.eoch.CentralOffSet
    else
      //nb: fEocInfo.eoch.CentralOffSet may NOT be accurate with SFX files
      //as the zip file may have simply been appended to the Sfx stub
      //without updating the EndOfCentralHeader.
      Stream.Position := fEocInfo.EocOffset - fEocInfo.eoch.CentralSize;

    fTrueSfxSize := Stream.Size; //set to an impossible value

    //read each central file header record ...
    i := 0;
    while i < fEocInfo.eoch.TotalEntries do
    begin
      if (Stream.read(cfhdr,sizeof(TCentralFileHeader)) <> sizeof(TCentralFileHeader)) then
      begin
        //if single disk zip then error!!
        if (fEocInfo.eoch.ThisDiskNo = 0) then exit;

        //Hopefully the read of header record failed due to spanning to
        //the next disk (multi-disk archive) ...
        j := GetSpannedZipDiskNum(Filename);
        //(nb: GetSpannedZipDiskNum() returns the current Disk No *1* based
        //while fEocInfo.eoch.ThisDiskNo is *zero* based. )
        if (j = 0) or (j > fEocInfo.eoch.ThisDiskNo) then exit; //error!!
        Stream.free;
        Stream := nil;
        //prompt for the next disk...
        if not PromptMultiDiskNum(fFilename, j+1) then exit;
        Stream := TFileStream.create(Filename,fmOpenRead or fmShareDenyWrite);
        continue;
      end;
      
      if (cfhdr.HeaderSig <> CENTRAL_HEADERSIG) then exit; //error!!

      if (i = 0) then //only to do this once (ie assume they're all the same)...
      begin
        // OEM <-> ANSI conversion rules:
        // cfh.HostVersionNo = FAT, VFAT, FAT32 (0) -> do conversion
        // cfh.HostVersionNo = OS2 HPFS (6) -> do conversion  (OS2)
        // cfh.HostVersionNo = WIN NTFS (11) && cfh.MadeByVersion = 50 -> do conversion
        // else no conversion required
        if (cfhdr.HostVersionNo = 0) or (cfhdr.HostVersionNo = 6) or
           ((cfhdr.HostVersionNo = 11) and (cfhdr.MadeByVersion = 50)) then
              fRequiresOemAnsiConversion := true else
              fRequiresOemAnsiConversion := false;
      end;

      //ok, now store the file header data...
      new(FileInfo);
      fFileList.add(FileInfo);
      //get filename
      setlength(FileInfo.Filename, cfhdr.FileNameLength);
      if cfhdr.FileNameLength > 0 then
        if (Stream.read(FileInfo.Filename[1], cfhdr.FileNameLength) <>
          cfhdr.FileNameLength) then exit; //error

      FileInfo.Filename := SwapSlashes(OemStrToAnsiStr(FileInfo.Filename));
      //get ExtraField
      setlength(FileInfo.ExtraField, cfhdr.ExtraFieldLength);
      if cfhdr.ExtraFieldLength > 0 then
        if (Stream.read(FileInfo.ExtraField[1],cfhdr.ExtraFieldLength) <>
          cfhdr.ExtraFieldLength) then exit; //error
      //get comment
      setlength(FileInfo.FileComment, cfhdr.FileCommentLen);
      if cfhdr.FileCommentLen > 0 then
        if (Stream.read(FileInfo.FileComment[1], cfhdr.FileCommentLen) <>
          cfhdr.FileCommentLen) then exit; //error
      FileInfo.FileComment := OemStrToAnsiStr(FileInfo.FileComment);

      //this is a block move of all the fixed fields (46 bytes total)...
      move(cfhdr, FileInfo.cfh, sizeof(TCentralFileHeader));
      //simplify getting the password flag...
      FileInfo^.PasswordUsed := Odd(FileInfo.cfh.Flag);

      //find the very first LocalHeader offset
      //nb: they don't have to be in the same order as the CentralDirectory
      if cfhdr.RelOffLocalHdr < fTrueSfxSize then
        fTrueSfxSize := cfhdr.RelOffLocalHdr;
      inc(i);
    end;
      //nb: A zip file may have simply been appended to the Sfx stub so
      //the Header Offset values inaccurate.
      //The TrueSfxSize is derived from the perceived offset of the first file
      //plus the difference between the real and perceived EocHeader offsets.
      if (fEocInfo.eoch.ThisDiskNo = 0) and (fTrueSfxSize < Stream.Size) then
          fTrueSfxSize := fTrueSfxSize + (fEocInfo.EocOffset -
            (fEocInfo.eoch.CentralOffset + fEocInfo.eoch.CentralSize)) else
          fTrueSfxSize := 0;
    result := true;
  end;
  //-------------------------------------------------------------------

begin {LoadFileList}

  ClearFileList;
  if Filename = '' then exit; //should never happen!
  Stream := TFileStream.create(Filename,fmOpenRead or fmShareDenyWrite);
  try
    //get EOCHeader...
    while (FindEOCHeaderOffset(Stream) = ERROR_VALUE) do
    begin
      Stream.free;
      Stream := nil;
      i := GetSpannedZipDiskNum(Filename);
      if i = 0 then
        raise ZipException.createfmt(s_not_a_zip_file,[Filename]);

      if application.messagebox(pchar(format(s_multidisk_prompt_last,[i])),
        pchar(application.title), mb_okcancel or mb_iconinformation) <> idok then
          exit; //cancelled
      Stream := TFileStream.create(Filename,fmOpenRead or fmShareDenyWrite);
    end;

    fEocInfo.EocOffset := stream.Position;
    Stream.read(fEocInfo, sizeof(TEndOfCentralHeader));
    //several checks in case eoc.HeaderSig has been found eroneously...
    if (fEocInfo.eoch.ThisDiskNo > 1000) or
       (fEocInfo.eoch.CentralDiskNo > 1000) then
         raise ZipException.createfmt(s_not_a_zip_file,[Filename]);

    //get the general zip comment (if there is one)...
    SetLength(fEocInfo.ZipComment, fEocInfo.eoch.ZipCommentLen);
    if fEocInfo.eoch.ZipCommentLen > 0 then
      if (Stream.read(fEocInfo.ZipComment[1], fEocInfo.eoch.ZipCommentLen) <>
        fEocInfo.eoch.ZipCommentLen) then fEocInfo.ZipComment := '';
    fEocInfo.ZipComment := OemStrToAnsiStr(fEocInfo.ZipComment);

    if (fEocInfo.eoch.TotalEntries = 0) then exit; //ie: empty zip file.

    //EOC Header has been found, now find Central header...
    //and load the info into fFileList...

    //if a multiple disk archive, the CentralHeader may be on another disk ...
    //and so, make sure the correct disk is inserted before proceeding.
    //(nb: exits if the user selects [cancel] in the prompt dialog.)
    if (fEocInfo.eoch.ThisDiskNo <> fEocInfo.eoch.CentralDiskNo) then
      if not PromptMultiDiskNum(Filename, fEocInfo.eoch.CentralDiskNo) then exit;

    if not GetCentralHeaderInfo then
    begin
      ClearFileList; //CleanUp
      raise ZipException.create(s_cfh_read_error);
    end;

  finally
    Stream.free;
  end;
end;
//---------------------------------------------------------------------

procedure TZip.Cancel;
begin
  fCancel := true;
end;
//---------------------------------------------------------------------

type
  //workaround for issue in Dll ver 1.6 - add stUnknown as first emumerator ...
  UnZipSkipTypes = ( stUnknown, stOnFreshen, stNoOverwrite, stFileExists,
    stBadPassword, stNoEncryptionDLL, stCompressionUnknown,
    stUnknownZipHost, stZipFileFormatWrong, stGeneralExtractError );

function ZCallback( ZCallBackRec: PZCallBackStruct ): LongBool; stdcall; export;
var
  txt: string;
  res: integer;
begin

  //if already cancelled don't bother me any more...
  if not TZip(ZCallBackRec.Caller).fCancel then
    with ZCallBackRec^, TZip(Caller) do
      case ActionCode of
        1: begin
             BatchFilename := msg; //save for progress event (ActionCode = 2) ...
             if assigned(fZEDBeginFileEvent) then
               fZEDBeginFileEvent(Caller, BatchFilename, Size,
                 BatchCount, BatchBytesTotal);
             if fShowProgress and assigned(fProgressForm) then
               TProgressForm(fProgressForm).Text := BatchFilename;
           end;

        2: //Size = size of each block just processed (usually <= 32k)
           begin
             inc(BatchBytes, Size);
             if assigned(fZEDProgressEvent) then
               fZEDProgressEvent(Caller, BatchFilename, Size, BatchBytesTotal);
             if fShowProgress and assigned(fProgressForm) then
               TProgressForm(fProgressForm).ProgressPosition :=
                 trunc(BatchBytes * fProgressRatio);
           end;

        3: //end of a batch of ZIP/UNZIP operations.
           begin
             if Assigned(fZEDEndBatchEvent) then
               fZEDEndBatchEvent(Caller, BatchSkipped, BatchCount, BatchBytesTotal);
             if fShowProgress and assigned(fProgressForm) then
               fProgressForm.hide;
           end;

        4: if assigned(traceLogStream) and (msg <> '') then
           begin
             //msg = routine status message - ErrorCode occasionally used
             traceLogStream.Write(ZCallBackRec^.msg[0], strlen(msg));
             traceLogStream.Write(CRLF, 2);
           end;


        5: BatchCount := Size; //No. of Files in batch

        6: begin
             BatchBytesTotal := Size;
             BatchBytes := 0;    //reset
             BatchSkipped := 0;  //reset
             fExtractErrors := 0;
             if BatchBytesTotal = 0 then
               fProgressRatio := 1 else
               fProgressRatio := 65535 / BatchBytesTotal;
             if fShowProgress and assigned(fProgressForm) then
               TProgressForm(fProgressForm).ProgressMax := 65535;
           end;

        7: ;//enables setting a new path+name just before zipping...
            //get/set fullname using Msg, flag to change by returning ActionCode := 8;
            //if Assigned(FOnSetNewName) then FOnSetNewName(Caller, Msg);

        8: //wrong or no password.
           begin
             txt := '';
             //Msg = filename...
             if fShowProgress and assigned(fProgressForm) then
               fProgressForm.hide;
             if Size = MAX_PASSWORD_RETRIES then
               txt := InputBox(application.title,
                 format(s_new_password_required, [StrPas(Msg)]),txt) else
               txt := InputBox(application.title, s_invalid_password_retry, txt);
             if txt <> '' then
             begin
               StrPCopy(Msg,txt);           //Msg = new password
               ErrorCode := 1;              //new attempt flagged
               if Size > 0 then Dec(Size);  //No. of retries
             end else
             begin
               if application.messagebox(pchar(s_no_password_query_continue),
                 pchar(application.title),
                 mb_yesno or mb_iconquestion) <> idYes then fCancel := true;
               //skip this file...
               ErrorCode := 0;
               Size := 0;
             end;
             //nb: continue with show progress as other files may still be extracted...
             if fShowProgress and assigned(fProgressForm) and not fCancel then
               fProgressForm.show;
           end;

        9: //CRC32 error...
            begin
             if fShowProgress and assigned(fProgressForm) then
               fProgressForm.hide;
              txt := format(s_crc_error_query_extract,[StrPas(Msg)]);
              res := application.messagebox(pchar(txt),pchar(application.title),
                mb_yesnocancel or mb_iconquestion or mb_defbutton2);
              case res of
                idcancel:  fCancel := true; //cancel extract op.
                idyes:     ErrorCode := 1;
                else       ErrorCode := 0; //skip (2 = issue warnings)
              end;
             if fShowProgress and assigned(fProgressForm) and not fCancel then
               fProgressForm.show;
            end;

        10: // Extract Overwrite check ...
            if (fExtractOptions * [eoFreshen, eoUpdate, eoAskOverwrite] = []) then
              Size := 1 //overwrite irrespective of file age
            else if (fExtractOptions * [eoFreshen,eoUpdate] <> []) then
              Size := (ErrorCode and $1) //overwrite if existing is older
            else // if (eoAskOverwrite in fExtractOptions) then
            begin
              if fShowProgress and assigned(fProgressForm) then
                fProgressForm.hide;
              if Odd(ErrorCode) then //existing is older...
              begin
                txt := format(s_older_file_query_extract,[StrPas(Msg)]);
                res := application.messagebox(pchar(txt),pchar(application.title),
                    mb_yesnocancel or mb_iconinformation or mb_defbutton1);
              end else
              begin //existing is newer...
                txt := format(s_newer_file_query_extract,[StrPas(Msg)]);
                res := application.messagebox(pchar(txt),pchar(application.title),
                    mb_yesnocancel or mb_iconstop or mb_defbutton2);
              end;
              case res of
                idcancel: begin Size := 0; fCancel := true; end; //cancel extract
                idyes: Size := 1; //continue with extract
                else Size := 0;   //skip this file only
              end;
              if fShowProgress and assigned(fProgressForm) and not FCancel then
                fProgressForm.hide;
            end;

        11: // Extract(UnZip) was skipped and why ...
            begin
              inc(BatchSkipped);
              if UnZipSkipTypes(ZCallBackRec.size) in
                [stBadPassword .. stGeneralExtractError] then
                  inc(fExtractErrors);     //oops!!!
            end;
    end;

  //If result = TRUE, the DLL will abort ASAP
  Result := TZip(ZCallbackRec.Caller).fCancel;
  Application.ProcessMessages;
end;
//--------------------------------------------------------------------------

procedure SetUnZipParams(UnZipParams: pUnZipParams; Owner: TZip);
begin
  with UnZipParams^ do
  begin
      //nb: all zero initialized
      Version := ZIP_VERSION;
      Caller  := Owner;
      Quiet  := True;
      ZCallbackFunc := ZCallback;
      TraceEnabled := False;
      VerboseEnabled := False;
      Quiet    := True;
      Comments := False;  //supported external to dll
      Convert  := False;  //ascii/EBCDIC conversion - not supported

      // ? recreate directory structure
      Directories := eoWithPaths in Owner.fExtractOptions;
      //These fields seems to be ignored by the dll and handled in ZCallback().
      Overwrite := (Owner.fExtractOptions * [eoFreshen, eoUpdate, eoTest] = []);
      if eoUpdate in Owner.fExtractOptions then Update := True
      else if eoFreshen in Owner.fExtractOptions then Freshen := True
      else if eoTest in Owner.fExtractOptions then Test := True;

      PwdReqCount := MAX_PASSWORD_RETRIES;
      Seven := $7;
  end;
end;
//---------------------------------------------------------------------

function TZip.Extract: integer;
var
  i: integer;
  UnZipParams: pUnZipParams;
  pUZFD: pUnzipFileData;
  RootDir: string;
begin
  result := 0;
  fCancel := false;
  if (fFileList.count = 0) then exit;
  if fEocInfo.eoch.ThisDiskNo > 0 then //multi-disk zip archive ...
  begin
    application.messagebox(pchar(s_multidisk_op_error),
      pchar(application.title),mb_iconinformation);
    exit;
  end;

  if (fExtractPath <> '') and not DirectoryExists(fExtractPath)  then
  begin
    application.messagebox(pchar(format(s_extract_error_no_folder,
      [fExtractPath])), pchar(application.title),mb_iconinformation);
    exit;
  end;

  if not LoadUnZipDll then
    raise ZipException.createfmt(s_unable_to_load,['"UnzDll.dll".']);

  if fExtractPath = '' then
    RootDir := extractfilepath(fFilename) else
    RootDir := AppendSlash(fExtractPath);
  ChDir(RootDir); //still necessary in Win2000 & WinXP (using dll ver160p)

  UnZipParams := AllocMem(sizeof(TUnZipParams)); //zero initialized
  try
    SetUnZipParams(UnZipParams,self);
    {$ifdef VER210}
    UnZipParams.ZipFilename := pansichar(ansistring(fFilename));
    UnZipParams.ExtractRoot := pansichar(ansistring(RootDir));
    if fPassword <> '' then UnZipParams.ZipPassword := pansichar(ansistring(fPassword));
    {$else}
    UnZipParams.ZipFilename := pchar(fFilename);
    UnZipParams.ExtractRoot := pchar(RootDir);
    if fPassword <> '' then UnZipParams.ZipPassword := pchar(fPassword);
    {$endif}
    UnZipParams.FileDataCount := fFileSpecList.count;
    UnZipParams.UnzipFileData := AllocMem(sizeof(TUnzipFileData)*fFileSpecList.count);
    try
       pUZFD := UnZipParams.UnzipFileData;
       for i := 0 to fFileSpecList.count-1 do
       begin
         {$ifdef VER210}
         pUZFD.fFileSpec := pansichar(ansistring(fFileSpecList[i]));
         {$else}
         pUZFD.fFileSpec := pchar(fFileSpecList[i]);
         {$endif}
         inc(pUZFD);
       end;

       traceLogStream := nil;
       if fSaveTraceLog then
         traceLogStream :=
           TFileStream.create(changeFileExt(paramstr(0),'.trace'),fmCreate);
       try
         //if assigned(traceLogStream) then traceLogStream.Seek(0,soFromEnd);
         //DO IT HERE...
         if not fShowProgress then
           result := UnzipDLLExec(UnZipParams)
         else
         begin
           fProgressForm := TProgressForm.createnew(self);
           try
             fProgressForm.show;
             application.ProcessMessages;
             result := UnzipDLLExec(UnZipParams);
           finally
             fProgressForm.free;
             fProgressForm := nil;
           end;
         end;
      finally
        traceLogStream.free;
      end;

    finally
      FreeMem(UnZipParams.UnzipFileData);
    end;
  finally
    FreeMem(UnZipParams);
  end;
  fFileSpecList.clear;
end;
//---------------------------------------------------------------------

procedure SetZipParams(ZipParams: pZipParams; Owner: TZip; IsAdd: Boolean);
begin
   with ZipParams^ do
   begin
      Version  := ZIP_VERSION;     // version we expect the DLL to be
      Caller   := Owner;           // point to our VCL instance; returned in callback
      Quiet   := True;
      ZCallbackFunc := ZCallback;  // pass function to be called from DLL
      Level   := 9;                //Compression level (0 - 9, 0=none and 9=best)
      Seven := 7;
      //The following are already initialized to zero (false)...
      //TraceEnabled := False;
      //VerboseEnabled := False;
      //JunkSFX := False;          // if True, convert input .EXE file to .ZIP
      //ComprSpecial := False;     // if True, try to compr already compressed files
      //Volume  := False;          // if True, include volume label from root dir
      //Extra   := False;          // if True, include extended file attributes-NOT SUPORTED
      //UseDate    := False;       // if True, exclude files earlier than specified date
      //CRLF_LF := False;          // if True, translate text file CRLF to LF (if dest Unix)
      //Handle := 0;
      //LatestTime := False;       // if True, make zipfile's timestamp same as newest file
      //Move := False;             // if True, beware! }

      if not IsAdd then //DELETE...
        DeleteEntries := True
      else
      begin //ADD...
        Grow    := True;              //Allow appending to zip file

        //Still needed in spite of changes to dll's...
        if (Owner.fAddPath <> '') and
          DirectoryExists(Owner.fAddPath) then ChDir(Owner.fAddPath);

        //Update = add files as well as freshen
        //Freshen = replace existing zipped files if newer
        if (aoUpdate in Owner.fAddOptions) then
          Update := true else
          Freshen := aoFreshen in Owner.fAddOptions;
        //? include system and hidden files...
        SystemFiles := aoHiddenFiles in Owner.fAddOptions;
        //force 8.3 filenames...
        ForceDOS := aoForceDOS in Owner.fAddOptions;
        Encrypt := (Owner.fPassword <> '');
        //Use the following together (ie: need directories if recursive) -
        Recurse := aoRecursive in Owner.fAddOptions;
        if Recurse then
          JunkDir := false //include (relative) folder paths
        else if not (aoWithFullPath in Owner.fAddOptions) then
          JunkDir := true;
        //? make separate folder entries too ...
        NoDirEntries := not (aoFolderEntries in Owner.fAddOptions);
      end;
   end;
end;
//---------------------------------------------------------------------

function TZip.Add: integer;
var
  tmpFile: string;
begin
  //save a backup of the archive incase cancelled -> corrupts archive
  tmpFile := GetTempFilename;
  copyfile(pchar(filename),pchar(tmpFile),false);
  try
    result := DoAddDelete(true);
    //if cancelled restore the backed-up archive...
    if fCancel then copyfile(pchar(tmpFile),pchar(filename),false);
  finally
    fAddPath := '';
    deleteFile(tmpFile);
  end;
end;
//---------------------------------------------------------------------

function TZip.Delete: integer;
begin
  result := DoAddDelete(false);
end;
//---------------------------------------------------------------------

function TZip.DoAddDelete(IsAdd: boolean): integer;
var
  i: integer;
  ZipParams: pZipParams;
  pFD: pZipFileData;
  MsgTxt, OemComment: string;
  RootDir: string;
begin

  result := 0;
  if fFilename = '' then
    MsgTxt := s_no_zip_no_op
  else if fFileSpecList.count = 0 then
    MsgTxt := s_no_select_no_op
  else if fEocInfo.eoch.ThisDiskNo > 0 then
    MsgTxt := s_multidisk_op_error;
  if MsgTxt <> '' then
  begin
    application.messagebox(pchar(MsgTxt),pchar(application.title),mb_iconinformation);
    exit;
  end;

  if not LoadZipDll then
    raise ZipException.createfmt(s_unable_to_load,['"ZipDll.dll".']);

  if IsAdd and (fAddPath <> '') then
    RootDir := AppendSlash(fAddPath) else
    RootDir := '';

  fCancel := false;
  ZipParams := AllocMem(sizeof(TZipParams)); //zero initialized
  try
    SetZipParams(ZipParams,self,IsAdd);
    //ZipParams.VerboseEnabled := true;
    ZipParams.TraceEnabled := fSaveTraceLog;
    ZipParams.ZipFilename := pansichar(ansistring(fFilename));
    ZipParams.TempPath := pansichar(ansistring(fTempPath));
    OemComment := AnsiStrToOemStr(fEocInfo.ZipComment);
    if (OemComment <> '') then
      ZipParams.FileComment := pansichar(ansistring(OemComment));
    if IsAdd and (fPassword <> '') then
      ZipParams.ZipPassword := pansichar(ansistring(fPassword));
    ZipParams.FileDataCount := fFileSpecList.count;
    ZipParams.FileData := AllocMem(sizeof(TZipFileData)*fFileSpecList.count);
    try
       pFD := ZipParams.FileData;
       for i := 0 to fFileSpecList.count-1 do
       begin
         if IsAdd and (aoWithFullPath in fAddOptions) then
           fFileSpecList[i] := PrependPath(fAddPath,fFileSpecList[i]);
         {$ifdef VER210}
         pFD.FileSpec := pansichar(ansistring(fFileSpecList[i]));
         {$else}
         pFD.FileSpec := pchar(fFileSpecList[i]);
         {$endif}
         pFD.Recurse := Word(ZipParams.Recurse);
         pFD.Encrypt := ZipParams.Encrypt;
         //don't know why this is necessary (??dll bug)
         pFD.Password := ZipParams.ZipPassword;
         {$ifdef VER210}
         pFD.AddRoot := pansichar(ansistring(RootDir));
         {$else}
         pFD.AddRoot := pchar(RootDir);
         {$endif}
         inc(pFD);
       end;

       if fSaveTraceLog then
         traceLogStream :=
           TFileStream.create(changeFileExt(paramstr(0),'.trace'),fmCreate);
       try
         //if assigned(traceLogStream) then traceLogStream.Seek(0,soFromEnd);
         //DO IT HERE...
         if not fShowProgress or not IsAdd then
           result := ZipDLLExec(ZipParams)
         else
         begin
           fProgressForm := TProgressForm.createnew(self);
           try
             fProgressForm.show;
             result := ZipDLLExec(ZipParams); //ADD!!!!!!!!!!!!!!!
           finally
             fProgressForm.free;
             fProgressForm := nil;
           end;
         end;
      finally
        traceLogStream.Free;
        traceLogStream := nil;
      end;

    finally
      FreeMem(ZipParams.FileData);
    end;
  finally
    FreeMem(ZipParams);
  end;
  fFileSpecList.clear;
  //update filelist if any changes...
  if result > 0 then
  begin
    LoadFileList;
    if assigned(fOnContentsChanged) then fOnContentsChanged(self);
  end;
end;
//---------------------------------------------------------------------

function TZip.OemStrToAnsiStr(const s: string): ansistring;
begin
  result := s;
  if (s = '') or not fRequiresOemAnsiConversion then exit;
  {$ifdef DELPHI2009UP}
  OemToChar(pansichar(s), pwidechar(result));
  {$else}
  OemToChar(pansichar(s), pansichar(result));
  {$endif}
end;
//---------------------------------------------------------------------

function TZip.AnsiStrToOemStr(const s: ansistring): string;
begin
  result := s;
  if (s = '') or not fRequiresOemAnsiConversion then exit;
  {$ifdef DELPHI2009UP}
  CharToOem(pwidechar(s), pansichar(result));
  {$else}
  CharToOem(pansichar(s), pansichar(result));
  {$endif}
end;

//---------------------------------------------------------------------

function TZip.GetFileComment(index: integer): string;
begin
  if (index < 0) or (index >= fFileList.count) then
    raise ZipException.create(s_index_range_error);
  result := pFileInfo(fFileList[index]).FileComment;
end;
//---------------------------------------------------------------------

procedure TZip.SetFileComment(index: integer; comment: string);
var
  Stream: TFileStream;
  FileInfo: TFileInfo;
  i, CommOffset, NewCommLen, FHOffset, CommDelta: integer;
  MemStream: TMemoryStream;
  Succeeded: boolean;
begin
  //check there are files in the archive, and it's not a multiple disk archive.
  if (fFilename = '') or (fEocInfo.eoch.ThisDiskNo > 0) or
    (index < 0) or (index >= fFileList.Count) then exit; //error

  Succeeded := false;
  Stream := TFileStream.create(fFilename,fmOpenReadWrite or fmShareDenyWrite);
  try
    Stream.Seek(fEocInfo.EocOffset - fEocInfo.eoch.CentralSize, soFromBeginning);
    i := 0;
    while Stream.Position < fEocInfo.EocOffset do
    begin
      if (Stream.Read(FileInfo, sizeof(TCentralFileHeader)) <>
        sizeof(TCentralFileHeader)) or
        (FileInfo.cfh.HeaderSig <> CENTRAL_HEADERSIG) then exit; //error

      if i = index then
      begin
        pFileInfo(fFileList[index]).FileComment := Comment;
        Comment := AnsiStrToOemStr(Comment);
        NewCommLen := length(comment);
        //if no change in comment length then it's easy...
        if FileInfo.cfh.FileCommentLen = NewCommLen then
        begin
          if FileInfo.cfh.FileCommentLen = 0 then exit; //ie: still empty
          with FileInfo.cfh do Stream.Seek(FileNameLength+ExtraFieldLength,soFromCurrent);
          Stream.Write(comment[1],NewCommLen);
        end else
        begin
          //space has to be inserted/deleted to allow for the comment...
          //so save a couple of file offsets...
          FHOffset := Stream.Position - sizeof(TCentralFileHeader);
          CommOffset := Stream.Position +
            FileInfo.cfh.FileNameLength + FileInfo.cfh.ExtraFieldLength;
          //how many bytes will be added to/deleted from the file...
          CommDelta := NewCommLen - FileInfo.cfh.FileCommentLen;
          //update the EndOfCentral Header to allow for the comment...
          fEocInfo.eoch.CentralSize := fEocInfo.eoch.CentralSize + CommDelta;
          Stream.Seek(fEocInfo.EocOffset,soFromBeginning);
          Stream.Write(fEocInfo,SizeOf(TEndOfCentralHeader));
          //update to the new fEocInfo.EocOffset...
          fEocInfo.EocOffset := fEocInfo.EocOffset + CommDelta;
          //save everything after the old comment to a temp stream...
          Stream.Seek(CommOffset+FileInfo.cfh.FileCommentLen,soFromBeginning);
          MemStream := TMemoryStream.create;
          try
            MemStream.copyfrom(Stream,Stream.Size-Stream.Position);
            //now update the central file header comment length...
            Stream.Seek(FHOffset,soFromBeginning);
            FileInfo.cfh.FileCommentLen := NewCommLen;
            Stream.write(FileInfo.cfh,sizeof(TCentralFileHeader));
            Stream.Seek(CommOffset,soFromBeginning);
            Stream.write(Comment[1],NewCommLen);
            //copy the remaining stuff back to the stream...
            Stream.copyfrom(MemStream,0);
            //increase/truncate the file setting eof marker...
            Stream.Size := Stream.Position;
          finally
            MemStream.free;
          end;
        end;
        Succeeded := true;
        break;
      end;
      inc(i);
      with FileInfo.cfh do
        Stream.Seek(FileNameLength+ExtraFieldLength+FileCommentLen,soFromCurrent);
      continue;
    end;
  finally
    Stream.free;
  end;
  if Succeeded and assigned(fOnContentsChanged) then
    fOnContentsChanged(self);
end;
//---------------------------------------------------------------------

procedure TZip.SetZipComment(comment: string);
var
  Stream: TFileStream;
  ComLen: integer;
begin
  if (fFilename = '') or (fEocInfo.eoch.ThisDiskNo > 0) then exit; //error
  if comment = fEocInfo.ZipComment then exit; //nothing to change
  Stream := TFileStream.create(fFilename,fmOpenReadWrite or fmShareDenyWrite);
  try
    //goto the start of the EndOfCentral Header...
    Stream.Seek(fEocInfo.EocOffset,soFromBeginning);
    fEocInfo.ZipComment := comment;
    comment := AnsiStrToOemStr(comment);
    ComLen :=  Length(comment);
    fEocInfo.eoch.ZipCommentLen := ComLen;
    if Stream.Write(fEocInfo,SizeOf(TEndOfCentralHeader)) <>
      SizeOf(TEndOfCentralHeader) then exit; //error
    Stream.write(comment[1],ComLen);
    Stream.Size := Stream.Position; //set end of file marker.
  finally
    Stream.free;
  end;
  if assigned(fOnContentsChanged) then fOnContentsChanged(self);
end;
//---------------------------------------------------------------------

function TZip.GetZipComment: string;
begin
  result := fEocInfo.ZipComment;
end;
//---------------------------------------------------------------------

function TZip.IsMultiDiskArchive: boolean;
begin
  result := (fFilename <> '') and (fEocInfo.eoch.ThisDiskNo > 0);
end;
//---------------------------------------------------------------------

const
  BUFFER_SIZE = 1024 * 32; //32k buffer

function TZip.MergeArchiveTo(const Target: string): boolean;
var
  SrcStream, TrgtStream: TFileStream;
  SrcSizesList: TList;
  i, ReadToBuffer, CurrentDiskNo, LastDiskNo: integer;
  TotalBytesToMerge: longint;
  NewEoc: TEndOfCentralHeader;
  NewCfhOffset: integer;
  Cfh: TCentralFileHeader;
  MultiSig: Cardinal;
  buffer: pchar;
begin
  result := false;
  if (fFilename = '') or (Target = '') then exit;

  if (fEocInfo.eoch.ThisDiskNo = 0) or (GetDiskType(fFilename) <> DRIVE_REMOVABLE) then
    raise ZipException.create(s_need_multidisk_zip);

  TotalBytesToMerge := 0;
  for i := 0 to fEocInfo.eoch.TotalEntries-1 do
    with pFileInfo(fFileList[i]).cfh do
      inc(TotalBytesToMerge, CompressedSize + FileNameLength +
        ExtraFieldLength + cfh.FileCommentLen);
  inc(TotalBytesToMerge,fEocInfo.eoch.CentralSize +
    fEocInfo.eoch.ZipCommentLen + SizeOf(TEndOfCentralHeader));

  //1. make sure that the target file will be on a local hard disk...
  if GetDiskType(Target) <> DRIVE_FIXED then
    raise ZipException.create(s_merge_only_to_fixed_disk);

  //2. find the first source disk ...
  if application.messagebox(pchar(format(s_multidisk_prompt,[1])),
    pchar(application.title), mb_okcancel or mb_iconinformation) <> IDOK then
      exit; //false
  if not PromptMultiDiskNum(fFilename,1) then exit;

  //3. loop through each source file, concatenating it into the target file
  LastDiskNo := fEocInfo.eoch.ThisDiskNo+1; //fEocInfo.ThisDiskNo is zero based
  CurrentDiskNo := 1; //CurrentDiskNo & LastDiskNo 1 based

  //Each CentralFileHeader.RelOffLocalHdr must be adjusted
  //so store a list of RelOffLocalHdr deltas...
  SrcSizesList := TList.create;
  //and to reduce the number of nested try-finally blocks...
  SrcStream := nil;
  TrgtStream := nil;
  buffer := nil;
  try
    SrcStream := TFileStream.create(fFilename,fmOpenRead or fmShareDenyWrite);
    TrgtStream := TFileStream.create(Target,fmCreate);
    buffer := allocmem(BUFFER_SIZE);
    //skip multidisk flag on first disk 1..
    SrcStream.read(MultiSig,sizeof(MultiSig));
    if MultiSig <> MULTIPLE_DISK_SIG then
      raise ZipException.create(s_multidisk_sig_error);//error
    SrcSizesList.add(pointer(-4)); //we've just stripped a 4 byte MultiSig header

    repeat //TOP OF LOOP
      //read thru whole file in BUFFER_SIZE blocks copying to target file
      screen.cursor := crHourglass;
      try
        repeat
          ReadToBuffer := SrcStream.read(buffer[0],BUFFER_SIZE);
          TrgtStream.write(buffer[0],ReadToBuffer);
          if assigned(fMergeProgressEvent) then
            fMergeProgressEvent(self,target,TrgtStream.position,TotalBytesToMerge);
          application.processmessages;
        until (ReadToBuffer <> BUFFER_SIZE); //end of file reached
      finally
        screen.cursor := crDefault;
      end;
      SrcSizesList.add(pointer(TrgtStream.Position));
      if CurrentDiskNo = LastDiskNo then break; //EXIT LOOP HERE
      //get next disk here...
      inc(CurrentDiskNo);
      SrcStream.free;
      SrcStream := nil; //just incase the TFileStream.create line fails...
      if not PromptMultiDiskNum(fFilename,CurrentDiskNo) then exit; //cancelled
      SrcStream := TFileStream.create(fFilename,fmOpenRead or fmShareDenyWrite);
    until false; //BOTTOM OF LOOP

    //4. Update the target file EndOfCentral and Central Directories...

    //first get a copy the old Eoch
    NewEoc := fEocInfo.eoch;

    //find the offset of the newly created target's CentralDirectoryHeader...
    NewCfhOffset := TrgtStream.position - NewEoc.ZipCommentLen -
      sizeof(TEndOfCentralHeader) - NewEoc.CentralSize;

    if NewCfhOffset < 0 then
      raise ZipException.create(s_eoch_read_error);

    TrgtStream.seek(NewCfhOffset,soFromBeginning);
    for i := 0 to NewEoc.TotalEntries-1 do
    begin
      TrgtStream.Read(Cfh,sizeof(TCentralFileHeader));
      if Cfh.HeaderSig <> CENTRAL_HEADERSIG then
        raise ZipException.create(s_cfh_read_error);
      TrgtStream.seek(-sizeof(Cfh),soFromCurrent);
      //adjust LocalHeader offsets by values stored in SrcSizesList
      inc(Cfh.RelOffLocalHdr,integer(SrcSizesList[Cfh.StartOnDisk]));
      Cfh.StartOnDisk := 0;
      TrgtStream.Write(Cfh,sizeof(TCentralFileHeader));
      TrgtStream.Seek(Cfh.FileNameLength+
        Cfh.ExtraFieldLength+Cfh.FileCommentLen, soFromCurrent);
    end;

    NewEoc.ThisDiskNo := 0;
    NewEoc.CentralDiskNo := 0;
    NewEoc.CentralOffset := NewCfhOffset;

    //update Eoc header...
    TrgtStream.write(NewEoc,sizeof(NewEoc));

  finally
    SrcSizesList.Free;
    SrcStream.free;
    TrgtStream.free;
    if assigned(buffer) then freemem(buffer);
  end;
end;
//---------------------------------------------------------------------

//This is a *very* long method but I'm not sure of the best way to break it up.
function TZip.SpanArchiveTo(const Target: string;
  MaxBytesPerDisk, BytesToReserveDisk1: integer): boolean;
var
  i, DiskNum, FreeSpace, DataRead, BytesRead, SfxDelta: integer;
  TargetDrivePath, VolText: string;
  SrcStream,TrgtStream: TFileStream;
  MultiDiskSig: cardinal;
  buffer: pchar;
  Loc : TLocalHeader;
  Cfh: TCentralFileHeader;
  Eoc: TEndOfCentralHeader;
  SavedCursor: TCursor;

  //-------------------------------------------------------------------

  function GetNextDisk(out SpaceAvailable: integer): boolean;
  begin
    result := false;
    inc(DiskNum);
    //Load a disk into drive and make sure it has space ...
    if application.messagebox(pchar(format(s_multidisk_prompt, [DiskNum])),
      pchar(application.title), mb_okcancel or mb_iconinformation) <> IDOK then
        exit; //cancelled
    SpaceAvailable := GetDiskFree(TargetDrivePath);
    while (SpaceAvailable < 1024*256) do //bare minimum = 256k
    begin
      if application.messagebox(pchar(s_no_space_prompt), pchar(application.title),
        mb_okcancel or mb_iconinformation) <> IDOK then exit; //cancelled
      SpaceAvailable := GetDiskFree(TargetDrivePath);
    end;
    //set the disk's volume label ...
    VolText := format('PKBACK# %0.3d',[DiskNum]);
    if not SetVolumeLabel(pchar(TargetDrivePath),pchar(VolText)) then
      raise ZipException.create(s_no_create_vol_label);
    result := true;
  end;
  //-------------------------------------------------------------------

  function GetNextDiskWithNewTgtStream: boolean;
  begin
    TrgtStream.free;
    TrgtStream := nil;
    screen.cursor := SavedCursor;
    result := GetNextDisk(FreeSpace);
    if not result then exit;
    screen.cursor := crHourglass;
    TrgtStream := TFileStream.create(Target,fmCreate);
    if MaxBytesPerDisk > 0 then
      FreeSpace := min(FreeSpace,MaxBytesPerDisk);
  end;
  //-------------------------------------------------------------------

begin
  result := false;
  if (fFilename = '') or (Target = '') then exit;
  TargetDrivePath := AppendSlash(ExtractFileDrive(target));
  //1. check that the destination is a floppy...
  if (GetDiskType(TargetDrivePath) <> DRIVE_REMOVABLE) then
    raise ZipException.create(s_create_multdisk_on_floppies_only);

  //2. make sure the source is not a multiple disk archive too...
  if (fEocInfo.eoch.ThisDiskNo > 0) or (GetDiskType(fFilename) = DRIVE_REMOVABLE) then
    raise ZipException.create(s_source_file_cannot_be_on_floppy);

  //3. check space issues...
  if (MaxBytesPerDisk > 0) then
  begin
    if MaxBytesPerDisk < 1024*256 then
      raise ZipException.create(s_max_multidisk_at_least_256);
    if MaxBytesPerDisk - BytesToReserveDisk1 < 1024*256 then
      raise ZipException.create(s_min_space_disk1_at_least_256);
  end;

  //4. Load the first disk into floppy, get free space & set volume label ...
  DiskNum := 0;
  if not GetNextDisk(FreeSpace) then exit; //also sets volume label here
  if MaxBytesPerDisk > 0 then
    FreeSpace := min(FreeSpace,MaxBytesPerDisk);
  dec(FreeSpace,BytesToReserveDisk1);

  SavedCursor := screen.cursor;
  buffer := nil;
  TrgtStream := nil;
  SrcStream := TFileStream.create(fFilename,fmOpenRead or fmShareDenyWrite);
  try
    TrgtStream := TFileStream.create(Target,fmCreate);

    //5. if the whole archive will fit on one disk, copy and exit ...
    if FreeSpace > SrcStream.Size then
    begin
      if not SetVolumeLabel(pchar(TargetDrivePath),'') then
        raise ZipException.create(s_no_create_vol_label);
      TrgtStream.CopyFrom(SrcStream,0);
      result := true;
      exit;
    end;

    buffer := allocmem(BUFFER_SIZE);

    //6. Write MULTIPLE_DISK_SIG.
    MultiDiskSig := MULTIPLE_DISK_SIG;
    TrgtStream.Write(MultiDiskSig,sizeof(MultiDiskSig));
    dec(FreeSpace,4);

    screen.cursor := crHourglass;

    //if the zip file (fFilename) has an SFX stub, then
    //cfh.RelOffLocalHdr values may or may not be accurate...
    SfxDelta := fEocInfo.EocOffset -
      (fEocInfo.eoch.CentralOffset + fEocInfo.eoch.CentralSize);

    //7. Copy all [LocalHeader, Data & DataDescriptor] blocks ...
    for i := 0 to fEocInfo.eoch.TotalEntries-1 do
    begin
      //Copy the local header...
      SrcStream.seek(pFileInfo(fFileList[i]).cfh.RelOffLocalHdr + SfxDelta, soFromBeginning);
      if SrcStream.read(Loc,SizeOf(TLocalHeader)) <> SizeOf(TLocalHeader) then
        raise ZipException.create(s_localheader_read_error);
      if Loc.HeaderSig <> LOCAL_HEADERSIG then
        raise ZipException.create(s_localheader_read_error);
      if FreeSpace < SizeOf(TLocalHeader)+Loc.FileNameLen+Loc.ExtraLen then
        if not GetNextDiskWithNewTgtStream then exit; //ie: cancelled

      //save the diskNum & offset to update the target CentralDirectory later...
      pFileInfo(fFileList[i]).Reserved1 := DiskNum-1; //zero based
      pFileInfo(fFileList[i]).Reserved2 := TrgtStream.Position;
      //if the Data Descriptor field is present in the source, it's not needed
      //in the target file if the target's LocalHeader info is updated ...
      if (Loc.Flag and $8) = $8 then
      begin
        Loc.Flag := Loc.Flag and not $8;
        Loc.CRC32 := pFileInfo(fFileList[i]).cfh.CRC32;
        Loc.ComprSize := pFileInfo(fFileList[i]).cfh.CompressedSize;
        Loc.UnComprSize := pFileInfo(fFileList[i]).cfh.UncompressedSize;
      end;

      if TrgtStream.Write(Loc,Sizeof(Loc))<> Sizeof(Loc) then
        raise ZipException.create(s_localheader_write_error);
      dec(FreeSpace,SizeOf(Loc));

      //Copy Filename & Extra field...
      BytesRead := Loc.FileNameLen+Loc.ExtraLen;
      BytesRead := SrcStream.read(buffer[0],BytesRead);
      BytesRead := TrgtStream.Write(buffer[0],BytesRead);
      dec(FreeSpace,BytesRead);
      if BytesRead <> Loc.FileNameLen+Loc.ExtraLen then
        raise ZipException.create(s_localheader_write_error);

      //Copy file data...
      DataRead := 0;
      while (DataRead < Loc.ComprSize) do
      begin
        if FreeSpace = 0 then
          if not GetNextDiskWithNewTgtStream then exit;
        BytesRead := min(Loc.ComprSize-DataRead,BUFFER_SIZE);
        BytesRead := min(BytesRead,FreeSpace);
        BytesRead := SrcStream.read(buffer[0],BytesRead);
        BytesRead := TrgtStream.Write(buffer[0],BytesRead);
        inc(DataRead,BytesRead);
        dec(FreeSpace,BytesRead);
        if BytesRead = 0 then
          raise ZipException.create(s_local_data_write_error);
        if assigned(fSpanProgressEvent) then
          fSpanProgressEvent(self,Target,SrcStream.Position,SrcStream.Size);
        application.processmessages;
      end;
    end;

    //8. Copy Central Directory...
    Move(fEocInfo.eoch ,Eoc, sizeof(TEndOfCentralHeader));
    Eoc.ThisDiskEntries := 0;
    SrcStream.seek(fEocInfo.eoch.CentralOffset + SfxDelta, soFromBeginning);
    for i := 0 to fEocInfo.eoch.TotalEntries-1 do
    begin
      if SrcStream.read(Cfh,SizeOf(TCentralFileHeader)) <> SizeOf(TCentralFileHeader) then
        raise ZipException.create(s_cfh_read_error);
      if Cfh.HeaderSig <> CENTRAL_HEADERSIG then
        raise ZipException.create(s_cfh_read_error);
      if FreeSpace < (SizeOf(TCentralFileHeader) + Cfh.FileNameLength +
        Cfh.ExtraFieldLength + Cfh.FileCommentLen + SizeOf(TEndOfCentralHeader)) then
      begin
        if not GetNextDiskWithNewTgtStream then exit;
        Eoc.ThisDiskEntries := 0; //reset this count
      end;

      inc(Eoc.ThisDiskEntries);
      if i = 0 then
      begin
        Eoc.CentralDiskNo := DiskNum-1; //zero based
        Eoc.CentralOffset := TrgtStream.Position;
      end;
      Cfh.StartOnDisk := pFileInfo(fFileList[i]).Reserved1;
      Cfh.RelOffLocalHdr := pFileInfo(fFileList[i]).Reserved2;

      if TrgtStream.Write(Cfh,Sizeof(Cfh))<> Sizeof(Cfh) then
        raise ZipException.create(s_cfh_write_error);
      dec(FreeSpace,SizeOf(Cfh));
      BytesRead := Cfh.FileNameLength + Cfh.ExtraFieldLength + Cfh.FileCommentLen;
      if BytesRead > BUFFER_SIZE then
        raise ZipException.create(s_cfh_write_error);
      BytesRead := SrcStream.read(buffer[0],BytesRead);
      BytesRead := TrgtStream.Write(buffer[0],BytesRead);
      if BytesRead < Cfh.FileNameLength + Cfh.ExtraFieldLength + Cfh.FileCommentLen then
        raise ZipException.create(s_cfh_write_error);
      dec(FreeSpace,BytesRead);
    end;

    //9. Copy EndOf Central Header (& main comment)...
    Eoc.ThisDiskNo := DiskNum-1; //zero based.
    if TrgtStream.Write(Eoc,SizeOf(TEndOfCentralHeader)) <> SizeOf(TEndOfCentralHeader) then
      raise ZipException.create(s_eoch_write_error);
    if Eoc.ZipCommentLen > 0 then
    begin
      SrcStream.seek(SizeOf(TEndOfCentralHeader),soFromCurrent);
      if SrcStream.read(buffer[0],Eoc.ZipCommentLen) <> Eoc.ZipCommentLen then
        raise ZipException.create(s_comment_read_error);
      if TrgtStream.Write(buffer[0],Eoc.ZipCommentLen) <> Eoc.ZipCommentLen then
        raise ZipException.create(s_comment_write_error);
    end;
    result := true; //ALL DONE!!
  finally
    screen.cursor := SavedCursor;
    SrcStream.free;
    TrgtStream.free;
    if assigned(buffer) then freemem(buffer);
  end;
end;
//---------------------------------------------------------------------

function StripSfxStub(const SfxExeFile, NewZipFile: string): boolean;
var
  i, BytesRead, TrueSfxSize, EocOffset: integer;
  SrcStream,TrgtStream: TFileStream;
  Eoc: TEndOfCentralHeader;
  cfh: TCentralFileHeader;
  SavedCursor: TCursor;
begin
  result := false;
  if (not FileExists(SfxExeFile)) or (NewZipFile = '') then exit;

  SrcStream := nil;
  TrgtStream := nil;
  SavedCursor := screen.cursor;
  screen.cursor := crHourglass;
  try
    SrcStream := TFileStream.create(SfxExeFile,fmOpenRead or fmShareDenyWrite);
    TrgtStream := TFileStream.create(NewZipFile, fmCreate); //nb - no check for overwrite

    EocOffset := FindEOCHeaderOffset(SrcStream);
    if (EocOffset = ERROR_VALUE) then
      raise ZipException.Createfmt(s_not_an_sfx_file, [SfxExeFile]);
    if SrcStream.Read(Eoc, sizeof(TEndOfCentralHeader)) <> sizeof(TEndOfCentralHeader) then
      raise ZipException.Create(s_sfx_read_error);

    if (Eoc.ThisDiskNo > 0) then exit; //Multi disk archives cannot be SfxExes!

    //get the relative offset of the first file in the archive ...
    SrcStream.seek(EocOffset - Eoc.CentralSize, soFromBeginning);
    SrcStream.read(cfh,sizeof(TCentralFileHeader));
    if cfh.HeaderSig <> CENTRAL_HEADERSIG then
      raise ZipException.Create(s_sfx_read_error);

    {TODO - nb: the central directory order does not have to reflect local order}  
    TrueSfxSize := cfh.RelOffLocalHdr +
      (EocOffset - (Eoc.CentralOffset + Eoc.CentralSize));
    if TrueSfxSize < 1 then exit; //not an Exe.

    //skip over the Sfx stub...
    SrcStream.Seek(TrueSfxSize,soFromBeginning);
    //copy the rest of the file...
    BytesRead := TrgtStream.copyfrom(SrcStream, SrcStream.size-TrueSfxSize);
    if BytesRead <> SrcStream.size-TrueSfxSize then exit; //error

    //now fix up the central directory offsets if necessary...
    if cfh.RelOffLocalHdr <> 0 then
    begin
      Eoc.CentralOffset := Eoc.CentralOffset - TrueSfxSize;
      TrgtStream.Seek(EocOffset-TrueSfxSize, soFromBeginning);
      TrgtStream.Write(Eoc,sizeof(TEndOfCentralHeader));
      TrgtStream.Seek(Eoc.CentralOffset, soFromBeginning);
      for i := 0 to Eoc.TotalEntries-1 do
      begin
        TrgtStream.read(cfh,sizeof(TCentralFileHeader));
        if cfh.HeaderSig <> CENTRAL_HEADERSIG then exit; //error
        cfh.RelOffLocalHdr := cfh.RelOffLocalHdr - TrueSfxSize;
        TrgtStream.Seek(-sizeof(TCentralFileHeader), soFromCurrent);
        TrgtStream.write(cfh,sizeof(TCentralFileHeader));
        //go to next central file header...
        TrgtStream.Seek(cfh.FileNameLength+
          cfh.ExtraFieldLength+cfh.FileCommentLen, soFromCurrent);
      end;
    end;
    result := true; //all ok if we get this far.
  finally
    SrcStream.free;
    TrgtStream.free;
    screen.cursor := SavedCursor;
  end;
end;
//--------------------------------------------------------------------------
//---------------------------------------------------------------------

end.
