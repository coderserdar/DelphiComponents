unit magfmtdisk;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

// Magenta Check Disk and Format Disk component
// 26th November 2018 - Release 1.3 (C) Magenta Systems Ltd, 2018
// based on Chkdskx and Formatx by Mark Russinovich at http://www.sysinternals.com

// Copyright by Angus Robertson, Magenta Systems Ltd, England
// delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/

// 20th Oct 2005 1.0 - baseline
// 30th Jul 2008 1.1 - tested with Unicode and Delphi 2009, and Vista
//                     a few more callback messages
// 20th Aug 2008 1.2 - corrected progress message charset which was OEM (IBM-PC) not
//                         ANSI or unicode, thanks to Francois Piette
// 8th Nov 2011  1.3 - added EXFAT file system


interface

uses
  Windows, Messages, SysUtils, Classes;

const
    fmifs = 'fmifs.dll' ;
    WM_GETOBJ = WM_USER + 701 ;

// media flags
    FMIFS_HARDDISK = $0C ;
    FMIFS_FLOPPY   = $08 ;

// Output command
type
    TextOutput = record
	    Lines:  DWORD ;
	    Output: PAnsiChar ;  // unicode
    end ;
   PTextOutput = ^TextOutput ;

// Callback command types
    TCallBackCommand = (
        PROGRESS,
        DONEWITHSTRUCTURE,
    	UNKNOWN2,
	    UNKNOWN3,
    	UNKNOWN4,
	    UNKNOWN5,
    	INSUFFICIENTRIGHTS,
	    FSNOTSUPPORTED,  // added 1.1
    	VOLUMEINUSE,     // added 1.1
    	UNKNOWN9,
	    UNKNOWNA,
    	DONE,
	    UNKNOWNC,
    	UNKNOWND,
    	OUTPUT,
        STRUCTUREPROGRESS,
        CLUSTERSIZETOOSMALL, // 16
        UNKNOWN11,
        UNKNOWN12,
        UNKNOWN13,
        UNKNOWN14,
        UNKNOWN15,
        UNKNOWN16,
        UNKNOWN17,
        UNKNOWN18,
        PROGRESS2,      // added 1.1, Vista percent done seems to duplicate PROGRESS
        UNKNOWN1A) ;
var

// Chkdsk command in FMIFS

Chkdsk: procedure (
    DriveRoot: PWCHAR;
    Format: PWChar ;
    CorrectErrors: BOOL;
	Verbose: BOOL;
	CheckOnlyIfDirty: BOOL;
	ScanDrive: BOOL;
	Unused2: DWORD;
	Unused3: DWORD;
	Callback: Pointer); stdcall;

// Format command in FMIFS

FormatEx: procedure (
    DriveRoot: PWCHAR;
	MediaFlag: DWORD;
	Format: PWCHAR;
	DiskLabel: PWCHAR;
	QuickFormat: BOOL;
	ClusterSize: DWORD;
	Callback: Pointer); stdcall;

// Enable/Disable volume compression command in FMIFS

EnableVolumeCompession: function (
    DriveRoot: PWCHAR;
	Enable: BOOL): BOOLEAN; stdcall;

type

  TMediaType = (mtHardDisk, mtFloppy) ;
  TFileSystem = (fsNTFS, fsFAT, fsFAT32, fsEXFAT) ;
  TProgressEvent = Procedure (Percent: integer; var Cancel: boolean) of object ;
  TInfoEvent = Procedure (Info: string; var Cancel: boolean) of object ;

  TMagFmtChkDsk = class(TComponent)
  private
    { Private declarations }
    fProgressEvent: TProgressEvent ;
    fInfoEvent: TInfoEvent ;
    fDoneOK: boolean ;
    fFileSysProblem: boolean ;
    fFreeSpaceAlloc: boolean ;
    fFirstErrorLine: string ;
  protected
    { Protected declarations }
    function CheckDriveExists (const WDrive: WideString;
                               CheckInUse: boolean ; var WFormat: WideString): boolean ;
    function doProgressEvent (const Percent: integer): boolean ;
    function doInfoEvent (const Info: string): boolean ;
    procedure WMGETOBJ (var msg: TMessage); message WM_GETOBJ;
  public
    { Public declarations }
    function LoadFmifs: boolean ;
    function FormatDisk (const DrvRoot: string; MediaType: TMediaType; FileSystem: TFileSystem;
             const DiskLabel: string; QuickFormat: boolean; ClusterSize: integer): boolean ;
    function CheckDisk (const DrvRoot: string; CorrectErrors, Verbose,
                                        CheckOnlyIfDirty, ScanDrive: boolean): boolean ;
    function VolumeCompression (const DrvRoot: string; Enable: boolean): boolean ;
  published
    { Published declarations }
    property FileSysProblem: boolean          read fFileSysProblem ;
    property FreeSpaceAlloc: boolean          read fFreeSpaceAlloc ;
    property FirstErrorLine: string           read fFirstErrorLine ;
    property onProgressEvent: TProgressEvent  read fProgressEvent write fProgressEvent ;
    property onInfoEvent:     TInfoEvent      read fInfoEvent     write fInfoEvent ;

  end;

  FmtChkException = class(Exception);

var
  MagFmifsib: THandle = 0 ;
  MagFmifs_Loaded: Boolean = false ;   // See if DLL functions are loaded
  MagFmtObj: TObject ;


implementation

procedure Register;
begin
  RegisterComponents('Samples', [TMagFmtChkDsk]);
end;

// FMIFS callback definition

function FormatCallback (Command: TCallBackCommand; SubAction: DWORD;
                                             ActionInfo: Pointer): Boolean; stdcall;
var
    flag: pboolean ;
    percent: pinteger ;
    toutput: PTextOutput ;
    Obj: TObject ;
    cancelflag: boolean ;
    info: string ;
    xlatbuf : AnsiString;
    progper, slen: integer ;
begin
    result := true ;
    cancelflag := false ;
//    Obj := TObject (SendMessage (HInstance, WM_GETOBJ, 0, 0)) ;
    Obj := MagFmtObj ;
    progper := -1 ;
    info := '' ;
    if NOT Assigned (TMagFmtChkDsk (Obj)) then exit ;
    case Command of
        Progress:
            begin
                percent := ActionInfo ;
                progper := percent^ ;
            end ;
        Progress2:   // 1.1 added for Vista
            begin
            //    percent := ActionInfo ;
            //    progper := percent^ ;
            end ;
        Output:
            begin
                toutput := ActionInfo ;
                slen := StrLen (toutput^.Output) ;
                SetLength (xlatbuf, slen) ;   // 1.2 change OEM charset to ANSI
                OemToCharBuffA (PAnsiChar (toutput^.Output), PAnsiChar (xlatBuf), slen) ;
                info := Trim (String (xlatBuf)) ;
            end ;
        Done:
            begin
                flag := ActionInfo ;
                TMagFmtChkDsk (Obj).fDoneOK := flag^ ;
                if flag^ then
                    info := 'Format Disk: Finished OK'
                else
                    info := 'Format Disk: Unable to Finish' ;
            end ;
        DoneWithStructure: info := 'Format Disk: Structure Created OK' ;
        InsufficientRights: info := 'Format Disk: Insufficient Rights' ;
        UNKNOWN9: info := 'Format Disk: Quick Format Not Allowed' ;
        ClusterSizeTooSmall: info := 'Format Disk: Cluster Size Too Small' ; // 1.1
	    FSNotSupported: info := 'Format Disk: FS Not Supported' ; // 1.1
    	VolumeInUse: info := 'Format Disk: Volume In-Use' ; // 1.1
        StructureProgress:
            begin
            //    percent := ActionInfo ;  does not seem to be a result
            //    if percent <> Nil then progper := percent^ ;
            end ;
        else
            info := 'Format Disk Callback: ' + IntToStr (Ord (Command)) ;
    end ;
    if progper >= 0  then cancelflag := TMagFmtChkDsk (Obj).doProgressEvent (progper) ;
    if info <> '' then cancelflag := TMagFmtChkDsk (Obj).doInfoEvent (info) ;
    result := NOT cancelflag ;
end ;

function ChkDskCallback (Command: TCallBackCommand; SubAction: DWORD;
                                             ActionInfo: Pointer): Boolean; stdcall;
var
    flag: pboolean ;
    percent: pinteger ;
    toutput: PTextOutput ;
    Obj: TObject ;
    info: string ;
    progper, slen: integer ;
    cancelflag: boolean ;
    xlatbuf : AnsiString;
begin
    result := true ;
    cancelflag := false ;
    progper := -1 ;
    info := '' ;
//    Obj := TObject (SendMessage (HInstance, WM_GETOBJ, 0, 0)) ;
    Obj := MagFmtObj ;
    if NOT Assigned (TMagFmtChkDsk (Obj)) then exit ;
    case Command of
        Progress:
            begin
                percent := ActionInfo ;
                progper := percent^ ;
            end ;
        Progress2:   // 1.1 added for Vista
            begin
            //    percent := ActionInfo ;
            //    progper := percent^ ;
            end ;
        Output:
            begin
                toutput := ActionInfo ;
                slen := StrLen (toutput^.Output) ;
                SetLength (xlatbuf, slen) ;   // 1.2 change OEM charset to ANSI
                OemToCharBuffA (PAnsiChar (toutput^.Output), PAnsiChar (xlatBuf), slen) ;
                info := Trim (String (xlatBuf)) ;
                if (Pos ('found problems', info) > 0) or
                     (Pos ('Correcting errors', info) > 0) or
                       (Pos ('Errors found', info) > 0) or
                         (Pos ('(fix) option', info) > 0) then
                begin
                     TMagFmtChkDsk (Obj).fFileSysProblem := true ;
                     if TMagFmtChkDsk (Obj).fFirstErrorLine = '' then
                                    TMagFmtChkDsk (Obj).fFirstErrorLine := info ;
                end ;
                if (Pos ('free space marked as allocated', info) > 0) then
                begin
                     TMagFmtChkDsk (Obj).fFreeSpaceAlloc := true ;
                     if TMagFmtChkDsk (Obj).fFirstErrorLine = '' then
                                    TMagFmtChkDsk (Obj).fFirstErrorLine := info ;
                end ;
            end ;
        Done:
            begin
                flag := ActionInfo ;
                TMagFmtChkDsk (Obj).fDoneOK := flag^ ;
                if flag^ then
                    info := 'Check Disk: Finished OK'
                else
                    info := 'Check Disk: Unable to Finish' ;
            end ;
	    FSNotSupported: info := 'Check Disk: FS Not Supported' ; // 1.1
    	VolumeInUse: info := 'Check Disk: Volume In-Use' ; // 1.1
        InsufficientRights: info := 'Check Disk: Insufficient Rights' ; // 1.1
        else
            info := 'Check Disk Callback: ' + IntToStr (Ord (Command)) ;
    end ;
    if progper >= 0  then cancelflag := TMagFmtChkDsk (Obj).doProgressEvent (progper) ;
    if info <> '' then cancelflag := TMagFmtChkDsk (Obj).doInfoEvent (info) ;
    result := NOT cancelflag ;
end ;

procedure TMagFmtChkDsk.WMGETOBJ (var msg: TMessage);
begin
    msg.Result := Integer (TMagFmtChkDsk) ;
end ;

function TMagFmtChkDsk.doProgressEvent (const Percent: integer): boolean ;
begin
    result := false ;
    if Assigned (fProgressEvent) then fProgressEvent (Percent, result) ;
end ;

function TMagFmtChkDsk.doInfoEvent (const Info: string): boolean ;
begin
    result := false ;
    if Assigned (fInfoEvent) then fInfoEvent (Info, result) ;
end ;


function TMagFmtChkDsk.CheckDriveExists (const WDrive: WideString;
                           CheckInUse: boolean ; var WFormat: WideString): boolean ;
var
    FileSysName  : Array[0..MAX_PATH] of WChar;
    VolumeName   : Array[0..MAX_PATH] of WChar;
    maxcomlen, flags: longword;
    handle: THandle ;
    voldev: WideString ;
begin
    if (Length (WDrive) < 2) or (WDrive [2] <> ':') then
    begin
        raise FmtChkException.Create('Invalid Drive Specification: ' + WDrive);
        exit ;
    end ;

// see if volume exists, get file system (FAT32, NTFS)
    if NOT GetVolumeInformationW (PWChar (WDrive), VolumeName, SizeOf(VolumeName) div 2,
                 Nil, maxcomlen, flags, FileSysName, SizeOf(FileSysName) div 2) then
    begin
        raise FmtChkException.Create('Drive Not Found: ' + WDrive);
        exit ;
    end ;
    WFormat := FileSysName ;
    doInfoEvent (WDrive + ' Volume Label: ' + VolumeName + ', File System: ' + FileSysName) ;

// try and get exclusive access to volume
    if CheckInUse then
    begin
        voldev := '\\.\' + WDrive [1] + ':' ;
        handle := CreateFileW (PWChar (voldev), Generic_Write, 0, nil, Open_Existing, 0, 0) ;
        if handle = INVALID_HANDLE_VALUE then
        begin
            raise FmtChkException.Create('Drive In Use: ' + WDrive);
            exit ;
        end ;
        CloseHandle (handle) ;
    end ;
    result := true ;
end ;

function TMagFmtChkDsk.FormatDisk (const DrvRoot: string; MediaType: TMediaType;
                               FileSystem: TFileSystem; const DiskLabel: string;
                                  QuickFormat: boolean; ClusterSize: integer): boolean ;
var
    wdrive, wformat, wfilesystem, wdisklabel: widestring ;
    mediaflags, newsize: DWORD ;

begin
    result := false ;
    if NOT LoadFmifs then exit ;
    wdrive := Uppercase (DrvRoot) ;
//    wdrive := 'T:\' ; // TESTING
    wdisklabel := Uppercase (DiskLabel) ;
    if MediaType = mtHardDisk then
        mediaflags := FMIFS_HARDDISK
    else if MediaType = mtFloppy then
        mediaflags := FMIFS_FLOPPY
    else
    begin
        doInfoEvent ('Unknown Media Type: ' + IntToStr (Ord (MediaType))) ;
        exit ;
    end;
    if FileSystem = fsFAT then
        wfilesystem := 'FAT'
    else if FileSystem = fsFAT32 then
        wfilesystem := 'FAT32'
    else if FileSystem = fsEXFAT then  // added 1.3
        wfilesystem := 'EXFAT'
    else if FileSystem = fsNTFS then
        wfilesystem := 'NTFS'
    else
    begin
        doInfoEvent ('Unknown File System: ' + IntToStr (Ord (FileSystem))) ;
        exit ;
    end;
    newsize := 0 ;
    if ((ClusterSize = 512) or (ClusterSize = 1024) or (ClusterSize = 2048) or
        (ClusterSize = 4096) or (ClusterSize = 8192) or (ClusterSize = 16384) or
            (ClusterSize = 32768) or (ClusterSize = 65536)) then newsize := ClusterSize ;
    fDoneOK := false ;
    if DiskSize (Ord (WDrive [1]) - 64) > 100 then  // don't check drive unless it exists
    begin
        doInfoEvent (WDrive + ' Checking Existing Drive Format') ;
        if NOT CheckDriveExists (wdrive, true, wformat) then exit ;
        if wformat <> wfilesystem then QuickFormat := false ;
    end
    else
    begin
        if (Length (WDrive) < 2) or (WDrive [2] <> ':') then
        begin
            raise FmtChkException.Create('Invalid Drive Specification: ' + WDrive);
            exit ;
        end ;
        doInfoEvent (WDrive + ' Appears to be Unformatted or No Drive') ;
        QuickFormat := false ;
    end ;
    MagFmtObj := Self ;
    fFirstErrorLine := '' ;
    doInfoEvent (WDrive + ' Starting to Format Drive') ;
    FormatEx (PWchar (wdrive), mediaflags, PWchar (wfilesystem), PWchar (wdisklabel),
                                                 QuickFormat, newsize, @FormatCallback) ;
    result := fDoneOK  ;
    if NOT result then exit ;
    doInfoEvent (WDrive + ' Checking New Drive Format') ;
    if NOT CheckDriveExists (wdrive, false, wformat) then exit ;
    doInfoEvent (WDrive + ' New Volume Space: ' + IntToStr (DiskFree (Ord (WDrive [1]) - 64))) ;
end ;

function TMagFmtChkDsk.CheckDisk (const DrvRoot: string; CorrectErrors, Verbose,
                                          CheckOnlyIfDirty, ScanDrive: boolean): boolean ;
var
    wdrive, wformat: widestring ;
begin
    result := false ;
    if NOT LoadFmifs then exit ;
    wdrive := Uppercase (DrvRoot) ;
    if NOT CheckDriveExists (wdrive, CorrectErrors, wformat) then exit ;
    MagFmtObj := Self ;
    fDoneOK := false ;
    fFileSysProblem := false ;
    fFreeSpaceAlloc := false ;
    fFirstErrorLine := '' ;
    Chkdsk (PWchar (wdrive), PWchar (wformat), CorrectErrors, Verbose,
                             CheckOnlyIfDirty, ScanDrive, 0, 0, @ChkDskCallback) ;
    if fFileSysProblem then
        result := true  // ignore stopped if got an error
    else
        result := fDoneOK ;
end ;

function TMagFmtChkDsk.VolumeCompression (const DrvRoot: string; Enable: boolean): boolean ;
var
    wdrive, wformat: widestring ;
begin
    result := false ;
    if NOT LoadFmifs then exit ;
    wdrive := Uppercase (DrvRoot) ;
    if NOT CheckDriveExists (wdrive, true, wformat) then exit ;
    result := EnableVolumeCompession (PWchar (wdrive), Enable) ;
end ;

// try and load various Format Manager for Installable File Systems functions.
// Returns false if failed

function TMagFmtChkDsk.LoadFmifs: boolean ;
begin
    result := Assigned (Chkdsk) ;
    if MagFmifs_Loaded then exit ;
    result := false ;
    if Win32Platform <> VER_PLATFORM_WIN32_NT then exit ;

// open libraries - only come here once
    result := false ;
    MagFmifs_Loaded := True ;
    MagFmifsib := LoadLibrary (fmifs) ;
    if MagFmifsib = 0 then exit ;

// set function addresses in DLL
    Chkdsk := GetProcAddress (MagFmifsib, 'Chkdsk') ;
    FormatEx := GetProcAddress (MagFmifsib, 'FormatEx') ;
    EnableVolumeCompession := GetProcAddress (MagFmifsib, 'EnableVolumeCompession') ;
    result := Assigned (Chkdsk) ;
end ;

Initialization
    MagFmifsib := 0 ;
    MagFmifs_Loaded := false ;
finalization
    if MagFmifs_Loaded then FreeLibrary (MagFmifsib) ;
end.

