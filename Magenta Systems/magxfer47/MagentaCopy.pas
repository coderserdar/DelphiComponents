unit MagentaCopy;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

{
Updated by Angus Robertson, Magenta Systems Ltd, England, 6th March 2017
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

TMagFileCopy is a high level HTTP Delphi component that allows indexing
and copying of multiple file directories, using a single function call.
The component handles listing source and destination files, and comparing
them to avoid unnecessary transfers, selection using a file mask, deletion
of old files, delete after copy, safe copy (to avoid partial files), and
zip before copying or unzip after copy.  A progress event provides various
levels of information for logging or display, depending upon application
requirements, and allows copying to be cancelled. The component is equally
useful for copying single files or entire volumes with hundreds of
thousands of files, and provides 'sync files' functionality making it ideal
for backups, only copying changed files and deleting old target files.

TMagFileCopy is undocumented, except for source code comments, but end user help is
available describing most of the functionality in Magenta Systems DUN Manager
application (from http://www.magsys.co.uk/dunman/), look under Scheduled Task
Properties, Sync Files. DUN Manager also provides a compiled test application for
TMagHttp (but DM source code is not available).

Requires Kevin Boylan's TVCLZip component for zipping from http://www.vclzip.net/,
if you don't purchase this component you will need to suppress DEFINE Zipping from
MAGZIP.INC so the zip code is not linked.

Requires Internet Component Suite (ICS) V8.42 dated March 2017 or later and
OpenSSL 1.1.0 or later, both of which may be downloaded from:
http://wiki.overbyte.eu/wiki/index.php/ICS_Download
The latest ICS version in the nightly zip includes the latest OpenSSL.

Compatible with Delphi 7/2006/2007/2009/2010/XE-XE8/D10/D10.1/D10.2
Tested with Windows Vista, 2008, 7, 8, 2012, 10, 2016

TMagFileCopy is copyrighted software, but may be used freely.

Main functions:

BuildDirList2 - build file directory, not sorted, date range selection
BuildDirList - build file directory, not sorted
GetDirList - build file directory, sorted
DispLocFiles - build formatted file directory, sorted
SelectCopyFileList - select files to copy by comparing source and target directories
SelCopyFiles - select and copy multiple files, optionally multiple directories
SelectFiles - select multiple files only
CopyFiles - copy multiple files only
DeleteFiles - delete multiple files
DeleteEmptyDirs - delete empty directories
CopyOneFile - copy single file
Cancel - cancel copying


All these functions may use of file directories comprising dynamic arrays
of TFileRec.  These arrays may be used in applications, where individual
access to files is required.



various file copying subroutines, Copyright Magenta Systems Ltd
08 May 2001 - baseline
28 May 2001 - fixed single dir never copying anything (recent bug)
6 June 2001 - added ReplyRO to replace read only files, set some errors for single dir
8 June 2001 - added Fr to TFileRec elements so names are unique, using FindFirstFile
11 June 2001 - should compile with D6
25 June 2001 - added LogLevelDelimFile and LogLevelDelimTot to return info
                 for each file suitable for further processing
29 June 2001 - fixed problem with fMask not getting all files to check
24 July 2001 - moved TFindList to magsub4
30 Oct 2001  - only return true if one or more files copied
11 Dec 2001  - added TTaskResult to distinguish results better
26 Dec 2001  - added zipping/unzipping of copies (quite messy, TypeSrcDirs not done yet)
7  Jan 2002  - BuildDirList now returns mixed case directory names as well as file names
21 Feb 2002  - moved FileTimeToDateTime and DateTimeToFileTime to magsubs4
25 Mar 2002  - added MaxFNameLen to improve directory listings (not for zip yet)
06 Aug 2002  - added TaskRunning to TTaskResult
4 Sept 2002  - added IgnoreFileExt - list is tmp;ftp;xxx;etc
8 Oct 2002   - check for file mask in multiple directories in copy type masked
             - no longer using trailing slashes on directories internally, still seems to work
14 Oct 2002  - added FrFileUDT UTC file time stamps to TFileRec record
15 Oct 2002  - fix bug always replacing masked files
9 Dec 2002   - added UseUTC (def true) to use UTC time for directory listings and selections
30 Jan 2003  - ensure source file deleted if it was zipped before copy
24 Apr 2003  - delete empty directories are deleting all files from them
               added CheckDirAny function (true if any files in directory)
29 Apr 2003  - added CopyType FCTypeDates for date range, BuildDirList2 function
2 May 2003   - added DeleteFiles and DeleteEmptyDirs functions
12 May 2003  - added CopyOneFile function, added duration (ms) to LogLevelDelimFile events
30 May 2003  - if duration fast, change 0 to 10 to indicate it did something
11 June 2003 - call copy event every 1000 files during listing and selection,
                   check for cancel (resets DM dead man timer on large dirs)
18 July 2003 - corrected FCReplNewer (did not work if file size different)
24 Aug 2003  - ensure unzip directory exists, might help trap unzipping errors
               unzip errors in task log
22 Oct 2003  - added unzip error handler, VCLZIP 3 only
               added UpdateUFileAge
               single file copy working again
5  Jan 2004  - using magsubs1 for common stuff instead of magsubs4
               directory stuff now part of component to avoid event issues
               fixed copy callback event issue
11 Jan 2004  - made Delphi 5 compatible (no TStringList.CaseSensitive)
29 Jan 2004  - allow to delete RO files after copy, delete empty source directories
               copy progress event now shows counts and file names
               fixed range error on multiple dirs copying totalling over 2 gigs
6 Feb 2004   - added event comment if single file copy skipped
11 Feb 2004  - ensure BuildDirList2 dynamic array is initialised, otherwise do it
21 Feb 2004  - replaced TMask by Match for better masked file selection
19 Mar 2004  - don't delete root source or target directory after copying
25 Aug 2004  - added MagRenameFile and MagDeleteFile to consolidate delete/rename read only files
               FCReplNewer replaces if target file is empty and source file not empty
               added CheckReplace to commonise date/size replace checking
11 Sept 2004 - GetDirList, SelectFiles and DeleteFiles now use SrcFName mask
                   for FCTypeDates as well as FCTypeMaskDir
30 July 2005 - TFindList moved to magclasses.pas
14 Aug 2005  - added network connection to a remote system with a logon (before copying files)
                  RemoteName is UNC name, LocalName is mapped drive, UserName and Password
                  then Connect and Disconnect.  GetConnection to check for mapped drive
27 Aug 2005  - supporting files larger than 2 gigs, FrFileBytes is now Int64
               sDirLine shows more size, MaxFNameLen from 22 to 32
5 Sept 2005  - MagCheckReplace and CopyOneFile support 64-bits, not backward compatible!
             - prevent too rapid progress messages, which slow down transfers, ProgressSecs = 5
11 Nov 2005  - added TSslCertCheck for FTP and HTTP
3  Nov 2006  - unit now MagentaCopy for ICS V6
03 Mar 2008  - added DelimActualSize which if non-zero is actual size relating to
                   duration, specifically during failed or partial FTP xfers
07 Aug 2008  - 2.3 - ensure lists set as sorted after sorting
               updated file copying to support Unicode/Widestring with Delphi 2009
               updated MaskMatches for unicode and performance (no string copying)
22 Sep 2008  - 3.0 - support Unicode file listing and copying with Delphi 2007 and earlier
               increased size of initial TFileRecs array from 1000 to 5000 or 25000
                 if subdirs to avoid too many resizes causing copying of data
               FmtFileDirList builds listing with stream
18 Nov 2008  - 3.1 - fixed FmtFileDirList with D2009
7 Aug 2010   - 3.5 - fixed various string casts for D2009 and later
11 Feb 2011  - 3.7 - list and copy empty directories.  Directory functions have a ListDirs parameter,
               TMagFileCopyW has a new EmptyDirs property, directory attributes should be copied
               Using FCTypeMaskDir and *.* now selects files without an extension
               added MultiThreaded property to stop Application.ProceessMessages being called
11 Aug 2011  - 3.7 - fix copying into new empty directory instead of next existing directory
               now registered in MagentaXferReg
20 Oct 2011  - 3.8 - most file sizes now reported in Kbytes. Mbytes, Gbytes instead of bytes
               added explict file name length errors (max 255 + drive/path)
24 Aug 2012 - 4.0 - updated to support ICS V8 with IPv6
11 Jun 2013 - 4.1 - added IgnorePaths to SelectCopyFileList and TMagFileCopy to ignore files where source
                 has specific partial path, list is c:\temp;c:\temp2\;c:\temp3\;etc, or destination for
                 deletion matches partial path
              added Wow64RedirDisable property for Win64 allow all files to be copied correctly from Win32
              added new ProgressEvent which passes TCopyProgress record updated for progress of
                  current file and session including total bytes copied allowing percentage progress display
              using TMagStringBuild to build listings instead of TMemoryStream
              when checking copy, use TMagStringBuild for improved listing performance
6 Mar 2017 - 4.6 - changed TULargeInteger to ULARGE_INTEGER to keep modern compilers happy


}

interface

{$I Include\OverbyteIcsDefs.inc}
{$I MAGZIP.INC}

uses
  Windows, Messages, SysUtils, Classes, Forms, Masks, ShellAPI, HTTPApp,
  MagSubs1, MagClasses
  {$IFDEF Zipping} , VCLZip, VCLUnZip, kpZipObj {$ENDIF} ;


{ex000616.log.zip      11,123,903  -rwx  23/03/2001 12:57:00    /dir}
{dvdimage.nrg           4,610,785,436    A 26/08/2005 16:01:49  \    }
const
    sDirLine = '%-40s %13s %4s %-20s %s' ;
    sDirLit = '<DIR>' ;
    FileTimeBase = -109205.0;
    FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nSek per Day

    DelimSrcFile = 0 ; DelimTarFile = 1 ; DelimSize = 2 ; DelimSucc = 3 ;
    DelimFail = 4 ; DelimComm = 5 ; DelimDuration = 6 ; DelimActualSize = 7 ;
    DelimTotFields = 8 ;
    sLineEnd: array [0..1] of Char = (CR, LF) ;  // 21 May 2013
type
// file copy selection and replace options
    TFileCopyType = (FCTypeSingle, FCTypeMaskDir, FCTypeArchDir,
                                                   FCTypeAllDir, FCTypeDates) ;

    TFileCopyRepl = (FCReplNever, FCReplAlways, FCReplDiff, FCReplNewer) ;

    TFileCopyState = (FCStateNone, FCStateIgnore,FCStateDir, FCStateSelect,
                                 FCStateCopying, FCStateOK, FCStateFailed) ;

    TLogLevel = (LogLevelInfo, LogLevelFile, LogLevelProg, LogLevelDiag,
                 LogLevelDelimFile, LogLevelDelimTot) ;

    TTaskResult = (TaskResNone, TaskResOKNew, TaskResOKNone, TaskResFail,
                    TaskResAbort, TaskRunning) ;
    TSslCertCheck = (SslCCNone, SslCCWarn, SslCCRequire) ;    // 11 Nov 2005
    {$IFDEF Zipping}
    TZipExtFmt = (ExtnAdd, ExtnReplace) ;
    TZipPath = (PathNone, PathNew, PathOriginal, PathNewOrig,
                PathSpecific, PathSpecOrig) ;
    TZipType = (TypeUnzip, TypeSrcAddX, TypeSrcReplX, TypeSrcDirs) ;
    {$ENDIF}

var
    TaskResultNames: array [0..6] of string =
      ('No Result', 'OK New', 'OK None', 'Failed', 'Aborted', 'Running', '') ;
const
    TaskResultStrings: array [Low(TTaskResult)..High(TTaskResult)] of ShortString =
      ('No Result', 'OK New', 'OK None', 'Failed', 'Aborted', 'Running') ;
    SslCertCheckStrings: array [Low(TSslCertCheck)..High(TSslCertCheck)] of ShortString =
      ('None', 'Warn', 'Require') ;

{.$DEFINE USESTRING}
{$DEFINE USEUNISTRING}

type
// file listing record
    TFileRec = record
{$IFDEF USESTRING}
        FrFileName: string ;    // filename and extension
        FrSubDirs: string ;     // \ for base directory, else located sub directories
        FrFullName: string ;    // basedir, subdirs, filename - complete path
{$ENDIF}
{$IFDEF USEUNISTRING}
        FrFileName: UnicodeString ;    // filename and extension
        FrSubDirs: UnicodeString ;     // \ for base directory, else located sub directories
        FrFullName: UnicodeString ;    // basedir, subdirs, filename - complete path
{$ENDIF}
        FrDirLevel: integer ;   // 0 for base directory, or level of sub dirs
        FrDirBaseLen: integer ; // length of basedir within FullName - used for display
        FrFileBytes: int64 ;    // size of file in bytes (FileSize is a function name) 27 Aug 2005 (was int)
        FrFileDT: TDateTime ;   // file time stamp - local time
        FrFileUDT: TDateTime ;  // file time stamp - UTC time
        FrFileAttr: integer ;   // bitmapped Windows file attributes
        FrExtra: string ;       // <DIR> or UNIX file attributes
        FrLinks: string ;       // not blank means this is not a real file
        FrFileCopy: TFileCopyState ;  // file copy state, selected, done, etc,
    end ;
    TFileRecs = array of TFileRec ;   // lots of records
    PTFileRec = ^TFileRec ;           // pointer once record added to TList
// 20 May 2013 - file copying progress, current file and session
    TCopyProgress = record
        LogLevel: TLogLevel ;         // log level
        Info: String ;                // simple display information
        ProgMessBase: String ;        // partial progess info
        CurSrcName: String ;          // current copy source file
        CurTarName: String ;          // current copy target file
        CurDelName: String ;          // current delete file
        CurOKDone: int64 ;            // bytes copied of current file
        CurFileBytes: int64 ;         // size of file being copied
        CurStartTick: longword ;      // when current file started copying or deleting in ticks (from when PC booted)
        CurEstimateTicks: longword ;  // current file estimated copying time based on first 5 seconds in ticks (1,000th/sec)
        CurDonePercent: integer ;     // current file percentage copied
        CurDoneTicks: longword ;      // current file copying duration in ticks
        TotProcFiles: integer ;       // total files to process in session
        TotDoneNr: integer ;          // how many files processed so far
        TotProcBytes: int64 ;         // total size of files to copy in session
        TotDelFiles: integer ;        // total number of files to delete
        TotDelBytes: int64 ;          // total size of files to delete in session
        SessStartTick: longword ;     // when session started in ticks (from when PC booted)
        SessEstimateTicks: longword ; // session estimated time updated after each file in ticks (1,000th/sec)
        SessDonePercent: integer ;    // total files percentage copied
        SessDoneTicks: longword ;     // total files copying duration in ticks
        SessAvSpeed: integer ;        // total files average speed so far
        ProcOKFiles: integer ;        // total files copied OK in session
        DelOKFiles: integer ;         // total files deleted OK in session
        ProcFailFiles: integer ;      // total files failed copy in session
        SkippedFiles: integer ;       // total files skipped copy in session
        ProcBytesDone: int64 ;        // total size of files copied OK or failed in session
        ProcBytesLast: int64 ;        // total bytes copied up to start of current file
        ProcOKBytes: int64 ;          // total size of files copied OK in session
        DelOKBytes: int64 ;           // total size of files deleted in session
        LastProgTick: longword ;      // used to suppress progress messages for ProgressSecs
    end ;

// copying event, allowing main program to log and display stuff, and cancel
    TBulkCopyEvent = Procedure (LogLevel: TLogLevel ; Info: string ;
                                              var Cancel: boolean) of object ;
    PBulkCopyEvent = ^TBulkCopyEvent ;

    TProgressEvent = Procedure (Sender: TObject ; CopyProgress: TCopyProgress ;     // 20 May 2013
                                                    var Cancel: boolean) of object ;
    PProgressEvent = ^TProgressEvent ;

    TThreadEvent = Procedure (LogLevel: TLogLevel ; const Id, Info: String ;
                                              var Cancel: boolean) of object ;      // 16 Feb 2011

// main multi file copy function
  TMagFileCopy = class(TComponent)

  private
    { Private declarations }
//        fProgMessBase: string ;  // 22 May 2013

  protected
    { Protected declarations }
        fCancelFlag: boolean ;
        fMultiDirList: String ;
        fSrcDir: String ;
        fSrcDirList: TStringList ;
        fSrcFName: String ;
        fTarDir: String ;
        fTarDirList: TStringList ;
        fCopyType: TFileCopyType ;
        fMultiDir: Boolean ;
        fSubDirs: Boolean ;
        fDelDone: Boolean ;
        fDelOldTar: Boolean ;
        fMask: Boolean ;
        fPrev: Boolean ;
        fRepl: TFileCopyRepl ;
        fReplRO: boolean ;
        fSafe: Boolean ;
        fLocalHost: string ;
        fCopyEvent: TBulkCopyEvent ;
        fReqResponse: string ;
//        fTotProcFiles: integer ;     // 22 May 2013 all replaced by fCopyProg below
//        fProcOKFiles: integer ;
//        fDelOKFiles: integer ;
//        fProcFailFiles: integer ;
//        fSkippedFiles: integer ;
//        fTotProcBytes: int64 ;
//        fProcOKBytes: int64 ;
//        fDelOKBytes: int64 ;
//        fLastProgTick: longword ;    // 5 Sept 2005
        fIgnoreFileExt: string ;
        fUseUTC: Boolean ;
        fCopyLoDT: TDateTime ;
        fCopyHiDT: TDateTime ;
        fUserName: string;
        fPassword: string;
        fLocalName: string;
        fRemoteName: string;
        fRemConnected: string ;
        fProgressSecs: integer ;     // 5 Sept 2005
        fMultiThreaded: boolean ;    // 16 Sept 2010
        fEmptyDirs: boolean ;        // 7 Feb 2011
        fIgnorePaths: String ;       // 22 May 2013
        fCopyProg: TCopyProgress ;   // 22 May 2013 replaces most fTotxx/fProcxx variables
        fWow64RedirDisable: boolean ; // 22 May 2013
        fProgressEvent: TProgressEvent ;  // 22 May 2013
     {$IFDEF Zipping}
        fZipDownDel: Boolean ;
        fZipped: Boolean ;
        fZipType: TZipType ;
        fZipPath: TZipPath ;
        fZipDir: String ;
     {$ENDIF}

        procedure SetMultiDirList (value: string) ;
        procedure EndUnZipEvent (Sender: TObject; FileIndex: Integer; FName: String) ;
        procedure UnZipHandleMessage(Sender: TObject;
                  const MessageID: Integer; const Msg1, Msg2: String;
                  const flags: Cardinal; var Return: Integer) ;
        procedure MessagePump;

  public
    { Public declarations }

    SrcFiles: TFileRecs ;
    SrcFileList: TFindList ;
    TarFiles: TFileRecs ;
    TarFileList: TFindList ;
    TotSrcFiles: integer ;
    TotTarFiles: integer ;
    CurProcFiles: integer ;
    CurSkippedFiles: integer ;
    CurDelFiles: integer ;

    constructor Create(Aowner:TComponent); override;
    destructor Destroy; override;
    procedure doCopyEvent (const LogLevel: TLogLevel ; const Info: string) ;
    function SelCopyFiles (const CheckFiles: boolean): TTaskResult ;
    function SelectFiles (const CurSrcDir, CurTarDir: string): TTaskResult ;
    function CopyFiles (const CurSrcDir, CurTarDir: string): TTaskResult ;
    function DeleteFiles (const CheckFiles: boolean): TTaskResult ;
    function DeleteEmptyDirs (RootDir: string; DirList: TStringList): integer ;
    function CopyOneFile (const Fnamesrc, Fnametar: string ;
        Replopt: TFileCopyRepl; const Safe: boolean; var Fsize: Int64) : TTaskResult ;
    procedure Cancel ;
    procedure ClearTarList ;
    procedure ClearSrcList ;
    function BuildDirList2 (LocDir, LocPartName: string; const SubDirs: boolean;
                Level, InitDLen: integer ; const LoDT, HiDT: TDateTime ; var TotFiles: integer;
                                var LocFiles: TFileRecs; const ListDirs: boolean = false): boolean ;
    function BuildDirList (LocDir, LocPartName: string; const SubDirs: boolean;
                 Level, InitDLen: integer; var TotFiles: integer;
                    var LocFiles: TFileRecs; const ListDirs: boolean = false): boolean ;
    function GetDirList (LocDir, LocPartName: string; const SelOpt: TFileCopyType;
              const SubDirs: boolean; LoDT, HiDT: TDateTime ; var LocFiles: TFileRecs;
                        var LocFileList: TFindList; const ListDirs: boolean = false): integer ;
    function DispLocFiles (const SrcDir, SrcFile: string ;
              const CopyType: TFileCopyType ; const SubDirs: boolean; const UTCFlag: boolean;
                                                const ListDirs: boolean = false): string ;
    function SelectCopyFileList (const SrcFileList, TarFileList: TFindList; const fname: string;
              const selopt: TFileCopyType; const replopt: TFileCopyRepl; const DiffStamp: integer;
              const IgnoreOldTime: boolean; IgnoreExt: string ; var skipped: integer;
                                    const UTCFlag: boolean; const IgnorePaths: String = ''): integer ;
    function Connect: boolean ;
    function Disconnect (AForce: boolean): boolean ;
    function GetConnection (ALocalName: string): string ;

  published
    { Published declarations }

    property CancelFlag: boolean       read fCancelFlag     write fCancelFlag ;
    property MultiDirList: String      read fMultiDirList   write SetMultiDirList ;
    property SrcDir: String            read fSrcDir         write fSrcDir ;
    property SrcDirList: TStringList   read fSrcDirList     write fSrcDirList ;
    property SrcFName: String          read fSrcFName       write fSrcFName ;
    property TarDir: String            read fTarDir         write fTarDir ;
    property TarDirList: TStringList   read fTarDirList     write fTarDirList ;
    property CopyType: TFileCopyType   read fCopyType       write fCopyType ;
    property MultiDir: Boolean         read fMultiDir       write fMultiDir ;
    property SubDirs: Boolean          read fSubDirs        write fSubDirs ;
    property DelDone: Boolean          read fDelDone        write fDelDone ;
    property DelOldTar: Boolean        read fDelOldTar      write fDelOldTar ;
    property Mask: Boolean             read fMask           write fMask ;
    property Prev: Boolean             read fPrev           write fPrev ;
    property Repl: TFileCopyRepl       read fRepl           write fRepl ;
    property ReplRO: boolean           read fReplRO         write fReplRO ;
    property Safe: Boolean             read fSafe           write fSafe ;
    property LocalHost: string         read fLocalHost      write fLocalHost ;
    property ProgressSecs: integer     read fProgressSecs   write fProgressSecs ;   // 5 Sept 2005
    property CopyEvent: TBulkCopyEvent read fCopyEvent      write fCopyEvent ;
    property TotProcFiles: integer     read fCopyProg.TotProcFiles ;
    property ProcOKFiles: integer      read fCopyProg.ProcOKFiles ;
    property DelOKFiles: integer       read fCopyProg.DelOKFiles ;
    property ProcFailFiles: integer    read fCopyProg.ProcFailFiles ;
    property SkippedFiles: integer     read fCopyProg.SkippedFiles ;    
    property ReqResponse: string       read fReqResponse ;
    property TotProcBytes: int64       read fCopyProg.TotProcBytes ;
    property ProcOKBytes: int64        read fCopyProg.ProcOKBytes ;
    property DelOKBytes: int64         read fCopyProg.DelOKBytes ;
    property IgnoreFileExt: string     read fIgnoreFileExt  write fIgnoreFileExt ;
    property UseUTC: Boolean           read fUseUTC         write fUseUTC ;
    property CopyLoDT: TDateTime       read fCopyLoDT       write fCopyLoDT ;
    property CopyHiDT: TDateTime       read fCopyHiDT       write fCopyHiDT ;
    property UserName: string          read fUserName       write fUserName ;
    property Password: string          read fPassword       write fPassword ;
    property LocalName: string         read fLocalName      write fLocalName ;
    property RemoteName: string        read fRemoteName     write fRemoteName ;
    property RemConnected: string      read fRemConnected ;
    property MultiThreaded : Boolean   read fMultiThreaded  write fMultiThreaded;   // 16 Sept 2010
    property EmptyDirs: Boolean        read fEmptyDirs      write fEmptyDirs ;  // 7 Feb 2011
    property IgnorePaths: String       read fIgnorePaths    write fIgnorePaths ;    // 22 May 2013
    property CopyProg: TCopyProgress  read fCopyProg ;                             // 22 May 2013
    property Wow64RedirDisable: boolean read fWow64RedirDisable write fWow64RedirDisable ; // 22 May 2013
    property ProgressEvent: TProgressEvent read fProgressEvent write fProgressEvent; // 22 May 2013
  {$IFDEF Zipping}
    property ZipDownDel: Boolean       read fZipDownDel     write fZipDownDel ;
    property Zipped: Boolean           read fZipped         write fZipped ;
    property ZipType: TZipType         read fZipType        write fZipType ;
    property ZipPath: TZipPath         read fZipPath        write fZipPath ;
    property ZipDir: String            read fZipDir         write fZipDir ;
  {$ENDIF}
  end ;

// various public file handling and copying functions
// also used by TMagFtp and TMagHttp

function GetMaskedName (const mask: string; const PrevFlag: boolean; const host: string): string ;
function FmtFileDir (const FileRecs: TFileRecs; const UTCFlag: boolean): string ;
function FmtFileDirList (const LocFileList: TFindList; const UTCFlag: boolean): string ;
function CompareFNext (Item1, Item2: Pointer): Integer ;
function AttrStr (const Attr: longword): string ;
function GetTaskResName (TaskResult: TTaskResult): string ;
function CheckDirAny (LocDir: string): boolean ;
function MagRenameFile (const OldName, NewName: string;
                                        const Replace, ReadOnly: boolean): Integer ;
function MagDeleteFile (const Fname: string; const ReadOnly: boolean): Integer ;
function MagCheckReplace (const replopt: TFileCopyRepl;
           const IgnoreOldTime: boolean; const AllowDiffDT: TDateTime;
            const SrcSize, TarSize: Int64; SrcDT, TarDT: TDateTime): boolean ;
procedure CopyProgClearAll (var CopyProg: TCopyProgress) ;    // 16 May 2013
procedure CopyProgClearCur (var CopyProg: TCopyProgress) ;    // 16 May 2013
procedure CopyProgDuration (var CopyProg: TCopyProgress; TicksPerFile: Longword = 50) ; // 16 May 2013
function CalcSpeed (DurTicks, FSize: int64): int64 ;  // 22 May 2013 moved from MagentaFTP


// need special dynamically loaded version of CopyFileEx since not available in Win9x
// 6 Aug 2008 - changed from Ansi to WideChar version
var
//    CopyFileExQ: function(lpExistingFileName, lpNewFileName: PWideChar;
//      lpProgressRoutine: TFNProgressRoutine; lpData: Pointer; pbCancel: PBool;
//          dwCopyFlags: DWORD): BOOL; stdcall {= Nil} ;  // 7 July 2011 compiler does not like this

    AppProcMessCount: integer = 0 ;
    AppProgressCount: integer = 0 ;
    MaxFNameLen: integer = 32 ;     // maximum file name length to display in listings

const
    AppProcMessMax = 50 ;  // how often to use Application.ProcessMessages, every 50 files
    AppProgressMax = 20 ;  // how often to report progress and check cancelled, 20*50=1000
    AppTicksPerFile = 20 ;   // 22 May 2013 millisecs to open file when calculation session duration
    AppDurationWait = 5000 ; // 22 May 2013 millisecs to wait before estimating duration and speed

{procedure Register;}

implementation

{procedure Register;
begin
  RegisterComponents('Magenta Systems', [TMagFileCopy]);
end;}

function GetTaskResName (TaskResult: TTaskResult): string ;
begin
    result := '' ;
    if TaskResult > TaskRunning then exit ;
    result := TaskResultNames [Ord (TaskResult)] ;
end ;

// 16 May 2013 clear current file progress stuff

procedure CopyProgClearCur (var CopyProg: TCopyProgress) ;
begin
    with CopyProg do
    begin
        CurStartTick := 0 ;
        CurEstimateTicks := 0 ;
        CurSrcName := '' ;
        CurTarName := '' ;
        CurDelName := '' ;
        CurOKDone := 0 ;
        CurFileBytes := 0 ;
        LastProgTick := 0 ;
        CurDonePercent := 0 ;
        CurDoneTicks := 0 ;
    end;
end;

// 16 May 2013 clear all progress stuff

procedure CopyProgClearAll (var CopyProg: TCopyProgress) ;
begin
    CopyProgClearCur (CopyProg) ;
    with CopyProg do
    begin
        Info := '' ;
        ProgMessBase := '' ;
        TotProcFiles := 0 ;
        ProcBytesDone := 0 ;
        TotProcBytes := 0 ;
        TotDelFiles := 0 ; 
        TotDelBytes := 0 ;
        SessStartTick := 0 ;
        SessEstimateTicks := 0 ;
        ProcOKFiles := 0 ;
        DelOKFiles := 0 ;
        ProcFailFiles := 0 ;
        SkippedFiles := 0 ;
        ProcOKBytes := 0 ;
        DelOKBytes := 0 ;
        SessDonePercent := 0 ;
        SessDoneTicks := 0 ;
        SessAvSpeed := 0 ;
    end;
end;

// 22 May 2013  moved from MagentaFTP

function CalcSpeed (DurTicks, FSize: int64): int64 ;
begin
    result := 0 ;
    if (FSize < 1000) or (DurTicks < 1000) then exit ;
    result := (1000 * FSize) div DurTicks;
end ;

// 16 May 2013 calculate file progress stuff

procedure CopyProgDuration (var CopyProg: TCopyProgress; TicksPerFile: Longword = 50) ;
//var
//    T: Int64 ;
begin
    with CopyProg do
    begin
        if (CurStartTick > 0) and (CurFileBytes > 0) then
        begin
         // how long it's taken so far in millisecs
            CurDoneTicks := ElapsedTicks (CurStartTick) ;
         // percent based on file size
            CurDonePercent := (CurOKDone * 100) div CurFileBytes ;
         // estimate how long file will take to copy in millisecs
            if (CurDoneTicks >= AppDurationWait) and (CurDonePercent > 0) then
                    CurEstimateTicks := (CurDoneTicks * 100) div LongWord (CurDonePercent) ;
        end;
        if (SessStartTick > 0) and (TotProcBytes > 0) then
        begin
         // how long it's taken so far in millisecs
            SessDoneTicks := ElapsedTicks (SessStartTick) ;
         // percent based on total of all file sizes
            SessDonePercent := (ProcBytesDone * 100) div TotProcBytes ;
            if (SessDoneTicks > AppDurationWait) and (SessDonePercent > 0) then
            begin
             // estimate how long session will take in millisecs, add extra for file opening overhead
                SessEstimateTicks := ((SessDoneTicks * 100) div LongWord (SessDonePercent)) +
                                                (LongWord (TotProcFiles - TotDoneNr) * TicksPerFile) ;
             // average speed, allowing for file opening overhead
              {  T := LongWord (TotDoneNr) * TicksPerFile ;  not very reliable
                If T < SessDoneTicks then
                    T := SessDoneTicks - T
                else
                    T := SessDoneTicks ;   }
                SessAvSpeed := CalcSpeed (SessDoneTicks, ProcBytesDone) ;  // bytes/sec
            end;
        end;
    end;
end;

constructor TMagFileCopy.Create(Aowner:TComponent);
begin
    inherited create(AOwner);
    fSrcDirList := TStringList.Create ;
    fTarDirList := TStringList.Create ;
    SrcFileList := TFindList.Create ;
    TarFileList := TFindList.Create ;
    ClearTarList ;
    ClearSrcList ;
    fCancelFlag := false ;
    fUseUTC := true ;
    fProgressSecs := 2 ;   // update progress every two seconds default
    CopyProgClearAll (fCopyProg) ;  // 22 May 2013
end ;

destructor TMagFileCopy.Destroy;
begin
    ClearTarList ;
    ClearSrcList ;
    fSrcDirList.Free ;
    fTarDirList.Free ;
    SrcFileList.Free  ;
    TarFileList.Free ;
    inherited Destroy;
end;

// 16 Sept 2010 - keep application interactive, unless multithreaded

procedure TMagFileCopy.MessagePump ;
begin
    if FMultiThreaded then
//        Self.ProcessMessages
    else
        Application.ProcessMessages ;
end;

function TMagFileCopy.Connect: boolean ;
var
    NetResource: TNetResource;
    dwFlags: DWORD;
begin
    result := false ;
    NetResource.dwType := RESOURCETYPE_DISK ;
    NetResource.lpLocalName := PChar (FLocalName) ;   // ie N: for drive N
    NetResource.lpRemoteName := PChar (FRemoteName) ; // ie \\sysname\share UNC name
    NetResource.lpProvider := Nil ;
    dwFlags := 0 ;
    if WNetAddConnection2 (NetResource, PChar (FPassword), PChar (FUserName), dwFlags) <> NO_ERROR then
    begin
        fReqResponse := 'Failed to Connect to Remote Name: ' +
                              FRemoteName + '; Error: ' + SysErrorMessage (GetLastError) ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
    end
    else
    begin
        result := true ;
        fRemConnected := FRemoteName ;
        fReqResponse := 'Connected OK to Remote Name: ' + FRemoteName ;
        if FLocalName <> '' then fReqResponse := fReqResponse + ' as Drive ' + FLocalName ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
    end
end;

function TMagFileCopy.Disconnect (AForce: boolean): boolean ;
var
    Device: string;
begin
    result := false ;
    if Trim (FLocalName) <> '' then
        Device := FLocalName
    else
        Device := FRemoteName;
    if WNetCancelConnection2 (PChar (Device), CONNECT_UPDATE_PROFILE, AForce) <> NO_ERROR then
    begin
        fReqResponse := 'Failed to Disconnect: ' + Device + '; Error: ' + SysErrorMessage (GetLastError) ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
    end
    else
    begin
        result := true ;
        fRemConnected := '' ;
        fReqResponse := 'Disconnected OK: ' + Device ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
    end
end;

function TMagFileCopy.GetConnection (ALocalName: string): string ;
var
    Buffer: array[0..1024] of Char;
    BufSize: DWORD ;
begin
    result := '' ;
    BufSize := 1024 ;
    if WNetGetConnection (PChar (ALocalName), Buffer, BufSize) <> NO_ERROR then
    begin
        fReqResponse := 'Connection Not Found: ' + ALocalName + '; Error: ' + SysErrorMessage (GetLastError) ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
    end
    else
    begin
        result := Buffer ;
        fReqResponse := ALocalName + ' is Connected to ' + result ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
    end
end;


// free memory used by Src lists

procedure TMagFileCopy.ClearSrcList ;
begin
    TotSrcFiles := 0 ;
    SrcFiles := Nil ;
    try
        SrcFileList.Clear ;
    except
    end ;
end ;

// free memory used by Tars lists

procedure TMagFileCopy.ClearTarList ;
begin
    TotTarFiles := 0 ;
    TarFiles := Nil ;
    try
        TarFileList.Clear ;
    except
    end ;
end ;


// multi directories are passed as srcdir+tab+tardir+recsep (repeat)
// parse them into simple stringlists

procedure TMagFileCopy.SetMultiDirList (value: string) ;
var
    I, rows: integer ;
    aitems, afields: StringArray ;
begin
    fSrcDirList.Clear ;
    fTarDirList.Clear ;
    if value = '' then exit ;
    SetLength (afields, 0) ;
    SetLength (aitems, 0) ;
    aitems := StrArraySplit (value, RECSEP) ;
    rows := Length (aitems) ;
    if rows = 0 then exit ;
    for I := 0 to pred (rows) do
    begin
        afields := StrArraySplit (aitems [I], TAB) ;
        if Length (afields) >= 2 then
        begin
            if (afields [0] <> '') and (afields [1] <> '') then
            begin
                fSrcDirList.Add (afields [0]) ;
                fTarDirList.Add (afields [1]) ;
            end ;
        end ;
    end ;
end ;

function MaskMatches(const Pattern: string; const aString: string; CaseSensitive: Boolean = false): Boolean;
{ Returns true if astring matches the Pattern, which may contain   }
{ wildcards * and ?. }
{ Copied from the "matcher" component for Delphi32.                                 }
{ Copyright 1996, Patrick Brisacier and Jean-Fabien Connault.  All Rights Reserved. }

  function RMatch(s: PChar; i: Integer; p: PChar; j: Integer): Boolean;
  Var
   matched        : Boolean;
   k              : Integer;
    begin
     if p[0]=#0 then
      RMatch :=  TRUE
     else while TRUE do
     if (s[i]=#0) and (p[j]=#0) then begin
      RMatch :=  TRUE;
      exit
    end
    else
      if p[j] = #0 then begin
        RMatch :=  FALSE;
        exit
      end
      else if (p[j] = '*') then begin
        k := i;
        if (p[j + 1] = #0) then begin
          RMatch :=  TRUE;
          exit
        end
        else while TRUE do begin
          matched := RMatch(s, k, p, j + 1);
          if matched OR (s[k] = #0) then begin
            RMatch :=  matched;
            exit;
          end; {if}
          inc(k);

        end; {while}
      end {if}
       else
       begin
        if not CaseSensitive then
         begin
          p[j] := UpCase(p[j]);
          s[i] := UpCase(s[i]);
         end;

        if ((p[j] = '?') and (s[i] <> #0)) OR (p[j] = s[i]) then
         begin
          inc(i);
          inc(j);
         end
        else
         begin
          RMatch :=  FALSE;
          exit
         end;
       end;
    end;

begin {Match}
    result := RMatch(PChar (aString), 0, PChar(Pattern), 0);
end;

function GetMaskedName (const mask: string; const PrevFlag: boolean;
                                            const host: string): string ;
begin
    result := trim (mask) ;
    if result = '' then result := '*.*' ;
    if result = '*.*' then exit ;
    result := StringReplace (result, '$H', host, [rfReplaceAll]) ;
    if PrevFlag then
        result := FormatDateTime (result, Now - 1)
    else
        result := FormatDateTime (result, Now) ;
end ;

// delete a single file, optionally read only

function MagDeleteFile (const Fname: string; const ReadOnly: boolean): Integer ;
var
    attrs: integer ;
begin
    result := -1 ;    // file not found
    attrs := FileGetAttr (Fname) ;
    if attrs < 0 then exit ;
    if ((attrs and faReadOnly) <> 0) and ReadOnly then
    begin
        result := FileSetAttr (Fname, 0) ;
        if result <> 0 then result := 1 ;
        if result <> 0 then exit ;  // 1 could not change file attribute, ignore system error
    end ;
    if DeleteFile (Fname) then
        result := 0   // OK
    else
        result := GetLastError ; // system error
end ;

// rename a single file, optionally replacing, optionally read only

function MagRenameFile (const OldName, NewName: string;
                                        const Replace, ReadOnly: boolean): Integer ;
begin
    if FileExists (NewName) then
    begin
        result := 2 ;  // rename failed, new file exists
        if NOT Replace then exit ;
        result := MagDeleteFile (NewName, ReadOnly) ;
        if result <> 0 then exit ;  // 1 could not change file attribute, higher could not delete file
    end ;
    if RenameFile (OldName, NewName) then
        result := 0   // OK
    else
        result := GetLastError ; // system error
end ;

// returns a string with ASCII file attributes

function AttrStr (const Attr: longword): string ;
begin
    Result := '';
    if Attr = $FFFFFFFF then exit ;
    if (Attr AND File_Attribute_Directory)  > 0 then Result := Result + 'D' ;
    if (Attr AND File_Attribute_Archive)    > 0 then Result := Result + 'A' ;
    if (Attr AND File_Attribute_Readonly)   > 0 then Result := Result + 'R' ;
    if (Attr AND File_Attribute_System)     > 0 then Result := Result + 'S' ;
    if (Attr AND File_Attribute_Hidden)     > 0 then Result := Result + 'H' ;
    if (Attr AND File_Attribute_Temporary)  > 0 then Result := Result + 'T' ;
    if (Attr AND File_Attribute_Compressed) > 0 then Result := Result + 'C' ;
    if (Attr AND File_Attribute_Encrypted)  > 0 then Result := Result + 'E' ;
end ;

// format file directory from arrays

function FmtFileDir (const FileRecs: TFileRecs; const UTCFlag: boolean): string ;
var
    I, tot, len: integer ;
    totfsize: int64 ;
    temp1, temp2, temp3, linemask: string ;
    listing: TMagStringBuild ;
begin
    result := '' ;
    linemask := sDirLine ;
    temp1 := IntToStr (MaxFNameLen) ;
    linemask [3] := temp1 [1] ;
    linemask [4] := temp1 [2] ;
    totfsize := 0 ;
    tot := Length (FileRecs) ;
    listing := TMagStringBuild.Create (tot * 100) ;  // allocate memory for each line
    try
    if tot <> 0 then
    begin
        for I := 0 to Pred (tot) do
        begin
            with FileRecs [I] do
            begin
                temp1 := FrFileName ;
                if temp1 = '' then temp1 := '???' ;  // should never happen
                len := Length (temp1) ;
                if len > MaxFNameLen then
                    temp1 := Copy (temp1, 1, MaxFNameLen - 3) + '...' ;  // too long to display
                inc (totfsize, FrFileBytes) ;
                if FrExtra = sDirLit then
                    temp2 := FrExtra
                else
                    temp2 := Int64ToCStr (FrFileBytes) ;
                temp3 := '' ;
                if FrfileDT <> 0 then
                begin
                    if UTCFlag then
                        temp3 := DateToStr (FrfileUDT) + ' ' + TimeToStr (FrfileUDT)
                    else
                        temp3 := DateToStr (FrfileDT) + ' ' + TimeToStr (FrfileDT) ;
                end ;
                listing.AppendLine (Format (linemask, [temp1, temp2, AttrStr (FrFileAttr), temp3, FrSubDirs])) ;
             end ;
        end ;
    end ;
    listing.AppendLine ('Total Files Found: '+ IntToCStr (tot) + ', Total size: ' + IntToKByte (totfsize, true)) ;
    Result := listing.ToString ;
    finally
        listing.Free ;
    end ;
end ;

// format file directory for list (pointers to arrays)

function FmtFileDirList (const LocFileList: TFindList; const UTCFlag: boolean): string ;
var
    I, tot, len: integer ;
    totfsize: int64 ;
    temp1, temp2, temp3, linemask: string ;
    FileRecX: PTFileRec ;
    listing: TMagStringBuild ;
begin
    tot := LocFileList.Count ;
    result := '' ;
    linemask := sDirLine ;
    listing := TMagStringBuild.Create (tot * 100) ;  // allocate memory for each line
    temp1 := IntToStr (MaxFNameLen) ;
    linemask [3] := temp1 [1] ;
    linemask [4] := temp1 [2] ;
    totfsize := 0 ;
    try
    try
    if tot <> 0 then
    begin
        for I := 0 to Pred (tot) do
        begin
            if LocFileList [I] = Nil then continue ;
            FileRecX := LocFileList [I] ;
            with FileRecX^ do
            begin
                temp1 := FrFileName ;
                if temp1 = '' then temp1 := '???' ;  // should never happen
                len := Length (temp1) ;
                if len > MaxFNameLen then temp1 := Copy (temp1, 1, MaxFNameLen - 3) + '...' ;  // too long to display
                inc (totfsize, FrFileBytes) ;
                if FrExtra = sDirLit then
                    temp2 := sDirLit
                else
                    temp2 := Int64ToCStr (FrFileBytes) ;
                temp3 := '' ;
                if FrfileDT <> 0 then
                begin
                    if UTCFlag then
                        temp3 := DateToStr (FrfileUDT) + ' ' + TimeToStr (FrfileUDT)
                    else
                        temp3 := DateToStr (FrfileDT) + ' ' + TimeToStr (FrfileDT) ;
                end ;
                listing.AppendLine (Format (linemask, [temp1, temp2, AttrStr (FrFileAttr), temp3, FrSubDirs])) ;
            end ;
       end ;
    end ;
    except
        listing.AppendLine ('Exception accessing file record') ;
    end ;
    listing.AppendLine ('Total Files Found: '+ IntToCStr (tot) + ', Total size: ' + IntToKByte (totfsize, true)) ;
    Result := listing.ToString ;
    finally
        listing.Free ;
    end ;
end ;

// check if directory has at least one file or subdirectory

function CheckDirAny (LocDir: string): boolean ;
var
  FindHandle: THandle;
  FindData: TWin32FindData;
  CurName: string;
  MoreFlag: boolean;
begin
    FindHandle := 0 ;
    result := false ;
    FillChar(FindData, SizeOf(FindData), #0) ;
    LocDir := IncludeTrailingBackslash (LocDir) + '*.*' ;
    try
    try

    // loop through directory until a real name found
        FindHandle := Windows.FindFirstFile(Pchar(LocDir), FindData);
        MoreFlag := (FindHandle <> INVALID_HANDLE_VALUE);
        while MoreFlag do
        begin
            CurName := FindData.cFileName;
            if ((CurName <> '.') and (CurName <> '..')) then
            begin
                result := true ;
                MoreFlag := false
            end
            else
                MoreFlag := Windows.FindNextFile(FindHandle, FindData);
        end;
    except
        result := false;
    end;
    finally
        if FindHandle <> INVALID_HANDLE_VALUE then Windows.FindClose(FindHandle);
    end;
end;

// builds list of files in a directory and sub directories, optional search path
// Level and InitDLen should be 0, except when called recursively
// LocFiles array should be set to length zero, generally
// returns false for error or if cancelled from copyevent

// this version uses W32 FindFirstFile API rather than VCL FindFirst

function TMagFileCopy.BuildDirList2 (LocDir, LocPartName: string; const SubDirs: boolean;
                Level, InitDLen: integer ; const LoDT, HiDT: TDateTime ;
                                var TotFiles: integer; var LocFiles: TFileRecs;
                                                        const ListDirs: boolean = false) : boolean ;
var
    FindHandle: THandle;
    FindData: TWin32FindData;
    fullname, CurName: string;
    CurAttr: integer;
    LocalFileTime: TFileTime;
    CheckMatch, keepflag, succflag, MoreFlag, DateCheckFlag: boolean;
    localDT: TDateTime ;
    TempSize: ULARGE_INTEGER ; // 64-bit integer record  - Mar 2017 was TULargeInteger
    OldWow64: BOOL ;        // 22 May 2013

    procedure AddFiletoArray ;  // 9 Feb 2011 commonise code
    begin
        inc(TotFiles);
// see if allocating more array memory
        if Length(LocFiles) <= TotFiles then SetLength(LocFiles, TotFiles * 2);  // 11 Feb 2004, was Length

// keep details
        with LocFiles[pred(TotFiles)] do
        begin
            FrFileName := CurName;
            FrSubDirs := Copy(LocDir, InitDLen, 255);
            FrFullName := LocDir + CurName;
            FrDirLevel := Level;
            FrDirBaseLen := Pred (InitDLen);
            FrFileAttr := CurAttr;
            FrFileDT := localDT ;
            FrFileUDT := FileTimeToDateTime (FindData.ftLastWriteTime) ; // UTC time
            TempSize.LowPart := FindData.nFileSizeLow ;
            TempSize.HighPart := FindData.nFileSizeHigh ;
            FrFileBytes := TempSize.QuadPart ;
            FrExtra := '';
            if ((CurAttr and faDirectory) = faDirectory) then
            begin
                FrFileBytes := 0;
                FrExtra := sDirLit ;
            end;
            FrFileCopy := FCStateNone;
            FrLinks := '';
        end;
    end;

begin
    FindHandle := 0 ;
    if (Length (LocFiles) = 0) then SetLength (LocFiles, 1000) ;
    if fCancelFlag then
    begin
        result := false ;
        exit ;
    end ;
    result := true ;
    FillChar (FindData, SizeOf(FindData), #0) ;
    LocPartName := Trim (AnsiLowerCase (LocPartName)) ;
    LocDir := IncludeTrailingBackslash (LocDir) ;  // 7 Jan 2001, keep case
    if InitDLen = 0 then InitDLen := Length(LocDir) ;
    CheckMatch := true;
    DateCheckFlag := (LoDT <> 0) and (HiDT <> 0) ;
    if (LocPartName = '*.*') or (LocPartName = '') then CheckMatch := false;
    OldWow64 := false ; // 22 May 2013
    if fWow64RedirDisable then DisableWow64Redir (OldWow64) ; // 22 May 2013
    try
        try

    // loop through directory getting all file names in directory
            fullname := LocDir + '*.*';
            FindHandle := Windows.FindFirstFile(Pchar(fullname), FindData);
            MoreFlag := (FindHandle <> INVALID_HANDLE_VALUE);
            while MoreFlag do
            begin
                CurAttr := FindData.dwFileAttributes;
                CurName := FindData.cFileName;
                if ((CurName <> '.') and (CurName <> '..')) then
                begin
            // found another directory, recursively call this function to process it
                    if ((CurAttr and faDirectory) = faDirectory) then
                    begin
                        if ListDirs then  // 9 Feb 2011 keep directory names
                        begin
                            FileTimeToLocalFileTime (FindData.ftLastWriteTime, LocalFileTime);
                            localDT := FileTimeToDateTime (LocalFileTime) ;
                            AddFiletoArray ;
                        end ;
                        if SubDirs then
                        begin
                            succflag := BuildDirList2 (LocDir + CurName, LocPartName,
                                  SubDirs, succ(Level), InitDLen, loDT, hiDT, TotFiles, LocFiles, ListDirs) ;
                            if not succflag then exit;
                        end;
                    end
                    else
                    begin
                   // if file matches mask, add record with all file details to dynamic array
                        if CheckMatch then
                        begin
                            keepflag :=  MaskMatches (LocPartName, AnsiLowerCase (CurName), true) ;
                        end
                        else
                           keepflag := true;

                   // see if checking any time stamps
                        localDT := 0 ;
                        if keepflag then
                        begin
                            FileTimeToLocalFileTime (FindData.ftLastWriteTime, LocalFileTime);
                            localDT := FileTimeToDateTime (LocalFileTime) ;
                            if DateCheckFlag then
                            begin
                                if (localDT < LoDT) or (localDT > HiDT) then keepflag := false;
                            end ;
                        end ;

                   // keep file details in dynamic array
                        if keepflag then AddFiletoArray ;
                    end;
                end;
                MoreFlag := Windows.FindNextFile(FindHandle, FindData);
                if AppProcMessCount <= 0 then      // make sure applications remains responsive
                begin
                    MessagePump ; // 16 Sept 2010
                    AppProcMessCount := AppProcMessMax ;
                    if AppProgressCount <= 0 then
                    begin
                        doCopyEvent (LogLevelProg, 'Listing Files, Total Found ' + IntToCStr (TotFiles)) ; // 20 May 2013
                        if fCancelFlag then
                        begin
                            result := false ;
                            exit ;
                        end ;
                        AppProgressCount := AppProgressMax ;
                    end ;
                    dec (AppProgressCount)
                end ;
                dec (AppProcMessCount) ;
            end;
        except
            result := false;
        end;
    finally
        if FindHandle <> INVALID_HANDLE_VALUE then Windows.FindClose(FindHandle);
        if fWow64RedirDisable then RevertWow64Redir (OldWow64) ; // 22 May 2013
    end;
end;

// backward compatible version, no dates

function TMagFileCopy.BuildDirList (LocDir, LocPartName: string; const SubDirs: boolean;
                 Level, InitDLen: integer ; var TotFiles: integer;
                                        var LocFiles: TFileRecs ; const ListDirs: boolean = false) : boolean ;
begin
    fCancelFlag := false ;
    AppProgressCount := 0 ;
    result := BuildDirList2 (LocDir, LocPartName, SubDirs, Level, InitDLen, 0, 0, TotFiles, LocFiles, ListDirs) ;
end ;

// called by TFindList for sort and find comparison of file records - case insensitive

function CompareFNext (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
var
    Sort1, Sort2: string ;
begin
{ // 16 Sept 2010 had to remove this since TMagCopy object not available
    if AppProcMessCount <= 0 then      // make sure applications remains responsive
    begin
        MessagePump ; // 16 Sept 2010
        AppProcMessCount := AppProcMessMax ;
    end ;
    dec (AppProcMessCount) ;  }

// using fullname might be faster, ! as last path delim makes files sort before dirs
    Sort1 := PTFileRec (Item1).FrSubDirs + '!' + PTFileRec (Item1).FrFileName ;
    Sort2 := PTFileRec (Item2).FrSubDirs + '!' + PTFileRec (Item2).FrFileName ;
    result := CompareText (Sort1, Sort2) ;  // case insensitive
end ;

// builds sorted list of files in a directory and sub directories, optional search path
// returns total files, or -1 for error or cancelled from CopyEvent

function TMagFileCopy.GetDirList (LocDir, LocPartName: string; const SelOpt: TFileCopyType;
              const SubDirs: boolean; LoDT, HiDT: TDateTime ;
                    var LocFiles: TFileRecs; var LocFileList: TFindList; const ListDirs: boolean = false): integer ;
var
    I, totfiles: integer ;
    flag: boolean ;
begin
    fCancelFlag := false ;
    AppProgressCount := 0 ;
    if SubDirs then  // 7 Sept 2008 was 1000 more if subdirs, avoids too much copying
        SetLength (LocFiles, 25000)
    else
        SetLength (LocFiles, 5000) ;
    totfiles := 0 ;
    if NOT Assigned (LocFileList) then LocFileList := TFindList.Create ;
    LocFileList.Clear ;
    if (SelOpt = FCTypeAllDir) or (SelOpt = FCTypeArchDir) or (LocPartName = '') then LocPartName := '*.*' ;
    if (SelOpt <> FCTypeDates) or (LoDT >= HiDT) then
    begin
        LoDT := 0 ;
        HiDT := 0 ;
    end ;
    flag := BuildDirList2 (LocDir, LocPartName, SubDirs, 0, 0, LoDT, HiDT, totfiles, LocFiles, ListDirs) ;
    if NOT flag then
    begin
        SetLength (LocFiles, 0) ;
        result := -1 ;
        exit ;
    end ;
    result := totfiles ;
    SetLength (LocFiles, totfiles) ;
    if result = 0 then exit ;

// build list and sort it
    LocFileList.Capacity := totfiles ;
    for I := 0 to Pred (totfiles) do LocFileList.Add (@LocFiles [I]) ;
    LocFileList.Sort (CompareFNext) ;
    LocFileList.Sorted := true ; // 11 June 2008 not sure if really needed
end ;

// build displayable file directory

function TMagFileCopy.DispLocFiles (const SrcDir, SrcFile: string ; const CopyType:
        TFileCopyType ; const SubDirs: boolean; const UTCFlag: boolean;
                                        const ListDirs: boolean = false): string ;
var
    DirSrcFiles: TFileRecs ;
    DirSrcFileList: TFindList ;
    totfiles: integer ;
begin
    result := '' ;
    fCancelFlag := false ;
    DirSrcFileList := TFindList.Create ;
    try
        totfiles := GetDirList (SrcDir, SrcFile, CopyType, SubDirs, 0, 0, DirSrcFiles, DirSrcFileList, ListDirs) ;
        if totfiles = -1 then
            result := 'Error Locating Files - ' + GetExceptMess (ExceptObject)
        else
            result := FmtFileDirList (DirSrcFileList, UTCFlag) ;

    finally
        DirSrcFileList.Free ;
        SetLength (DirSrcFiles, 0) ;
    end ;
end ;

function MagCheckReplace (const replopt: TFileCopyRepl;
           const IgnoreOldTime: boolean; const AllowDiffDT: TDateTime;
            const SrcSize, TarSize: Int64; SrcDT, TarDT: TDateTime): boolean ;
var
    realdiffDT: TDateTime ;
begin
    result := true ;
    if (replopt = FCReplAlways) then exit ;
    if (replopt = FCReplNever) then
    begin
         result := false ;
         exit ;
    end ;
    if ((replopt <> FCReplDiff) and (replopt <> FCReplNewer)) then exit ;

// see if FTP timestamp without time
    if IgnoreOldTime then
    begin
        if Frac (TarDT) = 0 then
            SrcDT := Int (SrcDT)
        else if Frac (SrcDT) = 0 then
            TarDT := Int (TarDT) ;
    end ;

// check time stamp difference, if close don't select
    if SrcDT >= TarDT then
        realdiffDT := SrcDT - TarDT
    else
        realdiffDT := TarDT - SrcDT ;

// replace if different date or size
    if (replopt = FCReplDiff) and (SrcSize = TarSize) then  // 18 July 2003
    begin
        if realdiffDT <= AllowDiffDT then result := false ; // 18 July 2003
    end
 // replace if newer date
    else if (replopt = FCReplNewer) then
    begin
        if realdiffDT <= AllowDiffDT then result := false
        else if (SrcDT <= TarDT) then result := false ;
     // unless target is empty (18 Aug 2004)
        if (SrcSize <> 0) and (TarSize = 0) then result := true ;
    end ;
end ;

// select files to copy from one place to another
// also used to select files not in target directory to delete

function TMagFileCopy.SelectCopyFileList (const SrcFileList, TarFileList: TFindList;
        const Fname: string; const Selopt: TFileCopyType; const Replopt: TFileCopyRepl;
            const DiffStamp: integer; const IgnoreOldTime: boolean; IgnoreExt: string ;
              var Skipped: integer; const UTCFlag: boolean; const IgnorePaths: String = ''): integer ;  // 16 May 2013 added IgnorePath
var
    I, J, TotSrc, TotTar, IgnorePathTot: integer ;
    srchBytes: int64 ;
    srchDT, oldDT, realdiffDT, allowdiffDT: TDateTime ;
    compname, extn: string ;
    flag, xlatsrc, xlattar: boolean ;
    SrcFileRec, TarFileRec: PTFileRec ;
    SrchFileRec: TFileRec ;
    NewSelopt: TFileCopyType ;
    IgnorePathList: StringArray ;    // 16 May 2013
begin
    result := 0 ;
    skipped := 0 ;
    AppProgressCount := 0 ;
//    fLastProgTick := GetTickCountX ; // 22 Oct 2008 ensure progress displayed  - 20 May 2013 never used
    fCancelFlag := false ;
    totSrc := SrcFileList.Count ;
    totTar := TarFileList.Count ;
    if totSrc = 0 then exit ;
    IgnoreExt := AnsiLowerCase (IgnoreExt) ;  // list, ie tmp;bak
    compname := AnsiLowerCase (trim (fname)) ;
    NewSelopt := Selopt ;
    if compname = '*.*' then NewSelopt := FCTypeAllDir ; // 9 Feb 2011 faster and allow files without an extension to be selected
    if DiffStamp = -1 then
        allowdiffDT := OneSecond * 2 // allow for errors on file time stamps
    else
        allowdiffDT := MinsToTime (DiffStamp) ;
    IgnorePathList := StrArraySplit (AnsiLowerCase (IgnorePaths), ';') ;   // not sorted!!   // 16 May 2013
    IgnorePathTot := Length (IgnorePathList) ;       // 16 May 2013

// test first file in both lists to see if path translation needed
    SrcFileRec := SrcFileList [0] ;
    xlatsrc := (Pos ('/', SrcFileRec^.FrFullName) > 0) ;
    if totTar <> 0 then
    begin
        TarFileRec := TarFileList [0] ;
        xlattar := (Pos ('/', TarFileRec^.FrFullName) > 0) ;
        if xlattar and xlatsrc then   // if both UNIX, don't translate
        begin
            xlatsrc := false ;
            xlattar := false ;
        end
    end
    else
        xlattar := false ;

// check each source file to see if it's being copied
    try
    for I := 0 to Pred (totSrc) do
    begin
        if AppProcMessCount <= 0 then      // make sure applications remains responsive
        begin
            MessagePump ; // 16 Sept 2010
            AppProcMessCount := AppProcMessMax ;
            if AppProgressCount <= 0 then
            begin
                doCopyEvent (LogLevelProg, 'Checking Files, Done ' + IntToCStr (I)) ;  // 20 May 2013
                if fCancelFlag then
                begin
                    result := 0 ;
                    exit ;
                end ;
                AppProgressCount := AppProgressMax ;
            end ;
            dec (AppProgressCount) ;
        end ;
        dec (AppProcMessCount) ;
        flag := false ;
        SrcFileRec := SrcFileList [I] ;
        with SrcFileRec^ do
        begin
            if { (FrExtra <> sDirLit) and } (FrLinks = '') then    // 9 Feb 2011 don't ignore directories
            begin
                if NewSelopt = FCTypeAllDir then flag := true
                else if NewSelopt = FCTypeDates then flag := true
                else if NewSelopt = FCTypeSingle then
                begin
                    if FrFileName = compname then flag := true ;
                end
                else if NewSelopt = FCTypeMaskDir then
                begin
                    if MaskMatches (compname, AnsiLowerCase (FrFileName), true) then flag := true ;
                end
                else if NewSelopt = FCTypeArchDir then
                begin
                    if (FrFileAttr AND File_Attribute_Archive) > 0 then flag := true ;
                end ;

           // ignore files with specific extensions - list is tmp;ftp;xxx;etc
                if flag and (Length (IgnoreExt) <> 0) then
                begin
                    extn := AnsiLowerCase (ExtractFileExt (FrFileName)) ;
                    if Length (extn) >= 4 then
                    begin
                        if Pos (Copy (extn, 2, 99), IgnoreExt) > 0 then flag := false ;
                    end ;
                end ;

          // ignore files where source has specific partial path, list is c:\temp;c:\temp2;c:\temp3;etc
                if flag and (IgnorePathTot > 0) then  // 16 May 2013
                begin
                    for J := 0 to Pred (IgnorePathTot) do
                    begin
                        if Pos (IgnorePathList [J], AnsiLowerCase (FrFullName)) = 1 then flag := false ;
                    end;
                end;
            end ;
        end ;

   // see if file already exists and whether we can skip downloading it again
        if flag and (totTar <> 0) and (replopt <> FCReplAlways) then
        begin
            SrchFileRec := SrcFileRec^ ;   // copy record to see can mess with it
            with SrchFileRec do
            begin
                if UTCFlag then
                    srchDT := FrFileUDT
                else
                    srchDT := FrFileDT ;
                srchBytes := FrFileBytes ;
            end ;
        // allow for comparing DOS and UNIX file paths
            if xlatsrc then SrchFileRec.FrSubDirs := UnixPathToDosPath (SrchFileRec.FrSubDirs) ;
            if xlattar then SrchFileRec.FrSubDirs := DosPathToUnixPath (SrchFileRec.FrSubDirs) ;
            if TarFileList.Find (@SrchFileRec, CompareFNext, J) then
            begin
               // check if we can ignore the file
                TarFileRec := TarFileList [J] ;
                with TarFileRec^ do
                begin
                 // 9 Feb 2011 directory exists, can not replace it
                    if ((FrFileAttr and faDirectory) = faDirectory) then
                        flag := false
                 // never replace
                    else if (replopt = FCReplNever) then
                        flag := false
                    else
                    begin
                        if ((replopt = FCReplDiff) or (replopt = FCReplNewer)) then // 18 July 2003
                        begin
                       // see if FTP timestamp without time
                            if UTCFlag then
                                oldDT := FrFileUDT
                            else
                                oldDT := FrFileDT ;
                            if IgnoreOldTime then
                            begin
                                if Frac (oldDT) = 0 then
                                    srchDT := Int (srchDT)
                                else if Frac (srchDT) = 0 then
                                    oldDT := Int (oldDT) ;
                            end ;

                        // check time stamp difference, if close don't select
                            if srchDT >= oldDT then
                                realdiffDT := srchDT - oldDT
                            else
                                realdiffDT := oldDT - srchDT ;

                        // replace if different date or size
                            if (replopt = FCReplDiff) and (srchBytes = FrFileBytes) then  // 18 July 2003
                            begin
                                if realdiffDT <= allowdiffDT then flag := false ; // 18 July 2003
                            end
                         // replace if newer date
                            else if (replopt = FCReplNewer) then
                            begin
                                if realdiffDT <= allowdiffDT then flag := false
                                else if (srchDT <= oldDT) then flag := false ;
                             // unless target is empty (18 Aug 2004)
                                if (srchBytes <> 0) and (FrFileBytes = 0) then flag := true ;
                            end ;
                        end ;
                    end ;
                end ;
            end ;
            if NOT flag then inc (skipped) ;
        end ;
        if flag then
        begin
            SrcFileRec^.FrFileCopy := FCStateSelect ;  // yes, we want file
            inc (result) ;
        end ;
    end ;
    doCopyEvent (LogLevelProg, '') ;
    finally
   //      CMask.Free ;
    end ;
end ;

// called by Windows CopyFileEx API

function CopyProgressRoutine (TotalFileSize, TotalBytesTransferred, StreamSize,
    StreamBytesTransferred: Int64; dwStreamNumber, dwCallbackReason: DWORD;
       hSourceFile, hDestinationFile: THandle; lpData: Pointer): DWORD ; stdcall;
var
    xCancelFlag: boolean ;
    Obj: TObject;
    info: String ;
begin
    result := PROGRESS_CONTINUE ;
    xCancelFlag := false ;
    Obj := lpData ;      // 16 Sept 2010
    TMagFileCopy (Obj).MessagePump ; // 16 Sept 2010
    if (dwCallbackReason = CALLBACK_CHUNK_FINISHED) or
                (dwCallbackReason = CALLBACK_STREAM_SWITCH) then  // 16 May 2013 switch is first call
    begin
        try
            with TMagFileCopy (Obj) do
            begin
                fCopyProg.CurFileBytes := TotalFileSize ;     // 20 May 2013
                fCopyProg.CurOKDone := TotalBytesTransferred ;
                fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + TotalBytesTransferred ;
                if (DiffTicks (fCopyProg.LastProgTick, GetTickCountX) >=
                            (LongWord (fProgressSecs) * TicksPerSecond)) or (fCopyProg.LastProgTick = 0) or
                                                                   (TotalBytesTransferred >= TotalFileSize) then
                begin
                    info := fCopyProg.ProgMessBase + ', ' + IntToKByte (TotalBytesTransferred) +
                                                                     ' of ' + IntToKByte (TotalFileSize) ;
                    fCopyProg.LastProgTick := GetTickCountX ;
                    if Assigned (fCopyEvent) then
                                  fCopyEvent (LogLevelProg, info, xCancelFlag) ;
                    if Assigned (fProgressEvent) then   // 22 May 2013
                    begin
                        CopyProgDuration (fCopyProg, AppTicksPerFile) ;
                        fCopyProg.LogLevel := LogLevelProg ;
                        fCopyProg.Info := info ;
                        fProgressEvent (Obj, fCopyProg, xCancelFlag) ;
                    end;
                end;
             end ;
        except
        end ;
    end ;
    if xCancelFlag then result := PROGRESS_CANCEL ;
end ;

procedure TMagFileCopy.doCopyEvent (const LogLevel: TLogLevel ; const Info: string) ;
begin
    if Assigned (fCopyEvent) then fCopyEvent (LogLevel, Info, fCancelFlag) ;
    if Assigned (fProgressEvent) then   // 22 May 2013
    begin
        fCopyProg.LogLevel := LogLevel ;
        fCopyProg.Info := Info ;
        fProgressEvent (Self, fCopyProg, fCancelFlag) ;
    end;
end ;

procedure TMagFileCopy.EndUnZipEvent (Sender: TObject; FileIndex: Integer; FName: String) ;
var
    newsize: int64 ;
begin
    if FName = '' then exit ;
    newsize := GetSize64File (Fname) ;
    doCopyEvent (LogLevelFile, 'Unzipped OK: ' + Fname + ', size: ' + IntToKByte (newsize, true)) ;
    doCopyEvent (LogLevelDelimFile, 'Unzipped|' + Fname + '|' + IntToStr (newsize) + '|1|0|OK|0|0') ;
end ;

procedure TMagFileCopy.UnZipHandleMessage(Sender: TObject; const MessageID: Integer;
                    const Msg1, Msg2: String; const flags: Cardinal; var Return: Integer);
begin
    doCopyEvent (LogLevelFile, 'Fatal Unzip Error: ' + Msg1) ;
    Return := 0 ;
end;

// get lists of files to copy

function TMagFileCopy.SelectFiles (const CurSrcDir, CurTarDir: string): TTaskResult ;
var
    newfname, newsrcdir: string ;
    nodeltot: integer ;
begin
    ClearTarList ;
    ClearSrcList ;
    CurProcFiles := 0 ;
    CurSkippedFiles := 0 ;
    CurDelFiles := 0 ;
    doCopyEvent (LogLevelInfo, 'Locating Files to Copy' + CRLF_ +
                                    'Source: ' + CurSrcDir + CRLF_ + 'Target: ' + CurTarDir) ;

// don't delete target files unless processing full directories
    if fDelOldTar then
    begin
        if NOT (fCopyType in [FCTypeArchDir, FCTypeAllDir, FCTypeDates]) then fDelOldTar := false ;
    end ;

// build list of source files
//    doCopyEvent (LogLevelDiag, 'GetSrcDir=' + Int64ToCStr (GetTickCount)) ; // DIAG
    newsrcdir := CurSrcDir ;
    newfname := '*.*' ;
    if fCopyType in [FCTypeMaskDir, FCTypeDates] then
    begin
        if fMultiDir then
        begin
            newfname := ExtractFileName (CurSrcDir) ;   // check found a file name
            if (Pos ('.', newfname) > 1) and (Pos ('*', newfname) > 0) then
                newsrcdir := ExtractFilePath (CurSrcDir)
            else
                newfname := '*.*' ;
        end
        else
            newfname := fSrcFName ;   /// partial masked directory
    end ;
    if (NOT fMultiDir) and fMask then newfname := '*.*' ;  // date mask
    doCopyEvent (LogLevelProg, 'Listing Files') ;
    TotSrcFiles := GetDirList (newsrcdir, newfname, fCopyType, fSubDirs,
                                         fCopyLoDT, fCopyHiDT, SrcFiles, SrcFileList, fEmptyDirs) ; // 8 Feb 2011
    doCopyEvent (LogLevelProg, '') ;
    MessagePump ; // 16 Sept 2010
    if fCancelFlag then
    begin
        result := TaskResAbort ;
        exit ;
    end ;
    if TotSrcFiles = -1  then
    begin
        result := TaskResFail ;
        fReqResponse := 'Error Locating Source Files - ' + GetExceptMess (ExceptObject) ;
        exit ;
    end ;
    if TotSrcFiles <= 0  then
    begin
        result := TaskResOKNone ;
        fReqResponse := 'No Source Files Found' ;
        exit ;
    end ;

// build list of target files, unless replacing everything or deleting old stuff
    if (fRepl <> FCReplAlways) or fDelOldTar then
    begin
//        doCopyEvent (LogLevelDiag, 'GetTarDir=' + Int64ToCStr (GetTickCount)) ; // DIAG
        doCopyEvent (LogLevelProg, 'Listing Files') ;
        TotTarFiles := GetDirList (CurTarDir, '*.*', fCopyType,
                                               fSubDirs, 0, 0, TarFiles, TarFileList, fEmptyDirs) ; // 8 Feb 2011
        doCopyEvent (LogLevelProg, '') ;
        MessagePump ; // 16 Sept 2010
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            exit ;
        end ;
        if TotTarFiles = -1  then
        begin
            result := TaskResFail ;
            fReqResponse := 'Error Locating Target Files - ' + GetExceptMess (ExceptObject) ;
            exit ;
        end ;
    end ;

// compare source and target files, see what to copy
    if NOT fMultiDir then
    begin
        if fMask then
            newfname := GetMaskedName (fSrcFName, fPrev, fLocalHost)
        else if (fCopyType = FCTypeSingle) then
            newfname := fSrcFName ;
    end ;
//    doCopyEvent (LogLevelDiag, 'CompDirs=' + Int64ToCStr (GetTickCount)) ; // DIAG
    CurProcFiles := SelectCopyFileList (SrcFileList, TarFileList, newfname,
               fCopyType, fRepl, -1, false, fIgnoreFileExt, CurSkippedFiles, fUseUTC, fIgnorePaths) ;  // 22 May 2013 added IgnorePaths
//    doCopyEvent (LogLevelDiag, 'DirsDone=' + Int64ToCStr (GetTickCount)) ; // DIAG
    doCopyEvent (LogLevelProg, '') ;
    MessagePump ; // 16 Sept 2010
    if fCancelFlag then
    begin
        result := TaskResAbort ;
        exit ;
    end ;

// see if deleting old target files no longer in source directories
    if fDelOldTar and (TotTarFiles <> 0) then
    begin
        CurDelFiles := SelectCopyFileList (TarFileList, SrcFileList, '*.*',
             FCTypeAllDir, FCReplNever, 0, false, fIgnoreFileExt, nodeltot, fUseUTC, fIgnorePaths) ;  // 22 May 2013 added IgnorePaths
    end ;
    doCopyEvent (LogLevelProg, '') ;
    if (CurProcFiles = 0) then
    begin
        result := TaskResOKNone ;
        fReqResponse := 'No Source Files Selected to Copy' ;
        exit ;
    end ;
    Result := TaskResOKNew ;
end ;

function TMagFileCopy.CopyOneFile (const Fnamesrc, Fnametar: string ;
                Replopt: TFileCopyRepl; const Safe: boolean; var Fsize: Int64) : TTaskResult ;
var
    fnamecopy, newdir, code: string ;
    ret, flag: boolean ;
    retval: integer ;
    duration: longword ;
    SrcFSize, TarFSize: Int64 ;
    SrcFileDT, TarFileDT: TDateTime;
    WideFnamesrc, WideFnamecopy: array [0..MAX_PATH] of WideChar ;   // Unicode
    OldWow64: BOOL ;        // 22 May 2013
begin
    result := TaskResFail ;
    Fsize := -1 ;
    CopyProgClearAll (fCopyProg) ;  // 22 May 2013 clear all progress stuff
    OldWow64 := false ; // 22 May 2013
    if fWow64RedirDisable then DisableWow64Redir (OldWow64) ; // 22 May 2013
    try  // finally

// check source exists
    if NOT GetUAgeSizeFile (Fnamesrc, SrcFileDT, SrcFSize) then
    begin
        doCopyEvent (LogLevelInfo, 'Copy Failed: ' + fnamesrc + ' - File Not Found') ;
        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Copy Failed: File Not Found|0|0') ;
        exit ;
    end ;

// create target directory
    fnamecopy := Fnametar ;
    if Safe then
        fnamecopy := ChangeFileExt (Fnametar, '.TMP')
    else
        fnamecopy := Fnametar ;
    newdir := ExtractFileDir (fnamecopy) ;
    if (Length (newdir) = 0) or (NOT ForceDirs (newdir)) then
    begin
        doCopyEvent (LogLevelInfo, 'Can Not Create Directory: ' + newdir) ;
        exit ;
    end ;

// see if replacing existing file
    if GetUAgeSizeFile (Fnametar, TarFileDT, TarFSize) and (replopt <> FCReplAlways) then
    begin
        flag := MagCheckReplace (replopt, false, OneSecond * 2, SrcFSize, TarFSize, SrcFileDT, TarFileDT) ;
        if NOT flag then
        begin
            result := TaskResOKNone ;
            doCopyEvent (LogLevelInfo, 'Copy Skipped: ' + fnamesrc + code) ;
            doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Copy Skipped|0|0') ;
            exit ;
        end ;
    end ;

// try and delete existing file before copying, removing read only if necessary
    retval := MagDeleteFile (fnamecopy, fReplRO) ;
    if retval = 1 then
    begin
        doCopyEvent (LogLevelInfo, 'Can Not Replace Read Only File: ' + fnametar) ;
        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Can Not Replace Read Only File|0|0') ;
        exit ;
    end
    else if retval > 1 then
    begin
        doCopyEvent (LogLevelInfo, 'Delete File Failed: ' + fnametar + ' - ' + SysErrorMessage (retval)) ;
        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Delete File Failed|0|0') ;
        exit ;
    end ;

// 16 May 2013 prepare current file progress info
//    fLastProgTick := GetTickCountX - (LongWord (fProgressSecs + 1) * TicksPerSecond) ; // 22 Oct 2008 ensure progress displayed
    fCopyProg.CurStartTick := GetTickCountX ;
    fCopyProg.SessStartTick := GetTickCountX ;
    fCopyProg.CurSrcName := fnamesrc ;
    fCopyProg.CurTarName := fnametar ;
    fCopyProg.CurFileBytes := SrcFSize ;
    fCopyProg.TotProcBytes := SrcFSize ;
    fCopyProg.TotDoneNr := 1 ;
    fCopyProg.TotProcFiles := 1 ;
    fCopyProg.ProcBytesDone := 0 ;
    fCopyProg.ProcBytesLast := fCopyProg.ProcBytesDone ;
    fCopyProg.ProgMessBase := 'Copying ' + fnamesrc  + ' to ' + fnametar ;
    doCopyEvent (LogLevelFile, fCopyProg.ProgMessBase) ;  // 16 May 2013 was earlier

// copy file and it's attributes
    inc (fCopyProg.TotDoneNr) ;
    ret := CopyFileExW (StringToWideChar (fnamesrc, WideFnamesrc, MAX_PATH),
                    StringToWideChar (fnamecopy, WideFnamecopy, MAX_PATH),
                                        Pointer (@CopyProgressRoutine), Pointer (Self), Nil, 0) ;  // Unicode
    duration := ElapsedTicks (fCopyProg.CurStartTick) ;
    doCopyEvent (LogLevelProg, '') ;  // clear progress display
    CopyProgClearCur (fCopyProg) ;  // 16 May 2013 clear current since copying done
    CopyProgDuration (fCopyProg, AppTicksPerFile) ;
    if fCancelFlag then
    begin
        result := TaskResAbort ;
        exit ;
    end ;
    if NOT ret then
    begin
        doCopyEvent (LogLevelInfo, 'Copy Failed: ' + fnametar + ' - ' +  SysErrorMessage (GetLastError)) ;
        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Copy Failed: ' + SysErrorMessage (GetLastError)+ '|0|0') ;
        exit ;
    end ;

// check new file same size, if not see if original has changed size
    if duration = 0 then duration := 10 ;  // special case of copy OK but duration too short to measure
    ret := GetUAgeSizeFile (fnamecopy, TarFileDT, TarFSize) ;
    if (NOT ret) or (SrcFSize <> TarFSize) then
    begin
        Fsize := -1 ;
        doCopyEvent (LogLevelInfo, 'Copy Failed: ' + fnametar + ' - Size Mismatch') ;
        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Size Mismatch|0|0') ;
        exit ;
    end ;
    Fsize := TarFSize ;

// see if renaming temporary file
    if Safe then
    begin
        retval := MagRenameFile (fnamecopy, fnametar, true, fReplRO) ;
        if retval <> 0 then
        begin
            if (retval = 1) then
            begin
                doCopyEvent (LogLevelInfo, 'Can Not Replace Read Only File: ' + fnametar) ;
                doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Can Not Replace Read Only File|0|0') ;
            end
            else
            begin
                doCopyEvent (LogLevelInfo, 'Final File Rename Failed: ' + fnametar + ' - ' +  SysErrorMessage (retval)) ;
                doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Final File Rename Failed: ' +
                                                                                SysErrorMessage (retval)+ '|0|0') ;
            end ;
            doCopyEvent (LogLevelInfo, 'File Copied as: ' + fnamecopy) ;
            exit ;
        end ;
    end ;

// copy OK
    result := TaskResOKNew ;
    fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + Fsize ;
    doCopyEvent (LogLevelFile, 'Copy Succeeded, size ' + IntToKByte (Fsize, true)) ;
    doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|' +
                                IntToStr (Fsize) + '|1|0|OK|' + IntToStr (duration) + '|' + IntToStr (Fsize)) ;
    finally
        if fWow64RedirDisable then RevertWow64Redir (OldWow64) ; // 22 May 2013
    end;
end ;

function TMagFileCopy.CopyFiles (const CurSrcDir, CurTarDir: string): TTaskResult ;
var
    fnamesrc, fnametar, fnamecopy, curdir, newdir, deldir, errmess: string ;
    ret: boolean ;
    I, J, {donenr,} oldFail, retval: integer ;
    newsize, srcfsize: int64 ;
    SrcFileRec, DelFileRec: PTFileRec ;
    DelDirSrcList, DelDirTarList: TStringList ;
    duration: longword ;
    OldWow64: BOOL ;        // 22 May 2013
    WideFnamesrc, WideFnamecopy: array [0..MAX_PATH] of WideChar ;   // Unicode
{$IFDEF Zipping}
    info: string ;
    VCLUnZip: TVCLUnZip ;
    VCLZip: TVCLZip ;
   {$ENDIF}

    procedure DeleteTempZip ;
    begin
    {$IFDEF Zipping}
        if fZipped and (fZipType in [TypeSrcAddX, TypeSrcReplX]) then
        begin
            if fnamesrc = VCLZip.ZipName then MagDeleteFile (fnamesrc, true) ;
        end ;
    {$ENDIF}
    end ;

begin
    result := TaskResOKNew ;
    DelDirSrcList := TStringList.Create ;
    DelDirSrcList.Sorted := true ;
    DelDirSrcList.Duplicates := dupIgnore ;
    DelDirTarList := TStringList.Create ;
    DelDirTarList.Sorted := true ;
    DelDirTarList.Duplicates := dupIgnore ;
{$IFNDEF VER130} // D5
    DelDirSrcList.CaseSensitive := false ;
    DelDirTarList.CaseSensitive := false ;
{$ENDIF}
    {$IFDEF Zipping}
    VCLUnZip := Nil ;
    VCLZip := Nil ;
    if fZipped then
    begin
        VCLZip := TVCLZip.Create (self) ;
        VCLUnZip := TVCLUnZip.Create (self) ;
        VCLUnZip.OnEndUnZip := EndUnZipEvent ;
        VCLZip.OnHandleMessage := UnZipHandleMessage ;
        VCLUnZip.OnHandleMessage := UnZipHandleMessage ;
    end ; {$ENDIF}
    OldWow64 := false ; // 22 May 2013
    if fWow64RedirDisable then DisableWow64Redir (OldWow64) ; // 22 May 2013
    try  // finally

// see if deleting old target files first
    if fDelOldTar and (CurDelFiles <> 0) then
    begin
        doCopyEvent (LogLevelInfo, 'Deleting Old Target Files: ' + CurTarDir) ;
        for I := 0 to Pred (TotTarFiles) do
        begin
            DelFileRec := TarFileList [I] ;
            with DelFileRec^ do
            begin
                if FrFileCopy = FCStateSelect then
                begin
                    if ((FrFileAttr and faDirectory) = faDirectory) then continue ;  // 8 Feb 2011 ignore directory
                    doCopyEvent (LogLevelFile, 'Deleting: ' + FrFullName) ;
                    retval := MagDeleteFile (FrFullName, true) ;
                    if retval <= 0 then
                    begin
                        doCopyEvent (LogLevelDelimFile, '|' + FrFullName + '|0|0|0|Old Target File Deleted|0|0') ;
                        inc (fCopyProg.DelOKBytes, FrFileBytes) ;
                        FrFileCopy := FCStateOK ;
                        inc (fCopyProg.DelOKFiles) ;

                     // add directory to list we'll try and delete later, 24 Apr 2003
                        deldir := AnsiLowerCase (Trim (FrSubDirs)) ;
                        if deldir <> '' then
                        begin
                            if NOT DelDirTarList.Find (deldir, J) then DelDirTarList.Add (deldir) ;
                        end ;
                    end
                    else
                    begin
                        doCopyEvent (LogLevelInfo, 'File Delete Failed: ' + FrFullName + ' - ' +  SysErrorMessage (retval)) ;
                        FrFileCopy := FCStateFailed ;
                        inc (fCopyProg.ProcFailFiles) ;
                    end ;
                    MessagePump ; // 16 Sept 2010
                    if fCancelFlag then exit ;
                end ;
            end ;
        end ;
    end ;
    ClearTarList ;  // don't need target list any more, get memory back
    MessagePump ; // 16 Sept 2010
    if fCancelFlag then
    begin
        result := TaskResAbort ;
        exit ;
    end ;

// ensure some minimal variables have been set
    if (TotSrcFiles <= 0) or (SrcFileList.Count <> TotSrcFiles) or (CurProcFiles = 0) then
    begin
        result := TaskResOKNone ;
        fReqResponse := 'No Files Selected to Copy' ;
        exit ;
    end ;

// make sure the destination is available
    if NOT ForceDirs (CurTarDir) then
    begin
        result := TaskResFail ;
        inc (fCopyProg.ProcFailFiles, CurProcFiles) ;
        fReqResponse := 'Can Not Create Destination Directory: ' + CurTarDir ;
        exit ;
    end ;
    doCopyEvent (LogLevelInfo, 'Copying Files:' + CRLF_ + 'Source: ' + CurSrcDir + CRLF_ + 'Target: ' + CurTarDir) ;

// start real copying
//    donenr := 0 ;
    curdir := '.,.,.' ;  // illegal directory
    oldfail := fCopyProg.ProcFailFiles ;
    for I := 0 to Pred (TotSrcFiles) do
    begin
        MessagePump ; // 16 Sept 2010
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            exit ;
        end ;
        CopyProgClearCur (fCopyProg) ;  // 22 May 2013 clear current about to start a file
        SrcFileRec := SrcFileList [I] ;
        with SrcFileRec^ do
        begin
            if FrFileCopy <> FCStateSelect then continue ;
            inc (fCopyProg.TotDoneNr) ;
            fnamesrc := FrFullName ;  // could build from CurSrcDir, FrSubDirs and FrFileName
            if FrSubDirs = '' then FrSubDirs := '\' ;
            if ((FrFileAttr and faDirectory) = faDirectory) then   // 8 Feb 2011 create directory, might be empty with no files
            begin
                curdir := '.,.,.' ;  // 8 July 2011, illegal directory to ensure newdir reset
                newdir := CurTarDir + FrSubDirs + FrFileName + '\' ;
                if (NOT ForceDirs (newdir)) then
                begin
                    inc (fCopyProg.ProcFailFiles) ;
                    FrFileCopy := FCStateFailed ;
                    doCopyEvent (LogLevelInfo, 'Can Not Create Directory: '+ newdir + ' - ' + FormatLastError) ;
                    continue ;
                end ;
            //    if ((FrFileAttr and faReadOnly) <> 0) then
                FrFileCopy := FCStateOK ;
                inc (fCopyProg.ProcOKFiles) ;
                doCopyEvent (LogLevelFile, 'Created Directory OK: ' + newdir) ;
                doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + newdir + '|0|1|0|OK|0|0') ;
                retval := FileSetAttr (newdir, FrFileAttr) ;
                if retval > 0 then doCopyEvent (LogLevelInfo,
                            'Failed to Set Directory Attributes: '+ newdir + ' - ' + SysErrorMessage (retval)) ;
                continue ;
            end ;
            if (curdir <> FrSubDirs) then
            begin
                curdir := FrSubDirs ;
                if curdir [1] <> '\' then
                    newdir := CurTarDir + '\' + curdir
                else
                    newdir := CurTarDir + curdir ;
                if (Length (newdir) = 0) or (NOT ForceDirs (newdir)) then
                begin
                    inc (fCopyProg.ProcFailFiles) ;
                    FrFileCopy := FCStateFailed ;
                    doCopyEvent (LogLevelInfo, 'Can Not Create Directory: '+ newdir + ' - ' + FormatLastError) ;
                    continue ;
                end ;
            end ;
            fnametar := newdir + FrFileName ;
            srcfsize := FrFileBytes ;

        // see if zipping file
            {$IFDEF Zipping}
            if fZipped and (fZipType in [TypeSrcAddX, TypeSrcReplX]) and
                    (AnsiLowerCase (ExtractFileExt (fnamesrc)) <> '.zip') then
            begin
                With VCLZip do
                begin
                    doCopyEvent (LogLevelFile, 'Compressing ' + fnamesrc) ;
                    ZipName := IncludeTrailingBackslash (sysTempPath) + FrFileName ;
                    FilesList.Clear ;
                    FilesList.Add (fnamesrc) ;  // zip all file in directory */
                    ZipComment := 'Zipped by Sync Files Task' ;
                    if Zip <> 1 then    // this zips files into an archive in zipname file */
                    begin
                        doCopyEvent (LogLevelFile, 'Zip Compression Failed ' + fnamesrc) ;
                    end
                    else
                    begin
                        srcfsize := ZipSize ;
                        if fZipType = TypeSrcReplX then
                        begin
                            fnametar := ChangeFileExt (fnametar, '.zip') ;
                        end
                        else
                        begin
                            fnametar := fnametar + '.zip' ;
                        end ;
                        doCopyEvent (LogLevelFile, 'Compressed ' + fnamesrc + ' from ' +
                                            IntToKByte (FrFileBytes, true)  + ' to ' + IntToKByte (srcfsize, true)) ;
                        fnamesrc := ZipName ;
                    end ;
                end ;
            end ;  {$ENDIF}

        // safe copying, will rename later
            FrFileCopy := FCStateCopying ;
            fnamecopy := fnametar ;
            if fSafe then fnamecopy := ChangeFileExt (fnametar, '.TMP') ;

        // try and delete existing file before copying, removing read only if necessary
            ret := true ;
            retval := MagDeleteFile (fnamecopy, fReplRO) ;
            if retval > 0 then
            begin
                if retval = 1 then
                begin
                    doCopyEvent (LogLevelInfo, 'Can Not Replace Read Only File: ' + fnametar) ;
                    doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Can Not Replace Read Only File|0|0') ;
                end
                else
                begin
                    doCopyEvent (LogLevelInfo, 'Delete File Failed: ' + fnametar + ' - ' + SysErrorMessage (retval)) ;
                    doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Delete File Failed|0|0') ;
                end ;
                FrFileCopy := FCStateFailed ;
                inc (fCopyProg.ProcFailFiles) ;
                continue ;
            end ;
            duration := 0 ;
            if ret then
            begin
            // 22 May 2013 prepare current file progress info
//                fLastProgTick := GetTickCountX - (LongWord (fProgressSecs + 1) * TicksPerSecond) ; // 22 Oct 2008 ensure progress displayed
                fCopyProg.CurStartTick := GetTickCountX ;
                fCopyProg.CurSrcName := fnamesrc ;
                fCopyProg.CurTarName := fnametar ;
                fCopyProg.CurFileBytes := SrcFSize ;
                fCopyProg.ProcBytesLast := fCopyProg.ProcBytesDone ;
                fCopyProg.ProgMessBase := 'Copying ' + IntToCStr (fCopyProg.TotDoneNr) + ' of ' +
                                    IntToCStr (fCopyProg.TotProcFiles) + ' - ' + fnamesrc  + ' to ' + fnametar ;
                doCopyEvent (LogLevelFile, fCopyProg.ProgMessBase) ;

            // actual file copy
                ret := CopyFileExW (StringToWideChar (fnamesrc, WideFnamesrc, MAX_PATH),
                                StringToWideChar (fnamecopy, WideFnamecopy, MAX_PATH),
                                             Pointer (@CopyProgressRoutine), Pointer (Self), Nil, 0) ;  // Unicode
                duration := ElapsedTicks (fCopyProg.CurStartTick) ;
            end ;
            doCopyEvent (LogLevelProg, '') ;  // clear progress display
            CopyProgClearCur (fCopyProg) ;  // 16 May 2013 clear current since copying done
            CopyProgDuration (fCopyProg, AppTicksPerFile) ;
            if NOT ret then
            begin
                if Length (fnamesrc) > 259 then  // 20 Oct 2011 explict file name length errors
                    errmess := 'Source file name too long ' + IntToCStr (Length (fnamesrc))
                else if Length (fnametar) > 259 then
                    errmess := 'Target file name too long ' + IntToCStr (Length (fnametar))
                else
                    errmess := SysErrorMessage (GetLastError) ;
                doCopyEvent (LogLevelInfo, 'Copy Failed: ' + fnametar + ' - ' +  errmess) ;
                doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Copy Failed: ' + errmess + '|0|0') ;
                inc (fCopyProg.ProcFailFiles) ;
                fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + SrcFSize ;
                FrFileCopy := FCStateFailed ;
                DeleteTempZip ;
                continue ;
            end ;

        // check new file same size, if not see if original has changed size
            if duration = 0 then duration := 10 ;  // special case of copy OK but duration too short to measure
            newsize := GetSize64File (fnamecopy) ;
            if newsize <> srcfsize then
            begin
                if newsize <> GetSize64File (fnamesrc) then
                begin
                    doCopyEvent (LogLevelInfo, 'Copy Failed: ' + fnametar + ' - Size Mismatch') ;
                    doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Size Mismatch|0|0') ;
                    inc (fCopyProg.ProcFailFiles) ;
                    fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + SrcFSize ;
                    FrFileCopy := FCStateFailed ;
                    DeleteTempZip ;
                    continue ;
                end ;
            end ;

        // see if renaming temporary file
            if fSafe then
            begin
                retval := MagRenameFile (fnamecopy, fnametar, true, fReplRO) ;
                if (retval > 0) then
                begin
                    if retval = 1 then
                    begin
                        doCopyEvent (LogLevelInfo, 'Can Not Replace Read Only File: ' + fnametar) ;
                        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Can Not Replace Read Only File|0|0') ;
                    end
                    else
                    begin
                        doCopyEvent (LogLevelInfo, 'Final File Rename Failed: ' + fnametar + ' - ' +  SysErrorMessage (retval)) ;
                        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|0|0|1|Final File Rename Failed: ' +
                                                                                        SysErrorMessage (retval)+ '|0|0') ;
                    end ;
                    doCopyEvent (LogLevelInfo, 'File Copied as: ' + fnamecopy) ;
                    FrFileCopy := FCStateFailed ;
                    inc (fCopyProg.ProcFailFiles) ;
                    fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + SrcFSize ;
                    DeleteTempZip ;
                    continue ;
                end ;
            end ;

       // copy OK
            inc (fCopyProg.ProcOKBytes, newsize) ;
            fCopyProg.ProcBytesDone := fCopyProg.ProcBytesLast + newsize ;
            FrFileCopy := FCStateOK ;
            inc (fCopyProg.ProcOKFiles) ;
            doCopyEvent (LogLevelFile, 'Copy Succeeded, size ' + IntToKByte (newsize, true)) ;
            doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar + '|' +
                                        IntToStr (newsize) + '|1|0|OK|' + IntToStr (duration) + '|' + IntToStr (newsize)) ;

       // see if unzipping it
           {$IFDEF Zipping}
            if fZipped and (fZipType = TypeUnzip) and (AnsiLowerCase (ExtractFileExt (fnametar)) = '.zip') then
            begin
                with VCLUnZip do
                begin
                    ZipName := fnametar ;    // set the zip filename
                    ReadZip;                 // open it and read its information

               // check it's not corrupted and not empty
                    if CheckArchive and (Count > 0) then
                    begin

               // List filenames in zip file
                        info := '' ;
                        if ZipHasComment then info := ZipComment + CRLF ;
                        for J := 0 to Pred (Count) do
                        begin
                           info := info + Format (sDirLine, [Filename [J], IntToKByte (UnCompressedSize [J], true), ' ',
                                           DateToStr (DateTime [J]) + ' ' + TimeToStr (DateTime [J]), Pathname[J]]) + CRLF ;
                        end ;
                        doCopyEvent (LogLevelInfo, 'Unzipping Files:' + CRLF + info) ;

                     // extract all files
                        FilesList.Clear ;
                        DoAll := true ;
                        if (fZipDir = '') and (fZipPath >= PathSpecific) then fZipPath := PathNew ;
                        DestDir := ExtractFileDir (fnametar) ;     // Set destination directory
                        RecreateDirs := false ;
                        RootDir := '' ;   // base subdirectory
                        if fZipPath in [PathOriginal, PathNewOrig, PathSpecOrig] then RecreateDirs := true ;
                        if fZipPath in [PathNew, PathNewOrig] then
                                             DestDir := ExtractFileDir (fnametar) + '\' + ExtractNameOnly (fnametar) ;
                        if fZipPath >= PathSpecific then DestDir := fZipDir ;
                        if NOT ForceDirs (DestDir) then
                        begin
                            doCopyEvent (LogLevelFile, 'Failed to Create Unzip Dir: ' + DestDir) ;
                            doCopyEvent (LogLevelDelimFile, fnametar + '|' + DestDir + '|0|0|1|Failed to Create Unzip Dir|0|0') ;
                            continue ;
                        end ;
                        ReplaceReadOnly := fReplRO ;
                        OverwriteMode := Always ;
                        RetainAttributes := True ;  // Set attributes to original after unzipping
                        J := Unzip;      // Extract files, return value is the number of files actually unzipped
                        if J = Count then
                        begin
                            doCopyEvent (LogLevelFile, 'Unzipped OK: ' + fnametar) ;
                            if fZipDownDel then
                            begin
                                doCopyEvent (LogLevelFile, 'Deleting: ' + fnametar) ;
                                MagDeleteFile (fnametar, true) ;
                                doCopyEvent (LogLevelDelimFile, fnametar + '| |0|0|0|File Deleted After Unzipping|0|0') ;
                            end ;
                        end
                        else
                        begin
                            doCopyEvent (LogLevelFile, 'Failed to Unzip: ' + fnametar) ;
                            doCopyEvent (LogLevelDelimFile, fnametar + '|' + DestDir + '|0|0|1|Failed to Unzip File|0|0') ;
                            ClearZip;
                            continue ;
                        end ;
                    end
                    else
                    begin
                        doCopyEvent (LogLevelInfo, 'Zip File Corrupted:' + fnametar) ;
                        doCopyEvent (LogLevelDelimFile, fnametar + '|' + DestDir + '|0|0|1|Zip File Corrupted|0|0') ;
                        ClearZip;
                        continue ;
                    end ;
                    ClearZip;
                end ;
            end ;  {$ENDIF}
            DeleteTempZip ;  // if zipped before copy

        // see if deleting source file
            if fDelDone then
            begin
                retval := MagDeleteFile (FrFullName, fReplRO) ;     // 30 Jan 2003, was fnamesrc
                if retval = 0 then
                begin
                    doCopyEvent (LogLevelDelimFile, FrFullName + '| |0|0|0|Source File Deleted After Copy|0|0') ;
                    deldir := AnsiLowerCase (Trim (FrSubDirs)) ;
                    if deldir <> '' then
                    begin
                        if NOT DelDirSrcList.Find (deldir, J) then DelDirSrcList.Add (deldir) ;
                    end ;
                end
                else
                begin
                    doCopyEvent (LogLevelFile, 'Failed to Delete File - ' + SysErrorMessage (retval)) ;
                    doCopyEvent (LogLevelDelimFile, FrFullName + '| |0|0|0|Source File Delete Failed - ' + SysErrorMessage (retval) + '|0|0') ;
                    continue ;
                end ;
            end ;
        end ;
    end ;
    if oldfail <> fCopyProg.ProcFailFiles then
    begin
        result := TaskResFail ;
        fReqResponse := 'Some Errors Encountered' ;
    end ;

// see if any old empty directories to delete
    if DelDirSrcList.Count <> 0 then DeleteEmptyDirs (CurSrcDir, DelDirSrcList) ;
    if DelDirTarList.Count <> 0 then DeleteEmptyDirs (CurTarDir, DelDirTarList) ;
    doCopyEvent (LogLevelProg, '') ;  // clear progress display

    finally
        {$IFDEF Zipping}
        if fZipped then
        begin
            if Assigned (VCLZip) then VCLZip.Free ;
            if Assigned (VCLUnZip) then VCLUnZip.Free ;
        end ;
       {$ENDIF}
        DelDirSrcList.Free ;
        DelDirTarList.Free ;
        if fWow64RedirDisable then RevertWow64Redir (OldWow64) ; // 22 May 2013
    end ;
end;

// select and copy multiple local files from one location to another

function TMagFileCopy.SelCopyFiles (const CheckFiles: boolean): TTaskResult ;
var
    I, totsecs: integer ;
    errflag: boolean ;
    starttick: DWORD ;
    xProcOKBytes: int64 ;   // 29 Jan 2003, was integer and wrapped
    xProcOKFiles, xProcFailFiles: integer ;
    TaskResult: TTaskResult ;

// get file totals, optionally list all files that will be copied
    procedure DispSel ;
    var
        I: integer ;
        newsize, delsize: int64 ;
        info: string ;
        FileRec: PTFileRec ;
        listing: TMagStringBuild ;
    begin

    // find size of stuff to copy
        newsize := 0 ;
        delsize := 0 ;
        listing := TMagStringBuild.Create ;
        try
        if fDelOldTar and (CurDelFiles <> 0) then
        begin
            if CheckFiles then
            begin
                listing.Capacity (TotSrcFiles * 50) ;  // no real idea yet
                listing.AppendLine (CRLF_ + 'Old Files Selected for Deletion are: ') ;
            end;
            for I := 0 to Pred (TotTarFiles) do
            begin
                FileRec := TarFileList [I] ;
                with FileRec^ do
                begin
                    if FrFileCopy = FCStateSelect then
                    begin
                        inc (delsize, FrFileBytes) ;
                        if CheckFiles then listing.AppendLine (FrFullName + ', Size ' + Int64ToCStr (FrFileBytes)) ;
                    end ;
                end ;
            end ;
        end ;
        if CheckFiles then
        begin
            listing.Capacity (TotSrcFiles * 50) ;   // no real idea yet
            listing.AppendLine (CRLF_ + 'Files Selected for Copying are: ') ;
        end;
        for I := 0 to Pred (TotSrcFiles) do
        begin
            FileRec := SrcFileList [I] ;
            with FileRec^ do
            begin
                if FrFileCopy = FCStateSelect then
                begin
                    inc (newsize, FrFileBytes) ;
                    if CheckFiles then
                    begin
                        if ((FrFileAttr and faDirectory) = faDirectory) then   // 8 Feb 2011 display directory
                            listing.AppendLine (FrFullName + space + sDirLit)
                        else
                            listing.AppendLine (FrFullName + ', Size ' + Int64ToCStr (FrFileBytes)) ;
                    end;
                end ;
            end ;
        end ;
        if CheckFiles then
        begin
            info := listing.ToString ;
            doCopyEvent (LogLevelInfo, info) ;
            info := '' ;
        end;
        finally
            listing.Free ;
        end;
        doCopyEvent (LogLevelInfo, 'Source Files Skipped ' + IntToCStr (CurSkippedFiles)) ;
        info := 'Selected Total Files ' + IntToCStr (CurProcFiles) + ', Total size ' + IntToKByte (newsize, true) ;
        if CurDelFiles <> 0 then info := info + CRLF_ + 'Old Files to Delete ' +
                                    IntToCStr (CurDelFiles) + ', Total size ' + IntToKByte (delsize, true) ;
        doCopyEvent (LogLevelInfo, info) ;
        inc (fCopyProg.SkippedFiles, CurSkippedFiles) ;
        inc (fCopyProg.TotProcFiles, CurProcFiles) ;
        inc (fCopyProg.TotDelFiles, CurDelFiles) ;
        inc (fCopyProg.TotProcBytes, newsize) ;
        inc (fCopyProg.TotDelBytes, delsize) ;
    end ;

begin
//    result := true ;
    CopyProgClearAll (fCopyProg) ;   // 16 May 2013 clear all progress information
    fCopyProg.SessStartTick := GetTickCountX ;
    fReqResponse := 'No Errors' ;
    totsecs := 0 ;
    try
    fCancelFlag := false ;

// multiple directories are done one at a time, to save memory
    if fMultiDir then
    begin
// some simple validation
        if (fSrcDirList.Count = 0) or (fTarDirList.Count = 0) then
        begin
            result := TaskResFail ;
            fReqResponse := 'Multiple Directories Must Be Specified' ;
            exit ;
        end ;
        errflag := false ;
        result := TaskResOKNone ;  // true if one file copied OK
        for I := 0 to Pred (fSrcDirList.Count) do
        begin
            doCopyEvent (LogLevelInfo, ' ') ;
            doCopyEvent (LogLevelInfo, 'Processing Multiple Directory ' +
                                        IntToCStr (succ (I)) + ' of ' + IntToCStr (fSrcDirList.Count)) ;
            ClearTarList ;
            ClearSrcList ;
            fSrcDirList [I] := ExcludeTrailingBackslash (fSrcDirList [I]) ;
            fTarDirList [I] := ExcludeTrailingBackslash (fTarDirList [I]) ;
            TaskResult := SelectFiles (fSrcDirList [I], fTarDirList [I]) ;
            MessagePump ; // 16 Sept 2010
            if fCancelFlag then
            begin
                ClearSrcList ;
                ClearTarList ;
                exit ;
            end ;
            if TaskResult = TaskResOKNew then
            begin
                DispSel ;    // shows selection info, maybe list of files
                if NOT CheckFiles then
                begin
               // start real copying
                    xProcOKBytes := fCopyProg.ProcOKBytes ;
                    xProcOKFiles := fCopyProg.ProcOKFiles ;
                    xProcFailFiles := fCopyProg.ProcFailFiles ;
                    starttick := GetTickCountX ;
                    TaskResult := CopyFiles (fSrcDirList [I], fTarDirList [I]) ;
                    if (result = TaskResOKNone) and (TaskResult <> TaskResOKNone) then result := TaskResult ;
                    totsecs :=  totsecs + ElapsedSecs (starttick) ;
                    doCopyEvent (LogLevelDelimTot, fSrcDirList [I] + '|' + fTarDirList [I] + '|' +
                                IntToStr (fCopyProg.ProcOKBytes - xProcOKBytes) + '|' + IntToStr (fCopyProg.ProcOKFiles -
                                     xProcOKFiles) + '|' + IntToStr (fCopyProg.ProcFailFiles - xProcFailFiles) + '|Totals|') ;
               end ;
            end ;
            if TaskResult = TaskResOKNone then
            begin
                doCopyEvent (LogLevelInfo, fReqResponse) ;  // nothing to copy
            end ;
            if TaskResult = TaskResFail then
            begin
                errflag := true ;
                doCopyEvent (LogLevelInfo, fReqResponse) ;  // failed, say why
            end ;
            MessagePump ; // 16 Sept 2010
            if fCancelFlag then
            begin
                ClearSrcList ;
                exit ;
            end ;
        end ;
        if errflag then
            fReqResponse := 'Some Errors Encountered'
        else
            fReqResponse := 'No Errors' ;
        ClearSrcList ;

    // show totals for all directories - checking only, no copying
        if CheckFiles then
        begin
            doCopyEvent (LogLevelInfo, ' ') ;
            doCopyEvent (LogLevelInfo, 'All Directories Files Skipped ' + IntToCStr (fCopyProg.SkippedFiles)) ;
            doCopyEvent (LogLevelInfo, 'All Directories Total Files ' +
                                 IntToCStr (fCopyProg.TotProcFiles) + ', Total size ' + IntToKByte (fCopyProg.TotProcBytes, true)) ;
            fReqResponse := '' ;
            result := TaskResOKNone ;
            exit ;
        end ;
    end
    else

// single directory pair
    begin
        ClearTarList ;
        ClearSrcList ;
        if (fSrcDir = '') or (fTarDir = '') then
        begin
            result := TaskResFail ;
            fReqResponse := 'Source and Target Directories Must Be Specified' ;
            exit ;
        end ;
        doCopyEvent (LogLevelInfo, ' ') ;
        fSrcDir := ExcludeTrailingBackslash (fSrcDir) ;
        fTarDir := ExcludeTrailingBackslash (fTarDir) ;
        TaskResult := SelectFiles (fSrcDir, fTarDir) ;

    // don't clear source list, application may want to look at it
        MessagePump ; // 16 Sept 2010
        if fCancelFlag or (TaskResult <> TaskResOKNew) then
        begin
            result := TaskResult ;
            ClearTarList ;
            ClearSrcList ;
            exit ;
        end ;

        DispSel ;    // shows selection info, maybe list of files
        if CheckFiles then    // test only
        begin
            fReqResponse := '' ;
            result := TaskResOKNone ;
            exit ;
        end ;
    // start real copying
        starttick := GetTickCountX ;
        result := CopyFiles (fSrcDir, fTarDir) ;
        totsecs := ElapsedSecs( starttick) ;
        doCopyEvent (LogLevelDelimTot, fSrcDir + '|' + fTarDir + '|' + IntToStr (fCopyProg.ProcOKBytes) +
                             '|' + IntToStr (fCopyProg.ProcOKFiles) + '|' + IntToStr (fCopyProg.ProcFailFiles) + '|Totals|') ;    
    end ;

// common to single and multi dirs
    if NOT fCancelFlag then
    begin
        doCopyEvent (LogLevelInfo, ' ') ;
        doCopyEvent (LogLevelInfo, 'Finished, files copied OK: ' + IntToCStr (fCopyProg.ProcOKFiles) +
                         ', failed: ' + IntToCStr (fCopyProg.ProcFailFiles) + ', skipped: ' + IntToCStr (fCopyProg.SkippedFiles)) ;
        doCopyEvent (LogLevelInfo, 'Total size copied ' + IntToKByte (fCopyProg.ProcOKBytes, true) +
                           ', duration ' + TimerToStr (SecsToTime (totsecs)) + ', average speed ' +
                              IntToKByte (CalcSpeed (fCopyProg.SessDoneTicks, fCopyProg.ProcOKBytes)) + '/sec') ;
        if fCopyProg.DelOKFiles <> 0 then doCopyEvent (LogLevelInfo, 'Old target files deleted OK: ' +
                          IntToCStr (fCopyProg.DelOKFiles) + ', Total size deleted ' + IntToKByte (fCopyProg.DelOKBytes, true)) ;
    end ;
    doCopyEvent (LogLevelProg, '') ;  // clear progress display
    finally
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Copying Files' ;
        end ;
   end ;
end ;

// delete list of empty directories, including higher level directories (but not root)

function TMagFileCopy.DeleteEmptyDirs (RootDir: string; DirList: TStringList): integer ;
var
    I, J: integer ;
    curdir: string ;
begin
    result := 0 ;
    if NOT Assigned (DirList) then exit ;
    if DirList.Count = 0 then exit ;
    doCopyEvent (LogLevelInfo, 'Checking for Directories: ' + RootDir) ;
    RootDir := trim (ExcludeTrailingBackslash (RootDir)) ;
    for I := 0 to Pred (DirList.Count) do
    begin
        if fCancelFlag then exit ;
        if Length (Trim (DirList [I])) <= 1 then continue ;   // 19 March 2003
        curdir := RootDir + Trim (DirList [I]) ;
        J := Length (curdir) ;
        while J >= 2 do     // loop deleting directories
        begin
            if curdir [J] = '\' then dec (J) ;
            curdir := copy (curdir, 1, J) ;
            if curdir = RootDir then break ;   // stop at root directory  // 19 March 2003
            doCopyEvent (LogLevelDiag, 'Checking Directory Empty: ' +  curdir) ;
            if NOT CheckDirAny (curdir) then
            begin
             // doCopyEvent (LogLevelDiag, 'Will Delete Dir: ' + curdir) ;
                if RemoveDir (curdir) then
                begin
                    inc (result) ;
                    doCopyEvent (LogLevelFile, 'Removed Directory OK: ' + curdir) ;
                end
                else
                    doCopyEvent (LogLevelFile, 'Failed to Remove Directory: ' + curdir) ;
                while J >= 2 do   // search for lower level directory
                begin
                    dec (J) ;
                    if curdir [J] = '\' then break ;
                end ;
            end
            else
                break ;
        end ;
    end ;
end ;

// delete specified files, use fSrcDir, fCopyType, fSubDirs, fSrcFName,
//   fCopyLoDT, fCopyHiDT

function TMagFileCopy.DeleteFiles (const CheckFiles: boolean): TTaskResult ;
var
    newfname, newsrcdir, curdir, info: string ;
    I, J, retval: integer ;
    newsize: int64 ;
    DelFileRec: PTFileRec ;
    DelDirList: TStringList ;
    OldWow64: BOOL ;        // 22 May 2013
    listing: TMagStringBuild ;
begin
    result := TaskResNone ;
    DelDirList := TStringList.Create ;
    DelDirList.Sorted := true ;
{$IFNDEF VER130} // D5
    DelDirList.CaseSensitive := false ;
{$ENDIF}
    DelDirList.Duplicates := dupIgnore ;
    OldWow64 := false ; // 22 May 2013
    if fWow64RedirDisable then DisableWow64Redir (OldWow64) ; // 22 May 2013
    try  // finally
    ClearSrcList ;
    CopyProgClearAll (fCopyProg) ;   // 16 May 2013 clear all progress information
    fReqResponse := 'No Errors' ;
    doCopyEvent (LogLevelInfo, 'Locating Files to Delete: ' + fSrcDir) ;

// build list of files to delete
    newsrcdir := fSrcDir ;
    newfname := '*.*' ;
    if fCopyType in [FCTypeMaskDir, FCTypeDates] then
    begin
        if fMultiDir then
        begin
            newfname := ExtractFileName (fSrcDir) ;   // check found a file name
            if (Pos ('.', newfname) > 1) and (Pos ('*', newfname) > 0) then
                newsrcdir := ExtractFilePath (fSrcDir)
            else
                newfname := '*.*' ;
        end
        else
            newfname := fSrcFName ;   /// partial masked directory
    end ;
    if (NOT fMultiDir) and fMask then newfname := '*.*' ;  // date mask
    TotSrcFiles := GetDirList (newsrcdir, newfname, fCopyType, fSubDirs, fCopyLoDT, fCopyHiDT, SrcFiles, SrcFileList) ;
    if TotSrcFiles = -1  then
    begin
        result := TaskResFail ;
        fReqResponse := 'Error Locating Files - ' + GetExceptMess (ExceptObject) ;
        exit ;
    end ;
    if TotSrcFiles <= 0  then
    begin
        result := TaskResOKNone ;
        fReqResponse := 'No Source Files Found' ;
        exit ;
    end ;
    MessagePump ; // 16 Sept 2010
    if fCancelFlag then
    begin
        result := TaskResAbort ;
        exit ;
    end ;

// list files for deletion
    listing := TMagStringBuild.Create ;
    try
    if CheckFiles then
     begin
        listing.Capacity (TotSrcFiles * 50) ;   // no real idea yet
        listing.AppendLine ('Files Selected for Deletion are: ') ;
     end;
    newsize := 0 ;
    for I := 0 to Pred (TotSrcFiles) do
    begin
        DelFileRec := SrcFileList [I] ;
        with DelFileRec^ do
        begin
            inc (newsize, FrFileBytes) ;
            if CheckFiles then
            begin
                if ((FrFileAttr and faDirectory) = faDirectory) then   // 8 Feb 2011 display directory
                    listing.AppendLine (FrFullName + space + sDirLit)
                else
                    listing.AppendLine (FrFullName + ', Size ' + Int64ToCStr (FrFileBytes)) ;
            end;
        end ;
    end ;
    if CheckFiles then
    begin
        info := listing.ToString ;
        doCopyEvent (LogLevelInfo, info) ;
        info := '' ;
    end;
    finally
        listing.Free ;
    end;
    info := 'Will Delete Total Files ' + IntToCStr (TotSrcFiles) + ', Total size ' + IntToKByte (newsize, true) ;
    fCopyProg.TotProcFiles := TotSrcFiles ;
    fCopyProg.TotProcBytes := newsize ;
    doCopyEvent (LogLevelInfo, info) ;
    if CheckFiles then    // test only
    begin
        fReqResponse := '' ;
        result := TaskResOKNone ;
        exit ;
    end ;

// delete files
    doCopyEvent (LogLevelInfo, 'Deleting Files: ' + newsrcdir) ;
    for I := 0 to Pred (TotSrcFiles) do
    begin
        DelFileRec := SrcFileList [I] ;
        with DelFileRec^ do
        begin
            if ((FrFileAttr and faDirectory) = faDirectory) then   // 8 Feb 2011 display directory
            begin
                DelDirList.Add (AnsiLowerCase (Trim (FrFullName))) ;
                FrFileCopy := FCStateOK ;
                inc (fCopyProg.DelOKFiles) ;
                continue ;
            end;
            doCopyEvent (LogLevelFile, 'Deleting: ' + FrFullName) ;
            inc (fCopyProg.TotDoneNr) ;
            retval :=  MagDeleteFile (FrFullName, fReplRO) ;
            if retval = 0 then
            begin
                doCopyEvent (LogLevelDelimFile, '|' + FrFullName + '|0|0|0|File Deleted|0|0') ;
                inc (fCopyProg.DelOKBytes, FrFileBytes) ;
                FrFileCopy := FCStateOK ;
                inc (fCopyProg.DelOKFiles) ;

             // add directory to list we'll try and delete later
                curdir := AnsiLowerCase (Trim (FrSubDirs)) ;
                if curdir <> '' then
                begin
                    if NOT DelDirList.Find (curdir, J) then DelDirList.Add (curdir) ;
                end ;
            end
            else
            begin
                doCopyEvent (LogLevelInfo, 'File Delete Failed: ' + FrFullName+ ' - ' +  SysErrorMessage (retval)) ;
                FrFileCopy := FCStateFailed ;
                inc (fCopyProg.ProcFailFiles) ;
            end ;
            MessagePump ; // 16 Sept 2010
            if fCancelFlag then exit ;
        end ;
    end ;

// see if any old empty directories to delete
    if DelDirList.Count <> 0 then DeleteEmptyDirs (newsrcdir, DelDirList) ;

// report totals
    if NOT fCancelFlag and (fCopyProg.DelOKFiles <> 0) then
    begin
        doCopyEvent (LogLevelInfo, 'Files deleted OK: ' + IntToCStr (fCopyProg.DelOKFiles) +
                                                 ', Total size deleted ' + IntToKByte (fCopyProg.DelOKBytes, true)) ;
        Result := TaskResOKNew ;
    end ;
    doCopyEvent (LogLevelProg, '') ;  // clear progress display
    finally
       DelDirList.Free ;
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Deleting Files' ;
        end ;
    end ;
end ;

procedure TMagFileCopy.Cancel ;
begin
    fCancelFlag := true ;
end ;

end.

