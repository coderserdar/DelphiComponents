Magenta Systems File Transfer Components v2.6 and v3.4 for ICS V6 and V7
========================================================================

Updated by Angus Robertson, Magenta Systems Ltd, 19th May 2009
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd, England

Magenta Systems File Transfer Components comprise three Delphi components,
TMagFtp, TMagHttp and TMagFileCopy, the first two of which are high level
descendants of the ICS TFtpClient and THttpCli components, all allowing
transfer of multiple files and subdirectories with a single function call.
An entire disk volume or just one file may be copied or sent using FTP.

The three components handle file listing from FTP, HTTP and disk volumes,
and multiple file copying.  They also include optional zipping or unzipping
(requires TVCLZip component).  There are numerous options available,
including copying only newer files, deleting old target files, deletion
after transfer, resumed failed FTP downloads and uploads, and a progress
events with various levels of information for logging or display, depending
upon application requirements that allows transfers to be cancelled. FTP
and file copying supports files larger than 2 gigs (64-bit).

There are three versions of the components, each supporting Delphi 7, 2007
and 2009:

- v2.6 only supports ANSI named files, with ICS v6 on Delphi 7 and 2007 
  dated August 2008 or later and with ICS v7 dated April 2009 or later,
  SVN revision 300 or later with Delphi 7 and later.    

- v3.4 supports ANSI named files with Delphi 7 and 2007, and Unicode named
  files with Delphi 2009, with ICS v7 only, dated April 2009 or later,
  SVN revision 300 or later.

- v3.4W supports Unicode named files on Delphi 2007 with ICS v7
  only, dated April 2009 or later, SVN revision 300 or later, using
  custom wide versions of the ICS components with widestrings.  This
  version also includes a widestring version of the ICS FTP server
  supporting full Unicode with Delphi 2007. The TNT Delphi Unicode
  Controls are used to display a Unicode log window.

All versions may be installed at the same time, all the v3.4W units
and types have a W suffix (for widestring).

Simple demonstration programs illustrate all major functionality.
End user help is available describing most of the functionality in Magenta
Systems DUN Manager application (from http://www.magsys.co.uk/dunman/),
look under Scheduled Task Properties, FTP General, FTP Common, FTP Upload,
FTP Download. HTTP Download and Sync Files.  DUN Manager also provides a
compiled test application for the components, using all functions (but DUN
Manager source code is not available).

Magenta Systems File Transfer Components are copyrighted software but may
be used without cost.

These components needs François PIETTE internet component suite (ICS)
version 6 or version 7 from http://www.overbyte.be/

The latest nightly SVN snapshort versions of ICS may be downloaded from:

V6: http://www.magsys.co.uk/download/wiki/icsv6w.zip

V7: http://www.magsys.co.uk/download/wiki/icsv7w.zip

or from: http://wiki.overbyte.be/wiki/index.php/FAQ

(which should be exactly the same zip files). 


Unicode Compatibility with various web servers
----------------------------------------------

Originally, FTP servers only support ASCII file names, but later sent file
names as 8-bit ANSI characters.  If the underlying operating system has
a Unicode file name with characters outside ANSI, they are substituted
with a question mark, which is an illegal file name character in Windows.
FTP servers with UTF8 support encode all file names in 8-bit UTF8, where
essentially the lower 128 characters are unchanged, and other characters
are represented by two or more alternate characters.

Even though an FTP server supports UTF8, it may not include Unicode
characters outside the ANSI codeset.  The FTP 'OPTS UTF8' or 'OPTS UTF8
ON' command must be sent before most servers support UTF8 file listings
or uploads.

ICS V6 - does not support UTF8

ICS V7 - support UTF8, fully Unicode capable when build with Delphi 2009
or later, defaults to UTF8 OFF and returns ANSI file listings, OPTS UTF8
or OPTS UTF8 ON enables UTF8 file listings and uploads

ICS V7 Wide - support UTF8, fully Unicode capable when build with Delphi
2007 or later defaults to UTF8 OFF and returns ANSI file listings, OPTS
UTF8 or OPTS UTF8 ON enables UTF8 file listings and uploads.

Microsoft IIS/5 and IIS/6 no UTF8 support.

Microsoft FTP7 IIS/7 for Windows 2008 - fully Unicode capable, defaults
to UTF8 OFF and returns ANSI file listings, OPTS UTF8 or OPTS UTF8 ON
enables UTF8 file listings and uploads.

FileZilla Server - fully Unicode capable, defaults to UTF8 ON returning
UFT8 file listings and uploads, but can be disabled with OPTS UTF8 OFF
command when listings revert to ANSI.

WS_FTP Server 6.1.1 and 7 - fully Unicode capable, defaults to UTF8 OFF
and returns ANSI file listings, OPTS UTF8 or OPTS UTF8 ON enables UTF8
file listings and uploads, UTF8 can not be set off so don't send command
if ANSI needed. MLST fails with 501 if file name includes a space.

RhinoSoft Serv-U FTP Server v7.2.0.1 - no Unicode support, defaults to
UTF8 OFF but returns UTF8 file listings (with ? for non-supported Unicode
characters), OPTS UTF8 ON enables UTF8 uploads.  OPTS UTF8 OFF command
reverts listings to ANSI. MLST fails with 550 for file names with any UTF8
escaped characters MDTM YYYYMMDDHHMMSS fail if time zone used (worked in
earlier releases).  v7.3 fixes the MLST and MDTM errors, and adds
the MFMT command, v8 will support Unicode.

Gene6 FTP Server v3.10.0 - fully Unicode capable, defaults to UTF8 OFF and
returns ANSI file listings, OPTS UTF8 or OPTS UTF8 ON enables UTF8 file
listings and uploads, UTF8 can not be set off so don't send command if ANSI
needed. MLSD fails with a Unicode sub-directory argument (but CWD works OK
with Unicode).


Unicode VCL
-----------

To display Unicode characters in Delphi 2007, TNT Delphi Unicode Controls
may be used to replace the standard VCL controls such as TMemo, TButton,
etc.  The original free version is available from:

http://mh-nexus.de/en/tntunicodecontrols.php

http://www.axolot.com/TNT/

while the latest commercial TNT version may be bought as the TMS Unicode
Component Pack from:

http://www.tmssoftware.com/site/tmsuni.asp

Note the TMagXfer wide demo optionally uses these components, but a define
may be removed to avoid them being a pre-requisite.


Usage Overview
--------------

TFileRec

The three file transfer components all rely on a common file listing
record type:

TFileRec = record
    FrFileName: string ;    // filename and extension
    FrSubDirs: string ;     // \ for base directory, else located sub dirs
    FrDirLevel: integer ;   // 0 for base directory, or level of sub dirs
    FrDirBaseLen: integer ; // length of basedir within FullName
    FrFullName: string ;    // basedir, subdirs, filename - complete path
    FrFileBytes: int64 ;    // size of file in bytes
    FrFileDT: TDateTime ;   // file time stamp - local time
    FrFileUDT: TDateTime ;  // file time stamp - UTC time
    FrFileAttr: integer ;   // bitmapped Windows file attributes
    FrExtra: string ;       // <DIR> or UNIX file attributes
    FrLinks: string ;       // not blank means this is not a real file
    FrFileCopy: TFileCopyState ;  // file copy state, selected, done, etc,
end ;

This record is used in TFileRecs dynamic arrays, and is filled by Windows
FindFile APIs for local files (in BuildDirList2) , from the FTP DIR or
MLSD commands (in FtpDir), or by parsing HTML for links (in Download).
Local and FTP file indexing optionally supports listing sub-directories.
Generally, two file record lists are built, for local and remote files,
and then compared (using SelectCopyFileList) to select the files that
need to be copied and optionally deleted (locally or remotely) to cause
synchronisation of directories.

The components also offer methods for handling single files, rather than
whole directories.


TFindList

This is a descendent of TList added a Find function using binary search
identical to sorting, that is much more efficient than the normal IndexOf
method when the file list contains potentially up to a million items.

function Find (const Item2: Pointer; Compare: TListSortCompare;
                                    var index: longint): Boolean;

This is used with the function CompareFNext to compare file name records
with case insensitivity.  A list of file name records in the dynamic array
is built into a TFindList and sorted, allowing the files to accessed in
sorted order, and also easing comparison between source and target lists.
Generally, this file list handling is all hidden, when doing straight
copying.


TBulkCopyEvent

The three component also share a common progress event, that returns
various levels of progress information for display to the user and
for logging, depending upon the application.  The progress events
also allows copying to be cancelled, including part way through a file.

TBulkCopyEvent = Procedure (LogLevel: TLogLevel ; Info: string ;
                                        var Cancel: boolean) of object ;

LogLevelInfo - general copying information, start, finish, etc
LogLevelFile - file level information, start, finish, etc
LogLevelProg - progress per file, chunks copied - avoid if possible
LogLevelDiag - diagnostic information - generally ignore
LogLevelDelimFile - seven | delimited fields after each file for logging
LogLevelDelimTot - seven | delimited fields after each job for logging

The delimited logging lines are seven fields:

SrcFile|TarFile|Size|Succ|Fail|Comm|Duration

Where succ and fail are digits, 0 or 1 for single files, larger for totals,
duration is milliseconds, comment might be OK or reason for failure.

For example, for LogLevelDelimFile after copying a single file:

d:\windows\system32\drivers\gmreadme.txt|d:\tempfiles\drivers\
  gmreadme.txt|646|1|0|OK|15

For example, for LogLevelDelimTot, totals after copying a directory:

d:\windows\system32|d:\tempfiles|419554|49|0|Totals|


TFileCopyType

Specifies the type of copying the component should perform, also used
for file deletion.

FCTypeSingle - just a single file will be copied
FCTypeMaskDir - multiple files will be copied that match the mask file
  Name (ie *.* for all *.zip for just zip files)
FCTypeArchDir - any files with the Archive file attribute set and matching
  the mask will be copied.  Note the attribute is not changed
FCTypeAllDir - all files found will be copied.
FCTypeDates - all files found will be copied within a date/time range


TFileCopyRepl

Specifies whether target files that exist should be replaced.

FCReplNever - copy will be skipped
FCReplAlways - always replace
FCReplDiff - replace if time stamp or size different
FCReplNewer - replace if time stamp newer or size different


TFileCopyState

This is primarily an internal copy state kept in TFileRec for each file,
which is updated as selection and copying proceeds.

FCStateNone - nothing done
FCStateIgnore - copy will be skipped
FCStateDir - file is directory entry
FCStateSelect - waiting to copy file
FCStateCopying - file being copied
FCStateOK - copied OK
FCStateFailed - copy failed


TTaskResult

Many of the methods return a typed task result:

TaskResNone - task did not start
TaskResOKNew - task finished OK and copied files
TaskResOKNone - task finished OK but found nothing to copy
TaskResFail - task failed, reason will be in ReqResponse property
TaskResAbort - task was aborted, usually by user or lost connection


The full interface sections of the various components are listed in
interfaces.txt.


TMagFileCopy
------------

TMagFileCopy is a high level HTTP Delphi component that allows indexing
and copying of multiple file directories, using a single function call.
The component handles listing source and destination files, and comparing
them to avoid unnecessary transfers, selection using a file mask, deletion
of old files, delete after copy, safe copy (to avoid partial files), and
zip before copying or unzip after copy.  The component is equally useful
for copying single files or entire volumes with hundreds of thousands of
files, and provides 'sync files' functionality making it ideal for
backups, only copying changed files and deleting old target files.

Note there are two distinct copying modes in TMagFileCopy, copying a
single directory (and subdirectories) and copying a list of multiple
specific directories (and subdirectories).

Properties to set before copying multiple files:

CopyEvent: TBulkCopyEvent - with a logging and progress function

MultiDir: boolean - true copy multiple specific directories, false single

MultiDirList: string - multi directories are passed as srcdir+tab+tardir+
    recsep (repeat) for as many pairs as needed.  The srcdir may
    optionally include a masked file name (including *)

SrcDirList: TStringLists - alternate to MultiDirList, source dirs only
TarDirListTStringLists - alternate to MultiDirList, target dirs only


SrcDir: string - not multidirs, the source directory
TarDir: string - not multidirs, the target directory
SrcFName: string - optional mask name (including *) for partial copy
SubDirs: boolean - true copy all sub directories

CopyType: TFileCopyType - see above
LocalHost: string - set to WSocket.LocalHostName if using masked name
Mask: boolean - true, allow date/time mask characters in SrcFName,
  also $H will be replaced by the LocalHost (see above)
Prev: boolean - true, use yesterday's date for Mask

DelDone: boolean - true delete source file after copy
DelOldTar: boolean -true, delete target files not in source directory
Repl: TFileCopyRepl - see above
ReplRO: boolean - true, replace read only files
Safe: boolean - true, copy file with TMP extension, rename when done
IgnoreFileExt: string - a delimited list of file extensions to ignore

UseUTC: Boolean - whether to compare file timestamps in UTC or local time
CopyLoDT: TDateTime - for CopyType FCTypeDates, lower date/time
CopyHiDT: TDateTime - for CopyType FCTypeDates, higher date/time

Main Methods

function SelCopyFiles (const CheckFiles: boolean): TTaskResult ;

This is the main method that selects and copies files.

If CheckFiles is true it skips the actual copying just returning a list
of the files selected for copying, with file records in SrcFiles and the
sorted pointers in SrcFileList, if CopyState is FCStateSelect the file
has been selected for copying.

If CheckFiles is false, the selected files will be copied, with the result
showing success or otherwise.  Several properties are completed with
various totals processed: TotProcFiles, ProcOKFiles, DelOKFiles,
ProcFailFiles, SkippedFiles, TotProcBytes, ProcOKBytes and DelOKBytes.
The file records in SrcFiles and the sorted pointers in SrcFileList are
updated with the success or failure of each file copied, although logging
will normally have been done on a file by file basis through the
CopyEvent.

Use of the SelCopyFiles function is illustrated in the demo application.


function DeleteFiles (const CheckFiles: boolean): TTaskResult ;

This method is for deleting multiple files, it uses the same source file
properties as SelCopyFiles, including CopyType, except no MultiDir.

Use of the DeleteFiles function is illustrated in the demo application.


procedure Cancel ;

This method will cancel copying.


function GetTaskResName (TaskResult: TTaskResult): string ;

This function returns an ASCII version of the task result.


Support Functions

A number of the functions used internally by SelCopyFiles are available
for separate use in applications that need to index and process files.

function MagDeleteFile (const Fname: string;
                                const ReadOnly: boolean): Integer ;

This function deletes a single file, optionally removing the read only
attribute. Returns 0 for delete OK, -1 file not found, 1 could not remove
read only, larger number failed and is GetLastError code.


function MagRenameFile (const OldName, NewName: string;
                            const Replace, ReadOnly: boolean): Integer ;

This function renames a single file, optionally replacing it, optionally
removing read only attribute.  Returns 0 for replace OK, 1 could not
remove read only, 2 new file exists (and Replace false), larger number
failed and is GetLastError code.


function DeleteEmptyDirs (RootDir: string; DirList: TStringList): integer ;

This function deletes empty directories, typically after all the files in
them have been deleted.  It also deletes higher level directories, down to
the specified root directory.


function CopyOneFile (const Fnamesrc, Fnametar: string ; Replopt:
  TFileCopyRepl; const Safe: boolean; var Fsize: Int64) : TTaskResult ;

This function copies a single file, using source and target file names,
and standard Replopt options.  Fsize will be filled with the size copied,
if successful.  CopyEvent is called with progress messages.


function BuildDirList2 (LocDir, LocPartName: string; const SubDirs:
    boolean; Level, InitDLen: integer ; const LoDT, HiDT: TDateTime ;
              var TotFiles: integer; var LocFiles: TFileRecs): boolean ;

This is the main local file indexing function, that calls itself
recursively if SubDirs is specified to build-up lists of files in the root
and sub-directories.  LocDir is the local directory to index, it may be a
root.  LocPartName is an optional file name mask for sub-selection.  Level
and InitDLen should be 0 (they are used for recursive calls).  LoDT and
HiDT are an optional date selection range (if non-zero). Files are listed
in non-sorted order into a dynamic array LocFiles, with the initial number
of listed files in the array specified by TotFiles (this need not be
zero). The  CopyEvent is called with progress messages. It returns false
for error or if cancelled from CopyEvent.  This function will index
hundreds of thousands of files on a large volume, but will take a few
minutes to do so.


function BuildDirList (LocDir, LocPartName: string; const SubDirs:
    boolean; Level, InitDLen: integer ; var TotFiles: integer;
                                  var LocFiles: TFileRecs): boolean ;

This function calls BuildDirList2, but ignores file date ranges.


function GetDirList (LocDir, LocPartName: string; const SelOpt:
    TFileCopyType; const SubDirs: boolean; LoDT, HiDT: TDateTime ;
        var LocFiles: TFileRecs; var LocFileList: TFindList): integer ;

This function calls BuildDirList2, but has differing parameters, in
particular SelOpt and a LocFileList which is filled with a sorted
pointer list to LocFiles. It returns the total number of files found
or -1 if cancelled or an error.


function SelectCopyFileList (const SrcFileList, TarFileList: TFindList;
     const fname: string; const selopt: TFileCopyType;
       const replopt: TFileCopyRepl; const DiffStamp: integer;
         const IgnoreOldTime: boolean; IgnoreExt: string ;
               var skipped: integer; const UTCFlag: boolean): integer ;

This is the main function that is used to compare source and target file
lists against each other and the specified Copy Type and Replace options
and to select files that should be copied.  It may also be used to locate
which files are in the target directory but not the source, to allow
deletion is synchronising directories.  When comparing hundreds of
thousands of files, this function will take a few minutes.  DiffStamp
is the number of minutes difference in time stamp before a file is
considered different, use 61 or 62 minutes to avoid summer times issues.


function MagCheckReplace (const replopt: TFileCopyRepl;
       const IgnoreOldTime: boolean; const AllowDiffDT: TDateTime;
         const SrcSize, TarSize: Int64; SrcDT, TarDT: TDateTime): boolean ;

This function is used to determine if a single file should be replaced, by
comparing file sizes and time stamps, and the specified Replopt option.


function FmtFileDir (const FileRecs: TFileRecs;
                                        const UTCFlag: boolean): string ;

Format an unsorted file directory from FileRecs array, into a multi-line
ASCII string which may be very large.


function FmtFileDirList (const LocFileList: TFindList;
                                        const UTCFlag: boolean): string ;

Format an sorted file directory from File List pointer to an array, into
a multi-line ASCII string which may be very large.


function DispLocFiles (const SrcDir, SrcFile: string ; const CopyType:
        TFileCopyType ; const SubDirs: boolean; const UTCFlag: boolean): string ;

This function builds a displayable file directory a multi-line ASCII string,
it calls BuildDirList2 and FmtFileDirList.

Use of the DispLocFiles function is illustrated in the demo application.


function CheckDirAny (LocDir: string): boolean ;

Check if a directory contains at least one file or sub-directory, used
before deleting the directory.


Network File Share Methods

File copying will often work across a LAN using UNC share names, provided
the remote computer accepts the same logon as the local computer.  If not,
it's necessary to first connect to the remote computer with a logon,
which can either map a drive letter to the remote share or just create a
connection so UNC file names may be used for copying.  Note it's more
efficient to use mapped drives.  Properties that should be set are:

LocalName: string - blank if using UNC names, otherwise drive, ie N:
RemoteName: string - UNC name of remote share, ie \\sysname\share
UserName: string - a logon user name for remote share
Password: string - a logon password for remote share

Methods

function Connect: boolean ;

Connect with LocalName, RemoteName, UserName and Password to the remote
computer, true if successful, error in ReqResponse, CopyEvent is called.

function Disconnect (AForce: boolean): boolean ;

Disconnect the remote share specified in LocalName or RemoteName.


function GetConnection (ALocalName: string): string ;

Checks if a local drive, ie N:, is connected to a remote share, returning
the remote name if found, or blank if not.  Result also in ReqResponse.



TMagFTP
-------

TMagFtp is a high level FTP Delphi component that allows uploading or
downloading of multiple files from or to an FTP server, from a single
function call.  The component handles listing local and remote files,
including subdirectories, and comparing them to avoid unnecessary
transfers, selection using a file mask, deletion of old files, resuming
failed FTP downloads, unzipping of downloaded files, or zipping before
upload, deletion after uploading or downloading, archive copy after
upload.  A progress event provides various levels of information for
logging or display, depending upon application requirements, and allows
transfers to be cancelled.

TMagFtp descends from ICS TFtpClient, and publishes all it's logon and
proxy properties and events.

Note the component transfer files using temporary file names, only
renaming them to the correct name once the file is confirmed as being
the correct size. So the file test.zip will be uploaded as test_zip.tmp.
This avoids partial real files being left if a connection drops, and is
also used for file resuming, which uses the name test_zip.ftp on the
local computer to store details for possible resumed upload or download
in case of interruption.  The component may be set to immediately repeat
a failed transfer, a specified number of times.

The component attempts to make of the most efficient FTP commands
supported by the server, as reported by the FEAT command.  Specifically,
historically FTP had very poor file indexing with different servers
returning widely differing responses to the LIST file directory command.
The component will use the MLSD, MSLST and MDTM commands if possible for
better listing (using UTC time in particular), and will also use MFMT or
MDTM YYYYMMDDHHMMSS to set the time stamp of uploaded files, if possible.
It uses the MD5 command to perform an MD5SUM check on transferred files to
confirm file integrity.

Properties to set before transferring any files, these relate to the FTP
server and optional proxies, retries, etc.  Also see the ICS documentation
for details of specific FTP properties.

HostName1: string - main FTP server host name or IP address
HostName2: string - alternate FTP server host name or IP address, if
  main server can not be contacted
UserName: string - FTP server logon name
PassWord: string - FTP server logon password
Port: string -  FTP server port, usually 21
MaxAttempts: integer - number of attempts to connect to the FTP server,
  if HostName2 is specified this will alternate with HostName1
AttemptDelay: integer - delay in seconds between connection attempts
FailRepeat: integer - how many times to retry a failed file transfer
ConnectionType: TFTPConnectionType - ftpDirect, ftpProxy, ftpSocks4,
    ftpSocks4A, ftpSocks5
ProxyPort: string - if ConnectionType = ftpProxy
ProxyServer: string - if ConnectionType = ftpProxy
SocksPort: string - if ConnectionType = ftpSocksX
SocksServer: string - if ConnectionType = ftpSocksX
SocksUsercode: string - if ConnectionType = ftpSocksX
SocksPassword: string - if ConnectionType = ftpSocksX

PassiveX: boolean - must only be changed after connection type, true
  if FTP passive mode is to used for the data connection
HostType: THostType - set to FTPTYPE_NONE for automatic detection
XferMode: TXferMode - set to XferModeBinary
CaseFile: TCaseFile  - FileLowerCase for Windows, sometimes FileMixedCase
    for Unix/Linux which has case sensitive file names
DiffStampMins: integer - number of minutes difference in time stamp
  before a file is considered different, use 61 or 62 minutes to avoid
  summer times issues
Timeout: integer - seconds to wait for response to command, typically 60
DispFiles: boolean - true to log file stamp information
DispRDir: boolean - true to log remote file directory
DispLDir: boolean - true to log local file directory
DispRemList: boolean - true to log FTP LIST responses for remote dir
UpArchive: boolean - true to move file to archive directory after
  successful upload, to avoid it being uploaded again
UpArchDir: string - if UpArchive true, directory to move file

ResFailed: boolean - if true, if possible resume failed uploads or
    downloads.  This requires a small file to be saved on the local
    PC, see above
BulkMode: TBulkMode - VERY important, either BulkModeDownload or
    BulkModeUpload, this determines how SrcDir/TarDir are used
SrcDir: string - the source directory, a local directory should include
    a drive or share, a remote directory should not since it will be
    the FTP server root for the user (also use forward slashes)
TarDir: string - the target directory
SrcFName: string - optional mask name (including *) for partial copy
SubDirs: boolean - true copy all sub directories

CopyType: TFileCopyType - see above, but not by Date Range
LocalHost: string - set to WSocket.LocalHostName if using masked name
Mask: boolean - true, allow date/time mask characters in SrcFName,
  also $H will be replaced by the LocalHost (see above)
Prev: boolean - true, use yesterday's date for Mask

DelDone: boolean - true delete source file after copy
DelOldTar: boolean -true, delete target files not in source directory
Repl: TFileCopyRepl - see above
ReplRO: boolean - true, replace read only files
IgnoreFileExt: string - a delimited list of file extensions to ignore


Main single functions:

These functions handle the entire FTP process, including connecting and
logging onto the FTP server, checking remote and local files, then
transferring any files, and finally logging off the FTP server.


function DispFtpDir (var dirlisting: string): TTaskResult ;

This function return a formatted file directory from the FTP server of
the target directory and optionally sub-directories, BulkMode should be
set to BulkModeDownload. The result will show success or otherwise.

Use of the DispFtpDir function is illustrated in the demo application.


function FtpDownload (const CheckFiles: boolean): TTaskResult ;

This is the main method that selects and download files.

If CheckFiles is true it skips the actual download just returning a list
of the files selected for downloading, with file records in SrcFiles and
the sorted pointers in SrcFileList, if CopyState is FCStateSelect the
file has been selected for downloading.

If CheckFiles is false, the selected files will be downloaded, with the
result showing success or otherwise.  Several properties are completed
with various totals processed: TotProcFiles, ProcOKFiles, DelOKFiles,
ProcFailFiles, SkippedFiles, TotProcBytes, ProcOKBytes and DelOKBytes.
The file records in SrcFiles and the sorted pointers in SrcFileList are
updated with the success or failure of each file downloaded, although
logging will normally have been done on a file by file basis through the
CopyEvent.

Use of the FtpDownload function is illustrated in the demo application.


function FtpUpload (const CheckFiles: boolean): TTaskResult ;

This is the main method that selects and uploads files.

If CheckFiles is true it skips the actual uploading just returning a list
of the files selected for uploading, with file records in SrcFiles and the
sorted pointers in SrcFileList, if CopyState is FCStateSelect the file
has been selected for uploading.

If CheckFiles is false, the selected files will be uploaded, with the
result showing success or otherwise.  Several properties are completed
with various totals processed: TotProcFiles, ProcOKFiles, DelOKFiles,
ProcFailFiles, SkippedFiles, TotProcBytes, ProcOKBytes and DelOKBytes.
The file records in SrcFiles and the sorted pointers in SrcFileList are
updated with the success or failure of each file uploaded, although
logging will normally have been done on a file by file basis through the
CopyEvent.

Use of the FtpUpload function is illustrated in the demo application.


procedure Cancel ;

This method will cancel downloads or uploads.


function GetTaskResName (TaskResult: TTaskResult): string ;

This function returns an ASCII version of the task result.


function LookupFTPReq (const RqType: TFtpRequest): String;

This functions returns an ASCII version of TFtpRequest, for diagnostic
purposes.


function LookupFtpState (const FtpState: TFtpState): String;

This functions returns an ASCII version of TTFtpState, for diagnostic
purposes.


Main multiple functions

These functions are used where more control is needed over the FTP
process, for instance where several single files are to be transferred
rather than an entire directory, or where a list of directories need
to be transferred.  FtpLogon must be called first, and FtpLogoff last.

function FtpLogon: TTaskResult ;

This function connects and logs onto to FTP server, the result will show
TaskResOKNew for successful logon, with any error in ReqResponse.  The
Connected property will also be true, and FtpLogoff must then be called
at some point.

Use of the FtpLogon function is illustrated in the demo application.


function FtpDir (var FtpFiles: TFileRecs;
                            var FtpFileList: TFindList): TTaskResult ;

This function builds file directory lists from the FTP server of the
target directory and optionally sub-directories, BulkMode should be
set to BulkModeDownload. The result will show success or otherwise.
It will fail unless the Connected property is true.


function FtpCheckFile (const RemDir, RemFile: string ;
                   var FSize: Int64; var FileUDT: TDateTime): boolean;

This function checks the time stamp and size of a single file on the FTP
server.  It is used by the MagFtp component before every download and
before and after every upload, to check the remote file details.
It will fail unless the Connected property is true.


function FtpDownFiles (const CheckFiles: boolean): TTaskResult ;

This function is identical to FtpDownload, except it assumes an already
connected FTP server. It will fail unless the Connected property is true.


function FtpDownOneFile (const FdirSrc, Fnamesrc, Fnametar: string ;
                                  Replopt: TFileCopyRepl) : TTaskResult ;

This function will download a single file from an already connected FTP
server. It will fail unless the Connected property is true.

Use of the FtpDownOneFile function is illustrated in the demo application.


function FtpUpOneFile (const LocFileFull, RemTarDir, RemTarFile: string;
                                    Replopt: TFileCopyRepl) : TTaskResult ;

This function will upload a single file to an already connected FTP server.
It will fail unless the Connected property is true.


procedure FtpLogoff ;

This function will disconnect from FTP server, if there is already a
connection to the server.  It will wait up to five seconds to confirm
the connection has been closed

Use of the FtpLogoff function is illustrated in the demo application.


TMagHttp
--------

TMagHttp is a high level HTTP Delphi component that allows downloading of
multiple files from an HTTP server using full URLs, or listed by parsing
links from a web page, using a single function call.  The component
handles listing local and remote files, and comparing them to avoid
unnecessary transfers, selection using a file mask, unzipping of
downloaded files.  A progress event provides various levels of information
for logging or display, depending upon application requirements, and
allows downloads to be cancelled. If authentication is needed for the
download, it may be specified in the URL in the format
http://logon:password@host/file.zip.

TMagHttp descends from ICS THttpCli, and publishes all it's properties
and events.

Properties to set before downloading any files, these relate to the HTTP
server and optional proxies, retries, etc.  Also see the ICS documentation
for details of specific HTTP properties such as authorisation or SOCKS.

URLList: string - one or more source URLs separated by CRLF
SrcMask: string - optional mask name (including *) for partial downloads
DownDir: string - the target directory for downloaded files
KeepPath: boolean - if true, use HTTP path for subdirs in DownDir
KeepHost: boolean - if true, use HTTP host for subdir in DownDir
ParseHTML: boolean - if true, parse HTML page for links to files
Proxy: string - if non-blank, an HTTP proxy server
ProxyPort: string - port for proxy server

Repl: TFileCopyRepl - see above
ReplRO: boolean - true, replace read only files
LogFiles: boolean - true to log each file downloaded
LogProt: boolean - true to log HTTP protocol
LogLDir: boolean - true to log local file directory
LogRDir: boolean - true to log remote file directory

Main functions:

function Download (CheckFiles: boolean): TTaskResult ;

If CheckFiles is true it skips the actual download just returning a list
of the URLs and parsed files selected for downloading, with file records
in SrcFiles and the sorted pointers in SrcFileList, if CopyState is
FCStateSelect the file has been selected for downloading.

If CheckFiles is false, the selected URLs and parsed files will be
downloaded, with the result showing success or otherwise.  Several
properties are completed with various totals processed: TotProcFiles,
ProcOKFiles, ProcFailFiles and SkippedFiles.  The file records in
SrcFiles and the sorted pointers in SrcFileList are updated with the
success or failure of each file downloaded, although logging will normally
have been done on a file by file basis through the CopyEvent.


procedure Cancel ;

This method will cancel downloads.


Requirements
------------

Requires Internet Component Suite (ICS) V6 or V7 from
this must generally be the latest snapshot version from: 
http://wiki.overbyte.be/wiki/index.php/FA.

Requires Kevin Boylan's TVCLZip component for zipping from
http://www.vclzip.net/, if you don't purchase this component you will need
to suppress DEFINE Zipping from MAGZIP.INC so the zip code is not linked.

Compatible with Delphi 7, 2006, 2007 and 2009, tested with Windows Windows
2000, XP, 2003, Vista and 2008.


Installation
------------

ICS V6 or V7 must be installed first, this must generally be the latest 
snapshot version from: http://wiki.overbyte.be/wiki/index.php/FA

Build and install the MagentaXfer and MagentaSubs (and ICS) packages for
the appropriate version of Delphi In Tools, Options, add the MagentaXfer
and ICS directories to the library path.  Note the packages expect the SSL
versions of ICS which are now available to all users.

The various File Transfer Components packages that may be built are as 
follows:

v2.x - Delphi 7 - MagentaXfer7a.dpk uses ICS v6 OverbyteIcsDel70
v2.x - Delphi 2007 - MagentaXfer11a.dpk uses ICS v6 OverbyteIcsDel110
v2.x - Delphi 2007 - MagentaXfer2007a.dpk uses ICS v7 OverbyteIcsD2007Design
v2.x - Delphi 2009 - MagentaXfer2009a.dpk uses ICS v7 OverbyteIcsD2009Design
v3.x - Delphi 7 - MagentaXfer7.dpk uses ICS v7 OverbyteIcsD7Design
v3.x - Delphi 2007 - MagentaXfer2007.dpk uses ICS v7 OverbyteIcsD2007Design
v3.x - Delphi 2009 - MagentaXfer2009.dpk uses ICS v7 OverbyteIcsD2009Design
v3.xW - Delphi 2007 - MagentaXfer2007W.dpk uses ICS v7 OverbyteIcsD2007Design

Note that that MagentaXfer2007W also installs widestring versions of the ICS
FTP client and server components to support Unicode under Delphi 2007. 

Please note we don't supply packages for C++ Builder and can not support it. 


Release Notes
-------------

Release 3.4 - 19th May 2009
Don't ignore failed MD5/XMD5 command but report error, prefer MD5 to XMD5.
Added MaskLocDir and MaskRemDir flags to take masked directory from 
SrcFName and add to local and/or remote directory, typically for dated 
directories.
Unicode now supported for MD5sum and CRC32B, add magftpNoMd5 and 
magftpNoCrc to allow them to be tested separately. 
Updated OverbyteIcsFtpcliW with latest changes, added ftptest.org hosts.
Updated the packages using ICS v7 to support the new package names 
introduced in December 2008, please see Installation above. 


Release 2.6 - 19th May 2009
Don't ignore failed MD5/XMD5 command but report error, prefer MD5 to XMD5.
Updated the packages using ICS v7 to support the new package names 
introduced in December 2008, please see Installation above. 


Release 3.2 - 18th November 2008
Added TMagFtp3 and Xferdemo3 which support ICS V7.02 and later.
TMagFtpW renamed TMagFtp3W, TMagCopyW renamed TmagCopy3W, TMagHttpW
renamed TMagHttp3W, which all support ICS V7.02W and later.
TMagFtp3 and TMagFtp3W support XDMLSD and XCLMSD commands.
TMagFtp3W all FTP display events now UnicodeString
Xferdemo3W now keeps last 50 FTP host names and HTTP URLs and uses the TNT
Unicode controls to display Unicode with Delphi 2007.
Three executable demos are included, xferdemo3a.exe was built with Delphi
  2007 with an ANSI GUI, xferdemo3u.exe with Delphi 2009 and displays
  Unicode file names, and xferdemo3W with  Delphi 2007 and also displays
  Unicode file names


Release 2.5 - 18th November 2008
TMagFtp fixed PASS argument not being sanitised.
TMagCopy fixed FmtFileDirList with D2009.
Two executable demos are included, xferdemo2a.exe was built with Delphi
  2007 with an ANSI GUI, xferdemo2u.exe with Delphi 2009 and displays
  Unicode file names


Release 3.1 - 22nd October 2008
Note this version has not been tested with Delphi 7.
TMagXfer now includes some custom ICS units modified for use with
  Unicode with Delphi 2007, OverbyteIcsFtpcliW, OverbyteIcsFtpSrvWT,
  OverbyteIcsFtpSrvW and OverbyteIcsFtpSrvWC (note the FTP server is
  only partially done) and ics-internet sub-directory with matching
  demo applications.  Currently, these components are created in code for
  TMagXfer and the ICS demos, so don't need to installed in a package.
FmtFileDirListW and FmtFileDirW are now Unicode, but probably D2007 or
  later due to the use of WideFormat.
In TMagFileCopyW, ensure the Progress event is called during copying of
  first file, and when copying starts rather than after 2 secs.
The IgnoreFileExt property may now include file extensions that are a
  minimum two characters instead of four.
Fixed an FTP bug if PWD returned blank rather than '/' (Indy).


Release 3.0 - 22nd September 2008
TMagCopy Unicode version unit renamed MagentaCopyW with TMagFileCopyW
Support Unicode file listing and copying with Delphi 7 and later, all
  parameters are passed as WideStrings, but the events only show full
  Unicode characters with Delphi 2009.
DeleteEmptyDirs takes UStringArray arg for widestrings, not StringList
New SrcDirUList and TarDirUList props as UStringArray to set Unicode dirs
TMagHttp Unicode version unit renamed MagentaHttpW with TMagFileHttpW
  but does not support Unicode file names
TMagFtp Unicode version unit renamed MagentaFtpW with TMagFtpW
Support UTF8 FTP commands and file listings with ICS v7 only
Added magftpNoUtf8 property to turn off UTF8, and magftpIgnoreUtf8 if
  server will not turn it off
Support Unicode file listing and xfers with Delphi 7 and later, all
  parameters are passed as WideStrings, but the events only show full
  Unicode characters with Delphi 2009.
Check file names only have ANSI characters unless UTF8 is enabled.
Send HOST command before logon unless magftpNoHost specified (for virtual
  FTP servers)
Two executable demos are included, xferdemo3a.exe was built with Delphi
  2007 with an ANSI GUI, xferdemo3u.exe with Delphi 2009 and displays
  Unicode file names


Release 2.4 - 22nd September 2008
All components support SSL as standard.
TMagCopy supports Unicode file listing and copying with Delphi 2007 and
  earlier, but directory parameters must be passed as ANSI.
TMagFtp adds magftpNoUtf8 property to turn off UTF8 if the server defaults
  to UTF8 (as does FileZilla).
Don't attempt to access files or directories with Unicode substitution ?
  character
Don't keep upload resume files unless some data actually sent
Don't attempt to upload _xxx.ftp resume files
Uploading always allowed to create the base directory, and it now works
Workaround for WS_FTP MLST with 501 Invalid number of arguments with
  spaces by quoting file name
SSL send PBSZ before PROT to keep MS FTP7 happy
Two executable demos are included, xferdemo2a.exe was built with Delphi
  2007 with an ANSI GUI, xferdemo2u.exe with Delphi 2009 and displays
  Unicode file names


Release 2.3 - 18th August 2008
updated for latest ICS V6 and V7 for Delphi 2009 and Unicode
Replaced RX FormStorage with local INI file support so RX does not need
  be installed
Updated TMagCopy to fully support Unicode file names with Delphi 2009
No longer supplying compiled DCUs, since the component is now free.


Release 2.2 - 3rd March 2008
The demo application has a Clear logs button, several new FTP tick
  boxes, Bandwidth Limiting.
FTP MSLD fix for Serv-U where it only listed directories and no files
New FTP NoFeatCmd property which stops FEAT command being sent where
  servers have not implemented features 'correctly' causing FTP to fail
FTP uses the ALLO command to check space on server before uploads
FTP sends CLNT client string on logon
FTP uses SITE DMLSD command to list directories including subdirectory
   and SITE CMLSD for single dirs (ICS FTP server only at present)
LookupFTPReq and LookupFTPState moved to TFtpCli
FTP uses XMD5 command if MD5 not available
FTP logs time taken by MD5/CRC commands
FTP adds File/Delim logging for failed MD5/CRC on download and
  report MD5/CRC error for upload failed (not FTP response)
FTP no longer uses ModeZ for directory listing, except recursive
FTP adds ZlibNoCompExt property which is list of file extensions which
  should not use Mode Z, defaults to '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;
  .wmv;.mpg;.mp3;.jpg;.png;'
FTP adds ZlibMaxSize property maximum file size for Mode Z (servers may
   struggle with large files)
FTP does not attempt to resume xfer if partial file larger than new file
FTP does not repeatedly abort as errors reported, which may cause problem
   with Zlib errors
FTP adds MaxResumeAttempts property, default 10 (was fixed 5)
FTP adds MaxXferAttempts property, default 3
FTP check if download tmp file already open, and uses tmp2 instead
FTP check more than fMinResSize downloaded for file size mismatch in case
   file is smaller on resume
FTP downloads now gets server file size/date before each repeat attempt
   in case changed during the xfer
If FTP passive connection fails, Abort xfer so server cleans up
FTP now retries on 501 errors
Added MagFtpOps property to disable advanced features:
       magftpNoFeat, magftpNoZlib, magftpNoMd5Crc, magftpNoTmpFile
For FTP uploads, when no new files to upload, still delete old target
  files, and warn if ignoring delete old target files
All units report DelimActualSize in LogLevelDelimFile which if non-zero is
  actual size  relating to xfer duration (instead of the total file size)
HTTP fixes content field parsing for text failed if multiple arguments


Release 2.1 - 17th August 2007
Tested with Delphi 2007 which is the same packages as 2006
Fixed bug where FTP directory listings failed after a resumed download
was aborted (bug in ICS FTP, but fixed here as well)
Fixed bug where MLSD listings ignore single character dirs and files


Release 2.0 - 8th January 2007
First release to support ICS V6, only Delphi 7 and 2006.


Release 1.9 - 8th January 2006
Fixes for FTP transfers over 2 gigs with ICS V5.
Added new MagentaSubs package with common files.
Object code support for Delphi 2005 dropped.


Release 1.8 - 11th August 2006
FTP fix for failed resumed downloads, KeepAliveSecs for FTP,
better error messages, added FTP XCRC command

Release 1.7 - 16th March 2006
FTP fixes with bad MLST responses, ICS logger and ICS SSL v5g

Release 1.6 - 6th December 2005
Added FTP ModeZ support, HTTP Gzip, ICS SSL v5a support


Release 1.5 - 22nd November 2005
First commercial release to support ICS V5.



Copyright Information
---------------------

Magenta Systems File Transfer Components are copyrighted software, but
may be used without cost.

Magenta Systems Ltd
9 Vincent Road
Croydon
CR0 6ED
United Kingdom

Phone 020 8656 3636, International Phone +44 20 8656 3636
Fax 020 8656 8127, International Fax +44 20 8656 8127

Email: delphi@magsys.co.uk
Web: http://www.magsys.co.uk/delphi/



