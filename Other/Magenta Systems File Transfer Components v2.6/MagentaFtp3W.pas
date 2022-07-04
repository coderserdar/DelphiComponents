unit MagentaFtp3W;

// W version supports widestring/Unicode for Delphi 2007 and earlier
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
Updated by Angus Robertson, Magenta Systems Ltd, England, 17th May 2009
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

TMagFtpW is a high level FTP Delphi component that allows uploading or
downloading of multiple files from or to an FTP server, from a single
function call.  The component handles listing local and remote files,
including subdirectories, and comparing them to avoid unnecessary
transfers, selection using a file mask, deletion of old files, resuming
failed FTP downloads, unzipping of downloaded files, or zipping before
upload, deletion after uploading or downloading, archive copy after
upload.  A progress event provides various levels of information for
logging or display, depending upon application requirements, and allows
transfers to be cancelled.

TMagFtpW descends from ICS TFtpClient, and publishes all it's logon and proxy
properties and events.  The component is undocumented, except for source code
comments, but end user help is available describing most of the functionality in
Magenta Systems DUN Manager application (from http://www.magsys.co.uk/dunman/),
look under Scheduled Task Properties, FTP General, FTP Common, FTP Upload and FTP
Download. DUN Manager also provides a compiled test application for TMagFtp (but
DM source code is not available).

Requires Internet Component Suite (ICS) V7 from http://www.overbyte.be/,
dated 11th November 2008 or later, SVN revison 221 or later.

The latest versions of ICS may be downloaded from the ICS SubVersion server using a
subversion client such as TortoiseSVN.  Once your SVN client is installed, for
ICS-V7 you can browse to svn://svn.overbyte.be/ics/branches/icsv7 or
http://svn.overbyte.be:8443/svn/ics/branches/icsv7. All use usercode = ics and
password = ics for read access.

Compatible with Delphi 7/2005/2006/2007/2009
Tested with Windows 2000, XP, 2003, Vista and 2008

Requires Kevin Boylan's TVCLZip component for zipping from http://www.vclzip.net/,
if you don't purchase this component you will need to suppress DEFINE Zipping from
MAGZIP.INC so the zip code is not linked.

TMagFtpW is copyrighted software, but may be used freely.

Main functions:

FtpDir - build file directory from FTP server
FtpLogon - connect and logon to FTP server
DispFtpDir - logon and return formatted file directory from FTP server
FtpDownload - logon and download files from FTP server
FtpUpload - logon and upload files to FTP server
Cancel - abort FTP xfers



14 May 2001  - baseline
5 June 2001  - added ReplRO to replace read only files on download
8 June 2001  - added Fr to TFileRec elements so names are unique
25 June 2001 - added LogLevelDelimFile and LogLevelDelimTot to return info
                 for each file suitable for further processing
29 June 2001 - fixed problem with fMask not getting all files to check
9 July 2001  - added HostName1 and HostName2 as alternates
1 Aug 2001   - use AbortXfer instead of Abort, cleaner
6 Aug 2001   - added archive directory move after upload
8 Aug 2001   - added zipping of uploads and downloads using VCLZip component from http://vclzip.bizland.com/
2 Sept 2001  - more error handling in delete after ftp upload, fixed case comparison
12 Dec 2001  - added TTaskResult to distinguish results better
22 Dec 2001  - finally made zipping a conditional compile for those that have not bought it
8 Jan 2002   - fixed UNIX dates being last year if greater than today
             - don't change case of directories if mixed specified
21 Apr 2002  - free VCLZIP arhivestream (needed for 2.23)
11 July 2002 - added ResFailed and MinResSize, allow FTP resume of partial downloads
4 Sept 2002  - added IgnoreFileExt - list is tmp;ftp;xxx;etc
08 Oct 2002  - no longer using trailing slashes on directories internally, still seems to work
09 Dec 2002  - slight change in UNIX directory detection
               uploads, added UpImmed to optionally delete/move after each file done
               time comparisons, added UseUTC to optionally with UTC/GMT time
07 Jan 2003  - better error handling when connection lost
               use FEAT command to see what FTP server will do
               try and use MDTM and SIZE command to get and set upload time stamp
03 Feb 2003  - show FTP host being contacted at info log level
24 Apr 2003  - no longer supporting UTC time, upload MDTM now standard time
               download deleting empty directories
               building FTP dirs now uses real temp file to avoid conflicts between multiple DMs
2 May 2003   - changed GetDirList with new params
17 Jun 2003  - added duration (ms) to LogLevelDelimFile events
24 Aug 2003  - ensure unzip directory exists, might help trap unzipping errors
               unzip errors in task log
4 Oct 2003   - when deleting old local files, don't delete FTP/TMP resume files
26 Oct 2003  - support MLSD directory list command - but ws_ftp server returns local date, not UTC
               fixed double slash in FTP download PC name
               added SpecificFiles property with | delimited list passed in SrcFName, up and down
               added unzip error handler - VCLZIP 3 only
               check each file mod date and size before download, more accurate than DIR
               set upload date in UTC
               use streams for DIR listing to avoid temporary files
15 Dec 2003  - check both sockets are closed, otherwise abort, reset fCancelFlag at start
30 Dec 2003  - allow FTP DIR response to be logged using fDispRemList
               fixed a problem with a blank subdir stopping directory listing (introduced with DIR streams)
4 Jan 2003   - using magsubs1 for common stuff instead of magsubs4
             - using TMagFileCopy for events
11 Jan 2004  - made Delphi 5 compatible, SpecificFiles not supported (no TStringList.Delimiter)
12 Feb 2004  - clear progress event, correct upload none response message
28 Aug 2004  - added FtpLogoff, FtpDownFiles (same as FtpDownload but already logged-on)
               added FtpDownOneFile, FtpUpOneFile and FtpCheckFile
               FtpDir no longer changes directories when listing (except LIST dirname fails if it contains a space on IIS/5)
               added workaround for error sending PORT command when PO lost, causing xfer failure
               fUpImmed forced, by checking each upload file after it's done to avoid directory afterwards
               if remote directory listing fails for uploads, give up to stop all files being uploaded, unless replacing all files
               FCReplNewer also replaces if target file empty and source file not empty
               support MFMT modify file modification time command if available
               support MD5 get has sum for download or upload file check corruption
               repeat failed download up to three times if FailRepeat<>0
               upload to temporary file and rename once checked successfully
               uploads now supporting resume on failure
               fixed divide by zero error calculating total upload duration (fixed download already)
               override WaitUntilReady in ftpcli.pas to avoid slow down problems
12 Oct 2004  - added file sizes when upload fails due to wrong size
               always log single line LIST/MLSD used for checking single files
               messing with updating UTC time stamps
29 Nov 2004  - suppressed state/request logging, more cancel breakouts and logging
13 Jan 2005  - removed sleep from WaitUntilReady
14 Feb 2005  - count failed resumed uploads/downloads, limit retries to five
10 Apr 2005  - fixed FTP MS-DOS LIST date 12:30AM being converted to 12:30 not 00:30
             - trim src and tar directories
18 Apr 2005  - using new GetTickCountX functions that support wrapping at 49 days
30 July 2005 - TFindList moved to magclasses.pas
11 Aug 2005  - more error handling to trap lost connection during upload/download and prevent false OK
20 Aug 2005  - corrected false fail with no connection for all uploads
6 Sept 2005  - cleaned up unused variables, wait up to 5 secs for sockets to close after quit
             - 64-bit support, FtpCheckFile has 64-bit size (not backward compatible)
             - prevent too rapid progress messages, which slow down transfers, ProgressSecs = 5
             - FtpCli v2.100 fixes problem uploading files about 10K in size
19 Oct 2005  - fixed bug with short or zero upload or download duration showing as 120 hours
               don't retry for 501 permission error
3 Nov 2005   - latest ICS needs OnProgress64 not OnProgre64
5 Dec 2005   - now supporting ICS v5 SSL dated Nov 2005 or later
             - support Mode Z compression
26 Jan 2006  - support for new icslogger (not here), new SSL version
15 Feb 2006  - report error descriptions as well as numbers
16 Mar 2006  - check for single file use MDTM if MLST fails
             - ignore MLSD cdir and pdir lines with names
24 June 2006 - if xfer cancelled, set abort in progress to stop data being written
               resume download assume end of part file corrupt and start fMinResSize earlier
               increased fMinResSize default from 10K to 65K
               if xfer cancelled, still report download result
6 Aug 2006   - added KeepAliveSecs property in TFtpCli, if not zero sets Winsock keepalive
                  for control connection to stop it being closed by firewalls, typically 30 secs
               supporting XCRC command if MD5 command not available, to check files xfer'd OK
3 Sept 2006  - fixed some logging that broke on files large than 2 gigs
               fixed FtpCheckFile now allowing for MLST mixed case facts (Serv-U)
18 Sep 2006  - unit is now MagentaFtp supporting ICS V6
31 Oct 2006  - use callback during MD5/CRC calculations for progress and processmessages
6 Nov 2006   - new SSL session cache
6 Jan 2007   - allow for base directory with drive (ie c:) when setting root directory
               stopped using LIST path (CD instead) since some servers don't support it
19 July 2007 - ensure resume position reset before directory commands
               report DIR error if FtpCheckFile fails
               more logging for resumed downloads
6 Aug 2007   - MLSD listings don't ignore single character files and directories
03 Mar 2008  - MSLD fix for Serv-U where it only listed directories and no files
               new NoFeatCmd property which stops FEAT command being sent where
                 servers have not implemented features 'correctly' causing FTP to fail
               using ALLO command to check space on server before uploads
               send CLNT client string on logon
               use SITE DMLSD command to list directories including subdirectore
                  and SITE CMLSD for single dirs (ICS FTP server only at present)
               LookupFTPReq and LookupFTPState moved to TFtpCli
               LogLevelDelimFile has actual xfer size after duration, including failures
               using XMD5 if available, log time taken by MD5/CRC commands
               adding File/Delim logging for failed MD5/CRC on download and
                 report MD5/CRC error for upload failed (not FTP response)
               don't use ModeZ for directory listing, except recursive
               added ZlibNoCompExt property which is list of file extensions which
                 should no use Mode Z, defaults to '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;'
               added ZlibMaxSize property maximum file size for Mode Z (servers may struggle with large files)
               don't attempt to resume xfer if partial file larger than new file
               don't repeatedly abort as errors reported, which may cause problem with Zlib errors
               added MaxResumeAttempts property, default 10 (was fixed 5)
               added MaxXferAttempts property, default 3
               if download tmp file already open, use tmp2 instead
               check more than fMinResSize downloaded for file size mismatch in case file is smaller on resume
               when downloading, get server file size/date before each repeat attempt in case changed
               if passive connection fails, Abort xfer so server cleans up
               retry on 501 errors
               added MagFtpOps property to disable advanced features:
                      magftpNoFeat, magftpNoZlib, magftpNoMd5Crc, magftpNoTmpFile
               when no new files to upload, still delete old target files
               warn if ignoring delete old target files
11 Jun 2008  - ensure lists set as sorted after sorting
7 Aug 2008   - 2.3 - updated for latest ICS V6 and V7, and for Delphi 2009 compatibility
               Note: FTP does not yet support Unicode commands
22 Sept 2008 - 3.0 - Unicode vesion unit renamed MagentaFtpW with TMagFtpW
               support UTF8 FTP commands and file listings with ICS V7 - currently ICS V6 not supported
               added magftpNoUtf8 property to turn off UTF8, and magftpIgnoreUtf8 if server will not turn it off
               support Unicode file listing and xfers with Delphi 2007 and earlier
                 (note this meant replacing vast numbers of Ansi functions with widestring versions)
               don't attempt to access files or directories with Unicode substitution ? character
               check file names only have ANSI characters unless UTF8 is enabled
               don't keep upload resume files unless some data actually sent
               don't attempt to upload _xxx.ftp resume files
               uploading always allowed to create base directory, and it now works properly
               WS_FTP MLST with 501 Invalid number of arguments with spaces, so try quoting file name
               SSL send PBSZ before PROT to keep MS FTP7 happy
               send HOST command before logon unless magftpNoHost specified (for virtual FTP servers)
22 Oct 2008  - 3.1 - fixed zipping for Unicode changes, but still VCLZip 3
               fixed bug if PWD returned blank rather than '/' (Indy)
               directory listings now Unicode
               using private wide ICS units, OverbyteIcsFtpcliW, OverbyteIcsFtpSrvWT which support Unicode with D2007
               ensure Progress event called xfer starts rather than after 2 secs
18 Nov 2008  - 3.2 - renamed to MagentaFtp3w, support XDMLSD and XCLMSD commands
               fixed PASS argument not being sanitised
               all FTP display events now UnicodeString
17 May 2009  - 3.4 - don't ignore failed MD5/XMD5 command but report error, prefer MD5 to XMD5
               added MaskLocDir and MaskRemDir flags to take masked directory from SrcFName and
                 add to local and/or remote directory, typically for dated directories
               Unicode MD5sum and CRC32B, add magftpNoMd5 and magftpNoCrc to allow them to be tested separately



pending - use VclZip v4 widestring version



Unicode Compatibility with various web servers
Note: UTF8 support may not include Unicode characters outside ANSI codeset
OPTS UTF8 or OPTS UTF8 ON command must be sent before most servers support
UTF8 file listings or uploads

ICS V6 - does not support UTF8

ICS V7 - support UTF8, fully Unicode capable when build with Delphi 2009 or later,
defaults to UTF8 OFF and returns ANSI file listings, OPTS UTF8 or OPTS UTF8 ON
enables UTF8 file listings and uploads

ICS V7 Wide - support UTF8, fully Unicode capable when build with Delphi 2007 or later
defaults to UTF8 OFF and returns ANSI file listings, OPTS UTF8 or OPTS UTF8 ON
enables UTF8 file listings and uploads

Microsoft IIS/5 and IIS/6 no UTF8 support

Microsoft FTP7 IIS/7 for Windows 2008 - fully Unicode capable, defaults to UTF8 OFF and
returns ANSI file listings, OPTS UTF8 or OPTS UTF8 ON enables UTF8 file listings and uploads

FileZilla Server - fully Unicode capable, defaults to UTF8 ON returning UFT8 file listings
and uploads, but can be disabled with OPTS UTF8 OFF command when listings revert to ANSI

WS_FTP Server 6.1.1 and 7.0.0 - fully Unicode capable, defaults to UTF8 OFF and returns
ANSI file listings, OPTS UTF8 or OPTS UTF8 ON enables UTF8 file listings and uploads,
UTF8 can not be set off so don't send command if ANSI needed
MLST fails with 501 if file name includes a space

RhinoSoft Serv-U FTP Server v7.2 - no Unicode support, defaults to UTF8 OFF but returns
UTF8 file listings (with ? for non-supported unicode characters), OPTS UTF8 ON enables
UTF8 uploads.  OPTS UTF8 OFF command reverts listings to ANSI
MLST fails with 550 for file names with any UTF8 escaped characters
MDTM YYYYMMDDHHMMSS fail if timezone used (worked in earlier releases)
v7.3 fixes the MLST and MDTM errors, and adds MFMT, v8 will support Unicode

Gene6 FTP Server v3.10.0 - fully Unicode capable, defaults to UTF8 OFF and returns ANSI
file listings, OPTS UTF8 or OPTS UTF8 ON enables UTF8 file listings and uploads, UTF8
can not be set off so don't send command if ANSI needed.
MLSD fails with a Unicode sub-directory argument (but CWD works OK with Unicode)

Indy 10 FTP Server component built with Delphi 2007 - no UTF8 support (probably,
but may be configured using some hidden option)
MLST fails with 250 end if a file name is passed


}
interface

{$I OverbyteIcsDefs.inc}
{$I MAGZIP.INC}
{$I OverbyteIcsZlib.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, StrUtils,
  MagSubs1, MagSubs4, MagClasses, MagentaCopy3W,
  OverbyteIcsFtpcliW, OverbyteIcsFtpSrvWT, OverbyteIcsWSocket,
  OverbyteIcsMd5, OverbyteIcsCRC, OverbyteIcsLibrary,
  OverbyteIcsUtils
  {$IFDEF Zipping} , VCLZip, VCLUnZip, kpZipObj {$ENDIF}
  {$IFDEF USE_SSL} , OverbyteIcsSSLEAY, OverbyteIcsLIBEAY,
  OverbyteIcsSslSessionCache {$ENDIF} ;

type
// host type, for directory listing
    THostType = (FTPTYPE_NONE, FTPTYPE_UNIX, FTPTYPE_DOS, FTPTYPE_MVS,
                 FTPTYPE_AS400, FTPTYPE_MLSD) ;
    TXferMode = (XferModeBinary, XferModeAscii) ;
    TBulkMode = (BulkModeNone, BulkModeDownload, BulkModeUpload) ;
    TCaseFile = (FileLowerCase, FileMixedCase) ;
    TFtpType  = (FtpTypeNone,
                 FtpTypeAuthSslCtl, FtpTypeAuthSslData, FtpTypeAuthSslBoth,
                 FtpTypeConnSslCtl, FtpTypeConnSslData, FtpTypeConnSslBoth);
    { AuthSsl    = explicit encryption on port 21 using AUTH command }
    { ConnSsl    = implicit encryption on port 990 forced on connection }
    TMagFtpOpt  = (magftpNoFeat, magftpNoZlib, magftpNoMd5Crc, magftpNoTmpFile,
                   magftpNoUtf8, magftpIgnoreUtf8, magftpNoHost,
                   magftpNoMd5, magftpNoCrc);   // 15 Apr 2009
    TMagFtpOpts = set of TMagFtpOpt;

const
    FtpTypeStrings: array[Low(TFtpType)..High(TFtpType)] of String =
    ('Not Secured',
     'Secured SSL/TLS Auth - Control Only',
     'Secured SSL/TLS Auth - Data Only',
     'Secured SSL/TLS Auth - Control and Data',
     'Secured SSL/TLS Conn - Control Only',
     'Secured SSL/TLS Conn - Data Only',
     'Secured SSL/TLS Conn - Control and Data') ;

    ResInfServer = 0 ; ResInfFName = 1 ; ResInfStamp = 2 ; ResInfSize = 3 ;
    ResInfAttempts = 4 ; ResInfLastBytes = 5 ;

type

{$IFDEF USE_SSL}
  TMagFtpW = class(TSslFtpClientW)
{$ELSE}
  TMagFtpW = class(TFtpClientW)
{$ENDIF}
  private
    { Private declarations }
        fCancelFlag: boolean ;
        fProgFileSize: Int64 ;
        fLoggedIn: boolean ;
        fProgMessBase: UnicodeString ;
        fFtpErrFlag: boolean ;
        fMagFileCopy: TMagFileCopyW ;
        fFtpType: TFtpType ;   // 11 Nov 2005, settable even if no SSL
        fSslCertCheck: TSslCertCheck ;
        fFtpSslPort: string ;
{$IFDEF USE_SSL}
        fSslSessCache: boolean ;
        fSslContext: TSslContext ;
        fExternalSslSessionCache: TSslAvlSessionCache ;
{$ENDIF}

  protected
    { Protected declarations }
        fBulkMode: TBulkMode ;
        fHostName1: string ;
        fHostName2: string ;
        fSrcDir: UnicodeString ;
        fSrcFName: UnicodeString ;
        fTarDir: UnicodeString ;
        fCopyType: TFileCopyType ;
        fSubDirs: Boolean ;
        fDelDone: Boolean ;
        fDelOldTar: Boolean ;
        fMask: Boolean ;
        fPrev: Boolean ;
        fRepl: TFileCopyRepl ;
        fReplRO: boolean ;
        fSafe: Boolean ;
        fTimeStamp: boolean ;
        fLocalHost: Ansistring ;
        fDispLog: boolean ;
        fDispFiles: boolean ;
        fDispLDir: boolean ;
        fDispRDir: boolean ;
        fHostType: THostType ;
        fXferMode: TXferMode ;
        fCaseFile: TCaseFile ;
        fDiffStampMins: integer ;
        fCopyEvent: TBulkCopyEventW ;
        fTotProcFiles: integer ;
        fProcOKFiles: integer ;
        fDelOKFiles: integer ;
        fProcFailFiles: integer ;
        fSkippedFiles: integer ;
        fDelOKBytes: int64 ;
        fReqResponse: String ;
        fServRootDir: UnicodeString ;
        fServBaseDir: UnicodeString ;
        fMaxAttempts: integer ;     // logon attempts
        fAttemptDelay: integer ;
        fUpArchDir: UnicodeString ;
        fUpArchive: Boolean ;
        fResFailed: Boolean ;
        fMinResSize: Int64 ;        // also used for Resume Overlap 24 June 2006
        fIgnoreFileExt: UnicodeString ;
        fUpImmed: Boolean ;                        // 17 Aug 2004 - now ignored
        fSpecificFiles: boolean ;    // 14 Oct 03
        fDispRemList: boolean ;      // 30 Dec 2003
        fCurRemDir: UnicodeString ;  // 10 Aug 2004
        fFailRepeat: integer ;       // 20 Aug 2004
        fLastProgTick: longword ;    // 5 Sept 2005
        fProgressSecs: integer ;     // 5 Sept 2005
        fUseCompression: boolean ;   // 2 Dec 2005
        fUsingCompression: boolean ; // 5 Dec 2005
        fTimeZoneStr: string ;       // 8 Nov 2007
        fZlibNoCompExt: string ;     // 2 Dec 2007
        fZlibMaxSize: int64 ;        // 9 Dec 2007 - zero means no compression
        fMaxResumeAttempts: integer ; // 31 Dec 2007  resume attempts
        fMagFtpOpts: TMagFtpOpts;    // 5 Jan 2008
        fMaskLocDir: boolean ;       // 8 Apr 2009
        fMaskRemDir: boolean ;       // 8 Apr 2009
    {$IFDEF Zipping}
        fZipDownDel: Boolean ;
        fZipped: Boolean ;
        fZipExtFmt: TZipExtFmt ;
        fZipPath: TZipPath ;
        fZipDir: String ;
     {$ENDIF}

        procedure doCopyEvent (const LogLevel: TLogLevel; const Info: UnicodeString) ;
        procedure onFtpClientProg64(Sender: TObject; Count: Int64;
                                                     var Abort: Boolean);
        procedure onFtpClientDisplay(Sender: TObject; var Msg: UnicodeString);
        procedure onFtpError(Sender: TObject; var Msg: UnicodeString);
//        procedure OnFtpCommand (Sender: TObject; var Cmd: String) ;
        procedure OnFtpResponse (Sender: TObject) ;
        procedure OnFtpSessConn (Sender: TObject; Error: word) ;
        procedure OnFtpSessClosed (Sender: TObject; Error: word) ;
        procedure OnFtpRequestDone (Sender: TObject; RqType: TFtpRequest; Error: Word) ;
        procedure OnFtpStateChange (Sender: TObject) ;
        procedure OnFTPSocksConnected (Sender: TObject; Error: word) ;
        procedure onMagCopyEvent (LogLevel: TLogLevel ; Info: UnicodeString ;
                                                  var Cancel: boolean) ;
        procedure EndUnZipEvent (Sender: TObject; FileIndex: Integer; FName: String) ;
        procedure UnZipHandleMessage(Sender: TObject;
                      const MessageID: Integer; const Msg1, Msg2: String;
                      const flags: Cardinal; var Return: Integer);
        function IntDownOne (const RemDir, RemFile, RemFull, LocFileFull: UnicodeString ;
                               const RFSize: Int64; RFileUDT: TDateTime): integer ;

        function IntUpOne (const LocFileFull, RemDir, RemFile: UnicodeString ;
                                 const RFSize: Int64; RFileUDT: TDateTime): integer ;
        function WaitUntilReady : Boolean; Override ;
        procedure SetSrcDir (S: UnicodeString) ;
        procedure SetTarDir (S: UnicodeString) ;
        procedure onZlibProg (Sender: TObject; Count: Int64; var Cancel: Boolean); // 9 Dec 2007

{$IFDEF USE_SSL}
        procedure OnFTPSslVerifyPeer(Sender: TObject; var Ok: Integer;
          Cert : TX509Base);
        procedure OnFTPSslCliNewSession(Sender: TObject; SslSession: Pointer;
                                        WasReused: Boolean; var IncRefCount : Boolean);
        procedure OnFTPSslCliGetSession(Sender: TObject; var SslSession: Pointer;
                                                                var FreeSession : Boolean);
        procedure OnFTPSslHandshakeDone(Sender: TObject; ErrCode: Word;
                                          PeerCert: TX509Base; var Disconnect: Boolean);
        procedure OnFTPSslCliCertRequest(Sender: TObject; var Cert: TX509Base);
{$ENDIF}
  public
    { Public declarations }

        SrcFiles: TFileRecsW ;
        SrcFileList: TFindList ;
        TotSrcFiles: integer ;
        TarFiles: TFileRecsW ;
        TarFileList: TFindList ;
        TotTarFiles: integer ;
        CurDelFiles: integer ;
        Utf8DiagFlag: boolean ;  // 13 Nov 2008

        constructor Create (Aowner: TComponent) ; override ;
        destructor  Destroy ; override ;
        function DispFtpDir (var dirlisting: UnicodeString): TTaskResult ;
        function FtpDir (var FtpFiles: TFileRecsW;
                                    var FtpFileList: TFindList): TTaskResult ;
        function FtpLogon: TTaskResult ;
//      function LookupFTPReq (const RqType: TFtpRequest): String; moved to TFtpCli
//      function LookupFtpState (const FtpState: TFtpState): String;
        function FtpDownload (const CheckFiles: boolean): TTaskResult ;
        function FtpUpload (const CheckFiles: boolean): TTaskResult ;
        procedure Cancel ;
        function UnpackFtpFDir (DirStream: TStream; RemDir, BaseDir: UnicodeString;
                Level: integer ; var DCodePage: Cardinal ; var HostType: THostType;
                    var TotFiles: integer; var RemFiles: TFileRecsW): integer ;
        procedure FtpLogoff ;
        function FtpDownFiles (const CheckFiles: boolean): TTaskResult ;
        function FtpDownOneFile (const FdirSrc, Fnamesrc, Fnametar: UnicodeString ;
                                            Replopt: TFileCopyRepl) : TTaskResult ;
        function FtpUpOneFile (const LocFileFull, RemTarDir, RemTarFile: UnicodeString;
                                            Replopt: TFileCopyRepl) : TTaskResult ;
        function FtpCheckFile (const RemDir, RemFile: UnicodeString ;
                                 var FSize: Int64; var FileUDT: TDateTime): boolean;

{ Pass it the string that you get back when you get the
{ Current path for the FTP site. It returns
{   FTPType of   FTP_TYPE_NONE or FTP_TYPE_MVS depending
{ if it looks like the folder string format is that of an VMS system
{ or not. After using this function, you will need to use the FTP_ConvertLines
{ to further determine if the server type is DOS/WINDOWS or UNIX or AS-400}
        Procedure SetupVMS(const BaseFolder: string; var HostType: THostType);

  published
    { Published declarations }

    property BulkMode: TBulkMode       read fBulkMode       write fBulkMode ;
    property HostName1: string         read fHostName1      write fHostName1 ;
    property HostName2: string         read fHostName2      write fHostName2 ;
    property SrcDir: UnicodeString     read fSrcDir         write SetSrcDir ;
    property SrcFName: UnicodeString   read fSrcFName       write fSrcFName ;
    property TarDir: UnicodeString     read fTarDir         write SetTarDir ;
    property CopyType: TFileCopyType   read fCopyType       write fCopyType ;
    property SubDirs: Boolean          read fSubDirs        write fSubDirs ;
    property DelDone: Boolean          read fDelDone        write fDelDone ;
    property DelOldTar: Boolean        read fDelOldTar      write fDelOldTar ;
    property Mask: Boolean             read fMask           write fMask ;
    property Prev: Boolean             read fPrev           write fPrev ;
    property Repl: TFileCopyRepl       read fRepl           write fRepl ;
    property ReplRO: boolean           read fReplRO         write fReplRO ;
    property Safe: Boolean             read fSafe           write fSafe ;
    property TimeStamp: boolean        read fTimeStamp      write fTimeStamp ;
    property LocalHost: AnsiString     read fLocalHost      write fLocalHost ;
    property DispLog: boolean          read fDispLog        write fDispLog ;
    property DispFiles: boolean        read fDispFiles      write fDispFiles ;
    property DispLDir: boolean         read fDispLDir       write fDispLDir ;
    property DispRDir: boolean         read fDispRDir       write fDispRDir ;
    property HostType: THostType       read fHostType       write fHostType ;
    property XferMode: TXferMode       read fXferMode       write fXferMode ;
    property CaseFile: TCaseFile       read fCaseFile       write fCaseFile ;
    property DiffStampMins: integer    read fDiffStampMins  write fDiffStampMins ;
    property FailRepeat: integer       read fFailRepeat     write fFailRepeat ;     // 20 Aug 2004
    property ProgressSecs: integer     read fProgressSecs   write fProgressSecs ;   // 5 Sept 2005
    property CopyEvent: TBulkCopyEventW read fCopyEvent      write fCopyEvent ;
    property FtpType: TFtpType         read fFtpType        write fFtpType ;      // 11 Nov 2005
    property UseCompression: boolean   read fUseCompression write fUseCompression ; // 3 Dec 2005
{$IFDEF USE_SSL}
    property SslSessCache: boolean     read fSslSessCache   write fSslSessCache ; // 11 Nov 2005
    property SslCertCheck:TSslCertCheck read fSslCertCheck  write fSslCertCheck ; // 11 Nov 2005
    property FtpSslPort: string        read fFtpSslPort     write fFtpSslPort ;   // 11 Nov 2005
{$ENDIF}
    property TotProcFiles: integer     read fTotProcFiles ;
    property ProcOKFiles: integer      read fProcOKFiles ;
    property DelOKFiles: integer       read fDelOKFiles ;
    property ProcFailFiles: integer    read fProcFailFiles ;
    property ReqResponse: string       read fReqResponse ;
    property SkippedFiles: integer     read fSkippedFiles ;
    property ServRootDir: UnicodeString read fServRootDir ;
    property ServBaseDir: UnicodeString read fServBaseDir ;
    property LoggedIn: boolean         read fLoggedIn ;

    property MaxAttempts: integer      read fMaxAttempts    write fMaxAttempts ;   // logon attempts
    property AttemptDelay: integer     read fAttemptDelay   write fAttemptDelay ;
    property PassiveX: Boolean         read FPassive        write FPassive;
    property UpArchDir: UnicodeString  read fUpArchDir      write fUpArchDir ;
    property UpArchive: Boolean        read fUpArchive      write fUpArchive ;
    property ResFailed: Boolean        read fResFailed      write fResFailed ;
    property MinResSize: Int64         read fMinResSize     write fMinResSize ;
    property IgnoreFileExt: UnicodeString  read fIgnoreFileExt  write fIgnoreFileExt ;
    property UpImmed: Boolean          read fUpImmed        write fUpImmed ;
    property SpecificFiles: Boolean    read fSpecificFiles  write fSpecificFiles ;
    property DispRemList: boolean      read fDispRemList    write fDispRemList ;
    property ZlibNoCompExt: string     read fZlibNoCompExt  write fZlibNoCompExt ; // 2 Dec 2007
    property ZlibMaxSize: Int64        read fZlibMaxSize    write fZlibMaxSize ;   // 9 Dec 2007
    property MaxResumeAttempts: integer  read fMaxResumeAttempts write fMaxResumeAttempts ; // 31 Dec 2007
    property MagFtpOpts: TMagFtpOpts   read fMagFtpOpts     write fMagFtpOpts ;    // 5 Jan 2008
    property MaskLocDir: boolean       read fMaskLocDir    write fMaskLocDir ;     // 8 Apr 2009
    property MaskRemDir: boolean       read fMaskRemDir    write fMaskRemDir ;     // 8 Apr 2009
  {$IFDEF Zipping}
    property ZipDownDel: Boolean       read fZipDownDel     write fZipDownDel ;
    property Zipped: Boolean           read fZipped         write fZipped ;
    property ZipExtFmt: TZipExtFmt     read fZipExtFmt      write fZipExtFmt ;
    property ZipPath: TZipPath         read fZipPath        write fZipPath ;
    property ZipDir: String            read fZipDir         write fZipDir ;
  {$ENDIF}

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Magenta Systems', [TMagFtpW]);
end;

function GetWinsockErr (error: integer): string ;
begin
    result := WSocketErrorDesc (error) + ' (' + IntToStr (error) + ')' ;
end ;

function atoi64(value : String) : Int64;
var
    i : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    while (i <= Length(Value)) and (Value[i] >= '0') and (Value[i] <= '9')do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
end;

constructor TMagFtpW.Create(Aowner:TComponent);
begin
    inherited create(AOwner);

 // winsock bug fix for fast connections
    FControlSocket.ComponentOptions := [wsoNoReceiveLoop] ;
    FDataSocket.ComponentOptions := [wsoNoReceiveLoop] ;
    fMagFileCopy := TMagFileCopyW.Create (self) ;
    fMagFileCopy.CopyEvent := onMagCopyEvent ;
    SrcFileList := TFindList.Create ;
    TarFileList := TFindList.Create ;
    SetLength (SrcFiles, 0) ;
    SetLength (TarFiles, 0) ;
    TotSrcFiles := 0 ;
    TotTarFiles := 0 ;
    fTotProcFiles := 0 ;
    fProcOKFiles := 0 ;
    fProcFailFiles := 0 ;
    fSkippedFiles := 0 ;
    fCancelFlag := false ;
    fLoggedIn := false ;
    fMaxAttempts := 3 ;
    fAttemptDelay := 5 ;   // seconds
    fProgressSecs := 2 ;   // update progress every two seconds default
    fUseCompression := false ;
    fResFailed := false ;
    fMinResSize := 65535 ;   // also used for resume overlap
    fFailRepeat := 3 ;       // 31 Dec 2007 was 0
    fSpecificFiles := false ;
    fDispRemList := false ;
    fCurRemDir := 'xXx' ;
    onDisplay := onFTPClientDisplay ;
    OnProgress64 := onFTPClientProg64 ;
    onError := onFtpError ;  // this event stops FtpClient raising exceptions
//    OnCommand := onFTPCommand ;
    OnResponse := onFTPResponse ;
    OnSessionConnected := onFTPSessConn ;
    OnSessionClosed := onFTPSessClosed ;
    OnRequestDone := onFTPRequestDone ;
    onStateChange := onFTPStateChange ;
    FControlSocket.OnSocksConnected := OnFTPSocksConnected ;
    fFtpType := FtpTypeNone ;  // 11 Nov 2005, even for no SSL
    fSslCertCheck := SslCCNone ;
    fFtpSslPort := '990' ;
    fZlibNoCompExt := '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;'; // 2 Dec 2007
    fZlibMaxSize := 500000000 ;  // 9 Dec 2007 - 500 meg
    OnZlibProgress := onZlibProg ; // 9 Dec 2007 not working yet...
    fMaxResumeAttempts := 10 ;     // 31 Dec 2007
    fMagFtpOpts := [] ;  // 5 Jan 2008
{$IFDEF USE_SSL}
    fSslSessCache := false ; // true ;
    fExternalSslSessionCache := nil ;
    OnSslVerifyPeer := onFTPSslVerifyPeer ;
    OnSslCliGetSession := onFTPSslCliGetSession ;
    OnSslCliNewSession := onFTPSslCliNewSession ;
    OnSslHandshakeDone := onFTPSslHandshakeDone ;
    OnSslCliCertRequest := onFTPSslCliCertRequest ;
    fSslContext := TSslContext.Create (self) ;
    SslContext := fSslContext ;
    fSslContext.SslVerifyPeer := false ;
 {      fSslContext.SslCertFile    := CertFileEdit.Text;
        fSslContext.SslPassPhrase  := PassPhraseEdit.Text;
        fSslContext.SslPrivKeyFile := PrivKeyFileEdit.Text;
        fSslContext.SslCAFile      := CAFileEdit.Text;
        fSslContext.SslCAPath      := CAPathEdit.Text;
        fSslContext.SetAcceptableHostsList(AcceptableHostsEdit.Text);
 }
{$ENDIF}
end ;

destructor TMagFtpW.Destroy;
begin
    fMagFileCopy.Free ;
    SrcFileList.Free  ;
    TarFileList.Free ;
{$IFDEF USE_SSL}
    FreeAndNil (fExternalSslSessionCache) ;
    FreeAndNil (fSslContext) ;
{$ENDIF}
    inherited Destroy;
end;

{$IFDEF USE_SSL}
procedure TMagFtpW.OnFTPSslVerifyPeer(Sender: TObject; var Ok: Integer;
  Cert : TX509Base);
begin
    { Alternate verification takes place in event HandshakeDone, we }
    { accept anything temporarily. }
    if Ok <> 0 then
        Cert.CustomVerifyResult := X509_V_OK;
    OK := 1;
    doCopyEvent (LogLevelDiag, 'Received SSL certificate; Subject: ' +
            Cert.SubjectOneLine + ';Issuer:  ' + Cert.IssuerOneLine +
                                   ';Verify result: ' + Cert.VerifyErrMsg);
end ;

procedure TMagFtpW.OnFTPSslCliNewSession(Sender: TObject; SslSession: Pointer;
                                WasReused: Boolean; var IncRefCount : Boolean);
var
    FtpCli: TSslFtpClientW ;
begin
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    doCopyEvent (LogLevelDiag, 'Starting SSL Session');
    if not fSslSessCache then Exit;
    if (not WasReused) then
    begin
        FtpCli := Sender as TSslFtpClientW ;
        fExternalSslSessionCache.CacheCliSession (SslSession,
            FtpCli.ControlSocket.PeerAddr + FtpCli.ControlSocket.PeerPort,
                                                           IncRefCount);
        doCopyEvent (LogLevelDiag, 'Cache SSL Session: New');
    end
    else
        doCopyEvent (LogLevelDiag, 'Cache SSL Session: Reuse');
    IncRefCount := false ;
end ;

procedure TMagFtpW.OnFTPSslCliGetSession(Sender: TObject; var SslSession: Pointer;
                                                        var FreeSession : Boolean);
var
    FtpCli: TSslFtpClientW ;
begin
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    if not fSslSessCache then Exit;
    doCopyEvent (LogLevelDiag, 'Check for Old SSL Session');
    FtpCli := Sender as TSslFtpClientW ;
    SslSession := fExternalSslSessionCache.GetCliSession(
             FtpCli.ControlSocket.PeerAddr + FtpCli.ControlSocket.PeerPort,
                                                              FreeSession);
    FreeSession := True;
end ;

procedure TMagFtpW.OnFTPSslHandshakeDone(Sender: TObject; ErrCode: Word;
                                  PeerCert: TX509Base; var Disconnect: Boolean);
var
    CertChain : TX509List;
    FtpCli: TSslFtpClientW ;
//    Msg       : String;
//    I         : Integer;
//    IP        : String;
//    Hash      : String;
begin
    FtpCli := Sender as TSslFtpClientW ;
    with FtpCli.ControlSocket do
    begin
        if ErrCode = 0 then
            doCopyEvent (LogLevelDiag, 'SSL Handshake Done OK, Secured with ' +
                SslVersion + ', Cipher ' + SslCipher + ', Secret Bits ' +
                IntToStr (SslSecretBits) + ' (Total ' + IntToStr (SslTotalBits) + ')')
        else
            doCopyEvent (LogLevelDiag, 'SSL Handshake Error #' + IntToStr(ErrCode));
        CertChain := SslCertChain;
    end ;
    doCopyEvent (LogLevelDiag, IntToStr(CertChain.Count) + ' Certificate(s) in the verify chain') ;
    if (ErrCode <> 0) or (FtpCli.ControlSocket.SslSessionReused) or
       not FtpCli.SslContext.SslVerifyPeer then
        Exit;
    exit ;

(*    IP   := FtpCli.GetPeerAddr;
    Hash := PeerCert.Sha1Hash;

    if FtpCli.SslAcceptableHosts.IndexOf(IP + Hash) > -1 then
        Exit;

    FtpCli.Abort;

    if CertChain.Count > 0 then
    begin
        Msg := 'Certificates in chain:'+ #13#10;
        for I := 0 to CertChain.Count -1 do
        begin
            if Length(Msg) > 0 then
                Msg := Msg + #13#10;
            Msg := Msg +  IntToStr(I + 1) + ')' +
                ' SubjectCommonName: ' + CertChain[I].SubjectCName + #13#10 +
                ' Last VerifyResult: ' + CertChain[I].VerifyErrMsg + #13#10;
        end;
    end;

    { Post connection check in case of SSL handshake went well.              }
    { Checks whether argument HostOrIp matches dnsName or commonName         }
    { of a peer certificate, this is the string-argument of the function.    }
    { For demo purposes a simple, constant string is passed.                 }
    if (not PeerCert.PostConnectionCheck('www.overbyte.be')) then
    begin
        if Length(Msg) > 0 then
            Msg := Msg + #13#10;
        if MessageDlg(Msg + 'Post connection check:'#13#10 +
                      'The name specified in the peer certificate is '#13#10 +
                      'invalid or does not match the site!'#13#10#13#10 +
                      'Do you want to trust the connection anyway?',
                       mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
            Disconnect := True;
            Display('Post connection check failed, peer will be disconnected');
        end
        else begin
            { Add peer cert to the trusted certs. }
            FtpCli.SslAcceptableHosts.Add(IP + Hash);
            PostMessage(Handle, WM_SSL_NOT_TRUSTED, 0, 0);
        end;
    end
    else begin
        if MessageDlg(Msg + #13#10 +
                      'Peer certificate was issued to the site.'#13#10#13#10 +
                      'Do you want to trust the connection anyway?',
                       mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
            Disconnect := True;
            Display('Custom verification failed, peer will be disconnected');
        end
        else begin
            { Add peer cert to the trusted certs. }
            FtpCli.SslAcceptableHosts.Add(IP + Hash);
            PostMessage(Handle, WM_SSL_NOT_TRUSTED, 0, 0);
        end;
    end;  *)
end ;

procedure TMagFtpW.OnFTPSslCliCertRequest(Sender: TObject; var Cert: TX509Base);
//var
//    X : TX509Base;
begin
    doCopyEvent (LogLevelDiag, 'Certificate Request Ignored') ;
    { A very simple test of the SslCliCertRequest event.               }
    { This event is triggered only if CertFileEdit.Text is empty,      }
    { the server requested a certificate from the client,              }
    { and of course only in case of the SSL session wasn't reused.     }
 (*   if not Assigned(FClientCerts) then
    begin
        { Create a pool of client certs }
        ClientCertDlg.CertListBox.Clear;
        FClientCerts := TX509List.Create(Self);
        try
            X := FClientCerts.Add;
            X.LoadFromPemFile('01cert.pem');
            X.PrivateKeyLoadFromPemFile('01key.pem', 'password');
            ClientCertDlg.CertListBox.Items.Add(X.SubjectOneLine);
            X := FClientCerts.Add;
            X.LoadFromPemFile('client.pem', True, 'password');
            ClientCertDlg.CertListBox.Items.Add(X.SubjectOneLine);
        except
            FreeAndNil(FClientCerts);
            raise
        end;
    end;
    ClientCertDlg.CertListBox.ItemIndex := 0;
    if ClientCertDlg.ShowModal = mrOK then
        Cert := FClientCerts[ClientCertDlg.CertListBox.ItemIndex];   *)
end;
{$ENDIF}

procedure TMagFtpW.SetSrcDir (S: UnicodeString) ;
begin
    fSrcDir := Trim (S) ;
end ;

procedure TMagFtpW.SetTarDir (S: UnicodeString) ;
begin
    fTarDir := Trim (S) ;
end ;

function TMagFtpW.WaitUntilReady : Boolean;
begin
    Result    := TRUE;           { Assume success }
    FTimeStop := LongInt(GetTickCount) + LongInt(FTimeout) * 1000;
    while TRUE do begin
        if FState in [ftpReady, ftpInternalReady] then begin
            { Back to ready state, the command is finished }
            Result := (FRequestResult = 0);
            break;
        end;

        if {$IFNDEF NOFORMS} Application.Terminated or
           {$ELSE}           Terminated or
           {$ENDIF}
           ((FTimeout > 0) and (LongInt(GetTickCount) > FTimeStop)) then begin
            { Timeout occured }
            AbortAsync;
            FErrorMessage := '426 Timeout';
            FStatusCode   := 426;
            Result        := FALSE; { Command failed }
            break;
        end;
        MessagePump;
    end;
end;

procedure sysDelayX (aMs: longword);
var
    Trg: longword;
begin
    Trg := GetTrgMsecs (aMs) ;
    while True do
    begin
        Application.ProcessMessages;
        if Application.Terminated then break ;
        if TestTrgTick (Trg) then break ;
    end ;
end;

(*
APPLE UNIX TYPE
drwx--x--x   2 64        512 Feb  5 06:30 mpo
-rw-r--r--   1 root        0 Nov 11  1999 mswin.qif
drwxr-xr-x   2 57        512 Nov  5  1999 neural
d-wx-wx--x   3 root     1536 Feb  4 23:30 outgoing
drwxr-xr-x   3 208       512 Nov  5  1999 papers
drwxr-xr-x   2 134       512 Nov  5  1999 patch
lrwxr-xr-x   1 root       12 Jan 26 19:00 pii -> winsock2/pii
drwxr-xr-x   2 78        512 Nov  5  1999 pld_fpga
drwxr-xr-x   2 770      1024 Jan 18 00:32 reqadm
drwxr-xr-x   3 70        512 Nov  5  1999 rmx


UNIX TYPE

drwx--x--x   2 64       root          512 Feb  5 06:30 mpo
-rw-r--r--   1 root     root            0 Nov 11  1999 mswin.qif
drwxr-xr-x   2 57       root          512 Nov  5  1999 neural
d-wx-wx--x   3 root     50           1536 Feb  4 23:30 outgoing
drwxr-xr-x   3 208      root          512 Nov  5  1999 papers
drwxr-xr-x   2 134      root          512 Nov  5  1999 patch
lrwxr-xr-x   1 root     root           12 Jan 26 19:00 pii -> winsock2/pii
drwxr-xr-x   2 78       root          512 Nov  5  1999 pld_fpga
drwxr-xr-x   2 770      root         1024 Jan 18 00:32 reqadm
drwxr-xr-x   3 70       root          512 Nov  5  1999 rmx
drwxr-xr-x   2 root     root          512 Nov  5  1999 rz1000
drwxr-xr-x   5 676      root          512 Nov  5  1999 sal
drwxr-xr-x   2 root     root          512 Nov  5  1999 sbios
lrwxr-xr-x   7 109      root          512 Nov  5  1999 support -> ../win95/support
lrwxr-x--x 116 730      50           2048 Feb  5 02:21 swdev -> support
drwxr-xr-x   2 67       root          512 Nov  5  1999 tis
-rw-r--r--   1 root     root            0 Nov 11  1999 unix.qif
drwxr-xr-x   2 91       root          512 Nov  5  1999 winsock2

-rw-r--r--   1 root     root            0 May 21  1996 .hide
lrwxrwxrwx   1 root     root           13 Apr 30  1998 ld-linux.so -> ld-linux.so.1
lrwxrwxrwx   1 root     root           18 Apr 30  1998 ld-linux.so.1 -> ld-linux.so.1.8.10
-rwxr-xr-x   1 root     root        82412 Mar 16  1997 ld-linux.so.1.8.10
-rwxr-xr-x   1 root     root        17412 May 21  1996 ld.so
lrwxrwxrwx   1 root     root           14 Mar 21  1998 libc.so.4 -> libc.so.4.6.27
-rwxr-xr-x   1 root     root       634880 May 21  1996 libc.so.4.6.27
lrwxrwxrwx   1 root     root           14 Apr 30  1998 libc.so.5 -> libc.so.5.4.23
-rwxr-xr-x   1 root     root       602076 Apr 30  1998 libc.so.5.4.23
lrwxrwxrwx   1 root     root           14 Mar 21  1998 libm.so.4 -> libm.so.4.6.27
-rwxr-xr-x   1 root     root       110592 May 21  1996 libm.so.4.6.27

list at 7 Jan
drwxrwxr-x   2 cixip    cixip        1024 Jun 13  2001 .
drwxrwxr-x   6 cixip    cixip        1536 Dec 23 13:03 ..
-rw-rw-r--   1 cixip    cixip         268 Dec 23 13:09 .htaccess
-rw-r--r--   1 cixip    cixip     1260608 Jan  2 15:57 codelook.zip
-rw-r--r--   1 cixip    cixip       32759 Dec 15 17:33 dialacc.htm
-rw-r--r--   1 cixip    cixip      402646 Dec 15 17:33 dialimob.htm
-rw-r--r--   1 cixip    cixip      322159 Dec 15 17:34 dialintl.htm
-rw-r--r--   1 cixip    cixip        9585 Dec 18 00:28 index.shtml
-rw-rw-r--   1 cixip    cixip        3640 Oct 11  1998 maglogo.gif
-rw-rw-r--   1 cixip    cixip        2763 Oct 11  1998 maglogo2.gif
-rw-rw-r--   1 cixip    cixip       67350 Dec 15 17:35 tarifcod.htm
-rw-rw-r--   1 cixip    cixip      867377 Dec 15 17:36 tariffr.zip
-rw-rw-r--   1 cixip    cixip      722470 Dec 15 17:37 tariffrz.zip
-rw-rw-r--   1 cixip    cixip      152289 Jul  6  2001 tarifmob1.htm
-rw-rw-r--   1 cixip    cixip       99824 Jul  6  2001 tarifmob2.htm
-rw-rw-r--   1 cixip    cixip        1126 Oct 11  1998 tcback.gif

magsys web merula
drw-rw-rw-   1 user     group           0 Dec  2 00:01 .
drw-rw-rw-   1 user     group           0 Dec  2 00:01 ..
-rw-rw-rw-   1 user     group      380560 Sep  2 00:00 ex020901.log
-rw-rw-rw-   1 user     group      825953 Sep  3 00:00 ex020902.log
-rw-rw-rw-   1 user     group      926278 Sep  4 00:00 ex020903.log
-rw-rw-rw-   1 user     group      906454 Sep  5 00:00 ex020904.log
-rw-rw-rw-   1 user     group      930487 Sep  6 00:00 ex020905.log


DOS/WINDOWS TYPE

02-04-00  12:16AM       <DIR>          TurboView
01-15-00  11:52PM       <DIR>          twain
06-09-98  06:07PM                 1197 default.htm
06-13-98  09:08PM                37376 initcaps.dll
06-13-98  12:08AM                 1107 initcaps.htm
06-13-98  12:38PM                  562 initcaps.inf
04-17-99  05:14PM                 2183 register.html
06-09-98  06:03PM              3720660 savage2.mp3
06-13-98  05:10PM                 3244 tcltest.htm

VMS - MULTINET TYPE

WORK:[POOR]
WORK:[POOR.WWW]
WORK:[POOR.WWW.TEST]

A1MAIL$.DIR;1              27  27-JUL-1997 08:46 [POOR] (RWE,RWE,,)
A1MAIL$USER_PROFILE.DAT;1
                            4  10-APR-1995 14:48 [POOR] (RWD,RW,,)
CECILIA.DIS;1               1  18-AUG-1994 16:48 [POOR] (RWED,RWED,,)
FTP_SERVER.LOG;16           2   6-FEB-2000 14:42 [POOR] (RWED,RWED,RE,)
GOPHERRC.;1                 0   5-OCT-1993 14:30 [POOR] (RWD,RWD,,)
LOGIN.COM;6                 1  12-OCT-1994 10:24 [POOR] (RWED,RWED,,)
MAIL$107FE97600050098.MAI;1
                           11   6-JUL-1994 17:50 [POOR] (RW,RW,,)
MAIL$1170B62600050098.MAI;1
                           23   7-JUL-1994 22:33 [POOR] (RW,RW,,)
MAIL$1283D3CF00050096.MAI;1
                            5  25-SEP-1992 17:53 [POOR] (RW,RW,,)
MAIL$26813FB800050098.MAI;1
                            7   3-AUG-1994 17:55 [POOR] (RW,RW,,)
MAIL$2A72F73700050098.MAI;1
                           13   8-AUG-1994 18:22 [POOR] (RW,RW,,)
MAIL$41A9489100050098.MAI;1
                           15   7-SEP-1994 07:19 [POOR] (RW,RW,,)
MAIL$437DF4FC00050098.MAI;1
                            4   9-SEP-1994 15:14 [POOR] (RW,RW,,)
MAIL$43B5E1B800050098.MAI;1
                            4   9-SEP-1994 21:54 [POOR] (RW,RW,,)
MAIL$4739E47E00050098.MAI;1
                            5  14-SEP-1994 09:17 [POOR] (RW,RW,,)
MAIL$4B27FF2500050098.MAI;1
                           15  19-SEP-1994 09:19 [POOR] (RW,RW,,)
MAIL$4BE3236800050098.MAI;1
                            8  20-SEP-1994 07:38 [POOR] (RW,RW,,)
MAIL$674BF17F00050097.MAI;1
                           22   3-DEC-1993 10:02 [POOR] (RW,RW,,)
MAIL$70F3003400050097.MAI;1
                            5  15-DEC-1993 16:50 [POOR] (RW,RW,,)
MAIL$74C790AA00050097.MAI;1
                           10  20-DEC-1993 13:49 [POOR] (RW,RW,,)
MAIL$BD5D07D400050097.MAI;1
                            8  22-MAR-1994 22:41 [POOR] (RW,RW,,)
MAIL$D08322A500050097.MAI;1
                            8  16-APR-1994 07:31 [POOR] (RW,RW,,)
MAIL$DFC3537400050097.MAI;1
                           75   5-MAY-1994 17:19 [POOR] (RW,RW,,)
MAIL$FBB7778400050097.MAI;1
                            5  10-JUN-1994 07:04 [POOR] (RW,RW,,)
MAIL$FE93373E00050097.MAI;1
                            4  13-JUN-1994 22:22 [POOR] (RW,RW,,)
MAIL.MAI;1                165   5-JUL-1994 08:15 [POOR] (RW,RW,,)
TEST.EXE;1                  4  27-OCT-1992 12:08 [POOR] (RWED,RWED,RE,)
TEST.PAS;2                  1   6-FEB-2000 14:33 [POOR] (RWED,RWED,RE,)
WWW.DIR;1                   1  27-JUL-1997 08:47 [POOR] (RWE,RWE,E,E)


Total of 453 blocks in 29 files.


AS-400 TYPE


QPGMR            8192 05/30/98 08:08:03 *FILE      QSRVSRC

QPGMR            8192 05/30/98 08:08:03 *FILE      QTBLSRC

QPGMR           12288 05/30/98 08:41:04 *FILE      QTXTSRC

QSECOFR         40960 10/08/99 10:25:23 *FILE      REPLACE

QSECOFR                                 *MEM       REPLACE.REPLACE
QUSER           69632 02/05/99 16:19:28 *DIR       HTML/
QUSER            1575 02/05/99 16:06:37 *STMF      index.htm.$$$  <---- EDITORS WAY OF .BAK
GUEST            1733 02/02/00 18:32:43 *STMF      index.htm
QUSER          151552 07/16/99 12:33:16 *DIR       DPISALES/
QUSER           14398 10/21/98 12:49:52 *STMF      readme.htm
QSECOFR         69632 10/03/98 15:22:35 *DIR       java_applets/
QSECOFR         53248 02/02/00 18:21:57 *DIR       PersonalWebPages/   <---- TURBOVIEW COOL PIC'S
QUSER           69632 12/04/98 15:17:54 *DIR       Art/
QUSER             949 10/21/98 15:48:59 *STMF      Manage_More_With_Less.htm
GUEST            1733 02/02/00 18:30:28 *STMF      index.htm.bak
QPGMR               0 02/10/00 15:59:33 *STMF      index.html
GUEST           45056 02/19/00 10:42:35 *DIR       Turbo/      <----- HERE'S MY DEFAULT DIRECTORY FOR USER TURBO
250 List completed.
770 bytes received in 0.00 seconds (770000.00 Kbytes/sec)  <---I TYPED CD PERSONALWEBPAGES
ftp> 250 "/home/personalwebpages" is current directory.    <--- FOR SOME REASON WHAT I TYPED DOESN'T SHOW UP
ftp> 200 PORT subcommand request successful.
125 List started.
QSECOFR         65536 02/02/00 18:21:57 *DIR       PhilWeb/
QSECOFR         61440 02/02/00 18:22:01 *DIR       KathyWeb/
250 List completed.                     ^^^^  NOTICE THIS IS A DIRECTORY
123 bytes received in 0.00 seconds (123000.00 Kbytes/sec)
ftp> 250 "/home/personalwebpages/philweb" is current directory.
ftp> 200 PORT subcommand request successful.
125 List started.
QSECOFR        187061 02/02/00 18:21:53 *STMF      wurz98.jpg
QSECOFR         44431 02/02/00 18:21:53 *STMF      wurz_can98.jpg
QSECOFR         98696 02/02/00 18:21:53 *STMF      stewart_elf.jpg
QSECOFR        102959 02/02/00 18:21:53 *STMF      schumacher_96.jpg
QSECOFR        123243 02/02/00 18:21:53 *STMF      schum_hun98.jpg
QSECOFR         33009 02/02/00 18:21:53 *STMF      schu_mon_97.jpg
QSECOFR        170500 02/02/00 18:21:54 *STMF      lauda_75.jpg
QSECOFR         71069 02/02/00 18:21:54 *STMF      hill_aus_98.jpg
QSECOFR         71935 02/02/00 18:21:54 *STMF      hakinn_br-gp98.jpg
QSECOFR         55924 02/02/00 18:21:54 *STMF      gilles.jpg
QSECOFR         35333 02/02/00 18:21:54 *STMF      emmo_73_sp.jpg
QSECOFR         80092 02/02/00 18:21:54 *STMF      diniz_fire.jpg
QSECOFR         76466 02/02/00 18:21:54 *STMF      brundle_aus_96.jpg
QSECOFR        137132 02/02/00 18:21:55 *STMF      brands_72.jpg
QSECOFR        233319 02/02/00 18:21:55 *STMF      andretti_zandvoort_78.jpg
QSECOFR         44398 02/02/00 18:21:55 *STMF      79brapiquet.jpg
QSECOFR          6657 02/02/00 18:21:55 *STMF      WP_THUMB_0.JPG
QSECOFR          7753 02/02/00 18:21:55 *STMF      WP_THUMB_1.JPG
QSECOFR          3792 02/02/00 18:21:55 *STMF      WP_THUMB_2.JPG
QSECOFR          3312 02/02/00 18:21:55 *STMF      WP_THUMB_3.JPG
QSECOFR          4550 02/02/00 18:21:55 *STMF      WP_THUMB_4.JPG
QSECOFR          3381 02/02/00 18:21:55 *STMF      WP_THUMB_5.JPG
QSECOFR          4336 02/02/00 18:21:56 *STMF      WP_THUMB_6.JPG
QSECOFR          4904 02/02/00 18:21:56 *STMF      WP_THUMB_7.JPG
QSECOFR          3673 02/02/00 18:21:56 *STMF      WP_THUMB_8.JPG
QSECOFR          4959 02/02/00 18:21:56 *STMF      WP_THUMB_9.JPG
QSECOFR          3416 02/02/00 18:21:56 *STMF      WP_THUMB_10.JPG
QSECOFR          4408 02/02/00 18:21:56 *STMF      WP_THUMB_11.JPG
QSECOFR          4686 02/02/00 18:21:56 *STMF      WP_THUMB_12.JPG
QSECOFR          5350 02/02/00 18:21:56 *STMF      WP_THUMB_13.JPG
QSECOFR          2096 02/02/00 18:21:56 *STMF      WP_THUMB_14.JPG
QSECOFR          4597 02/02/00 18:21:57 *STMF      WP_THUMB_15.JPG
QSECOFR          2867 02/02/00 18:21:57 *STMF      index.html
QSECOFR           532 02/02/00 18:21:57 *STMF      WebInfo.INI
250 List completed.                     ^^^^^  THIS IS A  STMF ====  Stream file
2293 bytes received in 0.27 seconds (8.49 Kbytes/sec)
ftp> ?Invalid command
ftp> 221 QUIT subcommand received.

MORE TYPES OF SHIT ON THE AS/400 FILE SYSTEM

 Object types that are commonly used or that you are likely to see on
 this display include the following:
AUTL        Authorization list
BLKSF       Block special file
CFGL        Configuration list
CLS         Class
CMD         Command
CTLD        Controller description
DDIR        Distributed directory
DEVD        Device description
DIR         Directory
DOC         Document
DSTMF       Distributed stream file
FILE        Database file or device file
FLR         Folder
JOBD        Job description
JOBQ        Job queue
LIB         Library
LIND        Line description
MSGQ        Message queue
OUTQ        Output queue
PGM         Program
SBSD        Subsystem description
SOMOBJ      System Object Model object
STMF        Stream file
SYMLNK      Symbolic link
USRPRF      User profile
*)
(*
Procedure LoadFakeInOutList(Var InOutListN:TStringList);
Begin
InOutListN.Clear;
InOutListN.Add('');
InOutListN.Add('QPGMR            8192 05/30/98 08:08:03 *FILE      QSRVSRC');
InOutListN.Add('');
InOutListN.Add('QPGMR            8192 05/30/98 08:08:03 *FILE      QTBLSRC');
InOutListN.Add('');
InOutListN.Add('QPGMR           12288 05/30/98 08:41:04 *FILE      QTXTSRC');
InOutListN.Add('');
InOutListN.Add('QSECOFR         40960 10/08/99 10:25:23 *FILE      REPLACE');
InOutListN.Add('');
InOutListN.Add('QSECOFR                                 *MEM       REPLACE.REPLACE');
InOutListN.Add('QUSER           69632 02/05/99 16:19:28 *DIR       HTML/');
InOutListN.Add('QUSER            1575 02/05/99 16:06:37 *STMF      index.htm.$$$  <---- EDITORS WAY OF .BAK');
InOutListN.Add('GUEST            1733 02/02/00 18:32:43 *STMF      index.htm');
InOutListN.Add('QUSER          151552 07/16/99 12:33:16 *DIR       DPISALES/');
InOutListN.Add('QUSER           14398 10/21/98 12:49:52 *STMF      readme.htm');
InOutListN.Add('QSECOFR         69632 10/03/98 15:22:35 *DIR       java_applets/');
InOutListN.Add('QSECOFR         53248 02/02/00 18:21:57 *DIR       PersonalWebPages/');
InOutListN.Add('QUSER           69632 12/04/98 15:17:54 *DIR       Art/');
InOutListN.Add('QUSER             949 10/21/98 15:48:59 *STMF      Manage_More_With_Less.htm');
InOutListN.Add('GUEST            1733 02/02/00 18:30:28 *STMF      index.htm.bak');
InOutListN.Add('QPGMR               0 02/10/00 15:59:33 *STMF      index.html');
InOutListN.Add('GUEST           45056 02/19/00 10:42:35 *DIR       Turbo/');
InOutListN.Add('QSECOFR         65536 02/02/00 18:21:57 *DIR       PhilWeb/');
InOutListN.Add('QSECOFR         61440 02/02/00 18:22:01 *DIR       KathyWeb/');
InOutListN.Add('123 bytes received in 0.00 seconds (123000.00 Kbytes/sec)');
InOutListN.Add('QSECOFR        187061 02/02/00 18:21:53 *STMF      wurz98.jpg');
InOutListN.Add('QSECOFR         44431 02/02/00 18:21:53 *STMF      wurz_can98.jpg');
InOutListN.Add('QSECOFR         98696 02/02/00 18:21:53 *STMF      stewart_elf.jpg');
InOutListN.Add('QSECOFR        102959 02/02/00 18:21:53 *STMF      schumacher_96.jpg');
InOutListN.Add('QSECOFR        123243 02/02/00 18:21:53 *STMF      schum_hun98.jpg');
InOutListN.Add('QSECOFR         33009 02/02/00 18:21:53 *STMF      schu_mon_97.jpg');
InOutListN.Add('QSECOFR        170500 02/02/00 18:21:54 *STMF      lauda_75.jpg');
InOutListN.Add('QSECOFR         71069 02/02/00 18:21:54 *STMF      hill_aus_98.jpg');
InOutListN.Add('QSECOFR         71935 02/02/00 18:21:54 *STMF      hakinn_br-gp98.jpg');
InOutListN.Add('QSECOFR         55924 02/02/00 18:21:54 *STMF      gilles.jpg');
InOutListN.Add('QSECOFR         35333 02/02/00 18:21:54 *STMF      emmo_73_sp.jpg');
InOutListN.Add('QSECOFR         80092 02/02/00 18:21:54 *STMF      diniz_fire.jpg');
InOutListN.Add('QSECOFR         76466 02/02/00 18:21:54 *STMF      brundle_aus_96.jpg');
InOutListN.Add('QSECOFR        137132 02/02/00 18:21:55 *STMF      brands_72.jpg');
InOutListN.Add('QSECOFR        233319 02/02/00 18:21:55 *STMF      andretti_zandvoort_78.jpg');
InOutListN.Add('QSECOFR         44398 02/02/00 18:21:55 *STMF      79brapiquet.jpg');
InOutListN.Add('QSECOFR          6657 02/02/00 18:21:55 *STMF      WP_THUMB_0.JPG');
InOutListN.Add('QSECOFR          7753 02/02/00 18:21:55 *STMF      WP_THUMB_1.JPG');
InOutListN.Add('QSECOFR          3792 02/02/00 18:21:55 *STMF      WP_THUMB_2.JPG');
InOutListN.Add('QSECOFR          3312 02/02/00 18:21:55 *STMF      WP_THUMB_3.JPG');
InOutListN.Add('QSECOFR          4550 02/02/00 18:21:55 *STMF      WP_THUMB_4.JPG');
InOutListN.Add('QSECOFR          3381 02/02/00 18:21:55 *STMF      WP_THUMB_5.JPG');
InOutListN.Add('QSECOFR          4336 02/02/00 18:21:56 *STMF      WP_THUMB_6.JPG');
InOutListN.Add('QSECOFR          4904 02/02/00 18:21:56 *STMF      WP_THUMB_7.JPG');
InOutListN.Add('QSECOFR          3673 02/02/00 18:21:56 *STMF      WP_THUMB_8.JPG');
InOutListN.Add('QSECOFR          4959 02/02/00 18:21:56 *STMF      WP_THUMB_9.JPG');
InOutListN.Add('QSECOFR          3416 02/02/00 18:21:56 *STMF      WP_THUMB_10.JPG');
InOutListN.Add('QSECOFR          4408 02/02/00 18:21:56 *STMF      WP_THUMB_11.JPG');
InOutListN.Add('QSECOFR          4686 02/02/00 18:21:56 *STMF      WP_THUMB_12.JPG');
InOutListN.Add('QSECOFR          5350 02/02/00 18:21:56 *STMF      WP_THUMB_13.JPG');
InOutListN.Add('QSECOFR          2096 02/02/00 18:21:56 *STMF      WP_THUMB_14.JPG');
InOutListN.Add('QSECOFR          4597 02/02/00 18:21:57 *STMF      WP_THUMB_15.JPG');
InOutListN.Add('QSECOFR          2867 02/02/00 18:21:57 *STMF      index.html');
InOutListN.Add('QSECOFR           532 02/02/00 18:21:57 *STMF      WebInfo.INI');
End;

FTP MLSD command, same format for MSLT for a single file
size=0;type=cdir;perm=fdelcmp;create=20020616151738;modify=20031002125810; .
size=0;type=pdir;perm=fdelcmp;create=20020616151738;modify=20031002125810; ..
size=17199;type=file;perm=fdrwa;create=20030616152030;modify=20031001190100; 00master.zip
size=182928;type=file;perm=fdrwa;create=20030922195127;modify=20030922190600; 12=page-004394.zip
size=134503;type=file;perm=fdrwa;create=20030923181732;modify=20030923170800; 12=page-004399.zip
size=225460;type=file;perm=fdrwa;create=20030923193147;modify=20030923185600; 12=page-004400.zip
size=205011;type=file;perm=fdrwa;create=20030923120836;modify=20030922225700; 12=page-004405.zip
size=191721;type=file;perm=fdrwa;create=20030905141821;modify=20030904181100; 20=page-004320.zip
size=183977;type=file;perm=fdrwa;create=20030905142247;modify=20030904181100; 20=page-004321.zip
size=0;type=dir;perm=fdelcmp;create=20030219123018;modify=20030305153855; errors
size=0;type=dir;perm=fdelcmp;create=20021217151845;modify=20030903193625; new software
size=0;type=dir;perm=fdelcmp;create=20020805160304;modify=20031002133003; sql logs
size=70806;type=file;perm=fdrwa;create=20030718113340;modify=20031001185600; vehinfiles.zip
size=0;type=dir;perm=fdelcmp;create=20020801100314;modify=20031004124403; zip logs


FTP SITE DMLSD -R and SITE CMLSD -R includes recursive sub-directories,
currently only ICS FTP server 1.54 and later
size=87284;type=file;perm=fdrwa;create=20071119100609;modify=20061129190135; /vmp-20061129.log
size=37256;type=file;perm=fdrwa;create=20071119100609;modify=20061130195708; /vmp-20061130.log
size=18785;type=file;perm=fdrwa;create=20070716105754;modify=20070716105754; /archmon/PC19==20070716-115754.zip
size=67489;type=file;perm=fdrwa;create=20070718115746;modify=20070718115746; /archmon/PC19==20070718-125746.zip
size=40207;type=file;perm=fdrwa;create=20070810085724;modify=20070810085724; /archmon/PC19==20070810-095724.zip
size=1011267;type=file;perm=fdrwa;create=20070303023249;modify=20050622130524; /downinst/99=inst-all-900179.zip
size=8531099;type=file;perm=fdrwa;create=20070810085721;modify=20070809184912; /download/30=page-023447.zip
size=1850041;type=file;perm=fdrwa;create=20070716153422;modify=20070716114356; /download/30=page-023817.zip
size=2870709;type=file;perm=fdrwa;create=20070716153423;modify=20070716124420; /download/30=page-023819.zip

*)



Procedure TMagFtpW.SetupVMS (const BaseFolder: string; var HostType: THostType);
Begin
    HostType := FTPTYPE_NONE ;
    If Pos(':[',BaseFolder) <> 0 Then HostType := FTPTYPE_MVS ;
End;

function IncludeTrailingUnixDelimiterW(const S : UnicodeString): UnicodeString;
begin
    if (Length(S) > 0) and (S[Length(S)] <> '/') then
        Result := S + '/'
    else if (Length(S) = 0) then    // 22 Oct 2008 
        Result := '/'
    else
        Result := S;
end;

Function Strunc(S1:UnicodeString):UnicodeString;
Var
    Len:Integer;
Begin
    Len:=Length(S1);
    While (Len>0) And (S1[1]=' ') Do
    Begin
        Delete(S1,1,1);
        Dec(Len)
    End;
    Len:=Length(S1);
    While (Len>0) And (S1[Len]=' ') Do
    Begin
        Delete(S1,Len,1);
        Dec(Len);
    End;
    Strunc:=S1;
End;

// unpack FTP directory in downloaded file into dynamic array
// returns number of actual files in directory (it may include other dirs)
// directory stream may be ANSI with a specific code page or UTF8

function TMagFtpW.UnpackFtpFDir (DirStream: TStream; RemDir, BaseDir: UnicodeString;
        Level: integer ; var DCodePage: Cardinal ; var HostType: THostType;
                    var TotFiles: integer; var RemFiles: TFileRecsW): integer ;
var
    RawLines, RawCurLine: RawByteString ;
    initdlen, RawTotLen, RawLineStart, RawLineEnd: integer ;
Type
    SArray=Array[0..20] Of UnicodeString;
Const
    Months:Array[1..12] Of UnicodeString =
     ('JANUARY','FEBRUARY','MARCH','APRIL',
     'MAY','JUNE','JULY','AUGUST',
     'SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER');
Var
    CurYear,Month,Day:Word;
    PosX,X,MaxX,I:Integer;
    MyGroup,MyLink,S2,MySize,MyAttr,MyDate,MyTime,MyName,MyDir,S1:UnicodeString;
    MyDateTime:TDateTime;
    Second,Fifth,CurLine:UnicodeString;
    JunkArray:SArray;
    ACodePage: Cardinal;

    Function IntToStr2(const W1:Integer):UnicodeString;
    Var
        S1:UnicodeString;
    Begin
        S1:=IntToStr(W1);
        If Length(S1)<2 Then S1:='0'+S1;
        IntToStr2:=S1;
    End;

    Function Value(const S1:UnicodeString):LongInt;
    Var
        X,Y:LongInt;
    Begin
        Val(S1,X,Y);
        Value:=X;
    End;

    Function GetFirstParam(Var S1:UnicodeString):UnicodeString;
    Var
        X:Integer;
    Begin
        If S1='' Then
        Begin
            Result:='';
            Exit;
        End;
        If S1[Length(S1)]<>' ' Then S1:=S1+' ';
        X:=Pos(' ',S1);
        Result:=Copy(S1,1,X-1);
        S1:=Copy(S1,X,Length((S1)));
        S1:=Strunc(S1);
    End;

    Function MonthMatch(P2:UnicodeString):Integer;
    Var
        X:Integer;
    Begin
        Result:=0;
        X:=1;
        Repeat
            If Pos(P2,Months[X])<>0 Then Result:=X;
            Inc(X);
        Until (Result<>0) Or (X>12);
    End;

    Function SuckParmsB(Var S1:UnicodeString):UnicodeString;
    Var
        Y:Integer;
    Begin
        If S1='' Then
        Begin
            Result:='';
            Exit;
        End;
        If S1[Length(S1)]<>' ' Then S1:=S1+' ';
        Y:=Pos(']',S1);
        Result:=Copy(S1,1,Y);
        S1:=Copy(S1,Y+1,Length((S1)));
        S1:=Strunc(S1);
    End;

    Function GetFifth(S1:UnicodeString):UnicodeString;
    Begin
        Result:=GetFirstParam(S1);
        Result:=GetFirstParam(S1);
        Result:=GetFirstParam(S1);
        Result:=GetFirstParam(S1);
        Result:=GetFirstParam(S1);
        If Result='' Then Result:='-';
    End;

    Function GetSecond(S1:UnicodeString):UnicodeString;
    Begin
        Result:=GetFirstParam(S1);
        Result:=GetFirstParam(S1);
        If Result='' Then Result:='-';
    End;

    Function Filter(S1:UnicodeString):UnicodeString;
    Var
        X:Integer;
    Begin
        For X:=1 To Length(S1) Do
        Begin
           If S1[X] <= #27 Then S1[X]:=' ';
        End;
        Result:=S1;
    End;

    Function FormatDate2(S1:UnicodeString):UnicodeString;
    Var
        Year:Integer;
    Begin
        While Pos('-',S1)<>0 Do S1[Pos('-',S1)]:='/';
        Year:=Value(Copy(S1,7,2));
        If Year<30 Then
            Year:=2000+Year
        Else
            Year:=1900+Year;
        Result:=Copy(S1,1,6)+IntToStr(Year);
    End;

    Function FormatDate1(S1:UnicodeString):UnicodeString;
    Var
        P1,P2,P3:UnicodeString;
        X:Integer;
        Found:Integer;
    Begin
        Result:=S1;
        X:=Pos('-',S1);
        If X=0 Then X:=Pos('/',S1);
        If X<>0 Then
        Begin
            P1:=IntToStr2(Value(Copy(S1,1,X-1)));
            S1:=Copy(S1,X+1,Length(S1));
            X:=Pos('-',S1);
            If X=0 Then X:=Pos('/',S1);
            If X<>0 Then
            Begin
                P2:=UpperCase(Copy(S1,1,X-1));
                P3:=Copy(S1,X+1,Length(S1));
                If NOT((P2[1] >= '0') and (P2[1] <= '9')) Then
                Begin
                    Found:=MonthMatch(P2);
                    If Found>0 Then P2:=IntToStr2(Found);
                    Result:=P2+'/'+P1+'/'+P3;
                End
                Else
                    Result:=P1+'/'+P2+'/'+P3;
            End;
        End;
    End;

    Function FormatTime1(S1:UnicodeString):UnicodeString;
    Var
        X:Integer;
    Begin
        S1:=UpperCase(S1);
        If Length(S1)>5 Then
        Begin
            If Pos('PM',S1)<>0 Then
            Begin
                X:=Value(Copy(S1,1,2));
                If X<12 Then Inc(X,12);
                Result:=IntToStr2(X)+Copy(S1,3,3);
            End
            Else If Pos('AM',S1)<>0 Then  // angus, 9 Apr 2005
            Begin
                X:=Value(Copy(S1,1,2));
                If X=12 Then X:=0 ; //  allow for 12:00AM midnight being hour 00:00
                Result:=IntToStr2(X)+Copy(S1,3,3);
            End
            Else
                Result:=Copy(S1,1,5);
            Result := Result + ':00' ;  // angus fake seconds
        End
        Else
            Result:=S1;
    End;

    Function AdjustS1(Junk1Array:SArray):UnicodeString;
    Var
        Found:Boolean;
        Y,X,TopEnd:Integer;
    Begin
        TopEnd:=High(Junk1Array);
        Found:=False;
        Result:='BAD INPUT LINE!';
        X:=0;
        Repeat
            If MonthMatch(UpperCase(Junk1Array[X]))>0 Then
            Begin
                Result:='';
                For Y:=X-1 To TopEnd-1 Do
                Begin
                    Result:=Result+Junk1Array[Y]+' ';
                End;
               Found:=True;
            End;
            Inc(X);
        Until (Found=True) Or (X>=TopEnd);
    End;

    function FindMlsFact (const response, fact: UnicodeString): UnicodeString ;
    var
        I: integer ;
        S: UnicodeString ;
    begin
        result := '' ;
        I := Pos (fact, response) ;   // ie type=, size=, modify=, perm=
        if I <= 0 then exit ;
        I := I + Length (fact) ;
        if I > Length (response) then exit ;
        S := Copy (response, I, 999) ;   // ie size=183977;type=fil
        I := Pos (';', S) ;  // fact terminator
        if I <= 0 then exit ;
        result := Copy (S, 1, Pred (I)) ;
    end ;


// mainline start
Begin
    result := 0 ;
    RemDir := IncludeTrailingUnixDelimiterW (RemDir) ;
    initdlen := Length (BaseDir) ;
    if NOT Assigned (DirStream) then exit ;
    RawTotLen := DirStream.Size ;
    if RawTotLen <= 1 then exit ;
    DirStream.Seek (0, soFromBeginning) ;
    try  // finally

// load FTP file directory, make sure it's not empty, check if any UTF8 escaped characters
    SetLength (RawLines, RawTotLen);
    DirStream.ReadBuffer (RawLines [1], RawTotLen);
    S2 := 'ANSI' ;
    ACodePage := CP_ACP ;
    if NOT IsUsAscii (RawLines) then
    begin
        if (DCodepage = CP_UTF8) or IsUtf8Valid (RawLines) then // 16 Sept 2008 check for UTF8
        begin
            S2 := 'UTF8' ;
            ACodePage := CP_UTF8 ;
            if DCodePage <> CP_UTF8 then
            begin
                doCopyEvent (LogLevelInfo, 'Setting UTF8 ON in Response to UTF8 Directory') ;
                DCodePage := CP_UTF8 ;  // pass back to caller
                fNewOpts := 'UTF8 ON' ;
                Opts ;
                doCopyEvent (LogLevelInfo, 'UTF8 Opts: ' + LastResponse) ;
            end ;
        end ;
    end;

// count lines in listing, and check for MVS header
    MaxX := 1 ;
    for RawLineEnd := 1 to RawTotLen - 1 do  // skip last LF
    begin
        if RawLines [RawLineEnd] = LF then inc (MaxX) ;
        if (MaxX < 10) and (HostType <> FTPTYPE_MVS) then  // look for header: WORK:[POOR]
        begin
            if (RawLines [RawLineEnd] = ':') then
            begin
                if (RawLines [RawLineEnd + 1] = '[') then
                begin
                    I := RawLineEnd + 1 ;
                    while (RawLines [I] <> LF) do
                    begin
                        if RawLines [I] = ']' then
                        begin
                            HostType := FTPTYPE_MVS;
                            break ;
                        end;
                        inc (I) ;
                        if (I > RawTotLen) then break ; // sanity test
                    end ;
                end ;
            end ;
        end ;
    end ;
    doCopyEvent (LogLevelDiag, 'Server Returned ' + IntToCStr (MaxX) + ' Line(s), Bytes ' +
                IntToCStr (DirStream.Size) + ' for Directory: ' + RemDir + ', Format: ' + S2) ;
    SetLength (RemFiles, TotFiles + MaxX + 2) ;   // allocate space for filerecs

// keep year, for UNIX listing without it
    MyDateTime:=Date;
    DecodeDate(MyDateTime,CurYear,Month,Day);

// read each line in the FTP filelist
    RawLineStart := 1 ;
    RawLineEnd := 1 ;
    For X:=1 To MaxX Do
    Begin
        if (RawLineEnd > RawTotLen) then break ; // sanity test
        while  (RawLineEnd <= RawTotLen) and
             (RawLines [RawLineEnd] <> LF)  // find next line end
                           do inc (RawLineEnd) ;
        I := RawLineEnd - RawLineStart ;
        if (RawLineEnd > 1) and (RawLines [RawLineEnd - 1] < space) then dec (I) ; // remove CR
        RawCurLine := Copy (RawLines, RawLineStart, I) ;  // get next line
        inc (RawLineEnd) ;
        RawLineStart := RawLineEnd ;

      // see if logging raw list, do it for single file anyway
        if fDispRemList or (MaxX = 1) then doCopyEvent (LogLevelDiag, UnicodeString (RawCurLine)) ;

       // 16 Sept 2008 convert UTF8 rawsting to UTF16 widestring
        CurLine := AnsiToUnicode (RawCurLine, ACodePage) ;
        if Length (CurLine) = 0 then continue ;
        MyDate:='';
        MyTime:='';
        MyName:='';
        MyDir:='';
        MyAttr:='';
        MySize:='';
        MyLink:='';
        MyGroup:='';
        S2:='';
        if HostType = FTPTYPE_MLSD then
            S1 := CurLine       // get full line for processing
        else
        begin
            S1:=Strunc(Filter(CurLine));    // get filtered full line for processing
            Fifth:=GetFifth(S1);
            Second:=GetSecond(S1);
        end ;
        If S1<>'' Then
        Begin

        // IBM MVS line
            If HostType = FTPTYPE_MVS Then
            Begin
                MyName:=GetFirstParam(S1);
                If S1='' Then   // get next line, listing split over two lines
                Begin
                    while (RawLines [RawLineEnd] <> LF) and  // find next line end
                                       (RawLineEnd <= RawTotLen) do inc (RawLineEnd) ;
                    I := RawLineEnd - RawLineStart ;
                    if (RawLineEnd > 1) and (RawLines [RawLineEnd - 1] < space) then dec (I) ; // remove CR
                    RawCurLine := Copy (RawLines, RawLineStart, I) ;  // get next line
                    inc (RawLineEnd) ;
                    RawLineStart := RawLineEnd ;
                    if fDispRemList then doCopyEvent (LogLevelDiag, UnicodeString (RawCurLine)) ;
                    S1 := Strunc (AnsiToUnicode (RawCurLine, ACodePage)) ;
                End;
                MySize:=IntToStr(Value(GetFirstParam(S1))*512);
                MyDate:=GetFirstParam(S1);
                MyTime:=FormatTime1(GetFirstParam(S1));
                MyDate:=FormatDate1(MyDate);
                MyGroup:=SuckParmsB(S1);
                MyAttr:=GetFirstParam(S1);
                If Pos('.DIR',UpperCase(MyName))<>0 Then
                Begin
                    MyDir:=sDirLit;
                    MyName:=IcsExtractNameOnlyW(Copy(MyName,1,Pos(';',MyName)-1));
                End
                Else
                Begin
                    MyName:=Copy(MyName,1,Pos(';',MyName)-1);
                    MyDir:='';
                End;
            End

         // FTP MSLD command
            else if HostType = FTPTYPE_MLSD then
            begin
                I := Pos (#32, S1) ;  /// file name follows first space in line
                if (I > 1) and (Length (S1) >= Succ (I)) then  // 6 Aug 2007 allow for single char directory
                                     MyName := Copy (S1, Succ (I), 999) ;
                S1 := IcsAnsiLowerCaseW (S1) ;
                S2 := FindMlsFact (S1, 'type=') ;
                if (S2 = 'dir') {or (S2 = 'cdir') or (S2 = 'pdir')} then  // 16 Mar 2006
                    MyDir := sDirLit
                else if S2 <> 'file' then MyName := '' ;
              // could look for os.unix=slink, etc
                if MyName <> '' then
                begin
                    MySize := FindMlsFact (S1, 'size=') ;
                    MyDate := FindMlsFact (S1, 'modify=') ;
                    MyAttr := FindMlsFact (S1, 'perm=') ;
                end ;
            end

        // UNIX line
            Else If (Pos(S1[1],'cldwrx-s')<>0) And (Pos(S1[2],'cldwrx-s')<>0) And
                  (Pos(S1[3],'cldwrx-s')<>0) And (Pos(S1[4],'cldwrx-s')<>0) And
                  (Pos(S1[5],'cldwrx-s')<>0) And (Pos(S1[6],'cldwrx-s')<>0) And
                  (Pos(S1[7],'cldwrx-s')<>0) And (Pos(S1[8],'cldwrx-s')<>0) Then
            Begin
                HostType := FTPTYPE_UNIX;
                MyAttr:=GetFirstParam(S1);
                If MyAttr[1]='d' Then MyDir:=sDirLit;
                GetFirstParam(S1);
                GetFirstParam(S1);   // DLR Fix to make sure it works with APPLE.COM
                JunkArray[0]:=GetFirstParam(S1);
                JunkArray[1]:=GetFirstParam(S1);
                JunkArray[2]:=GetFirstParam(S1);
                JunkArray[3]:=GetFirstParam(S1);
                JunkArray[4]:=GetFirstParam(S1);
                JunkArray[5]:=GetFirstParam(S1);
                JunkArray[6]:=S1;
                S1:=AdjustS1(JunkArray);

                MySize:=GetFirstParam(S1);
                MyDate:=GetFirstParam(S1);
                MyDate:=GetFirstParam(S1)+'/'+MyDate;
                MyTime:=FormatTime1(GetFirstParam(S1));
                If Pos(':',MyTime)=0 Then       // no time available, fake it
                Begin
                    MyDate:=FormatDate1(MyDate+'/'+MyTime);
                    MyTime:='00:00';
                End
                Else
                Begin
             // no year, use today - but correct to last year later if less than 9 months old
                    MyDate:=FormatDate1(MyDate+'/'+IntToStr(CurYear));
                End;
                MyDate:=FormatDate1(MyDate);
                MyName:=S1;
                If Pos('->',MyName)<>0 Then
                Begin
                    PosX:=Pos('->',MyName);
                    MyName:=Strunc(Copy(S1,1,PosX-1));
                    MyLink:=Strunc(Copy(S1,PosX+2,Length(S1)));
                End;
            End

        // Windows NT, maybe DOS
            Else If ((S1[3]='-') And (S1[6]='-')) Or
                                  ((S1[3]='/') And (S1[6]='/')) Then
            Begin
                HostType := FTPTYPE_DOS;
                MyDate:=FormatDate2(GetFirstParam(S1));
                MyTime:=FormatTime1(GetFirstParam(S1));
                MyDir:=GetFirstParam(S1);
                If (MyDir<>'') And (MyDir[1]<>'<') Then
                Begin
                    MySize:=MyDir;
                    MyDir:='';
                End;
                MyName:=Strunc(S1);
            End

        // IBM AS400
            Else If (Fifth[1]='*') Or (Second[1]='*') Then
            Begin
                HostType := FTPTYPE_AS400;
                GetFirstParam(S1);
                MySize:=GetFirstParam(S1);
                If MySize='*MEM' Then
                Begin
                    MySize:='N/A';
                    MyDate:='N/A';
                    MyTime:='N/A';
                    MyDir:='*MEM';
                    MyName:=S1;
                End
                Else
                Begin
                    MyDate:=GetFirstParam(S1);
                    MyTime:=GetFirstParam(S1);
                    MyDir:=GetFirstParam(S1);
                    MyName:=S1;
                End;
                If NOT((MyDir='*DDIR') Or (MyDir='*DIR') Or (MyDir='*LIB')  Or
                    (MyDir='*MEM') Or (MyDir='*FLR') Or (MyDir='*BLKSF')Or
                    (MyDir='*DOC') Or (MyDir='*DSTMF')Or (MyDir='*FILE') Or
                    (MyDir='*PGM') Or (MyDir='*STMF')) Then MyName:='';
                If (MyDir='*DIR') Or (MyDir='*DDIR') Or (MyDir='*LIB')  Or
                    (MyDir='*FLR') Then MyDir:=sDirLit
                Else
                    MyDir:='';
            End;

        // found a file, add to list
            If MyName<>'' Then
            Begin
                inc (TotFiles) ;
                inc (result) ;
                with RemFiles [pred (TotFiles)] do
                begin
                    if MyName [1] = '/' then // 22 Nov 2007 name includes directories
                    begin
                        FrFullName := MyName ;
                        I := Length (MyName) ;
                        while (I > 1) and (MyName [I] <> '/') do dec (I) ;
                        FrFileName := Copy (MyName, I + 1, 255) ;
                        FrSubDirs :=  Copy (MyName, 1, I) ;
                    end
                    else
                    begin
                        FrFileName := MyName ;
                        FrSubDirs := Copy (RemDir, initdlen, 99) ;
                        FrFullName := RemDir + FrFileName ;
                        FrDirLevel := Level ;
                    end ;
                    FrFileBytes := atoi64 (MySize) ;  // 4 Sept 2005
                    if HostType = FTPTYPE_MLSD then
                    begin
                         FrFileUDT := MDTM2Date (myDate) ;     // Warning - ws_ftp returns local time not UTC for mlsd
                         FrFileDT := UTCToLocalDT (FrFileUDT) ;
                    end
                    else
                    begin
                        if Length (MyDate) <> 0 then
                            FrFileDT := ConvUSADate (MyDate) + StrToTime (MyTime)
                        else
                            FrFileDT := 0 ;
                    // see if date is more than three months beyond today, assume last year
                        if FrFileDT > (MyDateTime + 90) then
                        begin
                            FrFileDT := IncMonth (FrFileDT, -12) ;
                        end ;
                        FrFileUDT := 0 ;  // accurate UTC time may be found using MDTM command later
                    end ;
                    if MyDir = '' then
                        FrExtra := Copy (MyAttr, 1, 4)
                    else
                        FrExtra := MyDir ;
                    FrLinks := MyLink ;
                    if ((MyName = '.') or (MyName = '..')) then
                        FrFileCopy := FCStateIgnore
                    else if (MyDir = sDirLit) then
                        FrFileCopy := FCStateDir
                    else if (MyLink <> '') then
                       FrFileCopy := FCStateIgnore
                    else
                    begin
                        FrFileCopy := FCStateNone ;
                        inc (result) ;
                    end ;
                end ;
            End;
        End;
    End;
    finally
        SetLength (RemFiles, TotFiles) ;
    end ;
End;

// connect and logon to FTP server
// also setups most common FTP parameters and event handlers
// keep server root directory and creates base directory

function TMagFtpW.FtpLogon: TTaskResult ;
var
    remdir: UnicodeString ;
    attemptnr: integer ;
//    waittick: DWORD ;
    ret: boolean ;
begin
    result := TaskResAbort ;
    fTotProcFiles := 0 ;
    fProcOKFiles := 0 ;
    fProcFailFiles := 0 ;
    fSkippedFiles := 0 ;
    fDelOKBytes := 0 ;
    fDelOKFiles := 0 ;
    fCancelFlag := false ;
    fServRootDir := '' ;
    fServBaseDir := '' ;
    if fBulkMode = BulkModeDownload then
        remdir := fSrcDir
    else
        remdir := fTarDir ;
    fUsingCompression := false ;
    if CaseFile = FileLowerCase then fServBaseDir := IcsAnsiLowerCaseW (fServBaseDir) ;
    if Timeout < 15 then Timeout := 15 ;
    if Port = '0' then Port := 'ftp' ;
    ResumeAt := 0 ; // 4 July 2007 ensure resume position reset
    if fMaxAttempts = 0 then fMaxAttempts := 3 ;
    if fAttemptDelay = 0 then fAttemptDelay := 5 ;
    fProgFileSize := 0 ;    // no progress counter
    if fHostName1 <> '' then fHostName := fHostName1 ;
// UserName 'anonymous', Password 'ftp@' for anonymous FTP access to public servers
    fReqResponse := '' ;
    if (fBulkMode = BulkModeNone) then
        fReqResponse := 'Must Specify FTP Mode'
    else if (fUserName = '') then
        fReqResponse := 'Must Specify FTP Logon Name'
    else if (fPassword = '') then
        fReqResponse := 'Must Specify FTP Logon Password'
    else if (fHostName = '') then
        fReqResponse := 'Must Specify FTP Host Name'
    else if (remdir = '') then
        fReqResponse := 'Must Specify FTP Remote Directory' ;
    if fReqResponse <> '' then
    begin
        result := TaskResFail ;
        exit ;
    end ;
{$IFDEF USE_SSL}
    fPBSZSize := 0 ;
    fSslType := sslTypeNone ;
    if fFtpType in [FtpTypeAuthSslCtl, FtpTypeAuthSslData, FtpTypeAuthSslBoth] then
    begin
        fSslType := sslTypeAuthTls ; // sslTypeAuthSsl ;
    end
    else if fFtpType in [FtpTypeConnSslCtl, FtpTypeConnSslData, FtpTypeConnSslBoth] then
    begin
        fPort := fFtpSslPort ;
        fSslType := sslTypeImplicit ;
    end ;
    if (fSslType > sslTypeNone) and (NOT Assigned (fExternalSslSessionCache)) then
    begin
        fExternalSslSessionCache := TSslAvlSessionCache.Create (self) ;
     //   fExternalSslSessionCache.AdjustTimeout := True;
     //   fExternalSslSessionCache.SessionTimeOut := 30;
     //   fExternalSslSessionCache.FlushInterval := 3000;
    end;
{$ENDIF}
    fReqResponse := 'No Errors' ;
    for attemptnr := 1 to fMaxAttempts do
    begin
    // toggle between two FTP servers
        if fHostName1 <> '' then
        begin
            if (NOT Odd (attemptnr)) and (fHostName2 <> '') then
                fHostName := fHostName2
            else
                fHostName := fHostName1 ;
        end ;
{$IFDEF USE_SSL}  // alternate TLS and SSL
        if (NOT Odd (attemptnr)) and (fSslType <> sslTypeNone) then
        begin
            if fSslType = sslTypeAuthTls then
               fSslType := sslTypeAuthSsl
            else if fSslType = sslTypeAuthSsl then
               fSslType := sslTypeAuthTls ;
        end ;
{$ENDIF}
        doCopyEvent (LogLevelInfo, 'Connect/Logon to FTP Server: ' +
                                            fHostName + ':' + fPort) ;
        fFtpErrFlag := false ;
        HostFileName := '' ;
        LocalFileName := '' ;
        DisplayFileFlag := false ;
        if fCancelFlag then exit ;
        if (magftpNoHost in fMagFtpOpts) then
            ret := Connect           // connect, user name and password
        else
            ret := ConnectHost ;     // connect, host, user name and password 18 Sept 2008 also sends HOST command
        if fCancelFlag then ret := false ;
        if fFtpErrFlag then ret := false ;   // set in error event
        if NOT CheckReady then
        begin
            doCopyEvent (LogLevelInfo, 'Warning - FTP Not Ready') ;
        end ;
        if ret then
        begin
            doCopyEvent (LogLevelInfo, Trim (LastMultiResponse)) ;
            ret := Pwd ;  // get initial working directory to DirResult
            if fCancelFlag then ret := false ;
            if NOT ret then
            begin
                doCopyEvent (LogLevelInfo, 'Logon Appeared to Fail, Retrying') ;
                ret := Connect ;             // connect, user name and password
                if fCancelFlag then ret := false ;
                if ret then ret := Pwd ;  // get initial working directory
            end ;
            if ret then
            begin
                if Pos (':', remdir) = 2 then  // 6 Jan 2006 if drive change now
                begin
                    HostDirName := remdir ;
                    ret := Cwd ;     // change to another drive
                    if ret then
                    begin
                        ret := Pwd ;  // get initial working directory on new drive
                    end ;
                end ;

            // keep PWD response as root
                fServRootDir := DirResult ;
                DosToUnixPathW (fServRootDir) ;  // 6 Jan 2006 change to Unix, 11 Sept 2008 was not changing directory!!
                fServRootDir := IncludeTrailingUnixDelimiterW (fServRootDir) ;
                doCopyEvent (LogLevelInfo, 'Server Start-up Directory: ' + fServRootDir) ;

        // set base directory, combining root and remote dir - warning, might not exist
                if Pos (':', remdir) = 2 then
                    fServBaseDir := fServRootDir   // 6 Jan 2006 no drive
                else if remdir [1] = '/' then
                       fServBaseDir := fServRootDir + Copy (remdir, 2, 999)
                else
                    fServBaseDir := fServRootDir + remdir ;
                fServBaseDir := IncludeTrailingUnixDelimiterW (fServBaseDir) ;
                fCurRemDir := fServBaseDir ;
                doCopyEvent (LogLevelInfo, 'Xfer Base Directory: ' + fCurRemDir) ;
            end ;
         // check special server features
            if ret and (NOT (magftpNoFeat in fMagFtpOpts)) then  // 7 Nov 2007 made it optional
            begin
                Feat ;
                doCopyEvent (LogLevelInfo, Trim (LastMultiResponse)) ;
                if (magftpNoMd5Crc in fMagFtpOpts) then FSupportedExtensions :=
                     FSupportedExtensions - [ftpFeatMD5, ftpFeatXMD5, ftpFeatXCRC] ;
                if (magftpNoMd5 in fMagFtpOpts) then FSupportedExtensions :=
                                 FSupportedExtensions - [ftpFeatMD5, ftpFeatXMD5] ;   // 15 Apr 2009
                if (magftpNoCrc in fMagFtpOpts) then FSupportedExtensions :=
                                             FSupportedExtensions - [ftpFeatXCRC] ;   // 15 Apr 2009
                if (magftpNoZlib in fMagFtpOpts) then FSupportedExtensions :=
                                             FSupportedExtensions - [ftpFeatModeZ] ;

             // 8 Nov 2007 tell server who we are - before UTF8 to keep Gene6 happy
                if (ftpFeatClnt in FSupportedExtensions) and (Length (ClientIdStr) > 4) then
                begin
                    Clnt ;
                end ;

            // 15 Sept 2008 Unicode/UTF8 - note some servers ignore ON/OFF and always turn on
                if (ftpFeatUtf8 in FSupportedExtensions) and
                                     (NOT (magftpIgnoreUtf8 in fMagFtpOpts)) then
                begin
                    if (magftpNoUtf8 in fMagFtpOpts) then
                    begin
                        fCodePage := CP_ACP;
                        fNewOpts := 'UTF8 OFF' ;
                    end
                    else
                    begin
                        fCodePage := CP_UTF8 ;
                        fNewOpts := 'UTF8 ON' ;
                    end;
                    Opts ;
                    doCopyEvent (LogLevelInfo, 'UTF8 Opts: ' + LastResponse) ;
                end ;
            end ;

         // 8 Nov 2007 find time zone unless using MLSD
            if (ftpFeatSiteZone in FSupportedExtensions) and
                            (NOT (ftpFeatMLST in FSupportedExtensions)) then
            begin
                if SiteZone then fTimeZoneStr := Trim (LastResponse) ;
            end ;
        end ;
        if ret then
        begin
            result := TaskResOKNew ;
            fLoggedIn := true ;
            doCopyEvent (LogLevelInfo, 'Succesfully logged onto FTP Server: ' +
                                                                        fUserName) ;
         // set transfer mode
            if fXferMode = XferModeBinary then TypeBinary ;  // ftp command
            if fXferMode = XferModeAscii then TypeAscii ;    // ftp command

         // setup Mode Z, if available
            if fUseCompression and (ftpFeatModeZ in FSupportedExtensions) then
            begin
                TransferMode := ftpTransModeStream ;  // 23 Nov 2007 start no compression
                if fZlibMaxSize > 10000 then  // 9 Dec 2007 skip mode z unless more than 10K
                begin
                    if ModeZ then
                    begin
                        fUsingCompression := true ;
                        fNewOpts := 'MODE Z LEVEL 8';    // 15 Sept 2008 ensure correct command sent
                        Opts ;
                    end ;
                end ;
            end ;

{$IFDEF USE_SSL}
            if fSslType <> sslTypeNone then
            begin
                Pbsz ; // ftp command, protection buffer size, always zero 18 Sept 2008 before PROT to keep MS FTP7 happy
                if fFtpType in [FtpTypeConnSslBoth, FtpTypeAuthSslBoth,
                                         FtpTypeAuthSslData, FtpTypeConnSslData] then
                begin
                    fProtLevel := 'P' ; // protect data with SSL
                    fPassive := true ;  // must be after connection type
                end
                else
                    fProtLevel := 'C' ; // clear data
                if NOT Prot then  // ftp command, set data channel protection Private or Clear
                begin
                    if fProtLevel = 'P' then
                        fProtLevel := 'C'
                    else
                    begin
                        fProtLevel := 'P' ; // protect data with SSL
                        fPassive := true ;  // must be after connection type
                    end ;
                    Prot ;   // second attempt
                end ;
                if fFtpType in [FtpTypeAuthSslData, FtpTypeConnSslData] then
                    CCC ;  // ftp command, clear control channel
            end ;
{$ENDIF}
            exit ;
        end ;
        fReqResponse := 'Request Failed: ' + ErrorMessage ;
        doCopyEvent (LogLevelInfo, 'Failed to Connect to FTP Server: ' + ErrorMessage) ;
        if Connected then Abort ;
        fLoggedIn := false ;
     // few seconds wait for next attempt
        if fCancelFlag then exit ;
        if attemptnr < fMaxAttempts then
        begin
            doCopyEvent (LogLevelInfo, 'Waiting for ' +
                        IntToStr (fAttemptDelay) + ' secs, then Retrying') ;
            sysDelayX (DWORD (fAttemptDelay) * 1000) ;
        end ;
    end ;
    result := TaskResFail ;
end;

// build file list for remote file using FTP, optionally including sub directories

function TMagFtpW.FtpDir (var FtpFiles: TFileRecsW;
                                    var FtpFileList: TFindList): TTaskResult ;
var
    fullpath, fname: UnicodeString ;
    ret, dirsflag, hostnameflag, chkdirflag: boolean ;
    lastdirrec, level, I, totfiles: integer ;
    DirStream: TStream ;
    fstarttick: longword ;
begin
    fCancelFlag := false ;
    result := TaskResAbort ;
    if NOT fLoggedIn then
    begin
        result := TaskResFail ;
        fReqResponse := 'Not Logged-In Yet' ;
        exit ;
    end ;
    fullpath := fServBaseDir ;  // base directory
    SetLength (FtpFiles, 0) ;
    totfiles := 0 ;
    FtpFileList.Clear ;
    HostFileName := '' ;  // no argument after LIST/MLST
    hostnameflag := ftpFeatMLST in FSupportedExtensions ;  // try and use HostFileName
    chkdirflag := hostnameflag ;  // one off check
    DirStream := TMemoryStream.Create ;
    LocalFileName := '' ;
    LocalStream := DirStream ; // 26 Oct 2003 - get dirs into stream instead of file
    if ftpFeatMLST in Self.FSupportedExtensions then fHostType := FTPTYPE_MLSD ;
    ResumeAt := 0 ; // 4 July 2007 ensure resume position reset
    fProgFileSize := -1 ;   // show it shows up as a directory
    fProgMessBase := 'Getting File Directories from FTP Server, chars ' ;
    if fUsingCompression then
    begin
        if ((ftpFeatSiteDmlsd in FSupportedExtensions) or   // 22 Nov 2007
                    (ftpFeatXDmlsd in FSupportedExtensions)) and fSubDirs then    // 10 Nov 2008
            TransferMode := ftpTransModeZDeflate
        else
            TransferMode := ftpTransModeStream ; // 22 Nov 2007 don't compress single dirs
        if (FCurrTransMode <> TransferMode) then ModeZ ;
    end ;
    HostDirName := fullpath ;
    ret := Cwd ;     // change to working directory in HostDirName
    if ret then ret := Pwd ;  // print working directory to DirResult
    if ret then fCurRemDir := HostDirName ;
    if fCancelFlag then ret := false ;
    if NOT Connected then ret := false ;
    if NOT ret then fCurRemDir := 'xXx' ;  // 15 Sept 2008 invalidate current remote directory if not found so it's created
    dirsflag := ret ;
    level := 0 ;
    lastdirrec := 0 ;
    fstarttick := GetTickCountX ;
    while dirsflag do
    begin
        TMemoryStream (DirStream).Clear ;
        Application.ProcessMessages ;
        if fCancelFlag then
        begin
            doCopyEvent (LogLevelDiag, 'Directory Listing Cancelled Before Next Dir') ;
            ret := false
        end
        else
        begin
            sysDelayX (200) ;  // 11 Aug 2004 - short delay to try and fix corrupted PORT command
         // get FTP directory from remote server with HostFileName argument
            if (ftpFeatXDmlsd in FSupportedExtensions) and fSubDirs then        // 10 Nov 2008
            begin
                HostFileName := '-R'; // recursive sub-directories
                ret := XDMlsd ;
            end
            else if (ftpFeatSiteDmlsd in FSupportedExtensions) and fSubDirs then        // 22 Nov 2007
            begin
                HostFileName := '-R'; // recursive sub-directories
                ret := SiteDMlsd ;
            end
            else if (ftpFeatXCmlsd in FSupportedExtensions) and (NOT fSubDirs) then  // 10 Nov 2008
            begin
                ret := XCMlsd ;
            end
            else if (ftpFeatSiteCmlsd in FSupportedExtensions) and (NOT fSubDirs) then  // 25 Nov 2007
            begin
                ret := SiteCMlsd ;
            end
            else if ftpFeatMLST in FSupportedExtensions then    // 5 Oct 2003
                ret := Mlsd
            else
                ret := Dir ;

         // 11 Aug 2004 - special handling for PORT command being truncated, repeat it
         // 500 'RT': command not understood.
            if (NOT ret) and (StatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
            begin
                sysDelayX (500) ;
                doCopyEvent (LogLevelDiag, 'Repeating Listing Command, Corrupted PORT Bug') ;
                if ftpFeatMLST in FSupportedExtensions then
                    ret := Mlsd
                else
                    ret := Dir ;
            end ;
        end ;
        if fCancelFlag then
        begin
            doCopyEvent (LogLevelDiag, 'Directory Listing Cancelled During Dir') ;
            ret := false
        end ;
        if NOT Connected then
        begin
            doCopyEvent (LogLevelDiag, 'Directory Listing Abandoned, Not Connected') ;
            ret := false
        end ;
        dirsflag := ret ;
        if dirsflag then
        begin
            dirsflag := false ;  // assume no more subdirectories
            UnpackFtpFDir (LocalStream, fullpath, fServBaseDir,
                            level, fCodePage, fHostType, totfiles, FtpFiles) ;

        // search for a subdirectory, add it's files (and subdirs) to list
            if (totfiles > lastdirrec) and fSubDirs and
                           (NOT ((ftpFeatSiteDmlsd in FSupportedExtensions) or   // 22 Nov 2007
                                (ftpFeatXDmlsd in FSupportedExtensions))) then    // 10 Nov 2008
                begin
                for I := lastdirrec to Pred (totfiles) do
                begin
                    with FtpFiles [I] do
                    begin
                        if (FrExtra = sDirLit) and (FrFileCopy = FCStateDir) then
                        begin
                         // 7 Nov 2007 check that MLSD HostFileName has not returned it's own directory
                         // Serv-U 4.6.4 bug, also ICS if / missing
                            if chkdirflag and (Length (HostFileName) > 0) then
                            begin
                                fname := FtpFiles [Pred (totfiles)].FrFileName ;
                                if Pos (fname, HostFileName) > 0 then // error, directory returned as filename
                                begin
                                    doCopyEvent (LogLevelDiag, '!!! TEMP, Last HFN ' +
                                                    HostFileName + ', FName ' + fname) ;
                                    HostFileName := FtpFiles [Pred (totfiles)].FrFullName + '/' ; // see if a real duplicate directory
                                    ret := MLST ;  // FTP command - file facts for HostFileName
                                    if NOT ret then
                                    begin
                                        doCopyEvent (LogLevelDiag,
                                            'Warning - MLSD has Not Correctly Listed Directory, Reverting to Current Mode') ;
                                        hostnameflag := false ;
                                        chkdirflag := false ;  // only once
                                        totfiles := totfiles - 1 ;  // remove directory from listings
                                        HostDirName := HostFileName ;
                                        HostFileName := '' ;
                                        ret := Cwd ;   // change to new directory in HostDirName
                                        if ret then ret := Pwd ;
                                        if ret then fCurRemDir := HostDirName ;
                                        dirsflag := true ; // more to do
                                        break ;
                                    end ;
                                end
                                else
                                    chkdirflag := false ;  // only once
                            end ;

                        // 14 Sept 2008 skip Unicode names with substitution characters
                            if Pos ('?', FrFullName) > 0 then
                            begin
                                doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + FrFullName) ;

                                continue ;
                            end ;

                        // find next directory to process
                            fullpath := FrFullName + '/' ;   // this is passed to parser
                            lastdirrec := I ;
                            level := succ (FrDirLevel) ;
                            FrFileCopy := FCStateIgnore ;  // stop it being processed again
                            dirsflag := true ; // more to do

                         // using HostFileName to avoid changing dirs
                            if hostnameflag then
                            begin
                                HostDirName := '' ;
                                if fCurRemDir <> fServBaseDir then
                                begin
                                    HostDirName := fServBaseDir ;
                                    ret := Cwd ;   // back to base directory in HostDirName
                                    if ret then ret := Pwd ;
                                    if ret then fCurRemDir := HostDirName ;
                                end ;
                              // 10 Aug 2004, path passed to LIST/MLST
                                HostFileName := fullpath ;
                            end ;
                            if NOT hostnameflag then  // 7 Nov 2007 not using host names
                            begin
                                HostFileName := '' ;
                                HostDirName := fullpath ;
                                ret := Cwd ;   // change to new directory in HostDirName
                                if ret then ret := Pwd ;
                                if ret then fCurRemDir := HostDirName ;
                            end ;
                            break ;
                        end ;
                    end ;
                end ;
            end ;
        end
        else
        begin
            fReqResponse := 'Request Failed: No Directory Listing' ;
            doCopyEvent (LogLevelDiag, 'Directory Listing Abandoned, Failed Response') ;
            dirsflag := false ;   // assume no more subdirectories
            result := TaskResFail ;
        end ;
    end ;
    DirStream.Free ;
    LocalStream := nil;
    fProgFileSize := 0 ;    // no progress counter
    if NOT ret then
    begin
        if fCancelFlag then exit ;
        if NOT Connected then exit ;
        result := TaskResFail ;
        fReqResponse := 'Request Failed: ' + ErrorMessage ;
    // 4 Jan 2008 'Unable to establish data connection - Connection refused' - abort to clean up
        if (StatusCode = 550) and (Pos ('#10061', LastResponse) > 1) then
        begin
            if Connected then AbortXfer ;
        end ;
        exit ;
    end ;
    doCopyEvent (LogLevelInfo, 'Files listed OK, total found ' + IntToStr (totfiles) +
                            ' took ' + IntToStr (ElapsedSecs (fstarttick)) + ' secs') ;

// build list from array, omitting directories and rubbish, then sort
    if (totfiles <> 0) then
    begin
        result := TaskResOKNew ;
        FtpFileList.Capacity := totfiles ;
        for I := 0 to Pred (totfiles) do
        begin
            if FtpFiles [I].FrFileCopy <> FCStateIgnore then
                                    FtpFileList.Add (@FtpFiles [I]) ;
        end ;
        FtpFileList.Sort (MagentaCopy3W.CompareFNextW) ;
        FtpFileList.Sorted := true ; // 11 June 2008 not sure if really needed
    end
    else
        result := TaskResOKNone ;
end;

function TMagFtpW.DispFtpDir (var dirlisting: UnicodeString): TTaskResult ;
var
    DirFiles: TFileRecsW ;
    DirFileList: TFindList ;
begin
    dirlisting := '' ;
    DirFileList := TFindList.Create ;
    try
        result := FtpLogon ;
        if result <> TaskResOKNew then exit ;
        if fCancelFlag then exit ;
        result := FtpDir (DirFiles, DirFileList) ;
        if fCancelFlag then exit ;
        if result in [TaskResOKNew, TaskResOKNone] then
                    dirlisting := 'Remote Directory: ' + fSrcDir +
                                    CRLF_ + FmtFileDirListW (DirFileList, false) ;
    finally
        if fLoggedIn then Quit ;   // clean log off
        fLoggedIn := false ;
        if Connected then Abort ;
        if (ControlSocket.State <> wsClosed) or (DataSocket.State <> wsClosed) then Abort ;
        DirFileList.Free ;
    end ;
end;

procedure TMagFtpW.onMagCopyEvent (LogLevel: TLogLevel ; Info: UnicodeString ;
                                                  var Cancel: boolean) ;
begin
    doCopyEvent (LogLevel, Info) ;
    Cancel := fCancelFlag ;
end ;

procedure TMagFtpW.onFtpClientProg64(Sender: TObject; Count: Int64;
                                                     var Abort: Boolean);
var
    temp: UnicodeString ;
begin
    Abort := fCancelFlag ;  // 23 June 2006
    if fProgFileSize = 0 then exit ;
    if (fProgressSecs > 0) and (Count < fProgFileSize) and (NOT fCancelFlag) then
    begin
        if DiffTicks (fLastProgTick, GetTickCountX) <
                        (LongWord (fProgressSecs) * TicksPerSecond) then exit ;
    end ;
    if fProgFileSize < 0 then
        temp := fProgMessBase + Int64ToCStr (Count)
    else
        temp := fProgMessBase + ', chars ' + Int64ToCStr (Count) +
                                     ' of ' +  Int64ToCStr (fProgFileSize)  ;
    doCopyEvent (LogLevelProg, temp) ;
    fLastProgTick := GetTickCountX ;
end;

procedure TMagFtpW.onFtpClientDisplay(Sender: TObject; var Msg: UnicodeString);
var
    temp: UnicodeString ;
begin
    temp := Msg ;
    if Length (temp) > 1 then
    begin
        if (Pos ('> PASS', temp) > 0) then
            temp := '> PASS ****'
        else if (temp [1] = '<') then exit    // ignore response
        else if ((temp [1] = '>') and Utf8DiagFlag) then
            temp := UnicodeString (FCmdUtf8) ;
    end ;
    doCopyEvent (LogLevelDiag, temp) ;
end;

procedure TMagFtpW.onFtpError(Sender: TObject; var Msg: UnicodeString);
begin
    fFtpErrFlag := true ;
    doCopyEvent (LogLevelInfo, '!! FTP Error: ' + Msg) ;
    if FState <> ftpAbort then Abort ;  // 28 Dec 2007 don't repeatedly abort
end;

{
procedure TMagFtpW.OnFtpCommand (Sender: TObject; var Cmd: UnicodeString) ;
var
    temp: string ;
begin
    temp := Cmd ;
    if (Pos ('PASS', temp) > 0) then temp := 'PASS ****' ;
    doCopyEvent (LogLevelDiag, '> '+ temp) ;
end;   }

procedure TMagFtpW.OnFtpResponse (Sender: TObject) ;
var
    temp: UnicodeString ;
begin
    if Utf8DiagFlag then
        temp := UnicodeString (FLastRawResponse)
    else
        temp := FLastResponse ;
// 25 Nov 2007 ignore continuation response for FEAT and SITE INDEX/CMLSD
    if Pos ('200-', temp) = 1 then exit ;
    if Pos ('250-', temp) = 1 then exit ;
    doCopyEvent (LogLevelDiag, '< ' + temp) ;
end;

procedure TMagFtpW.OnFtpSessConn (Sender: TObject; Error: word) ;
var
    temp: string ;
begin
    temp := 'FTP Session Connected to ' + DnsResult ;
    if Error <> 0 then temp := temp + ' - ' + GetWinsockErr (Error) ;
    doCopyEvent (LogLevelDiag, temp) ;
end;

procedure TMagFtpW.OnFTPSocksConnected (Sender: TObject; Error: word) ;
var
    temp: string ;
begin
    temp := 'FTP Socks Session Connected' ;
    if Error <> 0 then temp := temp + ' - ' + GetWinsockErr (Error) ;
    doCopyEvent (LogLevelDiag, temp) ;
end;

procedure TMagFtpW.OnFtpSessClosed (Sender: TObject; Error: word) ;
var
    temp: string ;
begin
    temp := 'FTP Session Closed' ;
    if Error <> 0 then temp := temp + ' - ' + GetWinsockErr (Error) ;
    doCopyEvent (LogLevelDiag, temp) ;
end;

procedure TMagFtpW.OnFtpRequestDone (Sender: TObject; RqType: TFtpRequest; Error: Word) ;
begin
//  error is FTP response, not a real error
//    if Error <> 0 then doCopyEvent (LogLevelDiag,
//        '!Error ReqDone=' + LookupFTPReq (RqType) + ' - ' + IntToStr (Error)) ;
end;

procedure TMagFtpW.OnFtpStateChange (Sender: TObject) ;
begin
// doCopyEvent (LogLevelDiag, '!FTP State Change=' + LookupFtpState (State)) ;  // TEMP !!!!
end;

procedure TMagFtpW.doCopyEvent (const LogLevel: TLogLevel; const Info: UnicodeString) ;
var
    oldflag: boolean ;
begin
    if Assigned (fCopyEvent) then
    begin
        oldflag := fCancelFlag ;
        fCopyEvent (LogLevel, Info, fCancelFlag) ;
        if oldflag <> fCancelFlag then
            fCopyEvent (LogLevelDiag, 'FTP Cancelled from Copy Event', oldflag) ;
    end ;
end ;

procedure MD5Progress (Obj: TObject; Count: Int64 ; var Cancel: Boolean); // callback
begin
    Application.ProcessMessages ;
    TMagFtpW (Obj).onFtpClientProg64 (Obj, Count, Cancel) ;
end ;

procedure TMagFtpW.onZlibProg (Sender: TObject; Count: Int64; var Cancel: Boolean); // 9 Dec 2007
begin
    Application.ProcessMessages ;
    Cancel := fCancelFlag ;
 //   TMagFtp (Obj).onFtpClientProg64 (Obj, Count, Cancel) ;
end ;

procedure TMagFtpW.EndUnZipEvent (Sender: TObject; FileIndex: Integer; FName: String) ;
var
    newsize: integer ;
begin
    if FName = '' then exit ;
    newsize := GetSize64File (Fname) ;
    doCopyEvent (LogLevelFile, 'Unzipped OK: ' + Fname + ', Size: '
                                                     + Int64ToCStr (newsize)) ;
    doCopyEvent (LogLevelDelimFile, 'Unzipped|' + Fname +
                                   '|' + IntToStr (newsize) + '|1|0|OK|0|0') ;
end ;

procedure TMagFtpW.UnZipHandleMessage(Sender: TObject;
          const MessageID: Integer; const Msg1, Msg2: String;
          const flags: Cardinal; var Return: Integer);
begin
    doCopyEvent (LogLevelFile, 'Fatal Unzip Error: ' + Msg1) ;
    Return := 0 ;
end;

function TMagFtpW.FtpCheckFile (const RemDir, RemFile: UnicodeString ;
                                 var FSize: Int64; var FileUDT: TDateTime): boolean;
var
    DirStream: TStream ;
    FtpFiles: TFileRecsW;
    ret: boolean ;
    RFname, RFType, RFAttr, facts: String ;
    S: UnicodeString ;
    totfiles, I, laststatus: Integer;
begin
    result := false ;
    try
    FSize := -1 ;
    FileUDT := 0 ;
    HostFileName := '' ;
    LocalFileName := '' ;
    HostDirName := RemDir ;

 // 14 Sept 2008 skip Unicode names with substitution characters
    if (Pos ('?', RemDir) > 0) or (Pos ('?', RemFile) > 0) then
    begin
        doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + RemDir + RemFile) ;
        exit ;
    end ;

    if fCurRemDir <> HostDirName then
    begin
        ret := Cwd ;  // change working directory
        if fCancelFlag then exit ;
        if ret then ret := Pwd ;
        if NOT ret then
        begin
            doCopyEvent (LogLevelInfo, 'Can Not Change to Source Directory: ' + HostDirName) ;
            exit ;
        end ;
        fCurRemDir := HostDirName ;
    end ;
    if NOT Connected then exit ;
    if fCancelFlag then exit ;

 // get latest file size and time stamp
    ResumeAt := 0 ; // 4 July 2007 ensure resume position reset
    laststatus := 550 ;
    if ftpFeatMLST in FSupportedExtensions then
    begin
        HostFileName := RemFile ;
        MLST ;  // FTP command - file facts for HostFileName
        if NOT Connected then exit ;
        if fCancelFlag then exit ;
        laststatus := StatusCode ;
     // 17 Sept 2008 WS_FTP MLST with 501 Invalid number of arguments with spaces, so try quoting file name
        if (laststatus = 501) and (Pos (space, RemFile) > 0) then
        begin
            HostFileName := '"' + RemFile + '"' ;
            MLST ;  // FTP command - file facts for HostFileName
            if NOT Connected then exit ;
            if fCancelFlag then exit ;
            laststatus := StatusCode ;
        end ;
        facts := IcsAnsiLowerCaseW (RemFacts) ; // 3 Sept 2006, Serv-U is mixed case
     //  15 March 2006, check we got some facts - parse may have failed
        if Pos ('size', facts) <= 0 then laststatus := 550 ;
        if laststatus = 250 then
        begin
            DecodeMlsResp64 (facts, RFname, RFType, RFAttr, FSize, FileUDT) ;  // note file name ignored
        end
        else
            doCopyEvent (LogLevelDiag, 'MLST Failed with Status ' +
                                         IntToStr (StatusCode) + ' - ' + LastResponse) ;
    end ;

// 15 March 2006, use MDTM if MLST fails
    if (ftpFeatMDTM in FSupportedExtensions) and (laststatus <> 250) then
    begin
        HostFileName := RemFile ;
        MDTM ;  // FTP command - returns UTC time
        if NOT Connected then exit ;
        if fCancelFlag then exit ;
        if StatusCode = 213 then
        begin
            FileUDT := RemFileDT ;
            Size ;   // FTP command
            if fCancelFlag then exit ;
            if StatusCode = 213 then
            begin
                FSize := SizeResult ;
                laststatus := 250 ;  // OK
            end ;
        end
        else
            doCopyEvent (LogLevelDiag, 'MDTM Failed with Status ' +
                                         IntToStr (StatusCode) + ' - ' + LastResponse) ;
    end ;

// 15 March 2006, use LIST if MDTM fails
    if (laststatus <> 250) then
    begin
      // IIS/5 W2K can not LIST directories or files with spaces so need to get whole directory
        HostFileName := RemFile ;
        if Pos (space, RemFile) <> 0 then HostFileName := '' ;
        DirStream := TMemoryStream.Create ;
        LocalFileName := '' ;
        LocalStream := DirStream ;
        SetLength (FtpFiles, 0) ;
        try
            totfiles := 0 ;
            sysDelayX (200) ;
            ret := Dir ;    // lists specific file in HostFileName
            if NOT Connected then exit ;
            if fCancelFlag then exit ;
         // 500 'RT': command not understood.
            if (NOT ret) and (StatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
            begin
                sysDelayX (500) ;
                doCopyEvent (LogLevelDiag, 'Repeating Listing Command, Corrupted PORT Bug') ;
                ret := Dir ;
            end ;
            if NOT Connected then exit ;
            if fCancelFlag then exit ;
            if ret then
            begin
                UnpackFtpFDir (LocalStream, HostDirName, fServBaseDir,
                                       0, FCodePage, fHostType, totfiles, FtpFiles) ;
                if (totfiles >= 1) then  // should only be single file, but may have listed more
                begin
                    S := IcsAnsiLowerCaseW (RemFile) ;
                    for I := 0 to Pred (totfiles) do
                    begin
                        if IcsAnsiLowerCaseW (FtpFiles [I].FrFileName) = S then
                        begin
                            FSize := FtpFiles [I].FrFileBytes ;
                            FileUDT := DateTimeToUTC (FtpFiles [I].FrFileDT) ;
                            break ;
                        end ;
                    end ;
                end ;
            end
            else
            begin
                doCopyEvent (LogLevelDiag, 'DIR Failed with Status ' +  // 4 July 2007 report error
                                         IntToStr (StatusCode) + ' - ' + LastResponse) ;
        // 4 Jan 2008 'Unable to establish data connection - Connection refused' - abort to clean up
                if (StatusCode = 550) and (Pos ('#10061', LastResponse) > 1) then
                begin
                    if Connected then AbortXfer ;
                end ;
            end ;
        finally
            SetLength (FtpFiles, 0) ;
            if Assigned (DirStream) then DirStream.Free ;
            LocalStream := nil;
        end ;
    end ;
    if FSize > 0 then result := true ;
    finally
        if fCancelFlag then
        begin
            result := false ;
            fReqResponse := 'Cancelled Checking File' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
        end ;
    end ;
end ;

// load and save TStringList from/to wide file name

procedure StrLoadFromWideFile (Obj: TStrings; const FileName: UnicodeString);
var
  Stream: TStream;
begin
  Stream := TIcsFileStreamW.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Obj.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure StrSaveToWideFile (Obj: TStrings; const FileName: UnicodeString);
var
  Stream: TStream;
begin
  Stream := TIcsFileStreamW.Create(FileName, fmCreate);
  try
    Obj.SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

// actual FTP download a file, resuming if necessary

function TMagFtpW.IntDownOne (const RemDir, RemFile, RemFull, LocFileFull: UnicodeString ;
                               const RFSize: Int64; RFileUDT: TDateTime): integer ;
var
    fnametmp, newtardir, fnameftp, info, fileext: UnicodeString ;
    ret, resflag: boolean ;
    fstarttick, duration: longword ;
    newsize, partfsize, lastbytes, actualbytes: Int64 ;
    retval, attempts: Integer;
//    LocFileUDT: TDateTime ;
//    LocSize64: Int64 ;
    ResInfRecs: TStringList ;
begin
    result := 1 ;  // fail
    if fCancelFlag then exit ;
    if NOT fLoggedIn then exit ;
    if NOT Connected then exit ;
    fLastProgTick := GetTickCountX - (LongWord (fProgressSecs + 1) * TicksPerSecond) ; // 22 Oct 2008 ensure progress displayed

// 14 Sept 2008 skip Unicode names with substitution characters
    if (Pos ('?', LocFileFull) > 0) then
    begin
        doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + LocFileFull) ;
        doCopyEvent (LogLevelDelimFile, RemFile + '|' + LocFileFull +
                                              '|0|0|1|Inaccessible Unicode File|0|0') ;
        result := 6 ;
        exit ;
    end ;
    if (Pos ('?', RemFull) > 0) then
    begin
        doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + RemFull) ;
        doCopyEvent (LogLevelDelimFile, RemFile + '|' + LocFileFull +
                                              '|0|0|1|Inaccessible Unicode File|0|0') ;
        result := 6 ;
        exit ;
    end ;

// create target directory
    newtardir := IcsExtractFileDirW (LocFileFull) ;
    if NOT IcsForceDirectoriesW (newtardir) then
    begin
        doCopyEvent (LogLevelInfo, 'Can Not Create Directory: ' + newtardir) ;
        exit ;
    end ;
    ResInfRecs := TStringList.Create ;
    try // finally

// try and delete existing file before copying, removing read only if necessary
//    GetAgeSizeFile (LocFileFull, LocFileUDT, LocSize64) ;  // !!! TEMP DIAG
//    if LocSize64 > 0 then                                  // !!! TEMP DIAG
//            doCopyEvent (LogLevelDiag, 'Old Time Stamp (local) ' + Date2Packed (LocFileUDT)) ;
    retval := MagDeleteFileW (LocFileFull, fReplRO) ;
    if retval > 0 then
    begin
        if retval = 1 then
        begin
            doCopyEvent (LogLevelInfo, 'Can Not Replace Read Only Target File: ' +
                                                                     LocFileFull) ;
            doCopyEvent (LogLevelDelimFile, RemFile + '|' + LocFileFull +
                                '|0|0|1|Can Not Replace Read Only Target File|0|0') ;
        end
        else
        begin
            doCopyEvent (LogLevelInfo, 'Delete Target File Failed: ' +
                          LocFileFull + ' - ' + SysErrorMessage (GetLastError)) ;
            doCopyEvent (LogLevelDelimFile, RemFile + '|' + LocFileFull +
                                          '|0|0|1|Delete Target File Failed|0|0') ;
        end ;
        exit ;
   end ;

// tell user what we are doing
    doCopyEvent (LogLevelFile, 'Downloading File: ' + RemFull + ' to ' +
                                LocFileFull  + ', Size ' + Int64ToCStr (RFSize)) ;
    fProgMessBase := fProgMessBase + CRLF_ + RemFull ;
    fProgFileSize := RFSize ;   // keep name and size for event handler

// set compression mode, don't compress certain files
    if fUsingCompression then
    begin
        fileext := IcsExtractFileExtW (IcsAnsiLowerCaseW (LocFileFull));  // 2 Dec 2007 longer list
        if (Pos (fileext, fZlibNoCompExt) > 0) OR (fZlibMaxSize < RFSize) then  // 9 Dec 2007 max size to compress
            TransferMode := ftpTransModeStream
        else
            TransferMode := ftpTransModeZDeflate ;
        if (FCurrTransMode <> TransferMode) then ModeZ ;
    end ;

// set remote directory, should be OK already since just checked remote file
    HostFileName := '' ;
    LocalFileName := '' ;
    HostDirName := RemDir ;
    ResumeAt := 0 ; // 4 July 2007 ensure resume position reset
    if fCurRemDir <> HostDirName then
    begin
        ret := Cwd ;  // change working directory
        if ret then ret := Pwd ;
        if NOT ret then
        begin
            doCopyEvent (LogLevelInfo, 'Can Not Change to Source Directory: ' + HostDirName) ;
            exit ;
        end ;
        fCurRemDir := HostDirName ;
    end ;
    if NOT Connected then exit ;
    if fCancelFlag then exit ;

// see if need to resume previous download
    HostFileName := RemFile ;  // FTP component remote file to download
    if (magftpNoTmpFile in fMagFtpOpts) then  // 6 Jan 2008 - download with correct name
    begin
        fnametmp := LocFileFull ;
        fResFailed := false ;
    end
    else
    begin
        fnametmp := IcsAnsiLowerCaseW (RemFile);
        StringTranChWide (fnametmp, '.', '_') ; // don't mess with directories
        fnametmp := IcsIncludeTrailingPathDelimiterW (IcsAnsiLowerCaseW (newtardir)) + fnametmp ;
        fnameftp := fnametmp + '.ftp' ;    // control file
        fnametmp := fnametmp + '.tmp' ;    // destination file name for FTP
    end ;
    if fResFailed then
    begin
        resflag := false ;
        partfsize := 0 ;
        attempts := 0 ;

   // see if resume info file exists, check file on server has same info
        if IcsFileExistsW (fnameftp) then
        begin
            try
                StrLoadFromWideFile (ResInfRecs, fnameftp) ;  // load string list from Unicode file name
           //     ResInfRecs.LoadFromFile (fnameftp) ;  // resume info text file
                partfsize := GetSize64FileW (fnametmp) ;
                if partfsize > 0 then
                begin
                    if FileInUse (fnametmp) then  // 31 Dec 2007 ensure file not open
                    begin
                        MagDeleteFileW (fnameftp, true) ;
                        partfsize := 0 ;
                        doCopyEvent (LogLevelFile, 'Error Temp File in Use - ' + fnametmp) ;
                        fnametmp := fnametmp + '2' ; // new name
                    end ;
                end ;
                if (partfsize >= (fMinResSize + 100)) and
                                         (ResInfRecs.Count > ResInfLastBytes) then
                begin
                    attempts := AscToInt (ResInfRecs [ResInfAttempts]) ;
                    lastbytes := atoi64 (ResInfRecs [ResInfLastBytes]) ;
                    if (attempts >= fMaxResumeAttempts) then        // 31 Dec 2007
                        doCopyEvent (LogLevelFile, 'Skipped Resume, Too Many Attempts (' +
                                                            IntToStr (fMaxResumeAttempts) + ')')
                    else if (lastbytes = partfsize) then
                        doCopyEvent (LogLevelFile, 'Skipped Resume, Same Size as Last Attempt')
                    else if (RFSize < partfsize) then
                        doCopyEvent (LogLevelFile, 'Skipped Resume, Old File too Large') // 9 Dec 2007
                    else
                    begin
                        if (ResInfRecs [ResInfServer] = fHostName1) and
                           (ResInfRecs [ResInfFName] = String (StringToUtf8 (RemFull))) and
                           (ResInfRecs [ResInfStamp] = FloatToStr (RFileUDT)) and
                           (ResInfRecs [ResInfSize] = IntToStr (RFSize)) then
                        begin
                        // reduce file size in case content is corrupted near end
                            newsize := TruncateFile (fnametmp, partfsize - fMinResSize) ;  // 24 June 2006
                            partfsize := GetSize64FileW (fnametmp) ;
                            if newsize <> partfsize then
                               doCopyEvent (LogLevelFile,
                                   'Failed to Reduce Resume File Size for Overlap')
                            else
                            begin
                                resflag := true ;
                                doCopyEvent (LogLevelFile, 'Resuming Partial File Download from: ' +
                                   Int64ToCStr (partfsize) + ', with Overlap ' +
                                                               IntToCStr (fMinResSize)) ;
                            end ;
                        end
                        else
                            doCopyEvent (LogLevelFile,
                                    'Unable to Resume, Download File Has Changed') ;
                    end ;
                end
                else
                    doCopyEvent (LogLevelFile, 'Skipped Resume, Old File Too Small ' + // 19 July 2007
                                 Int64ToCStr (partfsize) + ', or Invalid Resume Info') ;
            except
            end ;
        end ;
        ShareMode := ftpShareExclusive ;
        onFtpClientProg64 (Self, 0, fCancelFlag) ;
        LocalFileName := fnametmp ;        // FTP component destination file
        if NOT Connected then exit ;
        if fCancelFlag then exit ;
        if resflag then
        begin
            inc (attempts) ;
            fstarttick := GetTickCountX ;
            ResInfRecs [ResInfAttempts] := IntToStr (attempts) ;
            ResInfRecs [ResInfLastBytes] := IntToStr (partfsize) ;
            StrSaveToWideFile (ResInfRecs, fnameftp) ;
//            ResInfRecs.SaveToFile (fnameftp) ;
            doCopyEvent (LogLevelFile, 'Saved File Resume Info ' + fnameftp) ;
            ret := RestGet ;   // resume download into LocalFileName
            if (NOT (ret OR fCancelFlag)) and (StatusCode = 500) and
                                            (Pos ('RT', ErrorMessage) > 1) then
            begin
                doCopyEvent (LogLevelDiag, 'Repeating Get Command, Corrupted PORT Bug') ;
                sysDelayX (500) ;
                ret := RestGet ;
            end ;
            duration := ElapsedTicks (fstarttick) ;
        end
        else
        begin
         // write current file into into .ftp file so we can resume if necessary
            MagDeleteFileW (fnametmp, true) ;
            MagDeleteFileW (fnameftp, true) ;
            ResInfRecs.Clear ;
            ResInfRecs.Add (fHostName1) ;               // ResInfServer = 0
            ResInfRecs.Add (String (StringToUtf8 (RemFull))) ;   // ResInfFName = 1
            ResInfRecs.Add (FloatToStr (RFileUDT)) ;    // ResInfStamp = 2
            ResInfRecs.Add (IntToStr (RFSize)) ;        // ResInfSize = 3
            ResInfRecs.Add ('1') ;                      // ResInfAttempts = 4
            ResInfRecs.Add ('0') ;                      // ResInfLastBytes = 5
            StrSaveToWideFile (ResInfRecs, fnameftp) ;
//            ResInfRecs.SaveToFile (fnameftp) ;
            doCopyEvent (LogLevelFile, 'Saved File Resume Info ' + fnameftp) ;
            fstarttick := GetTickCountX ;
            ret := Get ;  // download it
            if (NOT (ret OR fCancelFlag)) and (StatusCode = 500) and
                                        (Pos ('RT', ErrorMessage) > 1) then
            begin
                doCopyEvent (LogLevelDiag, 'Repeating Get Command, Corrupted PORT Bug') ;
                sysDelayX (500) ;
                ret := Get ;
            end ;
            duration := ElapsedTicks (fstarttick) ;
        end ;
    end
    else
    begin
        LocalFileName := fnametmp ;
        ShareMode := ftpShareExclusive ;
        MagDeleteFileW (fnametmp, true) ;
        MagDeleteFileW (fnameftp, true) ;
        onFtpClientProg64 (Self, 0, fCancelFlag) ;
        if fCancelFlag then exit ;
        fstarttick := GetTickCountX ;
        ret := Get ;  // download it
        if (NOT (ret OR fCancelFlag)) and (StatusCode = 500) and
                                        (Pos ('RT', ErrorMessage) > 1) then
        begin
            doCopyEvent (LogLevelDiag, 'Repeating Get Command, Corrupted PORT Bug') ;
            sysDelayX (500) ;
            ret := Get ;
        end ;
        duration := ElapsedTicks (fstarttick) ;
    end ;
    if duration = 0 then duration := 10 ;  // special case of copy OK but duration too short to measure
    result := 2 ;  // failed after GET
    actualbytes := ByteCount ;  // 2 Jan 2007 keep FTP count before corrupted by LIST

// FTP download failed, give up
    newsize := GetSize64FileW (fnametmp) ;
    info := 'Downloaded File ' + fnametmp + ', Size ' + Int64ToCStr (newsize) ;
    doCopyEvent (LogLevelProg, info) ;
    doCopyEvent (LogLevelDiag, info) ;
    if NOT ret then
    begin
        if fResFailed then
        begin
            if (newsize <= fMinResSize) then  // failed, kill restart if too small
            begin
                MagDeleteFileW (fnameftp, true) ;  // kill restart info
                MagDeleteFileW (fnametmp, true) ;
                doCopyEvent (LogLevelFile, 'Request Failed: Partial File Deleted, ' +   // 19 July 2007
                                     'Too Small to Resume ' + Int64ToCStr (newsize)) ;
            end
            else if (newsize >= RFsize) then  // failed, kill restart if too big or correct size
            begin
                MagDeleteFileW (fnameftp, true) ;  // kill restart info
                MagDeleteFileW (fnametmp, true) ;
                doCopyEvent (LogLevelFile, 'Request Failed: File Deleted, ' +   // 9 Dec 2007
                   'Too Large to Resume, Expected File Size ' + Int64ToCStr (RFSize) +
                                              ', Actual Size ' + Int64ToCStr (newsize))
            end
            else
                doCopyEvent (LogLevelFile, 'Request Failed: Partial File Downloaded, ' +
                         'Resume Allowed, Expected File Size ' + Int64ToCStr (RFSize) +
                                          ', Actual Size ' + Int64ToCStr (newsize)) ;
        end
        else
            MagDeleteFileW (fnametmp, true) ;
        doCopyEvent (LogLevelFile, 'Download Failed: ' + LastResponse) ;
        doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
                     '|0|0|1|Download Failed: ' + LastResponse + '|' +
                                IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
    // 4 Jan 2008 'Unable to establish data connection - Connection refused' - abort to clean up
        if (StatusCode = 550) and (Pos ('#10061', LastResponse) > 1) then
        begin
            if Connected then AbortXfer ;
        end ;
        exit ;
    end ;
    if NOT Connected then exit ;
    if fCancelFlag then exit ;

// check size of downloaded file matches server size, will repeat if allowed
    if newsize <> RFSize then
    begin
        if newsize >= 0 then
        begin
            if NOT fResFailed then
            begin
                MagDeleteFileW (fnametmp, true) ;
                MagDeleteFileW (fnametmp, true) ;
                doCopyEvent (LogLevelFile, 'Request Failed: Partial File Deleted, ' +  // 19 July 2007
                       'Expected File Size ' + Int64ToCStr (RFSize) +
                                          ', Actual Size ' + Int64ToCStr (newsize)) ;
            end
            else if (newsize >= RFsize) then  // failed, kill restart if too big or correct size
            begin
                MagDeleteFileW (fnameftp, true) ;  // kill restart info
                MagDeleteFileW (fnametmp, true) ;
                doCopyEvent (LogLevelFile, 'Request Failed: File Deleted, ' +   // 9 Dec 2007
                   'Too Large to Resume, Expected File Size ' + Int64ToCStr (RFSize) +
                                              ', Actual Size ' + Int64ToCStr (newsize)) ;
            end
            else if (actualbytes <= (fMinResSize + 16)) then  // 31 Dec 2007 failed, not enough downloaded on resume
            begin
                MagDeleteFileW (fnameftp, true) ;  // kill restart info
                MagDeleteFileW (fnametmp, true) ;
                doCopyEvent (LogLevelFile, 'Request Failed: File Deleted, ' +   // 9 Dec 2007
                   'Too Little Downloaded to Resume, Expected File Size ' + Int64ToCStr (RFSize) +
                                              ', Actual Size ' + Int64ToCStr (newsize)) ;
            end
            else
                doCopyEvent (LogLevelFile, 'Request Failed: Partial File Downloaded, ' +
                       'Resume Allowed, Expected File Size ' + Int64ToCStr (RFSize) +
                                          ', Actual Size ' + Int64ToCStr (newsize)) ;
        end
        else
        begin
            MagDeleteFileW (fnameftp, true) ;  // kill restart info
            MagDeleteFileW (fnametmp, true) ;
            doCopyEvent (LogLevelFile, 'Request Failed: No File Downloaded') ;
        end ;
        doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
               '|0|0|1|Download Failed|' + IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
        exit ;
    end ;
    MagDeleteFileW (fnameftp, true) ;  // kill restart info, got file OK

 // check MD5 or CRC if possible and repeat if allowed
    if ((ftpFeatMD5 in FSupportedExtensions) OR
                                    (ftpFeatXMD5 in FSupportedExtensions)) then
    begin
        doCopyEvent (LogLevelProg, 'Getting Server MD5SUM ' + fnametmp) ;
        fstarttick := GetTickCountX ;
        PosStart := 0 ;  // force XMD5 to do entire file, in case size changed
        PosEnd := 0 ;
        if (ftpFeatMD5 in FSupportedExtensions) then  // 22 Nov 2007 support XMD5, 6 Apr 2009 prefer MD5
            ret := MD5
        else
            ret := XMD5 ;  // get MD5SUM
        if NOT Connected then exit ;
        if fCancelFlag then exit ;
        if ret and (Length (fMD5Result) = 32) then
        begin
            fProgMessBase := 'Checking Local MD5SUM ' + fnametmp ;
            doCopyEvent (LogLevelProg, fProgMessBase) ;
            fProgFileSize := newsize ;   // keep name and size for event handler
            info := FtpFileMD5 (fnametmp, Self, MD5Progress) ;  // 8 Apr 2009 widestring version
            if (fMD5Result <> info) { and (Length (info) = 32) } then // 6 Apr 2009 don't assume blank MD5sum is OK
            begin
                doCopyEvent (LogLevelInfo, 'MD5SUM Compare Failed: ' +
                          fnametmp + ';Rem='+ fMD5Result + ';Loc=' + info) ;
                doCopyEvent (LogLevelFile, 'Download Failed: MD5SUM Compare Failed') ;
                doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
                             '|0|0|1|Download Failed: MD5SUM Compare Failed|' +
                                     IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
                result := 3 ;
                exit ;
            end ;
            doCopyEvent (LogLevelInfo, 'MD5SUM Check OK: ' + fnametmp + ' took ' +
                IntToStr (ElapsedSecs (fstarttick)) + ' secs; Result ' + fMD5Result) ;
        end
        else
            doCopyEvent (LogLevelInfo, 'MD5SUM Response Failed: ' + fnametmp + ';Rem='+ fMD5Result) ;   // 6 Apr 2009
    end
    else if (ftpFeatXCrc in FSupportedExtensions) then  // added 10 July 2006
    begin
        doCopyEvent (LogLevelProg, 'Getting Server CRC32 ' + fnametmp) ;
        fstarttick := GetTickCountX ;
        PosStart := 0 ;
        PosEnd := 0 ;
     // PosStart := 1 ;
     // PosEnd := newsize ;
        ret := XCRC ;  // get CRC32B
        if NOT Connected then exit ;
        if fCancelFlag then exit ;
        if ret and (Length (fCrcResult) >= 5) then
        begin
            fProgMessBase := 'Checking Local CRC32 ' + fnametmp ;
            doCopyEvent (LogLevelProg, fProgMessBase) ;
            fProgFileSize := newsize ;   // keep name and size for event handler
            info := FtpFileCRC32B (fnametmp, Self, MD5Progress) ;  // 15 Apr 2009
            if (Length (info) = 8) and (Pos (fCrcResult, info) = 0) then
            begin
                doCopyEvent (LogLevelInfo, 'CRC32 Compare Failed: ' +
                          fnametmp + ';Rem='+ fCrcResult + ';Loc=' + info) ;
                doCopyEvent (LogLevelFile, 'Download Failed: CRC32 Compare Failed') ;
                doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
                             '|0|0|1|Download Failed: CRC32 Compare Failed|' +
                                     IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
                result := 3 ;
                exit ;
            end ;
            doCopyEvent (LogLevelInfo, 'CRC32 Check OK: ' + fnametmp + ' took ' +
                IntToStr (ElapsedSecs (fstarttick)) + ' secs; Result ' + fCrcResult) ;
        end ;
    end ;

// replace old file, removing read only if necessary
    if NOT (magftpNoTmpFile in fMagFtpOpts) then  // 6 Jan 2008 - download with correct name
    begin
        retval := MagRenameFileW (fnametmp, LocFileFull, true, fReplRO) ;
        if retval <> 0 then
        begin
            if (retval = 1) then
            begin
                doCopyEvent (LogLevelInfo, 'Can Not Replace Read Only File: ' + LocFileFull) ;
                doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
                                        '|0|0|1|Can Not Replace Read Only File|0|0') ;
            end
            else
            begin
                doCopyEvent (LogLevelInfo, 'Final File Rename Failed: ' +
                                      LocFileFull + ' - ' +  SysErrorMessage (retval)) ;
                doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
                      '|0|0|1|Final File Rename Failed: ' + SysErrorMessage (retval)+ '|0|0') ;
            end ;
            doCopyEvent (LogLevelInfo, 'File Copied as: ' + LocalFileName) ;
            result := 4 ;
            exit ;
        end ;
    end ;
    if NOT Connected then exit ;
    if fCancelFlag then exit ;
    result := 0 ;  // got here so successful

// set file time stamp,
    if RFileUDT > 10 then
    begin
        doCopyEvent (LogLevelDiag, 'Updating Time Stamp: ' + LocFileFull +
                                        ' to (UTC)=' + Date2Packed (RFileUDT)) ;
        if NOT UpdateUFileAge (LocFileFull, RFileUDT) then
             doCopyEvent (LogLevelInfo, 'Failed to Update Time Stamp: ' + LocFileFull) ;
    end ;

// tell user we did it OK
    doCopyEvent (LogLevelFile, 'Download OK, Size: ' + Int64ToCStr (newsize)) ;
    doCopyEvent (LogLevelDelimFile, RemFull + '|' + LocFileFull +
                '|' + IntToStr (newsize) + '|1|0|OK|' + IntToStr (duration) +
                                                     '|' + IntToStr (actualbytes)) ;

    finally
        ResInfRecs.Free ;
        if Assigned (LocalStream) then  // 30 Dec 2007 ensure file never left open
        begin
            LocalStream.Destroy;
            LocalStream := nil;
        end;
    end ;
end ;

function TMagFtpW.FtpDownOneFile (const FdirSrc, Fnamesrc, Fnametar: UnicodeString ;
                                            Replopt: TFileCopyRepl) : TTaskResult ;
var
    code, fullsrcname, remdir, locfilefull: UnicodeString ;
    flag: boolean ;
    RFSize, TarFSize: Int64 ;
    TarFileDT: TDateTime;
    retval, loop: Integer;
    RFileUDT: TDateTime ;
begin
    result := TaskResFail ;
    fReqResponse := '' ;
    fCancelFlag := false ;
    if NOT fLoggedIn then
    begin
        result := TaskResFail ;
        fReqResponse := 'Must Login to Server' ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;
    if NOT Connected then
    begin
        result := TaskResFail ;
        fReqResponse := 'No Connection to Server, Cancelled Downloading Files' ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;

 // make sure trailing slash for directory
    remdir := IncludeTrailingUnixDelimiterW (FdirSrc) ;
    fullsrcname := remdir + fnamesrc ;

// 14 Sept 2008 skip Unicode names with substitution characters
    if (Pos ('?', fullsrcname) > 0) then
    begin
        doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + fullsrcname) ;
        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar +
                                              '|0|0|1|Inaccessible Unicode File|0|0') ;
        exit ;
    end ;
    if (Pos ('?', Fnametar) > 0) then
    begin
        doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + Fnametar) ;
        doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar +
                                              '|0|0|1|Inaccessible Unicode File|0|0') ;
        exit ;
    end ;

 // add base directory to front of file directory, change to it, if needed
    doCopyEvent (LogLevelFile, 'Check Exists ' + fullsrcname) ;
    if remdir [1] = '/' then
        remdir := fServRootDir + Copy (remdir, 2, 999)
    else
        remdir := fServRootDir + remdir ;

// FTP download file, multiple attempts
    for loop := 1 to (fFailRepeat + 1) do        // 31 Dec 2007, check file each attempt
    begin
// check if remote file available, get size and UTC time stamp
        if NOT FtpCheckFile (remdir, fnamesrc, RFSize, RFileUDT) then
        begin
            result := TaskResFail ;
            fReqResponse := 'Can Not Find File: ' + fnamesrc ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
            exit ;
        end ;
        if NOT Connected then
        begin
            result := TaskResFail ;
            fReqResponse := 'No Connection to Server, Cancelled Downloading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
            exit ;
        end ;
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Downloading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
            exit ;
        end ;

    // see if replacing existing file
        flag := GetUAgeSizeFileW (Fnametar, TarFileDT, TarFSize) ;
        if (NOT flag) and (replopt <> FCReplAlways) then
              doCopyEvent (LogLevelDiag, 'Download Not Skipped: Target Not Found ' + Fnametar) ;
        if flag and (replopt <> FCReplAlways) then
        begin
            flag := MagCheckReplace (replopt, true, OneSecond * 2, RFSize, TarFSize,
                                                                    RFileUDT, TarFileDT) ;
            if fDispFiles then code := {code + }'; Src=' +
                        DateTimeToStr (RFileUDT) + '; Tar=' + DateTimeToStr (TarFileDT) ;
            if NOT flag then
            begin
                result := TaskResOKNone ;
                fReqResponse := 'Download Skipped: ' + fnamesrc ;
                doCopyEvent (LogLevelInfo, fReqResponse + code) ;
                doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + fnametar +
                                                         '|0|0|1|Download Skipped|0|0') ;
                exit ;
            end ;
            doCopyEvent (LogLevelDiag, 'Download Not Skipped: ' + fnamesrc + code) ;
        end ;

    // see if making lower case for PC
        if CaseFile = FileLowerCase then
            locfilefull := IcsAnsiLowerCaseW (Fnametar)
        else
            locfilefull := Fnametar ;

    // now get file
        fProgMessBase := 'Downloading File' ;
        retval := IntDownOne (remdir, Fnamesrc, fullsrcname, locfilefull, RFSize, RFileUDT) ;
        doCopyEvent (LogLevelProg, '') ;
        if retval = 0 then
        begin
            result := TaskResOKNew ;
            exit ;
        end ;
        if NOT Connected then
        begin
            result := TaskResFail ;
            fReqResponse := 'No Connection to Server, Cancelled Downloading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
            exit ;
        end ;
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Downloading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
            exit ;
        end ;
        if NOT (retval in [2, 3]) then exit ;  // fail download or fail MD5
     //   if (StatusCode = 501) then exit ;  // 19 Oct 2005 permissions or start failed, 5 Jan 2008 ignore
        if (loop < (fFailRepeat + 1)) then   // 31 Dec 2007
                doCopyEvent (LogLevelInfo, 'Repeating Download: ' + fnamesrc) ;
    end ;
end ;

// FTP download multiple local files
// returns false if error, with fReqResponse completed

function TMagFtpW.FtpDownload (const CheckFiles: boolean): TTaskResult ;
begin
    fCancelFlag := false ;
// logon to FTP server
    result := FtpLogon ;
    if result <> TaskResOKNew then exit ;

// download multiple files
    try
        result := FtpDownFiles (CheckFiles) ;
    finally
        FtpLogoff ;
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Downloading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
        end ;
        doCopyEvent (LogLevelProg, '') ;
        fCancelFlag := false ;
    end ;
end ;

// FTP log off and close down - check file not left open

procedure TMagFtpW.FtpLogoff ;
var
    endtick: longword ;
begin
    try
        if fLoggedIn and Connected then
        begin
            doCopyEvent (LogLevelInfo, 'Logging Off from FTP Server') ;
            Quit ;   // clean log off
        end ;
        fLoggedIn := false ;
        endtick := GetTickCount + (5 * TicksPerSecond) ;
        while (ControlSocket.State <> wsClosed) or (DataSocket.State <> wsClosed) do
        begin
            if endtick < GetTickCount then
            begin
                doCopyEvent (LogLevelInfo, 'Socket Still Open, Aborting FTP') ;
                Abort ;
                break ;
            end ;
            sysDelayX (50) ;
        end ;
{$IFDEF USE_SSL}
        if Assigned (fExternalSslSessionCache) then
                                            fExternalSslSessionCache.Flush;
{$ENDIF}
    except
        doCopyEvent (LogLevelInfo, 'Exception Logging-Off FTP') ;
    end ;
end ;


// FTP download multiple local files
// returns false if error, with fReqResponse completed

function TMagFtpW.FtpDownFiles (const CheckFiles: boolean): TTaskResult ;
var
    newfname, fnametar, cursrcdir, newtardir, fnametmp: UnicodeString ;
    tempdir, info, newsubdirs, basetardir: UnicodeString ;
    donenr, nodeltot, CurDelFiles, retval: integer ;
    newsize, totsize, delsize, RFSize: Int64;
    RFileUDT: TDateTime ;
    I, J, loop, CopyOnlyTot, DelDirTot: integer ;
    starttick: DWORD ;
    SrcFileRec: PTFileRecW ;
    DelDirList: UStringArray ;
    CopyOnlyList: UStringArray ;
    {$IFDEF Zipping}
    VCLUnZip: TVCLUnZip ;
    {$ENDIF}
begin
    fCancelFlag := false ;
    if NOT fLoggedIn then
    begin
        result := TaskResFail ;
        fReqResponse := 'Must Login to Server' ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;
    if NOT Connected then
    begin
        result := TaskResFail ;
        fReqResponse := 'No Connection to Server, Cancelled Downloading Files' ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;
{$IFDEF Zipping}
    VCLUnZip := Nil ;
    if fZipped then
    begin
          VCLUnZip := TVCLUnZip.Create (self) ;
          VCLUnZip.OnEndUnZip := EndUnZipEvent ;
          VCLUnZip.OnHandleMessage := UnZipHandleMessage ;
    end ; {$ENDIF}
    SetLength (DelDirList, 20) ;
    CopyOnlyList := Nil ;
    DelDirTot := 0 ;
    try   // finally
    CurDelFiles := 0 ;

// 8 Apr 2009 - fSrcFName may include directories and masks - yyyy-mm"/D"dd"/*.zip"
    newsubdirs := '' ;
    newfname := fSrcFName ;
    if (NOT fSpecificFiles) and fMask then
    begin
        UnixToDosPathW (newfname) ;
      // warning - GetMaskedName currently ANSI only in Delphi 7 to D2007
        newfname := GetMaskedName (newfname, fPrev, fLocalHost) ;
        newsubdirs := IcsExtractFilePathW (newfname) ;  // DOS delims
        if Length (newsubdirs) > 0 then
        begin
            if newsubdirs [1] = '\' then newsubdirs := Copy (newsubdirs, 2, 999) ;
        end;
        newfname := IcsExtractFileNameW (newfname) ;
    end ;
    if fSpecificFiles then newfname := '*.*' ;

// set base directory, combining root and remote dir, and optionally masked sub directories
    if fSrcDir = '' then fSrcDir := '/' ;
    if fSrcDir [1] = '/' then
        fServBaseDir := fServRootDir + Copy (fSrcDir, 2, 999)
    else
        fServBaseDir := fServRootDir + fSrcDir ;
    fServBaseDir := IncludeTrailingUnixDelimiterW (fServBaseDir) ;
    if fMaskRemDir and (newsubdirs <> '') then  // 8 Apr 2009 add sub-directories
    begin
        fServBaseDir := fServBaseDir + newsubdirs ;
        DosToUnixPathW (fServBaseDir) ;
        fServBaseDir := IncludeTrailingUnixDelimiterW (fServBaseDir) ;
    end;

// don't delete target files unless processing full directories
    if fDelOldTar then
    begin
        if NOT (fCopyType in [FCTypeArchDir, FCTypeAllDir]) then
        begin
            doCopyEvent (LogLevelInfo, 'Delete Old Target Files only available when using Copy Type All Directory') ;
            fDelOldTar := false ;
        end ;
    end ;

// build list of source files on FTP server
    doCopyEvent (LogLevelInfo, 'Locating files to Download') ;
    if fCancelFlag then
    begin
        result := TaskResAbort ;
        exit ;
    end ;
    result := FtpDir (SrcFiles, SrcFileList) ;  // uses fServBaseDir
    if result in [TaskResFail, TaskResAbort] then exit ;
    if fDispRDir then doCopyEvent (LogLevelInfo,
           'Source Files on FTP Server' + CRLF_ + FmtFileDirListW (SrcFileList, false)) ;
    TotSrcFiles := SrcFileList.Count ;
    info := fSrcDir + newsubdirs ;
    DosToUnixPathW (info) ;
    doCopyEvent (LogLevelFile, 'Source Directory: ' + info) ;
    if TotSrcFiles = 0  then
    begin
        result := TaskResOKNone ;
        fReqResponse := 'No Source Files Found' ;
        exit ;
    end ;
    Application.ProcessMessages ;
    if fCancelFlag then exit ;

// build list of target files, so we don't download unnecessary stuff
    basetardir := IcsExcludeTrailingPathDelimiterW (fTarDir) ;
    if fMaskLocDir and (newsubdirs <> '') then  // 8 Apr 2009 add sub-directories
    begin
        basetardir := IcsIncludeTrailingPathDelimiterW (basetardir) +
                                            IcsExcludeTrailingPathDelimiterW (newsubdirs) ;
    end;
    doCopyEvent (LogLevelFile, 'Target Directory: ' + basetardir) ;
    if NOT IcsForceDirectoriesW (basetardir) then
    begin
        result := TaskResFail ;
        fReqResponse := 'Can Not Create Target Directory: ' + basetardir ;
        exit ;
    end ;
    if fCancelFlag then exit ;
    TotTarFiles := fMagFileCopy.GetDirList (basetardir, newfname, fCopyType,
                                    fSubDirs, 0, 0, TarFiles, TarFileList) ;
    if fDispLDir then doCopyEvent (LogLevelInfo,
                'Target Files on PC' + CRLF_ + FmtFileDirListW (TarFileList, false)) ;
    Application.ProcessMessages ;
    if fCancelFlag then exit ;

// compare source and target files, see what to download
    fTotProcFiles := fMagFileCopy.SelectCopyFileList (SrcFileList, TarFileList,
                 newfname, fCopyType, fRepl, fDiffStampMins, true, fIgnoreFileExt,
                                                             fSkippedFiles, false) ;
    if fCancelFlag then exit ;

// see if deleting old target files no longer in source directories
    if fDelOldTar and (TotTarFiles <> 0) then
    begin
        CurDelFiles := fMagFileCopy.SelectCopyFileList (TarFileList, SrcFileList, '*.*',
            FCTypeAllDir, FCReplNever, 0, false, fIgnoreFileExt, nodeltot, false) ;
    end ;
    if fTotProcFiles = 0  then
    begin
        result := TaskResOKNone ;
        if fSkippedFiles <> 0 then
            fReqResponse := 'All Source Files Skipped Download'
        else
            fReqResponse := 'No Source Files Selected to Download' ;
        exit ;
    end ;

// see if only copying a list of specific files, deselect any we don't need
    if fSpecificFiles then
    begin
        CopyOnlyList := StrArraySplit (IcsAnsiLowerCaseW (fSrcFName), '|') ;   // not sorted!!
        CopyOnlyTot := Length (CopyOnlyList) ;
        if CopyOnlyTot = 0 then
        begin
            result := TaskResOKNone ;
            fReqResponse := 'No Specific Source Files Selected to Download' ;
            exit ;
        end ;
        for I := 0 to Pred (TotSrcFiles) do
        begin
            SrcFileRec := SrcFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy = FCStateSelect then
                begin
                    if StrArrayPosOf (IcsAnsiLowerCaseW (FrFileName), CopyOnlyList) < 0 then
                    begin
                         FrFileCopy := FCStateNone ;
                         dec (fTotProcFiles) ;
                    end ;
                end ;
            end ;
        end ;
        if fTotProcFiles = 0 then
        begin
            result := TaskResOKNone ;
            fReqResponse := 'No Specific Source Files Selected to Download' ;
            exit ;
        end ;
    end ;

// check size of files that will be deleted, make sure FT resume files not deleted
    delsize := 0 ;
    info := '' ;
    if fDelOldTar and (CurDelFiles <> 0) then
    begin
        info := info + CRLF_ + 'Old Files Selected for Deletion are: ' + CRLF_ ;
        for I := 0 to Pred (TotTarFiles) do
        begin
            SrcFileRec := TarFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy = FCStateSelect then
                begin
                    if Pos ('_', FrFileName) > 0 then  // resume temporary file
                    begin
                        fnametmp := IcsAnsiLowerCaseW (IcsExtractFileExtW (FrFileName)) ;
                        if (fnametmp = '.tmp') or (fnametmp = '.ftp') then
                                                    FrFileCopy := FCStateNone ;
                    end ;
                end ;
                if FrFileCopy = FCStateSelect then
                begin
                    inc (delsize, FrFileBytes) ;
                    if CheckFiles then info := info + FrFullName +
                           ', Size ' + Int64ToCStr (FrFileBytes) + CRLF_ ;
                end ;
            end ;
        end ;
    end ;
    if fCancelFlag then exit ;

// find size of stuff to copy
    info := info + CRLF_ + 'Files Selected for Downloading are: ' + CRLF_ ;
    newsize := 0 ;
    for I := 0 to Pred (TotSrcFiles) do
    begin
        SrcFileRec := SrcFileList [I] ;
        with SrcFileRec^ do
        begin
            if FrFileCopy = FCStateSelect then
            begin
                newsize := newsize + FrFileBytes ;
                if CheckFiles then info := info + FrFullName +
                            ', Size ' + Int64ToCStr (FrFileBytes) + CRLF_ ;
            end ;
        end ;
    end ;
    if CheckFiles then doCopyEvent (LogLevelInfo, info) ;
    doCopyEvent (LogLevelInfo, 'Source Files Skipped ' +
                                            IntToStr (fSkippedFiles)) ;
    info := 'Selected Total Files ' + IntToStr (fTotProcFiles) +
                                    ', Total Size ' + Int64ToCStr (newsize) ;
    if CurDelFiles <> 0 then info := info + CRLF_ + 'Old Files to Delete ' +
            IntToCStr (CurDelFiles) + ', Total Size ' + Int64ToCStr (delsize) ;
    doCopyEvent (LogLevelInfo, info) ;

// stop now if only checking what will be downoaded - test only
    if CheckFiles then
    begin
        result := TaskResOKNone ;
        exit ;
    end ;
    if fCancelFlag then exit ;

// see if deleting old target files first
    if fDelOldTar and (CurDelFiles <> 0) then
    begin
        doCopyEvent (LogLevelInfo, 'Deleting Old Local Files: ' + basetardir) ;
        for I := 0 to Pred (TotTarFiles) do
        begin
            Application.ProcessMessages ;
            if fCancelFlag then exit ;
            SrcFileRec := TarFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy = FCStateSelect then
                begin
            // 14 Sept 2008 skip Unicode names with substitution characters
                    if (Pos ('?', FrFullName) > 0) then
                    begin
                        doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + FrFullName) ;
                        doCopyEvent (LogLevelDelimFile, '|' + FrFullName +
                                              '|0|0|0|Inaccessible Unicode File|0|0') ;
                        FrFileCopy := FCStateFailed ;
                        inc (fProcFailFiles) ;
                        continue ;
                    end ;
                    doCopyEvent (LogLevelFile, 'Deleting: ' + FrFullName) ;
                    retval := MagDeleteFileW (FrFullName, fReplRO) ;
                    if retval <= 0 then
                    begin
                        doCopyEvent (LogLevelDelimFile, '|' + FrFullName +
                                            '|0|0|0|Old Target File Deleted|0|0') ;
                        inc (fDelOKBytes, FrFileBytes) ;
                        FrFileCopy := FCStateOK ;
                        inc (fDelOKFiles) ;

                     // add directory to list we'll try and delete later, 24 Apr 2003
                        newtardir := Trim (FrSubDirs) ;
                        if newtardir <> '' then
                                StrArrayAddSorted (DelDirList, newtardir, DelDirTot) ;
                    end
                    else
                    begin
                        doCopyEvent (LogLevelInfo, 'File Delete Failed: ' +
                                      FrFullName+ ' - ' +  SysErrorMessage (retval)) ;
                        FrFileCopy := FCStateFailed ;
                        inc (fProcFailFiles) ;
                    end ;
                end ;
            end ;
        end ;
    end ;
//    ClearTarList ;  // don't need target list any more, get memory back

// FTP download
    doCopyEvent (LogLevelInfo, 'Started FTP Download from: ' + fHostName) ;
    Application.ProcessMessages ;
    if fCancelFlag then exit ;

// start real FTP downloading
    cursrcdir := '.,.,.' ;  // illegal
    donenr := 0 ;
    totsize := 0 ;
    LocalStream := Nil ;  // download to files, not stream
    starttick := GetTickCountX ;
    for I := 0 to Pred (TotSrcFiles) do
    begin
        Application.ProcessMessages ;
        if fCancelFlag then exit ;
        if NOT Connected then
        begin
            doCopyEvent (LogLevelInfo, 'Lost FTP Control Connection, Abandoning Downloads') ;
            exit ;
        end ;
        SrcFileRec := SrcFileList [I] ;
        with SrcFileRec^ do
        begin
            if FrFileCopy <> FCStateSelect then continue ;
            inc (donenr) ;
        //    fnamesrc := FrFullName ;
            if (FrSubDirs [1] = '/') then     // remove leading slash
                tempdir := fServBaseDir + Copy (FrSubDirs, 2, 200)
            else
                tempdir := fServBaseDir + FrSubDirs ;
            if cursrcdir <> tempdir then
            begin
                cursrcdir := tempdir ;
                if (FrSubDirs [1] = '/') then     // check leading slash
                    newtardir := basetardir + FrSubDirs
                else
                    newtardir := basetardir + '/' + FrSubDirs ;
                UnixToDosPathW (newtardir) ;
                if CaseFile = FileLowerCase then
                                        newtardir := IcsAnsiLowerCaseW (newtardir) ;
            end ;

        // see if making lower case for PC
            if CaseFile = FileLowerCase then
                fnametar := IcsAnsiLowerCaseW (newtardir + FrFileName)
            else
                fnametar := newtardir + FrFileName ;

         // FTP download file, limit maximum attempts
            retval := 0 ;
            for loop := 1 to (fFailRepeat + 1) do  // 31 Dec 2007
            begin
                fProgMessBase := 'Downloading File ' + IntToStr (donenr) +
                                           ' of ' + IntToStr (fTotProcFiles) ;
             // try and get latest file size and time stamp, might have changed
                if FtpCheckFile (cursrcdir, FrFileName, RFSize, RFileUDT) then
                begin
                    if (RFSize > 0) and (RFileUDT > 10) then
                    begin
                        if RFileUDT > SrcFileRec^.FrFileUDT then FrFileUDT := RFileUDT ;
                        if RFSize <> FrFileBytes then
                        begin
                            doCopyEvent (LogLevelFile, 'File Size Changed: ' +
                               FrFullName  + ' from ' + Int64ToCStr (FrFileBytes)  + ' to ' +
                                                             Int64ToCStr (RFSize)) ;
                            FrFileBytes := RFSize ;
                        end ;
                    end ;
                end
                else
                begin
                    doCopyEvent (LogLevelFile, 'File Not Found On FTP Server: ' + FrFullName) ;
                    // continue ;  currently ignore error in case LIST file did not work
                end ;
                if FrFileUDT < 10 then FrFileUDT := DateTimeToUTC (FrFileDT) ;  // 18 Aug 2004

            // FTP get file
                FrFileCopy := FCStateCopying ;
                sysDelayX (200) ;  // 11 Aug 2004 - short delay to try and fix corrupted PORT command
                retval := IntDownOne (cursrcdir, FrFileName, FrFullName,
                                                     fnametar, FrFileBytes, FrFileUDT) ;
                doCopyEvent (LogLevelProg, '') ;
                if retval = 0 then break ;
                if fCancelFlag then exit ;
                if NOT Connected then exit ;
                if NOT (retval in [2, 3]) then break ;  // fail download or fail MD5
             //   if (StatusCode = 501) then break ;  // 19 Oct 2005 permissions or start failed 5 Jan 2008 ignore
                if (loop < (fFailRepeat + 1)) then   // 31 Dec 2007
                    doCopyEvent (LogLevelInfo, 'Repeating Download: ' + FrFullName) ;
            end ;
            if retval <> 0 then
            begin
                inc (fProcFailFiles) ;
                FrFileCopy := FCStateFailed ;
            end
            else
            begin
                newsize := GetSize64FileW (fnametar) ;
                inc (fProcOKFiles) ;
                inc (totsize, newsize) ;
                FrFileCopy := FCStateOK ;

          // see if unzipping it
                {$IFDEF Zipping}
                if fZipped and (IcsAnsiLowerCaseW (IcsExtractFileExtW (fnametar)) = '.zip') then
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
                            if ZipHasComment then info := ZipComment + CRLF_ ;
                            for J := 0 to Pred (Count) do
                            begin
                               info := info + Format (sDirLine, [Filename [J],
                                   Int64ToCStr (UnCompressedSize [J]), ' ',
                                   DateToStr (DateTime [J]) + ' ' + TimeToStr
                                       (DateTime [J]), Pathname[J]]) + CRLF_ ;
                            end ;
                            doCopyEvent (LogLevelInfo, 'Unzipping Files:' + CRLF_ + info) ;

                        // extract all files
                            FilesList.Clear ;
                            DoAll := true ;
                            if (fZipDir = '') and (fZipPath >= PathSpecific)
                                                     then fZipPath := PathNew ;
                            DestDir := IcsExtractFileDirW (fnametar) ;     // Set destination directory
                            RecreateDirs := false ;
                            RootDir := '' ;   // base subdirectory
                            if fZipPath in [PathOriginal, PathNewOrig,
                                    PathSpecOrig] then RecreateDirs := true ;
                            if fZipPath in [PathNew, PathNewOrig] then
                               DestDir := IcsExtractFileDirW (fnametar) + '\' +
                                                   IcsExtractNameOnlyW (fnametar) ;
                            if fZipPath >= PathSpecific then
                                                          DestDir := fZipDir ;
                            if NOT IcsForceDirectoriesW (DestDir) then
                            begin
                                doCopyEvent (LogLevelFile, 'Failed to Create Unzip Dir: ' + DestDir) ;
                                doCopyEvent (LogLevelDelimFile, fnametar + '|' + DestDir +
                                         '|0|0|1|Failed to Create Unzip Dir|0|0') ;
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
                                    MagDeleteFileW (fnametar, true) ;
                                    doCopyEvent (LogLevelDelimFile, fnametar +
                                         '| |0|0|0|File Deleted After Unzipping|0|0') ;
                                end ;
                            end
                            else
                            begin
                                doCopyEvent (LogLevelFile, 'Failed to Unzip: ' + fnametar) ;
                                doCopyEvent (LogLevelDelimFile, fnametar + '|' + DestDir +
                                             '|0|0|1|Failed to Unzip File|0|0') ;
                            end ;
                        end
                        else
                        begin
                            doCopyEvent (LogLevelInfo, 'Zip File Corrupted:' + fnametar) ;
                            doCopyEvent (LogLevelDelimFile, fnametar + '|' + DestDir +
                                             '|0|0|1|Zip File Corrupted|0|0') ;
                        end ;
                        ClearZip;
                    end ;
                end ;  {$ENDIF}
                if NOT Connected then exit ;

          // see if deleting remote file
                if fDelDone then
                begin
                    HostFileName := FrFileName ;
                    doCopyEvent (LogLevelFile, 'Deleting: ' + HostFileName) ;
                    Dele ;   // delete remote file
                    doCopyEvent (LogLevelDelimFile, FrFullName +
                               '| |0|0|0|Source File Deleted After Copy|0|0') ;
                end ;
           end ;
       end ;
    end ;

// see if any old empty directories to delete
    if DelDirTot <> 0 then
    begin
        doCopyEvent (LogLevelInfo, 'Checking for Empty Target Directories: ' + basetardir) ;
        for I := 0 to Pred (DelDirTot) do
        begin
            newtardir := basetardir + DelDirList [I] ;
            J := Length (newtardir) ;
            while J >= 2 do
            begin
                if newtardir [J] = '\' then dec (J) ;
                newtardir := copy (newtardir, 1, J) ;
                doCopyEvent (LogLevelDiag, 'Checking Directory Empty: ' +  newtardir) ;
                if NOT CheckDirAnyW (newtardir) then
                begin
                 // doCopyEvent (LogLevelDiag, 'Will Delete Dir: ' + curdir) ;
                   if RemoveDir (newtardir) then
                        doCopyEvent (LogLevelFile, 'Removed Directory OK: ' + newtardir)
                    else
                        doCopyEvent (LogLevelFile, 'Failed to Remove Directory: ' + newtardir) ;
                    while J >= 2 do   // search for lower level directory
                    begin
                        dec (J) ;
                        if newtardir [J] = '\' then break ;
                    end ;
                end
                else
                    break ;
            end ;
        end ;
    end ;

    if NOT fCancelFlag then
    begin
        I := ElapsedTicks (starttick) + 1 ;
        if (I > 100000) or (totsize > 1000000) then
        begin
            I := I div 1000 ;  // allow for bizarre divide by zero error
            if I > 0 then
                newsize := totsize div I
            else
                newsize := 0 ;
        end
        else
        begin
            newsize := (totsize * 1000) div I ;
            I := I div 1000 ;
        end ;
        result := TaskResOKNone ;
        if fProcFailFiles <> 0 then
            result := TaskResFail
        else
        begin
            if fProcOKFiles <> 0 then result := TaskResOKNew ;
        end ;
        doCopyEvent (LogLevelInfo, 'Finished, files downloaded OK: ' +
                IntToCStr (fProcOKFiles) + ', failed: ' + IntToStr
                    (fProcFailFiles) + ', skipped: ' +  IntToStr (fSkippedFiles)) ;
        doCopyEvent (LogLevelDelimTot, fSrcDir + '|' + basetardir + '|' +
                IntToStr (totsize) + '|' + IntToStr (fProcOKFiles) +
                           '|' + IntToStr (fProcFailFiles) + '|Totals|') ;
        doCopyEvent (LogLevelInfo, 'Total bytes downloaded ' + Int64ToCStr (totsize) +
                    ', duration ' + TimerToStr (SecsToTime (I)) +
                        ', average speed ' +  IntToCStr (newsize) + ' chars/sec') ;
        if fDelOKFiles <> 0 then doCopyEvent (LogLevelInfo,
                'Old target files deleted OK: ' + IntToCStr (fDelOKFiles) +
                            ', Total bytes deleted ' + Int64ToCStr (fDelOKBytes)) ;
    end ;
    finally
        if NOT Connected then
        begin
            result := TaskResFail ;
            fReqResponse := 'No Connection to Server, Cancelled Downloading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
        end
        else if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Downloading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
        end ;
        {$IFDEF Zipping}
        if fZipped and (Assigned (VCLUnZip)) then VCLUnZip.Free ;  {$ENDIF}
   end ;
end;

// FTP single file internal upload

function TMagFtpW.IntUpOne (const LocFileFull, RemDir, RemFile: UnicodeString;
                                  const RFSize: Int64; RFileUDT: TDateTime): integer ;
var
    fnametar, fnametmp, fnameftp, newtardir, info, remfull, fileext: UnicodeString ;
    remfileUDT: TDateTime ;
    I, J, attempts: integer;
    newsize, uploadsize, partfsize, lastbytes, actualbytes: Int64 ;
    fstarttick, duration: longword ;
    ret, resflag, zipflag: boolean ;
    ResInfRecs: TStringList ;
    {$IFDEF Zipping} VCLZip: TVCLZip ; {$ENDIF}


    // create new directory on FTP server, one level at a time
    // unless directory already exists
    function CreateFtpDir (XRemDir: UnicodeString): Boolean;
    var
        I, J: integer ;
        newdir, remaindir: UnicodeString ;
    begin

    // ignore server root, it must exist so don't try and create it
        remaindir := XRemDir ;
        if fServRootDir <> remaindir then
        begin
            I := Length (fServRootDir) ;
            if fServRootDir = copy (XRemDir, 1, I) then
                            remaindir := copy (XRemDir, succ (I), 999) ;
        end ;

    // remove leading / but not trailing /
        if (length (remaindir) > 1) and (remaindir [1] = '/') then
                                remaindir := copy (remaindir, 2, 999) ;
        J := 0 ;
        while true do
        begin
           // check if directory exists
            HostDirName := XRemDir ;  // full path for Cwd
            result := true ;
            if fCurRemDir = HostDirName then exit ;
            result := Cwd ;
            if fCancelFlag then exit ;
            if result then result := Pwd ;
            if result then fCurRemDir := HostDirName ;
            if result then exit ;
            if Length (remaindir) = 0 then exit ;

        // start in server root, first loop only
            if J = 0 then
            begin
                HostDirName := fServRootDir ;
                result := Cwd ;
                if result then result := Pwd ;
                if NOT result then exit ;
            end ;
            if fCancelFlag then exit ;

       // get next directory to create
            I := Pos ('/', remaindir) ;
            if I <= 1 then exit ;  // dead!!
            newdir  := Copy (remaindir, 1, Pred (I)) ;
            remaindir := Copy (remaindir, succ (I), 999) ;

       // change relatively to directory to see if it exists, else make it
            HostDirName := newdir ;   // relative path for Cwd
            HostFileName := newdir ;  // relative path for Mkd
            result := Cwd ;
            if fCancelFlag then exit ;
            if NOT result then
            begin
                Mkd ;   // make directory
                if fCancelFlag then exit ;

                // make sure it's OK
                if Length (remaindir) > 0 then   // if not reached end of path
                begin
                    result := Cwd ;
                    if result then result := Pwd ;
                    if NOT Result then exit ;
                    fCurRemDir := HostDirName ;
                end ;
            end ;
            inc (J) ;
            if J > 10 then exit ;   // prevent looping
        end ;
    end;

begin
    result := 1 ;  // fail
    if fCancelFlag then exit ;
    if NOT fLoggedIn then exit ;
    if NOT Connected then exit ;
    {$IFDEF Zipping}
    VCLZip := Nil ;
    if fZipped then
    begin
        VCLZip := TVCLZip.Create (self) ;
        VCLZip.OnHandleMessage := UnZipHandleMessage ;
    end ;
    {$ENDIF}

// 14 Sept 2008 skip Unicode file with non-ANSI characters unlesss UTF8 enabled
    if (FCodePage <> CP_UTF8) and (NOT CheckUnicodeToAnsi (LocFileFull)) then
    begin
        doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + LocFileFull) ;
        doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + RemFile +
                                              '|0|0|1|Inaccessible Unicode File|0|0') ;
        result := 6 ;  // fail
        exit ;
    end ;

// 15 Sept 2008 skip old FTP resume file
    I := Length (LocFileFull) ;
    J := Pos ('.ftp', LocFileFull) ;
    if (J > 4) and ((J + 3) = I) then
    begin
        if LocFileFull [J - 4] = '_' then
        begin
            doCopyEvent (LogLevelDiag, 'Skipped Old FTP Resume File: ' + LocFileFull) ;
            doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + RemFile +
                                                      '|0|0|1|Old FTP Resume File|0|0') ;
            result := 6 ;  // fail
            exit ;
        end ;
    end ;

    ResInfRecs := TStringList.Create ;
    try  // finally
    fLastProgTick := GetTickCountX - (LongWord (fProgressSecs + 1) * TicksPerSecond) ; // 22 Oct 2008 ensure progress displayed

// create archive directory
    if fUpArchive then
    begin
        fDelDone := false ;
        if NOT IcsForceDirectoriesW (fUpArchDir) then
        begin
            fReqResponse := 'Can Not Create Archive Directory' ;
            exit ;
        end ;
    end ;

// create remote directories,  see if making lower case for FTP server
    if CaseFile = FileLowerCase then
        newtardir := IcsAnsiLowerCaseW (RemDir)
    else
        newtardir := RemDir ;
    HostDirName := '/' ;
    if fCurRemDir <> newtardir then
    begin
        doCopyEvent (LogLevelDiag, 'Changing to Directory: ' + newtardir) ;
        ret := CreateFtpDir (newtardir) ;
        if NOT ret then
        begin
            fReqResponse := 'Can Not Create Target Directory' ;
            exit ;
        end ;
    end ;
    if NOT Connected then exit ;
    if fCancelFlag then exit ;

// see if making lower case for FTP server
    if CaseFile = FileLowerCase then
        fnametar := IcsAnsiLowerCaseW (RemFile)
    else
        fnametar := RemFile ;
    remfull := newtardir + fnametar ;
    LocalFileName := LocFileFull ;  // FTP source file to upload
    ShareMode := ftpShareDenyWrite ;
    uploadsize := RFSize ;

// see if zipping file
    zipflag := false ;
    {$IFDEF Zipping}
    if fZipped and (IcsAnsiLowerCaseW (IcsExtractFileExtW (LocFileFull)) <> '.zip') then
    begin
        With VCLZip do
        begin
            doCopyEvent (LogLevelFile, 'Compressing ' + LocFileFull) ;
            ArchiveStream := TMemoryStream.Create ; // Create archive in a memory stream */
            FilesList.Clear ;
            FilesList.Add (LocFileFull) ;  // zip one file  */
            ZipComment := 'Zipped by FTP' ;
            if Zip <> 1 then    // this zips files into an archive in ArchiveStream, a memory stream */
            begin
                doCopyEvent (LogLevelFile, 'Zip Compression Failed ' + LocFileFull) ;
            end
            else
            begin
                zipflag := true ;
                ArchiveStream.Seek (0, soFromBeginning) ;
                LocalStream := ArchiveStream ;
                LocalFileName := '' ;
                uploadsize := ArchiveStream.Size ;
                if fZipExtFmt = ExtnReplace then
                    fnametar := ChangeFileExt (fnametar, '.zip')
                else
                    fnametar := fnametar + '.zip' ;
                doCopyEvent (LogLevelFile, 'Compressed ' + LocFileFull  + ' from ' +
                       Int64ToCStr (RFSize)  + ' to ' + Int64ToCStr (uploadsize)) ;
            end ;
        end ;
    end ;  {$ENDIF}

// start upload, to temporary file, resuming if possible
    fnametmp := IcsAnsiLowerCaseW (fnametar) ;
    if (magftpNoTmpFile in fMagFtpOpts) then  // 6 Jan 2008 - download with correct name
    begin
        fResFailed := false ;
    end
    else
    begin
        StringTranChWide (fnametmp, '.', '_') ;// don't mess with directories
        fnametmp := fnametmp + '.tmp' ;    // destination file name for FTP
    end ;
    doCopyEvent (LogLevelFile, fProgMessBase + ', ' + LocFileFull  + ' to '
                             + remfull + ', Size ' + Int64ToCStr (uploadsize)) ;
    fProgMessBase :=  fProgMessBase + CRLF_ + RemFile ;
    fProgFileSize := uploadsize ;   // keep name and size for event handler
    onFtpClientProg64 (Self, 0, fCancelFlag) ;
    if NOT Connected then exit ;
    if fCancelFlag then exit ;

// set compression mode, don't compress zip files
    if fUsingCompression then
    begin
        fileext := IcsExtractFileExtW (IcsAnsiLowerCaseW (LocFileFull));  // 2 Dec 2007 longer list
        if (Pos (fileext, fZlibNoCompExt) > 0) or zipflag OR
                                 (fZlibMaxSize < uploadsize) then  // 9 Dec 2007 max size to compress
            TransferMode := ftpTransModeStream
        else
            TransferMode := ftpTransModeZDeflate ;
        if (FCurrTransMode <> TransferMode) then ModeZ ;
    end ;
    sysDelayX (200) ;
    if (NOT zipflag) and fResFailed then
    begin
        fnameftp := IcsExtractFileNameW (LocFileFull) ;
        StringTranChWide (fnameftp, '.', '_') ;// don't mess with directories
        fnameftp := IcsExtractFilePathW (LocFileFull) + fnameftp + '.ftp' ;  // control file in local directory
        resflag := false ;
        attempts := 0 ;
        partfsize := 0 ;

   // see if local resume info file and TMP file on FTP server exist
        if IcsFileExistsW (fnameftp) then
        begin
            try
                StrLoadFromWideFile (ResInfRecs, fnameftp) ;  // load string list from Unicode file name
             //   ResInfRecs.LoadFromFile (fnameftp) ;
                if NOT FtpCheckFile (newtardir, fnametmp, partfsize, remfileUDT) then
                begin
                     partfsize := 0 ;
                     doCopyEvent (LogLevelFile, 'Unable to Resume, Partial File Not Found: ' + fnametmp) ;
                end ;
                if NOT Connected then exit ;
                if fCancelFlag then exit ;
                if (partfsize >= fMinResSize) and (ResInfRecs.Count > ResInfLastBytes) then
                begin
                    attempts := AscToInt (ResInfRecs [ResInfAttempts]) ;
                    lastbytes := atoi64 (ResInfRecs [ResInfLastBytes]) ;
                    if (attempts >= fMaxResumeAttempts) then        // 31 Dec 2007
                        doCopyEvent (LogLevelFile, 'Skipped Resume, Too Many Attempts (' +
                                                            IntToStr (fMaxResumeAttempts) + ')')
                    else if (lastbytes = partfsize) then
                        doCopyEvent (LogLevelFile, 'Skipped Resume, Same Size as Last Attempt')
                    else if (uploadsize < partfsize) then
                        doCopyEvent (LogLevelFile, 'Skipped Resume, Part File too Large') // 9 Dec 2007
                    else
                    begin
                        if (ResInfRecs [ResInfServer] = fHostName1) and
                           (ResInfRecs [ResInfFName] = String (StringToUtf8(remfull))) and
                           (ResInfRecs [ResInfStamp] = FloatToStr (RFileUDT)) and
                           (ResInfRecs [ResInfSize] = IntToStr (uploadsize)) then
                        begin
                            if partfsize = uploadsize then
                            begin
                                partfsize := 0 ;
                                doCopyEvent (LogLevelFile,
                                                'Unable to Resume, Upload File Same Size') ;
                            end
                            else
                            begin
                                resflag := true ;
                                doCopyEvent (LogLevelFile, 'Resuming Partial File Upload from: ' +
                                                                     Int64ToCStr (partfsize)) ;
                            end ;
                        end
                        else
                            doCopyEvent (LogLevelFile, 'Unable to Resume, Upload File Has Changed') ;
                    end ;
                end ;
                sysDelayX (200) ;  // short delay to allows old socket to close
            except
            end ;
        end ;
        onFtpClientProg64 (Self, 0, fCancelFlag) ;
        if fCancelFlag then exit ;
        if NOT Connected then exit ;

   // 22 Nov 2007 check if sufficient space ALLOcated for upload
        if (NOT (magftpNoFeat in fMagFtpOpts)) then
        begin
            PosEnd := uploadsize - partfsize ;
        //    PosEnd := 20123456789; // !! TESTING
            Allo ;
            if StatusCode = 501 then   // 500 command not understood, 200 OK
            begin
                if (Pos ('insufficient', IcsAnsiLowerCaseW (LastResponse)) > 0) or
                    (Pos ('not enough', IcsAnsiLowerCaseW (LastResponse)) > 0) then
                begin
                    doCopyEvent (LogLevelFile, 'Upload Failed: ' + LastResponse) ;
                    doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + fnametmp +
                                     '|0|0|1|Upload Failed: ' + LastResponse + '|0|0') ;
                    exit ;
                end ;
            end ;
        end;
        if fCancelFlag then exit ;
        if NOT Connected then exit ;

     // start resume FTP upload
        HostFileName := fnametmp ;
        LocalFileName := LocFileFull ;  // may have been lost during FtpCheckFile
        ResumeAt := 0 ; // 4 July 2007 ensure resume position reset
        if resflag then
        begin
            inc (attempts) ;
            fstarttick := GetTickCountX ;
            ResInfRecs [ResInfAttempts] := IntToStr (attempts) ;
            ResInfRecs [ResInfLastBytes] := IntToStr (partfsize) ;
            StrSaveToWideFile (ResInfRecs, fnameftp) ;
//            ResInfRecs.SaveToFile (fnameftp) ;
            doCopyEvent (LogLevelFile, 'Saved File Resume Info ' + fnameftp) ;
            ResumeAt := partfsize ;
            doCopyEvent (LogLevelFile, 'Resuming FTP Upload from Offset ' +
                 Int64ToCStr (partfsize) + ', Total File Size ' + Int64ToCStr (uploadsize)) ;
            ret := RestPut ;   // resume download into LocalFileName
            if fCancelFlag then exit ;
            if NOT Connected then exit ;
            if (NOT ret) and (StatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
            begin
                doCopyEvent (LogLevelDiag, 'Repeating Put Command, Corrupted PORT Bug') ;
                sysDelayX (500) ;
                ret := RestPut ;
            end ;
            if fCancelFlag then exit ;
            if NOT Connected then exit ;
            duration := ElapsedTicks (fstarttick) ;
        end
        else
        begin
         // write current file into into .ftp file so we can resume if necessary
            MagDeleteFileW (fnameftp, true) ;
            ResInfRecs.Clear ;
            ResInfRecs.Add (fHostName1) ;               // ResInfServer = 0
            ResInfRecs.Add (String (StringToUtf8 (remfull))) ;   // ResInfFName = 1
            ResInfRecs.Add (FloatToStr (RFileUDT)) ;    // ResInfStamp = 2
            ResInfRecs.Add (IntToStr (uploadsize)) ;    // ResInfSize = 3
            ResInfRecs.Add ('1') ;                      // ResInfAttempts = 4
            ResInfRecs.Add ('0') ;                      // ResInfLastBytes = 5
            StrSaveToWideFile (ResInfRecs, fnameftp) ;
//            ResInfRecs.SaveToFile (fnameftp) ;
            doCopyEvent (LogLevelFile, 'Saved File Resume Info ' + fnameftp) ;
            fstarttick := GetTickCountX ;
            ret := Put ;  // upload it
            if fCancelFlag then exit ;
            if NOT Connected then exit ;
            if (NOT ret) and (StatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
            begin
                doCopyEvent (LogLevelDiag, 'Repeating Put Command, Corrupted PORT Bug') ;
                sysDelayX (500) ;
                ret := Put ;
            end ;
            if fCancelFlag then exit ;
            if NOT Connected then exit ;
            duration := ElapsedTicks (fstarttick) ;
        end ;
    end
    else
    begin
   // 22 Nov 2007 check if sufficient space ALLOcated for upload
        if (NOT (magftpNoFeat in fMagFtpOpts)) then
        begin
            PosEnd := uploadsize ;
       //     PosEnd := 20123456789; // !! TESTING
            Allo ;
            if StatusCode = 501 then      // 500 command not understood, 200 OK
            begin
                if (Pos ('insufficient', IcsAnsiLowerCaseW (LastResponse)) > 0) or
                    (Pos ('not enough', IcsAnsiLowerCaseW (LastResponse)) > 0) then
                begin
                    doCopyEvent (LogLevelFile, 'Upload Failed: ' + LastResponse) ;
                    doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + fnametmp +
                                 '|0|0|1|Upload Failed: ' + LastResponse + '|0|0') ;
                    exit ;
                end ;
            end ;
        end ;

    // start FTP upload
        MagDeleteFileW (fnameftp, true) ;
        HostFileName := fnametmp ;
      // don't set LocalFileName, it may have been replaced by a zipped LocalStream
        if fCancelFlag then exit ;
        if NOT Connected then exit ;
        fstarttick := GetTickCountX ;
        ret := Put ;  // upload it
        if fCancelFlag then exit ;
        if NOT Connected then exit ;
        if (NOT ret) and (StatusCode = 500) and (Pos ('RT', ErrorMessage) > 1) then
        begin
            doCopyEvent (LogLevelDiag, 'Repeating Put Command, Corrupted PORT Bug') ;
            sysDelayX (500) ;
            ret := Put ;
        end ;
        duration := ElapsedTicks (fstarttick) ;
    end ;
    result := 2 ;  // FTP done
    actualbytes := ByteCount ;  // 2 Jan 2007 keep FTP count before corrupted by LIST
    if fCancelFlag then exit ;
    if NOT Connected then exit ;
    if NOT ret then
    begin
        if actualbytes < fMinResSize then MagDeleteFileW (fnameftp, true) ;  // 15 Sept 2008 kill resume file if insufficient copied
        doCopyEvent (LogLevelFile, 'Upload Failed: ' + LastResponse) ;
        doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir + fnametar +
               '|0|0|1|Upload Failed: ' + LastResponse + '|'+ IntToStr (duration) +
                                                        '|' + IntToStr (actualbytes)) ;
    // 4 Jan 2008 'Unable to establish data connection - Connection refused' - abort to clean up
        if (StatusCode = 550) and (Pos ('#10061', LastResponse) > 1) then
        begin
            if Connected then AbortXfer ;
        end ;
        exit ;
    end ;

// check file arrived OK
    sysDelayX (200) ;  // short delay to allows old socket to close
    info := 'Uploaded File ' + fnametar ;
    doCopyEvent (LogLevelProg, info) ;
    if FtpCheckFile (newtardir, fnametmp, newsize, remfileUDT) then
    begin
        if fCancelFlag then exit ;
        if NOT Connected then exit ;
        if newsize <> uploadsize then
        begin
            doCopyEvent (LogLevelFile, 'File Wrong Size on Server: ' + fnametar +
                '; Client ' + Int64ToCStr (uploadsize) + '; Server ' + Int64ToCStr (newsize)) ;
            doCopyEvent (LogLevelFile, 'Upload Failed: ' + LastResponse) ;
            doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir + fnametar +
               '|0|0|1|Upload Failed: ' + LastResponse + '|'+ IntToStr (duration) +
                                                             '|' + IntToStr (actualbytes)) ;
            HostFileName := fnametmp ;
            Dele ;   // delete temp file, ignore error
            exit ;
        end ;
    end
    else
    begin
        doCopyEvent (LogLevelFile, '!!! File Not Found On FTP Server: ' + HostFileName) ;
        // exit ;  currently ignore error in case LIST file did not work
    end ;
    if NOT Connected then exit ;
    if fCancelFlag then exit ;
    MagDeleteFileW (fnameftp, true) ;  // kill resume file

 // check MD5 or CRC32 if possible (not from zip stream yet) and repeat if allowed
    HostFileName := fnametmp ;
    if (NOT zipflag) and ((ftpFeatMD5 in FSupportedExtensions) or
                                 (ftpFeatXMD5 in FSupportedExtensions)) then
    begin
        doCopyEvent (LogLevelProg, 'Getting Remote MD5SUM ' + LocFileFull) ;
        fstarttick := GetTickCountX ;
        PosStart := 0 ;  // force XMD5 to do entire file, in case size changed
        PosEnd := 0 ;
        if (ftpFeatMD5 in FSupportedExtensions) then  // 22 Nov 2007 support XMD5, 6 Apr 2009 but prefer MD5
            ret := MD5
        else
            ret := XMD5 ;  // get MD5SUM for remote file, compare with local file
        if fCancelFlag then exit ;
        if NOT Connected then exit ;
        if ret and (Length (fMD5Result) = 32) then
        begin
            fProgMessBase := 'Checking Local MD5SUM ' + LocFileFull ;
            doCopyEvent (LogLevelProg, fProgMessBase) ;
            fProgFileSize := uploadsize ;   // keep name and size for event handler
            info := FtpFileMD5 (LocFileFull, Self, MD5Progress) ;  // 8 Apr 2009 - widestring version
            if { (Length (info) = 32) and } (fMD5Result <> info) then  // 6 Apr 2009 don't assume blank MD5sum is OK
            begin
                doCopyEvent (LogLevelInfo, 'MD5SUM Compare Failed: ' +
                              LocalFileName + ';Rem='+ fMD5Result + ';Loc=' + info) ;
                doCopyEvent (LogLevelFile, 'Upload Failed: MD5SUM Compare Failed') ;
                doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir +
                    fnametar + '|0|0|1|Upload Failed: MD5SUM Compare Failed|' +
                                 IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
                HostFileName := fnametmp ;
                Dele ;   // delete temp file, ignore error
                result := 3 ;
                exit ;
            end ;
            doCopyEvent (LogLevelInfo, 'MD5SUM Check OK: ' + LocFileFull + ' took ' +
                IntToStr (ElapsedSecs (fstarttick)) + ' secs; Result ' + fMD5Result) ;
        end
        else
            doCopyEvent (LogLevelInfo, 'MD5SUM Response Failed: ' + fnametmp + ';Rem='+ fMD5Result) ;   // 6 Apr 2009
    end
    else if (NOT zipflag) and (ftpFeatXCrc in FSupportedExtensions) then
    begin
        doCopyEvent (LogLevelProg, 'Getting Remote CRC32 ' + LocFileFull) ;
        fstarttick := GetTickCountX ;
        PosStart := 0 ;
        PosEnd := 0 ;
    //  PosStart := 1 ;
    // PosEnd := uploadsize ;
        ret := XCRC ;  // get CRC32 for remote file, compare with local file
        if fCancelFlag then exit ;
        if NOT Connected then exit ;
        if ret and (Length (fCrcResult) >= 5) then
        begin
            fProgMessBase := 'Checking Local CRC32 ' + LocFileFull ;
            doCopyEvent (LogLevelProg, fProgMessBase) ;
            fProgFileSize := uploadsize ;   // keep name and size for event handler
            info := FtpFileCRC32B (LocFileFull, Self, MD5Progress) ;   // 15 Apr 2009
            if (Length (info) = 8) and (Pos (fCrcResult, info) = 0) then
            begin
                doCopyEvent (LogLevelInfo, 'CRC32 Compare Failed: ' +
                        LocalFileName + ';Rem='+ fCrcResult + ';Loc=' + info) ;
                doCopyEvent (LogLevelFile, 'Upload Failed: CRC32 Compare Failed') ;
                doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir +
                    fnametar + '|0|0|1|Upload Failed: CRC32 Compare Failed|'+
                                IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
                HostFileName := fnametmp ;
                Dele ;   // delete temp file, ignore error
                result := 3 ;
                exit ;
            end ;
            doCopyEvent (LogLevelInfo, 'CRC32 Check OK: ' + LocFileFull + ' took ' +
                 IntToStr (ElapsedSecs (fstarttick)) + ' secs; Result ' + fCrcResult) ;
        end ;
    end ;

// rename temporary file, deleting original
    if NOT (magftpNoTmpFile in fMagFtpOpts) then  // 6 Jan 2008 - download with correct name
    begin
        HostFileName := fnametar ;
        Dele ;   // delete old target file, ignore error if may not exist
        if fCancelFlag then exit ;
        if NOT Connected then exit ;
        HostFileName := fnametmp ;   // rename from
        LocalFileName := fnametar ;  // rename to
        ret := Ren ;  // rename file (sends RNFR and RNTO commands)
        if NOT Connected then exit ;
        if fCancelFlag then exit ;
        if NOT ret then
        begin
            doCopyEvent (LogLevelFile, 'Final Rename Failed from: ' + fnametmp +
                                                                    ' to ' + fnametar) ;
            doCopyEvent (LogLevelFile, 'Upload Failed: ' + LastResponse) ;
            doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir + fnametar +
                   '|0|0|1|Upload Failed: ' + LastResponse + '|'+
                                        IntToStr (duration) + '|' + IntToStr (actualbytes)) ;
            exit ;
        end ;
    end ;

// successful upload
    result := 0 ; // success
    if duration = 0 then duration := 10 ;  // special case of copy OK but duration too short to measure
    doCopyEvent (LogLevelFile, 'Upload OK') ;
    doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + newtardir + fnametar +
            '|' + IntToStr (uploadsize) + '|1|0|OK|' + IntToStr (duration) +
                                                         '|' + IntToStr (actualbytes)) ;
    if fCancelFlag then exit ;

// try and set file time stamp on server, few support command
    HostFileName := fnametar ;
    if ftpFeatMFMT in FSupportedExtensions then
    begin
        RemFileDT := RFileUDT ;       // set UTC time !!
        ret := MFMT ;   // FTP command
        if NOT ret then doCopyEvent (LogLevelInfo,
                                'Failed to Modify File Stamp: ' + LastResponse) ;
    end
    else if ftpFeatMDTMYY in FSupportedExtensions then
    begin
        RemFileDT := RFileUDT ;       // set UTC time !!
        ret := MDTMYY ;  // FTP command
        if NOT ret then doCopyEvent (LogLevelInfo,
                                 'Failed to Modify File Stamp: ' + LastResponse) ;
    end ;
    if NOT Connected then exit ;
    if fCancelFlag then exit ;

 // immediate delete or move
    if fDelDone then
    begin
        doCopyEvent (LogLevelFile, 'Deleting: ' + LocFileFull) ;
        MagDeleteFileW (LocFileFull, true) ;
        doCopyEvent (LogLevelDelimFile, LocFileFull +
                             '| |0|0|0|Source File Deleted After Copy|0|0') ;
    end ;
    if fUpArchive then
    begin
        doCopyEvent (LogLevelFile, 'Moving to Archive Directory: ' + LocFileFull) ;
        fnametar := IcsIncludeTrailingPathDelimiterW (fUpArchDir) + IcsExtractFileNameW (LocFileFull) ;
        ret := IcsRenameFileW (LocFileFull, fnametar) ;
        if NOT ret then
        begin
            doCopyEvent (LogLevelFile,
                'Failed to Move File to Archive Directory, Using Unique Name') ;
            fnametar := IcsIncludeTrailingPathDelimiterW (fUpArchDir) + FormatDateTime
                  ('"FTP-at-"yyyymmdd"-"hhnnss-z', Now) + IcsExtractFileExtW (LocFileFull) ;
            ret := IcsRenameFileW (LocFileFull, fnametar) ;
        end ;
        if ret then doCopyEvent (LogLevelFile, 'Archived as: ' + fnametar) ;
    end ;
    if fTimeStamp then
    begin
        doCopyEvent (LogLevelFile, 'Updating Local Timestamp: ' + LocFileFull) ;
        if RFileUDT > 10 then UpdateFileAge (LocFileFull, RFileUDT) ;
    end ;
    finally
        ResInfRecs.Free ;
        if Assigned (LocalStream) then  // 30 Dec 2007 ensure file never left open
        begin
            LocalStream.Destroy;
            LocalStream := nil;
        end;
        {$IFDEF Zipping}
        if fZipped then
        begin
            if Assigned (VCLZip.ArchiveStream) then
            begin
                VCLZip.ArchiveStream.Free ;
                VCLZip.ClearZip ;   // clears stream
            end ;
            if (Assigned (VCLZip)) then VCLZip.Free ;
        end ;   {$ENDIF}
    end ;
end ;

function TMagFtpW.FtpUpOneFile (const LocFileFull, RemTarDir, RemTarFile: UnicodeString;
                                            Replopt: TFileCopyRepl) : TTaskResult ;
var
    code, fulltarname, remdir: UnicodeString ;
    flag: boolean ;
    SrcFSize, RFSize: Int64 ;
    SrcFileUDT, RFileUDT: TDateTime ;
    retval, loop: integer;
begin
    result := TaskResFail ;
    fReqResponse := '' ;
    fCancelFlag := false ;
    if NOT fLoggedIn then
    begin
        result := TaskResFail ;
        fReqResponse := 'Must Login to Server' ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;
    if NOT Connected then
    begin
        result := TaskResFail ;
        fReqResponse := 'No Connection to Server, Cancelled Uploading Files (1)' ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;

 // make sure trailing slash for directory
    remdir := RemTarDir ;
    remdir := IncludeTrailingUnixDelimiterW (remdir) ;
    fulltarname := remdir + RemTarFile ;

// 14 Sept 2008 skip Unicode file with non-ANSI characters unlesss UTF8 enabled
    if (FCodePage <> CP_UTF8) and (NOT CheckUnicodeToAnsi (LocFileFull)) then
    begin
        doCopyEvent (LogLevelInfo, 'Skipped Inaccessible Unicode Name: ' + LocFileFull) ;
        doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + fulltarname +
                                              '|0|0|1|Inaccessible Unicode File|0|0') ;
        exit ;
    end ;

    doCopyEvent (LogLevelFile, 'Check Exists ' + LocFileFull) ;
    if NOT GetUAgeSizeFileW (LocFileFull, SrcFileUDT, SrcFSize) then
    begin
        result := TaskResFail ;
        fReqResponse := 'Can Not Find File: ' + LocFileFull ;
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        exit ;
    end ;

 // add base directory to front of file directory, change to it, if needed
    if remdir [1] = '/' then
        remdir := fServRootDir + Copy (remdir, 2, 999)
    else
        remdir := fServRootDir + remdir ;

// check if remote file available, get size and UTC time stamp, see if replacing it
    flag := FtpCheckFile (remdir, RemTarFile, RFSize, RFileUDT) ;
    if flag and (replopt <> FCReplAlways) then
    begin
        flag := MagCheckReplace (replopt, true, OneSecond * 2, SrcFSize, RFSize,
                                                               SrcFileUDT, RFileUDT) ;
        if fDispFiles then code := '; Src=' +
                    DateTimeToStr (SrcFileUDT) + '; Tar=' + DateTimeToStr (RFileUDT) ;
        if NOT flag then
        begin
            result := TaskResOKNone ;
            fReqResponse := 'Upload Skipped: ' + LocFileFull ;
            doCopyEvent (LogLevelInfo, fReqResponse + code) ;
            doCopyEvent (LogLevelDelimFile, LocFileFull + '|' + fulltarname +
                                                        '|0|0|1|Upload Skipped|0|0') ;
            exit ;
        end ;
        doCopyEvent (LogLevelDiag, 'Upload Not Skipped: ' + LocFileFull + code) ;
    end ;
    if fCancelFlag then exit ;

// FTP upload file, limit maximum attempts
    for loop := 1 to (fFailRepeat + 1) do  // 31 Dec 2007
    begin
        fProgMessBase := 'Uploading File' ;
        retval := IntUpOne (locfilefull, remdir, RemTarFile, SrcFSize, SrcFileUDT) ;
        doCopyEvent (LogLevelProg, '') ;
        if retval = 0 then
        begin
            result := TaskResOKNew ;
            exit ;
        end ;
        if NOT Connected then  // 11 Aug 2005
        begin
            result := TaskResFail ;
            fReqResponse := 'No Connection to Server, Cancelled Uploading Files (2)' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
            exit ;
        end ;
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Uploading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
            exit ;
        end ;
        if NOT (retval in [2, 3]) then exit ;  // fail download or fail MD5
     //   if (StatusCode = 501) then exit ;  // 19 Oct 2005 permissions or start failed 5 Jan 2008 ignore
        if (loop < (fFailRepeat + 1)) then   // 31 Dec 2007
            doCopyEvent (LogLevelInfo, 'Repeating Upload: ' + locfilefull) ;
    end ;
    fCancelFlag := false ;
end ;

// FTP upload multiple local files
// returns false if error, with fReqResponse completed

function TMagFtpW.FtpUpload (const CheckFiles: boolean): TTaskResult ;
var
    newfname, fnamesrc, cursrcdir, newtardir: UnicodeString ;
    ret: boolean ;
    tempdir, info, newsubdirs, basesrcdir: UnicodeString ;
    I, donenr, CurDelFiles, nodeltot, loop, retval, CopyOnlyTot: integer ;
    newsize, totsize, delsize, SrcFSize: Int64 ;
    SrcFileUDT: TDateTime ;
    starttick: DWORD ;
    SrcFileRec: PTFileRecW ;
    CopyOnlyList: UStringArray ;
begin
    fCancelFlag := false ;
    CurDelFiles := 0 ;
    fSrcDir := ExcludeTrailingBackslash (fSrcDir) ;
//    CopyOnlyList := TStringList.Create ;
    CopyOnlyList := Nil ;
    try

// 8 Apr 2009 - fSrcFName may include directories and masks - yyyy-mm"/D"dd"/*.zip"
    newsubdirs := '' ;
    newfname := fSrcFName ;
    if (NOT fSpecificFiles) and fMask then
    begin
        UnixToDosPathW (newfname) ;
      // warning - GetMaskedName currently ANSI only in Delphi 7
        newfname := GetMaskedName (newfname, fPrev, fLocalHost) ;
        newsubdirs := IcsExtractFilePathW (newfname) ;  // DOS delims
        if Length (newsubdirs) > 0 then
        begin
            if newsubdirs [1] = '\' then newsubdirs := Copy (newsubdirs, 2, 999) ;
        end;
        newfname := IcsExtractFileNameW (newfname) ;
    end ;
    if fSpecificFiles then newfname := '*.*' ;

// build list of source files on PC - before accessing server
    basesrcdir := IcsExcludeTrailingPathDelimiterW (fSrcDir) ;
    if fMaskLocDir and (newsubdirs <> '') then  // 8 Apr 2009 add sub-directories
    begin
        basesrcdir := IcsIncludeTrailingPathDelimiterW (basesrcdir) +
                                            IcsExcludeTrailingPathDelimiterW (newsubdirs) ;
    end;
    doCopyEvent (LogLevelFile, 'Source Directory: ' + basesrcdir) ;
    fMagFileCopy.GetDirList (basesrcdir, newfname, fCopyType,
                                fSubDirs, 0, 0, SrcFiles, SrcFileList) ;
    TotSrcFiles := SrcFileList.Count ;
    if TotSrcFiles = 0  then
    begin
        result := TaskResOKNone ;
        fReqResponse := 'No Source Files Found' ;
        exit ;
    end ;

    result := TaskResOKNone ;
    Application.ProcessMessages ;
    if fCancelFlag then exit ;

// don't delete target files unless processing full directories
    if fDelOldTar then
    begin
        if NOT (fCopyType in [FCTypeArchDir, FCTypeAllDir]) then
        begin
            doCopyEvent (LogLevelInfo, 'Delete Old Target Files only available when using Copy Type All Directory') ;
            fDelOldTar := false ;
        end ;
    end ;
    if fDispLDir then doCopyEvent (LogLevelInfo,
                'Source Files on PC' + CRLF_ + FmtFileDirListW (SrcFileList, false)) ;

// logon to FTP server
    doCopyEvent (LogLevelInfo, 'Connecting to FTP Server: ' + fHostName) ;
    result := FtpLogon ;
    if result <> TaskResOKNew then exit ;
    doCopyEvent (LogLevelInfo, 'Succesfully logged onto FTP Server: ' +
                                                            fUserName) ;
    Syst ;  // not sure if this is useful
    doCopyEvent (LogLevelInfo, Copy (LastResponse, 5, 999)) ;

 // 8 Apr 2009 base directory already set in FtpLogon, optionally add masked sub directories
    if fMaskRemDir and (newsubdirs <> '') then
    begin
        fServBaseDir := fServBaseDir + newsubdirs ;
        DosToUnixPathW (fServBaseDir) ;
        fServBaseDir := IncludeTrailingUnixDelimiterW (fServBaseDir) ;
    end;

// build list of target files, so we don't copy unnecessary stuff
// ignore errors, there may be nothing on server, yet
    doCopyEvent (LogLevelInfo, 'Checking files already on FTP Server: ' + fServBaseDir) ;
    result := FtpDir (TarFiles, TarFileList) ;
    if result = TaskResAbort then exit ;
    TotTarFiles := TarFileList.Count ;

// 11 Aug 2004, stop if can not list remote files
// 15 Sept 2008 don't stop, root may not exist
    if result = TaskResFail then
    begin
        doCopyEvent (LogLevelInfo, fReqResponse) ;
        doCopyEvent (LogLevelInfo, 'Failed to List Files on FTP Server') ;
     //   if fRepl <> FCReplAlways then exit ;
    end ;
    fReqResponse := '' ;
    if fDispRDir then doCopyEvent (LogLevelInfo,
           'Target Files on FTP Server' + CRLF_ + FmtFileDirListW (TarFileList, false)) ;
    Application.ProcessMessages ;
    if fCancelFlag then exit ;

// compare source and target files, see what to copy
{    if fSpecificFiles then
        newfname := '*.*'
    else if fMask then
        newfname := GetMaskedName (fSrcFName, fPrev, fLocalHost)
    else
        newfname := fSrcFName ;  }
    fTotProcFiles := fMagFileCopy.SelectCopyFileList (SrcFileList, TarFileList,
                 newfname, fCopyType, fRepl, fDiffStampMins, true, fIgnoreFileExt,
                                                             fSkippedFiles, false) ;
    if fCancelFlag then exit ;

// see if only copying a list of specific files, deselect any we don't need
    if fSpecificFiles then
    begin
{$IFNDEF VER130} // D5
//        CopyOnlyList.Delimiter := '|' ;
//        CopyOnlyList.DelimitedText := IcsAnsiLowerCaseW (fSrcFName) ;
//        CopyOnlyList.Sort ;
//        CopyOnlyList.Sorted := true ; // 11 June 2008 not sure if really needed
{$ENDIF}
        CopyOnlyList := StrArraySplit (IcsAnsiLowerCaseW (fSrcFName), '|') ;   // not sorted!!
        CopyOnlyTot := Length (CopyOnlyList) ;
        if CopyOnlyTot = 0 then
        begin
            result := TaskResOKNone ;
            fReqResponse := 'No Specific Source Files Selected to Upload' ;
            exit ;
        end ;
        for I := 0 to Pred (TotSrcFiles) do
        begin
            SrcFileRec := SrcFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy = FCStateSelect then
                begin
//                    if NOT CopyOnlyList.Find (IcsAnsiLowerCaseW (FrFileName), J) then
                    if StrArrayPosOf (IcsAnsiLowerCaseW (FrFileName), CopyOnlyList) < 0 then
                    begin
                         FrFileCopy := FCStateNone ;
                         dec (fTotProcFiles) ;
                    end ;
                end ;
            end ;
        end ;
     {   if fTotProcFiles = 0 then  22 Feb 2008 may still need to delete some files
        begin
            result := TaskResOKNone ;
            fReqResponse := 'No Specific Source Files Selected to Upload' ;
            exit ;
        end ;   }
    end ;

// see if deleting old target files no longer in source directories
    if fDelOldTar and (TotTarFiles <> 0) then
    begin
        CurDelFiles := fMagFileCopy.SelectCopyFileList (TarFileList, SrcFileList, '*.*',
            FCTypeAllDir, FCReplNever, 0, false, fIgnoreFileExt, nodeltot, false) ;
    end ;
    if (fTotProcFiles = 0) and (CurDelFiles = 0) then  // 22 Feb 2008 chek if need to delete some files
    begin
        result := TaskResOKNone ;
        if fSkippedFiles <> 0 then
            fReqResponse := 'All Source Files Skipped Upload'
        else
            fReqResponse := 'No Source Files Selected to Upload' ;
        exit ;
    end ;
    Application.ProcessMessages ;
    if fCancelFlag then exit ;

// find size of stuff to copy
    delsize := 0 ;
    info := '' ;
    if fDelOldTar and (CurDelFiles <> 0) then
    begin
        info := info + CRLF_ + 'Old Files Selected for Deletion are: ' + CRLF_ ;
        for I := 0 to Pred (TotTarFiles) do
        begin
            SrcFileRec := TarFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy = FCStateSelect then
                begin
                    inc (delsize, FrFileBytes) ;
                    if CheckFiles then info := info + FrFullName +
                           ', Size ' + Int64ToCStr (FrFileBytes) + CRLF_ ;
                end ;
            end ;
        end ;
    end ;
    if fCancelFlag then exit ;
    newsize := 0 ;
    if (fTotProcFiles > 0) then
    begin
        info := info + CRLF_ + 'Files Selected for Uploading are: ' + CRLF_ ;
        for I := 0 to Pred (TotSrcFiles) do
        begin
            SrcFileRec := SrcFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy = FCStateSelect then
                begin
                    newsize := newsize + FrFileBytes ;
                    if CheckFiles then info := info + FrFullName +
                                ', Size ' + Int64ToCStr (FrFileBytes) + CRLF_ ;
                end ;
            end ;
        end ;
    end
    else
       info := info + CRLF_ + 'No Source Files Selected to Upload' + CRLF_ ;
    if CheckFiles then doCopyEvent (LogLevelInfo, info) ;
    doCopyEvent (LogLevelInfo, 'Source Files Skipped ' +
                                            IntToStr (fSkippedFiles)) ;
    info := 'Selected Total Files ' + IntToStr
                (fTotProcFiles) + ', Total Size ' + Int64ToCStr (newsize) ;
    if CurDelFiles <> 0 then info := info + CRLF_ + 'Old Files to Delete ' +
            IntToCStr (CurDelFiles) + ', Total Size ' + Int64ToCStr (delsize) ;
    doCopyEvent (LogLevelInfo, info) ;

// stop now if only checking what will be uploaded - test only
    if CheckFiles then
    begin
        result := TaskResOKNone ;
        exit ;
    end ;
    doCopyEvent (LogLevelInfo, 'Started FTP Upload to: ' + fHostName) ;
    Application.ProcessMessages ;
    if fCancelFlag then exit ;

// see if deleting old target files first
    if fDelOldTar and (CurDelFiles <> 0) then
    begin
        doCopyEvent (LogLevelInfo, 'Deleting Old Remote Files: ' + fTarDir) ;
        cursrcdir := '.,.,.' ;  // illegal
        for I := 0 to Pred (TotTarFiles) do
        begin
            Application.ProcessMessages ;
            if fCancelFlag then exit ;
            if NOT Connected then exit ;
            SrcFileRec := TarFileList [I] ;
            with SrcFileRec^ do
            begin
                if FrFileCopy <> FCStateSelect then continue ;

            // 14 Sept 2008 skip Unicode names with substitution characters
                if (Pos ('?', FrFullName) > 0) then
                begin
                    doCopyEvent (LogLevelDiag, 'Skipped Inaccessible Unicode Name: ' + FrFullName) ;
                    doCopyEvent (LogLevelDelimFile, '|' + FrFullName +
                                          '|0|0|0|Inaccessible Unicode File|0|0') ;
                    FrFileCopy := FCStateFailed ;
                    inc (fProcFailFiles) ;
                    continue ;
                end ;
                if FrSubDirs [1] = '/' then      // remove leading slash
                    tempdir := fServBaseDir + Copy (FrSubDirs, 2, 200)
                else
                    tempdir := fServBaseDir + FrSubDirs ;
                if cursrcdir <> tempdir then
                begin
                    cursrcdir := tempdir ;
                    HostDirName := cursrcdir ;
                    HostFileName := '' ;
                    LocalFileName := '' ;
                    ret := Cwd ;  // change working directory
                    if ret then ret := Pwd ;
                    if NOT ret then
                    begin
                        result := TaskResFail ;
                        fReqResponse := 'Can Not Change to Remote Directory' ;
                        continue ;
                    end ;
                    fCurRemDir := HostDirName ;
                end ;
                HostFileName := FrFileName ;
                doCopyEvent (LogLevelFile, 'Deleting: ' + FrFullName) ;
                ret := Dele ;   // delete remote file
                if ret then
                begin
                    doCopyEvent (LogLevelDelimFile, '|' + FrFullName +
                                            '|0|0|0|Old Target File Deleted|0|0') ;
                    inc (fDelOKBytes, FrFileBytes) ;
                    FrFileCopy := FCStateOK ;
                    inc (fDelOKFiles) ;
                end
                else
                begin
                    doCopyEvent (LogLevelInfo, 'FTP Delete Failed: ' +
                                         FrFullName + ' - ' +  LastResponse) ;
                    FrFileCopy := FCStateFailed ;
                    inc (fProcFailFiles) ;
                end ;
            end ;
        end ;

     // 22 Feb 2008 report now instead of after uploads, stop if no files to upload
        doCopyEvent (LogLevelInfo, 'Old target files deleted OK: ' +
            IntToCStr (fDelOKFiles) + ', Total bytes deleted ' + Int64ToCStr (fDelOKBytes)) ;
        if fTotProcFiles = 0 then
        begin
            if fDelOKFiles > 0 then
                result := TaskResOKNew
            else
                result := TaskResFail ;
            fReqResponse := 'Files Deleted, No Source Files Selected to Upload' ;
            exit ;
        end ;
    end ;

// start real FTP uploading
    cursrcdir := '.,.,.' ;  // illegal
    donenr := 0 ;
    totsize := 0 ;
    LocalStream := Nil ;  // upload from files, not stream
    starttick := GetTickCountX ;
    for I := 0 to Pred (TotSrcFiles) do
    begin
        Application.ProcessMessages ;
        if fCancelFlag then exit ;
        if NOT Connected then
        begin
            doCopyEvent (LogLevelInfo, 'Lost FTP Control Connection, Abandoning Uploads') ;
            exit ;
        end ;
        SrcFileRec := SrcFileList [I] ;
        with SrcFileRec^ do
        begin
            if FrFileCopy <> FCStateSelect then continue ;
            inc (donenr) ;
            fnamesrc := FrFullName ;
            tempdir := FrSubDirs ;  // for source file
            if cursrcdir <> tempdir then
            begin
                cursrcdir := tempdir ;
                if tempdir [1] = '\' then      // remove leading slash
                    newtardir := fServBaseDir + Copy (tempdir, 2, 200)
                else
                    newtardir := fServBaseDir + tempdir ;
                DosToUnixPathW (newtardir) ;
            end ;

        // 13 Nov 2008 skip Unicode file with non-ANSI characters unlesss UTF8 enabled
            if (FCodePage <> CP_UTF8) and (NOT CheckUnicodeToAnsi (fnamesrc)) then
            begin
                doCopyEvent (LogLevelInfo, 'Skipped Inaccessible Unicode Name: ' + fnamesrc) ;
                doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + FrFileName +
                                                      '|0|0|1|Inaccessible Unicode File|0|0') ;
                inc (fProcFailFiles) ;
                FrFileCopy := FCStateFailed ;
                continue ;
            end ;

        // 15 Sept 2008 ensure it's not been deleted already, and get current size in case it's changed
            doCopyEvent (LogLevelFile, 'Check Exists ' + fnamesrc) ;
            if NOT GetUAgeSizeFileW (fnamesrc, SrcFileUDT, SrcFSize) then
            begin
                fReqResponse := 'Can Not Find File: ' + fnamesrc ;
                doCopyEvent (LogLevelInfo, fReqResponse) ;
                doCopyEvent (LogLevelDelimFile, fnamesrc + '|' + FrFileName +
                                              '|0|0|1|Source File Not Found|0|0') ;
                inc (fProcFailFiles) ;
                FrFileCopy := FCStateFailed ;
                continue ;
            end ;

      // FTP upload file
            FrFileCopy := FCStateCopying ;
            retval := 0 ;
            for loop := 1 to (fFailRepeat + 1) do  // 31 Dec 2007
            begin
                fProgMessBase := 'Uploading File ' + IntToStr (donenr) +
                                                 ' of ' + IntToStr (fTotProcFiles) ;
                retval := IntUpOne (fnamesrc, newtardir, FrFileName, SrcFSize, SrcFileUDT) ;
                doCopyEvent (LogLevelProg, '') ;
                if retval = 0 then break ;
                if NOT Connected then break ;
                if fCancelFlag then exit ;
                if NOT (retval in [2, 3]) then break ;  // fail download or fail MD5
                if (StatusCode = 501) then break ;  // 19 Oct 2005 permissions or start failed
                if (loop < (fFailRepeat + 1)) then   // 31 Dec 2007
                    doCopyEvent (LogLevelInfo, 'Repeating Upload: ' + FrFullName) ;
            end ;
            if retval = 0 then
            begin
                inc (fProcOKFiles) ;
                inc (totsize, FrFileBytes) ;
                FrFileCopy := FCStateOK ;
            end
            else
            begin
                inc (fProcFailFiles) ;
                FrFileCopy := FCStateFailed ;
            end ;
        end ;
    end ;

// done
    if NOT fCancelFlag then
    begin
        I := ElapsedTicks (starttick) + 1 ;
        if (I > 100000) or (totsize > 1000000) then
        begin
            I := I div 1000 ;  // allow for bizarre divide by zero error
            if I > 0 then
                newsize := totsize div I
            else
                newsize := 0 ;
        end
        else
        begin
            newsize := (totsize * 1000) div I ;
            I := I div 1000 ;
        end ;
        result := TaskResOKNone ;
        if fProcFailFiles <> 0 then
            result := TaskResFail
        else
        begin
            if fProcOKFiles <> 0 then result := TaskResOKNew ;
        end ;
        doCopyEvent (LogLevelInfo, 'Finished, files uploaded OK: ' +
            IntToCStr (fProcOKFiles) + ', failed: ' + IntToStr
            (fProcFailFiles) + ', skipped: ' +
                                            IntToStr (fSkippedFiles)) ;
        doCopyEvent (LogLevelDelimTot, fSrcDir + '|' + fTarDir + '|' +
                IntToStr (totsize) + '|' + IntToStr (fProcOKFiles) +
                           '|' + IntToStr (fProcFailFiles) + '|Totals|') ;
        doCopyEvent (LogLevelInfo, 'Total bytes uploaded ' + Int64ToCStr (totsize) +
                ', duration ' + TimerToStr (SecsToTime (I)) +
                    ', average speed ' +  IntToCStr (newsize) + ' chars/sec') ;
     {   if fDelOKFiles <> 0 then doCopyEvent (LogLevelInfo,         22 Feb 2008 done earlier
            'Old target files deleted OK: ' + IntToCStr (fDelOKFiles) +
                        ', Total bytes deleted ' + Int64ToCStr (fDelOKBytes)) ;   }
    end ;
    finally
  //      CopyOnlyList.Free ;
        if fLoggedIn and (NOT Connected) then  // 20 Aug 2005
        begin
            result := TaskResFail ;
            fReqResponse := 'No Connection to Server, Cancelled Uploading Files (3)' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
        end
        else if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Uploading Files' ;
            doCopyEvent (LogLevelInfo, fReqResponse) ;
        end ;
        FtpLogoff ;

        doCopyEvent (LogLevelProg, '') ;
        fCancelFlag := false ;
   end ;
end;

procedure TMagFtpW.Cancel ;
begin
    doCopyEvent (LogLevelInfo, 'Cancel FTP Triggered') ;
    if Connected then
    begin
        if FState <> ftpAbort then AbortXferAsync ;  // 28 Dec 2007 don't repeatedly abort
        sysDelayX (200) ;  // 9 Dec 2007 - short delay before aborting xfer
    end;
    fCancelFlag := true ;
    fLastProgTick := GetTickCountX ; // force progress event  23 June 2006
end ;

end.

