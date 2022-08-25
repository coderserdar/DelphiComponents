{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Description:  This is a test and demo application for the TIcsHttpMultiW,
              TIcsFtpMultiW and TIcsFileCopyW multiple file transfer components.
              W version supports widestring/Unicode for Delphi 2007 only
              Note this sample is not designed to function on Unicode compabible
              compilers, use OverbyteIcsXferTst instead.
Creation:     Sept 2004
Updated:      Apr 2022
Version:      8.69
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2022 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.


This is a test and demo application for the TIcsHttpMultiW, TIcsFtpMultiW and
TIcsFileCopyW multiple file transfer components.

TIcsFileCopyW will copy a single file or directory, a list of directories or an
entire volume of files (250,000 or more). TIcsFileCopyW will similarly also
delete multiple files and directories.  It includes directory listing and
reporting functions.

TIcsFtpMultiW FTP uploads and downloads multiple files and directories,
mirroring the directory structures, optionally only transferring changed
files and deleting old files, including resumed transfers after failure.

TIcsHttpMultiW downloads multiple HTTP files, either as a URL list or by
parsing HTML pages and locating URLs to download.

Note this demo program does not make use of all the component's functionality,
but there are comments indicating where alternative properties may be set.

Requires TNT Unicode controls to display Unicode with Delphi 2007, last free
version from https://github.com/rofl0r/TntUnicode), undefine USE_TNT is not installed.


22 Sep 2004 - 1.2 - added single file FTP download and upload (faster since no directory listing)
22 Aug 2005 - 1.3 - various bug fixes
6  Sep 2005 - 1.4 - magcopy and magftp supports files larger than 2 gigs
                    testing SSL and fix for small FTP file uploads
3 Nov 2005  - 1.5 - bug fixes
6 Dec 2005  - 1.6 - testing new SSL and HTTP and FTP compression
16 Mar 2006 - 1.7 - added delete after xfer tick boxes (not supported), IcsLogger
11 Aug 2006 - 1.8 - allow FTP port to be specified (to test keep alive on strange ports)
                    allow KeepAlive seconds to be specified
4 Sep 2006  - 1.9 - 64-bit fixes
8 Jan 2007  - 2.0 - supporting ICS V6, FTP timeout now 10 mins
17 Aug 2007 - 2.1 - FTP fixes
3 Mar 2008  - 2.2 - support new ICS and TMagFtp features
                    added Clear logs button, several new FTP tick boxes, Bandwidth Limiting
7 Aug 2008  - 2.3 - updated for latest ICS V6 beta and V7 for Delphi 2009 and Unicode
                    replaced RX FormStorage with local INI file support
22 Sep 2008 - 3.0 - widestring version for UTF-8 on Delphi 2007 and earlier
                    FTP supports Unicode with UTF8 command, and HOST command
                    new label shows whether SSL compiler, and whether compiled with 2007 or 2009
-------------------------------------------------------------------------------------------------
22 Oct 2008 - 3.1 - now using OverbyteIcsFtpcliW, OverbyteIcsFtpSrvWT
18 Nov 2008 - 3.2 - renamed project to xferdemo3W, uses TMagX3W units
                    keep last 50 FTP host names and HTTP URLs
                    use the TNT Unicode controls to display Unicode with Delphi 2007.
07 Jan 2009 - 3.3 - updated OverbyteIcsFtpcliW with latest changes, added ftptest.org hosts
17 May 2009 - 3.4 - FTP fixes for md5, added MaskLocDir and MaskRemDir for FTP, Unicode MD5 and CRC32
22 May 2009 - 3.5 - new tick boxes for magftpNoMd5 and magftpNoCrc to allow them to be tested separately
                    subdir listing bug fix
7 Jun 2010  - 3.6 - fix a problem parsing UNIX file listing with strange upper case file attributes
11 Aug 2011 - 3.7 - ICS changes, new throttling, zlib 1.2.5, support MultiThreading
                    list and copy empty directories.  Directory functions have a ListDirs parameter,
                    while TIcsFileCopyW has a new EmptyDirs property, directory attributes should be copied
                    large FTP winsock buffers for increased performance
                    fixed bug listing sub-directories from root with MLSD command
                    new FTP thread functions using TIcsFtpMultiThreadW component which runs TIcsFtpMultiW in a thread
14 Oct 2011 - 3.8 - log time and speed of each download
                    added Replace tickbox for HTTP, multiple HTTP URLs allowed
                    large HTTP winsock buffer for increased performance
24 Aug 2012 - 4.0 - updated to support ICS V8 with IPv6
24 Jul 2013 - 4.1 - default to allowing IPv4 or IPv6 host names
                    added IgnorePaths, Wow64Disable
                    added extended file progress for customisable display with percent and time left
                    removed duplicate event handlers, all components use the same ones
13 Jul 2015 - 4.2 - better SSL handshake reporting
                    added SSL server certificate checking
                    added manifest so Windows 10 version displays correctly
23 Oct 2015 - 4.3 - report more ertificate information
                    failed certificate report error as last HTTP error
                    Warning, self signed certificates are not trusted
23 Feb 2016 - 4.4 - fixed a bug that always treated upload file names as lower case
24 Nov 2016 - 4.5 - more friendly errors
                    fixed an HTTP bug that caused an exception with ICS V8.37
                    force latest OpenSSL from our directory
                    only works with latest digitally signed OpenSSL DLLs
                    using OpenSSL certificate verification host checking
6 Mar 2017  - 4.6 - simplified SSL certificate reporting
                    set SSL security level low, ideally should be configurable
18 Jun 2018 - 4.7 - Fixed HTTP to HTTPS relocation failed unless HTTPS URL precessed
                       first, always set SslContext
                    Added SSL Security drop down box to set SslCliSecurity
                    Only support SSL
18 Mar 2019 - V8.60 Adapted for ICS
                    Added Socket Family selection
                    Added Log file
                    Use timer to update windows to avoid problems with performance
                    Replace tick boxes now combos with all replace options
3 Nov 2019 - V8.63  Added Fix Passive LAN IP Addr option where the FTP server is
                      behind a NAT router and is not configured to present the
                      external IP.
06 Oct 2020 - V8.65 Don't set TCP buffer size, leave it to Windows TCP Autotune.
                    Fixed failed Ssl session cached and reused, remove it from
                       cache if certificate checks fail.
                     Save more than one FTP Host Name.
04 Feb 2021 - V8.66 Added Parse HTML page options, not sure why these were missing
                      from this sample, been working for years.
21 Dec 2021 - V8.68 Log OpenSSL version on startup.
                    Improved error reporting for HTTP errors.
14 Apr 2022 - V8.69 Support OCSP to check certificate revocation when verifying
                      handshake using certificate bundle.  Note OCSP settings
                      made in code, not from the GUI.




pending - use VclZip v4 widestring version

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit OverbyteIcsXferTstW1;

{$I Include\OverbyteIcsDefs.inc}

interface

{$IFNDEF UNICODE}
    {$DEFINE USE_TNT}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ComCtrls, StdCtrls, ExtCtrls,
  {$IFDEF USE_TNT} TntStdCtrls, {$ENDIF}
  OverbyteIcsTypes,
  OverbyteIcsWsocket,
  OverbyteIcsFtpcliW,
  OverbyteIcsFtpSrvWT,
  OverbyteIcsHttpProt,
  OverbyteIcsHttpCCodzlib,
  OverbyteIcsSSLEAY,
  OverbyteIcsLIBEAY,
  OverbyteIcsLogger,
  OverbyteIcsIniFiles,
  OverbyteIcsUtils,
  OverbyteIcsBlacklist,
  OverbyteIcsFileCopyW,
  OverbyteIcsFtpMultiW,
  OverbyteIcsHttpMultiW;

type
{$IFDEF USE_TNT}
    TCurrentMemo = TTntMemo;   // unicode control
{$ELSE}
    TCurrentMemo = TMemo;
{$ENDIF}

  TFormXferDemo = class(TForm)
// saved components
    CopyEmptyDirs: TCheckBox;
    CopyFileRepl: TComboBox;
    CopyIgnorePath: TEdit;
    CopySrcDir: TEdit;
    CopySrcFile: TEdit;
    CopySubdirs: TCheckBox;
    CopyTarDir: TEdit;
    CopyWow64Disable: TCheckBox;
    DirLogs: TEdit;
    Ftp1LocDir: TEdit;
    Ftp1Path: TEdit;
    Ftp1SrcName: TEdit;
    Ftp1UpFile: TEdit;
    FtpBandWidth: TEdit;
    FtpCopyAllDir: TCheckBox;
    FtpDelDone: TCheckBox;
    FtpDelOldTar: TCheckBox;
    FtpEmptyDirs: TCheckBox;
    FtpHost: TComboBox;
    FtpIgnorePath: TEdit;
    FtpIgnoreUtf8: TCheckBox;
    FtpKeepAlive: TEdit;
    FtpLocDir: TEdit;
    FtpMultiFileRepl: TComboBox;
    FtpNoCrc: TCheckBox;
    FtpNoFeatCmd: TCheckBox;
    FtpNoHost: TCheckBox;
    FtpNoMd5: TCheckBox;
    FtpNoTmpFile: TCheckBox;
    FtpNoUtf8: TCheckBox;
    FtpNoZlib: TCheckBox;
    FtpOneDelDone: TCheckBox;
    FtpOneFileRepl: TComboBox;
    FtpPassive: TCheckBox;
    FtpPassword: TEdit;
    FtpPath: TEdit;
    FtpPort: TEdit;
    FtpPortSsl: TEdit;
    FtpServerType: TComboBox;
    FtpSrcFile: TEdit;
    FtpSubdirs: TCheckBox;
    FtpUsername: TEdit;
    HttpBandWidth: TEdit;
    HttpFileRepl: TComboBox;
    HttpSrcDir: TMemo;
    HttpSrcFile: TComboBox;
    HttpTarDir: TEdit;
    NetLogon: TEdit;
    NetPassword: TEdit;
    ReportChain: TCheckBox;
    RevokeCheck: TCheckBox;
    ShowDiagsHigh: TCheckBox;
    ShowDiagsLow: TCheckBox;
    ShowDiagsSSL: TCheckBox;
    ShowDiagsUtf8: TCheckBox;
    ShowXProgesss: TCheckBox;
    SslSecurity: TComboBox;
    VerifyCertMode: TRadioGroup;
    XferSockFamily: TRadioGroup;
    FtpFixPassiveLanIP: TCheckBox;   { V8.63 }
    HttpParsePage: TCheckBox;       // V8.66
    HttpParseLevels: TEdit;         // V8.66

// non-saved
    TabSheet5: TTabSheet;
    Label13: TLabel;
    Label8: TLabel;
    LogText: TMemo;
    Label1: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    doCopyCheck: TButton;
    doCopyList: TButton;
    Label2: TLabel;
    Label3: TLabel;
    doCopyFiles: TButton;
    Label4: TLabel;
    doCopyAbort: TButton;
    LabelProgress: TLabel;
    LogDelim: TMemo;
    doExit: TButton;
    doDeleteCheck: TButton;
    doDeleteFiles: TButton;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    doHttpDownCheck: TButton;
    doHttpDownFiles: TButton;
    doHttpAbort: TButton;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    TabSheet4: TTabSheet;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    doFtpDown1: TButton;
    doFtpUp1: TButton;
    Label18: TLabel;
    doFtpDownCheck: TButton;
    doFtpDownFiles: TButton;
    doFtpAbort: TButton;
    doFtpUpCheck: TButton;
    doFtpUpFiles: TButton;
    doFtpList: TButton;
    Label12: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    doFtpAbort1: TButton;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    doClear: TButton;
    Label25: TLabel;
    LabelSslState: TLabel;
    Label26: TLabel;
    doFtpUpThread: TButton;
    doFtpListThread: TButton;
    doFtpDownThread: TButton;
    Label14: TLabel;
    Label28: TLabel;
    Label27: TLabel;
    lbl1: TLabel;
    Label29: TLabel;
    TimerUpdates: TTimer;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    procedure doCopyListClick(Sender: TObject);
    procedure CopyFiles(Sender: TObject);
    procedure doAbortClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure DeleteFiles(Sender: TObject);
    procedure HttpDownload(Sender: TObject);
    procedure FtpDownload(Sender: TObject);
    procedure doFtpListClick(Sender: TObject);
    procedure FtpUpload(Sender: TObject);
    procedure doFtpDown1Click(Sender: TObject);
    procedure doFtpUp1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure doClearClick(Sender: TObject);
    procedure doFtpListThreadClick(Sender: TObject);
    procedure doFtpDownThreadClick(Sender: TObject);
    procedure doFtpUpThreadClick(Sender: TObject);
    procedure TimerUpdatesTimer(Sender: TObject);
    procedure doFtpAbortClick(Sender: TObject);
  private
    { Private declarations }
    procedure AddLogText (S: Unicodestring) ;
    procedure AddLogDelim (S: Unicodestring) ;
    function SetFTPGen: boolean ;
    procedure SetFtpButtons (value: boolean) ;
    procedure onCopyEvent (LogLevel: TIcsCopyLogLevel ; Info: UnicodeString ;
                                                  var Cancel: boolean) ;
    procedure onProgressEvent (Sender: TObject ; CopyProgress: TIcsCopyProgressW ;
                                                  var Cancel: boolean) ;
    procedure IcsLogEvent (Sender: TObject; LogOption: TLogOption;
                              const Msg : String) ;
    procedure GetSettings ;
    procedure PutSettings ;
    function SetFTPThreadGen: integer ;
    procedure onThreadEventW (LogLevel: TIcsCopyLogLevel ; const Id, Info: UnicodeString ;
                                                          var Cancel: boolean) ;
    procedure OnThreadTerminate (Sender: TObject) ;
    procedure OpenLogFile;
  public
    { Public declarations }
  end;

const
    MaxThreads = 20 ;

var
    FormXferDemo: TFormXferDemo;
    HttpMultiClient: TIcsHttpMultiW ;
    FtpMultiClient: TIcsFtpMultiW ;
    FileCopyClient: TIcsFileCopyW ;
    IcsLog: TIcsLogger ;
    AbortFlag: boolean ;
    IniFileName: string ;
    CurrentLogText: TCurrentMemo;
    CurrentLogDelim: TCurrentMemo;
    IcsFtpThreads: array [0..MaxThreads] of TIcsFtpMultiThreadW ;
    CurThreads, NextThread: integer ;
    BuffDiags: UnicodeString ;
    FIcsBuffLogStream: TIcsBuffLogStream;

implementation

{$R *.dfm}

procedure TFormXferDemo.GetSettings ;
var
    IniFile: TIcsIniFile ;
    section: string ;
begin
    if NOT FileExists (IniFileName) then exit ;
    try
        IniFile := TIcsIniFile.Create (IniFileName) ;
    except
        exit ;
    end ;
    try // finally
        section := 'demomain' ;
        with IniFile do
        begin
// following lines created by DelphiVar tool
  if ReadString (section, 'CopyEmptyDirs_Checked', 'False') = 'True' then CopyEmptyDirs.Checked := true else CopyEmptyDirs.Checked := false ;
  CopyFileRepl.ItemIndex := ReadInteger (section, 'CopyFileRepl_ItemIndex', 0) ;
  CopyIgnorePath.Text := ReadString (section, 'CopyIgnorePath_Text', '') ;
  CopySrcDir.Text := ReadString (section, 'CopySrcDir_Text', '') ;
  CopySrcFile.Text := ReadString (section, 'CopySrcFile_Text', '') ;
  if ReadString (section, 'CopySubdirs_Checked', 'False') = 'True' then CopySubdirs.Checked := true else CopySubdirs.Checked := false ;
  CopyTarDir.Text := ReadString (section, 'CopyTarDir_Text', '') ;
  if ReadString (section, 'CopyWow64Disable_Checked', 'False') = 'True' then CopyWow64Disable.Checked := true else CopyWow64Disable.Checked := false ;
  DirLogs.Text := ReadString (section, 'DirLogs_Text', '') ;
  Ftp1LocDir.Text := ReadString (section, 'Ftp1LocDir_Text', '') ;
  Ftp1Path.Text := ReadString (section, 'Ftp1Path_Text', '') ;
  Ftp1SrcName.Text := ReadString (section, 'Ftp1SrcName_Text', '') ;
  Ftp1UpFile.Text := ReadString (section, 'Ftp1UpFile_Text', '') ;
  FtpBandWidth.Text := ReadString (section, 'FtpBandWidth_Text', '') ;
  if ReadString (section, 'FtpCopyAllDir_Checked', 'False') = 'True' then FtpCopyAllDir.Checked := true else FtpCopyAllDir.Checked := false ;
  if ReadString (section, 'FtpDelDone_Checked', 'False') = 'True' then FtpDelDone.Checked := true else FtpDelDone.Checked := false ;
  if ReadString (section, 'FtpDelOldTar_Checked', 'False') = 'True' then FtpDelOldTar.Checked := true else FtpDelOldTar.Checked := false ;
  if ReadString (section, 'FtpEmptyDirs_Checked', 'False') = 'True' then FtpEmptyDirs.Checked := true else FtpEmptyDirs.Checked := false ;
  FtpHost.Text := ReadString (section, 'FtpHost_Text', '') ;
  FtpIgnorePath.Text := ReadString (section, 'FtpIgnorePath_Text', '') ;
  if ReadString (section, 'FtpIgnoreUtf8_Checked', 'False') = 'True' then FtpIgnoreUtf8.Checked := true else FtpIgnoreUtf8.Checked := false ;
  FtpKeepAlive.Text := ReadString (section, 'FtpKeepAlive_Text', '') ;
  FtpLocDir.Text := ReadString (section, 'FtpLocDir_Text', '') ;
  FtpMultiFileRepl.ItemIndex := ReadInteger (section, 'FtpMultiFileRepl_ItemIndex', 0) ;
  if ReadString (section, 'FtpNoCrc_Checked', 'False') = 'True' then FtpNoCrc.Checked := true else FtpNoCrc.Checked := false ;
  if ReadString (section, 'FtpNoFeatCmd_Checked', 'False') = 'True' then FtpNoFeatCmd.Checked := true else FtpNoFeatCmd.Checked := false ;
  if ReadString (section, 'FtpNoHost_Checked', 'False') = 'True' then FtpNoHost.Checked := true else FtpNoHost.Checked := false ;
  if ReadString (section, 'FtpNoMd5_Checked', 'False') = 'True' then FtpNoMd5.Checked := true else FtpNoMd5.Checked := false ;
  if ReadString (section, 'FtpNoTmpFile_Checked', 'False') = 'True' then FtpNoTmpFile.Checked := true else FtpNoTmpFile.Checked := false ;
  if ReadString (section, 'FtpNoUtf8_Checked', 'False') = 'True' then FtpNoUtf8.Checked := true else FtpNoUtf8.Checked := false ;
  if ReadString (section, 'FtpNoZlib_Checked', 'False') = 'True' then FtpNoZlib.Checked := true else FtpNoZlib.Checked := false ;
  if ReadString (section, 'FtpOneDelDone_Checked', 'False') = 'True' then FtpOneDelDone.Checked := true else FtpOneDelDone.Checked := false ;
  FtpOneFileRepl.ItemIndex := ReadInteger (section, 'FtpOneFileRepl_ItemIndex', 0) ;
  if ReadString (section, 'FtpPassive_Checked', 'False') = 'True' then FtpPassive.Checked := true else FtpPassive.Checked := false ;
  FtpPassword.Text := ReadString (section, 'FtpPassword_Text', '') ;
  FtpPath.Text := ReadString (section, 'FtpPath_Text', '') ;
  FtpPort.Text := ReadString (section, 'FtpPort_Text', '') ;
  FtpPortSsl.Text := ReadString (section, 'FtpPortSsl_Text', '') ;
  FtpServerType.ItemIndex := ReadInteger (section, 'FtpServerType_ItemIndex', 0) ;
  FtpSrcFile.Text := ReadString (section, 'FtpSrcFile_Text', '') ;
  if ReadString (section, 'FtpSubdirs_Checked', 'False') = 'True' then FtpSubdirs.Checked := true else FtpSubdirs.Checked := false ;
  FtpUsername.Text := ReadString (section, 'FtpUsername_Text', '') ;
  HttpBandWidth.Text := ReadString (section, 'HttpBandWidth_Text', '') ;
  HttpFileRepl.ItemIndex := ReadInteger (section, 'HttpFileRepl_ItemIndex', 0) ;
  HttpSrcDir.Lines.CommaText := ReadString (section, 'HttpSrcDir_Lines', '') ;
  HttpSrcFile.Text := ReadString (section, 'HttpSrcFile_Text', '') ;
  HttpTarDir.Text := ReadString (section, 'HttpTarDir_Text', '') ;
  NetLogon.Text := ReadString (section, 'NetLogon_Text', '') ;
  NetPassword.Text := ReadString (section, 'NetPassword_Text', '') ;
  if ReadString (section, 'ReportChain_Checked', 'False') = 'True' then ReportChain.Checked := true else ReportChain.Checked := false ;
  if ReadString (section, 'RevokeCheck_Checked', 'False') = 'True' then RevokeCheck.Checked := true else RevokeCheck.Checked := false ;
  if ReadString (section, 'ShowDiagsHigh_Checked', 'False') = 'True' then ShowDiagsHigh.Checked := true else ShowDiagsHigh.Checked := false ;
  if ReadString (section, 'ShowDiagsLow_Checked', 'False') = 'True' then ShowDiagsLow.Checked := true else ShowDiagsLow.Checked := false ;
  if ReadString (section, 'ShowDiagsSSL_Checked', 'False') = 'True' then ShowDiagsSSL.Checked := true else ShowDiagsSSL.Checked := false ;
  if ReadString (section, 'ShowDiagsUtf8_Checked', 'False') = 'True' then ShowDiagsUtf8.Checked := true else ShowDiagsUtf8.Checked := false ;
  if ReadString (section, 'ShowXProgesss_Checked', 'False') = 'True' then ShowXProgesss.Checked := true else ShowXProgesss.Checked := false ;
  SslSecurity.ItemIndex := ReadInteger (section, 'SslSecurity_ItemIndex', 0) ;
  VerifyCertMode.ItemIndex := ReadInteger (section, 'VerifyCertMode_ItemIndex', 0) ;
  XferSockFamily.ItemIndex := ReadInteger (section, 'XferSockFamily_ItemIndex', 0) ;
  if ReadString (section, 'FtpFixPassiveLanIP_Checked', 'False') = 'True' then FtpFixPassiveLanIP.Checked := true else FtpFixPassiveLanIP.Checked := false ;
  FtpHost.Items.CommaText := ReadString (section, 'FtpHost_Items', FtpHost.Items.CommaText) ;                                                  { V8.65 }
  if ReadString (section, 'HttpParsePage_Checked', 'False') = 'True' then HttpParsePage.Checked := true else HttpParsePage.Checked := false ;  { V8.66 }
  HttpParseLevels.Text := ReadString (section, 'HttpParseLevels_Text', '0') ;                                                                  { V8.66 }
    end ;
    finally
        IniFile.Free ;
    end ;
   if SslSecurity.ItemIndex <= 0 then SslSecurity.ItemIndex := Ord(sslCliSecDefault);   // June 2018
   if FtpServerType.ItemIndex < 0 then FtpServerType.ItemIndex := 0;
end ;

procedure TFormXferDemo.PutSettings ;
var
    IniFile: TIcsIniFile ;
    section, temp: string ;
begin
    try
        IniFile := TIcsIniFile.Create (IniFileName) ;
    except
        exit ;
    end ;
    try // finally
        section := 'demomain' ;
        with IniFile do
        begin
// following lines created by DelphiVar tool
  if CopyEmptyDirs.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'CopyEmptyDirs_Checked', temp) ;
  WriteInteger (section, 'CopyFileRepl_ItemIndex', CopyFileRepl.ItemIndex) ;
  WriteString (section, 'CopyIgnorePath_Text', CopyIgnorePath.Text) ;
  WriteString (section, 'CopySrcDir_Text', CopySrcDir.Text) ;
  WriteString (section, 'CopySrcFile_Text', CopySrcFile.Text) ;
  if CopySubdirs.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'CopySubdirs_Checked', temp) ;
  WriteString (section, 'CopyTarDir_Text', CopyTarDir.Text) ;
  if CopyWow64Disable.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'CopyWow64Disable_Checked', temp) ;
  WriteString (section, 'DirLogs_Text', DirLogs.Text) ;
  WriteString (section, 'Ftp1LocDir_Text', Ftp1LocDir.Text) ;
  WriteString (section, 'Ftp1Path_Text', Ftp1Path.Text) ;
  WriteString (section, 'Ftp1SrcName_Text', Ftp1SrcName.Text) ;
  WriteString (section, 'Ftp1UpFile_Text', Ftp1UpFile.Text) ;
  WriteString (section, 'FtpBandWidth_Text', FtpBandWidth.Text) ;
  if FtpCopyAllDir.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpCopyAllDir_Checked', temp) ;
  if FtpDelDone.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpDelDone_Checked', temp) ;
  if FtpDelOldTar.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpDelOldTar_Checked', temp) ;
  if FtpEmptyDirs.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpEmptyDirs_Checked', temp) ;
  WriteString (section, 'FtpHost_Text', FtpHost.Text) ;
  WriteString (section, 'FtpIgnorePath_Text', FtpIgnorePath.Text) ;
  if FtpIgnoreUtf8.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpIgnoreUtf8_Checked', temp) ;
  WriteString (section, 'FtpKeepAlive_Text', FtpKeepAlive.Text) ;
  WriteString (section, 'FtpLocDir_Text', FtpLocDir.Text) ;
  WriteInteger (section, 'FtpMultiFileRepl_ItemIndex', FtpMultiFileRepl.ItemIndex) ;
  if FtpNoCrc.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpNoCrc_Checked', temp) ;
  if FtpNoFeatCmd.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpNoFeatCmd_Checked', temp) ;
  if FtpNoHost.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpNoHost_Checked', temp) ;
  if FtpNoMd5.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpNoMd5_Checked', temp) ;
  if FtpNoTmpFile.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpNoTmpFile_Checked', temp) ;
  if FtpNoUtf8.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpNoUtf8_Checked', temp) ;
  if FtpNoZlib.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpNoZlib_Checked', temp) ;
  if FtpOneDelDone.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpOneDelDone_Checked', temp) ;
  WriteInteger (section, 'FtpOneFileRepl_ItemIndex', FtpOneFileRepl.ItemIndex) ;
  if FtpPassive.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpPassive_Checked', temp) ;
  WriteString (section, 'FtpPassword_Text', FtpPassword.Text) ;
  WriteString (section, 'FtpPath_Text', FtpPath.Text) ;
  WriteString (section, 'FtpPort_Text', FtpPort.Text) ;
  WriteString (section, 'FtpPortSsl_Text', FtpPortSsl.Text) ;
  WriteInteger (section, 'FtpServerType_ItemIndex', FtpServerType.ItemIndex) ;
  WriteString (section, 'FtpSrcFile_Text', FtpSrcFile.Text) ;
  if FtpSubdirs.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpSubdirs_Checked', temp) ;
  WriteString (section, 'FtpUsername_Text', FtpUsername.Text) ;
  WriteString (section, 'HttpBandWidth_Text', HttpBandWidth.Text) ;
  WriteInteger (section, 'HttpFileRepl_ItemIndex', HttpFileRepl.ItemIndex) ;
  WriteString (section, 'HttpSrcDir_Lines', HttpSrcDir.Lines.CommaText) ;
  WriteString (section, 'HttpSrcFile_Text', HttpSrcFile.Text) ;
  WriteString (section, 'HttpTarDir_Text', HttpTarDir.Text) ;
  WriteString (section, 'NetLogon_Text', NetLogon.Text) ;
  WriteString (section, 'NetPassword_Text', NetPassword.Text) ;
  if ReportChain.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ReportChain_Checked', temp) ;
  if RevokeCheck.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'RevokeCheck_Checked', temp) ;
  if ShowDiagsHigh.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ShowDiagsHigh_Checked', temp) ;
  if ShowDiagsLow.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ShowDiagsLow_Checked', temp) ;
  if ShowDiagsSSL.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ShowDiagsSSL_Checked', temp) ;
  if ShowDiagsUtf8.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ShowDiagsUtf8_Checked', temp) ;
  if ShowXProgesss.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ShowXProgesss_Checked', temp) ;
  WriteInteger (section, 'SslSecurity_ItemIndex', SslSecurity.ItemIndex) ;
  WriteInteger (section, 'VerifyCertMode_ItemIndex', VerifyCertMode.ItemIndex) ;
  WriteInteger (section, 'XferSockFamily_ItemIndex', XferSockFamily.ItemIndex) ;
  if FtpFixPassiveLanIP.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpFixPassiveLanIP_Checked', temp) ;
  WriteString (section, 'FtpHost_Items', FtpHost.Items.CommaText) ;                                                            { V8.65 }
  if HttpParsePage.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'HttpParsePage_Checked', temp) ;   { V8.66 }
  WriteString (section, 'HttpParseLevels_Text', HttpParseLevels.Text) ;                                                        { V8.66 }
        end ;
    finally
        IniFile.UpdateFile ;
        IniFile.Free ;
    end ;

end ;

{ this event is used to open the log file, note log ls written as UTF8 codepage }

procedure TFormXferDemo.OpenLogFile;
var
    FName: String;
begin
    if DirLogs.Text = '' then Exit; // no log
    FName := '"' + IncludeTrailingPathDelimiter(DirLogs.Text) +
                                              'ics-xfertst-"yyyy-mm-dd".log"';
    if NOT Assigned(FIcsBuffLogStream) then
        FIcsBuffLogStream := TIcsBuffLogStream.Create(self, FName,
                                     FormXferDemo.Caption + IcsCRLF, FileCPUtf8)
    else begin
        if FName = FIcsBuffLogStream.NameMask then Exit; // skip no change
        if FIcsBuffLogStream.LogSize > 0 then
            FIcsBuffLogStream.FlushFile(True);  // changing log path, write old log first
        FIcsBuffLogStream.NameMask := FName;
    end;
    AddLogText(IcsCRLF + 'Opened log file: ' + FIcsBuffLogStream.FullName);
end;

procedure TFormXferDemo.AddLogText (S: Unicodestring) ;
begin
    if Application.Terminated then exit ;
    if Length(S) < 200 then
        S := FormatDateTime (ISOLongTimeMask, Now) + IcsSpace + S ;
    BuffDiags := BuffDiags + S + IcsCRLF ;

  { write log file }
    try
        if (DirLogs.Text = '') then Exit ;
        if NOT Assigned(FIcsBuffLogStream) then Exit; // sanity check
        FIcsBuffLogStream.WriteLine(S);
    except
        CurrentLogDelim.Lines.Add ('Exception Writing Log') ;
    end;
//    CurrentLogText.Lines.Add (S) ;   // general unformatted comments about xfers
end ;

procedure TFormXferDemo.AddLogDelim (S: Unicodestring) ;
begin
    CurrentLogDelim.Lines.Add (S) ;   // delimited formatted xfer information
end ;

procedure TFormXferDemo.TimerUpdatesTimer(Sender: TObject);
var
    displen: integer ;
begin
    displen := Length (BuffDiags) ;
    if displen > 0 then
    begin
        try
            SetLength (BuffDiags, displen - 2) ;  // remove CRLF
            CurrentLogText.Lines.Add (BuffDiags) ;
            SendMessage(LogText.Handle, WM_VSCROLL, SB_BOTTOM, 0);
        except
            LabelProgress.Caption := 'Error writing to diag log' ;
        end ;
        BuffDiags := '' ;
    end;
end;

procedure TFormXferDemo.doAbortClick(Sender: TObject);
begin
    AbortFlag := true ;
end;

procedure TFormXferDemo.doExitClick(Sender: TObject);
begin
    AbortFlag := true ;
    Close ;
end;

procedure TFormXferDemo.FormCreate(Sender: TObject);
var
    S: string ;
    I: TFtpType ;
    Level: TSslCliSecurity;
begin
    IcsLog := TIcsLogger.Create (self) ;
    IcsLog.OnIcsLogEvent := IcsLogEvent ;
    FtpServerType.Items.Clear ;
    for I := Low(TFtpType) to High(TFtpType) do
                 FtpServerType.Items.Add (FtpTypeStrings [I]) ;
    FtpServerType.ItemIndex := 0;
    IniFileName := GetIcsIniFileName;

    SslSecurity.Items.Clear;  // June 2018 update SSL client security levels
    for Level := Low(TSslCliSecurity) to High(TSslCliSecurity) do
         SslSecurity.Items.Add (SslCliSecurityNames[Level]);

    GetSettings ;  // June 2018 after SslSecurity load

    {$IFDEF UNICODE}
    S := 'SSL Enabled, Unicode GUI' ;
    {$ELSE}
    S := 'SSL Enabled, ASCII GUI' ;
    {$ENDIF}
{$IFDEF USE_TNT}
    LogText.Visible := false ;
    CurrentLogText := TTntMemo.Create(self);
    CurrentLogText.ParentFont  := LogText.ParentFont ;
    CurrentLogText.Parent      := LogText.Parent ;
    CurrentLogText.Align       := LogText.Align ;
    CurrentLogText.ScrollBars  := LogText.ScrollBars ;
    CurrentLogText.WordWrap    := LogText.WordWrap ;
    CurrentLogText.Font        := LogText.Font ;
    CurrentLogText.Left        := LogText.Left ;
    CurrentLogText.Top         := LogText.Top ;
    CurrentLogText.Width       := LogText.Width ;
    CurrentLogText.Height      := LogText.Height ;
    LogDelim.Visible := false ;
    CurrentLogDelim := TTntMemo.Create(self);
    CurrentLogDelim.ParentFont  := LogDelim.ParentFont ;
    CurrentLogDelim.Parent      := LogDelim.Parent ;
    CurrentLogDelim.Align       := LogDelim.Align ;
    CurrentLogDelim.ScrollBars  := LogDelim.ScrollBars ;
    CurrentLogDelim.WordWrap    := LogDelim.WordWrap ;
    CurrentLogDelim.Font        := LogDelim.Font ;
    CurrentLogDelim.Left        := LogDelim.Left ;
    CurrentLogDelim.Top         := LogDelim.Top ;
    CurrentLogDelim.Width       := LogDelim.Width ;
    CurrentLogDelim.Height      := LogDelim.Height ;
    S := S + IcsCRLF + 'Unicode Display' ;
{$ELSE}
    CurrentLogText := LogText ;
    CurrentLogDelim := LogDelim ;
{$ENDIF}
    LabelSslState.Caption := S ;
    AddLogText ('INI File: ' + IniFileName) ;

{ load OpenSSL, then display OpenSSL DLL name and version  }
//  GSSLEAY_DLL_IgnoreNew := True;     { ignore OpenSSL 3.0 and later }
//  GSSLEAY_DLL_IgnoreOld := True;     { ignore OpenSSL 1.1 }
// note both not allowed true
    GSSL_DLL_DIR := ExtractFilePath(ParamStr(0));  { V8.68 only from our directory }
    GSSL_SignTest_Check := True;       { V8.68 check digitally signed }
    GSSL_SignTest_Certificate := True; { V8.68 check digital certificate }
    GSSLEAY_LOAD_LEGACY := False;      { V8.68 OpenSSL 3.0 legacy provider for old algorithms }
    LoadSsl;                           { V8.66 need version number }
    if NOT GSSLStaticLinked  then begin
        if NOT FileExists (GLIBEAY_DLL_FileName) then
            AddLogText('SSL/TLS DLL not found: ' + GLIBEAY_DLL_FileName)
        else
            AddLogText('SSL/TLS DLL: ' + GLIBEAY_DLL_FileName + ', Version: ' + OpenSslVersion);
    end
    else
        AddLogText('SSL/TLS Static Linked, Version: ' + OpenSslVersion);    { V8.66 }
end;

procedure TFormXferDemo.FormDestroy(Sender: TObject);
begin
    PutSettings ;
    IcsLog.Free ;
end;

procedure TFormXferDemo.IcsLogEvent (Sender: TObject; LogOption: TLogOption;
                                       const Msg : String) ;
begin
    AddLogText (Msg) ;
end ;

procedure TFormXferDemo.onCopyEvent (LogLevel: TIcsCopyLogLevel ; Info: UnicodeString ;
                                                  var Cancel: boolean) ;
begin
    if (LogLevel = LogLevelInfo) or (LogLevel = LogLevelFile) then
    begin
        AddLogText (Info) ;
        LabelProgress.Caption := Info ;
    end ;
    if (LogLevel = LogLevelProg) then
    begin
        if Info <> '' then
            LabelProgress.Caption := 'Progress: ' + Info
        else
            LabelProgress.Caption := '' ;
    end ;
    if (LogLevel = LogLevelDiag) and (ShowDiagsLow.Checked or
                             ShowDiagsHigh.Checked) then AddLogText (Info) ;
    if (LogLevel = LogLevelDelimFile) then AddLogDelim (Info) ;
    if (LogLevel = LogLevelDelimTot) then AddLogDelim (Info) ;
    if AbortFlag then Cancel := true ;
end ;

procedure TFormXferDemo.onProgressEvent (Sender: TObject ; CopyProgress: TIcsCopyProgressW ;
                                                  var Cancel: boolean) ;
var
    S: string ;
begin
    with CopyProgress do
    begin
        if (LogLevel = LogLevelInfo) then
        begin
            AddLogText (Info) ;
            LabelProgress.Caption := Info ;
        end ;
        if (LogLevel = LogLevelFile) then
        begin
            AddLogText (Info) ;
        end ;
        if (LogLevel = LogLevelProg) then
        begin
            S := Info ;
            if (CurFileBytes > 0) then
            begin
                S := 'Copying File ' + CurSrcName + ' to ' + CurTarName + ', Size ' +
                                    InttoKByte (CurFileBytes) + ', Done ' + IntToStr (CurDonePercent) + '%' ;
                if (CurEstimateTicks > 0) then S := S + ', time left ' +
                                     IcsSecsToStr ((CurEstimateTicks - CurDoneTicks) div 1000) ;
            end ;
            if (TotProcBytes > 0) then
            begin
                S := S  + IcsCRLF + 'Totals: Copying '  + IcsInttoCStr (TotDoneNr) + ' of ' + IcsInttoCStr (TotProcFiles) +
                         ', Total Size ' + InttoKByte (TotProcBytes) + ', Done ' + IntToStr (SessDonePercent) + '%' ;
                if (SessEstimateTicks > 0) then S := S +
                    ', time left ' + IcsSecsToStr ((SessEstimateTicks - SessDoneTicks) div 1000) +
                                                        ', average speed ' + IntToKByte (SessAvSpeed) + '/sec' ;
            end;
            LabelProgress.Caption := S ;
        end ;
        if (LogLevel = LogLevelDiag) and (ShowDiagsLow.Checked or
                                 ShowDiagsHigh.Checked) then AddLogText (Info) ;
        if (LogLevel = LogLevelDelimFile) then AddLogDelim (Info) ;
        if (LogLevel = LogLevelDelimTot) then AddLogDelim (Info) ;
        if AbortFlag then Cancel := true ;
    end;
end ;

procedure TFormXferDemo.doCopyListClick(Sender: TObject);
begin
    OpenLogFile;
    AddLogText (DateToStr (Now)) ;
    AbortFlag := false ;
    FileCopyClient := TIcsFileCopyW.Create (self) ;
    doCopyFiles.Enabled := false ;
    doCopyCheck.Enabled := false ;
    doCopyList.Enabled := false ;
    doCopyAbort.Enabled := true ;
    try
        FileCopyClient.CopyEvent := onCopyEvent ;
        AddLogText (IcsCRLF + FileCopyClient.DispLocFiles (CopySrcDir.Text,
               CopySrcFile.Text, FCTypeMaskDir, CopySubdirs.Checked, false, CopyEmptyDirs.Checked)) ;
    finally
        FreeAndNil (FileCopyClient) ;
        LabelProgress.Caption := 'List Completed' ;
        doCopyFiles.Enabled := true ;
        doCopyCheck.Enabled := true ;
        doCopyList.Enabled := true ;
        doCopyAbort.Enabled := false ;
    end ;
end;

procedure TFormXferDemo.CopyFiles(Sender: TObject);
var
    taskres: TIcsTaskResult ;
    checkflag: boolean ;
begin
    OpenLogFile;
    AddLogText (DateToStr (Now)) ;
    AbortFlag := false ;
    FileCopyClient := TIcsFileCopyW.Create (self) ;
    doCopyFiles.Enabled := false ;
    doCopyCheck.Enabled := false ;
    doCopyList.Enabled := false ;
    doCopyAbort.Enabled := true ;
    try
        checkflag := false ;
        if Sender = doCopyCheck then checkflag := true ;
        try
            with FileCopyClient do
            begin
                MultiDir := false ; // true copy multiple specific directorie
           //  multi directories are passed as srcdir+tab+tardir+recsep (repeat)
           //   MultiDirList :=
           // or as a pair of TStringLists
           //   SrcDirList :=  ;
           //   TarDirList :=  ;
           // not multidirs, pass source, file and target separately
                SrcDir := CopySrcDir.Text ;
                SrcFName := CopySrcFile.Text ;
                TarDir := CopyTarDir.Text ;
                IgnorePaths := CopyIgnorePath.Text ;  // 22 May 2013
                Wow64RedirDisable := CopyWow64Disable.Checked ; // 22 May 2013
            // CopyType: FCTypeSingle, FCTypeMaskDir, FCTypeArchDir, FCTypeAllDir, FCTypeDates
                CopyType := FCTypeMaskDir ;
                SubDirs := CopySubdirs.Checked ;   // true copy all sub directories
                EmptyDirs := CopyEmptyDirs.Checked ; // true copy empty directories
                DelDone := false ;  // true delete source file after copy
                DelOldTar := false ; // true delete target files not in source directories
                Mask := false ;     // true, allow date/time masked characters in SrcFName
                Prev := false ;     // true, use yesterday's date for Mask
                Repl := TIcsFileCopyRepl (CopyFileRepl.ItemIndex);   { V8.60 }
                ReplRO := true ;    // true, replace read only files
                Safe := false ;     // true, copy file with TMP extension, rename when done
                LocalHost := OverbyteIcsWSocket.LocalHostName ;
                ProgressEvent := Nil ;
                CopyEvent := Nil ;
                if ShowXProgesss.Checked then    // 22 May 2013
                    ProgressEvent := onProgressEvent
                else
                    CopyEvent := onCopyEvent ;
            { following properties only available if VCLZip available
                Zipped := false ;
                ZipDownDel := false ;
           // ZipExtFmt: ExtnAdd, ExtnReplace
           // ZipPath: PathNone, PathNew, PathOriginal, PathNewOrig, PathSpecific, PathSpecOrig
           // ZipType: TypeUnzip, TypeSrcAddX, TypeSrcReplX, TypeSrcDirs
                ZipType := TypeUnzip ;
                ZipPath := PathNone ;
                ZipDir := '' ;   }
                IgnoreFileExt := 'tmp' ;
                taskres := SelCopyFiles (checkflag) ;   // main file copy function
                AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('Copying Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (FileCopyClient) ;
        LabelProgress.Caption := 'Copy Completed' ;
        doCopyFiles.Enabled := true ;
        doCopyCheck.Enabled := true ;
        doCopyList.Enabled := true ;
        doCopyAbort.Enabled := false ;
    end ;
end;

procedure TFormXferDemo.DeleteFiles(Sender: TObject);
var
    taskres: TIcsTaskResult ;
    checkflag: boolean ;
begin
    OpenLogFile;
    AddLogText (DateToStr (Now)) ;
    AbortFlag := false ;
    FileCopyClient := TIcsFileCopyW.Create (self) ;
    doDeleteFiles.Enabled := false ;
    doDeleteCheck.Enabled := false ;
    doCopyAbort.Enabled := true ;
    try
        checkflag := false ;
        if Sender = doDeleteCheck then checkflag := true ;
        try
            with FileCopyClient do
            begin
                SrcDir := CopyTarDir.Text ;
                SrcFName := CopySrcFile.Text ;
                MultiDir := false ; // true copy multiple specific directorie
                SubDirs := CopySubdirs.Checked ;   // true copy all sub directories
                EmptyDirs := CopyEmptyDirs.Checked ; // true copy empty directories
                IgnorePaths := CopyIgnorePath.Text ;  // 22 May 2013
                Mask := false ;     // true, allow date/time masked characters in SrcFName
                Prev := false ;     // true, use yesterday's date for Mask
                CopyLoDT := 0 ;     // lower date range
                CopyHiDT := Now ;   // higher date range
                if ShowXProgesss.Checked then    // 22 May 2013
                    ProgressEvent := onProgressEvent
                else
                    CopyEvent := onCopyEvent ;
                taskres := DeleteFiles (checkflag) ;   // main file delete function
                AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('Copying Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (FileCopyClient) ;
        LabelProgress.Caption := 'Delete Completed' ;
        doDeleteFiles.Enabled := true ;
        doDeleteCheck.Enabled := true ;
        doCopyAbort.Enabled := false ;
    end ;
end;

procedure TFormXferDemo.HttpDownload(Sender: TObject);
var
    taskres: TIcsTaskResult ;
    checkflag: boolean ;
    bandwidth: integer ;
begin
    OpenLogFile;
    AddLogText (DateToStr (Now)) ;
    AbortFlag := false ;
    doHttpDownCheck.Enabled := false ;
    doHttpDownFiles.Enabled := false ;
    doHttpAbort.Enabled := true ;
    HttpMultiClient := TIcsHttpMultiW.Create (self) ;
    try
        try
            checkflag := false ;
            if Sender = doHttpDownCheck then checkflag := true ;
            with HttpMultiClient do
            begin
                URLList := HttpSrcDir.Lines.Text ;  // one or more source URLs separated by CRLF
                SocketFamily := TSocketFamily (XferSockFamily.ItemIndex) ;  // Mar 2019
                SrcMask := HttpSrcFile.Text ; // optional source file mask to restrict downloads
                DownDir := HttpTarDir.Text ;  // directory for downloaded files
                KeepPath := false ;           // if true, use HTTP path for subdirs
                KeepHost := false ;           // if true, use HTTP host for subdir
                ParseHTML := true ;           // if true, parse HTML page for links to files
                MaxAttempts := 3 ;  // logon attempts
                SocketErrs := wsErrFriendly;        // Nov 2016
                Repl := TIcsFileCopyRepl (HttpFileRepl.ItemIndex);   { V8.60 }
                ReplRO := true ;              // replace read only files
                LogFiles := true ;            // log each file downloaded
                LogProt := ShowDiagsHigh.Checked ;// log HTTP protocol
                LogLDir := false ;            // log destination directory
                LogRDir := true ;             // log created HTTP directory
                Options := Options + [httpoEnableContentCoding] ;  // 27 Nov 2005
                bandwidth := atoi (HttpBandWidth.Text) ;  // 31 Dec 2007
                if bandwidth > 0 then
                begin
                    BandwidthLimit := bandwidth * 1024 ;
                    Options := Options + [HttpoBandwidthControl] ;
                end
                else
                    Options := Options - [HttpoBandwidthControl] ;
                ParseHTML := HttpParsePage.Checked;                // V8.66
                ParseLevels := atoi(HttpParseLevels.Text);         // V8.66 
{$IFNDEF NO_DEBUG_LOG}
                IcsLogger := IcsLog ;
                IcsLog.LogOptions := [] ;
                if ShowDiagsLow.Checked then
                    IcsLog.LogOptions := [loDestEvent, loAddStamp] + LogAllOptInfo ; // 3 Jan 2006
                if ShowDiagsSSL.Checked then
                    IcsLog.LogOptions := IcsLog.LogOptions + [loSslDump] ;
{$ENDIF}
                SslSessCache := false ;  // 27 Nov 2005
                HttpSslVerMethod := THttpSslVerifyMethod (VerifyCertMode.ItemIndex);  // 20 Apr 2015
                SslRevocation := RevokeCheck.Checked ;    // 20 Apr 2015
                SslReportChain := ReportChain.Checked ;   // 20 Apr 2015
                SslCliSecurity := TSslCliSecurity(SslSecurity.ItemIndex);  // June 2018
                CopyEvent := onCopyEvent ;
//              onBeforeHeaderSend := onHttpBeforeHeaderSendEvent ; // 11 July 2012 testing bad headers
           //   ProxyPort :=  ;
           //   Proxy :=  ;
             { following properties only available if VCLZip available
                Zipped := false ;
                ZipDownDel := false ;
           // ZipExtFmt: ExtnAdd, ExtnReplace
           // ZipPath: PathNone, PathNew, PathOriginal, PathNewOrig, PathSpecific, PathSpecOrig
           // ZipType: TypeUnzip, TypeSrcAddX, TypeSrcReplX, TypeSrcDirs
                ZipPath := PathNone ;
                ZipDir := '' ;   }
                taskres := Download (checkflag) ;  // main download function
                AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('Download Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (HttpMultiClient) ;
        LabelProgress.Caption := 'HTTP Completed' ;
        doHttpDownCheck.Enabled := true ;
        doHttpDownFiles.Enabled := true ;
        doHttpAbort.Enabled := false ;
    end ;
end;

function TFormXferDemo.SetFTPGen: boolean ;
var
    bandwidth: integer ;
begin
    OpenLogFile;
    AddLogText (DateToStr (Now)) ;
    AbortFlag := false ;
    result := false ;
    if FtpHost.Items.IndexOf (FtpHost.Text) < 0 then
    begin
        if FtpHost.Items.Count > 50 then
                FtpHost.Items.Delete (FtpHost.Items.Count - 1) ;
        FtpHost.Items.Insert (0, FtpHost.Text) ;
    end ;
    try
        with FtpMultiClient do
        begin
            Utf8DiagFlag := ShowDiagsUtf8.Checked ;  // 13 Nov 2008
            LocalHost := OverbyteIcsWSocket.LocalHostName ;
            HostName1 := FtpHost.Text ;
            HostName2 := '' ;
            SocketFamily := TSocketFamily (XferSockFamily.ItemIndex) ;  // Mar 2019
            UserName := FtpUsername.Text ;
            PassWord := FtpPassword.Text ;
            FtpType := TFtpType (FtpServerType.ItemIndex) ;
            Port := FtpPort.Text ;
            AttemptDelay := 5 ;
            MaxAttempts := 2 ;  // logon attempts
            FailRepeat := 3 ;   // retries for failed xfers
            SocketErrs := wsErrFriendly;        // Nov 2016
            KeepAliveSecs := atoi (FtpKeepAlive.Text) ;  // 10 July 2006
       // ConnectionType: ftpDirect, ftpProxy, ftpSocks4, ftpSocks4A, ftpSocks5
            ConnectionType := ftpDirect ;
       //     ConnectionType := ftpSocks4 ;
            SocksPort := '' ;
            SocksServer := '' ;
            ProxyPort := '' ;
            ProxyServer := '' ;
            SocksUsercode := '' ;
            SocksPassword := '' ;
            if ConnectionType = ftpProxy then
            begin
                ProxyPort := FtpPort.Text ;
                ProxyServer := '' ;
            end
            else if ConnectionType >= ftpSocks4 then
            begin
                SocksPort := '1080' ;
                SocksServer := '192.168.1.66' ;
                if ConnectionType = ftpSocks5 then
                begin
                    SocksUsercode := '' ;
                    SocksPassword := '' ;
                end ;
            end ;
            PassiveX := FtpPassive.Checked ;  // must be after connection type
       // HostType: FTPTYPE_NONE, FTPTYPE_UNIX, FTPTYPE_DOS, FTPTYPE_MVS, FTPTYPE_AS400, FTPTYPE_MLSD
            HostType := FTPTYPE_NONE ;
       // TXferMode: XferModeBinary, XferModeAscii
            XferMode := XferModeBinary ;
       // TCaseFile: FileLowerCase, FileMixedCase
            CaseFile := FileLowerCase ;
            DiffStampMins := 62 ;
            Timeout := 600 ;    // 18 Sept 2006, 60 secs was too slow for MD5Sum
            DispLog := true ;
            DispFiles := true ;
            DispRDir:= true ;
            DispLDir:= false ;
            MinResSize := 65535 ;   // also used for resume overlap
//            MinResSize := 0 ;       // test no resume overlap
            ProgressEvent := Nil ;
            CopyEvent := Nil ;
            if ShowXProgesss.Checked then    // 22 May 2013
                ProgressEvent := onProgressEvent
            else
                CopyEvent := onCopyEvent ;
            UpArchDir := '' ;
            UpArchive := false ;
            ResFailed := true ;
            UseCompression := true ;  // 3 Dec 2005
            if FtpNoFeatCmd.Checked then // 7 Nov 2007
                MagFtpOpts := MagFtpOpts + [magftpNoFeat]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoFeat] ;
         {   if FtpNoMd5Crc.Checked then // 5 Jan 2008
                MagFtpOpts := MagFtpOpts + [magftpNoMd5Crc]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoMd5Crc] ;   }
            if FtpNoZlib.Checked then // 5 Jan 2008
                MagFtpOpts := MagFtpOpts + [magftpNoZlib]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoZlib] ;
            if FtpNoTmpFile.Checked then // 5 Jan 2008
                MagFtpOpts := MagFtpOpts + [magftpNoTmpFile]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoTmpFile] ;
            if FtpNoUtf8.Checked then   // 20 Sept 2008
                MagFtpOpts := MagFtpOpts + [magftpNoUtf8]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoUtf8] ;
            if ftpNoHost.Checked then   // 20 Sept 2008
                MagFtpOpts := MagFtpOpts + [magftpNoHost]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoHost] ;
            if ftpIgnoreUtf8.Checked then   // 20 Sept 2008
                MagFtpOpts := MagFtpOpts + [magftpIgnoreUtf8]
            else
                MagFtpOpts := MagFtpOpts - [magftpIgnoreUtf8] ;
       // 22 May 2009 disable MD5 or CRC separately for testing
            if FtpNoMd5.Checked then
                MagFtpOpts := MagFtpOpts + [magftpNoMd5]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoMd5] ;
            if FtpNoCrc.Checked then
                MagFtpOpts := MagFtpOpts + [magftpNoCrc]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoCrc] ;
        //  MagFtpOpts := MagFtpOpts + [magftpNoCrc] ;
        //    MagFtpOpts := MagFtpOpts + [magftpNoMd5] ;

            ZlibNoCompExt := '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;'; // 2 Dec 2007
            ZlibMaxSize := 500000000 ;   // 9 Dec 2007 500 megs
            MaxResumeAttempts := 10 ;    // 31 Dec 2007
            bandwidth := atoi (FtpBandWidth.Text) ;  // 31 Dec 2007
            if bandwidth > 0 then
            begin
                BandwidthLimit := bandwidth * 1024 ;
                Options := Options + [ftpBandwidthControl] ;
            end
            else
                Options := Options - [ftpBandwidthControl] ;
            if FtpFixPassiveLanIP.Checked then                   { V8.63 }
                Options := Options + [ftpFixPasvLanIP]
            else
                Options := Options - [ftpFixPasvLanIP] ;
{$IFNDEF NO_DEBUG_LOG}
            IcsLogger := IcsLog ;
            IcsLog.LogOptions := [] ;
            if ShowDiagsLow.Checked then
                IcsLog.LogOptions := [loDestEvent, loAddStamp] +
                                                 LogAllOptInfo ; // 7 Jan 2006
            if ShowDiagsSSL.Checked then
                    IcsLog.LogOptions := IcsLog.LogOptions + [loSslDump] ;
{$ENDIF}
            FtpSslPort := FtpPortSsl.Text ;
            SslSessCache := true ;  // 27 Nov 2005
            FtpSslVerMethod := TFtpSslVerifyMethod (VerifyCertMode.ItemIndex);  // 20 Apr 2015
            FtpSslRevocation := RevokeCheck.Checked ;    // 20 Apr 2015
            FtpSslReportChain := ReportChain.Checked ;   // 20 Apr 2015
            FtpSslCliSecurity := TSslCliSecurity(SslSecurity.ItemIndex);  // June 2018

        { following properties only available if VCLZip available
            Zipped := false ;
            ZipDownDel := false ;
        // ZipExtFmt: ExtnAdd, ExtnReplace
        // ZipPath: PathNone, PathNew, PathOriginal, PathNewOrig, PathSpecific, PathSpecOrig
        // ZipType: TypeUnzip, TypeSrcAddX, TypeSrcReplX, TypeSrcDirs
            ZipExtFmt := ExtnAdd ;
            ZipPath := PathNone ;
            ZipDir := '' ;   }
            DispRemList := true ;
        end ;
    except
        AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        result := true ;
    end ;
end;

procedure TFormXferDemo.onThreadEventW (LogLevel: TIcsCopyLogLevel ; const Id, Info: UnicodeString ;
                                                                  var Cancel: boolean) ;
begin
    onCopyEvent (LogLevel, Id + ': ' + Info, Cancel) ;
end ;

procedure TFormXferDemo.OnThreadTerminate (Sender: TObject) ;
var
    MagFtpThread: TIcsFtpMultiThreadW ;
begin
    MagFtpThread := (Sender as TIcsFtpMultiThreadW) ;
    AddLogText ('Terminated FTP: ' + MagFtpThread.ID) ;
    dec (CurThreads) ;
    if CurThreads <= 0 then doFtpAbort.Enabled := false ;
    AddLogText ('Task Result: ' + IcsGetTaskResName (MagFtpThread.TaskRes)) ;
    if MagFtpThread.TaskRes = TaskResOKNew then
    begin
        if MagFtpThread.FtpThreadOpt = ftpthdList then
        begin
             AddLogText (MagFtpThread.Dirlisting) ;
        end;
    end;
end;

function TFormXferDemo.SetFTPThreadGen: integer ;
var
    bandwidth: integer ;
begin
    OpenLogFile;
    AddLogText (DateToStr (Now)) ;
    AbortFlag := false ;
    doFtpAbort.Enabled := true ;
    result := -1 ;
    if FtpHost.Items.IndexOf (FtpHost.Text) < 0 then
    begin
        if FtpHost.Items.Count > 50 then
                FtpHost.Items.Delete (FtpHost.Items.Count - 1) ;
        FtpHost.Items.Insert (0, FtpHost.Text) ;
    end ;
    try
        if CurThreads >= MaxThreads then exit ;
        IcsFtpThreads [NextThread] := TIcsFtpMultiThreadW.CreateThread ;
        result := NextThread ;
        inc (CurThreads) ;
        inc (NextThread) ;
        if NextThread >= MaxThreads then NextThread := 0 ;  // assume earlier threads have been freed
        with IcsFtpThreads [result] do
        begin
            FThreadEvent := onThreadEventW ;
            OnTerminate := OnThreadTerminate ;
            FreeOnTerminate := true ;
            Tag := result ;
            ID := 'ThreadNr=' + IntToStr (result) ;
      //      LogmaskName := '"' + TestingDir + 'logs\' + ID + '-"yyyymmdd".log"' ;
            Utf8DiagFlag := ShowDiagsUtf8.Checked ;  // 13 Nov 2008
            NoProgress := true ;  // stop progress log events
            LocalHost := OverbyteIcsWSocket.LocalHostName ;
            HostName1 := FtpHost.Text ;
            HostName2 := '' ;
            SocketFamily := TSocketFamily (XferSockFamily.ItemIndex) ;  // Mar 2019
            SocketErrs := wsErrFriendly;        // Nov 2016
            UserName := FtpUsername.Text ;
            PassWord := FtpPassword.Text ;
            FtpType := TFtpType (FtpServerType.ItemIndex) ;
            Port := FtpPort.Text ;
            AttemptDelay := 5 ;
            MaxAttempts := 2 ;  // logon attempts
            FailRepeat := 3 ;   // retries for failed xfers
            KeepAliveSecs := atoi (FtpKeepAlive.Text) ;  // 10 July 2006
       // ConnectionType: ftpDirect, ftpProxy, ftpSocks4, ftpSocks4A, ftpSocks5
            ConnectionType := ftpDirect ;
       //     ConnectionType := ftpSocks4 ;
            SocksPort := '' ;
            SocksServer := '' ;
            ProxyPort := '' ;
            ProxyServer := '' ;
            SocksUsercode := '' ;
            SocksPassword := '' ;
            if ConnectionType = ftpProxy then
            begin
                ProxyPort := FtpPort.Text ;
                ProxyServer := '' ;
            end
            else if ConnectionType >= ftpSocks4 then
            begin
                SocksPort := '1080' ;
                SocksServer := '192.168.1.66' ;
                if ConnectionType = ftpSocks5 then
                begin
                    SocksUsercode := '' ;
                    SocksPassword := '' ;
                end ;
            end ;
            PassiveX := FtpPassive.Checked ;  // must be after connection type
       // HostType: FTPTYPE_NONE, FTPTYPE_UNIX, FTPTYPE_DOS, FTPTYPE_MVS, FTPTYPE_AS400, FTPTYPE_MLSD
            HostType := FTPTYPE_NONE ;
       // TXferMode: XferModeBinary, XferModeAscii
            XferMode := XferModeBinary ;
       // TCaseFile: FileLowerCase, FileMixedCase
            CaseFile := FileLowerCase ;
            DiffStampMins := 62 ;
            Timeout := 600 ;    // 18 Sept 2006, 60 secs was too slow for MD5Sum
            DispLog := true ;
            DispFiles := true ;
            DispRDir:= true ;
            DispLDir:= false ;
            MinResSize := 65535 ;   // also used for resume overlap
//            MinResSize := 0 ;       // test no resume overlap
            UpArchDir := '' ;
            UpArchive := false ;
            ResFailed := true ;
            UseCompression := true ;  // 3 Dec 2005
            if FtpNoFeatCmd.Checked then // 7 Nov 2007
                MagFtpOpts := MagFtpOpts + [magftpNoFeat]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoFeat] ;
            if FtpNoZlib.Checked then // 5 Jan 2008
                MagFtpOpts := MagFtpOpts + [magftpNoZlib]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoZlib] ;
            if FtpNoTmpFile.Checked then // 5 Jan 2008
                MagFtpOpts := MagFtpOpts + [magftpNoTmpFile]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoTmpFile] ;
            if FtpNoUtf8.Checked then   // 20 Sept 2008
                MagFtpOpts := MagFtpOpts + [magftpNoUtf8]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoUtf8] ;
            if ftpNoHost.Checked then   // 20 Sept 2008
                MagFtpOpts := MagFtpOpts + [magftpNoHost]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoHost] ;
            if ftpIgnoreUtf8.Checked then   // 20 Sept 2008
                MagFtpOpts := MagFtpOpts + [magftpIgnoreUtf8]
            else
                MagFtpOpts := MagFtpOpts - [magftpIgnoreUtf8] ;
       // 22 May 2009 disable MD5 or CRC separately for testing
            if FtpNoMd5.Checked then
                MagFtpOpts := MagFtpOpts + [magftpNoMd5]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoMd5] ;
            if FtpNoCrc.Checked then
                MagFtpOpts := MagFtpOpts + [magftpNoCrc]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoCrc] ;
            ZlibNoCompExt := '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;'; // 2 Dec 2007
            ZlibMaxSize := 500000000 ;   // 9 Dec 2007 500 megs
            MaxResumeAttempts := 10 ;    // 31 Dec 2007
            bandwidth := atoi (FtpBandWidth.Text) ;  // 31 Dec 2007
            if bandwidth > 0 then
            begin
                BandwidthLimit := bandwidth * 1024 ;
                Options := Options + [ftpBandwidthControl] ;
            end
            else
                Options := Options - [ftpBandwidthControl] ;
            if FtpFixPassiveLanIP.Checked then                   { V8.63 }
                Options := Options + [ftpFixPasvLanIP]
            else
                Options := Options - [ftpFixPasvLanIP] ;
            FtpSslPort := FtpPortSsl.Text ;
            SslSessCache := true ;  // 27 Nov 2005
            FtpSslVerMethod := TFtpSslVerifyMethod (VerifyCertMode.ItemIndex);  // 20 Apr 2015
            FtpSslRevocation := RevokeCheck.Checked ;    // 20 Apr 2015
            FtpSslReportChain := ReportChain.Checked ;   // 20 Apr 2015
        { following properties only available if VCLZip available
            Zipped := false ;
            ZipDownDel := false ;
        // ZipExtFmt: ExtnAdd, ExtnReplace
        // ZipPath: PathNone, PathNew, PathOriginal, PathNewOrig, PathSpecific, PathSpecOrig
        // ZipType: TypeUnzip, TypeSrcAddX, TypeSrcReplX, TypeSrcDirs
            ZipExtFmt := ExtnAdd ;
            ZipPath := PathNone ;
            ZipDir := '' ;   }
            DispRemList := true ;
        end ;
    except
        AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
    end ;
end;


procedure TFormXferDemo.SetFtpButtons (value: boolean) ;
begin
    doFtpDownCheck.Enabled := value ;
    doFtpDownFiles.Enabled := value ;
    doFtpUpCheck.Enabled := value ;
    doFtpUpFiles.Enabled := value ;
    doFtpList.Enabled := value ;
    doFtpDown1.Enabled := value ;
    doFtpAbort.Enabled := NOT value ;
    doFtpAbort1.Enabled := NOT value ;
end ;

procedure TFormXferDemo.doFtpListClick(Sender: TObject);
var
    dirlisting: UnicodeString ;
    taskres: TIcsTaskResult ;
begin
    AbortFlag := false ;
    SetFtpButtons (false) ;
    FtpMultiClient := TIcsFtpMultiW.Create (self) ;
    try
        if SetFTPGen then exit ;
        try
            with FtpMultiClient do
            begin
                BulkMode := BulkModeDownload ;
                SrcDir := FtpPath.Text ;
                SubDirs := FtpSubdirs.Checked ;
                EmptyDirs := FtpEmptyDirs.Checked ; // true copy empty directories
                taskres := DispFtpDir (dirlisting) ;  // main FTP function
                AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
                AddLogText (IcsCRLF + dirlisting) ;
            end ;
        except
            AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (FtpMultiClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        SetFtpButtons (true) ;
    end ;
end;

procedure TFormXferDemo.doFtpListThreadClick(Sender: TObject);
var
    threadnr: integer ;
begin
    AbortFlag := false ;
    try
        threadnr := SetFTPThreadGen ;
        if threadnr < 0 then exit ;
        with IcsFtpThreads [threadnr] do
        begin
            BulkMode := BulkModeDownload ;
            SrcDir := FtpPath.Text ;
            SubDirs := FtpSubdirs.Checked ;
            EmptyDirs := FtpEmptyDirs.Checked ; // true copy empty directories
            FtpThreadOpt := ftpthdList ;
            Resume;   // thread starts
            AddLogText ('Created FTP: ' + ID) ;
        end ;
    except
        AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
    end ;
end;

procedure TFormXferDemo.FtpDownload(Sender: TObject);
var
    checkflag: boolean ;
    taskres: TIcsTaskResult ;
begin
    AbortFlag := false ;
    SetFtpButtons (false) ;
    FtpMultiClient := TIcsFtpMultiW.Create (self) ;
    try
        if SetFTPGen then exit ;
        try
            checkflag := false ;
            if Sender = doFtpDownCheck then checkflag := true ;
            with FtpMultiClient do
            begin
                BulkMode := BulkModeDownload ;
                SrcDir := FtpPath.Text ;
                TarDir := FtpLocDir.Text ;
            // CopyType: FCTypeSingle, FCTypeMaskDir, FCTypeArchDir, FCTypeAllDir, FCTypeDates
                if FtpCopyAllDir.Checked then     // 22 Feb 2008
                    CopyType := FCTypeAllDir
                else
                    CopyType := FCTypeMaskDir ;
                DelDone := FtpDelDone.Checked ;    // 18 Nov 2005
                DelOldTar := FtpDelOldTar.Checked ; // 22 Feb 2008
                SubDirs := FtpSubdirs.Checked ;   // true copy all sub directories
                EmptyDirs := FtpEmptyDirs.Checked ; // true copy empty directories
                IgnorePaths := FtpIgnorePath.Text ;  // 22 May 2013
                SrcFName := FtpSrcFile.Text ;  // 8 Apr 2009 - may include masked directories if Mask=true, and MaskLocDir and/or MaskRemDir
                Mask := false ;     // true, allow date/time masked characters and directories in SrcFName
                Prev := false ;     // true, use yesterday's date for Mask
                MaskLocDir := false ; // 8 Apr 2009 - true use masked directories from SrcFName
                MaskRemDir := false ; // 8 Apr 2009 - true use masked directories from SrcFName
                Repl := TIcsFileCopyRepl (FtpMultiFileRepl.ItemIndex);   { V8.60 }
                ReplRO := true ;    // true, replace read only files
                Safe := false ;     // true, copy file with TMP extension, rename when done
                IgnoreFileExt := 'tmp;ftp' ;
                taskres := FtpDownload (checkflag) ;   // main FTP function
                AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (FtpMultiClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        SetFtpButtons (true) ;
    end ;
end;


procedure TFormXferDemo.FtpUpload(Sender: TObject);
var
    checkflag: boolean ;
    taskres: TIcsTaskResult ;
begin
    AbortFlag := false ;
    SetFtpButtons (false) ;
    FtpMultiClient := TIcsFtpMultiW.Create (self) ;
    try
        if SetFTPGen then exit ;
        try
            checkflag := false ;
            if Sender = doFtpUpCheck then checkflag := true ;
            with FtpMultiClient do
            begin
                BulkMode := BulkModeUpload ;
                SrcDir := FtpLocDir.Text ;
                TarDir := FtpPath.Text ;
            // CopyType: FCTypeSingle, FCTypeMaskDir, FCTypeArchDir, FCTypeAllDir, FCTypeDates
                if FtpCopyAllDir.Checked then     // 22 Feb 2008
                    CopyType := FCTypeAllDir
                else
                    CopyType := FCTypeMaskDir ;
                DelDone := FtpDelDone.Checked ;   // 18 Nov 2005
                DelOldTar := FtpDelOldTar.Checked ; // 22 Feb 2008
                SubDirs := FtpSubdirs.Checked ;   // true copy all sub directories
                EmptyDirs := FtpEmptyDirs.Checked ; // true copy empty directories
                IgnorePaths := FtpIgnorePath.Text ;  // 22 May 2013
                SrcFName := FtpSrcFile.Text ;  // 8 Apr 2009 - may include masked directories if Mask=true, and MaskLocDir and/or MaskRemDir
                Mask := false ;     // true, allow date/time masked characters and directories in SrcFName
                Prev := false ;     // true, use yesterday's date for Mask
                MaskLocDir := false ; // 8 Apr 2009 - true use masked directories from SrcFName
                MaskRemDir := false ; // 8 Apr 2009 - true use masked directories from SrcFName
                Repl := TIcsFileCopyRepl (FtpMultiFileRepl.ItemIndex);   { V8.60 }
                Safe := true ;     // ignored
                IgnoreFileExt := 'tmp' ;
                TimeStamp := false ; // update local file time stamp to match remote
                taskres := FtpUpload (checkflag) ;   // main FTP function
                AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (FtpMultiClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        SetFtpButtons (true) ;
    end ;
end;

procedure TFormXferDemo.doFtpAbortClick(Sender: TObject);
begin
    AbortFlag := True;
    SetFtpButtons (true) ;    // Oct 2020
end;

procedure TFormXferDemo.doFtpDown1Click(Sender: TObject);
var
    taskres: TIcsTaskResult ;
    Replace: TIcsFileCopyRepl ;
begin
    AbortFlag := false ;
    SetFtpButtons (false) ;
    FtpMultiClient := TIcsFtpMultiW.Create (self) ;
    try
        if SetFTPGen then exit ;
        try
            with FtpMultiClient do
            begin
                SrcDir := '/' ;
                BulkMode := BulkModeDownload ;
            //  DelFile := FtpDelDone.Checked ;    pending TMagFtp fix
                Replace := TIcsFileCopyRepl (FtpOneFileRepl.ItemIndex);   { V8.60 }
                taskres := FtpLogon ;
                if taskres = TaskResOKNew then
                begin
                    taskres := FtpDownOneFile (Ftp1Path.Text, Ftp1SrcName.Text,
                            Ftp1LocDir.Text + '\' + Ftp1SrcName.Text, Replace) ;
                end ;
                AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FtpMultiClient.FtpLogoff ;
        FreeAndNil (FtpMultiClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        SetFtpButtons (true) ;
    end ;
end;

procedure TFormXferDemo.doFtpUp1Click(Sender: TObject);
var
    taskres: TIcsTaskResult ;
    Replace: TIcsFileCopyRepl ;
begin
    AbortFlag := false ;
    SetFtpButtons (false) ;
    FtpMultiClient := TIcsFtpMultiW.Create (self) ;
    try
        if SetFTPGen then exit ;
        try
            with FtpMultiClient do
            begin
                TarDir := '/' ;
                BulkMode := BulkModeUpload ;
            //  DelFile := FtpDelDone.Checked ;    pending TMagFtp fix
                Replace := TIcsFileCopyRepl (FtpOneFileRepl.ItemIndex);   { V8.60 }
                taskres := FtpLogon ;
                if taskres = TaskResOKNew then
                begin
                    taskres := FtpUpOneFile (Ftp1UpFile.Text, Ftp1Path.Text,
                                                  Ftp1SrcName.Text, Replace) ;
                end ;
                AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FtpMultiClient.FtpLogoff ;
        FreeAndNil (FtpMultiClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        SetFtpButtons (true) ;
    end ;
end;

procedure TFormXferDemo.doFtpDownThreadClick(Sender: TObject);
var
    threadnr: integer ;
begin
    AbortFlag := false ;
    try
        threadnr := SetFTPThreadGen ;
        if threadnr < 0 then exit ;
        with IcsFtpThreads [threadnr] do
        begin
            BulkMode := BulkModeDownload ;
            SrcDir := FtpPath.Text ;
            TarDir := FtpLocDir.Text ;
        // CopyType: FCTypeSingle, FCTypeMaskDir, FCTypeArchDir, FCTypeAllDir, FCTypeDates
            if FtpCopyAllDir.Checked then     // 22 Feb 2008
                CopyType := FCTypeAllDir
            else
                CopyType := FCTypeMaskDir ;
            DelDone := FtpDelDone.Checked ;    // 18 Nov 2005
            DelOldTar := FtpDelOldTar.Checked ; // 22 Feb 2008
            SubDirs := FtpSubdirs.Checked ;   // true copy all sub directories
            EmptyDirs := FtpEmptyDirs.Checked ; // true copy empty directories
//            IgnorePaths := FtpIgnorePath.Text ;  // 22 May 2013
            SrcFName := FtpSrcFile.Text ;  // 8 Apr 2009 - may include masked directories if Mask=true, and MaskLocDir and/or MaskRemDir
            Mask := false ;     // true, allow date/time masked characters and directories in SrcFName
            Prev := false ;     // true, use yesterday's date for Mask
            MaskLocDir := false ; // 8 Apr 2009 - true use masked directories from SrcFName
            MaskRemDir := false ; // 8 Apr 2009 - true use masked directories from SrcFName
            Repl := TIcsFileCopyRepl (FtpMultiFileRepl.ItemIndex);   { V8.60 }
            ReplRO := true ;    // true, replace read only files
            Safe := false ;     // true, copy file with TMP extension, rename when done
            IgnoreFileExt := 'tmp;ftp' ;
            FtpThreadOpt := ftpthdDownFiles ;
            Resume;   // thread starts
            AddLogText ('Created FTP: ' + ID) ;
        end ;
    except
        AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
    end ;
end;

procedure TFormXferDemo.doFtpUpThreadClick(Sender: TObject);
var
    threadnr: integer ;
begin
    AbortFlag := false ;
    try
        threadnr := SetFTPThreadGen ;
        if threadnr < 0 then exit ;
        with IcsFtpThreads [threadnr] do
        begin
            BulkMode := BulkModeUpload ;
            SrcDir := FtpLocDir.Text ;
            TarDir := FtpPath.Text ;
        // CopyType: FCTypeSingle, FCTypeMaskDir, FCTypeArchDir, FCTypeAllDir, FCTypeDates
            if FtpCopyAllDir.Checked then     // 22 Feb 2008
                CopyType := FCTypeAllDir
            else
                CopyType := FCTypeMaskDir ;
            DelDone := FtpDelDone.Checked ;   // 18 Nov 2005
            DelOldTar := FtpDelOldTar.Checked ; // 22 Feb 2008
            SubDirs := FtpSubdirs.Checked ;   // true copy all sub directories
            EmptyDirs := FtpEmptyDirs.Checked ; // true copy empty directories
//            IgnorePaths := FtpIgnorePath.Text ;  // 22 May 2013
            SrcFName := FtpSrcFile.Text ;  // 8 Apr 2009 - may include masked directories if Mask=true, and MaskLocDir and/or MaskRemDir
            Mask := false ;     // true, allow date/time masked characters and directories in SrcFName
            Prev := false ;     // true, use yesterday's date for Mask
            MaskLocDir := false ; // 8 Apr 2009 - true use masked directories from SrcFName
            MaskRemDir := false ; // 8 Apr 2009 - true use masked directories from SrcFName
            Repl := TIcsFileCopyRepl (FtpMultiFileRepl.ItemIndex);   { V8.60 }
            Safe := true ;     // ignored
            IgnoreFileExt := 'tmp' ;
            TimeStamp := false ; // update local file time stamp to match remote
            FtpThreadOpt := ftpthdUpFiles ;
            Resume;   // thread starts
            AddLogText ('Created FTP: ' + ID) ;
        end ;
    except
        AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
    end ;
end;

procedure TFormXferDemo.doClearClick(Sender: TObject);
begin
    CurrentLogDelim.Lines.Clear ;
    CurrentLogText.Lines.Clear ;
end;

end.
