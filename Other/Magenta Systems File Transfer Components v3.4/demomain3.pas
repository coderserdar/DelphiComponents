unit demomain3;
{
Updated by Angus Robertson, Magenta Systems Ltd, England, 17th May 2009
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

This is a test and demo application for the TMagHttp, TMagFtp and TMagFileCopy
Magenta Systems File Transfer Components.

Magenta Systems DUN Manager application (from http://www.magsys.co.uk/dunman/),
has more extensive demonstrations of these components, look under Scheduled Task
Properties, HTTP Download, FTP Upload, FTP Download and Sync Files.

Requires Internet Component Suite (ICS) V6 or V7 from http://www.overbyte.be/,
dated August 2008 or later.

Compatible with Delphi 7/2005/2006/2007/2009
Tested with Windows 98, NT4, Windows 2000, XP, 2003, Vista and 2008

Requires Kevin Boylan's TVCLZip component for zipping from http://www.vclzip.net/,
if you don't purchase this component you will need to suppress DEFINE Zipping from
MAGZIP.INC so the zip code is not linked.


Note this demo program does not make use of all the component's functionality,
but there are comments indicating where alternative properties may be set.

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
18 Nov 2008 - 3.2 - support ICS V7.02 with UTF-8, but only full Unicode with 2009 and later
                    FTP supports Unicode with UTF8 command, and HOST command
                    new label shows whether SSL compiled, and whether compiled with 2007 or 2009
                    SOCKS proxy should now work
                    keep last 50 FTP host names and HTTP URLs
07 Jan 2009 - 3.3 - updated OverbyteIcsFtpcli with latest changes, added ftptest.org hosts
17 May 2009 - 3.4 - FTP fixes for md5, added MaskLocDir and MaskRemDir for FTP, Unicode MD5 and CRC32
                    add magftpNoMd5 and magftpNoCrc to allow them to be tested separately



}
interface

{$I OverbyteIcsDefs.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ComCtrls, StdCtrls, IniFiles,
  MagentaCopy, MagentaFtp3, MagentaHttp, MagSubs1,
  OverbyteIcsWsocket, OverbyteIcsFtpcli, OverbyteIcsFtpSrvT,
  OverbyteIcsHttpProt, OverbyteIcsHttpCCodzlib, OverbyteIcsLogger;


type
  TForm1 = class(TForm)
// saved components
    CopySrcDir: TEdit;
    CopySrcFile: TEdit;
    CopyTarDir: TEdit;
    Ftp1LocDir: TEdit;
    Ftp1Path: TEdit;
    Ftp1SrcName: TEdit;
    Ftp1UpFile: TEdit;
    FtpBandWidth: TEdit;
    FtpCopyAllDir: TCheckBox;
    FtpDelDone: TCheckBox;
    FtpDelOldTar: TCheckBox;
    FtpHost: TComboBox;
    FtpKeepAlive: TEdit;
    FtpLocDir: TEdit;
    FtpNoFeatCmd: TCheckBox;
    FtpNoMd5Crc: TCheckBox;
    FtpNoTmpFile: TCheckBox;
    FtpNoZlib: TCheckBox;
    FtpNoUtf8: TCheckBox;
    ftpNoHost: TCheckBox;
    ftpIgnoreUtf8: TCheckBox;
    FtpOneDelDone: TCheckBox;
    FtpOneReplace: TCheckBox;
    FtpPassive: TCheckBox;
    FtpPassword: TEdit;
    FtpPath: TEdit;
    FtpPort: TEdit;
    FtpPortSsl: TEdit;
    FtpReplace: TCheckBox;
    FtpServerType: TComboBox;
    FtpSrcFile: TEdit;
    FtpSubdirs: TCheckBox;
    FtpUsername: TEdit;
    HttpSrcDir: TComboBox;
    HttpSrcFile: TComboBox;
    HttpTarDir: TEdit;
    NetLogon: TEdit;
    NetPassword: TEdit;
    ShowDiagsHigh: TCheckBox;
    ShowDiagsLow: TCheckBox;
    ShowDiagsSSL: TCheckBox;

// non-saved
    TabSheet5: TTabSheet;
    Label13: TLabel;
    Label8: TLabel;
    doTest: TButton;
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
    Label14: TLabel;
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
    LabelVersion: TLabel;
    doClear: TButton;
    Label25: TLabel;
    LabelSslState: TLabel;
    Label26: TLabel;
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
    procedure doTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure doClearClick(Sender: TObject);
  private
    { Private declarations }
    procedure AddLogText (S: string) ;
    procedure AddLogDelim (S: string) ;
    function SetFTPGen: boolean ;
    procedure SetFtpButtons (value: boolean) ;
    procedure onCopyEvent (LogLevel: TLogLevel ; Info: string ;
                                                      var Cancel: boolean) ;
    procedure onHttpEvent (LogLevel: TLogLevel ; Info: string ;
                                                  var Cancel: boolean) ;
    procedure onFtpEvent (LogLevel: TLogLevel ; Info: string ;
                                                  var Cancel: boolean) ;
    procedure IcsLogEvent (Sender: TObject; LogOption: TLogOption;
                              const Msg : String) ;
    procedure GetSettings ;
    procedure PutSettings ;
  public
    { Public declarations }
  end;

var
    Form1: TForm1;
    MagHTTPClient: TMagHTTP ;
    MagFTPClient: TMagFTP ;
    MagFileCopyClient: TMagFileCopy ;
    IcsLog: TIcsLogger ;
    AbortFlag: boolean ;
    IniFileName: string ;

implementation

{$R *.dfm}

procedure TForm1.GetSettings ;
var
    IniFile: TMemIniFile ;
    section: string ;
begin
    if NOT FileExists (IniFileName) then exit ;
    try
        IniFile := TMemIniFile.Create (IniFileName) ;
    except
        exit ;
    end ;
    try // finally
        section := 'demomain' ;
        with IniFile do
        begin
// following lines created by DelphiVar tool
  CopySrcDir.Text := ReadString (section, 'CopySrcDir_Text', '') ;
  CopySrcFile.Text := ReadString (section, 'CopySrcFile_Text', '') ;
  CopyTarDir.Text := ReadString (section, 'CopyTarDir_Text', '') ;
  Ftp1LocDir.Text := ReadString (section, 'Ftp1LocDir_Text', '') ;
  Ftp1Path.Text := ReadString (section, 'Ftp1Path_Text', '') ;
  Ftp1SrcName.Text := ReadString (section, 'Ftp1SrcName_Text', '') ;
  Ftp1UpFile.Text := ReadString (section, 'Ftp1UpFile_Text', '') ;
  FtpBandWidth.Text := ReadString (section, 'FtpBandWidth_Text', '') ;
  if ReadString (section, 'FtpCopyAllDir_Checked', 'False') = 'True' then FtpCopyAllDir.Checked := true else FtpCopyAllDir.Checked := false ;
  if ReadString (section, 'FtpDelDone_Checked', 'False') = 'True' then FtpDelDone.Checked := true else FtpDelDone.Checked := false ;
  if ReadString (section, 'FtpDelOldTar_Checked', 'False') = 'True' then FtpDelOldTar.Checked := true else FtpDelOldTar.Checked := false ;
  FtpHost.Text := ReadString (section, 'FtpHost_Text', '') ;
  FtpKeepAlive.Text := ReadString (section, 'FtpKeepAlive_Text', '') ;
  FtpLocDir.Text := ReadString (section, 'FtpLocDir_Text', '') ;
  if ReadString (section, 'FtpNoFeatCmd_Checked', 'False') = 'True' then FtpNoFeatCmd.Checked := true else FtpNoFeatCmd.Checked := false ;
  if ReadString (section, 'FtpNoMd5Crc_Checked', 'False') = 'True' then FtpNoMd5Crc.Checked := true else FtpNoMd5Crc.Checked := false ;
  if ReadString (section, 'FtpNoTmpFile_Checked', 'False') = 'True' then FtpNoTmpFile.Checked := true else FtpNoTmpFile.Checked := false ;
  if ReadString (section, 'FtpNoZlib_Checked', 'False') = 'True' then FtpNoZlib.Checked := true else FtpNoZlib.Checked := false ;
  if ReadString (section, 'FtpOneDelDone_Checked', 'False') = 'True' then FtpOneDelDone.Checked := true else FtpOneDelDone.Checked := false ;
  if ReadString (section, 'FtpOneReplace_Checked', 'False') = 'True' then FtpOneReplace.Checked := true else FtpOneReplace.Checked := false ;
  if ReadString (section, 'FtpPassive_Checked', 'False') = 'True' then FtpPassive.Checked := true else FtpPassive.Checked := false ;
  FtpPassword.Text := ReadString (section, 'FtpPassword_Text', '') ;
  FtpPath.Text := ReadString (section, 'FtpPath_Text', '') ;
  FtpPort.Text := ReadString (section, 'FtpPort_Text', '') ;
  FtpPortSsl.Text := ReadString (section, 'FtpPortSsl_Text', '') ;
  if ReadString (section, 'FtpReplace_Checked', 'False') = 'True' then FtpReplace.Checked := true else FtpReplace.Checked := false ;
  FtpServerType.ItemIndex := ReadInteger (section, 'FtpServerType_ItemIndex', 0) ;
  FtpSrcFile.Text := ReadString (section, 'FtpSrcFile_Text', '') ;
  if ReadString (section, 'FtpSubdirs_Checked', 'False') = 'True' then FtpSubdirs.Checked := true else FtpSubdirs.Checked := false ;
  FtpUsername.Text := ReadString (section, 'FtpUsername_Text', '') ;
  HttpSrcDir.Text := ReadString (section, 'HttpSrcDir_Text', '') ;
  HttpSrcFile.Text := ReadString (section, 'HttpSrcFile_Text', '') ;
  HttpTarDir.Text := ReadString (section, 'HttpTarDir_Text', '') ;
  NetLogon.Text := ReadString (section, 'NetLogon_Text', '') ;
  NetPassword.Text := ReadString (section, 'NetPassword_Text', '') ;
  if ReadString (section, 'ShowDiagsHigh_Checked', 'False') = 'True' then ShowDiagsHigh.Checked := true else ShowDiagsHigh.Checked := false ;
  if ReadString (section, 'ShowDiagsLow_Checked', 'False') = 'True' then ShowDiagsLow.Checked := true else ShowDiagsLow.Checked := false ;
  if ReadString (section, 'ShowDiagsSSL_Checked', 'False') = 'True' then ShowDiagsSSL.Checked := true else ShowDiagsSSL.Checked := false ;
  if ReadString (section, 'FtpNoUtf8_Checked', 'False') = 'True' then FtpNoUtf8.Checked := true else FtpNoUtf8.Checked := false ;
  if ReadString (section, 'ftpNoHost_Checked', 'False') = 'True' then ftpNoHost.Checked := true else ftpNoHost.Checked := false ;
  if ReadString (section, 'ftpIgnoreUtf8_Checked', 'False') = 'True' then ftpIgnoreUtf8.Checked := true else ftpIgnoreUtf8.Checked := false ;
  FtpHost.Items.CommaText := ReadString (section, 'FtpHost_Items', '') ;
  HttpSrcFile.Items.CommaText := ReadString (section, 'HttpSrcFile_Items', '') ;
    end ;
    finally
        IniFile.Free ;
    end ;

end ;

procedure TForm1.PutSettings ;
var
    IniFile: TMemIniFile ;
    section, temp: string ;
begin
    try
        IniFile := TMemIniFile.Create (IniFileName) ;
    except
        exit ;
    end ;
    try // finally
        section := 'demomain' ;
        with IniFile do
        begin
// following lines created by DelphiVar tool
  WriteString (section, 'CopySrcDir_Text', CopySrcDir.Text) ;
  WriteString (section, 'CopySrcFile_Text', CopySrcFile.Text) ;
  WriteString (section, 'CopyTarDir_Text', CopyTarDir.Text) ;
  WriteString (section, 'Ftp1LocDir_Text', Ftp1LocDir.Text) ;
  WriteString (section, 'Ftp1Path_Text', Ftp1Path.Text) ;
  WriteString (section, 'Ftp1SrcName_Text', Ftp1SrcName.Text) ;
  WriteString (section, 'Ftp1UpFile_Text', Ftp1UpFile.Text) ;
  WriteString (section, 'FtpBandWidth_Text', FtpBandWidth.Text) ;
  if FtpCopyAllDir.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpCopyAllDir_Checked', temp) ;
  if FtpDelDone.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpDelDone_Checked', temp) ;
  if FtpDelOldTar.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpDelOldTar_Checked', temp) ;
  WriteString (section, 'FtpHost_Text', FtpHost.Text) ;
  WriteString (section, 'FtpKeepAlive_Text', FtpKeepAlive.Text) ;
  WriteString (section, 'FtpLocDir_Text', FtpLocDir.Text) ;
  if FtpNoFeatCmd.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpNoFeatCmd_Checked', temp) ;
  if FtpNoMd5Crc.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpNoMd5Crc_Checked', temp) ;
  if FtpNoTmpFile.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpNoTmpFile_Checked', temp) ;
  if FtpNoZlib.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpNoZlib_Checked', temp) ;
  if FtpOneDelDone.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpOneDelDone_Checked', temp) ;
  if FtpOneReplace.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpOneReplace_Checked', temp) ;
  if FtpPassive.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpPassive_Checked', temp) ;
  WriteString (section, 'FtpPassword_Text', FtpPassword.Text) ;
  WriteString (section, 'FtpPath_Text', FtpPath.Text) ;
  WriteString (section, 'FtpPort_Text', FtpPort.Text) ;
  WriteString (section, 'FtpPortSsl_Text', FtpPortSsl.Text) ;
  if FtpReplace.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpReplace_Checked', temp) ;
  WriteInteger (section, 'FtpServerType_ItemIndex', FtpServerType.ItemIndex) ;
  WriteString (section, 'FtpSrcFile_Text', FtpSrcFile.Text) ;
  if FtpSubdirs.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpSubdirs_Checked', temp) ;
  WriteString (section, 'FtpUsername_Text', FtpUsername.Text) ;
  WriteString (section, 'HttpSrcDir_Text', HttpSrcDir.Text) ;
  WriteString (section, 'HttpSrcFile_Text', HttpSrcFile.Text) ;
  WriteString (section, 'HttpTarDir_Text', HttpTarDir.Text) ;
  WriteString (section, 'NetLogon_Text', NetLogon.Text) ;
  WriteString (section, 'NetPassword_Text', NetPassword.Text) ;
  if ShowDiagsHigh.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ShowDiagsHigh_Checked', temp) ;
  if ShowDiagsLow.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ShowDiagsLow_Checked', temp) ;
  if ShowDiagsSSL.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ShowDiagsSSL_Checked', temp) ;
  if FtpNoUtf8.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'FtpNoUtf8_Checked', temp) ;
  if ftpNoHost.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ftpNoHost_Checked', temp) ;
  if ftpIgnoreUtf8.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ftpIgnoreUtf8_Checked', temp) ;
  WriteString (section, 'FtpHost_Items', FtpHost.Items.CommaText) ;
  WriteString (section, 'HttpSrcFile_Items', HttpSrcFile.Items.CommaText) ;
        end ;
    finally
        IniFile.UpdateFile ;
        IniFile.Free ;
    end ;

end ;

procedure TForm1.AddLogText (S: string) ;
begin
    if Application.Terminated then exit ;
    LogText.Lines.Add (S) ;   // general unformatted comments about xfers
end ;

procedure TForm1.AddLogDelim (S: string) ;
begin
    LogDelim.Lines.Add (S) ;   // delimited formatted xfer information
end ;

procedure TForm1.doAbortClick(Sender: TObject);
begin
    AbortFlag := true ;
end;

procedure TForm1.doExitClick(Sender: TObject);
begin
    AbortFlag := true ;
    Close ;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
    I: TFtpType ;
begin
    IcsLog := TIcsLogger.Create (self) ;
    IcsLog.OnIcsLogEvent := IcsLogEvent ;
    FtpServerType.Items.Clear ;
    for I := Low(TFtpType) to High(TFtpType) do
                 FtpServerType.Items.Add (FtpTypeStrings [I]) ;
    LabelVersion.Caption := GetOSVersion ;
    IniFileName := ExtractFilePath (ParamStr (0)) + 'xferdemo3.ini' ;
    GetSettings ;
{$IFDEF USE_SSL}
    {$IFDEF UNICODE}
    LabelSslState.Caption := 'SSL Enabled, Unicode GUI' ;
    {$ELSE}
    LabelSslState.Caption := 'SSL Enabled, ASCII GUI' ;
    {$ENDIF}
{$ELSE}
    {$IFDEF UNICODE}
    LabelSslState.Caption := 'SSL Disabled, Unicode GUI' ;
    {$ELSE}
    LabelSslState.Caption := 'SSL Disabled, ASCII GUI' ;
    {$ENDIF}
{$ENDIF}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    PutSettings ;
    IcsLog.Free ;
end;

procedure TForm1.IcsLogEvent (Sender: TObject; LogOption: TLogOption;
                                       const Msg : String) ;
begin
    AddLogText (Msg) ;
end ;

procedure TForm1.onCopyEvent (LogLevel: TLogLevel ; Info: String ;
                                                  var Cancel: boolean) ;
begin
    if (LogLevel = LogLevelInfo) or (LogLevel = LogLevelFile) then
    begin
        AddLogText (Info) ;
        LabelProgress.Caption := Info ;
    end ;
    if (LogLevel = LogLevelProg) then
    begin
        if Info <> '' then LabelProgress.Caption := 'Progress: ' + Info ;
    end ;
    if (LogLevel = LogLevelDiag) and (ShowDiagsLow.Checked or
                             ShowDiagsHigh.Checked) then AddLogText (Info) ;
    if (LogLevel = LogLevelDelimFile) then AddLogDelim (Info) ;
    if (LogLevel = LogLevelDelimTot) then AddLogDelim (Info) ;
    if AbortFlag then Cancel := true ;
end ;

procedure TForm1.doCopyListClick(Sender: TObject);
begin
    AddLogText (DateTimeToAStr (Now)) ;
    AbortFlag := false ;
    MagFileCopyClient := TMagFileCopy.Create (self) ;
    doCopyFiles.Enabled := false ;
    doCopyCheck.Enabled := false ;
    doCopyList.Enabled := false ;
    doCopyAbort.Enabled := true ;
    try
        MagFileCopyClient.CopyEvent := onCopyEvent ;
        AddLogText (MagFileCopyClient.DispLocFiles (CopySrcDir.Text,
                                    CopySrcFile.Text, FCTypeMaskDir, true, false)) ;
    finally
        FreeAndNil (MagFileCopyClient) ;
        LabelProgress.Caption := 'List Completed' ;
        doCopyFiles.Enabled := true ;
        doCopyCheck.Enabled := true ;
        doCopyList.Enabled := true ;
        doCopyAbort.Enabled := false ;
    end ;
end;

procedure TForm1.CopyFiles(Sender: TObject);
var
    taskres: TTaskResult ;
    checkflag: boolean ;
begin
    AddLogText (DateTimeToAStr (Now)) ;
    AbortFlag := false ;
    MagFileCopyClient := TMagFileCopy.Create (self) ;
    doCopyFiles.Enabled := false ;
    doCopyCheck.Enabled := false ;
    doCopyList.Enabled := false ;
    doCopyAbort.Enabled := true ;
    try
        checkflag := false ;
        if Sender = doCopyCheck then checkflag := true ;
        try
            with MagFileCopyClient do
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
            // CopyType: FCTypeSingle, FCTypeMaskDir, FCTypeArchDir, FCTypeAllDir, FCTypeDates
                CopyType := FCTypeMaskDir ;
                SubDirs := true ;   // true copy all sub directories
                DelDone := false ;  // true delete source file after copy
                DelOldTar := false ; // true delete target files not in source directories
                Mask := false ;     // true, allow date/time masked characters in SrcFName
                Prev := false ;     // true, use yesterday's date for Mask
            // Repl: FCReplNever, FCReplAlways, FCReplDiff, FCReplNewer
                Repl := FCReplNever ;
                ReplRO := true ;    // true, replace read only files
                Safe := false ;     // true, copy file with TMP extension, rename when done
                LocalHost := OverbyteIcsWSocket.LocalHostName ;
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
                AddLogText ('Task Result: ' + GetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('Copying Error - ' + GetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (MagFileCopyClient) ;
        LabelProgress.Caption := 'Copy Completed' ;
        doCopyFiles.Enabled := true ;
        doCopyCheck.Enabled := true ;
        doCopyList.Enabled := true ;
        doCopyAbort.Enabled := false ;
    end ;
end;

procedure TForm1.DeleteFiles(Sender: TObject);
var
    taskres: TTaskResult ;
    checkflag: boolean ;
begin
    AddLogText (DateTimeToAStr (Now)) ;
    AbortFlag := false ;
    MagFileCopyClient := TMagFileCopy.Create (self) ;
    doDeleteFiles.Enabled := false ;
    doDeleteCheck.Enabled := false ;
    doCopyAbort.Enabled := true ;
    try
        checkflag := false ;
        if Sender = doDeleteCheck then checkflag := true ;
        try
            with MagFileCopyClient do
            begin
                SrcDir := CopyTarDir.Text ;
                SrcFName := CopySrcFile.Text ;
                MultiDir := false ; // true copy multiple specific directorie
                SubDirs := true ;   // true copy all sub directories
                Mask := false ;     // true, allow date/time masked characters in SrcFName
                Prev := false ;     // true, use yesterday's date for Mask
                CopyLoDT := 0 ;     // lower date range
                CopyHiDT := Now ;   // higher date range
                CopyEvent := onCopyEvent ;
                taskres := DeleteFiles (checkflag) ;   // main file delete function
                AddLogText ('Task Result: ' + GetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('Copying Error - ' + GetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (MagFileCopyClient) ;
        LabelProgress.Caption := 'Delete Completed' ;
        doDeleteFiles.Enabled := true ;
        doDeleteCheck.Enabled := true ;
        doCopyAbort.Enabled := false ;
    end ;
end;

procedure TForm1.onHttpEvent (LogLevel: TLogLevel ; Info: String ;
                                                  var Cancel: boolean) ;
begin
    if (LogLevel = LogLevelInfo) or (LogLevel = LogLevelFile) then
    begin
        AddLogText (Info) ;
        LabelProgress.Caption := Info ;
    end ;
    if (LogLevel = LogLevelProg) then LabelProgress.Caption := 'Progress: ' + Info ;
    if (LogLevel = LogLevelDiag) and (ShowDiagsLow.Checked or
                             ShowDiagsHigh.Checked) then AddLogText (Info) ;
    if (LogLevel = LogLevelDelimFile) then AddLogDelim (Info) ;
    if (LogLevel = LogLevelDelimTot) then AddLogDelim (Info) ;
    if AbortFlag then Cancel := true ;
end ;

procedure TForm1.HttpDownload(Sender: TObject);
var
    taskres: TTaskResult ;
    checkflag: boolean ;
const
    ReplaceList: array[0..2] of TFileCopyRepl = (FCReplNever, FCReplAlways, FCReplNewer) ;
begin
    AddLogText (DateTimeToAStr (Now)) ;
    AbortFlag := false ;
    doHttpDownCheck.Enabled := false ;
    doHttpDownFiles.Enabled := false ;
    doHttpAbort.Enabled := true ;
    if HttpSrcDir.Items.IndexOf (HttpSrcDir.Text) < 0 then
    begin
        if HttpSrcDir.Items.Count > 50 then
                HttpSrcDir.Items.Delete (HttpSrcDir.Items.Count - 1) ;
        HttpSrcDir.Items.Insert (0, HttpSrcDir.Text) ;
    end ;
    MagHTTPClient := TMagHTTP.Create (self) ;
    try
        try
            checkflag := false ;
            if Sender = doHttpDownCheck then checkflag := true ;
            with MagHTTPClient do
            begin
                URLList := HttpSrcDir.Text ;  // one or more source URLs separated by CRLF
                SrcMask := HttpSrcFile.Text ; // optional source file mask to restrict downloads
                DownDir := HttpTarDir.Text ;  // directory for downloaded files
                KeepPath := false ;           // if true, use HTTP path for subdirs
                KeepHost := false ;           // if true, use HTTP host for subdir
                ParseHTML := true ;           // if true, parse HTML page for links to files
                Repl := ReplaceList [2] ;     // replace options
                ReplRO := true ;              // replace read only files
  //              Timeout := 60 ;               // command timeout
                LogFiles := true ;            // log each file downloaded
                LogProt := ShowDiagsHigh.Checked ;// log HTTP protocol
                LogLDir := false ;            // log destination directory
                LogRDir := true ;             // log created HTTP directory
                Options := Options + [httpoEnableContentCoding] ;  // 27 Nov 2005
{$IFNDEF NO_DEBUG_LOG}
                IcsLogger := IcsLog ;
                IcsLog.LogOptions := [] ;
                if ShowDiagsLow.Checked then
                    IcsLog.LogOptions := [loDestEvent, loAddStamp] + LogAllOptInfo ; // 3 Jan 2006
{$IFDEF USE_SSL}
                if ShowDiagsSSL.Checked then
                    IcsLog.LogOptions := IcsLog.LogOptions + [loSslDump] ;
{$ENDIF}
{$ENDIF}
{$IFDEF USE_SSL}
                SslSessCache := true ;  // 27 Nov 2005
{$ENDIF}
                CopyEvent := onHttpEvent ;
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
                AddLogText ('Task Result: ' + GetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('Download Error - ' + GetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (MagHTTPClient) ;
        LabelProgress.Caption := 'HTTP Completed' ;
        doHttpDownCheck.Enabled := true ;
        doHttpDownFiles.Enabled := true ;
        doHttpAbort.Enabled := false ;
    end ;
end;

procedure TForm1.onFtpEvent (LogLevel: TLogLevel ; Info: String ;
                                                  var Cancel: boolean) ;
begin
    if (LogLevel = LogLevelInfo) or (LogLevel = LogLevelFile) then
    begin
        AddLogText (Info) ;
        LabelProgress.Caption := Info ;
    end ;
    if (LogLevel = LogLevelProg) then
    begin
        LabelProgress.Caption := 'Progress: ' + Info ;
//        AddLogText (Info) ; // TEMP !!!!
    end ;
    if (LogLevel = LogLevelDiag) and (ShowDiagsLow.Checked or
                             ShowDiagsHigh.Checked) then AddLogText (Info) ;
    if (LogLevel = LogLevelDelimFile) then AddLogDelim (Info) ;
    if (LogLevel = LogLevelDelimTot) then AddLogDelim (Info) ;
    if AbortFlag then Cancel := true ;
end ;

function TForm1.SetFTPGen: boolean ;
var
    bandwidth: integer ;
begin
    AddLogText (DateTimeToAStr (Now)) ;
    AbortFlag := false ;
    result := false ;
    if FtpHost.Items.IndexOf (FtpHost.Text) < 0 then
    begin
        if FtpHost.Items.Count > 50 then
                FtpHost.Items.Delete (FtpHost.Items.Count - 1) ;
        FtpHost.Items.Insert (0, FtpHost.Text) ;
    end ;
    try
        with MagFTPClient do
        begin
            LocalHost := OverbyteIcsWSocket.LocalHostName ;
            HostName1 := FtpHost.Text ;
            HostName2 := '' ;
            UserName := FtpUsername.Text ;
            PassWord := FtpPassword.Text ;
            FtpType := TFtpType (FtpServerType.ItemIndex) ;
            Port := FtpPort.Text ;
            AttemptDelay := 5 ;
            MaxAttempts := 2 ;  // logon attempts
            FailRepeat := 3 ;   // retries for failed xfers
            KeepAliveSecs := AscToInt (FtpKeepAlive.Text) ;  // 10 July 2006
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
            CopyEvent := onFtpEvent ;
            UpArchDir := '' ;
            UpArchive := false ;
            ResFailed := true ;
            UseCompression := true ;  // 3 Dec 2005
            if FtpNoFeatCmd.Checked then // 7 Nov 2007
                MagFtpOpts := MagFtpOpts + [magftpNoFeat]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoFeat] ;
            if FtpNoMd5Crc.Checked then // 5 Jan 2008
                MagFtpOpts := MagFtpOpts + [magftpNoMd5Crc]
            else
                MagFtpOpts := MagFtpOpts - [magftpNoMd5Crc] ;
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

       // 16 Apr 2009 disable MD5 or CRC separately for testing
        //  MagFtpOpts := MagFtpOpts + [magftpNoCrc] ;
        //  MagFtpOpts := MagFtpOpts + [magftpNoMd5] ;

            ZlibNoCompExt := '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;'; // 2 Dec 2007
            ZlibMaxSize := 500000000 ;   // 9 Dec 2007 500 megs
            MaxResumeAttempts := 10 ;    // 31 Dec 2007
            bandwidth := AscToInt (FtpBandWidth.Text) ;  // 31 Dec 2007
            if bandwidth > 0 then
            begin
                BandwidthLimit := bandwidth * KBYTE ;
                Options := Options + [ftpBandwidthControl] ;
            end
            else
                Options := Options - [ftpBandwidthControl] ;
{$IFNDEF NO_DEBUG_LOG}
            IcsLogger := IcsLog ;
            IcsLog.LogOptions := [] ;
            if ShowDiagsLow.Checked then
                IcsLog.LogOptions := [loDestEvent, loAddStamp] +
                                                 LogAllOptInfo ; // 7 Jan 2006
{$IFDEF USE_SSL}
            if ShowDiagsSSL.Checked then
                    IcsLog.LogOptions := IcsLog.LogOptions + [loSslDump] ;
{$ENDIF}
{$ENDIF}
{$IFDEF USE_SSL}
            FtpSslPort := FtpPortSsl.Text ;
            SslSessCache := true ;  // 27 Nov 2005
{$ENDIF}
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
        AddLogText ('FTP Error - ' + GetExceptMess (ExceptObject)) ;
        result := true ;
    end ;
end;

procedure TForm1.SetFtpButtons (value: boolean) ;
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

procedure TForm1.doFtpListClick(Sender: TObject);
var
    dirlisting: String ;
    taskres: TTaskResult ;
begin
    AbortFlag := false ;
    SetFtpButtons (false) ;
    MagFTPClient := TMagFtp.Create (self) ;
    try
        if SetFTPGen then exit ;
        try
            with MagFTPClient do
            begin
                BulkMode := BulkModeDownload ;
                SrcDir := FtpPath.Text ;
                SubDirs := FtpSubdirs.Checked ;
                taskres := DispFtpDir (dirlisting) ;  // main FTP function
                AddLogText ('Task Result: ' + GetTaskResName (taskres)) ;
                AddLogText (dirlisting) ;
            end ;
        except
            AddLogText ('FTP Error - ' + GetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (MagFTPClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        SetFtpButtons (true) ;
    end ;
end;

procedure TForm1.FtpDownload(Sender: TObject);
var
    checkflag: boolean ;
    taskres: TTaskResult ;
begin
    AbortFlag := false ;
    SetFtpButtons (false) ;
    MagFTPClient := TMagFtp.Create (self) ;
    try
        if SetFTPGen then exit ;
        try
            checkflag := false ;
            if Sender = doFtpDownCheck then checkflag := true ;
            with MagFTPClient do
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
                SrcFName := FtpSrcFile.Text ;  // 8 Apr 2009 - may include masked directories if Mask=true, and MaskLocDir and/or MaskRemDir
                Mask := false ;     // true, allow date/time masked characters and directories in SrcFName
                Prev := false ;     // true, use yesterday's date for Mask
                MaskLocDir := false ; // 8 Apr 2009 - true use masked directories from SrcFName
                MaskRemDir := false ; // 8 Apr 2009 - true use masked directories from SrcFName
            // Repl: FCReplNever, FCReplAlways, FCReplDiff, FCReplNewer
                Repl := FCReplNever ;
                if FtpReplace.Checked then Repl := FCReplAlways ;
                ReplRO := true ;    // true, replace read only files
                Safe := false ;     // true, copy file with TMP extension, rename when done
                IgnoreFileExt := 'tmp;ftp' ;
                taskres := FtpDownload (checkflag) ;   // main FTP function
                AddLogText ('Task Result: ' + GetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('FTP Error - ' + GetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (MagFTPClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        SetFtpButtons (true) ;
    end ;
end;


procedure TForm1.FtpUpload(Sender: TObject);
var
    checkflag: boolean ;
    taskres: TTaskResult ;
begin
    AbortFlag := false ;
    SetFtpButtons (false) ;
    MagFTPClient := TMagFtp.Create (self) ;
    try
        if SetFTPGen then exit ;
        try
            checkflag := false ;
            if Sender = doFtpUpCheck then checkflag := true ;
            with MagFTPClient do
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
                SrcFName := FtpSrcFile.Text ;  // 8 Apr 2009 - may include masked directories if Mask=true, and MaskLocDir and/or MaskRemDir
                Mask := false ;     // true, allow date/time masked characters and directories in SrcFName
                Prev := false ;     // true, use yesterday's date for Mask
                MaskLocDir := false ; // 8 Apr 2009 - true use masked directories from SrcFName
                MaskRemDir := false ; // 8 Apr 2009 - true use masked directories from SrcFName
            // Repl: FCReplNever, FCReplAlways, FCReplDiff, FCReplNewer
                Repl := FCReplNewer ;
                if FtpReplace.Checked then Repl := FCReplAlways ;
                Safe := true ;     // ignored
                IgnoreFileExt := 'tmp' ;
                UpImmed := false ;   // immediate move/delete after upload
                TimeStamp := false ; // update local file time stamp to match remote
                taskres := FtpUpload (checkflag) ;   // main FTP function
                AddLogText ('Task Result: ' + GetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('FTP Error - ' + GetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (MagFTPClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        SetFtpButtons (true) ;
    end ;
end;

procedure TForm1.doFtpDown1Click(Sender: TObject);
var
    taskres: TTaskResult ;
    Replace: TFileCopyRepl ;
begin
    AbortFlag := false ;
    SetFtpButtons (false) ;
    MagFTPClient := TMagFtp.Create (self) ;
    try
        if SetFTPGen then exit ;
        try
            with MagFTPClient do
            begin
                SrcDir := '/' ;
                BulkMode := BulkModeDownload ;
            //  DelFile := FtpDelDone.Checked ;    pending TMagFtp fix
                Replace := FCReplNewer ;
                if FtpOneReplace.Checked then Replace := FCReplAlways ;
                taskres := FtpLogon ;
                if taskres = TaskResOKNew then
                begin
                    taskres := FtpDownOneFile (Ftp1Path.Text, Ftp1SrcName.Text,
                            Ftp1LocDir.Text + '\' + Ftp1SrcName.Text, Replace) ;
                end ;
                AddLogText ('Task Result: ' + GetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('FTP Error - ' + GetExceptMess (ExceptObject)) ;
        end ;
    finally
        MagFTPClient.FtpLogoff ;
        FreeAndNil (MagFTPClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        SetFtpButtons (true) ;
    end ;
end;

procedure TForm1.doFtpUp1Click(Sender: TObject);
var
    taskres: TTaskResult ;
    Replace: TFileCopyRepl ;
begin
    AbortFlag := false ;
    SetFtpButtons (false) ;
    MagFTPClient := TMagFtp.Create (self) ;
    try
        if SetFTPGen then exit ;
        try
            with MagFTPClient do
            begin
                TarDir := '/' ;
                BulkMode := BulkModeUpload ;
            //  DelFile := FtpDelDone.Checked ;    pending TMagFtp fix
                Replace := FCReplNewer ;
                if FtpOneReplace.Checked then Replace := FCReplAlways ;
                taskres := FtpLogon ;
                if taskres = TaskResOKNew then
                begin
                    taskres := FtpUpOneFile (Ftp1UpFile.Text, Ftp1Path.Text,
                                                  Ftp1SrcName.Text, Replace) ;
                end ;
                AddLogText ('Task Result: ' + GetTaskResName (taskres)) ;
                AddLogText (ReqResponse) ;
            end ;
        except
            AddLogText ('FTP Error - ' + GetExceptMess (ExceptObject)) ;
        end ;
    finally
        MagFTPClient.FtpLogoff ;
        FreeAndNil (MagFTPClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        SetFtpButtons (true) ;
    end ;
end;

procedure TForm1.doTestClick(Sender: TObject);
{var
    taskres: TTaskResult ;
    fname: string ;
    actualDT, newDT: TDateTime ;
    FSize: Int64 ;}
begin
{    fname := 'd:\temp1\testfile.tst' ;
    newDT := Packed2Date ('20040102-003456') ;
    if NOT UpdateUFileAge (fname, newDT) then
    begin
        AddLogText ('Failed to update File Age: ' + fname) ;
        exit ;
    end ;
    GetUAgeSizeFile (fname, actualDT, FSize) ;
    if Abs (actualDT - newDT) <= (OneSecond * 2) then
        AddLogText ('Update File Age OK: ' + fname)
    else
        AddLogText ('File Age Incorrect for: ' + fname) ;
    AddLogText ('New Age: ' + DateTimetoStr (newDT) +
                                    ', Actual: ' + DateTimetoStr (actualDT)) ;

// test install download
    MagFTPClient := TMagFtp3.Create (self) ;
    try
        with MagFTPClient do
        begin
            LocalHost := WSocket.LocalHostName ;
            HostName1 := '192.168.1.63' ;
            HostName2 := '' ;
            UserName := 'angus' ;
            PassWord := 'fgh' ;
            Port := '21' ;
            AttemptDelay := 5 ;
            MaxAttempts := 2 ;
            FailRepeat := 1 ;  // retries for failure
       // ConnectionType: ftpDirect, ftpProxy, ftpSocks4, ftpSocks4A, ftpSocks5
            ConnectionType := ftpDirect ;
            SocksPort := '' ;
            SocksServer := '' ;
            ProxyPort := '' ;
            ProxyServer := '' ;
            SocksUsercode := '' ;
            SocksPassword := '' ;
            PassiveX := false ;  // must be after connection type
       // HostType: FTPTYPE_NONE, FTPTYPE_UNIX, FTPTYPE_DOS, FTPTYPE_MVS, FTPTYPE_AS400, FTPTYPE_MLSD
            HostType := FTPTYPE_NONE ;
       // TXferMode: XferModeBinary, XferModeAscii
            XferMode := XferModeBinary ;
       // TCaseFile: FileLowerCase, FileMixedCase
            CaseFile := FileLowerCase ;
            DiffStampMins := 2 ;
            Timeout := 60 ;
            DispLog := true ;
            DispFiles := true ;
            DispRDir:= true ;
            DispLDir:= true ;
            CopyEvent := onFtpEvent ;
            UpArchDir := '' ;
            UpArchive := false ;
            ResFailed := true ;
            DispRemList := true ;
            BulkMode := BulkModeDownload ;
            SrcDir := '/download/alldepots/install' ;
            SrcFName := '*.zip' ;
            TarDir := 'e:\temp1\install' ;
            CopyType := FCTypeAllDir ;
            DelDone := false ;
            DelOldTar := false ;
            SubDirs := true ;   // true copy all sub directories
            Mask := false ;     // true, allow date/time masked characters in SrcFName
            Prev := false ;     // true, use yesterday's date for Mask
        // Repl: FCReplNever, FCReplAlways, FCReplDiff, FCReplNewer
            Repl := FCReplDiff ;
            ReplRO := true ;    // true, replace read only files
            Safe := false ;     // true, copy file with TMP extension, rename when done
            IgnoreFileExt := 'tmp;ftp' ;
            taskres := FtpDownload (false) ;   // main FTP function
            AddLogText ('Task Result: ' + GetTaskResName (taskres)) ;
            AddLogText (ReqResponse) ;
        end ;
    except
        AddLogText ('FTP Error - ' + GetExceptMess (ExceptObject)) ;
    end ;
    FreeAndNil (MagFTPClient) ; }
end;

procedure TForm1.doClearClick(Sender: TObject);
begin
    LogDelim.Lines.Clear ;
    LogText.Lines.Clear ;
end;

end.
