{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Aug 1997
Version:      8.01
Object:       Demo for TFtpClient object (RFC 959 implementation)
              It is a graphical FTP client program
              Compatible with Delphi 1, 2, 3, 4 and 5
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2017 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

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

Updates:
Sep 13, 97    Added directory functions. Added button to show how to makes
              several transferts in one session
Sep 27, 97    Change identifiers names to be more standard with other sources
Jan 10, 98    Saved edit boxes content to an IniFile, added FileSize, Quote
              and RestartGet commands
Jan 25, 1998  Completely rewritten for new component version (Asynchronous)
Feb 02, 1998  V2.17 Added a checkbox to run the synchronous or asynchronous
              version of the component methods.
Feb 15, 1998  V2.18 Removed useless wait unit from the use clause.
              Added display of winsock information at startup.
Feb 22, 1998  V2.19 Added Append and AppendFile commands
Aug 21, 1998  V2.20 Added a comment in OnProgress event handler to warn user
              about CPU usage.
Dec 22, 1998  V2.21 Replaced DisplayFlag by DysplayFileFlag.
Oct 19, 1999  V2.22 Correctly display Winsock version
Nov 24, 1999  V2.23 Added Restart Put and NoAutoResumeAt.
Jan 28, 2000  V2.24 Added code to OnProgress event handler to update screen
              only once per second. This solve problem on fast LAN.
Jul 21, 2000  V2.25 Added two button to show TSream usage instead of files.
May 20, 2004  V2.26 Added FTPS support (require ICS-SSL to work)
Aug 24, 2004  V2.27 Fixed AuthSslButtonClick to automatically check the
              AuthSsl checkbox so that subsequent commands correctly request
              SSL for everything.
Oct 31, 2004  V2.28 Added account button
Nov 02, 2004  V2.29 Fixed problem displaying dir list when "Display Data"
              checkbox checked. Added persistance for all checkboxes.
Dec 19, 2004  V2.30 Added Trim() and similar for Delphi 1 compatibility.
Apr 16, 2009  V7.07 Angus assume STREAM64, USE_ONPROGRESS64_ONLY, removed OnProgress
              Removed local GetFileSize using IcsGetFileSize instead
Dec 9, 2014   V8.00 Angus added SslHandshakeRespMsg for better error handling
Apr 15, 2017  V8.01 FPiette removed compiler warnings for D10.2

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslFtpTst1;

{$I Include\OverbyteIcsDefs.inc}
{$IFDEF DELPHI25_UP}
   {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF USE_SSL}
  {$MESSAGE FATAL 'Define conditional define "USE_SSL" in the project options'};
{$ENDIF}
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, OverbyteIcsIniFiles, ExtCtrls, OverbyteIcsWinSock,
  OverbyteIcsWSocket, OverbyteIcsSSLEAY, OverbyteIcsLIBEAY, OverbyteIcsLogger,
  OverbyteIcsWndControl, OverbyteIcsFtpCli, OverbyteIcsFtpSrvT, OverByteIcsUtils;

const
  FTPTstVersion      = 800;
  CopyRight : String = ' SslFtpTst (c) 1997-2014 F. Piette V8.00 ';
  WM_SSL_NOT_TRUSTED = WM_USER + 1;
  WM_TMP_SOMETHING   = WM_USER + 2;
type
  TSyncCmd  = function : Boolean  of object;
  TAsyncCmd = procedure of object;

  TFtpReceiveForm = class(TForm)
    DisplayMemo: TMemo;
    FtpClient1: TSslFtpClient;
    Panel1: TPanel;
    ExitButton: TButton;
    OpenAsyncButton: TButton;
    QuitAsyncButton: TButton;
    CwdAsyncButton: TButton;
    UserAsyncButton: TButton;
    PassAsyncButton: TButton;
    ConnectAsyncButton: TButton;
    GetAsyncButton: TButton;
    ReceiveAsyncButton: TButton;
    AbortAsyncButton: TButton;
    DirAsyncButton: TButton;
    DirectoryAsyncButton: TButton;
    LsAsyncButton: TButton;
    ListAsyncButton: TButton;
    SystAsyncButton: TButton;
    SystemAsyncButton: TButton;
    FileSizeAsyncButton: TButton;
    SizeAsyncButton: TButton;
    MkdAsyncButton: TButton;
    MkdirAsyncButton: TButton;
    RmdAsyncButton: TButton;
    RmdirAsyncButton: TButton;
    RenAsyncButton: TButton;
    RenameAsyncButton: TButton;
    DeleAsyncButton: TButton;
    DeleteAsyncButton: TButton;
    PwdAsyncButton: TButton;
    QuoteAsyncButton: TButton;
    DoQuoteAsyncButton: TButton;
    PutAsyncButton: TButton;
    TransmitAsyncButton: TButton;
    TypeSetAsyncButton: TButton;
    RestGetAsyncButton: TButton;
    RestartGetAsyncButton: TButton;
    CDupAsyncButton: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    HostNameEdit: TEdit;
    HostFileEdit: TEdit;
    UserNameEdit: TEdit;
    PassWordEdit: TEdit;
    DisplayCheckBox: TCheckBox;
    LocalFileEdit: TEdit;
    BinaryCheckBox: TCheckBox;
    HostDirEdit: TEdit;
    PortEdit: TEdit;
    InfoLabel: TLabel;
    StateLabel: TLabel;
    ClearButton: TButton;
    SyncCheckBox: TCheckBox;
    AppendFileAsyncButton: TButton;
    AppendAsyncButton: TButton;
    PassiveCheckBox: TCheckBox;
    Button1: TButton;
    RestPutAsyncButton: TButton;
    RestartPutAsyncButton: TButton;
    ResumeAtEdit: TEdit;
    Label7: TLabel;
    NoAutoResumeAtCheckBox: TCheckBox;
    TransmitUsingStreamButton: TButton;
    ReceiveUsingStreamButton: TButton;
    StressPutButton: TButton;
    AbortXferAsyncButton: TButton;
    Label8: TLabel;
    DataPortRangeStartEdit: TEdit;
    DataPortRangeEndEdit: TEdit;
    Label9: TLabel;
    AuthButton: TButton;
    AcctAsyncButton: TButton;
    Label10: TLabel;
    AccountEdit: TEdit;
    Label11: TLabel;
    CertFileEdit: TEdit;
    Label12: TLabel;
    CAFileEdit: TEdit;
    CAPathEdit: TEdit;
    PrivKeyFileEdit: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    PassPhraseEdit: TEdit;
    VerifyPeerCheckBox: TCheckBox;
    Label15: TLabel;
    Label16: TLabel;
    AcceptableHostsEdit: TEdit;
    ProtButton: TButton;
    PbszButton: TButton;
    Label17: TLabel;
    Label18: TLabel;
    PbszSizeEdit: TEdit;
    ProtLevelEdit: TEdit;
    SslContext1: TSslContext;
    FeatButton: TButton;
    SessCacheCheckBox: TCheckBox;
    SslTypeComboBox: TComboBox;
    Label19: TLabel;
    Label20: TLabel;
    SslPortEdit: TEdit;
    Label21: TLabel;
    IcsLogger1: TIcsLogger;
    SslRenegotiateButton: TButton;
    CccButton: TButton;
    OptsEdit: TComboBox;
    ClearTraceButton: TButton;
    Label23: TLabel;
    OptsAsyncButton: TButton;
    MaxKbEdit: TEdit;
    Label22: TLabel;
    procedure ExitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DisplayHandler(Sender: TObject; var Msg : String);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OpenAsyncButtonClick(Sender: TObject);
    procedure FtpClient1RequestDone(Sender: TObject; RqType: TFtpRequest;
      Error: Word);
    procedure FtpClient1SessionConnected(Sender: TObject; Error: Word);
    procedure FtpClient1SessionClosed(Sender: TObject; Error: Word);
    procedure QuitAsyncButtonClick(Sender: TObject);
    procedure CwdAsyncButtonClick(Sender: TObject);
    procedure UserAsyncButtonClick(Sender: TObject);
    procedure PassAsyncButtonClick(Sender: TObject);
    procedure ConnectAsyncButtonClick(Sender: TObject);
    procedure FtpClient1StateChange(Sender: TObject);
    procedure GetAsyncButtonClick(Sender: TObject);
    procedure ReceiveAsyncButtonClick(Sender: TObject);
    procedure AbortAsyncButtonClick(Sender: TObject);
    procedure DirAsyncButtonClick(Sender: TObject);
    procedure DirectoryAsyncButtonClick(Sender: TObject);
    procedure LsAsyncButtonClick(Sender: TObject);
    procedure ListAsyncButtonClick(Sender: TObject);
    procedure SystAsyncButtonClick(Sender: TObject);
    procedure SystemAsyncButtonClick(Sender: TObject);
    procedure FileSizeAsyncButtonClick(Sender: TObject);
    procedure SizeAsyncButtonClick(Sender: TObject);
    procedure MkdAsyncButtonClick(Sender: TObject);
    procedure MkdirAsyncButtonClick(Sender: TObject);
    procedure RmdAsyncButtonClick(Sender: TObject);
    procedure RmdirAsyncButtonClick(Sender: TObject);
    procedure RenAsyncButtonClick(Sender: TObject);
    procedure RenameAsyncButtonClick(Sender: TObject);
    procedure DeleAsyncButtonClick(Sender: TObject);
    procedure DeleteAsyncButtonClick(Sender: TObject);
    procedure PwdAsyncButtonClick(Sender: TObject);
    procedure QuoteAsyncButtonClick(Sender: TObject);
    procedure DoQuoteAsyncButtonClick(Sender: TObject);
    procedure PutAsyncButtonClick(Sender: TObject);
    procedure TransmitAsyncButtonClick(Sender: TObject);
    procedure TypeSetAsyncButtonClick(Sender: TObject);
    procedure RestGetAsyncButtonClick(Sender: TObject);
    procedure RestartGetAsyncButtonClick(Sender: TObject);
    procedure CDupAsyncButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure AppendAsyncButtonClick(Sender: TObject);
    procedure AppendFileAsyncButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RestPutAsyncButtonClick(Sender: TObject);
    procedure RestartPutAsyncButtonClick(Sender: TObject);
    procedure TransmitUsingStreamButtonClick(Sender: TObject);
    procedure ReceiveUsingStreamButtonClick(Sender: TObject);
    procedure StressPutButtonClick(Sender: TObject);
    procedure AbortXferAsyncButtonClick(Sender: TObject);
    procedure AcctAsyncButtonClick(Sender: TObject);
    procedure FtpClient1DisplayFile(Sender: TObject; var Msg: String);
    procedure FtpClient1SslVerifyPeer(Sender: TObject; var Ok: Integer;
      Cert: TX509Base);
    procedure FormDestroy(Sender: TObject);
    procedure ProtButtonClick(Sender: TObject);
    procedure PbszButtonClick(Sender: TObject);
    procedure ClearTraceButtonClick(Sender: TObject);
    procedure FtpClient1SslCliNewSession(Sender: TObject;
      SslSession: Pointer; WasReused: Boolean; var IncRefCount: Boolean);
    procedure FtpClient1SslCliGetSession(Sender: TObject;
      var SslSession: Pointer; var FreeSession: Boolean);
    procedure FeatButtonClick(Sender: TObject);
    procedure FtpClient1SslCliCertRequest(Sender: TObject;
      var Cert: TX509Base);
    procedure AuthButtonClick(Sender: TObject);
    procedure SslRenegotiateButtonClick(Sender: TObject);
    procedure CccButtonClick(Sender: TObject);
    procedure OptsAsyncButtonClick(Sender: TObject);
    procedure FtpClient1Progress64(Sender: TObject; Count: Int64; var Abort: Boolean);
    procedure FtpClient1SslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
  private
    FIniFileName   : String;
    FInitialized   : Boolean;
    FLastProgress  : Int64;
    FProgressCount : LongInt;
    FRunning       : Boolean;
    FTrustedList   : TStringList;
    FNotTrusted    : String;
    FQueryingTrusted : Boolean;
    FCachedSession : Pointer;
    FClientCerts   : TX509List;
    procedure WMSslNotTrusted(var Msg: TMessage); message WM_SSL_NOT_TRUSTED;
    procedure DisplayFile(FileName : String);
    procedure ExecuteCmd(SyncCmd : TSyncCmd; ASyncCmd : TAsyncCmd);
    function  SendFile : Boolean;
    procedure Display(const Msg: String);
  end;

const
  TEMP_FILE_NAME = 'FTPDIR.TXT';

var
  FtpReceiveForm: TFtpReceiveForm;

implementation

uses
  OverbyteIcsSslFtpTst2, OverbyteIcsCliCertDlg;

{$R *.DFM}
const
    SectionData        = 'Data';
    KeyHostName        = 'HostName';
    KeyUserName        = 'UserName';
    KeyPassWord        = 'PassWord';
    KeyAccount         = 'Account';
    KeyHostDir         = 'HostDir';
    KeyPort            = 'Port';
    KeyHostFile        = 'HostFile';
    KeyLocalFile       = 'LocalFile';
    KeyResumeAt        = 'ResumeAt';
    KeyPortStart       = 'DataPortRangeStart';
    KeyPortEnd         = 'DataPortRangeEnd';
    KeyPassive         = 'PassiveMode';
    KeyAuthSSL         = 'AuthSSLEnabled';
    KeyDisplay         = 'DisplayData';
    KeySync            = 'SyncMode';
    KeyNoResume        = 'NoAutoResumeAt';
    KeyBinary          = 'BinaryMode';
    KeyCertFile        = 'CertFile';
    KeyPassPhrase      = 'PassPhrase';
    KeyPrivKeyFile     = 'PrivKeyFile';
    KeyCAFile          = 'CAFile';
    KeyCAPath          = 'CAPath';
    KeyVerifyPeer      = 'VerifyPeer';
    KeyAcceptableHosts = 'AcceptableHosts';
    KeyProtLevel       = 'ProtLevel';
    KeyPbszSize        = 'PbszSize';
    SectionWindow      = 'Window';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    KeySessCache       = 'SessCache';
    KeySslType         = 'SslType';
    KeySslPort         = 'SslPort';
    KeyMaxKB           = 'MaxKB';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF VER80 }
{ To debug event driven programs, it is often handy to just use writeln to  }
{ write debug messages to the console. To get a console, just ask the       }
{ linker to build a console mode application. Then you'll get the default   }
{ console. The function below will make it the size you like...             }
procedure BigConsole(nCols, nLines : Integer);
var
    sc : TCoord;
    N  : DWord;
begin
    if not IsConsole then
        Exit;
    sc.x := nCols;
    sc.y := nLines;
    SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), sc);
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
                            BACKGROUND_BLUE or BACKGROUND_GREEN or
                            BACKGROUND_RED or BACKGROUND_INTENSITY);
    sc.x := 0;
    sc.y := 0;
    FillConsoleOutputAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
                               BACKGROUND_BLUE or BACKGROUND_GREEN or
                               BACKGROUND_RED or BACKGROUND_INTENSITY,
                               nCols * nLines, sc, N);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FormCreate(Sender: TObject);
begin
{$IFNDEF VER80}
    BigConsole(80, 100);
{$ENDIF}
{$IFDEF DELPHI10}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
    DisplayMemo.Clear;
    InfoLabel.Caption  := '';
    StateLabel.Caption := '';
    FIniFileName    := GetIcsIniFileName;
    FTrustedList    := TStringList.Create;
    FClientCerts    := nil;
    FCachedSession  := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FormDestroy(Sender: TObject);
begin
    FreeAndNil(FTrustedList);
    FreeAndNil(FClientCerts);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
    Data    : TWSAData;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIcsIniFile.Create(FIniFileName);
        HostNameEdit.Text  := IniFile.ReadString(SectionData, KeyHostName,
                                                 'ftp.simtel.net');
        PortEdit.Text      := IniFile.ReadString(SectionData, KeyPort,
                                                 'ftp');
        UserNameEdit.Text  := IniFile.ReadString(SectionData, KeyUserName,
                                                 'anonymous');
        PassWordEdit.Text  := IniFile.ReadString(SectionData, KeyPassWord,
                                                 'your.name@your.company.com');
        AccountEdit.Text   := IniFile.ReadString(SectionData, KeyAccount,
                                                 '');
        HostDirEdit.Text   := IniFile.ReadString(SectionData, KeyHostDir,
                                                 '/pub/simtelnet');
        HostFileEdit.Text  := IniFile.ReadString(SectionData, KeyHostFile,
                                                 'index.html');
        LocalFileEdit.Text := IniFile.ReadString(SectionData, KeyLocalFile,
                                                 'c:\temp\index.htm');
        ResumeAtEdit.Text  := IniFile.ReadString(SectionData, KeyResumeAt,
                                                 '0');
        DataPortRangeStartEdit.Text :=
                              IniFile.ReadString(SectionData, KeyPortStart,
                                                 '0');
        DataPortRangeEndEdit.Text :=
                              IniFile.ReadString(SectionData, KeyPortEnd,
                                                 '0');
        PassiveCheckBox.Checked        := Boolean(IniFile.ReadInteger(SectionData, KeyPassive , 0));
        DisplayCheckBox.Checked        := Boolean(IniFile.ReadInteger(SectionData, KeyDisplay , 0));
        SyncCheckBox.Checked           := Boolean(IniFile.ReadInteger(SectionData, KeySync    , 0));
        NoAutoResumeAtCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData, KeyNoResume, 0));
        BinaryCheckBox.Checked         := Boolean(IniFile.ReadInteger(SectionData, KeyBinary,   0));
        MaxKbEdit.Text                 := IniFile.ReadString(SectionData, KeyMaxKB, '0');

        Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    (Screen.Height - Height) div 2);
        Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   (Screen.Width - Width) div 2);

        CertFileEdit.Text    := IniFile.ReadString(SectionData, KeyCertFile,
                                                   '01cert.pem');
        PrivKeyFileEdit.Text := IniFile.ReadString(SectionData, KeyPrivKeyFile,
                                                   '01key.pem');
        PassPhraseEdit.Text  := IniFile.ReadString(SectionData, KeyPassPhrase,
                                                   'password');
        CAFileEdit.Text      := IniFile.ReadString(SectionData, KeyCAFile,
                                                   'TrustedCABundle.pem');
        CAPathEdit.Text      := IniFile.ReadString(SectionData, KeyCAPath,
                                                   'TrustedCaStore');
        AcceptableHostsEdit.Text := IniFile.ReadString(SectionData, KeyAcceptableHosts,
                                                       'www.overbyte.be;www.borland.com');
        VerifyPeerCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData,
                                                                  KeyVerifyPeer,
                                                                  0));
        ProtLevelEdit.Text := IniFile.ReadString(SectionData, KeyProtLevel,
                                                   'P');
        PbszSizeEdit.Text  := IniFile.ReadString(SectionData, KeyPbszSize,
                                                   '0');
        SessCacheCheckBox.Checked := IniFile.ReadBool(SectionData,
                                                      KeySessCache, False);
        SslTypeCombobox.ItemIndex := IniFile.ReadInteger(SectionData,
                                                         KeySslType, 0);
        SslPortEdit.Text := IniFile.ReadString(SectionData, KeySslPort, '990');
        IniFile.Free;

        { Display winsock info }
        Data := WinsockInfo;
        Display('Winsock version ' +
                IntToStr(LOBYTE(Data.wHighVersion)) + '.' +
                IntToStr(HIBYTE(Data.wHighVersion)));
        Display(String(StrPas(Data.szDescription)));
        Display(String(StrPas(Data.szSystemStatus)));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyHostName,  HostNameEdit.Text);
    IniFile.WriteString(SectionData, KeyPort,      PortEdit.Text);
    IniFile.WriteString(SectionData, KeyUserName,  UserNameEdit.Text);
    IniFile.WriteString(SectionData, KeyPassWord,  PassWordEdit.Text);
    IniFile.WriteString(SectionData, KeyAccount,   AccountEdit.Text);
    IniFile.WriteString(SectionData, KeyHostDir,   HostDirEdit.Text);
    IniFile.WriteString(SectionData, KeyHostFile,  HostFileEdit.Text);
    IniFile.WriteString(SectionData, KeyLocalFile, LocalFileEdit.Text);
    IniFile.WriteString(SectionData, KeyResumeAt,  ResumeAtEdit.Text);
    IniFile.WriteString(SectionData, KeyPortStart, DataPortRangeStartEdit.Text);
    IniFile.WriteString(SectionData, KeyPortEnd,   DataPortRangeEndEdit.Text);
    IniFile.WriteInteger(SectionData, KeyPassive , Ord(PassiveCheckBox.Checked));
    IniFile.WriteInteger(SectionData, KeyDisplay , Ord(DisplayCheckBox.Checked));
    IniFile.WriteInteger(SectionData, KeySync    , Ord(SyncCheckBox.Checked   ));
    IniFile.WriteInteger(SectionData, KeyNoResume, Ord(NoAutoResumeAtCheckBox.Checked));
    IniFile.WriteInteger(SectionData, KeyBinary,   Ord(BinaryCheckBox.Checked));
    IniFile.WriteString(SectionData,  KeyMaxKB,    MaxKbEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyVerifyPeer,  Ord(VerifyPeerCheckBox.Checked));
    IniFile.WriteString(SectionData,    KeyCertFile,    CertFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPrivKeyFile, PrivKeyFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPassPhrase,  PassPhraseEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAFile,      CAFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAPath,      CAPathEdit.Text);
    IniFile.WriteString(SectionData,    KeyAcceptableHosts, AcceptableHostsEdit.Text);
    IniFile.WriteString(SectionData,    KeyProtLevel, ProtLevelEdit.Text);
    IniFile.WriteString(SectionData,    KeyPbszSize,  PbszSizeEdit.Text);
    IniFile.WriteBool(SectionData,      KeySessCache, SessCacheCheckBox.Checked);
    IniFile.WriteInteger(SectionData,   KeySslType, SslTypeCombobox.ItemIndex);
    IniFile.WriteString(SectionData,    KeySslPort, SslPortEdit.Text);

    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function DeleteFile(const FileName: string): Boolean;
var
   F : File of char;
begin
    Result := TRUE;
    try
        AssignFile(F, FileName);
        Erase(F);
    except
        Result := FALSE;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.Display(const Msg : String);
var
    I : Integer;
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            for I := 1 to 50 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        {$IFNDEF VER80}
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
        {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DisplayHandler(Sender : TObject; var Msg : String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ExitButtonClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1Progress64(Sender: TObject; Count: Int64; var Abort: Boolean);
begin
    FProgressCount := Count;
    { Be sure to update screen only once every second }
    if FLastProgress < GetTickCount then begin
        FLastProgress := GetTickCount + 1000;
        InfoLabel.Caption := IntToStr(FProgressCount);
        InfoLabel.Repaint;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DisplayFile(FileName : String);
var
    Strm : TFileStream;
    S : AnsiString;
    ACodePage: Cardinal;
begin
    { When display on the fly, no file is generated }
    if DisplayCheckBox.Checked then
        Exit;
    try
        Strm := TFileStream.Create(FileName, fmOpenRead);
        try
            SetLength(S, Strm.Size);
            Strm.Read(S[1], Length(S));
            { Auto-detect UTF-8 if Option is set }
            if (FtpClient1.CodePage <> CP_UTF8) and
               (ftpAutoDetectCodePage in FtpClient1.Options) and
               (CharsetDetect(S) = cdrUtf8) then
                ACodePage := CP_UTF8
            else
                ACodePage := FtpClient1.CodePage;
            {$IFDEF UNICODE}
                DirectoryForm.DirListBox.Items.Text := AnsiToUnicode(S, ACodePage);
            {$ELSE}
                DirectoryForm.DirListBox.Items.Text := ConvertCodepage(S, ACodePage, CP_ACP)
            {$ENDIF}
        finally
            Strm.Free;
        end;
    except
        DirectoryForm.DirListBox.Clear;
    end;
    DirectoryForm.ShowModal;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1RequestDone(Sender: TObject;
  RqType: TFtpRequest; Error: Word);
begin
    Display('Request ' + IntToStr(Ord(RqType)) + ' Done.');
    Display('StatusCode = ' + IntToStr(FtpClient1.StatusCode));
    Display('LastResponse was : ''' + FtpClient1.LastResponse + '''');
    if Error = 0 then
        Display('No error')
    else
        Display('Error = ' + IntToStr(Error) +
                ' (' + FtpClient1.ErrorMessage + ')');

    { Display last progress value }
    InfoLabel.Caption := IntToStr(FProgressCount);

    if Error = 0 then begin
        case RqType of
        ftpOptsAsync : if (FtpClient1.NewOpts = 'UTF8 ON') and
                          (FtpClient1.StatusCode = 200) then
                          FtpClient1.CodePage := CP_UTF8
                       else if (FtpClient1.NewOpts = 'UTF8 OFF') and
                          (FtpClient1.StatusCode = 200) then
                          FtpClient1.CodePage := CP_ACP;
        ftpDirAsync, ftpDirectoryAsync,
        ftpLsAsync,  ftpListAsync       : DisplayFile(TEMP_FILE_NAME);
        ftpSizeAsync                    : Display(
                                             'File size is ' +
                                             IntToStr(FtpClient1.SizeResult) +
                                             ' bytes' );
        ftpPwdAsync, ftpMkdAsync,
        ftpCDupAsync, ftpCwdAsync       : Display(
                                             'Directory is "' +
                                             FtpClient1.DirResult + '"');
        ftpGetAsync                     :
            begin
            InfoLabel.Caption := InfoLabel.Caption + ' [' + IntToStr(IcsGetFileSize(FtpClient1.LocalFileName)) + ']';
            {WriteLn(LogFile, 'FTP GET RequestDone "',
                             FtpClient1.HostFileName, '" FileSize = ',
                             GetFileSize(FtpClient1.LocalFileName));}
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1SessionConnected(
    Sender : TObject;
    Error  : Word);
begin
    Display('Session Connected, error = ' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1SessionClosed(
    Sender : TObject;
    Error  : Word);
begin
    Display('Session Closed, error = ' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1StateChange(Sender: TObject);
begin
    StateLabel.Caption := IntToStr(Ord(FtpClient1.State));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure execute either the synchronous command or the asynchronous }
{ command depending on the SyncCheckBox state.                              }
{ All date from UI is copied to FTP component properties.                   }
procedure TFtpReceiveForm.ExecuteCmd(SyncCmd : TSyncCmd; ASyncCmd : TAsyncCmd);
var
    Bandwidth : Integer;
begin
    Display('Executing Requested Command');
    { Initialize progress stuff }
    FLastProgress  := 0;
    FProgressCount := 0;

    FtpClient1.HostName           := HostNameEdit.Text;
    FtpClient1.Port               := PortEdit.Text;
    FtpClient1.DataPortRangeStart := StrToInt(Trim(DataPortRangeStartEdit.Text));
    FtpClient1.DataPortRangeEnd   := StrToInt(Trim(DataPortRangeEndEdit.Text));
    FtpClient1.UserName           := UserNameEdit.Text;
    FtpClient1.Password           := PasswordEdit.Text;
    FtpClient1.Account            := AccountEdit.Text;
    FtpClient1.HostDirName        := HostDirEdit.Text;
    FtpClient1.HostFileName       := HostFileEdit.Text;
    FtpClient1.LocalFileName      := LocalFileEdit.Text;
    FtpClient1.Passive            := PassiveCheckBox.Checked;
    FtpClient1.Binary             := BinaryCheckBox.Checked;
    FtpClient1.ResumeAt           := StrToInt(ResumeAtEdit.Text);
    FtpClient1.DisplayFileFlag    := DisplayCheckBox.Checked;
    FtpClient1.OnDisplay          := DisplayHandler;

    SslContext1.SslVerifyPeer     := VerifyPeerCheckBox.Checked;
    SslContext1.SslCertFile       := CertFileEdit.Text;
    SslContext1.SslPassPhrase     := PassPhraseEdit.Text;
    SslContext1.SslPrivKeyFile    := PrivKeyFileEdit.Text;
    SslContext1.SslCAFile         := CAFileEdit.Text;
    SslContext1.SslCAPath         := CAPathEdit.Text;

    FtpClient1.ProtLevel          := ProtLevelEdit.Text;
    FtpClient1.PBSZSize           := 0;  { note value never actually sent }

    { For directory functions, we use a temporary file }
    if (@SyncCmd = @TFtpClient.Dir) or (@SyncCmd = @TFtpClient.Directory) or
       (@SyncCmd = @TFtpClient.Ls)  or (@SyncCmd = @TFtpClient.List) then begin
        DeleteFile(TEMP_FILE_NAME);
        FtpClient1.LocalFileName   := TEMP_FILE_NAME;
    end;

    if NoAutoResumeAtCheckBox.Checked then
        FtpClient1.Options     := FtpClient1.Options + [ftpNoAutoResumeAt]
    else
        FtpClient1.Options     := FtpClient1.Options - [ftpNoAutoResumeAt];

    if SslTypeCombobox.ItemIndex > -1 then begin
        FtpClient1.SslType := TFtpCliSslType(SslTypeCombobox.ItemIndex);
        if FtpClient1.SslType = sslTypeImplicit then
            FtpClient1.Port := SslPortEdit.Text;
    end;
    Bandwidth := StrToIntDef(MaxKbEdit.Text, 0);
    if Bandwidth > 0 then begin
        FtpClient1.BandwidthLimit := Bandwidth * 1024;
        FtpClient1.Options := FtpClient1.Options + [ftpBandwidthControl];
    end
    else
        FtpClient1.Options := FtpClient1.Options - [ftpBandwidthControl];

    if SyncCheckBox.Checked then begin
        if SyncCmd then
            Display('Command Success')
        else
            Display('Command Failure');
    end
    else
        ASyncCmd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.OpenAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Open, FtpClient1.OpenAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.QuitAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Quit, FtpClient1.QuitAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.CwdAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Cwd, FtpClient1.CwdAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.UserAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.User, FtpClient1.UserAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.PassAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Pass, FtpClient1.PassAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.AcctAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Acct, FtpClient1.AcctAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ProtButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Prot, FtpClient1.ProtAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.PbszButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Pbsz, FtpClient1.PbszAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ConnectAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Connect, FtpClient1.ConnectAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.GetAsyncButtonClick(Sender: TObject);
begin
    //WriteLn(LogFile, '====================== FTP GET ===============================');
    ExecuteCmd(FtpClient1.Get, FtpClient1.GetAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ReceiveAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Receive, FtpClient1.ReceiveAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.AbortAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Abort, FtpClient1.AbortAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DirAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Dir, FtpClient1.DirAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DirectoryAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Directory, FtpClient1.DirectoryAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.LsAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Ls, FtpClient1.LsAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ListAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.List, FtpClient1.ListAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.SystAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Syst, FtpClient1.SystAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.SystemAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.System, FtpClient1.SystemAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FileSizeAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.FileSize, FtpClient1.FileSizeAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.SizeAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Size, FtpClient1.SizeAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.MkdAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Mkd, FtpClient1.MkdAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.MkdirAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.MkDir, FtpClient1.MkdirAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RmdAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Rmd, FtpClient1.RmdAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RmdirAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.RmDir, FtpClient1.RmDirAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RenAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Ren, FtpClient1.RenAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RenameAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Rename, FtpClient1.RenameAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DeleAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Dele, FtpClient1.DeleAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DeleteAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Delete, FtpClient1.DeleteAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.PwdAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Pwd, FtpClient1.PwdAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.QuoteAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Quote, FtpClient1.QuoteAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DoQuoteAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.DoQuote, FtpClient1.DoQuoteAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.PutAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Put, FtpClient1.PutAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.TransmitAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Transmit, FtpClient1.TransmitAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.TypeSetAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.TypeSet, FtpClient1.TypeSetAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RestGetAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.RestGet, FtpClient1.RestGetAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RestartGetAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.RestartGet, FtpClient1.RestartGetAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.CDupAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.CDup, FtpClient1.CDupAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ClearButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.AppendAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Append, FtpClient1.AppendAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.AppendFileAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.AppendFile, FtpClient1.AppendFileAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.AbortXferAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.AbortXfer, FtpClient1.AbortXferAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.AuthButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Auth, FtpClient1.AuthAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.CccButtonClick(Sender: TObject);
begin
     ExecuteCmd(FtpClient1.Ccc, FtpClient1.CccAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FeatButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Feat, FtpClient1.FeatAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.Button1Click(Sender: TObject);
var
    Count : Integer;
begin
    SyncCheckBox.Checked := TRUE;
    if not FtpClient1.Connected then begin
        FtpClient1.HostName        := HostNameEdit.Text;
        FtpClient1.Port            := PortEdit.Text;
        FtpClient1.UserName        := UserNameEdit.Text;
        FtpClient1.Password        := PasswordEdit.Text;
        FtpClient1.DisplayFileFlag := DisplayCheckBox.Checked;
        FtpClient1.OnDisplay       := DisplayHandler;
        ExecuteCmd(FtpClient1.Connect, FtpClient1.ConnectAsync);
        if Copy(FtpClient1.LastResponse, 1, 3) <> '230' then
            Exit;
    end;
    Count := 0;
    repeat
        DisplayMemo.Clear;
        Inc(Count);
        Display('Count=' + IntToStr(Count));
        FtpClient1.HostDirName     := HostDirEdit.Text;
        FtpClient1.HostFileName    := HostFileEdit.Text;
        FtpClient1.LocalFileName   := LocalFileEdit.Text;
        FtpClient1.Binary          := BinaryCheckBox.Checked;
        FtpClient1.Passive         := PassiveCheckBox.Checked;
        FtpClient1.DisplayFileFlag := DisplayCheckBox.Checked;
        FtpClient1.OnDisplay       := DisplayHandler;
        ExecuteCmd(FtpClient1.Put, FtpClient1.PutAsync);
    until Copy(FtpClient1.LastResponse, 1, 3) <> '226';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RestPutAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.RestPut, FtpClient1.RestPutAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RestartPutAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.RestartPut, FtpClient1.RestartPutAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.TransmitUsingStreamButtonClick(Sender: TObject);
var
    MyStream : TStream;
begin
    MyStream := TFileStream.Create(LocalFileEdit.Text, fmOpenRead);
    try
        FtpClient1.HostName        := HostNameEdit.Text;
        FtpClient1.Port            := PortEdit.Text;
        FtpClient1.UserName        := UserNameEdit.Text;
        FtpClient1.PassWord        := PassWordEdit.Text;
        FtpClient1.HostDirName     := HostDirEdit.Text;
        FtpClient1.HostFileName    := HostFileEdit.Text;
        FtpClient1.LocalStream     := MyStream;
        FtpClient1.Binary          := BinaryCheckBox.Checked;
        FtpClient1.DisplayFileFlag := DisplayCheckBox.Checked;
        FtpClient1.OnDisplay       := DisplayHandler;
        if ResumeAtEdit.Text > '' then
            FtpClient1.ResumeAt    := StrToInt(ResumeAtEdit.Text);
        FtpClient1.Transmit;
    finally
        FtpClient1.LocalStream     := nil;
        MyStream.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ReceiveUsingStreamButtonClick(Sender: TObject);
var
    MyStream : TStream;
begin
    MyStream := TFileStream.Create(LocalFileEdit.Text, fmCreate);
    try
        FtpClient1.HostName        := HostNameEdit.Text;
        FtpClient1.Port            := PortEdit.Text;
        FtpClient1.UserName        := UserNameEdit.Text;
        FtpClient1.PassWord        := PassWordEdit.Text;
        FtpClient1.HostDirName     := HostDirEdit.Text;
        FtpClient1.HostFileName    := HostFileEdit.Text;
        FtpClient1.LocalStream     := MyStream;
        FtpClient1.Binary          := BinaryCheckBox.Checked;
        FtpClient1.DisplayFileFlag := DisplayCheckBox.Checked;
        FtpClient1.OnDisplay       := DisplayHandler;
        if ResumeAtEdit.Text > '' then
            FtpClient1.ResumeAt    := StrToInt(ResumeAtEdit.Text);
        FtpClient1.Receive;
    finally
        FtpClient1.LocalStream     := nil;
        MyStream.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.SslRenegotiateButtonClick(Sender: TObject);
begin
    { This is just a test, don't use it in real world apps, use }
    { Auth TLS/SSL instead.                                     }
    if not FtpClient1.ControlSocket.SslStartRenegotiation then
        raise Exception.Create('SslRenegotiation can be started only on an ' +
                               'existing secure Sslv3 or better connection!');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetFileSize(const FileName: string): LongInt;
var
    SearchRec: TSearchRec;
begin
    if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then begin
        Result := SearchRec.Size;
        SysUtils.FindClose(SearchRec);
    end
    else
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpReceiveForm.SendFile : Boolean;
var
    LocalSize : LongInt;
begin
    try
        FtpClient1.Abort;  { Whatever occured, abort ! }
        FtpClient1.LocalFileName := LocalFileEdit.Text;
        FtpClient1.HostDirName   := HostDirEdit.Text;
        FtpClient1.HostFileName  := HostFileEdit.Text;
        FtpClient1.HostName      := HostNameEdit.Text;
        FtpClient1.Port          := PortEdit.Text;
        FtpClient1.UserName      := UserNameEdit.Text;
        FtpClient1.Password      := PassWordEdit.Text;
        FtpClient1.Passive       := PassiveCheckBox.Checked;
        FtpClient1.Binary        := BinaryCheckBox.Checked;
        FtpClient1.Timeout       := 30;
        FtpClient1.ShareMode     := ftpShareDenyNone;
        if not FtpClient1.Connect then
            raise Exception.Create('FtpClient1.Connect failed: ' + FtpClient1.ErrorMessage);
        if FtpClient1.HostDirName <> '' then begin
            if not FtpClient1.Cwd then
                raise Exception.Create('FtpClient1.Cwd failed: ' + FtpClient1.ErrorMessage);
        end;
        if not FtpClient1.TypeSet then
            raise Exception.Create('FtpClient1.TypeSet failed: ' + FtpClient1.ErrorMessage);
        if not FtpClient1.Put then
            raise Exception.Create('FtpClient1.Put failed: ' + FtpClient1.ErrorMessage);
        if FtpClient1.StatusCode <> 226 then
            raise Exception.Create('FtpClient1.Put failed: ' + FtpClient1.LastResponse);
        if not FtpClient1.Size then
            raise Exception.Create('FtpClient1.Size failed: ' + FtpClient1.ErrorMessage);
        LocalSize := GetFileSize(FtpClient1.LocalFileName);
        Result    := (FtpClient1.SizeResult = LocalSize);
        if not Result then
            Display('Incorrect file size on server (S=' +
                    IntToStr(FtpClient1.SizeResult) + ' L=' +
                    IntToStr(LocalSize) + ')');
        if not FtpClient1.Quit then
            raise Exception.Create('FtpClient1.Quit failed: ' + FtpClient1.ErrorMessage);
    except
        on E:Exception do begin
            Display('Exception ' + E.ClassName + ': ' + E.Message);
            Result := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.StressPutButtonClick(Sender: TObject);
var
    Count : Integer;
begin
    if FRunning then begin
        FRunning := FALSE;
        Display('Stress Put interrupted.');
        StressPutButton.Caption := 'Stress Put';
        Exit;
    end;
    FRunning := TRUE;
    try
        Display('Stress Put started.');
        StressPutButton.Caption := 'STOP';
        Count := 0;
        while FRunning and (not Application.Terminated) do begin
            Inc(Count);
            Display('STRESS COUNT = ' + IntToStr(Count));
            if not SendFile then
                break;
            Application.ProcessMessages;
        end;
    finally
        FRunning := FALSE;
        Display('Stress Put finished.');
        StressPutButton.Caption := 'Stress Put';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1DisplayFile(
    Sender: TObject;
    var Msg: String);
begin
    Display('DATA: "' + Msg + '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1SslVerifyPeer(Sender: TObject;
    var Ok: Integer; Cert: TX509Base);
var
    ErrCode : Integer;
begin
    if Ok <> 0 then
        Display('Received certificate. Issuer = "' + Cert.IssuerOneLine + '"')
    else begin
        ErrCode := Cert.VerifyResult;
        if (ErrCode = X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN) or
           (ErrCode = X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT) then begin
            FNotTrusted := Cert.IssuerOneLine + '/SN=' + IntToStr(Cert.SerialNum);
            if FTrustedList.IndexOf(FNotTrusted) >= 0 then
                Ok := 1
            else if Application.MessageBox(
                   PChar('Do you want to trust this certificate ?' + #10 +
                         FNotTrusted),
                   PChar(FtpClient1.HostName), MB_YESNO + MB_DEFBUTTON2) = ID_YES then begin
                FTrustedList.Add(FNotTrusted);
                Ok := 1;
            end;
            if Ok = 1 then begin
                Display('Received certificate. Issuer = "' + Cert.IssuerOneLine + '"');
                Display('Serial number = ' + IntToStr(Cert.SerialNum));
                Display('We trust this one');
            end;
            Exit;
        end;
        Display('Can''t verify certificate:');
        Display('  Issuer  = "' + Cert.IssuerOneLine + '"');
        Display('  Subject = "' + Cert.SubjectOneLine + '"');
        Display('  Error   = ' + IntToStr(ErrCode) + ' (' + Cert.VerifyErrMsg + ')');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.WMSslNotTrusted(var Msg: TMessage);
begin
    if FTrustedList.IndexOf(FNotTrusted) >= 0 then
        Exit;
    if FQueryingTrusted then
        Exit;
    FQueryingTrusted := TRUE;
    try
        if Application.MessageBox(
               PChar('Do you want to trust this certificate ?' + #10 +
                     FNotTrusted),
               PChar(FtpClient1.HostName), MB_YESNO + MB_DEFBUTTON2) = ID_YES then begin
            FTrustedList.Add(FNotTrusted);
            // Now that the user accepted the certificate, we can restart
            // the request (It was interrupted because of untrusted certificate)
            // SslHttpCli1.GetAsync;
        end;
    finally
         FQueryingTrusted := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ClearTraceButtonClick(Sender: TObject);
begin
{$IFDEF DEBUG_OUTPUT_NEVER}
    CloseFile(LogFile);
    DeleteFile(PChar(LogFileName));
    AssignFile(LogFile,
               IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) +
               LogFileName);
    {$I-}
        Rewrite(LogFile);
    {$I+}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1SslCliNewSession(Sender: TObject;
    SslSession: Pointer; WasReused: Boolean; var IncRefCount: Boolean);
begin
    if not SessCacheCheckBox.Checked then
        Exit;
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching. See demo HttpsTst how to use a real external cache.          }
    FCachedSession := SslSession;
    IncRefCount    := True;
    if WasReused then
        Display('! SSL Session reused');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1SslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base;
  var Disconnect: Boolean);
begin
    Display('SSL handshake done, error #' + IntToStr (ErrCode) +
             ' - ' + (Sender as TSslFtpClient).ControlSocket.SslHandshakeRespMsg);  { V8.00 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1SslCliGetSession(Sender: TObject;
    var SslSession: Pointer; var FreeSession: Boolean);
begin
    if not SessCacheCheckBox.Checked then begin
        FCachedSession := nil;
        Exit;
    end;
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching. See demo HttpsTst how to use a real external cache.          }
    SslSession  := FCachedSession;
    FreeSession := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1SslCliCertRequest(Sender: TObject;
  var Cert: TX509Base);
var
    X : TX509Base;
begin
    { A very simple test of the SslCliCertRequest event.               }
    { This event is triggered only if CertFileEdit.Text is empty,      }
    { the server requested a certificate from the client,              }
    { and of course only in case of the SSL session wasn't reused.     }
    if not Assigned(FClientCerts) then
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
        Cert := FClientCerts[ClientCertDlg.CertListBox.ItemIndex];

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.OptsAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.NewOpts    := OptsEdit.Text;
    ExecuteCmd(FtpClient1.Opts, FtpClient1.OptsAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.

