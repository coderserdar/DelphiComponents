{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Fran�ois PIETTE
Creation:     Aug 1997
Version:      7.07 Wide
Object:       Demo for TFtpClient object (RFC 959 implementation)
              It is a graphical FTP client program
              Compatible with Delphi 1, 2, 3, 4 and 5
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2007 by Fran�ois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

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
Aug 31, 2006  V2.31 64-bit stream support for Delphi 6 up by A. Garrels.
Oct 29, 2006  V2.32 Reworked the button click event handler to show exactly
              what parameter is needed for each of the methods. The code could
              be simplifyed by moving all data from UI to component properties
              in ExecuteCmd but then no one has a clear vision about which
              property is used by each method.
06 Jan 2008   V2.33 Angus added about 20 new buttons and edits for new client commands
15 May 2008   V2.34 Arno added OverByteIcsUtils.pas to the uses clause.
Aug 27, 2008  V2.35 Arno added UTF-8 and code page support.
Sep 18, 2008  V2.36 Angus added ConnectHost, Host and Rein (re-initialise connection)
Oct 22, 2008  V7.00W wide version, client component created in code to avoid installing package
Nov 17, 2008  V7.02 added LANG button and edit, OPTS edit with drop down with UTF8 opts
              SiteCmlsd now uses XCmlsd, SiteDmlsd uses XDmlsd
Apr 16, 2009  V7.07 Angus assume STREAM64, USE_ONPROGRESS64_ONLY, removed OnProgress
              Removed local GetFileSize using IcsGetFileSize instead


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsFtpTstW1;

{$I OverbyteIcsDefs.inc} { Is STREAM64 defined? It is by default since D6! }
{$B-}                    { Enable partial boolean evaluation   }
{$T-}                    { Untyped pointers                    }
{$X+}                    { Enable extended syntax              }
{$H+}                    { Use long strings                    }
{$J+}                    { Allow typed constant to be modified }
{$IFNDEF DELPHI7_UP}
    Bomb('This program requires Delphi 7 or later');
{$ENDIF}

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, IniFiles, ExtCtrls, WinSock,
  OverByteIcsUtils, OverByteIcsFtpCliW, OverbyteIcsFtpSrvWT,
  OverByteIcsWSocket, OverbyteIcsWndControl;

const
  FTPTstVersion      = 707;
  CopyRight : String = ' FtpTstW (c) 1997-2009 F. Piette V7.07 ';

type
  TSyncCmd   = function : Boolean  of object;
  TAsyncCmd  = procedure of object;

  TFtpReceiveForm = class(TForm)
    DisplayMemo: TMemo;
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
    AuthSslButton: TButton;
    AcctAsyncButton: TButton;
    Label10: TLabel;
    AccountEdit: TEdit;
    SiteExecAsyncButton: TButton;
    ClntAsyncButton: TButton;
    MlsdAsyncButton: TButton;
    MlstAsyncButton: TButton;
    XCmlsdAsyncButton: TButton;
    FeatAsyncButton: TButton;
    XDmlsdAsyncButton: TButton;
    Md5AsyncButton: TButton;
    MdtmAsyncButton: TButton;
    XcrcAsyncButton: TButton;
    MfmtAsyncButton: TButton;
    SiteZoneAsyncButton: TButton;
    SiteIndexAsyncButton: TButton;
    SitePaswdAsyncButton: TButton;
    SiteMsgAsyncButton: TButton;
    XMd5AsyncButton: TButton;
    AlloAsyncButton: TButton;
    Label11: TLabel;
    Label12: TLabel;
    PosStartEdit: TEdit;
    PosEndEdit: TEdit;
    CombAsyncButton: TButton;
    OptsAsyncButton: TButton;
    ModeZAsyncButton: TButton;
    ModeSAsyncButton: TButton;
    Label13: TLabel;
    MaxKB: TEdit;
    CodePageEdit: TEdit;
    Label14: TLabel;
    ConnectHostAsyncButton: TButton;
    HostAsyncButton: TButton;
    ReinAsyncButton: TButton;
    LangAsyncButton: TButton;
    Label15: TLabel;
    Lanugage: TEdit;
    Label16: TLabel;
    OptsEdit: TComboBox;
    procedure ExitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DisplayHandler(Sender: TObject; var Msg : UnicodeString);
    procedure FtpClient1Progress64(Sender: TObject; Count: Int64;
      var Abort: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OpenAsyncButtonClick(Sender: TObject);
    procedure FtpClient1RequestDone(Sender: TObject; RqType: TFtpRequest;
      ErrCode: Word);
    procedure FtpClient1SessionConnected(Sender: TObject; ErrCode: Word);
    procedure FtpClient1SessionClosed(Sender: TObject; ErrCode: Word);
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
    procedure FtpClient1DisplayFile(Sender: TObject; var Msg: UnicodeString);
    procedure ClntAsyncButtonClick(Sender: TObject);
    procedure MlsdAsyncButtonClick(Sender: TObject);
    procedure MlstAsyncButtonClick(Sender: TObject);
    procedure SiteExecAsyncButtonClick(Sender: TObject);
    procedure XCmlsdAsyncButtonClick(Sender: TObject);
    procedure FeatAsyncButtonClick(Sender: TObject);
    procedure MdtmAsyncButtonClick(Sender: TObject);
    procedure Md5AsyncButtonClick(Sender: TObject);
    procedure XcrcAsyncButtonClick(Sender: TObject);
    procedure MfmtAsyncButtonClick(Sender: TObject);
    procedure SiteIndexAsyncButtonClick(Sender: TObject);
    procedure XDmlsdAsyncButtonClick(Sender: TObject);
    procedure SitePaswdAsyncButtonClick(Sender: TObject);
    procedure SiteMsgAsyncButtonClick(Sender: TObject);
    procedure SiteZoneAsyncButtonClick(Sender: TObject);
    procedure XMd5AsyncButtonClick(Sender: TObject);
    procedure AlloAsyncButtonClick(Sender: TObject);
    procedure CombAsyncButtonClick(Sender: TObject);
    procedure OptsAsyncButtonClick(Sender: TObject);
    procedure ModeZAsyncButtonClick(Sender: TObject);
    procedure ModeSAsyncButtonClick(Sender: TObject);
    procedure CodePageEditChange(Sender: TObject);
    procedure ConnectHostAsyncButtonClick(Sender: TObject);
    procedure HostAsyncButtonClick(Sender: TObject);
    procedure ReinAsyncButtonClick(Sender: TObject);
    procedure LangAsyncButtonClick(Sender: TObject);
  private
    FtpClient1 	   : TFtpClientW;
    FIniFileName   : String;
    FInitialized   : Boolean;
    FLastProgress  : DWORD;
    FProgressCount : TFtpBigInt;
    FRunning       : Boolean;
    procedure DisplayFile(FileName : UnicodeString);
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
    OverByteIcsFtpTstW2;

{$R *.DFM}
const
    SectionData   = 'Data';
    KeyHostName   = 'HostName';
    KeyUserName   = 'UserName';
    KeyPassWord   = 'PassWord';
    KeyAccount    = 'Account';
    KeyHostDir    = 'HostDir';
    KeyPort       = 'Port';
    KeyHostFile   = 'HostFile';
    KeyLocalFile  = 'LocalFile';
    KeyResumeAt   = 'ResumeAt';
    KeyPortStart  = 'DataPortRangeStart';
    KeyPortEnd    = 'DataPortRangeEnd';
    KeyPassive    = 'PassiveMode';
    KeyAuthSSL    = 'AuthSSLEnabled';
    KeyDisplay    = 'DisplayData';
    KeySync       = 'SyncMode';
    KeyNoResume   = 'NoAutoResumeAt';
    KeyBinary     = 'BinaryMode';
    SectionWindow = 'Window';
    KeyTop        = 'Top';
    KeyLeft       = 'Left';
    KeyWidth      = 'Width';
    KeyHeight     = 'Height';
    KeyPosStart   = 'PosStart';
    KeyPosEnd     = 'PosEnd';
    KeyMaxKB      = 'MaxKB';
    KeyCP         = 'CodePage';  


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
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


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FormCreate(Sender: TObject);
begin

    FtpClient1 := TFtpClientW.Create(Self);
    with FtpClient1 do begin
        Timeout := 15;
        MultiThreaded := False;
        Port := 'ftp';
        CodePage := 0;
        DataPortRangeStart := 0;
        DataPortRangeEnd := 0;
        LocalAddr := '0.0.0.0';
        DisplayFileFlag := False;
        Binary := True;
        ShareMode := ftpShareExclusive;
        Options := [ftpAcceptLF];
        ConnectionType := ftpDirect;
        OnDisplayFile := FtpClient1DisplayFile;
        OnProgress64 := FtpClient1Progress64;
        OnSessionConnected := FtpClient1SessionConnected;
        OnSessionClosed := FtpClient1SessionClosed;
        OnRequestDone := FtpClient1RequestDone;
        OnStateChange := FtpClient1StateChange;
        BandwidthLimit := 10000;
        BandwidthSampling := 1000;
    end;

    //BigConsole(80, 100);
    DisplayMemo.Clear;
    InfoLabel.Caption  := '';
    StateLabel.Caption := '';
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
    Data    : TWSAData;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIniFile.Create(FIniFileName);
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
        PosStartEdit.Text  := IniFile.ReadString(SectionData, KeyPosStart,
                                                 '0');
        PosEndEdit.Text    := IniFile.ReadString(SectionData, KeyPosEnd,
                                                 '0');
        CodePageEdit.Text  := IniFile.ReadString(SectionData, KeyCP, '0');
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
        MaxKB.Text                     := IniFile.ReadString(SectionData, KeyMaxKB, '0');

        Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    (Screen.Height - Height) div 2);
        Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   (Screen.Width - Width) div 2);

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
procedure TFtpReceiveForm.FormClose(
    Sender     : TObject;
    var Action : TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyHostName,  HostNameEdit.Text);
    IniFile.WriteString(SectionData, KeyPort,      PortEdit.Text);
    IniFile.WriteString(SectionData, KeyUserName,  UserNameEdit.Text);
    IniFile.WriteString(SectionData, KeyPassWord,  PassWordEdit.Text);
    IniFile.WriteString(SectionData, KeyAccount,   AccountEdit.Text);
    IniFile.WriteString(SectionData, KeyHostDir,   HostDirEdit.Text);
    IniFile.WriteString(SectionData, KeyHostFile,  HostFileEdit.Text);
    IniFile.WriteString(SectionData, KeyLocalFile, LocalFileEdit.Text);
    IniFile.WriteString(SectionData, KeyResumeAt,  ResumeAtEdit.Text);
    IniFile.WriteString(SectionData, KeyPosStart,  PosStartEdit.Text);
    IniFile.WriteString(SectionData, KeyPosEnd,    PosEndEdit.Text);
    IniFile.WriteString(SectionData, KeyPortStart, DataPortRangeStartEdit.Text);
    IniFile.WriteString(SectionData, KeyPortEnd,   DataPortRangeEndEdit.Text);
    IniFile.WriteInteger(SectionData, KeyPassive , Ord(PassiveCheckBox.Checked));
    IniFile.WriteInteger(SectionData, KeyDisplay , Ord(DisplayCheckBox.Checked));
    IniFile.WriteInteger(SectionData, KeySync    , Ord(SyncCheckBox.Checked   ));
    IniFile.WriteInteger(SectionData, KeyNoResume, Ord(NoAutoResumeAtCheckBox.Checked));
    IniFile.WriteInteger(SectionData, KeyBinary,   Ord(BinaryCheckBox.Checked));
    IniFile.WriteString(SectionData,  KeyMaxKB,    MaxKB.Text);
    IniFile.WriteString(SectionData,  KeyCP, CodePageEdit.Text);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.Display(const Msg : String);
var
    I : Integer;
{$IFNDEF UNICODE}
    ACodePage: Cardinal;
{$ENDIF}    
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 2000 then begin
            for I := 1 to 200 do
                DisplayMemo.Lines.Delete(0);
        end;
    {$IFDEF UNICODE}
        DisplayMemo.Lines.Add(Msg);
    {$ELSE}
        if (FtpClient1.Codepage = CP_UTF8) and (not IsUtf8Valid(Msg)) then
            ACodePage := CP_ACP
        else
            ACodePage := FtpClient1.Codepage;
        DisplayMemo.Lines.Add(ConvertCodepage(Msg, ACodePage, CP_ACP));
    {$ENDIF}
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DisplayHandler(Sender : TObject; var Msg : UnicodeString);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ExitButtonClick(Sender: TObject);
begin
    Close;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1Progress64(
    Sender    : TObject;
    Count     : Int64;
    var Abort : Boolean);
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
procedure TFtpReceiveForm.DisplayFile(FileName : UnicodeString);
var
    Strm : TIcsFileStreamW;
    S : AnsiString;
    ACodePage: Cardinal;
begin
    { When display on the fly, no file is generated }
    if DisplayCheckBox.Checked then
        exit;
    try
        Strm := TIcsFileStreamW.Create(FileName, fmOpenRead);
        try
            SetLength(S, Strm.Size);
            Strm.Read(S[1], Length(S));
            if (FtpClient1.Codepage = CP_UTF8) and (not IsUtf8Valid(S)) then
                ACodePage := CP_ACP
            else
                ACodePage := FtpClient1.Codepage;
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
procedure TFtpReceiveForm.FtpClient1RequestDone(
    Sender  : TObject;
    RqType  : TFtpRequest;
    ErrCode : Word);
begin
    Display('Request ' + LookupFTPReq (RqType) + ' Done.');
    Display('StatusCode = ' + IntToStr(FtpClient1.StatusCode));
    Display('LastResponse was : ''' + FtpClient1.LastResponse + '''');
    if ErrCode = 0 then
        Display('No error')
    else
        Display('Error = ' + IntToStr(ErrCode) +
                ' (' + FtpClient1.ErrorMessage + ')');

    { Display last progress value }
    InfoLabel.Caption := IntToStr(FProgressCount);

    if ErrCode = 0 then begin
        case RqType of
        ftpFeatAsync : if ftpFeatUtf8 in FtpClient1.SupportedExtensions then
                            CodePageEdit.Text := IntToStr(CP_UTF8);
        ftpDirAsync, ftpDirectoryAsync,
        ftpLsAsync,  ftpListAsync,
        ftpMlsdAsync, ftpSiteCmlsdAsync,
        ftpXCmlsdAsync, ftpXDmlsdAsync,
        ftpSiteDmlsdAsync, ftpSiteIndexAsync : DisplayFile(TEMP_FILE_NAME);

        ftpSizeAsync                    : Display(
                                             'File size is ' +
                                             IntToStr(FtpClient1.SizeResult) +
                                             ' bytes' );
        ftpPwdAsync, ftpMkdAsync,
        ftpCDupAsync, ftpCwdAsync       : Display(
                                             'Directory is "' +
                                             FtpClient1.DirResult + '"');
        ftpGetAsync  : InfoLabel.Caption := InfoLabel.Caption + ' [' +
                       IntToStr(IcsGetFileSize(FtpClient1.LocalFileName)) + ']';
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1SessionConnected(
    Sender  : TObject;
    ErrCode : Word);
begin
    Display('Session Connected, error = ' + IntToStr(ErrCode));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FtpClient1SessionClosed(
    Sender  : TObject;
    ErrCode : Word);
begin
    Display('Session Closed, error = ' + IntToStr(ErrCode));
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
    bandwidth: integer;
begin
    Display('Executing Requested Command');
    { Initialize progress stuff }
    FLastProgress  := 0;
    FProgressCount := 0;

    FtpClient1.HostName           := HostNameEdit.Text;
    FtpClient1.Port               := PortEdit.Text;
    FtpClient1.DisplayFileFlag    := DisplayCheckBox.Checked;
    FtpClient1.OnDisplay          := DisplayHandler;
    bandwidth                     := StrToInt(MaxKB.Text);
    if bandwidth > 0 then begin
        FtpClient1.BandwidthLimit := bandwidth * 1024;
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
    // Open doesn't require any parameter except HostName and Port
    // which are set in ExecuteCmd
    ExecuteCmd(FtpClient1.Open, FtpClient1.OpenAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.QuitAsyncButtonClick(Sender: TObject);
begin
    // Quit doesn't require any parameter
    ExecuteCmd(FtpClient1.Quit, FtpClient1.QuitAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.CwdAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.HostDirName     := HostDirEdit.Text;
    ExecuteCmd(FtpClient1.Cwd, FtpClient1.CwdAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.UserAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.UserName        := UserNameEdit.Text;
    ExecuteCmd(FtpClient1.User, FtpClient1.UserAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.PassAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.Password        := PasswordEdit.Text;
    ExecuteCmd(FtpClient1.Pass, FtpClient1.PassAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.AcctAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.Account         := AccountEdit.Text;
    ExecuteCmd(FtpClient1.Acct, FtpClient1.AcctAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ConnectAsyncButtonClick(Sender: TObject);
begin
    // Connect require HostName and Port which are set in ExecuteCmd
    // Connect is 3 commands in sequence: Open, User and Pass.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.Password        := PasswordEdit.Text;
    ExecuteCmd(FtpClient1.Connect, FtpClient1.ConnectAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ConnectHostAsyncButtonClick(Sender: TObject);
begin
    // Connect require HostName and Port which are set in ExecuteCmd
    // Connect is 4 commands in sequence: Open, Host, User and Pass.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.Password        := PasswordEdit.Text;
    ExecuteCmd(FtpClient1.ConnectHost, FtpClient1.ConnectHostAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.GetAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.HostDirName     := HostDirEdit.Text;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := LocalFileEdit.Text;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    ExecuteCmd(FtpClient1.Get, FtpClient1.GetAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.HostAsyncButtonClick(Sender: TObject);
begin
  // uses HostName parameter, may be sent before logon  
    ExecuteCmd(FtpClient1.Host, FtpClient1.HostAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ReceiveAsyncButtonClick(Sender: TObject);
begin
    // Receive is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, TypeSet, Pasv, Cwd, Get and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := LocalFileEdit.Text;
    FtpClient1.Binary          := BinaryCheckBox.Checked;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    ExecuteCmd(FtpClient1.Receive, FtpClient1.ReceiveAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.AbortAsyncButtonClick(Sender: TObject);
begin
    // Abort doesn't require any parameter
    ExecuteCmd(FtpClient1.Abort, FtpClient1.AbortAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DirAsyncButtonClick(Sender: TObject);
begin
    DeleteFile(TEMP_FILE_NAME);
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := TEMP_FILE_NAME;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    ExecuteCmd(FtpClient1.Dir, FtpClient1.DirAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DirectoryAsyncButtonClick(Sender: TObject);
begin
    // Directory is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Pasv, Cwd, List and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    DeleteFile(TEMP_FILE_NAME);
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := TEMP_FILE_NAME;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    ExecuteCmd(FtpClient1.Directory, FtpClient1.DirectoryAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.LsAsyncButtonClick(Sender: TObject);
begin
    DeleteFile(TEMP_FILE_NAME);
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := TEMP_FILE_NAME;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    ExecuteCmd(FtpClient1.Ls, FtpClient1.LsAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ListAsyncButtonClick(Sender: TObject);
begin
    // List is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Pasv, Cwd, List and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    DeleteFile(TEMP_FILE_NAME);
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := TEMP_FILE_NAME;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    ExecuteCmd(FtpClient1.List, FtpClient1.ListAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.LangAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.Language := Lanugage.Text;
    ExecuteCmd(FtpClient1.System, FtpClient1.LangAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.SystAsyncButtonClick(Sender: TObject);
begin
    // Doesn't require any parameter
    ExecuteCmd(FtpClient1.Syst, FtpClient1.SystAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.SystemAsyncButtonClick(Sender: TObject);
begin
    // System is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Syst and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    ExecuteCmd(FtpClient1.System, FtpClient1.SystemAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FileSizeAsyncButtonClick(Sender: TObject);
begin
    // FileSize is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Cwd, Size and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    ExecuteCmd(FtpClient1.FileSize, FtpClient1.FileSizeAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.SizeAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.HostFileName    := HostFileEdit.Text;
    ExecuteCmd(FtpClient1.Size, FtpClient1.SizeAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.MkdAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.HostFileName    := HostFileEdit.Text; // For directory name
    ExecuteCmd(FtpClient1.Mkd, FtpClient1.MkdAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.MkdirAsyncButtonClick(Sender: TObject);
begin
    // MkDir is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Cwd, Mkd and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;   // For base directory
    FtpClient1.HostFileName    := HostFileEdit.Text;  // For directory name
    ExecuteCmd(FtpClient1.MkDir, FtpClient1.MkdirAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RmdAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.HostFileName    := HostFileEdit.Text; // For directory name
    ExecuteCmd(FtpClient1.Rmd, FtpClient1.RmdAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RmdirAsyncButtonClick(Sender: TObject);
begin
    // RmDir is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Cwd, Rmd and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;   // For base directory
    FtpClient1.HostFileName    := HostFileEdit.Text;  // For directory name
    ExecuteCmd(FtpClient1.RmDir, FtpClient1.RmDirAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RenAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.HostFileName    := HostFileEdit.Text;  // Old name
    FtpClient1.LocalFileName   := LocalFileEdit.Text; // New name
    ExecuteCmd(FtpClient1.Ren, FtpClient1.RenAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RenameAsyncButtonClick(Sender: TObject);
begin
    // Rename is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Cwd, Ren and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;    // Directory of file
    FtpClient1.HostFileName    := HostFileEdit.Text;   // Old name
    FtpClient1.LocalFileName   := LocalFileEdit.Text;  // nEw name
    ExecuteCmd(FtpClient1.Rename, FtpClient1.RenameAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DeleAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.HostFileName    := HostFileEdit.Text;
    ExecuteCmd(FtpClient1.Dele, FtpClient1.DeleAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DeleteAsyncButtonClick(Sender: TObject);
begin
    // Delete is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Cwd, Del and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;    // Directory of file
    FtpClient1.HostFileName    := HostFileEdit.Text;   // File to delete
    ExecuteCmd(FtpClient1.Delete, FtpClient1.DeleteAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.PwdAsyncButtonClick(Sender: TObject);
begin
    // Pwd doesn't require any parameter
    ExecuteCmd(FtpClient1.Pwd, FtpClient1.PwdAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.QuoteAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.LocalFileName   := LocalFileEdit.Text;  // Command to send
    ExecuteCmd(FtpClient1.Quote, FtpClient1.QuoteAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.DoQuoteAsyncButtonClick(Sender: TObject);
begin
    // DoQuote is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Cwd, Quote and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;    // Directory for command
    FtpClient1.LocalFileName   := LocalFileEdit.Text;  // Command to send
    ExecuteCmd(FtpClient1.DoQuote, FtpClient1.DoQuoteAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.PutAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.HostDirName     := HostDirEdit.Text;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := LocalFileEdit.Text;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    ExecuteCmd(FtpClient1.Put, FtpClient1.PutAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.TransmitAsyncButtonClick(Sender: TObject);
begin
    // Transmit is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Cwd, TypeSet, Pasv, Put and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := LocalFileEdit.Text;
    FtpClient1.Binary          := BinaryCheckBox.Checked;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    ExecuteCmd(FtpClient1.Transmit, FtpClient1.TransmitAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.TypeSetAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.Binary          := BinaryCheckBox.Checked;
    ExecuteCmd(FtpClient1.TypeSet, FtpClient1.TypeSetAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RestGetAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := LocalFileEdit.Text;
    FtpClient1.Options         := [];
    if NoAutoResumeAtCheckBox.Checked then
        FtpClient1.Options     := FtpClient1.Options + [ftpNoAutoResumeAt];
    FtpClient1.ResumeAt        := StrToInt(ResumeAtEdit.Text);
    ExecuteCmd(FtpClient1.RestGet, FtpClient1.RestGetAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RestartGetAsyncButtonClick(Sender: TObject);
begin
    // RestartGet is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Cwd, TypeSet, Pasv, restGet and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := LocalFileEdit.Text;
    FtpClient1.Binary          := BinaryCheckBox.Checked;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    FtpClient1.Options         := [];
    if NoAutoResumeAtCheckBox.Checked then
        FtpClient1.Options     := FtpClient1.Options + [ftpNoAutoResumeAt];
    FtpClient1.ResumeAt        := StrToInt(ResumeAtEdit.Text);
    ExecuteCmd(FtpClient1.RestartGet, FtpClient1.RestartGetAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.CDupAsyncButtonClick(Sender: TObject);
begin
    // Doesn't require any parameter
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
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := LocalFileEdit.Text;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    ExecuteCmd(FtpClient1.Append, FtpClient1.AppendAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.AppendFileAsyncButtonClick(Sender: TObject);
begin
    // AppendFile is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Cwd, TypeSet, Pasv, Append and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := LocalFileEdit.Text;
    FtpClient1.Binary          := BinaryCheckBox.Checked;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    ExecuteCmd(FtpClient1.AppendFile, FtpClient1.AppendFileAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.AbortXferAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.AbortXfer, FtpClient1.AbortXferAsync);
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
    FtpClient1.HostDirName     := HostDirEdit.Text;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := LocalFileEdit.Text;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    FtpClient1.ResumeAt        := StrToInt(ResumeAtEdit.Text);
    ExecuteCmd(FtpClient1.RestPut, FtpClient1.RestPutAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.RestartPutAsyncButtonClick(Sender: TObject);
begin
    // RestartPut is an "all-in-one" command. It is equivalent of
    // Open, User, Pass, Cwd, TypeSet, Pasv, RestPut and Quit commands
    // If you need best error control possible, it is better to use
    // the individual commands yourself.
    FtpClient1.UserName        := UserNameEdit.Text;
    FtpClient1.PassWord        := PassWordEdit.Text;
    FtpClient1.HostDirName     := HostDirEdit.Text;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := LocalFileEdit.Text;
    FtpClient1.Binary          := BinaryCheckBox.Checked;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    FtpClient1.ResumeAt        := StrToInt(ResumeAtEdit.Text);
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
procedure TFtpReceiveForm.ReinAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Rein, FtpClient1.ReinAsync);
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
        LocalSize := IcsGetFileSize(FtpClient1.LocalFileName);
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
    Sender  : TObject;
    var Msg : UnicodeString);
begin
    Display('DATA: "' + Msg + '"');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ClntAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatClnt in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.ClientIdStr := CopyRight;
    ExecuteCmd(FtpClient1.Clnt, FtpClient1.ClntAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.MlsdAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatMLST in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    DeleteFile(TEMP_FILE_NAME);
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := TEMP_FILE_NAME;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    ExecuteCmd(FtpClient1.Mlsd, FtpClient1.MlsdAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.MlstAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatMLST in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    ExecuteCmd(FtpClient1.Mlst, FtpClient1.MlstAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.SiteExecAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatSiteExec in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    ExecuteCmd(FtpClient1.SiteExec, FtpClient1.SiteExecAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.FeatAsyncButtonClick(Sender: TObject);
begin
    ExecuteCmd(FtpClient1.Feat, FtpClient1.FeatAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.MdtmAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatMdtm in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    ExecuteCmd(FtpClient1.Mdtm, FtpClient1.MdtmAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.Md5AsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatMd5 in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    ExecuteCmd(FtpClient1.Md5, FtpClient1.Md5Async);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.XcrcAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatXcrc in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.PosStart        := atoi64(PosStartEdit.Text);
    FtpClient1.PosEnd          := atoi64(PosEndEdit.Text);
    ExecuteCmd(FtpClient1.XCrc, FtpClient1.XCrcAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.MfmtAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatMfmt in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    ExecuteCmd(FtpClient1.Mfmt, FtpClient1.MfmtAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.SiteIndexAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatSiteIndex in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    ExecuteCmd(FtpClient1.SiteIndex, FtpClient1.SiteIndexAsync);

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.XCmlsdAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatXCmlsd in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    DeleteFile(TEMP_FILE_NAME);
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := TEMP_FILE_NAME;
    ExecuteCmd(FtpClient1.XCmlsd, FtpClient1.XCmlsdAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.XDmlsdAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatXDmlsd in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    DeleteFile(TEMP_FILE_NAME);
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.LocalFileName   := TEMP_FILE_NAME;
    FtpClient1.Passive         := PassiveCheckBox.Checked;
    ExecuteCmd(FtpClient1.XDmlsd, FtpClient1.XDmlsdAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.SitePaswdAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatSitePaswd in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    ExecuteCmd(FtpClient1.SitePaswd, FtpClient1.SitePaswdAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.SiteMsgAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatSiteMsg in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    ExecuteCmd(FtpClient1.SiteMsg, FtpClient1.SiteMsgAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.SiteZoneAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatSiteZone in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    ExecuteCmd(FtpClient1.SiteZone, FtpClient1.SiteZoneAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.XMd5AsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatXMd5 in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    FtpClient1.PosStart        := atoi64(PosStartEdit.Text);
    FtpClient1.PosEnd          := atoi64(PosEndEdit.Text);
    ExecuteCmd(FtpClient1.Xmd5, FtpClient1.Xmd5Async);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.AlloAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.PosEnd          := atoi64(PosEndEdit.Text);
    ExecuteCmd(FtpClient1.Allo, FtpClient1.AlloAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.CombAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatComb in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.HostFileName    := HostFileEdit.Text;
    ExecuteCmd(FtpClient1.Comb, FtpClient1.CombAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.OptsAsyncButtonClick(Sender: TObject);
begin
    FtpClient1.NewOpts    := OptsEdit.Text;
    ExecuteCmd(FtpClient1.Opts, FtpClient1.OptsAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ModeZAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatModeZ in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.TransferMode := ftpTransModeZDeflate;
    ExecuteCmd(FtpClient1.ModeZ, FtpClient1.ModeZAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.ModeSAsyncButtonClick(Sender: TObject);
begin
    if NOT (ftpFeatModeZ in FtpClient1.SupportedExtensions) then begin
        Display('Feature Not Available or FEAT Command Not Sent Yet');
        exit;
    end;
    FtpClient1.TransferMode := ftpTransModeStream;
    ExecuteCmd(FtpClient1.ModeZ, FtpClient1.ModeZAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpReceiveForm.CodePageEditChange(Sender: TObject);
begin
    FtpClient1.CodePage := StrToIntDef(CodePageEdit.Text, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.

