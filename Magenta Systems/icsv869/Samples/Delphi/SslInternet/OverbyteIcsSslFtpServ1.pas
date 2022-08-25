{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This is a demo program showing how to use the TFtpServer
              component to build a FTP server.
              Warning: As this demo is written, full access is given to all
              users to all files accessible by the computer running the demo.
              In production program, you should add code to implement
              security issues.
Creation:     April 21, 1998
Version:      8.64
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1998-2020 by François PIETTE
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

History:
Apr 29, 1998  V0.90 Released for beta testing.
Apr 30, 1998  V0.91 Added an example of virtual file (see the code for
              FtpServer1RetrSessionConnected.
May 01, 1998  V0.92 Adapted for Delphi 1.0
May 03, 1998  V0.93 Adapted for Delphi 2.0 and C++Builder
May 04, 1998  V0.94 Added tools menu.
Jul 09, 1998  V1.00 Adapted for Delphi 4, removed beta status.
Jul 21, 1998  V1.01 Show how to refuse a client in OnClientConnected
Oct 25, 2000  V1.02 Added "List Clients" menu item and coding.
Jun 18, 2001  V1.03 Display version informations
Jul 30, 2001  V1.04 Add Trim function for Delphi 1
Feb 26, 2002  V1.05 Add DisconectAll in main menu
Jun 07, 2002  V1.06 Added a processing thread (not for Delphi 1) for Get
Oct 21, 2005  V1.07 Arno Garrels added SSL features.
Jun 04, 2008  V1.08 Arno Garrels adjusted WorkerThreadTerminated().
Aug 04, 2008  V1.09 A. Garrels made a few changes to prepare code for Unicode.
Nov 6, 2008   V1.12 Angus, support server V7.00 which does not use OverbyteIcsFtpSrvC
                    Added ftpaccounts-default.ini file with user accounts setting defaults for each user
                    Home directory, etc, now set from user account instead of common to all users
                    ReadOnly account supported
                   (next release will have a different file for each HOST supported)
                    Note: random account names are no longer allowed for this demo
Nov 8, 2008  V1.13 Angus, support HOST and REIN(ialise) commands
Nov 13, 2008 V1.14 Angus, ensure servers have ftpsCwdCheck set
Dec 9, 2014  V8.00 Angus added SslHandshakeRespMsg for better error handling
May 24, 2016 V8.01 Angus added OverbyteIcsLIBEAY, OverbyteIcsSsLeay to uses
Nov 12 2016  V8.37 Set friendly errors
                   Specify minimum and maximum SSL version supported
                   Allow server IP address to be specified
Apr 15, 2017  V8.38 FPiette removed compiler warnings for D10.2
Aug 25, 2017, V8.50 Angus added server onDisplay event
Jan 22, 2020, V8.64 Added TFtpOptions ftpsAuthForceSsl which require SSL/TLS for
                       LOGIN so no clear credentials allowed.

Sample entry from ftpaccounts-default.ini

[ics]
Password=ics
ForceSsl=false
HomeDir=c:\temp
OtpMethod=none
ForceHomeDir=true
HidePhysicalPath=true
ReadOnly=false


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslFtpServ1;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, OverbyteIcsIniFiles, OverbyteIcsFtpSrv, OverbyteIcsWSocket,
  StdCtrls, ExtCtrls, Menus,
  OverbyteIcsWinsock, OverbyteIcsLibeay, OverbyteIcsLogger,
  OverbyteIcsSsLeay, OverbyteIcsWndControl, OverbyteIcsOneTimePw;

const
  FtpServVersion      = 864;
  CopyRight : String  = ' SslFtpServ (c) 1998-2020 F. Piette V8.64 ';
  WM_APPSTARTUP       = WM_USER + 1;

type
  TLogMsg = class(TComponent)
  public
     procedure Text(Prefix : Char; Msg : String);
  end;

{$IFNDEF VER80}   { Not for Delphi 1, sorry }
  TGetProcessingThread = class; { Forward declaration }
  { We use our own client class to hold our thread }
  TMyClient = class(TFtpCtrlSocket)
  private
      FWorkerThread    : TGetProcessingThread;
      LastSslHandshake : Longword;
  end;

  TGetProcessingThread = class(TThread)
  protected
    Server : TFtpServer;
    Client : TMyClient;
  public
      procedure Execute; override;
  end;
{$ENDIF}

  TSslFtpServerForm = class(TForm)
    SslFtpServer1: TSslFtpServer;
    InfoMemo: TMemo;
    Panel1: TPanel;
    StartMinimizedCheckBox: TCheckBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    MnuStartServer: TMenuItem;
    MnuStopServer: TMenuItem;
    MnuQuit: TMenuItem;
    N1: TMenuItem;
    About1: TMenuItem;
    GreenImage: TImage;
    ClientCountLabel: TLabel;
    RedImage: TImage;
    Tools1: TMenuItem;
    Cleardisplay1: TMenuItem;
    MnuListClients: TMenuItem;
    N2: TMenuItem;
    DisconnectAllMnu: TMenuItem;
    SslContext1: TSslContext;
    CertFileEdit: TEdit;
    Label4: TLabel;
    Label6: TLabel;
    PrivKeyFileEdit: TEdit;
    Label7: TLabel;
    PassPhraseEdit: TEdit;
    Label11: TLabel;
    CAFileEdit: TEdit;
    Label10: TLabel;
    CAPathEdit: TEdit;
    VerifyPeerCheckBox: TCheckBox;
    SslTypeConnPortEdit: TEdit;
    Label1: TLabel;
    SslFtpServer2: TSslFtpServer;
    Label17: TLabel;
    RenegotiationIntervalEdit: TEdit;
    Label18: TLabel;
    Label16: TLabel;
    OpenSslVer1: TMenuItem;
    DisplaySslInfoCheckBox: TCheckBox;
    IcsLogger1: TIcsLogger;
    Label2: TLabel;
    ServIpAddr: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure SslFtpServer1ClientConnect(Sender: TObject;
      Client: TFtpCtrlSocket; Error: Word);
    procedure SslFtpServer1ClientDisconnect(Sender: TObject;
      Client: TFtpCtrlSocket; Error: Word);
    procedure SslFtpServer1Start(Sender: TObject);
    procedure SslFtpServer1Stop(Sender: TObject);
    procedure SslFtpServer1ClientCommand(Sender: TObject;
      Client: TFtpCtrlSocket; var Keyword, Params, Answer: TFtpString);
    procedure SslFtpServer1StorSessionConnected(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure SslFtpServer1StorSessionClosed(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure SslFtpServer1RetrDataSent(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure SslFtpServer1RetrSessionConnected(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure SslFtpServer1RetrSessionClosed(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SslFtpServer1AnswerToClient(Sender: TObject;
      Client: TFtpCtrlSocket; var Answer: TFtpString);
    procedure SslFtpServer1Authenticate(Sender: TObject;
      Client: TFtpCtrlSocket; UserName, Password: TFtpString;
      var Authenticated: Boolean);
    procedure SslFtpServer1ChangeDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; Directory: TFtpString; var Allowed: Boolean);
    procedure MnuQuitClick(Sender: TObject);
    procedure MnuStopServerClick(Sender: TObject);
    procedure MnuStartServerClick(Sender: TObject);
    procedure ImagesDblClick(Sender: TObject);
    procedure SslFtpServer1BuildDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; var Directory: TFtpString; Detailed: Boolean);
    procedure SslFtpServer1AlterDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; var Directory: TFtpString; Detailed: Boolean);
    procedure Cleardisplay1Click(Sender: TObject);
    procedure MnuListClientsClick(Sender: TObject);
    procedure DisconnectAllMnuClick(Sender: TObject);
    procedure SslFtpServer1GetProcessing(Sender: TObject;
      Client: TFtpCtrlSocket; var DelayedSend : Boolean);
    procedure SslFtpServer1SslVerifyPeer(Sender: TObject; var Ok: Integer;
      Cert: TX509Base);
    procedure SslFtpServer1SslHandshakeDone(Sender: TObject; ErrCode: Word;
      PeerCert: TX509Base; var Disconnect: Boolean);
    procedure OpenSslVer1Click(Sender: TObject);
    procedure RenegotiationIntervalEditChange(Sender: TObject);
    procedure SslFtpServer1OtpGetPassword(Sender: TObject; Client: TFtpCtrlSocket; UserName: TFtpString;
      var UserPassword: string);
    procedure SslFtpServer1OtpMethod(Sender: TObject; Client: TFtpCtrlSocket; UserName: TFtpString;
      var OtpMethod: TOtpMethod);
    procedure SslFtpServer1ValidateDele(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
      var Allowed: Boolean);
    procedure SslFtpServer1ValidatePut(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
      var Allowed: Boolean);
    procedure SslFtpServer1ValidateRnFr(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
      var Allowed: Boolean);
    procedure SslFtpServer1MakeDirectory(Sender: TObject; Client: TFtpCtrlSocket; Directory: TFtpString;
      var Allowed: Boolean);
    procedure SslFtpServer1Host(Sender: TObject; Client: TFtpCtrlSocket; Host: TFtpString; var Allowed: Boolean);
    procedure SslFtpServer1Rein(Sender: TObject; Client: TFtpCtrlSocket; var Allowed: Boolean);
    procedure SslFtpServer1Display(Sender: TObject; Client: TFtpCtrlSocket;
      Msg: TFtpString);
  private
    FInitialized              : Boolean;
    FIniFileName              : String;
    FPort                     : String;
    FXTop                     : Integer;
    FXLeft                    : Integer;
    FXWidth                   : Integer;
    FXHeight                  : Integer;
    FSslRenegotiationInterval : Longword;
    FOtpSequence              : integer;
    FOtpSeed                  : String;
    FIniRoot                  : String;
    procedure CheckRenegotiation(Client: TMyClient; IsData: Boolean);
    procedure WMAppStartup(var msg: TMessage); message WM_APPSTARTUP;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure StartServer;
    procedure StopServer;
    procedure UpdateClientCount;
{$IFNDEF VER80}
    procedure WorkerThreadTerminated(Sender : TObject);
{$ENDIF}
  end;

var
  SslFtpServerForm: TSslFtpServerForm;
  Log          : TLogMsg;

implementation

{$R *.DFM}

const
    MainTitle         = 'SSL FTP Server - http://www.overbyte.be';

    { Ini file layout }
    SectionData       = 'Data';
    KeyPort           = 'Port';

    SectionWindow       = 'Window';
    KeyTop              = 'Top';
    KeyLeft             = 'Left';
    KeyWidth            = 'Width';
    KeyHeight           = 'Height';
    KeyMinim            = 'RunMinimized';
    KeyCertFile         = 'CertFile';
    KeyPassPhrase       = 'PassPhrase';
    KeyPrivKeyFile      = 'PrivKeyFile';
    KeyVerifyPeer       = 'VerifyPeer';
    KeyCAFile           = 'CAFile';
    KeyCAPath           = 'CAPath';
    KeySslConnPort      = 'SslConnPort';
    KeyRenegInterval    = 'RenegotiationInterval';
    KeyDisplaySslInfo   = 'DisplaySslInfo';
    KeyServIpAddr       = 'ServIpAddr';


    STATUS_GREEN        = 0;
    STATUS_YELLOW       = 1;
    STATUS_RED          = 2;

   { account INI file layout }
    KeyPassword             = 'Password';
    KeyHomeDir              = 'HomeDir';
    KeyOtpMethod            = 'OtpMethod';
    KeyForceHomeDir         = 'ForceHomeDir';
    KeyHidePhysicalPath     = 'HidePhysicalPath';
    KeyReadOnly             = 'ReadOnly';
    KeyForceSsl             = 'ForceSsl';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TLogMsg.Text(Prefix : Char; Msg : String);
begin
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] in [' ', #9]) do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
    Minim   : Integer;
begin
    if not FInitialized then begin
        FInitialized        := TRUE;
        Caption             := 'Starting ' + MainTitle;
        Left                := -Width;

{$IFDEF DELPHI10}
        // BDS2006 has built-in memory leak detection and display
        ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
        IniFile  := TIcsIniFile.Create(FIniFileName);
        FXTop    := IniFile.ReadInteger(SectionWindow, KeyTop,    Top);
        FXLeft   := IniFile.ReadInteger(SectionWindow, KeyLeft,   Left);
        FXWidth  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        FXHeight := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Minim    := IniFile.ReadInteger(SectionWindow, KeyMinim,  0);
        IniFile.Free;

        LoadConfig;
        SaveConfig;    { Create the inifile keys if they don't exists }

        { Be sure to always have the window visible }
        { with a reasonable width and height        }
        if FXLeft < 0 then
            FXLeft := 0;
        if FXTop < 0 then
            FXTop := 0;
        if FXWidth < 341 then
            FXWidth := 341;
        if FXHeight <= 6831 then
            FXHeight := 683;
        if (FXLeft + FXWidth) > Screen.Width then
            FXLeft := Screen.Width - FXWidth;
        if (FXTop + FXHeight) > Screen.Height then
            FXTop := Screen.Height - FXHeight;

        StartMinimizedCheckBox.Checked := (Minim <> 0);

        { We use a custom message to initialize things once the form }
        { is visible                                                 }
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    Minim   : Integer;
begin
    try
        StopServer;
        Minim   := ord(StartMinimizedCheckBox.Checked);
        IniFile := TIcsIniFile.Create(FIniFileName);
        IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
        IniFile.WriteInteger(SectionWindow, KeyMinim,  Minim);
        IniFile.WriteString(SectionData,    KeyPort,   FPort);
        IniFile.WriteString(SectionData,    KeyCertFile,    CertFileEdit.Text);
        IniFile.WriteString(SectionData,    KeyPrivKeyFile, PrivKeyFileEdit.Text);
        IniFile.WriteString(SectionData,    KeyPassPhrase,  PassPhraseEdit.Text);
        IniFile.WriteString(SectionData,    KeyCAFile,      CAFileEdit.Text);
        IniFile.WriteString(SectionData,    KeyCAPath,      CAPathEdit.Text);
        IniFile.WriteInteger(SectionData,   KeyVerifyPeer,  Ord(VerifyPeerCheckBox.Checked));
        IniFile.WriteString(SectionData,    KeySslConnPort, SslTypeConnPortEdit.Text);
        IniFile.WriteInteger(SectionData,   KeyDisplaySslInfo,
                                            Ord(DisplaySslInfoCheckBox.Checked));
        IniFile.WriteInteger(SectionData,   KeyRenegInterval, FSslRenegotiationInterval);
        IniFile.WriteString(SectionData,    KeyServIpAddr,  ServIpAddr.Text);  { V8.37 }
        IniFile.UpdateFile;
        IniFile.Free;
    except
        { Ignore any exception when we are closing }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.LoadConfig;
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    FPort   := IniFile.ReadString(SectionData,    KeyPort,   'ftp');
    CertFileEdit.Text    := IniFile.ReadString(SectionData, KeyCertFile,
                                               '01cert.pem');
    PrivKeyFileEdit.Text := IniFile.ReadString(SectionData, KeyPrivKeyFile,
                                               '01key.pem');
    PassPhraseEdit.Text  := IniFile.ReadString(SectionData, KeyPassPhrase,
                                               'password');
    CAFileEdit.Text      := IniFile.ReadString(SectionData, KeyCAFile,
                                               'cacert.pem');
    CAPathEdit.Text      := IniFile.ReadString(SectionData, KeyCAPath,
                                               '');
    VerifyPeerCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData,
                                                              KeyVerifyPeer,
                                                              0));
    SslTypeConnPortEdit.Text := IniFile.ReadString(SectionData,
                                                   KeySslConnPort,
                                                   '990');
    ServIpAddr.Text := IniFile.ReadString(SectionData,            { V8.37 }
                                          KeyServIpAddr,
                                         '0.0.0.0');
    FSslRenegotiationInterval := IniFile.ReadInteger(SectionData,
                                                     KeyRenegInterval, 0);
    DisplaySslInfoCheckBox.Checked :=
                Boolean(IniFile.ReadInteger(SectionData, KeyDisplaySslInfo, 0));
    IniFile.Free;
    RenegotiationIntervalEdit.Text := IntToStr(FSslRenegotiationInterval div 1000);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SaveConfig;
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyPort, FPort);
    IniFile.WriteString(SectionData,    KeyCertFile,    CertFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPrivKeyFile, PrivKeyFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyPassPhrase,  PassPhraseEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAFile,      CAFileEdit.Text);
    IniFile.WriteString(SectionData,    KeyCAPath,      CAPathEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyVerifyPeer,  Ord(VerifyPeerCheckBox.Checked));
    IniFile.WriteString(SectionData,    KeySslConnPort, SslTypeConnPortEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyRenegInterval, FSslRenegotiationInterval);
    IniFile.WriteString(SectionData,    KeyServIpAddr,  ServIpAddr.Text);  { V8.37 }
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This message handler is triggered by the FormShow event. We comes here    }
{ only when the form is visible on screen.                                  }
procedure TSslFtpServerForm.WMAppStartup(var msg: TMessage);
var
    PrvWnd  : HWND;
    Buf     : String;
begin
    if StartMinimizedCheckBox.Checked then
        Application.Minimize;
    Top    := FXTop;
    Left   := FXLeft;
    Width  := FXWidth;
    Height := FXHeight;

    { Prevent the server from running twice }
    Buf    := ClassName + #0;
    PrvWnd := FindWindow(@Buf[1], MainTitle);
    if PrvWnd <> 0 then begin
        Log.Text('E', 'Server already running. Shutdown.');
{        Close; }
{        Exit;  }
    end;
    Caption := MainTitle;
    Update;                { It's nice to have the form completely displayed }
    UpdateClientCount;
end;


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
procedure TSslFtpServerForm.FormCreate(Sender: TObject);
begin
    { Build Ini file name }
    FIniFileName := GetIcsIniFileName;
    FIniRoot := LowerCase(ExtractFilePath(Application.ExeName));
    { Create the Log object }
    Log := TLogMsg.Create(Self);

{$IFNDEF VER80}
{    BigConsole(80, 100); }
{$ENDIF}
    InfoMemo.Clear;
    GreenImage.Visible := FALSE;
    RedImage.Visible   := TRUE;
    RedImage.Top       := GreenImage.Top;
    RedImage.Left      := GreenImage.Left;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.StartServer;
var
    wsi     : TWSADATA;
begin
    GreenImage.Visible := FALSE;
    RedImage.Visible   := TRUE;
    Update;

    { Display version info for program and use components }
    wsi := WinsockInfo;
    InfoMemo.Lines.Add(Trim(CopyRight));
    InfoMemo.Lines.Add('Using:');
    InfoMemo.Lines.Add('   ' + OverbyteIcsWSocket.CopyRight);
    InfoMemo.Lines.Add('   ' + OverbyteIcsFtpSrv.CopyRight);
    InfoMemo.Lines.Add('    Winsock:');
    InfoMemo.Lines.Add('        Version ' +
            Format('%d.%d', [WinsockInfo.wHighVersion shr 8,
                             WinsockInfo.wHighVersion and 15]));
    InfoMemo.Lines.Add('        ' + String(StrPas(wsi.szDescription)));
    InfoMemo.Lines.Add('        ' + String(StrPas(wsi.szSystemStatus)));
{$IFNDEF VER100}
    { A bug in Delphi 3 makes lpVendorInfo invalid }
    if wsi.lpVendorInfo <> nil then
        InfoMemo.Lines.Add('        ' + String(StrPas(wsi.lpVendorInfo)));
{$ENDIF}
    { Set SSL properties, internal session caching enabled }
//    SslContext1.SslVersionMethod            := sslV23_SERVER;
    //SslContext1.SslOptions                  := [sslOpt_NO_SSLv2]; //it's unsecure
    SslContext1.SslMinVersion       := sslVerTLS1;  { V8.37}
    SslContext1.SslMaxVersion       := sslVerMax;   { V8.37}
    SslContext1.SslCipherList       := sslCiphersMozillaSrvBack;

    { Enables OpenSsl's internal session caching }
    SslContext1.SslSessionCacheModes        := [sslSESS_CACHE_SERVER];
    SslContext1.SslSessionTimeout           := 300; //sec
    SslContext1.SslDefaultSessionIDContext  := 'AnyStringForSessionCaching';

    SslContext1.SslCertFile                 := CertFileEdit.Text;
    SslContext1.SslPassPhrase               := PassPhraseEdit.Text;
    SslContext1.SslPrivKeyFile              := PrivKeyFileEdit.Text;
    SslContext1.SslCAFile                   := CAFileEdit.Text;
    SslContext1.SslCAPath                   := CAPathEdit.Text;
    SslContext1.SslVerifyPeer               := VerifyPeerCheckBox.Checked;
    SslFtpServer1.FtpSslTypes               := [ftpAuthTls, ftpAuthSsl];
    SslFtpServer2.FtpSslTypes               := [ftpImplicitSsl];

{$IFNDEF VER80}
    { If not running 16 bits, we use our own client class }
    SslFtpServer1.ClientClass := TMyClient;
    SslFtpServer2.ClientClass := TMyClient;
{$ENDIF}
    { Use a custom multiline banner }
    SslFtpServer1.Banner := '220-Welcome to my Server' + #13#10 +
                            '220-' + #13#10 +
                            '220 ICS FTP Server ready.';
    SslFtpServer1.Addr   := ServIpAddr.Text;  { V8.37 }
    SslFtpServer2.Addr   := ServIpAddr.Text;  { V8.37 }
    SslFtpServer1.Port   := FPort;
    SslFtpServer2.Port   := SslTypeConnPortEdit.Text;
    SslFtpServer1.Start;
    SslFtpServer2.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.StopServer;
begin
    SslFtpServer1.Stop;
    SslFtpServer1.DisconnectAll;
    SslFtpServer2.Stop;
    SslFtpServer2.DisconnectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.MnuQuitClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.MnuStopServerClick(Sender: TObject);
begin
    StopServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.MnuStartServerClick(Sender: TObject);
begin
    StartServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.ImagesDblClick(Sender: TObject);
begin
    if SslFtpServer1.Active then
        StopServer
    else
        StartServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.UpdateClientCount;
var
    Count : Integer;
begin
    Count := SslFtpServer1.ClientCount + SslFtpServer2.ClientCount;
    if Count = 0  then
        ClientCountLabel.Caption := 'No user'
    else
        ClientCountLabel.Caption := IntToStr(Count) + ' users';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1ClientConnect(Sender: TObject;
  Client: TFtpCtrlSocket; Error: Word);
begin
    { The next test shows how to refuse a client }
    if Client.GetPeerAddr = '193.121.12.25' then begin
        Client.SendStr('421 Connection not allowed.' + #13#10);
        Client.Close;
        Exit;
    end;
// - Some speed tuning
    Client.RcvSize :=  64 * 1024;
    //Client.DataSocket.BufSize := Client.DataSocket.SocketSndBufSize - 1; // optimize sends
    //Client.ComponentOptions := [wsoTcpNoDelay];
//--------
    InfoMemo.Lines.Add('! ' + Client.GetPeerAddr + ' connected');
  { get INI file for default accounts, may be changed if HOST command used }
    Client.AccountIniName := SslFtpServerForm.FIniRoot + 'ftpaccounts-default.ini';
    Client.AccountReadOnly := true;
    Client.AccountPassword := '';
    UpdateClientCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1ClientDisconnect(Sender: TObject;
  Client: TFtpCtrlSocket; Error: Word);
begin
    InfoMemo.Lines.Add('! ' + Client.GetPeerAddr + ' disconnected');
    UpdateClientCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1Display(Sender: TObject;    { V8.50 }
  Client: TFtpCtrlSocket; Msg: TFtpString);
begin
    InfoMemo.Lines.Add('! ' + Msg);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1Start(Sender: TObject);
var
    SrvName : String;
begin
    SrvName := (Sender as TComponent).Name;
    GreenImage.Visible := TRUE;
    RedImage.Visible   := FALSE;
    InfoMemo.Lines.Add('! ' + SrvName + ' started');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1Stop(Sender: TObject);
var
    SrvName : String;
begin
    SrvName := (Sender as TComponent).Name;
    GreenImage.Visible := FALSE;
    RedImage.Visible   := TRUE;
    InfoMemo.Lines.Add('! ' + SrvName + ' stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1StorSessionConnected(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.GetPeerAddr +
                           ' Data session failed to open. Error #' +
                           IntToStr(Error));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1ValidateDele(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
  var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1ValidatePut(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
  var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1ValidateRnFr(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
  var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1StorSessionClosed(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.GetPeerAddr +
                           ' Data session closed. Error #' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure TSslFtpServerForm.SslFtpServer1RetrDataSent(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.GetPeerAddr +
                           ' Data sent. Error #' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the data session for a get file has     }
{ been opened. This is a good place build a file or a stream if the data    }
{ requested is not already stored in a file on the file system.             }
{ This feature is very powerfull and enable the FTP protocol to be used to  }
{ retrieve any kind of data. It this sample, we just check for C:\VIRTUAL   }
{ directory. If this directory is curent, then a TMemoryStream is created   }
{ on the fly with some data. If another directory is selected, the FTP      }
{ server works as any other: just send the requested file, if it exist !    }
{ This event handler is also a place where you can abort the file transfer. }
{ Simply trigger an exception and transfer will not take place.             }
{ Note that if you just wants to prohibe access to some directory or file,  }
{ the best place to code that is in the OnValidateGet or OnValidatePut      }
{ event handlers.                                                           }
procedure TSslFtpServerForm.SslFtpServer1RetrSessionConnected(Sender: TObject;
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    Error  : Word);
var
    Buf : String;
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.GetPeerAddr +
                           ' Data session connected. Error #' + IntToStr(Error))
    else if Copy(UpperCase(Client.FilePath), 1, 19) = 'C:\VIRTUAL\FORBIDEN' then
        raise Exception.Create('Access prohibed !')
    else if Copy(UpperCase(Client.FilePath), 1, 11) = 'C:\VIRTUAL\' then begin
        InfoMemo.Lines.Add('! VIRTUAL FILE');
        Client.UserData   := 1;        { Remember we created a stream }
        if Assigned(Client.DataStream) then
            Client.DataStream.Destroy; { Prevent memory leaks         }
        Client.DataStream := TMemoryStream.Create;
        Buf := 'This is a file created on the fly by the FTP server' + #13#10 +
               'It could result of a query to a database or anything else.' + #13#10 +
               'The request was: ''' + Client.FilePath + '''' + #13#10;
        Client.DataStream.Write(Buf[1], Length(Buf));
        Client.DataStream.Seek(0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1RetrSessionClosed(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.GetPeerAddr +
                           ' Data session closed. Error #' + IntToStr(Error));
    if Client.UserData = 1 then begin
        { We created a stream for a virtual file or dir. Delete the TStream }
        if Assigned(Client.DataStream) then begin
            { There is no reason why we should not come here, but who knows ? }
            Client.DataStream.Destroy;
            Client.DataStream := nil;
        end;
        Client.UserData   := 0;     { Reset the flag }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the FTP component needs to build a      }
{ directory listing. You can just return without doing anything then the    }
{ component will build the directory for you, based on the actual disk      }
{ content. But you can also build your own directory listing with anything  }
{ you like in it. Just create a stream with the required content. The       }
{ example below construct a virtual directory when the user is on the       }
{ C:\VIRTUAL subdirectory (use elsewhere in this sample program).           }
procedure TSslFtpServerForm.SslFtpServer1BuildDirectory(
    Sender        : TObject;
    Client        : TFtpCtrlSocket;
    var Directory : TFtpString;
    Detailed      : Boolean);
var
    Buf : String;
begin
    if UpperCase(Client.Directory) <> 'C:\VIRTUAL\' then
        Exit;
    InfoMemo.Lines.Add('! VIRTUAL DIR');
    Client.UserData   := 1;        { Remember we created a stream }
    if Assigned(Client.DataStream) then
        Client.DataStream.Destroy; { Prevent memory leaks         }
    Client.DataStream := TMemoryStream.Create;
    if Detailed then
        { We need to format directory lines according to the Unix standard }
        Buf :=
      '-rwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 FORBIDEN' + #13#10 +
      '-rwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 TEST' + #13#10 +
      'drwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 SOME DIR' + #13#10
    else
        Buf := 'FORBIDEN' + #13#10 +
               'TEST' + #13#10;
    Client.DataStream.Write(Buf[1], Length(Buf));
    Client.DataStream.Seek(0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called by the FTP component once it has built the   }
{ directory listing. We can use this handler to alter the listing, adding   }
{ or removing some info. This sample add the 'virtual' directory.           }
procedure TSslFtpServerForm.SslFtpServer1AlterDirectory(
    Sender        : TObject;
    Client        : TFtpCtrlSocket;
    var Directory : TFtpString;
    Detailed      : Boolean);
var
    Buf : String;
begin
    if UpperCase(Client.Directory) <> 'C:\' then
        Exit;
    { Add our 'virtual' directory to the list }
    if Detailed then begin
        { We need to format directory lines according to the Unix standard }
        Buf :=
        'drwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 VIRTUAL' + #13#10;
        Client.DataStream.Write(Buf[1], Length(Buf));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1ClientCommand(Sender: TObject;
  Client: TFtpCtrlSocket; var Keyword, Params, Answer: TFtpString);
begin
    InfoMemo.Lines.Add('< ' + Client.GetPeerAddr + ' ' +
                       Keyword + ' ' + Params);
    if (Keyword <> 'CCC') and (Keyword <> 'AUTH') and (Keyword <> 'QUIT') then
        CheckRenegotiation(TMyClient(Client), False);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1AnswerToClient(Sender: TObject;
  Client: TFtpCtrlSocket; var Answer: TFtpString);
begin
    InfoMemo.Lines.Add('> ' + Client.GetPeerAddr + ' ' + Answer)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1Authenticate(Sender: TObject;
  Client: TFtpCtrlSocket; UserName, Password: TFtpString;
  var Authenticated: Boolean);
begin
  { One Time Passwords - keep sequence and seed for next login attempt }
    if Client.OtpMethod > OtpKeyNone then begin
        if not Authenticated then exit;
        InfoMemo.Lines.Add('! ' + Client.SessIdInfo +
                                    ' is One Time Password authenticated');
        FOtpSequence := Client.OtpSequence;
        FOtpSeed := Client.OtpSeed;
    end
    else begin

        { You should place here the code needed to authenticate the user. }
        { For example a text file with all permitted username/password.   }
        { If the user can't be authenticated, just set Authenticated to   }
        { false before returning.                                         }
        { It is also the right place to setup Client.HomeDir              }
        { If you need to store info about the client for later processing }
        { you can use Client.UserData to store a pointer to an object or  }
        { a record with the needed info.                                  }

        { 1.12 authentication taken from INI file in OtpMethodEvent }
        if ((Client.UserName = UserName) and (Password <> '')) and
             ((Client.AccountPassword = Password) or (Client.AccountPassword = '*')) then { * anonymous logon }
            InfoMemo.Lines.Add('! ' + Client.SessIdInfo + ' is authenticated')
        else begin
            InfoMemo.Lines.Add('! ' + Client.SessIdInfo + ' failed authentication');
            Authenticated := FALSE;
        end;
        if Password = 'bad' then
            Authenticated := FALSE;
    end;
    if NOT Authenticated then exit;
    InfoMemo.Lines.Add('! ' + Client.SessIdInfo + ' Home Directory: ' + Client.HomeDir);

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1ChangeDirectory(Sender: TObject;
  Client: TFtpCtrlSocket; Directory: TFtpString; var Allowed: Boolean);
begin
{$IFDEF NEVER}
    { It the right place to check if a user has access to a given directory }
    { The example below disable C:\ access to non root user.                }
    if (UpperCase(Client.UserName) <> 'ROOT') and
       (UpperCase(Client.Directory) = 'C:\') then
       Allowed := FALSE;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.Cleardisplay1Click(Sender: TObject);
begin
    InfoMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.OpenSslVer1Click(Sender: TObject);
begin
    SslContext1.InitContext; //Pre-loads OpenSSL DLL's
    InfoMemo.Lines.Add(OpenSslVersion);
    InfoMemo.Lines.Add(OpenSslCompilerFlags);
    InfoMemo.Lines.Add(OpenSslBuiltOn);
    InfoMemo.Lines.Add(OpenSslPlatForm);
    InfoMemo.Lines.Add(OpenSslDir);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.MnuListClientsClick(Sender: TObject);
var
    I : Integer;
begin
    if SslFtpServer1.ClientCount <= 0 then begin
        InfoMemo.Lines.Add('No client');
        Exit;
    end;

    for I := 0 to SslFtpServer1.ClientCount - 1 do begin
        InfoMemo.Lines.Add('Client ' + IntToStr(I + 1) + ': ' +
                           SslFtpServer1.Client[I].GetPeerAddr + '/' +
                           SslFtpServer1.Client[I].GetPeerPort);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.DisconnectAllMnuClick(Sender: TObject);
begin
    SslFtpServer1.DisconnectAll;
    SslFtpServer2.DisconnectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}    { Sorry, Delphi 1 doesn't support multi-threading }
procedure TSslFtpServerForm.SslFtpServer1GetProcessing(
    Sender          : TObject;
    Client          : TFtpCtrlSocket;
    var DelayedSend : Boolean);
begin
    { Nothing to do here... }
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF VER80}    { 32 bits support multi-threading }
procedure TSslFtpServerForm.SslFtpServer1GetProcessing(
    Sender          : TObject;
    Client          : TFtpCtrlSocket;
    var DelayedSend : Boolean);
var
    MyServer : TFtpServer;
    MyClient : TMyClient;
begin
    MyServer := Sender as TFtpServer;
    MyClient := Client as TMyClient;
    { If client request a *.ZZZ file, then start a thread to do some      }
    { processing (here the thread just sleep 10 sec to show other clients }
    { are not blocked.                                                    }
    if UpperCase(ExtractFileExt(MyClient.FileName)) = '.ZZZ' then begin
        MyClient.FWorkerThread := TGetProcessingThread.Create(TRUE);
        MyClient.FWorkerThread.Server          := MyServer;
        MyClient.FWorkerThread.Client          := MyClient;
        MyClient.FWorkerThread.FreeOnTerminate := TRUE;
        MyClient.FWorkerThread.OnTerminate     := WorkerThreadTerminated;
    {$IF CompilerVersion >= 21} //D2010 TThread.Resume / Suspend deprecated
        MyClient.FWorkerThread.Start;
    {$ELSE}
        MyClient.FWorkerThread.Resume;
    {$IFEND}
        { Ask server component to not start sending immediately           }
        { We will ask to start sending from WorkerThreadTerminated event  }
        DelayedSend := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1Host(Sender: TObject; Client: TFtpCtrlSocket; Host: TFtpString;
  var Allowed: Boolean);
var
    fname: string ;
begin
{ HOST might be ftp.domain.com or [123.123.123.123]   }
    fname := SslFtpServerForm.FIniRoot + 'ftpaccounts-' + Lowercase (Host) + '.ini';
    if NOT FileExists (fname) then begin
        InfoMemo.Lines.Add('! Could not find Accounts File: ' + fname);
        Allowed := false;
        exit;
    end;
    Client.AccountIniName := fname;
    Allowed := true;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1Rein(Sender: TObject; Client: TFtpCtrlSocket; var Allowed: Boolean);
begin
    Allowed := true;
    InfoMemo.Lines.Add('! Reinitialise client accepted');
    Client.SessIdInfo := Client.GetPeerAddr + '=(Not Logged On)';
    InfoMemo.Lines.Add('! ' + Client.SessIdInfo + ' connected');
    Client.AccountIniName := SslFtpServerForm.FIniRoot + 'ftpaccounts-default.ini';
    Client.AccountReadOnly := true;
    Client.AccountPassword := '';
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1MakeDirectory(Sender: TObject; Client: TFtpCtrlSocket; Directory: TFtpString;
  var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1OtpGetPassword(Sender: TObject; Client: TFtpCtrlSocket; UserName: TFtpString;
  var UserPassword: string);
begin
    UserPassword := Client.AccountPassword;   // expected password will used to create OTP
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1OtpMethod(Sender: TObject; Client: TFtpCtrlSocket; UserName: TFtpString;
  var OtpMethod: TOtpMethod);
var
    IniFile : TIcsIniFile;
    S: string;
begin
    { look up user account to find One Time Password method, root directory, etc, blank password means no account}
    if NOT FileExists (Client.AccountIniName) then begin
        InfoMemo.Lines.Add('! Could not find Accounts File: ' + Client.AccountIniName);
        exit;
    end;
    InfoMemo.Lines.Add('! Opening Accounts File: ' + Client.AccountIniName);
    IniFile := TIcsIniFile.Create(Client.AccountIniName);
    Client.AccountPassword := IniFile.ReadString(UserName, KeyPassword, '');  // keep password to check later
    S := IniFile.ReadString(UserName, KeyOtpMethod, 'none');
    Client.AccountReadOnly := (IniFile.ReadString(UserName, KeyReadOnly, 'true') = 'true');
    Client.HomeDir := IniFile.ReadString(UserName, KeyHomeDir, 'c:\temp');
    Client.Directory := Client.HomeDir;
    if (IniFile.ReadString(UserName, KeyForceHomeDir, 'true') = 'true') then
                                   Client.Options := Client.Options + [ftpCdUpHome];
    if (IniFile.ReadString(UserName, KeyHidePhysicalPath, 'true') = 'true') then
                           Client.Options := Client.Options + [ftpHidePhysicalPath];
    if (IniFile.ReadString(UserName,  KeyForceSsl, 'false') = 'true') then begin
        if NOT Client.SslEnable then Client.AccountPassword := '';  // if SSL not enabled fail password
    end;
    IniFile.Free;

    { sequence and seed }
    OtpMethod := OtpGetMethod (S);
    Client.OtpSequence := FOtpSequence;
    Client.OtpSeed := FOtpSeed;

  { this could be user account information, SQL id or something }
    Client.SessIdInfo := Client.GetPeerAddr + '=' + UserName;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.WorkerThreadTerminated(Sender : TObject);
var
    MyThread : TGetProcessingThread;
    Answer   : TFtpString;
begin
    MyThread := Sender as TGetProcessingThread;
    MyThread.Server.DoStartSendData(MyThread.Client, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TGetProcessingThread.Execute;
begin
    Sleep(10000);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1SslVerifyPeer(Sender: TObject;
  var Ok: Integer; Cert: TX509Base);
begin
    if DisplaySslInfoCheckBox.Checked then
        InfoMemo.Lines.Add('Received certificate'#13#10 +
                           'Subject: ' + Cert.SubjectOneLine + #13#10 +
                           'Issuer: '  + Cert.IssuerOneLine);
    if OK <> 1 then begin
        if DisplaySslInfoCheckBox.Checked then
            InfoMemo.Lines.Add('Error msg: ' + Cert.VerifyErrMsg + #13#10 +
                               'In this example we accept any cert');
        OK := 1; //In this example we accept any client.
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.SslFtpServer1SslHandshakeDone(Sender: TObject;
  ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    Sock    : TWSocket;
    Str     : String;
    B       : Boolean;
begin
    if Sender is TFtpCtrlSocket then begin
         Sock := TWSocket(Sender as TFtpCtrlSocket);
        (Sender as TMyClient).LastSslHandshake := GetTickCount;
        Str    := 'CtrlSocket ';
        B      := (Sender as TMyClient).SslSessionReused;
    end
    else begin
        Sock := (Sender as TWSocket);
        Str    := 'DataSocket ';
        B      := Sock.SslSessionReused;
    end;
    if not DisplaySslInfoCheckBox.Checked then
        Exit;
    if ErrCode = 0 then
        InfoMemo.Lines.Add('! ' + Sock.GetPeerAddr + ' ' + Str + Sock.SslHandshakeRespMsg +
                                                    ', SessionReused ' + IntToStr(Ord(B)))   { V8.00 }
      { InfoMemo.Lines.Add(Format('! %s %s SslHandshakeDone. Secure ' +
                     'connection with %s, cipher %s, %d secret bits ' +
                     '(%d total), SessionReused %d',
                     [Sock.GetPeerAddr, Str, Sock.SslVersion,
                     Sock.SslCipher,
                     Sock.SslSecretBits, Sock.SslTotalBits,
                     Ord(B)]))  }
    else
        InfoMemo.Lines.Add('! ' + Sock.GetPeerAddr + ' ' + Str +
                           ' SslHandshake failed - ' + Sock.SslHandshakeRespMsg);  { V8.00 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Request SSL3 renegotiation                                                }
procedure TSslFtpServerForm.CheckRenegotiation(
    Client : TMyClient;
    IsData : Boolean);
var
    Ticks  : Longword;
    Socket : TWSocket;
    Str    : String;
begin
    if Client.SslEnable and Assigned(Client.SSL) and
       (FSslRenegotiationInterval > 0) then begin
        Ticks := GetTickCount;
        if Client.LastSslHandshake + FSslRenegotiationInterval < Ticks then begin
            if IsData then begin
                Socket := Client.DataSocket;
                Str    := 'DataSocket';
            end
            else begin
                Socket := TWSocket(Client as TFtpCtrlSocket);
                Str    := 'CtrlSocket';
            end;
            if not Socket.SslStartRenegotiation then begin
                if DisplaySslInfoCheckBox.Checked then
                    InfoMemo.Lines.Add('! ' + Socket.GetPeerAddr + ' ' + Str +
                                       ' SslStartRenegotiation failed.');
            end
            else
                if DisplaySslInfoCheckBox.Checked then
                    InfoMemo.Lines.Add('! ' + Client.GetPeerAddr + ' ' + Str +
                                       ' SSL renegotiation started.');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServerForm.RenegotiationIntervalEditChange(
    Sender: TObject);
begin
    try
        FSslRenegotiationInterval := StrToInt((Sender as TEdit).Text) * 1000;
    except
        FSslRenegotiationInterval := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.


