{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This is a demo program showing how to use the TFtpServer
              component to build a FTP server.
              Warning: As this demo is written, full access is given to all
              users to all files accessible by the computer running the demo.
              In production program, you should add code to implement
              security issues.
Creation:     April 21, 1998
Version:      8.54
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1998-2010 by François PIETTE
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
Sep 20, 2006  V1.07 A.Garrels added MD5 checksum caching.
              Note that I used a cache class from OverbyteIcsAvlTrees.pas,
              which can be optimized for this special use. If you use
              OverbyteIcsAvlTrees.pas in your application use FastMM4
              memory manager to avoid memory leaks, FastMM4 is included in
              BDS2006 as well as in Turbo versions.
Oct 29, 2006  V1.08 Added D2006 memory leak detection.
              Added compiler options and check for DELPHI7_UP.
May 22, 2007  V1.09 A.Garrels added demo-code that logs on a user
              Windows account and switches security context upon file
              system access. Added option to hide the physical path.
03 Mar 2008   V1.10 Angus added One Time Password authentication to Options menu
              added Root Directory edit box, saved in INI file and set during logon
              put directory listing into log to ease debugging
              lots of new events for new FTP server commands
              TCacheData has CRC32B, ZFileName and more admin information
               (not quite finished yet)
              set PasvPortRangeStart/End
Apr 14, 2008 V1.11 A. Garrels, a few Unicode related changes.
Nov 6, 2008, V1.12 Angus, support server V7.00 which does not use OverbyteIcsFtpSrvC
             Added ftpaccounts-default.ini file with user accounts setting defaults for each user
             Home directory, etc, now set from user account instead of common to all users
             ReadOnly account supported
             Note: random account names are no longer allowed for this demo
Nov 8, 2008, V1.13 Angus, support HOST and REIN(ialise) commands
             HOST ftp.ics.org would open account file ftpaccounts-ftp.ics.org.ini
             Added menu items for Display UTF8 and Display Directories
             If the account password is 'windows', authenticate against Windows
Nov 13, 2008 V1.14 Arno adjusted UTF-8/code page support.
Nov 21, 2008 V1.15 Angus removed raw display
Jun 10, 2010 V1.16 Angus added MaxKB bandwidth limit
Aug 8, 2011  V1.17 Angus constant FtpBuffSize = 32768 to increase FTP internal and data socket buffer sizes
May 2012 - V8.00 - this is a Windows only demo, IPv4 only
May 21, 2018 V8.54 GetUAgeSizeFile now IcsGetUAgeSizeFile


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
unit OverbyteIcsFtpServ1;
{$I Include\OverbyteIcsDefs.inc}
{$IFNDEF DELPHI7_UP}
    Bomb('This sample requires Delphi 7 or later');
{$ENDIF}
{$B-}                 { Enable partial boolean evaluation   }
{$T-}                 { Untyped pointers                    }
{$X+}                 { Enable extended syntax              }
{$I+}                 { Turn IO exceptions to on            }
{$H+}                 { Use long strings                    }
{$J+}                 { Allow typed constant to be modified }
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, Menus,
  OverbyteIcsFtpSrvT,
  OverbyteIcsWSocket,    OverbyteIcsWinsock,
  OverbyteIcsWndControl, OverbyteIcsFtpSrv,
  OverbyteIcsAvlTrees,   OverbyteIcsOneTimePw,
  OverbyteIcsUtils;

const
  FtpServVersion      = 854;
  CopyRight : String  = ' FtpServ (c) 1998-2018 F. Piette V8.54 ';
  WM_APPSTARTUP       = WM_USER + 1;
  FtpBuffSize         = 32768;  // V1.17

type
  TLogMsg = class(TComponent)
  public
     procedure Text(Prefix : Char; Msg : String);
  end;

  TCacheData = packed record
    MD5Sum    : String;
    CRC32B    : String;
    ZFileName : String;
    CFileUDT  : TDateTime;
    CFSize    : Int64;
    CReadCount: Integer;
    CLastTick : LongWord;
  end;
  PCacheData = ^TCacheData;

  TGetProcessingThread = class; { Forward declaration }

  { We use our own client class to hold our thread }
  TMyClient  = class(TFtpCtrlSocket)
  private
      FWorkerThread   : TGetProcessingThread;
      FAccessToken    : THandle;
  public
      constructor Create(AOwner: TComponent);override;
      destructor Destroy; override;
  end;

  TGetProcessingThread = class(TThread)
  protected
    Server : TFtpServer;
    Client : TMyClient;
  public
      procedure Execute; override;
  end;

  TFtpServerForm = class(TForm)
    FtpServer1: TFtpServer;
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
    Options1: TMenuItem;
    ForceHomeDir: TMenuItem;
    HidePhysicalPath: TMenuItem;
    Authenticateotpmd5: TMenuItem;
    Authenticateotpmd4: TMenuItem;
    Authenticateotpsha1: TMenuItem;
    Label1: TLabel;
    RootDirectory: TEdit;
    DisplayDirectories1: TMenuItem;
    Label13: TLabel;
    MaxKB: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FtpServer1ClientConnect(Sender: TObject;
      Client: TFtpCtrlSocket; Error: Word);
    procedure FtpServer1ClientDisconnect(Sender: TObject;
      Client: TFtpCtrlSocket; Error: Word);
    procedure FtpServer1Start(Sender: TObject);
    procedure FtpServer1Stop(Sender: TObject);
    procedure FtpServer1ClientCommand(Sender: TObject;
      Client: TFtpCtrlSocket; var Keyword, Params, Answer: TFtpString);
    procedure FtpServer1StorSessionConnected(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FtpServer1StorSessionClosed(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FtpServer1RetrDataSent(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FtpServer1RetrSessionConnected(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FtpServer1RetrSessionClosed(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FtpServer1AnswerToClient(Sender: TObject;
      Client: TFtpCtrlSocket; var Answer: TFtpString);
    procedure FtpServer1Authenticate(Sender: TObject;
      Client: TFtpCtrlSocket; UserName, Password: TFtpString;
      var Authenticated: Boolean);
    procedure FtpServer1ChangeDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; Directory: TFtpString; var Allowed: Boolean);
    procedure MnuQuitClick(Sender: TObject);
    procedure MnuStopServerClick(Sender: TObject);
    procedure MnuStartServerClick(Sender: TObject);
    procedure ImagesDblClick(Sender: TObject);
    procedure FtpServer1BuildDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; var Directory: TFtpString; Detailed: Boolean);
    procedure FtpServer1AlterDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; var Directory: TFtpString; Detailed: Boolean);
    procedure Cleardisplay1Click(Sender: TObject);
    procedure MnuListClientsClick(Sender: TObject);
    procedure DisconnectAllMnuClick(Sender: TObject);
    procedure FtpServer1GetProcessing(Sender: TObject;
      Client: TFtpCtrlSocket; var DelayedSend : Boolean);
    procedure FtpServer1Md5Calculated(Sender: TObject; Client: TFtpCtrlSocket;
      const FilePath, Md5Sum: TFtpString);
    procedure FtpServer1CalculateMd5(Sender: TObject; Client: TFtpCtrlSocket;
      var FilePath, Md5Sum: TFtpString; var Allowed: Boolean);
    procedure FtpServer1LeaveSecurityContext(Sender: TObject;
      Client: TFtpCtrlSocket);
    procedure FtpServer1EnterSecurityContext(Sender: TObject;
      Client: TFtpCtrlSocket);
    procedure ForceHomeDirClick(Sender: TObject);
    procedure HidePhysicalPathClick(Sender: TObject);
    procedure FtpServer1OtpMethodEvent(Sender: TObject; Client: TFtpCtrlSocket;
                UserName: TFtpString; var OtpMethod: TOtpMethod);
    procedure FtpServer1OtpGetPasswordEvent(Sender: TObject; Client: TFtpCtrlSocket;
                UserName: TFtpString; var UserPassword: String);
    procedure Authenticateotpmd5Click(Sender: TObject);
    procedure Authenticateotpmd4Click(Sender: TObject);
    procedure Authenticateotpsha1Click(Sender: TObject);
    procedure FtpServer1CalculateCrc(Sender: TObject;
      Client: TFtpCtrlSocket; var FilePath, Md5Sum: TFtpString;
      var Allowed: Boolean);
    procedure FtpServer1Combine(Sender: TObject; Client: TFtpCtrlSocket;
      var Params, Answer: TFtpString);
    procedure FtpServer1SiteExec(Sender: TObject; Client: TFtpCtrlSocket;
      var Params, Answer: TFtpString);
    procedure FtpServer1SiteMsg(Sender: TObject; Client: TFtpCtrlSocket;
      var Params, Answer: TFtpString);
    procedure FtpServer1SitePaswd(Sender: TObject; Client: TFtpCtrlSocket;
      var Params, Answer: TFtpString);
    procedure FtpServer1ValidateAllo(Sender: TObject;
      Client: TFtpCtrlSocket; var Params, Answer: TFtpString);
    procedure FtpServer1Display(Sender: TObject; Client: TFtpCtrlSocket;
      Msg: TFtpString);
    procedure FtpServer1UpCompressedFile(Sender: TObject;
      Client: TFtpCtrlSocket);
    procedure FtpServer1UpCompressFile(Sender: TObject;
      Client: TFtpCtrlSocket; var Done: Boolean);
    procedure FtpServer1Timeout(Sender: TObject; Client: TFtpCtrlSocket;
      Duration: Integer; var Abort: Boolean);
    procedure FtpServer1ValidatePut(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
      var Allowed: Boolean);
    procedure FtpServer1ValidateRnFr(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
      var Allowed: Boolean);
    procedure FtpServer1ValidateDele(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
      var Allowed: Boolean);
    procedure FtpServer1MakeDirectory(Sender: TObject; Client: TFtpCtrlSocket; Directory: TFtpString;
      var Allowed: Boolean);
    procedure FtpServer1ValidateMfmt(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
      var Allowed: Boolean);
    procedure FtpServer1Rein(Sender: TObject; Client: TFtpCtrlSocket; var Allowed: Boolean);
    procedure FtpServer1Host(Sender: TObject; Client: TFtpCtrlSocket; Host: TFtpString; var Allowed: Boolean);
    procedure DisplayDirectories1Click(Sender: TObject);
  private
    FInitialized      : Boolean;
    FIniFileName      : String;
    FPort             : String;
    FXTop             : Integer;
    FXLeft            : Integer;
    FXWidth           : Integer;
    FXHeight          : Integer;
    FMD5Cache         : TCacheTree;
    FOtpMethod        : TOtpMethod;
    FOtpSequence      : integer;
    FOtpSeed          : String;
    FIniRoot          : String;
    procedure CacheFreeData(Sender: TObject; Data: Pointer; Len: Integer);
    procedure WMAppStartup(var msg: TMessage); message WM_APPSTARTUP;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure StartServer;
    procedure StopServer;
    procedure UpdateClientCount;
    procedure WorkerThreadTerminated(Sender : TObject);
  end;

var
  FtpServerForm: TFtpServerForm;
  Log          : TLogMsg;

implementation

{$R *.DFM}

{ TMyClient }

constructor TMyClient.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    RcvSize := FtpBuffSize ;     // V1.17 increase FTP local xmit/receive buffer size
    RcvBufSize := FtpBuffSize ;  // V1.17 increase data socket receive buffer size
    SndBufSize := FtpBuffSize ;  // V1.17 increase data socket send buffer size
end;

destructor TMyClient.Destroy;
begin
  if FAccessToken <> 0 then begin
      CloseHandle(FAccessToken);
      FAccessToken := 0;
  end;
  inherited Destroy;
end;

const
    MainTitle         = 'FTP Server - http://www.overbyte.be';

    { server Ini file layout }
    SectionData       = 'Data';
    KeyPort           = 'Port';
    KeyRoot           = 'Root';

    SectionWindow     = 'Window';
    KeyTop            = 'Top';
    KeyLeft           = 'Left';
    KeyWidth          = 'Width';
    KeyHeight         = 'Height';
    KeyMinim          = 'RunMinimized';

    STATUS_GREEN      = 0;
    STATUS_YELLOW     = 1;
    STATUS_RED        = 2;

    { account INI file layout }
    KeyPassword             = 'Password';
    KeyHomeDir              = 'HomeDir';
    KeyOtpMethod            = 'OtpMethod';
    KeyForceHomeDir         = 'ForceHomeDir';
    KeyHidePhysicalPath     = 'HidePhysicalPath';
    KeyReadOnly             = 'ReadOnly';
    KeyForceSsl             = 'ForceSsl';
    KeyMaxKB                = 'MaxKB';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TLogMsg.Text(Prefix : Char; Msg : String);
begin
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
    Minim   : Integer;
begin
    if not FInitialized then begin
        FInitialized        := TRUE;
        Caption             := 'Starting ' + MainTitle;
        Left := -Width;

        IniFile  := TIcsIniFile.Create(FIniFileName);
        FXTop    := IniFile.ReadInteger(SectionWindow, KeyTop,    Top);
        FXLeft   := IniFile.ReadInteger(SectionWindow, KeyLeft,   Left);
        FXWidth  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        FXHeight := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Minim    := IniFile.ReadInteger(SectionWindow, KeyMinim,  0);
        RootDirectory.Text := IniFile.ReadString(SectionData, KeyRoot, 'c:\temp');
        MaxKB.Text := IniFile.ReadString(SectionData, KeyMaxKB, '0');

        IniFile.Free;

        LoadConfig;
        SaveConfig;    { Create the inifile keys if they don't exists }
        FOtpMethod := OtpKeyNone;

        { Be sure to always have the window visible }
        { with a reasonable width and height        }
        if FXLeft < 0 then
            FXLeft := 0;
        if FXTop < 0 then
            FXTop := 0;
        if FXWidth < 310 then
            FXWidth := 310;
        if FXHeight <= 250 then
            FXHeight := 250;
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
procedure TFtpServerForm.FormClose(Sender: TObject;
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
        IniFile.WriteString(SectionData,    KeyRoot,   RootDirectory.Text);
        IniFile.WriteString(SectionData,    KeyMaxKB,  MaxKB.Text);
        IniFile.UpdateFile;
        IniFile.Free;
    except
        { Ignore any exception when we are closing }
    end;
    FMD5Cache.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.LoadConfig;
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    FPort   := IniFile.ReadString(SectionData,    KeyPort,   'ftp');
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SaveConfig;
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyPort, FPort);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This message handler is triggered by the FormShow event. We comes here    }
{ only when the form is visible on screen.                                  }
procedure TFtpServerForm.WMAppStartup(var msg: TMessage);
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

    StartServer;
    UpdateClientCount;
end;


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
procedure TFtpServerForm.FormCreate(Sender: TObject);
begin
    { Build Ini file name }
    FIniFileName := GetIcsIniFileName;
    FIniRoot := LowerCase(ExtractFilePath(Application.ExeName));
    { Create the Log object }
    Log                  := TLogMsg.Create(Self);
    FMD5Cache            := TCacheTree.Create(FALSE);
    FMD5Cache.OnFreeData := CacheFreeData;
{    BigConsole(80, 100); }
    InfoMemo.Clear;
    GreenImage.Visible   := FALSE;
    RedImage.Visible     := TRUE;
    RedImage.Top         := GreenImage.Top;
    RedImage.Left        := GreenImage.Left;
{$IFDEF DELPHI10}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.StartServer;
var
    wsi     : TWSADATA;
{$IFDEF BUILTIN_THROTTLE}
    bandwidth: integer;
{$ENDIF}
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
            Format('%d.%d', [wsi.wHighVersion shr 8,
                             wsi.wHighVersion and 15]));
    InfoMemo.Lines.Add('        ' + String(wsi.szDescription));
    InfoMemo.Lines.Add('        ' + String(wsi.szSystemStatus));
{$IFNDEF VER100}
    { A bug in Delphi 3 makes lpVendorInfo invalid }
    if wsi.lpVendorInfo <> nil then
        InfoMemo.Lines.Add('        ' + String(wsi.lpVendorInfo));
{$ENDIF}
    { If not running 16 bits, we use our own client class }
    FtpServer1.ClientClass := TMyClient;
    { Use a custom multiline banner }
    FtpServer1.Banner := '220-Welcome to my Server' + #13#10 +
                         '220-' + #13#10 +
                         '220 ICS FTP Server ready.';
{$IFDEF BUILTIN_THROTTLE}
    bandwidth := StrToInt(MaxKB.Text);
    if bandwidth > 0 then
        FtpServer1.BandwidthLimit := bandwidth * 1024  { limit server bandwidth }
    else
        FtpServer1.BandwidthLimit := 0;
{$ENDIF}
    FtpServer1.Port   := FPort;
    FtpServer1.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.StopServer;
begin
    FtpServer1.Stop;
    FtpServer1.DisconnectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.MnuQuitClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.MnuStopServerClick(Sender: TObject);
begin
    StopServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.MnuStartServerClick(Sender: TObject);
begin
    StartServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.ImagesDblClick(Sender: TObject);
begin
    if FtpServer1.Active then
        StopServer
    else
        StartServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.UpdateClientCount;
begin
    if FtpServer1.ClientCount = 0 then
        ClientCountLabel.Caption := 'No user'
    else
        ClientCountLabel.Caption := IntToStr(FtpServer1.ClientCount) + ' users';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ClientConnect(Sender: TObject;
  Client: TFtpCtrlSocket; Error: Word);
begin
    { The next test shows how to refuse a client }
    if Client.GetPeerAddr = '193.121.12.25' then begin
        Client.SendStr('421 Connection not allowed.' + #13#10);
        Client.Close;
        Exit;
    end;
    Client.SessIdInfo := Client.GetPeerAddr + '=(Not Logged On)';
    InfoMemo.Lines.Add('! ' + Client.SessIdInfo + ' connected');
  { get INI file for default accounts, may be changed if HOST command used }
    Client.AccountIniName := FtpServerForm.FIniRoot + 'ftpaccounts-default.ini';
    Client.AccountReadOnly := true;
    Client.AccountPassword := '';
    UpdateClientCount;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1Host(Sender: TObject; Client: TFtpCtrlSocket; Host: TFtpString;
  var Allowed: Boolean);
var
    fname: string ;
begin
{ HOST might be ftp.domain.com or [123.123.123.123]   }
    fname := FtpServerForm.FIniRoot + 'ftpaccounts-' + Lowercase (Host) + '.ini';
    if NOT FileExists (fname) then begin
        InfoMemo.Lines.Add('! Could not find Accounts File: ' + fname);
        Allowed := false;
        exit;
    end;
    Client.AccountIniName := fname;
    Allowed := true;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1Rein(Sender: TObject; Client: TFtpCtrlSocket; var Allowed: Boolean);
begin
    Allowed := true;
    InfoMemo.Lines.Add('! Reinitialise client accepted');
    Client.SessIdInfo := Client.GetPeerAddr + '=(Not Logged On)';
    InfoMemo.Lines.Add('! ' + Client.SessIdInfo + ' connected');
    Client.AccountIniName := FtpServerForm.FIniRoot + 'ftpaccounts-default.ini';
    Client.AccountReadOnly := true;
    Client.AccountPassword := '';
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ClientDisconnect(Sender: TObject;
  Client: TFtpCtrlSocket; Error: Word);
begin
    InfoMemo.Lines.Add('! ' + Client.SessIdInfo + ' disconnected after ' +
             IntToStr (IcsElapsedSecs (Client.SessStartTick)) +
              ' secs, total recv ' + IntToKbyte (Client.TotPutBytes) +
                      ', total xmit ' + IntToKbyte (Client.TotGetBytes) ) ;
    if FtpServer1.ClientCount -1 = 0 then
        ClientCountLabel.Caption := 'No user'
    else
        ClientCountLabel.Caption := IntToStr(FtpServer1.ClientCount - 1) + ' users';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1Start(Sender: TObject);
begin
    GreenImage.Visible := TRUE;
    RedImage.Visible   := FALSE;
    InfoMemo.Lines.Add('! Server started');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1Stop(Sender: TObject);
begin
    GreenImage.Visible := FALSE;
    RedImage.Visible   := TRUE;
    InfoMemo.Lines.Add('! Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1StorSessionConnected(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.SessIdInfo +
                           ' Data session failed to open. Error #' +
                           IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1StorSessionClosed(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.SessIdInfo +
                           ' Data session closed. Error #' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1RetrDataSent(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.SessIdInfo +
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
procedure TFtpServerForm.FtpServer1RetrSessionConnected(Sender: TObject;
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    Error  : Word);
var
    Buf : AnsiString;
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.SessIdInfo +
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
               'The request was: ''' + AnsiString(Client.FilePath) + '''' + #13#10;
        Client.DataStream.Write(Buf[1], Length(Buf));
        Client.DataStream.Seek(0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1RetrSessionClosed(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; Error: Word);
begin
    if Error <> 0 then
        InfoMemo.Lines.Add('! ' + Client.SessIdInfo +
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
procedure TFtpServerForm.FtpServer1BuildDirectory(
    Sender        : TObject;
    Client        : TFtpCtrlSocket;
    var Directory : TFtpString;
    Detailed      : Boolean);
var
    Buf : AnsiString;
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
procedure TFtpServerForm.FtpServer1AlterDirectory(
    Sender        : TObject;
    Client        : TFtpCtrlSocket;
    var Directory : TFtpString;
    Detailed      : Boolean);
var
    Buf : String;
begin
  { Arno - show physical file path we listed, and the result - the stream   }
  { might be already encoded with a different ANSI code page inluding UTF-8.}
  { Pass property Client.CurrentCodePage or the system code page to         }
  { function Client.DataStreamReadString() to display a decoded string.     }
    if DisplayDirectories1.Checked then begin
        InfoMemo.Lines.Add('! ' + Client.SessIdInfo +
                           ' Directory Listing Path: ' + Client.DirListPath) ;
        if Assigned (Client.DataStream) then begin
            Client.DataStream.Seek(0, 0);
            Client.DataStreamReadString(Buf, Client.DataStream.Size,
                                                    Client.CurrentCodePage);
            Client.DataStream.Seek(0, 0);
            InfoMemo.Lines.Add(Buf);
        end;
    end;
    if UpperCase(Client.Directory) <> 'C:\' then
        Exit;
    { Add our 'virtual' directory to the list }
    if Detailed then begin
        { We need to format directory lines according to the Unix standard }
        Buf :=
        'drwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 VIRTUAL' + #13#10;
        Client.DataStreamWriteString(Buf, Client.CurrentCodePage);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ClientCommand(Sender: TObject;
  Client: TFtpCtrlSocket; var Keyword, Params, Answer: TFtpString);
begin
    InfoMemo.Lines.Add('< ' + Client.SessIdInfo + ' ' + Keyword + ' ' + Params);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1AnswerToClient(
    Sender     : TObject;
    Client     : TFtpCtrlSocket;
    var Answer : TFtpString);
begin
    InfoMemo.Lines.Add('> ' + Client.SessIdInfo + ' [' +
                  IntToStr (Client.ReqDurMilliSecs) + 'ms] ' + Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1OtpMethodEvent(
    Sender: TObject;
    Client: TFtpCtrlSocket;
    UserName: TFtpString;
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
    Client.HomeDir := IniFile.ReadString(UserName, KeyHomeDir, RootDirectory.Text);
    Client.Directory := Client.HomeDir;
    if (IniFile.ReadString(UserName, KeyForceHomeDir, 'true') = 'true') then
                                   Client.Options := Client.Options + [ftpCdUpHome];
    if (IniFile.ReadString(UserName, KeyHidePhysicalPath, 'true') = 'true') then
                           Client.Options := Client.Options + [ftpHidePhysicalPath];
    if (IniFile.ReadString(UserName,  KeyForceSsl, 'false') = 'true') then
                                       Client.AccountPassword := '';  // SSL not supported so fail password
    IniFile.Free;

    { sequence and seed }
    OtpMethod := OtpGetMethod (S);
    Client.OtpSequence := FOtpSequence;
    Client.OtpSeed := FOtpSeed;

  { this could be user account information, SQL id or something }
    Client.SessIdInfo := Client.GetPeerAddr + '=' + UserName;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1OtpGetPasswordEvent(
    Sender: TObject;
    Client: TFtpCtrlSocket;
    UserName: TFtpString;
    var UserPassword: String);
begin
    UserPassword := Client.AccountPassword;   // expected password will used to create OTP
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1Authenticate(
    Sender             : TObject;
    Client             : TFtpCtrlSocket;
    UserName, Password : TFtpString;
    var Authenticated  : Boolean);
var
    MyClient : TMyClient;
begin
    MyClient := Client as TMyClient;

  { One Time Passwords - keep sequence and seed for next login attempt }
    if Client.OtpMethod > OtpKeyNone then begin
        if not Authenticated then exit;
        InfoMemo.Lines.Add('! ' + Client.SessIdInfo +
                                    ' is One Time Password authenticated');
        FOtpSequence := Client.OtpSequence;
        FOtpSeed := Client.OtpSeed;
    end
    else if (Client.AccountPassword <> 'windows') then begin

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
    end
    else begin

        { If the client is already logged in log her off }
        if MyClient.FAccessToken <> 0 then begin
            CloseHandle(MyClient.FAccessToken); //logoff
            MyClient.FAccessToken := 0;
        end;

        { We call LogonUser API (see the PSDK) to get an access token }
        { that we can use to impersonate the user, see event handlers }
        { OnEnterSecurityContext and OnLeaveSecurityContext.          }

        Authenticated := LogonUser(PChar(UserName),
                                   nil,
                                   PChar(Password),
                                   LOGON32_LOGON_NETWORK,
                                   LOGON32_PROVIDER_DEFAULT,
                                   MyClient.FAccessToken);

        if Authenticated then
            InfoMemo.Lines.Add('! ' + Client.SessIdInfo +
                               ' User ''' + UserName +
                               ''' is authenticated and logged on locally to Windows')
        else
            InfoMemo.Lines.Add('! ' + Client.SessIdInfo +
                               ' User ''' + UserName + ''' not authenticated to Windows');

    end;
    if NOT Authenticated then exit;

    { Set the home and current directory - now done in OtpMethodEvent }
//    Client.HomeDir   := RootDirectory.Text;
//    Client.Directory := Client.HomeDir;
    InfoMemo.Lines.Add('! ' + Client.SessIdInfo + ' Home Directory: ' + Client.HomeDir);

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event is triggered before file system access.                        }
{ We may want to impersonate client's access token.                         }
procedure TFtpServerForm.FtpServer1EnterSecurityContext(
    Sender : TObject;
    Client : TFtpCtrlSocket);
var
    MyClient : TMyClient;
begin
    MyClient := Client as TMyClient;
    if MyClient.FAccessToken <> 0 then
        if not ImpersonateLoggedOnUser(MyClient.FAccessToken) then
            RaiseLastOsError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event is triggered after file system access finished.                }
{ If we impersonated this client we MUST revert security context to self.   }
procedure TFtpServerForm.FtpServer1LeaveSecurityContext(
    Sender : TObject;
    Client : TFtpCtrlSocket);
var
    MyClient : TMyClient;
begin
    MyClient := Client as TMyClient;
    if MyClient.FAccessToken <> 0 then
         if not RevertToSelf then
             RaiseLastOsError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ChangeDirectory(
    Sender      : TObject;
    Client      : TFtpCtrlSocket;
    Directory   : TFtpString;
    var Allowed : Boolean);
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
procedure TFtpServerForm.Cleardisplay1Click(Sender: TObject);
begin
    InfoMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.ForceHomeDirClick(Sender: TObject);
var
    I : Integer;
begin
    if TMenuItem(Sender).Checked then
    begin
        FtpServer1.Options := FtpServer1.Options + [ftpsCdUpHome];
        for I := 0 to FtpServer1.ClientCount -1 do
            FtpServer1.Client[I].Options := FtpServer1.Client[I].Options +
                                            [ftpCdUpHome];
    end
    else begin
        FtpServer1.Options := FtpServer1.Options - [ftpsCdUpHome];
        for I := 0 to FtpServer1.ClientCount -1 do
            FtpServer1.Client[I].Options := FtpServer1.Client[I].Options -
                                            [ftpCdUpHome];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.HidePhysicalPathClick(Sender: TObject);
var
    I : Integer;
begin
    if TMenuItem(Sender).Checked then
    begin
        FtpServer1.Options := FtpServer1.Options + [ftpsHidePhysicalPath];
        for I := 0 to FtpServer1.ClientCount -1 do
            FtpServer1.Client[I].Options := FtpServer1.Client[I].Options +
                                            [ftpHidePhysicalPath];
    end
    else begin
        FtpServer1.Options := FtpServer1.Options - [ftpsHidePhysicalPath];
        for I := 0 to FtpServer1.ClientCount -1 do
            FtpServer1.Client[I].Options := FtpServer1.Client[I].Options -
                                            [ftpHidePhysicalPath];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.MnuListClientsClick(Sender: TObject);
var
    I : Integer;
begin
    if FtpServer1.ClientCount <= 0 then begin
        InfoMemo.Lines.Add('No client');
        Exit;
    end;

    for I := 0 to FtpServer1.ClientCount - 1 do begin
        InfoMemo.Lines.Add('Client ' + IntToStr(I + 1) + ': ' +
                           FtpServer1.Client[I].GetPeerAddr + '/' +
                           FtpServer1.Client[I].GetPeerPort);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.DisconnectAllMnuClick(Sender: TObject);
begin
    FtpServer1.DisconnectAll;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.DisplayDirectories1Click(Sender: TObject);
begin
    DisplayDirectories1.Checked := NOT DisplayDirectories1.Checked;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1GetProcessing(
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
        MyClient.FWorkerThread.Resume;
        { Ask server component to not start sending immediately           }
        { We will ask to start sending from WorkerThreadTerminated event  }
        DelayedSend := TRUE;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.WorkerThreadTerminated(Sender : TObject);
var
    MyThread : TGetProcessingThread;
    Answer   : TFtpString;
begin
    MyThread := Sender as TGetProcessingThread;
    MyThread.Server.DoStartSendData(MyThread.Client, Answer);  { angus }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TGetProcessingThread.Execute;
begin
    Sleep(10000);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.CacheFreeData(
    Sender : TObject;
    Data   : Pointer;
    Len    : Integer);
begin
    Dispose(PCacheData(Data));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1CalculateMd5(
    Sender               : TObject;
    Client               : TFtpCtrlSocket;
    var FilePath, Md5Sum : TFtpString;
    var Allowed          : Boolean);
var
    Node : TCacheNode;
    FileUDT: TDateTime;
    FSize: Int64;
begin
    { Before the server calculates the MD5 checksum for a file this event   }
    { is to provide a MD5 checksum, mostly from some cache. MD5 calculation }
    { may take a while specially upon big files.                            }
    if not IcsGetUAgeSizeFile (Filepath, FileUDT, FSize) then exit ;
        Node := FMD5Cache.FindKey(FilePath);
    if (Node <> nil) and (PCacheData(Node.Data)^.CFileUDT = FileUDT) and
                            (PCacheData(Node.Data)^.CFSize = FSize) then begin
       { only return cache md5sum if getting it for entire file }
        if (Client.HashEndPos = 0) or (Client.HashEndPos = FSize) then begin
            Md5Sum := PCacheData(Node.Data)^.MD5Sum;
            inc (PCacheData(Node.Data)^.CReadCount); { so we know how useful was }
            PCacheData(Node.Data)^.CLastTick := IcsGetTickCountX;
            InfoMemo.Lines.Add('Read MD5sum from cache: ' + Md5Sum);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1Md5Calculated(Sender: TObject;
  Client: TFtpCtrlSocket; const FilePath, Md5Sum: TFtpString);
var
    PData : PCacheData;
    Node  : TCacheNode;
    FileUDT : TDateTime;
    FSize : Int64;
begin
    { This event triggeres either when the internal MD5 calculation     }
    { finished or the cached value needs to be updated, an empty Md5Sum }
    { signals to delete the cached entry except when a file has been    }
    { renamed.                                                          }
    if (Client.CurCmdType = ftpcRNTO) then begin
        Node := FMD5Cache.FindKey(Client.FromFileName);
        if (Node <> nil) then begin
            New(PData);
            PData^.Md5Sum    := PCacheData(Node.Data)^.MD5Sum;
            PData^.CRC32B    := PCacheData(Node.Data)^.CRC32B;
            PData^.ZFileName := PCacheData(Node.Data)^.ZFileName;
            PData^.CFileUDT  := PCacheData(Node.Data)^.CFileUDT;
            PData^.CFSize    := PCacheData(Node.Data)^.CFSize;
            PData^.CReadCount := PCacheData(Node.Data)^.CReadCount;
            PData^.CLastTick := IcsGetTickCountX;
            FMD5Cache.Insert(FilePath, PData, SizeOf(TCachedata));
            FMD5Cache.Remove(Node);
            InfoMemo.Lines.Add('Renamed cache for ' + Client.FromFileName +
                               ' to ' + FilePath);
        end;
        exit;
    end;

    if IcsGetUAgeSizeFile (FilePath, FileUDT, FSize) then begin
        if Md5Sum <> '' then begin
          { only update cache md5sum if got it for entire file }
            if (Client.HashEndPos <> 0) AND (Client.HashEndPos <> FSize) then
                exit;
            Node := FMD5Cache.FindKey(Client.FromFileName);
            if (Node <> nil) then begin
                PCacheData(Node.Data)^.MD5Sum := Md5Sum;
                PCacheData(Node.Data)^.CLastTick := IcsGetTickCountX;
              { if cached file has changed, clear old details }
                if (PCacheData(Node.Data)^.CFileUDT <> FileUDT) OR
                            (PCacheData(Node.Data)^.CFSize <> FSize) then begin
                    PCacheData(Node.Data)^.CFileUDT := FileUDT;
                    PCacheData(Node.Data)^.CFSize := FSize;
                    PCacheData(Node.Data)^.CRC32B := '' ;
                    if PCacheData(Node.Data)^.ZFileName <> '' then begin
                        InfoMemo.Lines.Add('Deleted Old cached compressed file ' +
                                           PCacheData(Node.Data)^.ZFileName);
                        DeleteFile (PCacheData(Node.Data)^.ZFileName);
                    end;
                    PCacheData(Node.Data)^.ZFileName := '';
                end;
                InfoMemo.Lines.Add('Old cached MD5sum updated for file ' +
                                   FilePath + '=' + Md5Sum);
            end
            else begin
            New(PData);
            PData^.Md5Sum    := Md5Sum;
                PData^.CFileUDT  := FileUDT;
                PData^.CFSize    := FSize;
                PData^.CReadCount := 0;
                PData^.CLastTick := IcsGetTickCountX;
            FMD5Cache.Insert(FilePath, PData, SizeOf(TCachedata));
            end;
            InfoMemo.Lines.Add('Cached MD5sum for new file ' +
                               FilePath + '=' + Md5Sum);
        end
        else
            FMD5Cache.RemoveKey(FilePath);
    end
    else
        FMD5Cache.RemoveKey(FilePath);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.Authenticateotpmd5Click(Sender: TObject);
begin
    Authenticateotpmd5.Checked := NOT Authenticateotpmd5.Checked;
    FOtpMethod := OtpKeyNone;
    if Authenticateotpmd5.Checked then begin
        FOtpMethod := OtpKeyMd5;
        Authenticateotpmd4.Checked := false;
        Authenticateotpsha1.Checked := false;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.Authenticateotpmd4Click(Sender: TObject);
begin
    Authenticateotpmd4.Checked := NOT Authenticateotpmd4.Checked;
    FOtpMethod := OtpKeyNone;
    if Authenticateotpmd4.Checked then begin
        FOtpMethod := OtpKeyMd4;
        Authenticateotpmd5.Checked := false;
        Authenticateotpsha1.Checked := false;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.Authenticateotpsha1Click(Sender: TObject);
begin
    Authenticateotpsha1.Checked := NOT Authenticateotpsha1.Checked;
    FOtpMethod := OtpKeyNone;
    if Authenticateotpsha1.Checked then begin
        FOtpMethod := OtpKeySha1;
        Authenticateotpmd4.Checked := false;
        Authenticateotpmd5.Checked := false;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1CalculateCrc(Sender: TObject;
  Client: TFtpCtrlSocket; var FilePath, Md5Sum: TFtpString;
  var Allowed: Boolean);
begin
    { read from CRC cache or let server do it }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1Combine(Sender: TObject;
  Client: TFtpCtrlSocket; var Params, Answer: TFtpString);
var
    File1, File2, File3, File4: String;
    Offset: Integer;
begin
    Offset := 1 ;
    File1 := ScanGetNextArg (Params, Offset);
    File2 := ScanGetNextArg (Params, Offset);
    File3 := ScanGetNextArg (Params, Offset);
    File4 := ScanGetNextArg (Params, Offset);
    InfoMemo.Lines.Add('Client wants to combine several files: ' + Params);
    Answer := '501 Can not combine files';
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1SiteExec(Sender: TObject;
  Client: TFtpCtrlSocket; var Params, Answer: TFtpString);
begin
    InfoMemo.Lines.Add('Client wants to run program: ' + Params);
    Answer := '501 Can not run program';
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1SiteMsg(Sender: TObject;
  Client: TFtpCtrlSocket; var Params, Answer: TFtpString);
begin
    InfoMemo.Lines.Add('Log client message: ' + Params);
    Answer := '200 Noted OK';
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1SitePaswd(Sender: TObject;
  Client: TFtpCtrlSocket; var Params, Answer: TFtpString);
var
    OldPw, NewPw: String;
    Offset: Integer;
begin
    Offset := 1 ;
    OldPw := ScanGetNextArg (Params, Offset);
    NewPw := ScanGetNextArg (Params, Offset);
    InfoMemo.Lines.Add('Client want to change password from: ' +
                       OldPw + ' to ' + NewPw);
    Answer := '501 Can not change password';
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ValidateAllo(Sender: TObject;
  Client: TFtpCtrlSocket; var Params, Answer: TFtpString);
begin
    { this event can be used to check if account has sufficient space for }
    { upload and format the correct answers, otherwise the command checks }
    { space on the Client volume  }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ValidateDele(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
  var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ValidateMfmt(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
  var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ValidatePut(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
  var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1ValidateRnFr(Sender: TObject; Client: TFtpCtrlSocket; var FilePath: TFtpString;
  var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1MakeDirectory(Sender: TObject; Client: TFtpCtrlSocket; Directory: TFtpString;
  var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1Display(Sender: TObject;
  Client: TFtpCtrlSocket; Msg: TFtpString);
begin
    InfoMemo.Lines.Add('! ' + Client.SessIdInfo + ' ' + Msg);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1UpCompressFile(Sender: TObject;
  Client: TFtpCtrlSocket; var Done: Boolean);
begin
// !!! TEMP should use proper cache stuff
    exit ;
    if NOT FileExists (Client.ZCompFileName) then Exit;
    try
       Client.ZFileStream := FtpServer1.OpenFileStream(Client.ZCompFileName,
                                                  fmOpenRead + fmShareDenyNone);
       Client.ZCompFileDelete := False ;
       Done := True ;
       InfoMemo.Lines.Add('Opened cached compressed file: ' +
                          Client.ZCompFileName);
    except
       InfoMemo.Lines.Add('Failed to open file: ' + Client.ZCompFileName);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1UpCompressedFile(Sender: TObject;
  Client: TFtpCtrlSocket);
begin
   InfoMemo.Lines.Add('Save cache file?: ' + Client.ZCompFileName);

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FtpServer1Timeout(Sender: TObject;
  Client: TFtpCtrlSocket; Duration: Integer; var Abort: Boolean);
begin
    InfoMemo.Lines.Add('! ' + Client.SessIdInfo +
                       ' client being timed out after ' + IntToStr (Duration) +
                       ' secs inactivity');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.


