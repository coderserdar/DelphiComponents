{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Oct 10, 1999
Description:  WebSrv1 show how to use THttpServer component to implement
              a web server with dynamic page capabilities. To see dynamic pages
              in action, launch the demo and point your browser to:
              http://localhost/demo.htm (replace "localhost" by the name of the
              computer where you run the demo if it is not on the same computer
              as the browser). Demo.htm is a dynamic page (that is generated
              by code) that show a menu for other dynamic pages. All dynamic
              pages are implemented in procedure whose names begins by
              "CreateVirtualDocument". See the code below.
              WARNING: The code below is for demonstration only.
              You need to add code to fit your needs about security.
              The code below allows to get all files on the computer running
              the demo. Add code in OnGetDocument, OnHeadDocument and
              OnPostDocument to check for authorized access to files.
Version:      7.18
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1999-2009 by François PIETTE
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

History:
May 21, 2000 V1.01 Worked around a bug with Delphi 3 and lpVendorInfo
Oct 07, 2001 V1.02 Added Logfile feature
                   Added display if time and IP Addr for GET command.
Feb 15, 2003 V1.03 Call PostedDataReceived so that things are handled
                   correctly with HTTP 1.1 version.
Mar 11, 2003 V1.04 Changer LingerOnOff to LingerNotSet (Wilfried)
Jan 03, 2004 V1.05 Added DirList feature checkbox
Jan 15, 2004 V1.06 Added RedirURLEdit feature.
Jan 16, 2004 V1.07 Simplified virtual document procedure.
                   Added demo_htm, form_htm and formdata.htm virtual documents.
                   Now demo.htm link show a menu for all demonstrated features.
                   And all virtual documents are linked back to the demo menu.
                   NOTE: The code will not work properly with Delphi 1 because
                         Delphi 1 is limited to 255 character strings. To use
                         the demo, you must rewrite some code, mostly the
                         code involving the Answer method. Instead of using
                         Answer, you must prepare the complete header yourself
                         and send it using ont or more PutStringInSendBuffer,
                         and then send the document body using yet more
                         PutStringInSendBuffer or using DocStream.
Sep 11, 2004 V1.08 Added virtual page to show client IP address. The code is
                   implemented in CreateVirtualDocument_myip_htm.
Jan 09, 2005 V1.09 Added demos with AnswerPage showing how to use a template
                   file with or without embedded table.
                   Added TEdit for template directory.
                   Restructured the web file organisation.
Mar 04, 2006 V1.10 Added authentication demo pages.
Sep 07, 2006 V1.11 A. Garrels added NTLM authentication demo page.
Oct 29, 2006 V1.12 Made authentication demo pages better.
                   Added compiler switches and DELPHI7_UP check.
                   Added D2006 memory leak detection
Nov 05, 2006 V1.13 Removed IsDirectory function which wasn't used
Apr 14, 2008 V1.14 A. Garrels, a few Unicode related changes.
May 15, 2008 V1.15 A. Garrels, a few more Unicode related changes, removed some
                   ifdefs for older compiler.
Nov 03, 2008 V7.16 A. Garrels Added Keep-Alive timeout and a maximum number
                   of allowed requests during a persistent connection. Set
                   property KeepAliveTimeSec to zero in order disable this
                   feature entirely, otherwise persistent connections are dropped
                   either after an idle time of value KeepAliveTimeSec or if the
                   maximum number of requests (property MaxRequestKeepAlive) is
                   reached.
Nov 05, 2008 V7.17 A. Garrels made the POST demo UTF-8 aware.
Jan 03, 2009 V7.18 A. Garrels added some lines to force client browser's login
                   dialog when the nonce is stale with digest authentication.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWebServ1;

{$I OverbyteIcsDefs.inc}
{$IFNDEF DELPHI7_UP}
    Bomb('This sample requires Delphi 7 or later');
{$ENDIF}
{$B-}                 { Enable partial boolean evaluation   }
{$T-}                 { Untyped pointers                    }
{$X+}                 { Enable extended syntax              }
{$I+}                 { Turn IO exceptions to on            }
{$H+}                 { Use long strings                    }
{$J+}                 { Allow typed constant to be modified }
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  OFF}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Controls, Forms,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, StrUtils,
  OverbyteIcsWinSock,  OverbyteIcsWSocket, OverbyteIcsWndControl,
  OverbyteIcsHttpSrv, OverbyteIcsUtils;

const
  WebServVersion     = 718;
  CopyRight : String = 'WebServ (c) 1999-2009 F. Piette V7.18 ';
  NO_CACHE           = 'Pragma: no-cache' + #13#10 + 'Expires: -1' + #13#10;
  WM_CLIENT_COUNT    = WM_USER + WH_MAX_MSG + 1;


type
  { This component is used for client connection instead of default one.    }
  { This enables adding any data we need to handle our application.         }
  { As this data is located in client component, each connected client has  }
  { his own private data.                                                   }
  TMyHttpConnection = class(THttpConnection)
  protected
    FPostedRawData    : PAnsiChar; { Will hold dynamically allocated buffer }
    FPostedDataBuffer : PChar;     { Contains either Unicode or Ansi data   }
    FPostedDataSize   : Integer;   { Databuffer size                        }
    FDataLen          : Integer;   { Keep track of received byte count.     }
    FDataFile         : TextFile;  { Used for datafile display              }
    FFileIsUtf8       : Boolean;
  public
    destructor  Destroy; override;
  end;

  { This is the main form for our application. Any data here is global for  }
  { all clients. Put private data in TMyHttpConnection class (see above).   }
  TWebServForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    HttpServer1: THttpServer;
    Label1: TLabel;
    DocDirEdit: TEdit;
    Label2: TLabel;
    DefaultDocEdit: TEdit;
    StartButton: TButton;
    StopButton: TButton;
    Label3: TLabel;
    PortEdit: TEdit;
    ClientCountLabel: TLabel;
    Label5: TLabel;
    ClearButton: TButton;
    DisplayHeaderCheckBox: TCheckBox;
    WriteLogFileCheckBox: TCheckBox;
    DirListCheckBox: TCheckBox;
    OutsideRootCheckBox: TCheckBox;
    Label4: TLabel;
    RedirURLEdit: TEdit;
    TemplateDirEdit: TEdit;
    Label6: TLabel;
    KeepAliveTimeSecEdit: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    MaxRequestsKeepAliveEdit: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure HttpServer1ClientConnect(Sender: TObject;
      Client: TObject; Error: Word);
    procedure HttpServer1ClientDisconnect(Sender: TObject;
      Client: TObject; Error: Word);
    procedure HttpServer1ServerStarted(Sender: TObject);
    procedure HttpServer1ServerStopped(Sender: TObject);
    procedure HttpServer1HeadDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure HttpServer1PostedData(Sender: TObject;
      Client: TObject; Error: Word);
    procedure HttpServer1PostDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure ClearButtonClick(Sender: TObject);
    procedure WriteLogFileCheckBoxClick(Sender: TObject);
    procedure HttpServer1AuthGetType(Sender, Client: TObject{;
      var AuthenticationType: TAuthenticationType});
    procedure HttpServer1AuthGetPassword(Sender, Client: TObject;
      var Password: String);
    procedure HttpServer1AuthResult(Sender, Client: TObject;
      Success: Boolean);
    procedure HttpServer1GetDocument(Sender, Client: TObject;
      var Flags: THttpGetFlag);
    procedure FormDestroy(Sender: TObject);
    procedure HttpServer1AuthNtlmBeforeValidate(Sender, Client: TObject;
      var Allow: Boolean);
  private
    FIniFileName   : String;
    FInitialized   : Boolean;
    FCountRequests : Integer;
    FLogFile       : TextFile;
    FLogFileName   : String;
    FLogFileOpened : Boolean;
    procedure CreateVirtualDocument_Demo(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_DemoAuthAll(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_DemoNtlmAuth(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_DemoDigestAuth(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_DemoBasicAuth(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_Time(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_Bruno(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_Redir(Sender    : TObject;
                                             ClientCnx : TMyHttpConnection;
                                             var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_MyIP(Sender    : TObject;
                                             ClientCnx : TMyHttpConnection;
                                             var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_HeaderBug(Sender    : TObject;
                                              ClientCnx : TMyHttpConnection;
                                              var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_Template(Sender    : TObject;
                                             ClientCnx : TMyHttpConnection;
                                             var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_ViewFormData(Sender    : TObject;
                                                 ClientCnx : TMyHttpConnection;
                                                 var Flags : THttpGetFlag);
    procedure FormDataGetRow(Sender          : TObject;
                             const TableName : String;
                             Row             : Integer;
                             TagData         : TStringIndex;
                             var More        : Boolean;
                             UserData        : TObject);
    procedure DisplayHeader(ClientCnx : TMyHttpConnection);
    procedure ProcessPostedData_FormHandler(ClientCnx : TMyHttpConnection);
    procedure CloseLogFile;
    procedure OpenLogFile;
  public
    procedure Display(const Msg : String);
    property  IniFileName : String read FIniFileName write FIniFileName;
  protected
    procedure WmClientCount(var Msg: TMessage); message WM_CLIENT_COUNT;
  end;

var
  WebServForm       : TWebServForm;
  DisplayLock       : TRTLCriticalSection;
  LockFileAccess    : TRtlCriticalSection;

implementation

{$R *.DFM}

const
    { IniFile layout for persistent data }
    SectionWindow      = 'WindowMain';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Config';
    KeyDocDir          = 'DocDir';
    KeyDefaultDoc      = 'DefaultDoc';
    KeyTemplateDir     = 'TemplateDir';
    KeyPort            = 'Port';
    KeyDisplayHeader   = 'DisplayHeader';
    KeyLogToFile       = 'LogToFile';
    KeyDirList         = 'AllowDirList';
    KeyOutsideRoot     = 'AllowOutsideRoot';
    KeyRedirUrl        = 'RedirURL';
    KeyMaxRequests     = 'MaxRequestsKeepAlive';
    KeyKeepAliveSec    = 'KeepAliveTimeSec';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF DELPHI7_UP}
function DupeString(const AText: String; ACount: Integer): String;
var
    P: PChar;
    C: Integer;
begin
    C := Length(AText);
    SetLength(Result, C * ACount);
    P := Pointer(Result);
    if P = nil then
        Exit;
    while ACount > 0 do begin
        Move(Pointer(AText)^, P^, C);
        Inc(P, C);
        Dec(ACount);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.FormCreate(Sender: TObject);
begin
    { Create IniFileName based on EXE file name; }
    FIniFileName := GetIcsIniFileName;
    FLogFileName := ChangeFileExt(FIniFileName, '.log');
{$IFDEF DELPHI10_UP}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.FormDestroy(Sender: TObject);
begin
    //
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
    wsi     : TWSADATA;
    MyIp    : TStringList;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        { Restore persistent data from INI file }
        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        DocDirEdit.Text     := IniFile.ReadString(SectionData, KeyDocDir,
                                     ExtractFilePath(Application.ExeName) +
                                     'WebServData\wwwRoot\');
        DefaultDocEdit.Text := IniFile.ReadString(SectionData, KeyDefaultDoc,
                                                  'index.html');
        TemplateDirEdit.Text := IniFile.ReadString(SectionData, KeyTemplateDir,
                                     ExtractFilePath(Application.ExeName) +
                                     'WebServData\Template\');
        PortEdit.Text       := IniFile.ReadString(SectionData, KeyPort,
                                                  '80');
        RedirUrlEdit.Text   := IniFile.ReadString(SectionData, KeyRedirUrl,
                                     '/time.html');
        KeepAliveTimeSecEdit.Text := IniFile.ReadString(SectionData,
                                     KeyKeepAliveSec, '10');
        MaxRequestsKeepAliveEdit.Text := IniFile.ReadString(SectionData,
                                         KeyMaxRequests, '100');
        DirListCheckBox.Checked :=
                Boolean(IniFile.ReadInteger(SectionData, KeyDirList, 1));
        OutsideRootCheckBox.Checked :=
                Boolean(IniFile.ReadInteger(SectionData, KeyOutsideRoot, 0));
        DisplayHeaderCheckBox.Checked :=
                Boolean(IniFile.ReadInteger(SectionData, KeyDisplayHeader, 0));
        WriteLogFileCheckBox.Checked :=
                Boolean(IniFile.ReadInteger(SectionData, KeyLogToFile, 0));
        IniFile.Free;
        { Start log file }
        if WriteLogFileCheckBox.Checked then begin
            OpenLogFile;
            WriteLogFileCheckBox.Checked := FLogFileOpened;
        end;
        { Initialize client count caption }
        ClientCountLabel.Caption := '0';
        { Display version info for program and used components }
        wsi := WinsockInfo;
        DisplayMemo.Clear;
        Display(CopyRight);
        Display('Using:');
        Display('   ' + OverbyteIcsWSocket.CopyRight);
        Display('   ' + OverbyteIcsWSocket.CopyRight);
        Display('   ' + OverbyteIcsHttpSrv.CopyRight);
        Display('    Winsock:');
        Display('        Version ' +
                Format('%d.%d', [WinsockInfo.wHighVersion shr 8,
                                 WinsockInfo.wHighVersion and 15]));
        Display('        ' + StrPas(wsi.szDescription));
        Display('        ' + StrPas(wsi.szSystemStatus));
{$IFNDEF DELPHI3}
        { A bug in Delphi 3 makes lpVendorInfo invalid }
        if wsi.lpVendorInfo <> nil then
            Display('        ' + StrPas(wsi.lpVendorInfo));
{$ENDIF}
        MyIp := TStringList.Create;
        try
            MyIP.Delimiter := ' ';
            MyIP.AddStrings(LocalIPList);
            Display('My IP addresses: ' + MyIP.DelimitedText);
        finally
            MyIp.Free;
        end;
        { Automatically start server }
        StartButtonClick(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    { Save persistent data to INI file }
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData,    KeyDocDir,      HttpServer1.DocDir);
    IniFile.WriteString(SectionData,    KeyDefaultDoc,  HttpServer1.DefaultDoc);
    IniFile.WriteString(SectionData,    KeyTemplateDir, HttpServer1.TemplateDir);
    IniFile.WriteString(SectionData,    KeyPort,        HttpServer1.Port);
    IniFile.WriteString(SectionData,    KeyRedirUrl,    RedirUrlEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyDirList,
                                        Ord(DirListCheckBox.Checked));
    IniFile.WriteInteger(SectionData,   KeyOutsideRoot,
                                        Ord(OutsideRootCheckBox.Checked));
    IniFile.WriteInteger(SectionData,   KeyDisplayHeader,
                                        Ord(DisplayHeaderCheckBox.Checked));
    IniFile.WriteInteger(SectionData,   KeyLogToFile,
                                        Ord(WriteLogFileCheckBox.Checked));
    IniFile.WriteInteger(SectionData,   KeyKeepAliveSec,
                                        StrToIntDef(KeepAliveTimeSecEdit.Text, 10));
    IniFile.WriteInteger(SectionData,   KeyMaxRequests,
                                        StrToIntDef(MaxRequestsKeepAliveEdit.Text, 100));
    
    IniFile.UpdateFile;
    IniFile.Free;
    CloseLogFile;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in display memo box, making sure we don't overflow it.  }
procedure TWebServForm.Display(const Msg : String);
begin
    if csDestroying in ComponentState then Exit;
    EnterCriticalSection(DisplayLock);
    try
        DisplayMemo.Lines.BeginUpdate;
        try
            { We preserve only 200 lines }
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
            DisplayMemo.Lines.Add(Msg);
        finally
            DisplayMemo.Lines.EndUpdate;
            { Makes last line visible }
            {$IFNDEF VER80}
            SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
            {$ENDIF}
        end;
        if FLogFileOpened then begin
            try
                WriteLn(FLogFile, Msg);
            except
                on E:Exception do begin
                    DisplayMemo.Lines.Add('*** Exception' +
                                      E.CLassName + ': ' + E.Message +
                                      ' writing to log file ***');
                end;
            end;
        end;
    finally
        LeaveCriticalSection(DisplayLock);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when user clicks on start button. It is also }
{ called from FormShow event handler, at program startup. It starts server. }
{ We need to pass default document, document directory and client class     }
{ to HTTP server component. Client class is very usefull because it         }
{ instruct server component to instanciate our own client class instead of  }
{ defualt client class. Using our own client class will enables you to add  }
{ any data we need to handle our application. This data is private for each }
{ client.                                                                   }
{ When server is started, we will get OnServerStarted event triggered.      }
procedure TWebServForm.StartButtonClick(Sender: TObject);
begin
    if DirListCheckBox.Checked then
        HttpServer1.Options := HttpServer1.Options + [hoAllowDirList]
    else
        HttpServer1.Options := HttpServer1.Options - [hoAllowDirList];
    if OutsideRootCheckBox.Checked then
        HttpServer1.Options := HttpServer1.Options + [hoAllowOutsideRoot]
    else
        HttpServer1.Options := HttpServer1.Options - [hoAllowOutsideRoot];

    HttpServer1.DocDir               := Trim(DocDirEdit.Text);
    HttpServer1.DefaultDoc           := Trim(DefaultDocEdit.Text);
    HttpServer1.TemplateDir          := Trim(TemplateDirEdit.Text);
    HttpServer1.Port                 := Trim(PortEdit.Text);
    HttpServer1.KeepAliveTimeSec     := StrToIntDef(KeepAliveTimeSecEdit.Text, 10);
    HttpServer1.MaxRequestsKeepAlive := StrToIntDef(MaxRequestsKeepAliveEdit.Text, 100);
    HttpServer1.ClientClass          := TMyHttpConnection;
    try
        HttpServer1.Start;
    except
        on E: Exception do
        begin
            Display('**** Unable to start server ****');
            if HttpServer1.WSocketServer.LastError = WSAEADDRINUSE then
            begin
                    Display('**** Port ' + HttpServer1.Port +
                        ' already used by another application ****');
                    Exit;
            end;
            Display('**** ' + E.ClassName + ': ' + E.Message + ' ****');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when user clicks on stop button. We just  }
{ stop the server. We will get OnServerStopped event triggered.             }
procedure TWebServForm.StopButtonClick(Sender: TObject);
begin
    HttpServer1.Stop;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when user clicks on clear buttoN; We just }
{ clear the memo used for displaying activity.                              }
procedure TWebServForm.ClearButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server is started, that is when }
{ server socket has started listening.                                      }
procedure TWebServForm.HttpServer1ServerStarted(Sender: TObject);
var
    DemoUrl : String;
begin
    DocDirEdit.Enabled                := FALSE;
    DefaultDocEdit.Enabled            := FALSE;
    DirListCheckBox.Enabled           := FALSE;
    OutsideRootCheckBox.Enabled       := FALSE;
    PortEdit.Enabled                  := FALSE;
    KeepAliveTimeSecEdit.Enabled      := FALSE;
    MaxRequestsKeepAliveEdit.Enabled  := FALSE;
    StartButton.Enabled               := FALSE;
    StopButton.Enabled                := TRUE;
    Display('Server is waiting for connections on port ' + HttpServer1.Port);

    DemoUrl := 'http://' + LowerCase(LocalHostName);
    if (HttpServer1.Port <> '80') and (HttpServer1.Port <> 'http') then
        DemoUrl := DemoUrl + ':' + HttpServer1.Port;
    DemoUrl := DemoUrl + '/demo.html';

    Display('Point your browser to ' + DemoUrl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when server has been stopped, that is     }
{ when server socket stop listening.                                        }
procedure TWebServForm.HttpServer1ServerStopped(Sender: TObject);
begin
    DocDirEdit.Enabled                := TRUE;
    DefaultDocEdit.Enabled            := TRUE;
    DirListCheckBox.Enabled           := TRUE;
    OutsideRootCheckBox.Enabled       := TRUE;
    PortEdit.Enabled                  := TRUE;
    StartButton.Enabled               := TRUE;
    KeepAliveTimeSecEdit.Enabled      := TRUE;
    MaxRequestsKeepAliveEdit.Enabled  := TRUE;
    StopButton.Enabled                := FALSE;
    Display('Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when a new client has connected.          }
procedure TWebServForm.HttpServer1ClientConnect(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client connecting                     }
    Error  : Word);                 { Error in connection                   }
begin
    PostMessage(Handle, WM_CLIENT_COUNT, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when a client is disconnecting, just      }
{ before client component is closed.                                        }
procedure TWebServForm.HttpServer1ClientDisconnect(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client connecting                     }
    Error  : Word);                 { Error in disconnection                }
begin
    PostMessage(Handle, WM_CLIENT_COUNT, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.HttpServer1GetDocument(Sender, Client: TObject;
  var Flags: THttpGetFlag);
var
    ClientCnx  : TMyHttpConnection;
begin
    if Flags = hg401 then
        Exit;
    { It's easyer to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);
    { Count request and display a message }
    InterlockedIncrement(FCountRequests);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
            ': ' + ClientCnx.Version + ' GET ' + ClientCnx.Path);
    DisplayHeader(ClientCnx);

    { Instead of the long if/then/else below, we could use a lookup table  }
    { Trap '/demo.html' to dynamically generate a simple HTML document }
    if CompareText(ClientCnx.Path, '/demo.html') = 0 then
        CreateVirtualDocument_Demo(Sender, ClientCnx, Flags)
    else if CompareText(ClientCnx.Path, '/demoBasicAuth.html') = 0 then
        CreateVirtualDocument_DemoBasicAuth(Sender, ClientCnx, Flags)
    else if CompareText(ClientCnx.Path, '/demoDigestAuth.html') = 0 then
        CreateVirtualDocument_DemoDigestAuth(Sender, ClientCnx, Flags)
    else if CompareText(ClientCnx.Path, '/demoNtlmAuth.html') = 0 then
        CreateVirtualDocument_DemoNtlmAuth(Sender, ClientCnx, Flags)
    else if CompareText(ClientCnx.Path, '/demoAuthAll.html') = 0 then
        CreateVirtualDocument_DemoAuthAll(Sender, ClientCnx, Flags)
    { Trap '/time.html' path to dynamically generate a dynamic answer. }
    else if CompareText(ClientCnx.Path, '/time.html') = 0 then
        CreateVirtualDocument_Time(Sender, ClientCnx, Flags)
    else if CompareText(ClientCnx.Path, '/bruno.html') = 0 then
        CreateVirtualDocument_Bruno(Sender, ClientCnx, Flags)
    { Trap '/myip.html' path to dynamically generate a dynamic answer. }
    else if CompareText(ClientCnx.Path, '/myip.html') = 0 then
        CreateVirtualDocument_MyIP(Sender, ClientCnx, Flags)
    { Trap '/HeaderBug.html' path to dynamically generate a dynamic answer. }
    else if CompareText(ClientCnx.Path, '/HeaderBug.html') = 0 then
        CreateVirtualDocument_HeaderBug(Sender, ClientCnx, Flags)
    { Trap '/redir.html' to dynamically generate a redirection answer }
    else if CompareText(ClientCnx.Path, '/redir.html') = 0 then
        CreateVirtualDocument_Redir(Sender, ClientCnx, Flags)
    { Trap '/formdata.html' to dynamically generate a HTML form answer }
    else if CompareText(ClientCnx.Path, '/formdata.html') = 0 then
        CreateVirtualDocument_ViewFormData{CreateVirtualDocument_formdata_htm}(Sender, ClientCnx, Flags)
    else if CompareText(ClientCnx.Path, '/template.html') = 0 then
        CreateVirtualDocument_template(Sender, ClientCnx, Flags);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server component receive a HEAD }
{ command from any client.                                                  }
{ We just count the request, display a message and let HTTP server          }
{ component handle everything.                                              }
{ We should trap every URI we handle internally...                          }
procedure TWebServForm.HttpServer1HeadDocument(
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    ClientCnx  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);
    InterlockedIncrement(FCountRequests);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
            ': ' + ClientCnx.Version + ' HEAD ' + ClientCnx.Path);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server component receive a GET  }
{ command from any client.                                                  }
{ We count the request, display a message and trap '/time.htm' path for     }
{ special handling.                                                         }
{ There is no document time.htm on disk, we will create it on the fly. With }
{ a classic webserver we would have used a CGI or ISAPI/NSAPI to achieve    }
{ the same goal. It is much easier here since we can use Delphi code        }
{ directly to generate whatever we wants. Here for the demo we generate a   }
{ page with server data and time displayed.                                 }


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /demo.html document                      }
procedure TWebServForm.CreateVirtualDocument_Demo(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Demo - Menu</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS WebServer Demo Menu</H2>' +
            '<A HREF="/time.html">Server time</A><BR>'  +
            '<A HREF="/template.html">Template demo</A><BR>'  +
            '<A HREF="/form.html">Data entry</A><BR>'   +
            '<A HREF="/formdata.html">Show data file</A><BR>'   +
            '<A HREF="/redir.html">Redirection</A><BR>' +
            '<A HREF="/myip.html">Show client IP</A><BR>' +
            '<A HREF="/DemoBasicAuth.html">Password protected page</A> ' +
                     '(Basic method, usercode=test, password=basic)<BR>' +
            '<A HREF="/DemoDigestAuth.html">Password protected page</A> ' +
                     '(Digest method, usercode=test, password=digest)<BR>' +
            '<A HREF="/DemoNtlmAuth.html">Password protected page</A> ' +
                     '(NTLM method, usercode=(Windows) user, password=(Windows) password)<BR>' +
            '<A HREF="/DemoAuthAll.html">Password protected page</A> ' +
                     '(method is selected by the browser. Usercode/Password as above)<BR>' +
            '<A HREF="/">Default document</A><BR>'     +
            '<A HREF="http://www.overbyte.be">ICS Home page</A><BR>' +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoBasicAuth.html document                      }
procedure TWebServForm.CreateVirtualDocument_DemoBasicAuth(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS WebServer Demo - Password Protected Page - Basic' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS WebServer Password Protected Page</H2>' +
            'This page is protected by a username and password.<BR>' +
            'The authentication is done using basic mode.<BR>' +
            '<A HREF="/Demo.html">Demo menu</A><BR>'  +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoBasicAuth.html document            }
procedure TWebServForm.CreateVirtualDocument_DemoDigestAuth(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS WebServer Demo - Password Protected Page - Digest' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS WebServer Password Protected Page</H2>' +
            'This page is protected by a username and password.<BR>' +
            'The authentication is done using digest mode.<BR>' +
            '<A HREF="/Demo.html">Demo menu</A><BR>'  +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoNtlmAuth.html document             }
procedure TWebServForm.CreateVirtualDocument_DemoNtlmAuth(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS WebServer Demo - Password Protected Page - NTLM' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS WebServer Password Protected Page</H2>' +
            'This page is protected by a username and password.<BR>' +
            'The authentication is done using NTLM mode.<BR>' +
            '<A HREF="/Demo.html">Demo menu</A><BR>'  +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoAuthAll.html document             }
procedure TWebServForm.CreateVirtualDocument_DemoAuthAll(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS WebServer Demo - Password Protected Page - NTLM' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS WebServer Password Protected Page</H2>' +
            'This page is protected by a username and password.<BR>' +
            'Your browser selected the <B>' +
            HttpAuthTypeNames[ClientCnx.AuthType] +
            '</B> authentication mode.<BR>' +
            '<A HREF="/Demo.html">Demo menu</A><BR>'  +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.CreateVirtualDocument_Template(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerPage(
        Flags,
        '',
        NO_CACHE,
        'TemplateDemo.html',
        nil,
        ['TIME',    DateTimeToStr(Now),
         'PROGVER', WebServVersion,
         'SOURCE',  TextToHtmlText(HttpServer1.TemplateDir +
                                   'TemplateDemo.html')]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /redir.html document                     }
procedure TWebServForm.CreateVirtualDocument_Redir(
    Sender    : TObject;            { HTTP server component                 }
    ClientCnx : TMyHttpConnection;  { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Location : String;
begin
    Location := ClientCnx.Params;
    if Location = '' then
        Location := Trim(RedirUrlEdit.text);

    ClientCnx.AnswerString(Flags,
        '302 Moved',                    { Tell the browser about relocation }
        '',                             { Default Content-Type: text/html   }
        'Location: ' + Location + #13#10,            { Specify new location }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Demo - Redir</TITLE>' +
          '</HEAD>' + #13#10 +
          '<BODY>' +
            'You should be redirected automatically !<BR>' + #13#10 +
            '<A HREF="' + Location + '">Click Here</A><BR>' + #13#10 +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /time.html document                     }
procedure TWebServForm.CreateVirtualDocument_Time(
    Sender    : TObject;            { HTTP server component                 }
    ClientCnx : TMyHttpConnection;  { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
begin
    ClientCnx.AnswerString(Flags,
        '',                            { Default Status '200 OK'            }
        '',                            { Default Content-Type: text/html    }
        'Pragma: no-cache' + #13#10 +  { No client caching please           }
        'Expires: -1'      + #13#10,   { I said: no caching !               }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Demo</TITLE>' +
          '</HEAD>' + #13#10 +
          '<BODY>' +
            '<H2>Time at server side:</H2>' + #13#10 +
            '<P>' + DateTimeToStr(Now) +'</P>' + #13#10 +
            '<A HREF="/demo.html">Demo menu</A>' + #13#10 +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.CreateVirtualDocument_Bruno(
    Sender    : TObject;            { HTTP server component                 }
    ClientCnx : TMyHttpConnection;  { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Body      : String;
    Header : String;
    I      : Integer;
    Buf    : String;
    Count  : Integer;
begin
    Buf   := ClientCnx.Params;
    Count := StrToIntDef(Buf, 5000);
    if Count > 20000 then
        Count := 20000;
    Body := '<HTML>' + '<HEAD>' + '<TITLE>Test Page from Bruno :-)</TITLE>' +
            '</HEAD>' + #13#10 +  '<BODY>';
    for I := 1 to Count do
        Body := Body + '<br>This is line number ' + IntToStr(I);
    Body := Body + '</BODY></HTML>';
    Header := 'Pragma: no-cache' + #13#10 +  { No client caching please     }
              'Expires: -1'      + #13#10 +
              'Set-Cookie: Usuario=a; path=/'+ #13#10 +
              'Set-Cookie: Senha=a; path=/'+ #13#10;
//    ClientCnx.OnRequestDone := RequestDone;
    ClientCnx.AnswerString(Flags, '', '', Header, Body);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Produce a reply with a huge header line. Used to check client behaviour.  }
procedure TWebServForm.CreateVirtualDocument_HeaderBug(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',                            { Default Status '200 OK'            }
        '',                            { Default Content-Type: text/html    }
        'Pragma: no-cache' + #13#10 +  { No client caching please           }
        'Expires: -1'      + #13#10,   { I said: no caching !               }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Demo</TITLE>' +
          '</HEAD>' + #13#10 +
          '<BODY>' +
            'Congratulations !' + #13#10 +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.CreateVirtualDocument_MyIP(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        'Pragma: no-cache' + #13#10 +  { No client caching please           }
        'Expires: -1'      + #13#10,   { I said: no caching !               }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Demo</TITLE>' +
          '</HEAD>' + #13#10 +
          '<BODY>' +
            'Your IP is: ' +
            ClientCnx.PeerAddr + #13#10 +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ FormDataGetRow is called by AnswerPage when in find <#TABLE_ROWS> tag.    }
{ We have to read a line from the data file and call TagData.Add to seed    }
{ the HTML table with row data.                                             }
procedure TWebServForm.FormDataGetRow(
    Sender          : TObject;
    const TableName : String;
    Row             : Integer;
    TagData         : TStringIndex;
    var More        : Boolean;
    UserData        : TObject);
var
    BufA       : AnsiString;
    Buf        : String;
    ClientCnx : TMyHttpConnection;
begin
    { Check if the table name. There could be several tables or table       }
    { embedded in another table in the template file                        }
    if TableName <> 'DATAFILE' then
        Exit;

    { Get reference to the connection. It has our data private.             }
    ClientCnx := Sender as TMyHttpConnection;

    { Check if we have read all the data file                               }
    More := not Eof(ClientCnx.FDataFile);
    if not More then
        Exit;

    { Read a line form data file                                            }
    ReadLn(ClientCnx.FDataFile, BufA);

    { Convert to String }
    if ClientCnx.FFileIsUtf8 then
    {$IFDEF UNICODE}
        Buf := Utf8ToStringW(BufA)
    {$ELSE}
        Buf := Utf8ToStringA(BufA)
    {$ENDIF}
    else
        Buf := String(BufA);

    { Extract column data from the datafile line                            }
    TagData.Add('DATE', Copy(Buf, 1, 8));
    TagData.Add('TIME', Copy(Buf, 10, 6));
    TagData.Add('DATA', TextToHtmlText(Copy(Buf, 17, High(Integer))));

    { Alternate style for even or odd table lines                           }
    if (Row and 1) <> 0 then
        TagData.Add('STYLE', 'stEven')
    else
        TagData.Add('STYLE', 'stOdd');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Build the dynamic page to show datafile. This page is based on a template }
{ with a table.                                                             }
procedure TWebServForm.CreateVirtualDocument_ViewFormData(
    Sender    : TObject;            { HTTP server component                 }
    ClientCnx : TMyHttpConnection;  { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Bom : array [0..2] of Byte;
    I   : Integer;
begin
    { Open data file                                                        }
    AssignFile(ClientCnx.FDataFile, 'FormHandler.txt');
    Reset(ClientCnx.FDataFile);
    try
        { Check whether the data file is UTF-8 encoded }
        I := 0;
        while not EOF(ClientCnx.FDataFile) and (I < 3) do
        begin
            Read(ClientCnx.FDataFile, AnsiChar(BOM[I]));
            Inc(I);
        end;
        ClientCnx.FFileIsUtf8 := (I = 3) and (Bom[0] = $EF) and
                                 (Bom[1] = $BB) and (Bom[2] = $BF);
        if not ClientCnx.FFileIsUtf8 then
            Reset(ClientCnx.FDataFile);

        { Set event handler for getting datafile rows                       }
        ClientCnx.OnGetRowData := FormDataGetRow;
        ClientCnx.AnswerPage(
            Flags,
            '',
            NO_CACHE,
            'FormData.html',
            nil,
            ['NOW', DateTimeToStr(Now)]);
        { Clear event handler                                               }
        ClientCnx.OnGetRowData := nil;
    finally
        CloseFile(ClientCnx.FDataFile);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server component receive a POST }
{ command from any client.                                                  }
{ We count the request, display a message and trap posted data.             }
{ To check for posted data, you may construct the following HTML document:  }
{ <HTML>                                                                    }
{   <HEAD>                                                                  }
{     <TITLE>Test Form 1</TITLE>                                            }
{   </HEAD>                                                                 }
{   <BODY>                                                                  }
{     <H2>Enter your first and last name</H2>                               }
{     <FORM METHOD="POST" ACTION="/cgi-bin/FormHandler">                    }
{       <TABLE BORDER="0" ALIGN="DEFAULT" WIDTH="100%">                     }
{         <TR>                                                              }
{           <TD>First name</TD>                                             }
{           <TD><INPUT TYPE="TEXT" NAME="FirstName"                         }
{                      MAXLENGTH="25" VALUE="YourFirstName"></TD>           }
{         </TR>                                                             }
{         <TR>                                                              }
{           <TD>Last name</TD>                                              }
{           <TD><INPUT TYPE="TEXT" NAME="LastName"                          }
{                      MAXLENGTH="25" VALUE="YourLastName"></TD>            }
{         </TR>                                                             }
{       </TABLE>                                                            }
{       <P><INPUT TYPE="SUBMIT" NAME="Submit" VALUE="Button"></P>           }
{     </FORM>                                                               }
{   </BODY>                                                                 }
{ </HTML>                                                                   }
procedure TWebServForm.HttpServer1PostDocument(
    Sender    : TObject;            { HTTP server component                 }
    Client    : TObject;            { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    ClientCnx  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);

    { Count request and display a message }
    InterlockedIncrement(FCountRequests);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
            ': ' + ClientCnx.Version + ' POST ' + ClientCnx.Path);
    DisplayHeader(ClientCnx);

    { Check for request past. We only accept data for '/cgi-bin/FormHandler' }
    if CompareText(ClientCnx.Path, '/cgi-bin/FormHandler') = 0 then begin
        { Tell HTTP server that we will accept posted data        }
        { OnPostedData event will be triggered when data comes in }
        Flags := hgAcceptData;
        { We wants to receive any data type. So we turn line mode off on   }
        { client connection.                                               }
        ClientCnx.LineMode := FALSE;
        { We need a buffer to hold posted data. We allocate as much as the }
        { size of posted data plus one byte for terminating nul char.      }
        { We should check for ContentLength = 0 and handle that case...    }
        ReallocMem(ClientCnx.FPostedRawData,
                   ClientCnx.RequestContentLength + 1);
        { Clear received length }
        ClientCnx.FDataLen := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered for each data packet posted by client     }
{ when we told HTTP server component that we will accept posted data.       }
{ We have to receive ALL data which is sent by remote client, even if there }
{ is more than what ContentLength tells us !                                }
{ If ContentLength = 0, then we should receive data until connection is     }
{ closed...                                                                 }
procedure TWebServForm.HttpServer1PostedData(
    Sender : TObject;               { HTTP server component                 }
    Client : TObject;               { Client posting data                   }
    Error  : Word);                 { Error in data receiving               }
var
    Len     : Integer;
    Remains : Integer;
    Junk    : array [0..255] of AnsiChar;
    ClientCnx  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);

    { How much data do we have to receive ? }
    Remains := ClientCnx.RequestContentLength - ClientCnx.FDataLen;
    if Remains <= 0 then begin
        { We got all our data. Junk anything else ! }
        Len := ClientCnx.Receive(@Junk, SizeOf(Junk) - 1);
        if Len >= 0 then
            Junk[Len] := #0;
        Exit;
    end;
    { Receive as much data as we need to receive. But warning: we may       }
    { receive much less data. Data will be split into several packets we    }
    { have to assemble in our buffer.                                       }
    Len := ClientCnx.Receive(ClientCnx.FPostedRawData + ClientCnx.FDataLen, Remains);
    { Sometimes, winsock doesn't wants to givve any data... }
    if Len <= 0 then
        Exit;

    { Add received length to our count }
    Inc(ClientCnx.FDataLen, Len);
    { Add a nul terminating byte (handy to handle data as a string) }
    ClientCnx.FPostedRawData[ClientCnx.FDataLen] := #0;
    { Display receive data so far }
    Display('Data: ''' + StrPas(ClientCnx.FPostedRawData) + '''');
    { When we received the whole thing, we can process it }
    if ClientCnx.FDataLen = ClientCnx.RequestContentLength then begin
{$IFDEF COMPILER12_UP}
        ClientCnx.FPostedDataBuffer := Pointer(UnicodeString(ClientCnx.FPostedRawData)); // Cast to Unicode
{$ELSE}
        ClientCnx.FPostedDataBuffer := ClientCnx.FPostedRawData;
{$ENDIF}
        { First we must tell the component that we've got all the data }
        ClientCnx.PostedDataReceived;
        { Then we check if the request is one we handle }
        if CompareText(ClientCnx.Path, '/cgi-bin/FormHandler') = 0 then
            { We are happy to handle this one }
            ProcessPostedData_FormHandler(ClientCnx)
        else
            { We don't accept any other request }
            ClientCnx.Answer404;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will process posted data for FormHandler                             }
{ Data is saved in FormHandler.txt file                                     }
procedure TWebServForm.ProcessPostedData_FormHandler(
    ClientCnx : TMyHttpConnection);
var
    Stream    : TStream;
    FileName  : String;
    FirstName : String;
    LastName  : String;
    HostName  : String;
    Buf       : String;
    Dummy     : THttpGetFlag;
    Bom       : array[0..2] of Byte;
    IsUtf8    : Boolean;
    Len       : Integer;
    Utf8Str   : AnsiString;
begin
    { Extract fields from posted data. }
    ExtractURLEncodedValue(ClientCnx.FPostedDataBuffer, 'FirstName', FirstName);
    ExtractURLEncodedValue(ClientCnx.FPostedDataBuffer, 'LastName',  LastName);
    { Get client IP address. We could to ReverseDnsLookup to get hostname }
    HostName := ClientCnx.PeerAddr;
    { Build the record to write to data file }
    Buf      := FormatDateTime('YYYYMMDD HHNNSS ', Now) +
                FirstName + '.' + LastName + '@' + HostName + #13#10;

    { Save data to a text file }
    FileName := ExtractFilePath(Application.ExeName) + 'FormHandler.txt';

    EnterCriticalSection(LockFileAccess);
    try
        if FileExists(FileName) then begin
            Stream := TFileStream.Create(FileName, fmOpenReadWrite);
            { Check whether the data file is UTF-8 encoded }
            Len := Stream.Read(Bom[0], SizeOf(Bom));
            IsUtf8 := (Len = 3) and (Bom[0] = $EF) and (Bom[1] = $BB) and
                      (Bom[2] = $BF);
            Stream.Seek(0, soFromEnd);
        end
        else begin
            { We use UTF-8 by default for new data files }
            Stream := TFileStream.Create(FileName, fmCreate);
            IsUtf8 := TRUE;
            Bom[0] := $EF; Bom[1] := $BB; Bom[2] := $BF;
            Stream.Write(Bom[0], SizeOf(Bom));
        end;
        if IsUtf8 then
        begin
            Utf8Str := StringToUtf8(Buf);
            Stream.Write(PAnsiChar(Utf8Str)^, Length(Utf8Str));
        end
        else
            StreamWriteStrA(Stream, Buf);
        Stream.Destroy;
    finally
        LeaveCriticalSection(LockFileAccess);
    end;

    { Here is the place to check for valid input data and produce a HTML }
    { answer according to data validation.                               }
    { Here for simplicity, we don't check data and always produce the    }
    { same HTML answer.                                                  }
    ClientCnx.AnswerString(Dummy,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Form Demo</TITLE>' +
          '</HEAD>' + #13#10 +
          '<BODY>' +
            '<H2>Your data has been recorded:</H2>' + #13#10 +
            '<P>' + TextToHtmlText(FirstName) + '.' +
                    TextToHtmlText(LastName)  + '@' +
                    TextToHtmlText(HostName)  +'</P>' +
            '<A HREF="/form.html">More data entry</A><BR>' +
            '<A HREF="/FormData.html">View data file</A><BR>' +
            '<A HREF="/demo.html">Back to demo menu</A><BR>' +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.DisplayHeader(ClientCnx : TMyHttpConnection);
var
    I : Integer;
begin
    if not DisplayHeaderCheckBox.Checked then
        Exit;
    for I := 0 to ClientCnx.RequestHeader.Count - 1 do
        Display('HDR' + IntToStr(I + 1) + ') ' +
                ClientCnx.RequestHeader.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We need to override parent class destructor because we have allocated     }
{ memory for our data buffer.                                               }
destructor TMyHttpConnection.Destroy;
begin
    if Assigned(FPostedRawData) then begin
        FreeMem(FPostedRawData, FPostedDataSize);
        FPostedRawData  := nil;
        FPostedDataSize := 0;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.OpenLogFile;
begin
    if FLogFileOpened then
        Exit;
    try
        AssignFile(FLogFile, FLogFileName);
        if FileExists(FLogFileName) then
            Append(FLogFile)
        else
            Rewrite(FLogFile);
        WriteLn(FLogFile, '[' + FormatDateTime('HH:NN:SS YYYY/MM/DD', Now) +
                          ' Log file opened.]');
        FLogFileOpened := TRUE;
    except
        FLogFileOpened := FALSE;
        Display('*** Unable to open log file ***');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.CloseLogFile;
begin
    if not FLogFileOpened then
        Exit;
    FLogFileOpened := FALSE;
    WriteLn(FLogFile, '[' + FormatDateTime('HH:NN:SS YYYY/MM/DD', Now) +
                      ' Log file Closed.]');
    CloseFile(FLogFile);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.WmClientCount(var Msg: TMessage);
begin
    ClientCountLabel.Caption := IntToStr(HttpServer1.ClientCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.WriteLogFileCheckBoxClick(Sender: TObject);
begin
    if WriteLogFileCheckBox.Checked then
        OpenLogFile
    else
        CloseLogFile;
    WriteLogFileCheckBox.Checked := FLogFileOpened;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.HttpServer1AuthGetType(
    Sender, Client: TObject);
var
    ClientCnx  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);
    if CompareText(ClientCnx.Path, '/DemoBasicAuth.html') = 0 then begin
        ClientCnx.AuthTypes  := [atBasic];
        ClientCnx.AuthRealm := 'DemoBasicAuth';
    end
    else if CompareText(ClientCnx.Path, '/DemoDigestAuth.html') = 0 then begin
        ClientCnx.AuthTypes  := [atDigest];
        ClientCnx.AuthRealm := 'DemoDigestAuth';
    end
    else if CompareText(ClientCnx.Path, '/DemoNtlmAuth.html') = 0 then begin
        ClientCnx.AuthTypes  := [atNtlm];
        ClientCnx.AuthRealm := 'DemoNtlmAuth';
    end
    else if CompareText(ClientCnx.Path, '/DemoAuthAll.html') = 0 then begin
        ClientCnx.AuthTypes  := [atBasic, atDigest, atNtlm];
        ClientCnx.AuthRealm := 'DemoAuthAll';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.HttpServer1AuthGetPassword(
    Sender       : TObject;
    Client       : TObject;
    var Password : String);
var
    ClientCnx  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);
    Display('AuthGetPassword for "' + ClientCnx.Path + '" AuthType is "' +
            AuthTypesToString(ClientCnx.AuthTypes) + '"');
    if (ClientCnx.AuthTypes     = [atDigest]) and
       (ClientCnx.AuthUserName = 'test') then
        Password := 'digest'
    else if (ClientCnx.AuthTypes     = [atBasic]) and
            (ClientCnx.AuthUserName = 'test') then
        Password := 'basic'
    else if (ClientCnx.AuthTypes = [atNtlm]) then ;
        //  nothing to do windows will validate credentials
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.HttpServer1AuthResult(
    Sender, Client : TObject;
    Success        : Boolean);
var
    ClientCnx  : TMyHttpConnection;
const
    SuccessStr : array [Boolean] of String = ('failed', 'OK');
begin
    { It's easier to do the cast one time. Could use with clause...         }
    ClientCnx := TMyHttpConnection(Client);

    { If we always want to pop up client browser's login dialog with digest }
    { authentication when the nonce is stale we may set FAuthDigestStale    }
    { back to FALSE.  Note: Do not set this value to TRUE.                  }
    { A nonce is considered stale after AuthDigestNonceLifeTimeMin expired. }
    { Uncomment next three lines to see what changes.                       }
    {if (not Success) and (ClientCnx.AuthTypes = [atDigest]) and
       ClientCnx.FAuthDigestStale then
        ClientCnx.FAuthDigestStale := FALSE;}

    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] authentication ' +
            SuccessStr[Success] + ' for ' +
            ClientCnx.Path);

    if (not Success) and (ClientCnx.AuthTypes = [atNtlm]) and
       (ClientCnx.AuthNtlmSession <> nil) then
        Display(ClientCnx.AuthNtlmSession.AuthErrorDesc);  // just for debugging!
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServForm.HttpServer1AuthNtlmBeforeValidate(Sender, Client: TObject;
  var Allow: Boolean);
var
    ClientCnx  : TMyHttpConnection;
begin
    ClientCnx := TMyHttpConnection(Client);
    Allow := (ClientCnx.AuthNtlmSession.Username <> '') and
             (ClientCnx.AuthNtlmSession.Domain <> 'SomeDomain');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
    InitializeCriticalSection(DisplayLock);
    InitializeCriticalSection(LockFileAccess);

finalization
    DeleteCriticalSection(DisplayLock);
    DeleteCriticalSection(LockFileAccess);

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}    
end.

