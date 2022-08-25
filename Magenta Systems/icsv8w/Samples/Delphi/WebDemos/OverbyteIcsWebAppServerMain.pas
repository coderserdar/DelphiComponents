{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     April 11, 2009
Description:  WebAppServer is a demo application showing the HTTP application
              server component (THttpAppSrv).
Version:      8.54
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009-2018 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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
Jul 3, 2009 V1.01 Angus - added mailer send email form demo
                          added W3C format log file
Jul 10, 2009 V1.02 Arno - Assigned correct ClientClass which fixed Access Violations
                   whenever a connection was closed or established.
Sep 01, 2009 V1.03 Angus - report exceptions creating virtual pages
Aug 08, 2010 V1.04 F.Piette: OnBgException is now published. Use a different
                   method for listening and client sockets.
Jun 09, 2013 V1.05 FPiette added code for DWScript support. Contionnaly
                   compiled using "use_DWScript" symbol.
Nov 16, 2013 V1.06 Arno added handler for new event OnDisplay.
May 21, 2018 V8.54 Added Utils instead of FtpSrvT

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWebAppServerMain;

{ DEFINE use_DWScript}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls,
  OverbyteIcsWndControl,
  OverbyteIcsWSocket,
  OverbyteIcsWebSession,
  OverbyteIcsHttpSrv,
  OverbyteIcsUtils,
  OverbyteIcsHttpAppServer,
  OverbyteIcsWebAppServerDataModule,
  OverbyteIcsWebAppServerSessionData,
  OverbyteIcsWebAppServerConfig,
  OverbyteIcsWebAppServerUrlDefs,
  OverbyteIcsWebAppServerHomePage,
  OverbyteIcsWebAppServerHelloWorld,
  OverbyteIcsWebAppServerCounter,
  OverbyteIcsWebAppServerLogin,
  OverbyteIcsWebAppServerCounterView,
  OverbyteIcsWebAppServerMailer,
{$IFDEF use_DWScript}
  OverbyteIcsWebAppServerDWScriptUrlHandler,
{$ENDIF}
  OverbyteIcsWebAppServerHead;

const
  WM_APPSTARTUP = WM_USER + 1;
  SimpLogName = '"webapp-"yyyymmdd".log"' ;
  WebLogFields = '#Fields: date time s-sitename s-computername s-ip cs-method cs-uri-stem cs-uri-query ' +
                   's-port cs-username c-ip cs-version cs(User-Agent) cs(Referer) cs-host sc-status ' +
                   'sc-bytes cs-bytes time-taken' ;
  ISODateMask = 'yyyy-mm-dd' ;
  ISODateTimeMask = 'yyyy-mm-dd"T"hh:nn:ss' ;
  ISOTimeMask = 'hh:nn:ss' ;

type
  TAppHttpConnection = class(THttpAppSrvConnection)
  protected
  public
    CStartTick: longword ;
    CLastRead: int64 ;
    CLastWrite: int64 ;
    constructor Create(AOwner: TComponent); override;
  end ;

type
  TWebAppSrvForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    HttpAppSrv1: THttpAppSrv;
    HousekeepingTimer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HttpAppSrv1ClientConnect(Sender, Client: TObject; Error: Word);
    procedure HttpAppSrv1GetDocument(Sender, Client: TObject;
                                     var Flags: THttpGetFlag);
    procedure HttpAppSrv1ServerStarted(Sender: TObject);
    procedure HttpAppSrv1ServerStopped(Sender: TObject);
    procedure HttpAppSrv1DeleteSession(Sender: TObject; Session: TWebSession);
    procedure HousekeepingTimerTimer(Sender: TObject);
    procedure HttpAppSrv1AfterAnswer(Sender, Client: TObject);
    procedure HttpAppSrv1BeforeProcessRequest(Sender, Client: TObject);
    procedure HttpAppSrv1VirtualException(Sender: TObject; E: Exception; Method: THttpMethod; const Path: string);
    procedure HttpAppSrv1BgException(Sender: TObject; E: Exception; var CanClose : Boolean);
    procedure HttpAppSrv1Display(Sender: TObject; const Msg: string);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FDataDir     : String;
    FSessionFile : String;
    procedure WMAppStartup(var msg: TMessage); message WM_APPSTARTUP;
  public
    procedure Display(const Msg : String);
    procedure DisplayHandler(Sender : TObject; const Msg : String);
    procedure HttpAppSrvClientBgException(Sender: TObject; E: Exception; var CanClose : Boolean);
    property  IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  WebAppSrvForm: TWebAppSrvForm;
  SrvCompName: string;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ simple log file, writes Msg to text file in progam directory with FNameMask
  unless mask includes // or x:
  mask generally includes date only - "mylog-"yyyymmdd".log"
  path needed quote at start - "c:\temp\mylog-"yyyymmdd".log"  }
procedure SimpleLogging (const FNameMask, Msg: String);
var
    LogFileName, S: String;
    f: TextFile;
begin
    S := FormatDateTime (ISOTimeMask, Time) + ' ' + Msg;
    try
        LogFileName := FormatDateTime (FNameMask, Date) ;
        if (Pos ('//', LogFileName) <> 1) and (Pos (':', LogFileName) <> 2) then
            LogFileName := ExtractFileDir (ParamStr (0)) + '\' + LogFileName ;
        AssignFile (f, LogFileName);
        if FileExists (LogFileName) then
        begin
            Append (f);
        end
        else
        begin
            Rewrite (f);
        end;
        Writeln (f, S);
        CloseFile(f);
    except
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ get the computer name from networking }
function GetCompName: string;
var
    Buffer: array[0..255] of WideChar ;
    NLen: DWORD ;
begin
    Buffer [0] := #0 ;
    result := '' ;
    NLen := Length (Buffer) ;
    if GetComputerNameW (Buffer, NLen) then Result := Buffer ;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TAppHttpConnection.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
  { keep alive means connection may be used for multiple requests so we must track how much
    data is sent before and after each request }
    CLastRead := 0 ;
    CLastWrite := 0 ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.FormCreate(Sender: TObject);
begin
    FIniFileName := OverbyteIcsIniFiles.GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIcsIniFile.Create(FIniFileName);
        try
            Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
            Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
            Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        finally
            IniFile.Free;
        end;
        DisplayMemo.Clear;
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HousekeepingTimerTimer(Sender: TObject);
begin
    CleanupTimeStampedDir(WebAppSrvDataModule.DataDir);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ write W3C log, similar to Microsoft IIS, log line looks something like this:
12:34:48 2009-07-03 12:34:48 WebAppDemo PC19 0.0.0.0 GET /login/loginform.html - 20105 - 192.168.1.119 HTTP/1.1 Mozilla/5.0+(Windows;+U;+Windows+NT+5.1;+en-GB;+rv:1.9.0.11)+Gecko/2009060215+Firefox/3.0.11+(.NET+CLR+3.5.30729) http://pc19:20105/ pc19:20105 200 2101 1138 31 }
procedure TWebAppSrvForm.HttpAppSrv1AfterAnswer(Sender, Client: TObject);
var
    RemoteClient: TAppHttpConnection;
    newparams, newuser, info: string ;
    duration: longword ;
    curread, curwrite: int64 ;

    function SpaceToPlus (const S: String): String ;
    begin
        result := StringReplace (S, #32, '+', [rfReplaceAll]) ;
    end;

begin
    RemoteClient := TAppHttpConnection(Client) ;
    info := FormatDateTime (ISODateMask + #32 + ISOTimeMask, Now) ;
    with RemoteClient do
    begin
        try
            newparams := Params ;
            if newparams = '' then newparams := '-' ;
            newuser := SpaceToPlus (AuthUserName)  ;
            if newuser = '' then newuser := '-' ;
            curread := ReadCount - CLastRead ;
            curwrite := WriteCount - CLastWrite ;
            duration := IcsElapsedMSecs (CStartTick) ;
// #Fields: date time s-sitename s-computername s-ip cs-method
            info := info + #32 + 'WebAppDemo' + #32 + SrvCompName + #32 + Server.Addr + #32 + Method + #32 +
// #Fields: cs-uri-stem cs-uri-query s-port
                    SpaceToPlus (Path) + #32 + SpaceToPlus (newparams) + #32 + Server.Port + #32 +
//  #Fields: cs-username c-ip cs-version
                    newuser + #32 + GetPeerAddr + #32 + Version + #32 +
// #Fields: cs(User-Agent) cs(Referer)
                    SpaceToPlus (RequestUserAgent) + #32 + SpaceToPlus (RequestReferer) + #32 +
// #Fields: cs-host sc-status
                    RequestHost + #32 + IntToStr (FAnswerStatus) + #32 +
// #Fields: sc-bytes cs-bytes time-taken
                    IntToStr (curwrite) + #32 + IntToStr (curread) + #32 + IntToStr (duration);
            SimpleLogging (SimpLogName, info) ;
        except
            Display ('AfterAnswer Error - ' + info) ;
        end;
    end;
    RemoteClient.CLastRead := RemoteClient.ReadCount ;   // reset read ready for next request
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1BeforeProcessRequest(Sender, Client: TObject);
var
    RemoteClient: TAppHttpConnection;
begin
    RemoteClient := TAppHttpConnection(Client) ;
    RemoteClient.CStartTick := IcsGetTickCountX ;
    RemoteClient.CLastWrite := RemoteClient.WriteCount ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1ClientConnect(
    Sender, Client: TObject;
    Error: Word);
var
    ClientCnx : THttpAppSrvConnection;
begin
    ClientCnx                := Client as THttpAppSrvConnection;
    ClientCnx.WSessionCookie := 'OverbyteIcsWebAppServer' + HttpAppSrv1.Port;
    ClientCnx.OnBgException  := HttpAppSrvClientBgException;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1DeleteSession(
    Sender: TObject;
    Session: TWebSession);
var
    MySessionData : TAppSrvSessionData;
begin
    MySessionData := Session.SessionData as TAppSrvSessionData;
    Display('Session for user "' + MySessionData.UserCode + '" timed out');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1Display(Sender: TObject; const Msg: string);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1GetDocument(
    Sender, Client: TObject;
    var Flags: THttpGetFlag);
var
    ClientCnx : THttpAppSrvConnection;
begin
    ClientCnx := Client as THttpAppSrvConnection;
    if ClientCnx.Params = '' then
        Display(ClientCnx.PeerAddr + ' ' + ClientCnx.Path)
    else
        Display(ClientCnx.PeerAddr + ' ' + ClientCnx.Path + '?' + ClientCnx.Params);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1ServerStarted(Sender: TObject);
begin
    Display('Waiting for client on port ' + HttpAppSrv1.Port);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1ServerStopped(Sender: TObject);
begin
    HttpAppSrv1.WSessions.SaveToFile(FSessionFile);
    CleanupTimeStampedDir(WebAppSrvDataModule.DataDir);
    Display('Server is now stopped');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1VirtualException(Sender: TObject; E: Exception; Method: THttpMethod;
  const Path: string);
begin
    Display('Exception creating virtual page: ' + Path + ' - ' + E.Message);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrv1BgException(Sender: TObject; E: Exception; var CanClose : Boolean);
begin
    Display('Exception processing page - ' +
            E.ClassName + ': ' + E.Message);
    CanClose := FALSE;  // Let the server continue to accept connections
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.HttpAppSrvClientBgException(Sender: TObject; E: Exception; var CanClose : Boolean);
begin
    Display('Exception processing page - ' +
            E.ClassName + ': ' + E.Message);
    CanClose := TRUE;  // Shutdown client
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    HttpAppSrv1.Stop;
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.Display(const Msg : String);
var
    I : Integer;
begin
    if not Assigned(DisplayMemo) then
        Exit;
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            with TStringList.Create do
            try
                BeginUpdate;
                Assign(DisplayMemo.Lines);
                for I := 1 to 50 do
                    Delete(0);
                DisplayMemo.Lines.Text := Text;
            finally
                Free;
            end;
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.DisplayHandler(Sender : TObject; const Msg : String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebAppSrvForm.WMAppStartup(var msg: TMessage);
var
    BaseDir : String;
begin
    // ** Assign our derived client class ** //
    HttpAppSrv1.ClientClass    := TAppHttpConnection;

    BaseDir                    := IncludeTrailingPathDelimiter(
                                      ExtractFilePath(Application.ExeName));
    FDataDir                   := BaseDir + 'WebAppServerData\Data';
    FSessionFile               := FDataDir + '\Sessions.dat';
    HttpAppSrv1.DocDir         := BaseDir + 'WebAppServerData\wwwRoot';
    HttpAppSrv1.TemplateDir    := BaseDir + 'WebAppServerData\Templates';
    HttpAppSrv1.DefaultDoc     := 'index.html';

    // Force directory creation
    ForceDirectories(FDataDir);
    ForceDirectories(HttpAppSrv1.TemplateDir);
    ForceDirectories(HttpAppSrv1.DocDir);
    ForceDirectories(HttpAppSrv1.DocDir + '\Js');
    ForceDirectories(HttpAppSrv1.DocDir + '\Styles');
    ForceDirectories(HttpAppSrv1.DocDir + '\Images');

    WebAppSrvDataModule.IniFileName := FIniFileName;
    WebAppSrvDataModule.OnDisplay   := DisplayHandler;
    WebAppSrvDataModule.DataDir     := FDataDir;
    WebAppSrvDataModule.ImagesDir   := HttpAppSrv1.DocDir + '\Images';
    WebAppSrvDataModule.LoadConfig;

    // Allow direct access to all folders where static documents are.
    HttpAppSrv1.AddGetAllowedPath('/js/',     afBeginBy);
    HttpAppSrv1.AddGetAllowedPath('/styles/', afBeginBy);
    HttpAppSrv1.AddGetAllowedPath('/images/', afBeginBy);

    // Add all dynamic webpage handlers
    HttpAppSrv1.AddGetHandler('/',
                              TUrlHandlerDefaultDoc);
    HttpAppSrv1.AddGetHandler('/' + HttpAppSrv1.DefaultDoc,
                              TUrlHandlerDefaultDoc);
    HttpAppSrv1.AddGetHandler(UrlLogin,
                              TUrlHandlerLoginFormHtml);
    HttpAppSrv1.AddGetHandler(UrlDoLoginSecure,
                              TUrlHandlerDoLoginSecureHtml);
    HttpAppSrv1.AddGetHandler(UrlCounter,
                              TUrlHandlerCounterJpg);
    HttpAppSrv1.AddGetHandler(UrlHomePage,
                              TUrlHandlerHomePageHtml);
    HttpAppSrv1.AddGetHandler(UrlConfigForm,
                              TUrlHandlerConfigFormHtml);
    HttpAppSrv1.AddGetHandler(UrlConfigLogoPng,
                              TUrlHandlerConfigLogoPng);
    HttpAppSrv1.AddGetHandler(UrlDoConfigConfirmSaveHtml,
                              TUrlHandlerDoConfigConfirmSaveHtml);
    HttpAppSrv1.AddPostHandler(UrlDoConfigHtml,
                               TUrlHandlerDoConfigHtml);
    HttpAppSrv1.AddGetHandler(UrlCounterViewHtml,
                              TUrlHandlerCounterViewHtml);
    HttpAppSrv1.AddGetHandler(UrlAjaxFetchCounter,
                              TUrlHandlerAjaxFetchCounter);
    HttpAppSrv1.AddGetHandler(UrlJavascriptErrorHtml,
                              TUrlHandlerJavascriptErrorHtml);
    HttpAppSrv1.AddGetHandler('/mailer.html', TUrlHandlerMailer);
    HttpAppSrv1.AddPostHandler('/mailer.html', TUrlHandlerMailer);
    HttpAppSrv1.AddGetHandler(UrlHeadForm, TUrlHandlerHead);
{$IFDEF use_DWScript}
    HttpAppSrv1.AddGetHandler('/DWScripts/*',
                              TUrlHandlerDWScript);
{$ENDIF}

    // Just for demoing the simplest handler, let's add an "Helloworld" one
    HttpAppSrv1.AddGetHandler('/HelloWorld.html',
                              TUrlHandlerHelloWorld);

    if FileExists(FSessionFile) then begin
        try
            HttpAppSrv1.WSessions.LoadFromFile(FSessionFile);
            Display(IntToStr(HttpAppSrv1.SessionsCount) + ' sessions loaded');
        except
            // Ignore any exception, but clear anything partially loaded
            HttpAppSrv1.WSessions.Clear;
            // and delete existing (corrupted) file
            DeleteFile(FSessionFile);
            Display('Unable to load existing sessions');
        end;
    end;

    // Cleanup temporary files left from a previous run
    CleanupTimeStampedDir(WebAppSrvDataModule.DataDir);

    // Start the timer to do some housekeeping while running
    HousekeepingTimer.Interval := 5 * 60 * 1000;
    HousekeepingTimer.Enabled  := TRUE;

    // start logging file
    SrvCompName := GetCompName ;
    SimpleLogging (SimpLogName, FormatDateTime ('"#Date: "' + ISODateMask + #32 + ISOTimeMask + '"', Now)) ;
    SimpleLogging (SimpLogName, WebLogFields) ;


    // Start the HTTP server
    HttpAppSrv1.KeepAliveTimeSec := 30;
    HttpAppSrv1.SessionTimeout := 5 * 60;  // Seconds, must be done after loading
    HttpAppSrv1.Port           := WebAppSrvDataModule.Port;
    HttpAppSrv1.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


end.
